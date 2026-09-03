library(ggpubr)
library(dplyr)
library(tidyr)
library(car)
library(writexl)
library(factoextra)
library(readxl)
library(ggcorrplot)
library(grid)



library(PerformanceAnalytics)
library(psych)                

#load data##########################################################################
# Verkrijgen van relevante libraries en functies
source('../shared/helperFunctionsSQLDb.R')
installLoadPackages(c("dplyr","tidyr", "ggplot2", "ggalluvial","stringr","ggforce","cowplot","writexl","ggpmisc"))
#data per voedermiddle_________________________________________________________________________________________________
sql = "SELECT g.ID AS [groepID]
  , t.Name AS [grondsoort]
  , g.Naam AS [groep]
  , d.jaar
  , d.bedrijfID
  , b.Naam AS [naam]
  , o.ID AS [KLWoutputID]
  , i.ID AS [MetaFeatureSetID]
  , f.ID AS [MetaFeatureID]
  , f.Name AS [var]
  , v.AsDouble AS [waarde]
FROM tbGroep g
JOIN tbTemplateValue t ON t.DataTemplateID = 24 AND t.ID = g.GrondsoortID
JOIN tbGroepDeelname d ON d.GroepID = g.ID
JOIN tbBedrijf b ON b.ID = d.BedrijfID
JOIN tbKLWoutput o ON o.BedrijfID = d.BedrijfID AND o.Jaar = d.Jaar AND o.OutputType = 1
, tbMetaFeatureSet i
JOIN tbMetaFeature f ON f.MetaFeatureSetID = i.ID
, tbFeatureValue v
WHERE g.Nummer > 0
AND i.Name = 'KLWoutput'
AND (f.Name LIKE '%_geh_re' OR 
     f.Name LIKE '%_geh_vem' OR 
     f.Name LIKE '%_re_kvem' OR 
     f.Name LIKE '%_aandeel' OR 
     f.Name LIKE '%_verbruik' OR 
     f.Name LIKE 'opn%mk' OR
     f.Name LIKE '%geh_ds' OR
     f.Name LIKE '%geh_p')
AND v.MetaFeatureSetID = i.ID 
AND v.MetaFeatureID = f.ID 
AND v.TupleID = o.ID
AND o.KLWversie = 'KringloopWijzer intern 2025.09'
ORDER BY d.BedrijfID
 , d.Jaar
 , f.Name"

voeders.abbr = c("gr","gk","sm","rv","bp","kv","mp","ov", 'vg')
voeders.dict = c("gr" = "Vers gras", 'vg'= "Vers gras", 'gk'="Graskuil", "sm" = "Snijmais", "rv" = "Overig ruwvoer", "bp" = "Bijproducten", "kv" = "Krachtvoer", "mp" = "Melkproducten", "ov" = "Overig")
voeders.order = c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Melkproducten", "Vers gras","Overig")
voeders.color = colors <- c("Vers gras" = "#92D050", "Graskuil" =	"#548235", "Snijmais" =	"#FFC000", "Overig ruwvoer" =	"#843C0C", "Bijproducten"	= "#ED7D31", "Krachtvoer" =	"#4472C4")
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Melkproducten", "Vers gras","Overig")
property.abbr = c("aandeel","verbruik","re","kvem","vem","ds", "opn", "p")
property.dict = c("aandeel" = "aandeel (%)",
                  "verbruik" = "verbruik",
                  "re" = "RE gehalte (g/kg DS)", 
                  "kvem" = "REkVEM (g/kVEM)",
                  "vem" = "VEM/kg DS", 
                  "opn" = "opnamekoe (kg DS)",
                  "ds" = "DS gehalte (g/kg)",
                  "p" = "P gehalte (g/kg)")
listofvariables = c("aandeel (%)",
                    "RE gehalte (g/kg DS)",
                    "REkVEM (g/kVEM)",
                    "VEM/kg DS", 
                    "opnamekoe (kg DS)",
                    "opname incl. jongvee (kg DS)",
                    "DS gehalte (g/kg)",
                    "P gehalte (g/kg)")

data <- executeSQL(sql, "SqlServerRemote") %>%
  select(bedrijfID, naam, groep, jaar, grondsoort, var, waarde) %>%
  mutate(var = case_when(
    startsWith(var,"aanleg") ~ sub("(aanleg_)(.._)(.*)","\\2\\1\\3", var),
    startsWith(var,"opn") ~ sub("(opn_)(.._)(.*)","\\2\\1\\3", var),
    TRUE ~ var
  )) %>%
  separate(var, into = c("var1","var2","var3"), sep = "_", fill = "right") %>%
  mutate(
    var1 = ifelse(var1 == "opn", "rants", var1),
    var2 = ifelse(var2 == "tot", "opn", var2),
    voedermiddel = ifelse(var1 %in% voeders.abbr, voeders.dict[var1], NA_character_),
    eigenschap = case_when(
      var2 %in% names(property.dict) & (is.na(var3) | !(var3 %in% property.abbr)) ~ property.dict[var2],
      var3 %in% names(property.dict) ~ property.dict[var3],
      var2 == "opn" ~ "opname incl. jongvee (kg DS)",  # force opname incl. jongvee
      TRUE ~ NA_character_
    ),
    kenmerk = ifelse(var1 == "rants", "rantsoen", "")
  )

check <- data %>% distinct(var2,var3, eigenschap)

Voedermiddel <- data %>%
  filter(kenmerk != "rantsoen") %>%
  distinct(bedrijfID, naam, groep, jaar, grondsoort, voedermiddel, kenmerk, eigenschap, .keep_all = TRUE) %>%
  mutate(eigenschap = ifelse(eigenschap == "verbruik", "opname incl. jongvee (kg DS)", eigenschap)) %>%
  select(bedrijfID, naam, grondsoort, jaar, waarde, voedermiddel, eigenschap)%>%
  pivot_wider(names_from = eigenschap,
              values_from = waarde)%>%
  mutate(across(all_of(listofvariables),  # Use all_of to ensure we're referencing a vector of column names
                ~ ifelse(`opname incl. jongvee (kg DS)` == 0, NA, .)))

data2 <- data %>%
  filter(kenmerk == "rantsoen")%>%
  distinct(bedrijfID, naam, groep, jaar, grondsoort, kenmerk, eigenschap, .keep_all = T) %>% 
  pivot_wider(id_cols = c("bedrijfID","naam","groep", "jaar", "grondsoort", "voedermiddel"),
              names_from = c(kenmerk,eigenschap),names_sep = "",
              values_from = waarde) %>%
  select(bedrijfID, naam, groep, jaar, grondsoort, 
         `rantsoenopnamekoe (kg DS)`, 
         `rantsoenRE gehalte (g/kg DS)`, 
         `rantsoenREkVEM (g/kVEM)`,
         `rantsoenVEM/kg DS`, 
         rantsoenverbruik,
         `rantsoenP gehalte (g/kg)`)%>%
  rename(`rantsoen opname incl. jongvee (kg DS)` = rantsoenverbruik)

sql = "SELECT g.ID AS [groepID]
  , t.Name AS [grondsoort]
  , g.Naam AS [groep]
  , d.jaar
  , d.bedrijfID
  , b.Naam AS [naam]
  , o.ID AS [KLWoutputID]
  , i.ID AS [MetaFeatureSetID]
  , f.ID AS [MetaFeatureID]
  , f.Name AS [var]
  , v.AsDouble AS [waarde]
FROM tbGroep g
JOIN tbTemplateValue t ON t.DataTemplateID = 24 AND t.ID = g.GrondsoortID
JOIN tbGroepDeelname d ON d.GroepID = g.ID
JOIN tbBedrijf b ON b.ID = d.BedrijfID
JOIN tbKLWoutput o ON o.BedrijfID = d.BedrijfID AND o.Jaar = d.Jaar AND o.OutputType = 1
, tbMetaFeatureSet i
JOIN tbMetaFeature f ON f.MetaFeatureSetID = i.ID
, tbFeatureValue v
WHERE g.Nummer > 0
AND i.Name = 'KLWoutput'
AND (f.Name IN (
     'gve_melkvee', 
     'melkperha', 
     'efficientie_N', 
     'fpcmpkoe', 
     'melkpkoe', 
     'vet', 
     'eiwit', 
     'ureum', 
     'provincie',
     'opb_graspr_ds',
     'opb_mais_ds',
     'pcuren_mlk_wei',  
     'rants_geh_ds',
     'efficientie_P', 
     'aanleg_gk_re',
     'em_nh3_hagrond',
     'emnh3_stl_gve',
     'emnh3_ove_ha',
     'co2_pens_mk',
     'co2_pens',
     'co2_stal',
     'co2_voer',
     'co2_ene',
     'co2_aanv',
     'gr_geh_ch4_0',
     'emco2_tot_fpcm',
     'dzh_nbodem_over',
     'dzhm_nbodem_over',
     'nkoeien',
     'nkalf',
     'npink',
     'rantsoen_eiweig_sm',
     'jvper10mk',
     'opp_totaal',
     'melk_ha',
     'oppgras',
     'oppmais',
     'oppoverig',
     'oppbraak',
     'eiwiteig_pcteelt',
     'uurweidb', 'uurweido','uurcombib','uurcombio',
     'dgncombib','dgncombio','dgnweidb','dgnweido',
     'dgnzstvb','dgnzstvo',
     'dgnweidpi', 'dgnweidka',
     'graspr_tmst_kgn',
     'voereff_fpcm',
     'kring1_benut_tot',
     'graspr_kmst_kgn',
     'graspr_dmst_kgn',
     'screening',
     'vgnweidb','vgnweido','vgncombib','vgncombio','vgnzstvb','vgnzstvo','vgnweidpi','vgnweidka','vgnweidovg',
     'pcuren_mlk_stal','pcuren_mlk_wei','pcuren_ovg_stal','pcuren_ovg_wei',
     'dzh_blijgras_aand', 'dzh_eiwit_pcrants', 'opn_vg_mk', 'graspr_dmst_m3', 'graspr_dmst_kgn', 'graspr_kmst_kgn', 'graspr_wmst_kgn',
     'aankoop_aanleg_sm_hoev',
     'screening',
     'grond_gras'
))
AND v.MetaFeatureSetID = i.ID 
AND v.MetaFeatureID = f.ID 
AND v.TupleID = o.ID
AND o.KLWversie = 'KringloopWijzer intern 2025.09'
ORDER BY d.BedrijfID
 , d.Jaar
 , f.Name"

dontdelete <- read_excel("C:/Rfiles/WR/Koe en eiwit 2025/datameans3.xlsx")
datameans1 <- dontdelete

voeders.abbr = c("gr","gk","sm","rv","bp","kv","mp","ov", 'vg')
voeders.dict = c("gr" = "Vers gras", 'vg'= "Vers gras", 'gk'="Graskuil", "sm" = "Snijmais", "rv" = "Overig ruwvoer", "bp" = "Bijproducten", "kv" = "Krachtvoer", "mp" = "Melkproducten", "ov" = "Overig")
voeders.order = c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Melkproducten", "Vers gras","Overig")
voeders.color = colors <- c("Vers gras" = "#92D050", "Graskuil" =	"#548235", "Snijmais" =	"#FFC000", "Overig ruwvoer" =	"#843C0C", "Bijproducten"	= "#ED7D31", "Krachtvoer" =	"#4472C4")
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Melkproducten", "Vers gras","Overig")
fracSM.colors = c("< 5%" = "#548235", "5-25%" = "#843C0C", "> 25%" =	"#FFC000")
REgroup.colors <- c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c", "NA" = "darkgray")
REgroup.breaks <- c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")
prodCat.colors <- c("6000 - 8000 kg" = "#ff4d4d", "8000 - 10000 kg" = "#ffc14d", "10000 - 12000 kg" ="#3385ff", "12000 - 14000 kg" = "#00b33c")

# Prijzen instellen
prijszomerstal <- 0.18
prijsweide <- 0.1
prijsnijmaiseigenland <- 0.16
prijsnijmaisaankoop <- 0.16
prijsGraskuil <- 0.2
PrijskrachtvoerDVE <- 1.18
PrijskrachtvoerVEM <- 0.169
Prijskrachtvoermineralen <- 0.0047
PrijsperKVEMruwvoer <- 0.33
Melkprijsvet <- 5.04
Melkprijseiwit <- 7.56
Weidetoeslag <- 1.30 / 100

data <- executeSQL(sql, "SqlServerRemote") %>%
  distinct(bedrijfID, naam, groep, jaar, grondsoort, var, .keep_all = T) %>% 
  pivot_wider(id_cols = c("bedrijfID","naam","groep", "jaar", "grondsoort"),
              names_from = var,
              values_from = waarde) %>%  
  arrange(bedrijfID,jaar) %>%
  rename(intensiteit = melkperha,
         `fpcmProductie per koe (kg)`=fpcmpkoe,
         jongvee_per_10_melkkoe = jvper10mk,
         `eiwit_eigenteelt (%)` = eiwiteig_pcteelt,
         `vet (%)`=vet, 
         `eiwit (%)` = eiwit,
         `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)` = co2_pens_mk,
         `CO2 pensfermentatie: totaal (g CO2-eq/kg FPCM)` = co2_pens,
         `CO2 stal en mestopslag: totaal (g CO2-eq/kg FPCM)` = co2_stal,
         `CO2 voerproductie: totaal (g CO2-eq/kg FPCM)` = co2_voer,
         `CO2 energiebronnen: totaal (g CO2-eq/kg FPCM)` = co2_ene,
         `CO2 aanvoerbronnen: totaal (g CO2-eq/kg FPCM)` = co2_aanv,
         `emissie totaal per ton meetmelk (kg CO2-eq/ton FPCM)` = emco2_tot_fpcm,
         `Ammoniakemissie per ha (kg/ha)` = em_nh3_hagrond,
         `Ammoniakemissie uit stal en mestopslag (kg/gve)` = emnh3_stl_gve,
         `Ammoniakemissie uit bemesting en oogst (kg/ha)` = emnh3_ove_ha,
         `Duurzaamheid gehele bedrijf: overschot bodem totaal (kg N/ha)` = dzh_nbodem_over,
         `Duurzaamheid melkveetak: overschot bodem totaal (kg N/ha)` = dzhm_nbodem_over,
         `Melk per koe (kg)` = melkpkoe,
         `N-tot gras (kg N/ha)` = graspr_tmst_kgn,
         `Voerefficientie kg FPCM / kg ds voeropname`= voereff_fpcm,
         `N-benut bedrijf` = kring1_benut_tot,
         `eiwit eigen land DZK (%)` = dzh_eiwit_pcrants,
         `oppervlakte totaal (ha)` =opp_totaal,
         `oppervlakte gras (ha)` = oppgras,
         `oppervlakte mais (ha)` = oppmais,
         `melk kg/ha`  = melk_ha,
         `Kunstmest N grasland (kg N/ha)` =`graspr_kmst_kgn`,
         `organische N grasland (kg N/ha)` =`graspr_dmst_kgn`
         )%>%
  left_join(data2, by=c('bedrijfID', 'naam', 'grondsoort', 'jaar', "groep")) %>%
  right_join(Voedermiddel, by = c('bedrijfID', 'naam', 'grondsoort', 'jaar')) %>%
  mutate(`Aantalbeweiding_onbeperkt_(uur)` = `uurweido`*`dgnweido`,
         `Aantalbeweiding_beperkt_(uur)` = `uurweidb`*`dgnweidb`,
         `Aantalbeweiding_onbeperkt_zomerstal(uur)` = `uurcombio`*`dgncombib`,
         `Aantalbeweiding_beperkt_zomerstal(uur)` = `uurcombib`*`dgncombio`,
         `Totaal_beweiding(uur)` = `Aantalbeweiding_onbeperkt_(uur)`+ 
           `Aantalbeweiding_beperkt_(uur)`+ 
           `Aantalbeweiding_onbeperkt_zomerstal(uur)`+
           `Aantalbeweiding_beperkt_zomerstal(uur)`,
         `Totaal zomerstalvoedering (dagen per jaar)` = dgnzstvb + dgnzstvo,
         REgroup = case_when(`rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
                             `rantsoenRE gehalte (g/kg DS)` %in% c(156:160) ~ "RE 156-160",
                             `rantsoenRE gehalte (g/kg DS)` %in% c(161:165) ~ "RE 161-165",
                             `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"),
         REgroup = factor(REgroup, levels = c("RE > 165","RE 161-165","RE 156-160","RE < 156")),
         intensiteitcat = case_when(between(intensiteit,-Inf,14000) ~ "< 14000 kg/ha",
                                    between(intensiteit,14000,20000) ~ "14000 - 20000 kg/ha",
                                    between(intensiteit,20000,Inf) ~ "> 20000 kg/ha"),
         `fpcmProductie per ha` = `fpcmProductie per koe (kg)` * `gve_melkvee`/`oppervlakte totaal (ha)`,
         intensiteitcat = factor(intensiteitcat,levels = c("< 14000 kg/ha","14000 - 20000 kg/ha","> 20000 kg/ha")),
         `opname (kg DS/koe/dag)` = `opnamekoe (kg DS)` / nkoeien / 365,
         `opname (kg DS/koe)` = `opnamekoe (kg DS)` / nkoeien,
         `opname incl. jongvee (kg DS/koe)` = `opname incl. jongvee (kg DS)` / nkoeien,
           `aandeel (%)` = ifelse(is.na(`aandeel (%)`), 0, `aandeel (%)`),
           `RE gehalte (g/kg DS)` = ifelse(`aandeel (%)`==0, NA, `RE gehalte (g/kg DS)`),
           `opname (kg DS/koe/dag)`= ifelse(is.na(`opname (kg DS/koe/dag)`), 0,`opname (kg DS/koe/dag)`),
           `opname (kg DS/koe)`= ifelse(is.na(`opname (kg DS/koe)`), 0, `opname (kg DS/koe)`),
         `opname incl. jongvee (kg DS/koe)` =  ifelse(is.na(`opname incl. jongvee (kg DS/koe)`), 0, `opname incl. jongvee (kg DS/koe)`)
  )  %>%
  mutate(
    `prijsvoer (euro/kg ds)` = case_when(
      voedermiddel == "Vers gras" ~ {
        rel <- (gr_geh_ch4_0 - min(gr_geh_ch4_0, na.rm = TRUE)) / 
          (max(gr_geh_ch4_0, na.rm = TRUE) - min(gr_geh_ch4_0, na.rm = TRUE))
        rel * prijszomerstal + (1 - rel) * prijsweide
      },
      voedermiddel == "Snijmais" ~ 
        (rantsoen_eiweig_sm / 100) * prijsnijmaiseigenland + 
        (1 - rantsoen_eiweig_sm / 100) * prijsnijmaisaankoop,
      voedermiddel == "Graskuil" ~ prijsGraskuil,
      voedermiddel == "Bijproducten" ~ 
        (18.14374 + `DS gehalte (g/kg)` * -0.0007 +
           `RE gehalte (g/kg DS)` * 0.02573 +
           `VEM/kg DS` * 0.003398) / 100,
      voedermiddel == "Krachtvoer" ~ {
        ds_frac <- `DS gehalte (g/kg)` / 1000
        re_kg <- `RE gehalte (g/kg DS)` * ds_frac
        DVE_ds <- (0.5838 * re_kg + 12.559) / ds_frac
        DVE_ds * PrijskrachtvoerDVE / 1000 + 
          `VEM/kg DS` * PrijskrachtvoerVEM / 1000 + 
          Prijskrachtvoermineralen / ds_frac
      },
      voedermiddel == "Overig ruwvoer" ~ 
        `VEM/kg DS` / 1000 * PrijsperKVEMruwvoer,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    `Voerkosten per voedermiddel excl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname (kg DS/koe)`,
    `Voerkosten per voedermiddel incl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`
  ) %>%
  group_by(bedrijfID, jaar) %>%
  mutate(
    `Voerkosten  excl. jongvee (euro/koe)` = sum(`Voerkosten per voedermiddel excl. jongvee (euro/koe)`, na.rm = TRUE),
    `Voerkosten  incl. jongvee (euro/koe)` = sum(`Voerkosten per voedermiddel incl. jongvee (euro/koe)`, na.rm = TRUE),
    `Voerkosten  incl. jongvee (euro/kg melk)` = `Voerkosten  incl. jongvee (euro/koe)` / `fpcmProductie per koe (kg)`,
    `Voerkosten  excl. jongvee (euro/kg melk)` = `Voerkosten  excl. jongvee (euro/koe)` / `fpcmProductie per koe (kg)`
  ) %>%
  ungroup() %>%
  mutate(
    `Melkopbrengst (euro/koe)` = 
      ((`eiwit (%)` / 100) * Melkprijseiwit + 
         (`vet (%)` / 100) * Melkprijsvet) * (`Melk per koe (kg)`) +
      ifelse(`Totaal_beweiding(uur)` >= 720, `Melk per koe (kg)` * Weidetoeslag, 0),
    `Saldo excl. jongvee (euro/koe)` = `Melkopbrengst (euro/koe)` - `Voerkosten  excl. jongvee (euro/koe)`,
    `Saldo excl. jongvee (euro/100kg FPCM)` = `Saldo excl. jongvee (euro/koe)` / (`fpcmProductie per koe (kg)` / 100),
    `Saldo incl. jongvee (euro/koe)` = `Melkopbrengst (euro/koe)` - `Voerkosten  incl. jongvee (euro/koe)`,
    `Saldo incl. jongvee (euro/100kg FPCM)` = `Saldo incl. jongvee (euro/koe)` / (`fpcmProductie per koe (kg)` / 100)
  )%>%
    group_by(jaar) %>%
    mutate(
      min_ch4 = min(gr_geh_ch4_0[voedermiddel == "Vers gras"], na.rm = TRUE),
      max_ch4 = max(gr_geh_ch4_0[voedermiddel == "Vers gras"], na.rm = TRUE),
      rel_ch4 = if_else(voedermiddel == "Vers gras", 
                        (gr_geh_ch4_0 - min_ch4) / (max_ch4 - min_ch4), 
                        0),
      `Zomerstalvoeren (kgDS/koe)` = if_else(voedermiddel == "Vers gras", rel_ch4 * `opname (kg DS/koe)`, 0),
      `Weidegras (kgDS/koe)` = if_else(voedermiddel == "Vers gras", (1 - rel_ch4) * `opname (kg DS/koe)`, 0)
    ) %>%
    group_by(bedrijfID, jaar) %>%
    mutate(
      `Zomerstalvoeren (kgDS/koe)` = sum(`Zomerstalvoeren (kgDS/koe)`, na.rm = TRUE),
      `Weidegras (kgDS/koe)` = sum(`Weidegras (kgDS/koe)`, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(`Zomerstalvoeren (ja/Nee)` = if_else(`Zomerstalvoeren (kgDS/koe)` > 0, "Wel zomerstalvoeren", "Geen zomerstalvoeren")) %>%
    select(-min_ch4, -max_ch4, -rel_ch4) %>%
  mutate(`Zomerstalvoeren (kgDS/koe)` = ifelse(`Zomerstalvoeren (kgDS/koe)`==0, NA, `Zomerstalvoeren (kgDS/koe)`),
         `Weidegras (kgDS/koe)` = ifelse(`Weidegras (kgDS/koe)`==0, NA, `Weidegras (kgDS/koe)`))

test <- data %>% distinct(bedrijfID, grondsoort, grond_gras)

fouten1 <- read_excel("C:/Rfiles/WR/Koe en eiwit 2025/analyse fouten in K&E totaal.xlsx", sheet = "overzicht gescreende bedrijven ") %>%
  left_join(read_excel("C:/Rfiles/WR/Koe en eiwit 2025/analyse fouten in K&E totaal.xlsx", sheet = "2025") %>%
  select(voornaam, naam, jaar, `bruikbaar voor RE?`, `bruikbaar voor alle andere analyses?`),
  by=c("naam", "jaar")) %>%
  filter(!is.na(`bruikbaar voor RE?`)) %>%
  select(bedrijfID, jaar, `bruikbaar voor RE?`, `bruikbaar voor alle andere analyses?`) 

data <- data %>%
  left_join(fouten1, by = c('bedrijfID', 'jaar'))%>%
  mutate(
    `bruikbaar voor RE?` = if_else(
      is.na(`bruikbaar voor RE?`),
      "ja",
      `bruikbaar voor RE?`
    ),
    `bruikbaar voor alle andere analyses?` = if_else(
      is.na(`bruikbaar voor alle andere analyses?`),
      "ja",
      `bruikbaar voor alle andere analyses?`
    )
  ) %>%
  filter(`bruikbaar voor RE?` != "nee")

check_number <- data %>%
  filter(jaar == 2025) %>%
  distinct(bedrijfID) 

datameans <- data

# Samenvatting per jaar
jaar_summary <- data %>%
  group_by(jaar) %>%
  summarise(
    n_bedrijven = n_distinct(bedrijfID),
    mean_melk_koe = mean(`Melk per koe (kg)`, na.rm = TRUE),
    mean_eiwit = mean(`eiwit (%)`, na.rm = TRUE),
    mean_vet = mean(`vet (%)`, na.rm = TRUE),
    `mean_melkopbrengst_(euro/koe)` = mean(`Melkopbrengst (euro/koe)`, na.rm = TRUE),
    `mean_voerkosten_incl_(euro/koe)` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE),
    `mean_saldo_incl_(euro/koe)` = mean(`Saldo incl. jongvee (euro/koe)`, na.rm = TRUE),
    weidetoeslag_geslaagd = sum(ifelse(`Totaal_beweiding(uur)` >= 720, 1, 0))
  )

print(jaar_summary)

check <- data %>%
  select(jaar, bedrijfID, naam, screening) %>%
  distinct() %>%
  filter(screening != 0)

# Controleer opname en voerkosten per voedermiddel in 2024
voer_2024 <- data %>%
  group_by(voedermiddel, jaar) %>%
  summarise(
    total_opname_koe = sum(`opname incl. jongvee (kg DS/koe)`, na.rm = TRUE),
    total_voerkosten = sum(`Voerkosten per voedermiddel incl. jongvee (euro/koe)`, na.rm = TRUE)
  ) %>%
  arrange(desc(total_voerkosten))

print(voer_2024)

test <- data %>%
  group_by(jaar) %>%
  filter(voedermiddel == "Krachtvoer") %>%
  summarise(
    mean_DS = mean(`DS gehalte (g/kg)`, na.rm = TRUE),
    mean_RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE),
    mean_VEM = mean(`VEM/kg DS`, na.rm = TRUE),
    mean_opname = mean(`opname incl. jongvee (kg DS/koe)`, na.rm = TRUE)
  )

test <- data %>%
  group_by(jaar) %>%
  filter(voedermiddel == "Bijproducten") %>%
  summarise(
    mean_DS = mean(`DS gehalte (g/kg)`, na.rm = TRUE),
    mean_RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE),
    mean_VEM = mean(`VEM/kg DS`, na.rm = TRUE),
    mean_opname = mean(`opname incl. jongvee (kg DS/koe)`, na.rm = TRUE)
  )


# Optioneel: check Zomerstalvoeren vs Weidegras per jaar
zomer_weide <- data %>%
  group_by(jaar) %>%
  summarise(
    totaal_zomerstal = sum(`Zomerstalvoeren (kgDS/koe)`, na.rm = TRUE),
    totaal_weide = sum(`Weidegras (kgDS/koe)`, na.rm = TRUE)
  )

print(zomer_weide)


# Classification companies
df_sorted <- datameans %>%
  group_by(naam) %>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%
  ungroup() %>%
  mutate(naam = factor(naam, levels = unique(naam))) %>%
  distinct(naam, jaar, .keep_all = TRUE)

countname <- df_sorted %>% count(naam, name = "Ntotal")

# Stable LOW: only RE <156 or RE 156-160 in ALL years
Stabiel_laag <- df_sorted %>%
  group_by(naam) %>%
  filter(
    all(REgroup %in% c("RE < 156", "RE 156-160")) &
      sum(REgroup == "RE 156-160") <= 1
  ) %>%
  ungroup() %>%
  distinct(naam) %>%
  mutate(Typebedrijf = "Stabiel laag")

# Stable HIGH: only RE 161-165 or RE >165 in ALL years
Stabiel_hoog <- df_sorted %>%
  group_by(naam) %>%
  filter(
    all(REgroup %in% c("RE 161-165", "RE > 165"))
  ) %>%
  ungroup() %>%
  distinct(naam) %>%
  mutate(Typebedrijf = "Stabiel hoog")

# Combine stable farms
Stabiel <- bind_rows(Stabiel_laag, Stabiel_hoog)

# Movers (Bewegers)
Bewegers <- df_sorted %>%
  anti_join(
    bind_rows(Stabiel_laag, Stabiel_hoog),
    by = "naam"
  ) %>%
  group_by(naam) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  transmute(
    naam,
    Typebedrijf = case_when(
      `rantsoenRE gehalte (g/kg DS)` <= 155 ~ "Doel gehaald",
      `rantsoenRE gehalte (g/kg DS)` > 155  ~ "Doel niet gehaald"
    )
  )

# Final dataset with classification
datameans1 <- bind_rows(Stabiel, Bewegers) %>%
  distinct(naam, .keep_all = TRUE) %>%  # Ensure 1 row per naam
  right_join(datameans, by = "naam") %>%
  filter(!is.na(Typebedrijf))


write_xlsx(datameans1, path = "C:/Rfiles/WR/Koe en eiwit 2025/datameans3.xlsx")
write_xlsx(datameans1, path = "C:/Rfiles/WR/Koe en eiwit 2025/Statestiek team documenten/total_dataproject.xlsx")

#old classification############################################################################################################
# Classification companies
df_sorted <- datameans %>%
  group_by(naam) %>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%
  ungroup() %>%
  mutate(naam = factor(naam, levels = unique(naam))) %>%
  distinct(naam, jaar, .keep_all = TRUE)

countname <- df_sorted %>% count(naam, name = "Ntotal")

# Stable LOW: only RE <156 or RE 156-160 in ALL years
Stabiel_laag <- df_sorted %>%
  group_by(naam) %>%
  filter(all(REgroup %in% c("RE < 156", "RE 156-160"))) %>%
  ungroup() %>%
  distinct(naam) %>%
  mutate(Typebedrijf = "Stabiel laag")

# Stable HIGH: only RE 161-165 or RE >165 in ALL years
Stabiel_hoog <- df_sorted %>%
  group_by(naam) %>%
  filter(all(REgroup %in% c("RE 161-165", "RE > 165"))) %>%
  ungroup() %>%
  distinct(naam) %>%
  mutate(Typebedrijf = "Stabiel hoog")

# Combine stable farms
Stabiel <- bind_rows(Stabiel_laag, Stabiel_hoog)

# Movers (Bewegers)
Bewegers <- df_sorted %>%
  anti_join(Stabiel, by = "naam") %>%
  group_by(naam) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  transmute(naam, Typebedrijf = case_when(
    `rantsoenRE gehalte (g/kg DS)` <= 155 ~ "Doel gehaald",
    `rantsoenRE gehalte (g/kg DS)` > 155  ~ "Doel niet gehaald"
  ))

# Final dataset with classification
datameans1 <- bind_rows(Stabiel, Bewegers) %>%
  distinct(naam, .keep_all = TRUE) %>%  # Ensure 1 row per naam
  right_join(datameans, by = "naam") %>%
  filter(!is.na(Typebedrijf))

write_xlsx(datameans1, path = "C:/Rfiles/WR/Koe en eiwit 2025/datameans2.xlsx")
write_xlsx(datameans1, path = "C:/Rfiles/WR/Koe en eiwit 2025/Statestiek team documenten/total_dataproject.xlsx")

C:\Rfiles\WR\Koe en eiwit 2025\Statestiek team documenten

#RE grafieken##############################################################back#########################################################################
RE <- datameans1 %>%
  select(jaar, naam, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, `rantsoenREkVEM (g/kVEM)`)%>%
  distinct()%>%
  group_by(jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm=TRUE),
            `rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm=TRUE))%>%
  pivot_longer(
    cols = c(`rantsoenRE gehalte (g/kg DS)`, `rantsoenREkVEM (g/kVEM)`),
    names_to = "variable",
    values_to = "value"
  )

# Calculate a scale factor
scale_factor <- max(RE$value[RE$variable == "rantsoenRE gehalte (g/kg DS)"], na.rm = TRUE) / 
  max(RE$value[RE$variable == "rantsoenREkVEM (g/kVEM)"], na.rm = TRUE)

p <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  annotate("text", x = 2022, y = max(RE$value), 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0.75, color = "gray60", size=3.5) +
  coord_cartesian(ylim = c(152, max(RE$value)))+
  geom_line(data = RE %>% filter(variable == "rantsoenRE gehalte (g/kg DS)"),
            aes(x = jaar, y = value, color = "RE gehalte"), size=1) +
  geom_line(data = RE %>% filter(variable == "rantsoenREkVEM (g/kVEM)"),
            aes(x = jaar, y = value * scale_factor, color = "RE/kVEM"), size=1) +
  scale_y_continuous(
    name = "Ruw eiwit gehalte (g/kg DS)",
    sec.axis = sec_axis(~./scale_factor, name = "Ruw eiwit per kVEM (g/kVEM)")
  ) +
  scale_color_manual(values = c("RE gehalte" =  "#1c6c30", "RE/kVEM" = "#e29f02")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black", size=3.5)

ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final_report/RE vs REkVEM.jpg"),
  plot = p,
  width = 5, height = 3.5
)

#Typebedrijven
RE <- datameans1 %>%
  select(jaar, naam, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort, `rantsoenREkVEM (g/kVEM)`, `rantsoenVEM/kg DS`)%>%
  distinct()%>%
  group_by(jaar, grondsoort) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm=TRUE),
            `rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm=TRUE),
            `rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm=TRUE))


p1 <- ggplot(RE, aes(x=jaar, y=`rantsoenRE gehalte (g/kg DS)`, color=grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  annotate("text", x = 2022, y = max(RE$`rantsoenRE gehalte (g/kg DS)`*0.99), 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black")+
  ylab("Ruw eiwitgehalte (g RE/kg ds)")

p2 <- ggplot(RE, aes(x=jaar, y= `rantsoenREkVEM (g/kVEM)`, color=grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  ylab("Ruw eiwitgehalte per kVEM (g/kVEM)")

p3 <- ggplot(RE, aes(x=jaar, y= `rantsoenVEM/kg DS`, color=grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  ylab("VEM (VEM/kg DS)")

p <- ggarrange(
  plotlist = list(p1, p2, p3),
  ncol = 3,
  widths = c(1, 1, 1),
  common.legend = TRUE,
  legend = "bottom"   # optional: to place the common legend at the bottom
)
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final_report/Lijnen rantsoen veranderingen grondsoort.jpg"),
  plot = p,
  width = 10, height = 3.5
)

#Typebedrijven
RE <- datameans1 %>%
  select(jaar, naam, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, Typebedrijf, `rantsoenREkVEM (g/kVEM)`, `rantsoenVEM/kg DS`)%>%
  distinct()%>%
  group_by(jaar, Typebedrijf) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm=TRUE),
            `rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm=TRUE),
            `rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm=TRUE)) %>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"),
                              labels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Hoog")))


p1 <- ggplot(datameans1 %>% 
               select(jaar, naam, bedrijfID, `fpcmProductie per koe (kg)`, Typebedrijf) %>%
               distinct() %>%
               group_by(jaar, Typebedrijf) %>%
               summarise(`fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm=TRUE)) %>%
               mutate(Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))), 
             aes(x=jaar, y=`fpcmProductie per koe (kg)`, color=Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  annotate("text", x = 2022, y = 10200, 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0.1, color = "gray60") +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Stabiel hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))

p1 <- ggplot(RE, aes(x=jaar, y=`rantsoenRE gehalte (g/kg DS)`, color=Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  annotate("text", x = 2022, y = max(RE$`rantsoenRE gehalte (g/kg DS)`*0.99), 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0.1, color = "gray60") +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black")+
  ylab("RE gehalte rantsoen (g/kgDS)")

p2 <- ggplot(RE, aes(x=jaar, y= `rantsoenREkVEM (g/kVEM)`, color=Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Stabiel hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  ylab("Ruw eiwitgehalte per kVEM (g/kVEM)")

p3 <- ggplot(RE, aes(x=jaar, y= `rantsoenVEM/kg DS`, color=Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Stabiel hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color="Black"),
        axis.line.y.left = element_line(color="Black"))+
  ylab("VEM (VEM/kg DS)")

p <- ggarrange(
  plotlist = list(p1),
  #ncol = 3,
  #widths = c(1, 1, 1),
  common.legend = TRUE,
  legend = "bottom"   # optional: to place the common legend at the bottom
)
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final_report/Lijnen RE rantsoen Typebedrijf.jpg"),
  plot = p,
  width = 5.6, height = 4.5
)


ggplot(RE, aes(x=jaar, y=`rantsoenRE gehalte (g/kg DS)`, color=grondsoort)) +
  coord_cartesian(ylim = c(150, max(RE$`rantsoenRE gehalte (g/kg DS)`)))+
  geom_line(size=1) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02")) +
  theme_minimal() +
  xlab("Jaar") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  annotate("text", x = 2022, y = max(RE$`rantsoenRE gehalte (g/kg DS)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black")



RE <- datameans1 %>%
  select(jaar, naam, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort)%>%
  distinct() %>%
  pivot_wider(names_from=jaar, values_from = `rantsoenRE gehalte (g/kg DS)`)

write_xlsx(RE, "C:/Rfiles/WR/Koe en eiwit 2025/Graphs/heatplotsgroups/RErantsoen_2020_2024.xlsx")


p1 <- ggplot(datameans1 %>% 
         filter(jaar == 2024) %>%
         distinct(bedrijfID, `rantsoenRE gehalte (g/kg DS)`, intensiteit, grondsoort),
       aes(x=intensiteit, y=`rantsoenRE gehalte (g/kg DS)`, color=grondsoort, shape=grondsoort)) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Dark gray", size = 1) +
  geom_point(stroke = 1.5)+
  theme_minimal() +
  xlab("intensiteit (kg melk/ha)")  +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02"))+
  ylab("Ruw eiwitgehalte rantsoen\n(g RE/kg ds)")+
  scale_shape_manual(
    values = c(
      "Klei" = 16,   # rondje
      "Veen" = 3,   # driehoek
      "Zand" = 16    # rondje
    )
  )

ggplot(datameans1 %>% 
         filter(voedermiddel == "Snijmais"),
       aes(y = `aandeel (%)`, 
           x = `rantsoenRE gehalte (g/kg DS)`, 
          color = `oppervlakte mais (ha)` / `oppervlakte totaal (ha)` * 100)) +
  geom_point(size = 1.5, stroke = 1.2) +
  #geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  scale_color_gradientn(
    colors = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9",
               "#FDAE61", "#F46D43", "#D73027", "black"),
    #name = "grasoppervlakte\n(% van totaal)"
    name = "Mais oppervlakte (%)"
    #name = "aandeel"
  ) +
  theme_bw(base_size = 14) +
  #ylab("RE gehalte graskuil") +
  xlab("Aandeel snijmais (%)") +
  #xlab("Maisoppervlakte\n(% van totaal)") +
  ylab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  facet_wrap(.~jaar, nrow=1)


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/snijmaisaandeel vs rantsoen.jpg", plot = p1, width =10, height = 5, dpi = 300)


#RE grafiek punten wolk#################################################################################
# helper function to reduce repetition
library(dplyr)
library(tidyr)

bedrijven_both_years <- datameans1 %>%
  filter(jaar %in% c(2020, 2024)) %>%
  distinct(bedrijfID, jaar) %>%
  group_by(bedrijfID) %>%
  summarise(n_years = n_distinct(jaar), .groups = "drop") %>%
  filter(n_years == 2) %>%
  pull(bedrijfID)

# 👉 Dan filter je je hoofddata op alleen die bedrijven
test2 <- datameans1 %>%
  filter(bedrijfID %in% bedrijven_both_years) %>%
  filter(voedermiddel != "Melkproducten") %>%
  filter(jaar %in% c(2020, 2024)) %>%
  mutate(
    #Project = ifelse(jaar %in% 2020:2022, "Voor Koe & Eiwit", "Na Koe & Eiwit"),
    Project = ifelse(jaar == 2020, "Voor Koe & Eiwit", "Na Koe & Eiwit"),
         `RE gehalte (g/kg DS)` = replace_na(`RE gehalte (g/kg DS)`, 0),
         `Ruw eiwit g/kgDS` = (`aandeel (%)`/100) * `RE gehalte (g/kg DS)`) %>%
  group_by(Project, bedrijfID, voedermiddel, grondsoort, Typebedrijf, naam) %>%
  summarise(`Ruw eiwit g/kgDS` = mean(`Ruw eiwit g/kgDS`, na.rm=TRUE),
            `RE gehalte (g/kg DS)` = mean(`RE gehalte (g/kg DS)`, na.rm=TRUE),
            `aandeel (%)` = mean(`aandeel (%)`, na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(
    cols = c(`Ruw eiwit g/kgDS`, `RE gehalte (g/kg DS)`, `aandeel (%)`),
    names_to = "Level",
    values_to = "Value") %>%
  pivot_wider(
    names_from = Project,
    values_from = Value) %>%
  mutate(`Verschil (Na - voor)` = `Na Koe & Eiwit` - `Voor Koe & Eiwit`,
         voedermiddel = factor(voedermiddel, levels = c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil","Vers gras")),
         Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag","Doel gehaald","Doel niet gehaald","Stabiel hoog")),
         Level = factor(Level, levels = c("Ruw eiwit g/kgDS","RE gehalte (g/kg DS)","aandeel (%)"))) 


n_bedrijven <- length(unique(bedrijven_both_years))

voedermiddel_bijdrage <- test2 %>%
  filter(Level == "Ruw eiwit g/kgDS") %>%
  group_by(voedermiddel) %>%
  summarise(
    gemiddelde_bijdrage = sum(`Verschil (Na - voor)`, na.rm = TRUE) / n_bedrijven,
    .groups = "drop"
  )

som_bijdragen <- sum(voedermiddel_bijdrage$gemiddelde_bijdrage, na.rm = TRUE)
som_bijdragen

dataforPaul <- test2%>%
  filter(Level == "Ruw eiwit g/kgDS") %>%
  group_by(voedermiddel) %>%
  summarise(`Verschil (Na - voor)` = mean(`Verschil (Na - voor)`, na.rm = TRUE), .groups = "drop")%>%
  ungroup() %>%
  summarise(`Verschil (Na - voor)` = sum(`Verschil (Na - voor)`, na.rm = TRUE), .groups = "drop")

check_diff <- datameans1 %>%
  filter(jaar %in% c(2020, 2024)) %>%
  filter(bedrijfID %in% bedrijven_both_years) %>%
  distinct(`rantsoenRE gehalte (g/kg DS)`, .keep_all = TRUE) %>%
  select(bedrijfID, jaar, `rantsoenRE gehalte (g/kg DS)`) %>%
  pivot_wider(
    names_from = jaar,
    values_from = `rantsoenRE gehalte (g/kg DS)`
  ) %>%
  mutate(verschil = `2024` - `2020`) %>%
  summarise(gemiddeld_verschil = mean(verschil, na.rm = TRUE))

check_diff
   

#extra_row <- test[1, ]  # copy structure
#extra_row[1, ] <- NA    # set all values to NA
#extra_row$voedermiddel <- ""  # ensure voedermiddel is NA

# Bind it at the top
#test <- bind_rows(extra_row, test) %>%
#mutate(voedermiddel = factor(voedermiddel,  levels = c("Bijproducten","Krachtvoer","Overig ruwvoer", "Snijmais", "Graskuil","Vers gras", ""))) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Function to build one plot for a given Level
make_plot <- function(data, level_name, ylab) {
  
  # calculate symmetric y-limits per level
  y_abs_max <- data %>%
    filter(Level == level_name) %>%
    pull(`Verschil (Na - voor)`) %>%
    abs() %>%
    max(na.rm = TRUE)
  
  y_limits <- c(-y_abs_max, y_abs_max)
  
  p <- ggplot(data %>% filter(Level == level_name), 
              aes(x = voedermiddel, y = `Verschil (Na - voor)`, color = voedermiddel)) +
    geom_jitter(width = 0.25, size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.5,
      color = "black",
      fill = "white",
      stroke = 1.5
    ) +
    coord_flip() +
    ylim(y_limits) +
    theme_minimal() +
    labs(y = ylab, x = "Voedermiddel") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      panel.spacing = unit(1, "cm"),
      axis.line.x = element_line(color = "black")
    ) +
    scale_color_manual(values = c(voeders.color))
  
  # Conditional axis text
  if (level_name != "aandeel (%)") {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  p
}

# Generate all plots
plots <- list(
  make_plot(test2, "aandeel (%)", "Aandeel (%)"),
  make_plot(test2, "RE gehalte (g/kg DS)", "RE gehalte (g/kg DS)"),
  make_plot(test2, "Ruw eiwit g/kgDS", "Ruw eiwit g/kgDS")
)

# Arrange them
p1 <- ggarrange(plotlist = plots, ncol = 3, widths = c(1.4, 1, 1))
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/610/Plots_jitter_3levels.jpg"),
  plot = p1,
  width = 7.5, height = 3
)


# Function to build one plot for a given Level################################################################
make_plot <- function(data, level_name) {
  
  # calculate symmetric y-limits per level
  y_abs_max <- data %>%
    filter(Level == "Ruw eiwit g/kgDS") %>%
    pull(`Verschil (Na - voor)`) %>%
    abs() %>%
    max(na.rm = TRUE)
  
  y_limits <- c(-y_abs_max, y_abs_max)
  
  p <- ggplot(data %>% filter(Level == "Ruw eiwit g/kgDS") %>% filter(grondsoort == level_name), 
              aes(x = voedermiddel, y = `Verschil (Na - voor)`, color = voedermiddel)) +
    geom_jitter(width = 0.25, size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 2.5,
      color = "black",
      fill = "white",
      stroke = 1.5
    ) +
    coord_flip() +
    facet_wrap(.~grondsoort, scales = "free")+
    ylim(y_limits) +
    theme_minimal() +
    labs(y = ylab, x = "Voedermiddel") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      panel.spacing = unit(1, "cm"),
      axis.line.x = element_line(color = "black"),
      strip.text = element_text(size = 14, face = "bold")
    ) +
    scale_color_manual(values = c(voeders.color))+
    ylab("Ruw eiwit g/kgDS")
  # Conditional axis text
  if (level_name != "Klei") {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  p
}

# Generate all plots
plots <- list(
  make_plot(test2, "Klei"),
  make_plot(test2, "Veen"),
  make_plot(test2, "Zand")
)

# Arrange them
p1 <- ggarrange(plotlist = plots, ncol = 3, widths = c(1.4, 1, 1))
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/610/Plots_grondsoort_RE.jpg"),
  plot = p1,
  width = 7.5, height = 3
)

# Function to build one plot for a given Level#############################3
###########################################################################3

make_plot <- function(data, level_name, ylab) {
  
  # filter en eventueel groeperen voor mean per voedermiddel
  data2 <- data %>%
    filter(Level == level_name) %>%
    group_by(voedermiddel) %>%
    summarise(`Verschil (Na - voor)` = mean(`Verschil (Na - voor)`, na.rm = TRUE), .groups = "drop")
  
  # calculate symmetric y-limits per level
  y_abs_max <- max(abs(data2$`Verschil (Na - voor)`), na.rm = TRUE)
  y_limits <- c(-y_abs_max, y_abs_max)
  
  p <- ggplot(data2, aes(x = voedermiddel, y = `Verschil (Na - voor)`, color = voedermiddel)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    geom_point(      shape = 21,
                     size = 2.5,
                     color = "black",
                     fill = "white",
                     stroke = 1.5) + 
    coord_flip() +
    ylim(y_limits) +
    theme_minimal() +
    labs(y = ylab, x = "Voedermiddel") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      panel.spacing = unit(1, "cm"),
      axis.line.x = element_line(color = "black")
    ) +
    scale_color_manual(values = voeders.color)
  
  # Conditional axis text
  if (level_name != "aandeel (%)") {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  p
}

# Generate all plots
plots <- list(
  make_plot(test2, "aandeel (%)", "Aandeel (%)"),
  make_plot(test2, "RE gehalte (g/kg DS)", "RE gehalte (g/kg DS)"),
  make_plot(test2, "Ruw eiwit g/kgDS", "Ruw eiwit g/kgDS")
)

# Arrange them
p2 <- ggarrange(plotlist = plots, ncol = 3, widths = c(1.4, 1, 1))
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/610/Plots_mean_3levels.jpg"),
  plot = p2,
  width = 7.5, height = 3
)



# Function to build one plot for a given Level grondsoorten#############################3
###########################################################################3

make_plot <- function(data, level_name, ylab) {
  
  # filter en eventueel groeperen voor mean per voedermiddel
  data2 <- data %>%
    filter(Level == level_name) %>%
    select(voedermiddel, `Verschil (Na - voor)`, grondsoort) %>%
    #rbind(
      #data %>%
        #filter(Level == level_name) %>%
       # group_by(voedermiddel) %>%
        #summarise(`Verschil (Na - voor)` = mean(`Verschil (Na - voor)`, na.rm = TRUE)) %>%
        #mutate(grondsoort = "mean") ) %>%
    group_by(voedermiddel, grondsoort) %>%
    summarise(`Verschil (Na - voor)` = mean(`Verschil (Na - voor)`, na.rm = TRUE), .groups = "drop")
  
  data2$grondsoort <- factor(
    data2$grondsoort,
    levels = c("Klei", "Veen", "Zand"
               #, "mean"
               )  # gewenste volgorde
  )
  
  # calculate symmetric y-limits per level
  y_abs_max <- max(abs(data2$`Verschil (Na - voor)`), na.rm = TRUE)
  y_limits <- c(-y_abs_max, y_abs_max)
  
  p <- ggplot(data2, aes(x = voedermiddel, y = `Verschil (Na - voor)`)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    geom_point(
      data = data2 %>% filter(grondsoort %in% c("Klei", "Zand")),
      aes(color = grondsoort, shape = grondsoort),
      size = 1.5,
      stroke = 2
    ) +
    geom_point(
      data = data2 %>% filter(grondsoort == "Veen"),
      aes(color = grondsoort, shape = grondsoort),
      size = 1,    # hier kleiner
      stroke = 1.5
    ) +
    geom_point(
      data = data2 %>% filter(grondsoort == "mean"),
      aes(shape = grondsoort, color = grondsoort),
      size = 2.5,
      stroke = 1.5,
      fill = "white"
    ) +
    scale_color_manual(
      values = c(
        "Klei" = "#213b73",
        "Veen" = "#1c6c30",
        "Zand" = "#e29f02",
        "mean" = "black"
      )
    ) +
    scale_shape_manual(
      values = c(
        "Klei" = 16,
        "Veen" = 3,
        "Zand" = 16,
        "mean" = 21
      )
    ) +
    coord_flip() +
    theme_minimal() +
    ylim(y_limits) +
    labs(y = ylab, x = "Voedermiddel") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      panel.spacing = unit(1, "cm"),
      axis.line.x = element_line(color = "black")
    )
  
  # Conditional axis text
  if (level_name != "aandeel (%)") {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  p
}

# Generate all plots
plots <- list(
  make_plot(test2, "aandeel (%)", "Aandeel (%)"),
  make_plot(test2, "RE gehalte (g/kg DS)", "RE gehalte (g/kg DS)"),
  make_plot(test2, "Ruw eiwit g/kgDS", "Ruw eiwit g/kgDS")
)

# Arrange them
p2 <- ggarrange(
  plotlist = plots,
  ncol = 3,
  widths = c(1.35, 1, 1),
  common.legend = TRUE,   # ✅ gedeelde legenda
  legend = "bottom"       # ✅ onderaan plaatsen
)
ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/610/Plots_mean_3levels_grondsoorten.jpg"),
  plot = p2,
  width = 8, height = 3
)


# Loop over grondsoorten and create final ggarrange for each
grondsoorten <- unique(test2$grondsoort)

for (grond in grondsoorten) {
  
  data_grond <- test2 %>% filter(grondsoort == grond)
  
  plots <- lapply(names(levels_labels), function(lv) {
    make_plot(data_grond, lv, levels_labels[[lv]])
  })
  
  final_plot <- ggarrange(plotlist = plots, ncol = 3, widths = c(1.2, 1, 1))
  
  # Add grondsoort title
  final_plot <- annotate_figure(final_plot,
                                top = text_grob(paste("Grondsoort:", grond),
                                                face = "bold", size = 14))
  
  # Save
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/Plots_", grond, ".jpg"),
    plot = final_plot,
    width = 10, height = 4
  )
}



make_plot <- function(data, level_name, ylab) {
  
  data2 <- data %>%
    filter(Level == level_name) %>%
    group_by(voedermiddel) %>%
    summarise(
      `Na Koe & Eiwit` = mean(`Na Koe & Eiwit`, na.rm = TRUE),
      `Voor Koe & Eiwit` = mean(`Voor Koe & Eiwit`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # reshape to long for points
  data_long <- data2 %>%
    tidyr::pivot_longer(cols = c(`Na Koe & Eiwit`, `Voor Koe & Eiwit`),
                        names_to = "Project", values_to = "Value")
  
  # add segments/arrows
  arrow_data <- data2 %>%
    mutate(x = voedermiddel)
  
  p <- ggplot(data_long, aes(x = voedermiddel, y = Value)) +
    geom_segment(data = arrow_data, 
                 aes(x = x, xend = x, 
                     y = `Voor Koe & Eiwit`, yend = `Na Koe & Eiwit`),
                 arrow = arrow(length = unit(0.2,"cm")), 
                 color = "black", size = 0.8) +
    geom_point(aes(color = Project, shape = Project), size = 3) +
    coord_flip() +
    theme_minimal() +
    labs(y = ylab, x = "Voedermiddel") +
    theme(
      legend.position = "top",
      axis.title.y = element_blank(),
      panel.spacing = unit(1, "cm"),
      axis.line.x = element_line(color = "black")
    ) +
    scale_color_manual(values = c("Voor Koe & Eiwit" = "blue", "Na Koe & Eiwit" = "red"))
  
  if (level_name != "aandeel (%)") {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  return(p)
}
# Levels and labels
levels_labels <- list(
  "aandeel (%)" = "Aandeel (%)",
  "RE gehalte (g/kg DS)" = "RE gehalte (g/kg DS)",
  "Ruw eiwit g/kgDS" = "Ruw eiwit g/kgDS"
)

# Build plots for each grondsoort
grondsoorten <- unique(test2$grondsoort)

grond_plots <- lapply(grondsoorten, function(grond) {
  data_grond <- test2 %>% filter(grondsoort == grond)
  
  plots <- lapply(names(levels_labels), function(lv) {
    make_plot(data_grond, lv, levels_labels[[lv]])
  })
  
  final_plot <- ggarrange(plotlist = plots, ncol = 3, widths = c(1.2, 1, 1))
  
  annotate_figure(final_plot,
                  top = text_grob(paste("Grondsoort:", grond),
                                  face = "bold", size = 14))
})

# Stack them vertically
all_plots <- ggarrange(plotlist = grond_plots, ncol = 1)

# Save one big file
ggsave(
  filename = "C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/All_grondsoorten.jpg",
  plot = all_plots,
  width = 12, height = 4 * length(grondsoorten)  # hoogte schaalt mee met aantal grondsoorten
)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/Ruw eiwitgehalte rantsoen vs.jpg", plot = p1, width = 5, height = 4, dpi = 300)


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/Ruw eiwitgehalte rantsoen vs_ grondoosr.jpg", plot = p1, width = 10, height = 4, dpi = 300)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/Ruw eiwitgehalte rantsoen vs_Typebedrijf.jpg", plot = p1, width = 10, height = 4, dpi = 300)

#verdeling RE over verschillende groepen
# Basis dataset en kwartielen
test_long <- datameans1 %>%
  filter(jaar == 2024) %>%
  group_by(jaar, bedrijfID) %>%
  mutate(`Vers gras aandeel (%)` = mean(`aandeel (%)`[voedermiddel == "Vers gras"], na.rm = TRUE)) %>%
  select(bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort, `Vers gras aandeel (%)`, `fpcmProductie per ha`, intensiteitcat) %>%
  distinct() %>%
  ungroup() %>%
  mutate(`Vers gras groep` = ntile(`Vers gras aandeel (%)`, 4)) %>%
  group_by(`Vers gras groep`) %>%
  mutate(label = paste0(round(min(`Vers gras aandeel (%)`),0), "–", round(max(`Vers gras aandeel (%)`),0), "%")) %>%
  ungroup() %>%
  mutate(`Vers gras groep` = label) %>%
  select(-label) %>%
  mutate(grondsoort = as.character(grondsoort), intensiteitcat = as.character(intensiteitcat)) %>%
  pivot_longer(cols = c(grondsoort, `Vers gras groep`, intensiteitcat),
               names_to = "factor_type", values_to = "factor_value") %>%
  mutate(factor_type = case_when(factor_type == "intensiteitcat" ~ "Intensiteit",
                                 factor_type == "Vers gras groep" ~ "Aandeel vers gras",
                                 TRUE ~ "Grondsoort"),
         factor_type = factor(factor_type, levels=c("Grondsoort","Intensiteit","Aandeel vers gras")),
         factor_value = factor(factor_value, levels=c("Klei","Veen","Zand",
                                                      "< 14000 kg/ha","14000 - 20000 kg/ha","> 20000 kg/ha",
                                                      "0–6%","6–9%","10–15%","16–31%")))

# n en % <155 per groep
summary_df <- test_long %>%
  group_by(factor_type, factor_value) %>%
  summarise(n = n(),
            pct_below_155 = round(mean(`rantsoenRE gehalte (g/kg DS)` < 155) * 100, 1),
            y_pos = max(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE) + 3,
            .groups = "drop")


p1 <- ggplot(test_long, aes(x = factor_value, y = `rantsoenRE gehalte (g/kg DS)`, color = factor_type)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  facet_wrap(~factor_type, scales = "free")+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  geom_text(
    data = summary_df, 
    aes(x = factor_value, y = 177, label = paste0("n = ", n, "\n", pct_below_155, "% <155")),
    inherit.aes = FALSE,
    size = 3.2,      # iets groter dan 3.5
    color = "gray30" 
  ) +
  scale_color_manual(values = c("Grondsoort" = "#213b73","Intensiteit" = "#e29f02","Aandeel vers gras" ="#1c6c30")) +
  ylab("Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),       # y-as titel groter
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1), # x-as labels groter
    axis.text.y = element_text(size = 11),        # y-as labels groter
    strip.text = element_text(size = 13),         # facet labels groter
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(137, 177)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/Ruw eiwitgehalte rantsoen per groep – 2024.jpg", plot = p1, width = 11.5, height = 5, dpi = 300)


# Data voorbereiden: filter jaar 2024 en maak kwartielen
df_quart <- datameans1 %>%
  filter(jaar == 2024) %>%
  distinct(jaar, bedrijfID, `Saldo incl. jongvee (euro/koe)`, `rantsoenRE gehalte (g/kg DS)`) %>%
  mutate(quartile = ntile(`rantsoenRE gehalte (g/kg DS)`, 4))

quart_means <- df_quart %>%
  group_by(quartile) %>%
  summarise(mean_RE = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE),
            mean_saldo = mean(`Saldo incl. jongvee (euro/koe)`, na.rm = TRUE),
            .groups = "drop")

p1 <- ggplot(df_quart, aes(`rantsoenRE gehalte (g/kg DS)`, `Saldo incl. jongvee (euro/koe)`, color = factor(quartile))) +
  geom_point(alpha = 0.6) +
  geom_point(data = quart_means, aes(mean_RE, mean_saldo), color = "black", size = 3.5, shape = 18, stroke = 2) +
  theme_minimal() +
  labs(color = "Quartile (RE)", x = "Rantsoen RE gehalte (g/kg DS)", y = "Saldo incl. jongvee (euro/koe)") +
  facet_grid(. ~ jaar)+
  theme(legend.position = "none")

p2 <- ggplot(df_quart, aes(`rantsoenRE gehalte (g/kg DS)`, `Saldo incl. jongvee (euro/koe)`)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(color = "Quartile (RE)", x = "Rantsoen RE gehalte (g/kg DS)", y = "Saldo incl. jongvee (euro/koe)") +
  facet_grid(. ~ jaar)

plot_grid(p2,p1)
########Barplots RE ###
graph <- datameans1 %>% 
  filter(jaar != 2024) %>%
  filter(voedermiddel != "Melkproducten") %>%
  group_by(Typebedrijf, voedermiddel) %>% 
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Rantsoen RE gehalte (g/kg DS)` = `RE gehalte (g/kg DS)` * `aandeel (%)`/100) %>%
  summarise(`Rantsoen RE gehalte (g/kg DS)` = mean(`Rantsoen RE gehalte (g/kg DS)`, na.rm = TRUE), .groups = 'drop',
            fpcmProductie = mean(`fpcmProductie per koe (kg)`, na.rm = T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)),
         Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
  )  %>%
  group_by(Typebedrijf, voedermiddel) %>%
  mutate(label = ifelse(`Rantsoen RE gehalte (g/kg DS)` > 10, round(`Rantsoen RE gehalte (g/kg DS)`), NA)) %>%
  ungroup()

totals <- graph %>%
  group_by(Typebedrijf) %>%
  summarise(totals = sum(`Rantsoen RE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  mutate(label = round(totals))

ggplot(graph, aes(x = Typebedrijf, y = `Rantsoen RE gehalte (g/kg DS)`, fill = voedermiddel)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = voeders.color) +
  #geom_line(aes(x = jaar, y = (fpcmProductie - 8000)/20), color="#BAD3B4", size=1.5) +
  #geom_point(aes(x = jaar, y = (fpcmProductie - 8000)/20), shape = 21,fill = "#1c6c30", color = "white", size = 2) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
    geom_text(data = totals, aes(x = Typebedrijf, y = totals, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  #facet_wrap(~ Typebedrijf, nrow=1) +
  #scale_x_continuous(breaks = c(2020:2024),labels = c("2020","2021","2022","2023", "2024")) +
  #scale_y_continuous(limits = c(0,220), expand = c(0,0), sec.axis = sec_axis(~. * 20 + 8000, name = "Meetmelkproductie [kg/koe/jr]")) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outside",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/RErantsoen_RE_typebedrijf.jpg", width = 7.5, height = 4, dpi = 300)

#Voerkosten#################################################################

totals <- graph %>%
  group_by(Typebedrijf) %>%
  summarise(total = sum(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p1 <- ggplot(graph, aes(x = Typebedrijf, y = `Voerkosten  incl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = Typebedrijf, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  ylab("Voerkosten  incl. jongvee (euro/koe/jaar)")+
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/Voerkosten  incl. jongvee (euro_koe).jpg", plot = p1, width = 7.5, height = 4, dpi = 300)



#graph voerkosten barplot:
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Data voorbereiden
graph_wide <- datameans1 %>%
  distinct(jaar, bedrijfID, grondsoort, `Voerkosten  excl. jongvee (euro/koe)`,`Melkopbrengst (euro/koe)`,`Saldo excl. jongvee (euro/koe)` )%>%
  filter(jaar == 2024) %>%
  group_by(grondsoort) %>%
  summarise(
    Voerkosten = mean(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE),
    Melkopbrengst = mean(`Melkopbrengst (euro/koe)`, na.rm = TRUE),
    Saldo = mean(`Saldo excl. jongvee (euro/koe)`, na.rm = TRUE),
    .groups = "drop")


# Calculate a small offset for the arrow to stop below the cross
offset <- 0.05 * (max(graph_wide$Melkopbrengst, na.rm = TRUE) - min(graph_wide$Voerkosten, na.rm = TRUE))

graph_wide <- graph_wide %>%
  mutate(grondsoort_num = as.numeric(factor(grondsoort)))

p1 <- ggplot(graph_wide, aes(x = grondsoort)) +
  geom_bar(aes(y = Voerkosten, fill = "Voerkosten"), stat = "identity", width = 0.6) +
  geom_text(aes(
    y = Voerkosten - 150,
    label = paste0("€", round(Voerkosten, 0))),
    color = "white", fontface = "bold", size = 3.5) +
  geom_point(aes(y = Melkopbrengst, shape = "Melkopbrengst"), color = "black", size = 4, stroke = 2) +
  geom_text(aes(
    x = grondsoort_num,
    y = Melkopbrengst,
    label = paste0("€", round(Melkopbrengst, 0))),
    vjust = 0.5, hjust = -0.4, fontface = "bold", size = 3.5) +
  geom_segment(aes(y = Voerkosten, yend = Melkopbrengst - offset, xend = grondsoort),
               arrow = arrow(length = unit(0.2, "cm")), color ="gray50", linewidth = 0.7) +
  geom_text(aes(
    x = grondsoort_num,
    y = (Voerkosten + Melkopbrengst)/2,
    label = paste0("Saldo (€", round(Saldo, 0), ",-)")),
    vjust = 0.5, hjust = -0.1, fontface = "bold", size = 3.5, color="gray50") +
  
  scale_fill_manual(name = "", values = c("Voerkosten" = "#1c6c30"), labels =c("Voerkosten  excl. jongvee (euro/koe)")) +
  scale_shape_manual(name = "", values = c("Melkopbrengst" = 4)) +  # shape 4 is a cross
  labs(y = "euro/koe", x = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.placement = "outer",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 13),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=13)
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/saldomelkopbrengste.jpg", plot = p1, width = 7, height = 4.5, dpi = 300)

########Voerkosten Barplots exl per koe#
graph <- datameans1 %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten  excl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname (kg DS/koe)`,
         `Voerkosten  excl. jongvee (euro/koe)` = ifelse(is.na(`Voerkosten  excl. jongvee (euro/koe)`), 0, `Voerkosten  excl. jongvee (euro/koe)`)) %>%
  group_by(Typebedrijf, voedermiddel) %>%
  summarise(`Voerkosten  excl. jongvee (euro/koe)` = mean(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten  excl. jongvee (euro/koe)` > 70, paste0("€", round(`Voerkosten  excl. jongvee (euro/koe)`)), NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)))

totals <- graph %>%
  group_by(Typebedrijf) %>%
  summarise(total = sum(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p1 <- ggplot(graph, aes(x = Typebedrijf, y = `Voerkosten  excl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = Typebedrijf, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  ylab("Voerkosten  excl. jongvee (euro/koe/jaar)")+
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/ArtikelTypebedrijfen/voerskosten_excl.jpg", plot = p1, width = 6, height = 4, dpi = 300)

########Voerkosten Barplots incl per koe
graph <- datameans1 %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten  incl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`,
         `Voerkosten  incl. jongvee (euro/koe)` = ifelse(is.na(`Voerkosten  incl. jongvee (euro/koe)`), 0, `Voerkosten  incl. jongvee (euro/koe)`)) %>%
  group_by(Typebedrijf, voedermiddel) %>%
  summarise(`Voerkosten  incl. jongvee (euro/koe)` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten  incl. jongvee (euro/koe)` > 70, paste0("€", round(`Voerkosten  incl. jongvee (euro/koe)`)), NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)),
                               Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
         )

totals <- graph %>%
  group_by(Typebedrijf) %>%
  summarise(total = sum(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p1 <- ggplot(graph, aes(x = Typebedrijf, y = `Voerkosten  incl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = Typebedrijf, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  ylab("Voerkosten  incl. jongvee (euro/koe/jaar)")+
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/Voerkosten  incl. jongvee (euro_koe).jpg", plot = p1, width = 7.5, height = 4, dpi = 300)

########Voerkosten Barplots incl per koe
graph <- datameans1 %>% 
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten  incl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`,
         `Voerkosten  incl. jongvee (euro/koe)` = ifelse(is.na(`Voerkosten  incl. jongvee (euro/koe)`), 0, `Voerkosten  incl. jongvee (euro/koe)`)) %>%
  group_by(jaar, voedermiddel) %>%
  summarise(`Voerkosten  incl. jongvee (euro/koe)` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten  incl. jongvee (euro/koe)` > 70, paste0("€", round(`Voerkosten  incl. jongvee (euro/koe)`)), NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot))
  )

totals <- graph %>%
  group_by(jaar) %>%
  summarise(total = sum(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p1 <- ggplot(graph, aes(x = jaar, y = `Voerkosten  incl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = jaar, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  ylab("Voerkosten  incl. jongvee (euro/koe/jaar)")+
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/Voerkosten  incl. jongvee (euro_koe).jpg", plot = p1, width = 7.5, height = 4, dpi = 300)

########Voerkosten Barplots exl per melk
graph <- datameans %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten excl. jongvee (euro/100 kg melk)` = 
           (`prijsvoer (euro/kg ds)` * `opname (kg DS/koe)`) / `fpcmProductie per koe (kg)` * 100) %>%
  group_by(grondsoort, voedermiddel) %>%
  summarise(`Voerkosten excl. jongvee (euro/100 kg melk)` = mean(`Voerkosten excl. jongvee (euro/100 kg melk)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten excl. jongvee (euro/100 kg melk)` > 0.7, 
                        paste0("€", format(round(`Voerkosten excl. jongvee (euro/100 kg melk)`, 2), nsmall = 2)), 
                        NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)))

totals <- graph %>%
  group_by(grondsoort) %>%
  summarise(total = sum(`Voerkosten excl. jongvee (euro/100 kg melk)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", format(round(total, 2), nsmall = 2)))

p1<-ggplot(graph, aes(x = grondsoort, y = `Voerkosten excl. jongvee (euro/100 kg melk)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = grondsoort, y = total, label = label), 
            vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/voerskosten_melkperkoe2.jpg", plot = p1, width = 6, height = 4, dpi = 300)

########Voerkosten Barplots incl per melk
graph <- datameans1 %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten incl. jongvee (euro/100 kg melk)` = 
           (`prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`) / `fpcmProductie per koe (kg)` * 100) %>%
  group_by(Typebedrijf, voedermiddel) %>%
  summarise(`Voerkosten incl. jongvee (euro/100 kg melk)` = mean(`Voerkosten incl. jongvee (euro/100 kg melk)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten incl. jongvee (euro/100 kg melk)` > 0.8, 
                        paste0("€", format(round(`Voerkosten incl. jongvee (euro/100 kg melk)`, 1), nsmall = 2)), 
                        NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)),
         Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
         )

totals <- graph %>%
  group_by(Typebedrijf) %>%
  summarise(total = sum(`Voerkosten incl. jongvee (euro/100 kg melk)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", format(round(total, 2), nsmall = 2)))

p1<-ggplot(graph, aes(x = Typebedrijf, y = `Voerkosten incl. jongvee (euro/100 kg melk)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = Typebedrijf, y = total, label = label), 
            vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/Voerkosten incl. jongvee (euro_100 kg melk).jpg", plot = p1, width = 7.5, height = 4, dpi = 300)

#Further analyzes#
test <- datameans1 %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, `Melkopbrengst (euro/koe)`, `Voerkosten  excl. jongvee (euro/koe)`, `Saldo excl. jongvee (euro/koe)`, grondsoort) %>%
  distinct() %>%
  group_by(grondsoort) %>%
  summarise(
    `Voerkosten  excl. jongvee` = mean(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE),
    `Melkopbrengst` = mean(`Melkopbrengst (euro/koe)`, na.rm = TRUE),
    `Saldo excl. jongvee` = mean(`Saldo excl. jongvee (euro/koe)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -grondsoort, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("Voerkosten  excl. jongvee", "Melkopbrengst", "Saldo excl. jongvee")))

my_colors <- c("#213b73", "#1c6c30", "#e29f02")

p1 <- ggplot(test, aes(x = grondsoort, y = Value, fill = grondsoort)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Variable) +
  geom_text(aes(label = paste0("€", round(Value, 0))),  
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5, color = "black", fontface = "bold", na.rm = TRUE) +
  scale_fill_manual(values = my_colors) +
  labs(x = NULL, y = "Euro/koe/jaar") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/bar_saldomelkvoer.jpg", plot = p1, width = 7, height = 4, dpi = 300)

#barplot opbrengste, saldo, kosten incl jongvee per koe
test <- datameans1 %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, `Melkopbrengst (euro/koe)`, `Voerkosten  incl. jongvee (euro/koe)`, `Saldo incl. jongvee (euro/koe)`, Typebedrijf) %>%
  distinct() %>%
  group_by(Typebedrijf) %>%
  summarise(
    `Voerkosten  incl. jongvee` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE),
    `Melkopbrengst` = mean(`Melkopbrengst (euro/koe)`, na.rm = TRUE),
    `Saldo incl. jongvee` = mean(`Saldo incl. jongvee (euro/koe)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -Typebedrijf, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("Voerkosten  incl. jongvee", "Melkopbrengst", "Saldo incl. jongvee")),
         Typebedrijf = factor(Typebedrijf, levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
  )


p1 <- ggplot(test, aes(x = Typebedrijf, y = Value, fill = Typebedrijf)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Variable) +
  geom_text(aes(label = paste0("€", round(Value, 0))),  
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5, color = "black", fontface = "bold", na.rm = TRUE) +
  scale_fill_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  labs(x = NULL, y = "Euro/koe/jaar") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.placement = "outside",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/bar_saldomelkvoer_incl.jpg", plot = p1, width = 7, height = 4, dpi = 300)

#barplot opbrengste, saldo, kosten incl jongvee per melk
test <- data %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, `Melkopbrengst (euro/koe)`, `Voerkosten  incl. jongvee (euro/koe)`, `Saldo excl. jongvee (euro/koe)`, grondsoort) %>%
  distinct() %>%
  pivot_longer(cols = -c(bedrijfID, grondsoort), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c(
    "Melkopbrengst (euro/koe)",
    "Voerkosten  excl. jongvee (euro/koe)",
    "Saldo excl. jongvee (euro/koe)"
  )))

ggplot(test, aes(x = grondsoort, y = Value, color = Variable)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "black", color = "black", size = 3, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), vjust = -1.5, color = "black", size = 4, show.legend = FALSE, fontface = "bold") +
  facet_wrap(~Variable, nrow = 1) +
  labs(y = "euro/koe", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

test <- data %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, `Melkopbrengst (euro/koe)`, `Voerkosten  excl. jongvee (euro/koe)`, `Saldo excl. jongvee (euro/koe)`, grondsoort) %>%
  distinct() %>%
  pivot_longer(cols = -c(bedrijfID, grondsoort), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c(
    "Melkopbrengst (euro/koe)",
    "Voerkosten  excl. jongvee (euro/koe)",
    "Saldo excl. jongvee (euro/koe)"
  )))

ggplot(test, aes(x = Variable, y = Value, color = grondsoort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "black", color = "black", size = 3, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), vjust = -1.5, color = "black", size = 4, show.legend = FALSE, fontface = "bold") +
  facet_wrap(~grondsoort, nrow = 3) +
  labs(y = "euro/koe", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip()


p2<- ggplot(data %>% filter(jaar == "2024") %>% distinct(jaar, bedrijfID, .keep_all=TRUE), aes(x = "2024", y = `Saldo (euro/100kg FPCM)`, color = `Saldo (euro/100kg FPCM)`)) +
  geom_boxplot(outliers = FALSE, color="Gray") +
  geom_jitter(width = 0.2, alpha=0.6, color="Gray")+ 
  labs(title = "Saldo per FPCM bedrijfID in 2024", y='euro/100 kg FPCM') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank())+
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "black", color = "black", size = 3, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -1.5, color = "black", size = 4, show.legend = FALSE, fontface="bold") 
plot_grid(p1, p2, rel_widths = c(1.5, 1))

ggplot(data %>% filter(jaar == "2024") %>% filter(voedermiddel != "Melkproducten") %>% distinct(bedrijfID, voedermiddel, .keep_all=TRUE), 
       aes(x=factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")),
           y=`prijsvoer (euro/kg ds)`, color = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 3)), vjust = -5, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "black", size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.position = "none")





graph <- datameans %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten  excl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname (kg DS/koe)`) %>%
  group_by(REgroup, voedermiddel) %>%
  summarise(`Voerkosten  excl. jongvee (euro/koe)` = mean(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten  excl. jongvee (euro/koe)` > 100, paste0("€", round(`Voerkosten  excl. jongvee (euro/koe)`)), NA),
         REgroup = factor(REgroup, levels=c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)))

totals <- graph %>%
  group_by(REgroup) %>%
  summarise(total = sum(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p2 <- ggplot(graph, aes(x = REgroup, y = `Voerkosten  excl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = REgroup, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 8),
    strip.text = element_text(size = 8, color = "black"),
    axis.title.x = element_blank()
  )


correlationmatix <- datameans %>%
  filter(voedermiddel == "Snijmais") %>%
  filter(jaar == "2024")%>%
  mutate(`Voerkosten  excl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname (kg DS/koe)`)%>%
  group_by(bedrijfID)%>%
  mutate(`Voerkosten  excl. jongvee (euro/koe)` = sum(`Voerkosten  excl. jongvee (euro/koe)`, na.rm = TRUE))%>%
  ungroup()%>%
    select(`Voerkosten  excl. jongvee (euro/koe)`,
           `rantsoenRE gehalte (g/kg DS)`,
           `Saldo excl. jongvee (euro/koe)`,
           `Saldo (euro/100kg FPCM)`,
           `prijsvoer (euro/kg ds)`,
           `opname (kg DS/koe)`,
           voedermiddel)%>%
    pivot_wider(
      names_from = voedermiddel,
      values_from = c(`prijsvoer (euro/kg ds)`, `opname (kg DS/koe)`),
      names_glue = "{voedermiddel}_{.value}"
    )%>%
  select(where(is.numeric))%>%
  mutate(across(everything(), ~ifelse(is.finite(.), ., 0)))

summary(correlationmatix)
# Calculate correlation matrix with pairwise deletion
cor_matrix <- cor(correlationmatix, use = "pairwise.complete.obs")
heatmap(cor_matrix, main = "Correlation Matrix")
chart.Correlation(correlationmatix, histogram = TRUE, pch = 19)


ggcorrplot(cor_matrix,
           method = "circle",        # dots/circles, not tiles
           type = "lower",           # lower triangle only
           lab = FALSE,              # no numbers in circles
           tl.cex = 8,               # smaller labels (try 6-8)
           tl.srt = 45,              # tilt labels 45 degrees
           colors = c("blue", "white", "red"),
           title = "Correlation plot (dots) of livestock feed data (2024)",
           ggtheme = ggplot2::theme_minimal())


ggplot(datameans %>% filter(jaar == "2024"), aes(x = (`opnamekoe (kg DS)`)/nkoeien, y = `opname incl. jongvee (kg DS)`/nkoeien)) +
  geom_point() +
  labs(
    title = "DS Opname per voedermiddel (met vs. zonder jongvee)"
  ) +
  facet_wrap(.~ voedermiddel, scales="free", nrow=2)+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) 

    S
#Beweiding dataset###########################################################################################
beweiding <- datameans%>%
  select(
    jaar, bedrijfID,
    matches("Aantalbeweiding.*\\(uur\\)"),
    matches("^uur"),
    matches("^dgn"),
    matches("^vgn"),
    starts_with("pcuren"),
    `Zomerstalvoeren (kgDS/koe)`,
    `Zomerstalvoeren (ja/Nee)`,
    `Weidegras (kgDS/koe)`
  ) %>%
  distinct() %>%
  filter(jaar == 2024) %>%
  rename(
    `Aandeel uren stal, melkvee` = pcuren_mlk_stal,
    `Aandeel uren wei, melkvee` = pcuren_mlk_wei,
    `Aandeel uren stal, overige graasdieren` = pcuren_ovg_stal,
    `Aandeel uren wei, overige graasdieren` = pcuren_ovg_wei,
    
    `Beweiding koeien, dagen beperkt weiden` = dgnweidb,
    `Beweiding koeien, dagen onbeperkt weiden` = dgnweido,
    `Beweiding koeien, dagen beperkt weiden met zomerstalvoedering` = dgncombib,
    `Beweiding koeien, dagen onbeperkt weiden met zomerstalvoedering` = dgncombio,
    `Beweiding koeien, dagen beperkt zomerstalvoedering` = dgnzstvb,
    `Beweiding koeien, dagen onbeperkt zomerstalvoedering` = dgnzstvo,
    `Totaal zomerstalvoedering (dagen per jaar)` = `Totaal zomerstalvoedering (dagen per jaar)`,
    `Beweiding pinken, aantal dagen` = dgnweidpi,
    `Beweiding kalveren, aantal dagen` = dgnweidka,
    
    `Beweiding koeien, uren/dag beperkt weiden` = uurweidb,
    `Beweiding koeien, uren/dag onbeperkt weiden` = uurweido,
    `Beweiding koeien, uren/dag beperkt weiden met zomerstalvoedering` = uurcombib,
    `Beweiding koeien, uren/dag onbeperkt weiden met zomerstalvoedering` = uurcombio,
    
    `Beweiding koeien, beperkt weiden, aandeel natuurgras (%)` = vgnweidb,
    `Beweiding koeien, onbeperkt weiden, aandeel natuurgras (%)` = vgnweido,
    `Beweiding koeien, onbeperkt weiden met zomerstalvoedering, aandeel natuurgras (%)` = vgncombib,
    `Beweiding koeien, beperkt weiden met zomerstalvoedering, aandeel natuurgras (%)` = vgncombio,
    `Beweiding koeien, beperkt zomerstalvoedering, aandeel natuurgras (%)` = vgnzstvb,
    `Beweiding koeien, onbeperkt zomerstalvoedering, aandeel natuurgras (%)` = vgnzstvo,
    `Pinken: grasopname aandeel natuurgras (%)` = vgnweidpi,
    `Kalveren: grasopname aandeel natuurgras (%)` = vgnweidka,
    `Overige graasdieren: grasopname aandeel natuurgras (%)` = vgnweidovg
  )



#----------------------------------RE K & E vs Nederland-----------------------------------------------------
data2 <- data.frame(
  Jaar = c(2020, 2021, 2022, 2023, 2024),
  Waarde1 = c(166, 162, 159, 158, 156),
  Waarde2 = c(167, 161, 161, 163, 161)
)%>%
  pivot_longer(cols = -Jaar, names_to = "Categorie", values_to = "Waarde") %>%
  mutate(Categorie = ifelse(Categorie == "Waarde1", "Koe en Eiwit-deelnemers", "Melkveehouders NL"))

# Plot de gegevens
p1 <- ggplot(data2, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  geom_line(size = 1.2) +  # Lijn dikte
  #annotate("text", x = 2022, y = max(data$Waarde), label = "Startjaar", vjust = -3, hjust=-0.2, color = "gray") +
  labs(title = "Eiwitgehalte rantsoen Koe & Eiwit daalt t.o.v. NL-gemiddelde
       ",
       x = "Jaar",
       y = "Ruw eiwitgehalte rantsoen
  (g RE/kg ds)",
       color = "Legenda",
       text = element_text(family = "Dosis")) +
  theme_minimal()+
  scale_color_manual(values = c("#1A6B04", "#E29E00")) +  # Kleuren handmatig instellen
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_text(size =8),
        axis.text.x = element_text(size =8),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size = 8),
        plot.title = element_blank())+
  ylim(150,170)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/linegraphoveralREeiwitvsnederland.pdf", plot = p1, width = 5, height = 3.5, dpi = 300)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/linegraphoveralREeiwitvsnederland.jpg", plot = p1, width = 5, height = 3.5, dpi = 300)

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  bind_rows(data.frame(
    jaar = c(2020, 2021, 2022, 2023, 2024),
    `rantsoenRE gehalte (g/kg DS)` = c(167, 161, 161, 163, 161),
    grondsoort = "NL-gemiddelde"
  )) %>%
  mutate( `rantsoenRE gehalte (g/kg DS)` = ifelse(grondsoort == "NL-gemiddelde", rantsoenRE.gehalte..g.kg.DS., `rantsoenRE gehalte (g/kg DS)`))%>%
  mutate(alpha_group = ifelse(grondsoort == "NL-gemiddelde", 0.6, 1))%>%
  mutate(linetype_group = ifelse(grondsoort == "NL-gemiddelde", "NL-gemiddelde", "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = ifelse(grondsoort == "Klei", "Klei (n=57)", 
                                  ifelse(grondsoort == "Veen", "Veen (n=31)",
                                        ifelse(grondsoort == "Zand", "Zand (n=57)", "NL-gemiddelde"))))


labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenRE gehalte (g/kg DS)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = linetype_group),
            size = 1.2) +
  annotate("text", x = 2022, y = max(data2$`rantsoenRE gehalte (g/kg DS)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray") +
  geom_text(data = labels_df, 
            aes(jaar, `rantsoenRE gehalte (g/kg DS)`, label = grondsoortlable, color = grondsoort), 
            hjust = 0.4, vjust = 1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "NL-gemiddelde" = "gray"),
    labels = c("Klei (n=11)", "Veen (n=22)", "Zand (n=34)", "NL-gemiddelde"),
    guide = "none")+
  scale_linetype_manual(
    values = c("NL-gemiddelde" = "dashed", "Koe & Eiwit deelnemers" = "solid"),
    name = NULL ) +
  labs(y = "Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  ylim(150, 172) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  )

###n
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(data.frame(
    jaar = c(2020, 2021, 2022, 2023, 2024),
    `rantsoenRE gehalte (g/kg DS)` = c(167, 161, 161, 163, 161),
    grondsoort = "NL",
    check.names = FALSE   # <---- voorkomt dat R de naam vervormt
  )) %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "NL" ~ 0.6,
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "NL" ~ "NL-gemiddelde",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Klei",
    grondsoort == "Veen" ~ "Veen",
    grondsoort == "Zand" ~ "Zand",
    grondsoort == "NL" ~ "NL",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenRE gehalte (g/kg DS)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  annotate("text", x = 2022, y = max(data2$`rantsoenRE gehalte (g/kg DS)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_text(
    data = labels_df, 
    aes(
      x = jaar - 0.2,
      y = `rantsoenRE gehalte (g/kg DS)`,
      label = grondsoortlable,
      color = grondsoort
    ), 
    hjust = 0,          # start label nét rechts van de lijn
    vjust = 0.5,        # verticaal gecentreerd op de lijn
    size = 3.5, 
    fontface = "bold", 
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("NL" = "dashed", "Koe & Eiwit" = "dashed", "Klei"="solid", "Veen"= "solid", "Zand"="solid"),
    name = NULL, guide = "none" ) +
  labs(y = "Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  ylim(150, 172) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/linegraphRE.jpg", plot = p1, width = 6, height = 4, dpi = 300)

###VEMMM#################################################################################################################new graph hate it here !!!!
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Klei",
    grondsoort == "Veen" ~ "Veen",
    grondsoort == "Zand" ~ "Zand",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))

labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenVEM/kg DS`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  annotate("text", x = 2022, y = max(data2$`rantsoenVEM/kg DS`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_text(
    data = labels_df,
    aes(
      x = jaar - 0.2, 
      y = `rantsoenVEM/kg DS`,
      label = grondsoortlable,
      color = grondsoort
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE,
    position = position_nudge(y = c(0, -0.2,0, 0.2))  # give each line a different nudge
  )+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "solid", "Zand" = "solid"),
    name = NULL,
    guide = "none" ) +
  labs(y = "VEM/kg DS rantsoen") +
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/linegraphVEM.jpg", plot = p1, width = 6, height = 3.5, dpi = 300)

###VEMMMkg#################################################################################################################new graph hate it here !!!!
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenREkVEM (g/kVEM)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenREkVEM (g/kVEM)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Klei",
    grondsoort == "Veen" ~ "Veen",
    grondsoort == "Zand" ~ "Zand",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenREkVEM (g/kVEM)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  annotate("text", x = 2022, y = max(data2$`rantsoenREkVEM (g/kVEM)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_text(
    data = labels_df,
    aes(
      x = jaar - 0.2, 
      y = `rantsoenREkVEM (g/kVEM)`,
      label = grondsoortlable,
      color = grondsoort
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE,
    position = position_nudge(y = c(0, 0,-0.1, 0))  # give each line a different nudge
  )+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "solid", "Zand" = "solid"),
    name = NULL,
    guide = "none" ) +
  labs(y = "REkVEM (g/kVEM) rantsoen") +
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/linegraphREkVEM.jpg", plot = p1, width = 6, height = 3.5, dpi = 300)


###P-gehalte#################################################################################################################new graph hate it here !!!!
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenP gehalte (g/kg)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenP gehalte (g/kg)` = mean(`rantsoenP gehalte (g/kg)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenP gehalte (g/kg)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenP gehalte (g/kg)` = mean(`rantsoenP gehalte (g/kg)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Klei",
    grondsoort == "Veen" ~ "Veen",
    grondsoort == "Zand" ~ "Zand",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenP gehalte (g/kg)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  annotate("text", x = 2022, y = max(data2$`rantsoenP gehalte (g/kg)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_text(
    data = labels_df,
    aes(
      x = jaar - 0.2, 
      y = `rantsoenP gehalte (g/kg)`,
      label = grondsoortlable,
      color = grondsoort
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE,
    position = position_nudge(y = c(0, 0,0, -1))  # give each line a different nudge
  )+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "solid", "Zand" = "solid"),
    name = NULL,
    guide = "none" ) +
  labs(y = "P gehalte rantsoen (g/kg)") +
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10)
  )+
  ylim(1.45,5.45)

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/linegrapPgehalte.jpg", plot = p1, width = 6, height = 3.5, dpi = 300)

#ugh puntenwolken
ggplot(datameans1 %>%
         distinct(jaar, bedrijfID, .keep_all = TRUE),
       aes(x=grondsoort, y=datameans1$RE))


#heatplots......
df_groep <- data2 %>%
  group_by(jaar, grondsoort) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(REgroup = case_when(
    `rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
    `rantsoenRE gehalte (g/kg DS)` >= 156 & `rantsoenRE gehalte (g/kg DS)` < 161 ~ "RE 156-160",  # Change condition to < 161 for better handling of 160.x
    `rantsoenRE gehalte (g/kg DS)` >= 161 & `rantsoenRE gehalte (g/kg DS)` <= 165 ~ "RE 161-165",
    `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"),
    REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156")),
    facetv = ifelse(grondsoort == "NL-gemiddelde", "", "Koe & Eiwit deelnemers"),
    facetv = factor(facetv, levels = c("", "Koe & Eiwit deelnemers"))
  )

p1 <- ggplot(df_groep, aes(x = jaar, y = grondsoort, fill = REgroup)) +
  geom_tile()  +  # Creates the heatmap tiles
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),  # Format the average values with 0 digits after the decimal
            color = "white", fontface = "bold", size = 4) +  # Creates the heatmap tiles
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(    panel.grid = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            legend.position = "right",
            axis.text = element_text(size = 11),
            legend.text = element_text(size = 11),
            strip.placement = "outside",
            strip.text.y = element_text(size = 12))+  # <-- note this change
  scale_fill_manual(values = REgroup.colors) +
  ylab("Kleur per boer")+
  facet_grid(facetv ~ ., scales = "free_y", space = "free") 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/heatplotgronssortvsnederland.jpg", plot = p1, width = 8, height = 4, dpi = 300)
#----------------------------------RE-klasse Alluvial - percentage---------------------------------------

plot_data <- data %>% 
  group_by(bedrijfID,jaar,REgroup) %>% 
  slice(1) %>% 
  group_by(jaar) %>% 
  mutate(nrFarmsTotal = length(unique(bedrijfID))) %>% 
  group_by(jaar, REgroup) %>% 
  mutate(nrFarms = paste0(length(unique(bedrijfID))," bedrijven"),
         nrFarmsPerc = paste0(round(length(unique(bedrijfID)) / nrFarmsTotal * 100),"%"),
         farmFreq = 1 / nrFarmsTotal,
         nrFarmsTotal = case_when(bedrijfID == min(bedrijfID, na.rm = T) ~ nrFarmsTotal))

p3<- ggplot(plot_data, aes(x = as.factor(jaar), y = farmFreq, stratum = REgroup, fill = REgroup)) +
  geom_flow(aes(alluvium = bedrijfID)) +
  geom_stratum() + 
  geom_text(stat = "stratum", aes(label = nrFarmsPerc), color = "white",fontface = "bold", size=3)+
  geom_text(aes(label = ifelse(!is.na(nrFarmsTotal), paste("n=", nrFarmsTotal), "")),  # Label only if nrFarmsTotal is not NA
            y = 1.05, color = "black", size =2)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size =8),
        axis.text.x = element_text(size =8),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size = 8),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size =8)
  ) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.1), labels = scales::percent) +
  labs(y = "Aandeel bedrijven (%)", x = "Jaar", fill = "RE groep") +
  scale_fill_manual(values = REgroup.colors, breaks = REgroup.breaks) +
  ggtitle("Verloop van het aantal bedrijven per RE-groep")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/RE klasse alluvial percentage.pdf", plot = p3, width = 7.5, height = 4, dpi = 300)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/RE klasse alluvial percentage.jpg", plot = p3, width = 7.5, height = 4, dpi = 300)

df_sorted <- datameans %>%
  group_by(naam)%>%
  mutate(naam = ifelse(naam == "Jan van Dieren (Aeres Landbouwbedrijf Dronten)", "Jan van Dieren", naam)) %>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%  # Arrange first by REgroup and then by jaar
  ungroup() %>%
  mutate(naam = factor(naam, levels = unique(naam)))%>%
  distinct(naam, jaar, .keep_all = TRUE)

df_groep <- df_sorted %>%
  group_by(jaar, grondsoort, intensiteitcat) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(REgroup = case_when(
    `rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
    `rantsoenRE gehalte (g/kg DS)` >= 156 & `rantsoenRE gehalte (g/kg DS)` < 161 ~ "RE 156-160",  # Change condition to < 161 for better handling of 160.x
    `rantsoenRE gehalte (g/kg DS)` >= 161 & `rantsoenRE gehalte (g/kg DS)` <= 165 ~ "RE 161-165",
    `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"),
    REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156"))
    ,
    intensiteit = factor(intensiteitcat, 
                         levels = c("< 14000 kg/ha", "14000 - 20000 kg/ha", "> 20000 kg/ha"))
  )

p <- ggplot(df_groep, aes(x = jaar, y = intensiteitcat, fill = REgroup)) +
  geom_tile()  +  # Creates the heatmap tiles
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),  # Format the average values with 0 digits after the decimal
            color = "white", fontface = "bold", size = 4.5) +  # Creates the heatmap tiles
  facet_wrap(. ~ grondsoort)+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.y =element_text(size=8, color="black"),
        axis.text.x = element_text(size=8,angle = 45, hjust = 1),
        legend.text = element_text(size=8),
        plot.title = element_text(size = 8),
        strip.text = element_text(size=8, color="black"),
        legend.position = "bottom") +
  scale_fill_manual(values = REgroup.colors) +
  ylab("Kleur per boer")

# Save the plot to the specified directory
ggsave(filename = "C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/REheatplot.pdf",
       plot = p, width = 11, height = 4.3, dpi = 1500)
ggsave(filename = "C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/REheatplot.jpg",
       plot = p, width = 11, height = 4.3, dpi = 1500)


#---------------------------------opbouw RE-gehalte ~ grondsoort--------------------------------
### Vanuit de database
sql = "SELECT g.ID AS [groepID]
  , t.Name AS [grondsoort]
  , g.Naam AS [groep]
  , d.jaar
  , d.bedrijfID
  , b.Naam AS [naam]
  , o.ID AS [KLWoutputID]
  , i.ID AS [MetaFeatureSetID]
  , f.ID AS [MetaFeatureID]
  , f.Name AS [var]
  , v.AsDouble AS [waarde]
FROM tbGroep g
JOIN tbTemplateValue t ON t.DataTemplateID = 24 AND t.ID = g.GrondsoortID
JOIN tbGroepDeelname d ON d.GroepID = g.ID
JOIN tbBedrijf b ON b.ID = d.BedrijfID
JOIN tbKLWoutput o ON o.BedrijfID = d.BedrijfID AND o.Jaar = d.Jaar AND o.OutputType = 1
, tbMetaFeatureSet i
JOIN tbMetaFeature f ON f.MetaFeatureSetID = i.ID
, tbFeatureValue v
WHERE g.Nummer > 0
AND i.Name = 'KLWoutput'
AND (f.Name LIKE '%_geh_re' OR 
     f.Name LIKE '%_geh_vem' OR 
     f.Name LIKE '%_re_kvem' OR 
     f.Name LIKE '%_aandeel' OR 
     f.Name LIKE '%_verbruik' OR 
     f.Name LIKE 'aanleg_%' OR 
     f.Name IN ('gve_melkvee', 'melkperha', 'efficientie_N', 'fpcmpkoe', 'melkpkoe', 'vet', 'eiwit', 'ureum', 'opb_graspr_ds'))
AND v.MetaFeatureSetID = i.ID 
AND v.MetaFeatureID = f.ID 
AND v.TupleID = o.ID
ORDER BY d.BedrijfID
 , d.Jaar
 , f.Name"

voeders.abbr = c("gr","gk","sm","rv","bp","kv","mp","ov")
voeders.dict = c("gr" = "Vers gras", "gk" = "Graskuil", "sm" = "Snijmais", "rv" = "Overig ruwvoer", "bp" = "Bijproducten", "kv" = "Krachtvoer", "mp" = "Melkproducten", "ov" = "Overig")
voeders.order = c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
voeders.color = colors <- c("Vers gras" = "#92D050", "Graskuil" =	"#548235", "Snijmais" =	"#FFC000", "Overig ruwvoer" =	"#843C0C", "Bijproducten"	= "#ED7D31", "Krachtvoer" =	"#4472C4")
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
kenmerk.abbr = c("gve","melkperha","rants","efficientie","fpcmpkoe","melkpkoe","vet","eiwit","ureum",'opbGrasprDs',"aanleg")
kenmerk.dict = c("gve" = "nrMK","melkperha" = "intensiteit","rants" = "rantsoen","efficientie" = "Nefficientie","fpcmpkoe" = "fpcmProductie","melkpkoe" = "melkProductie","vet" = "vet","eiwit" = "eiwit","ureum" = "ureum", 'opbGrasprDs' = "Grasopbrengst", "aanleg" = "aanleg")
property.abbr = c("aandeel","verbruik","re","kvem","vem","ds")
property.dict = c("aandeel" = "aandeel","verbruik" = "opname","re" = "RE", "kvem" = "REkVEM","vem" = "VEM", "ds" = "DS")
fracSM.colors = c("< 5%" = "#548235", "5-25%" = "#843C0C", "> 25%" =	"#FFC000")
REgroup.colors <- c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c", "NA" = "darkgray")
REgroup.breaks <- c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")
prodCat.colors <- c("6000 - 8000 kg" = "#ff4d4d", "8000 - 10000 kg" = "#ffc14d", "10000 - 12000 kg" ="#3385ff", "12000 - 14000 kg" = "#00b33c")

data <- executeSQL(sql, "SqlServerRemote") %>%
  dplyr::select(bedrijfID, naam, groep, jaar, grondsoort, var, waarde) %>%
  mutate(var = case_when(var == "opb_graspr_ds" ~ "opbGrasprDs", TRUE ~ var),
         var = case_when(startsWith(var,"aanleg") ~ sub("(aanleg_)(.._)(.*)","\\2\\1\\3", var), TRUE ~ var)) %>% 
  separate(var, c("var1","var2","var3"),sep = "_") %>% 
  mutate(voedermiddel = case_when(var1 %in% voeders.abbr ~ str_replace_all(var1, voeders.dict)),
         kenmerk = case_when(var1 %in% kenmerk.abbr ~ str_replace_all(var1, kenmerk.dict),
                             var2 %in% kenmerk.abbr ~ str_replace_all(var2, kenmerk.dict),TRUE ~ ""),
         eigenschap = case_when((var2 %in% property.abbr) & (!var3 %in% property.abbr) ~ str_replace_all(var2,property.dict),
                                (var3 %in% property.abbr) ~ str_replace_all(var3,property.dict),
                                TRUE ~ "")) %>% 
  distinct(bedrijfID, naam, groep, jaar, grondsoort, voedermiddel, kenmerk, eigenschap, .keep_all = T) %>% 
  pivot_wider(id_cols = c("bedrijfID","naam","groep", "jaar", "grondsoort", "voedermiddel"),
              names_from = c(kenmerk,eigenschap),names_sep = "",
              values_from = waarde) %>% 
  arrange(bedrijfID,jaar) %>% 
  group_by(bedrijfID,naam,groep, jaar, grondsoort) %>% 
  tidyr::fill(nrMK, intensiteit, Grasopbrengst, rantsoenopname, rantsoenRE, rantsoenREkVEM, rantsoenVEM,Nefficientie,fpcmProductie,melkProductie, vet, eiwit,ureum,.direction = "downup") %>% 
  ungroup() %>% 
  filter(!is.na(voedermiddel), grondsoort %in% c("Klei","Veen","Zand")) %>%
  mutate(opnameFrac = opname / rantsoenopname,
         opnamePkoe = opname / nrMK / 365,
         REcontr = round(opname / rantsoenopname * RE, 1),
         fracSM = case_when(opnameFrac > 0.25 & voedermiddel == "Snijmais" ~ "> 25%",
                            between(opnameFrac,0.05,0.25) & voedermiddel == "Snijmais" ~ "5-25%", 
                            opnameFrac < 0.05 & voedermiddel == "Snijmais" ~ "< 5%"),
         fracSM = factor(fracSM, levels = c("< 5%","5-25%","> 25%")),
         REgroup = case_when(rantsoenRE < 156 ~ "RE < 156",
                             rantsoenRE %in% c(156:160) ~ "RE 156-160",
                             rantsoenRE %in% c(161:165) ~ "RE 161-165",
                             rantsoenRE > 165 ~ "RE > 165"),
         REgroup = factor(REgroup, levels = c("RE > 165","RE 161-165","RE 156-160","RE < 156")),
         intensiteit = case_when(jaar == 2023 & between(intensiteit,-Inf,14000) ~ "< 14000 kg/ha",
                                 jaar == 2023 & between(intensiteit,14000,20000) ~ "14000 - 20000 kg/ha",
                                 jaar == 2023 & between(intensiteit,20000,Inf) ~ "> 20000 kg/ha"),
         intensiteit = factor(intensiteit,levels = c("< 14000 kg/ha","14000 - 20000 kg/ha","> 20000 kg/ha")),
         prodCat = floor(fpcmProductie / 2000),
         prodCatLabel = paste(prodCat * 2000,"-",(prodCat + 1) * 2000,"kg"),
         prodCatLabel = factor(prodCatLabel, levels = c("6000 - 8000 kg","8000 - 10000 kg","10000 - 12000 kg","12000 - 14000 kg")),
         prodCatLabel2 = case_when(between(fpcmProductie,-Inf,9000) ~ "< 9000 kg",
                                   between(fpcmProductie,9000,10000) ~ "9000 - 10000 kg",
                                   between(fpcmProductie,10000,11000) ~ "10000 - 11000 kg",
                                   between(fpcmProductie,11000,Inf) ~ "> 11000 kg"),
         prodCatLabel2 = factor(prodCatLabel2, levels = c("< 9000 kg","9000 - 10000 kg","10000 - 11000 kg","> 11000 kg")),
         prodCatLabel2 = case_when(between(fpcmProductie,-Inf,9500) ~ "< 9500 kg",
                                   between(fpcmProductie,9500,10500) ~ "9500 - 10500 kg",
                                   between(fpcmProductie,10500,Inf) ~ "> 10500 kg"),
         prodCatLabel2 = factor(prodCatLabel2, levels = c("< 9500 kg","9500 - 10500 kg","> 10500 kg")),
         versGrasGroep = case_when(voedermiddel == "Vers gras" & opnamePkoe == 0 ~ "Geen",
                                   voedermiddel == "Vers gras" & between(opnamePkoe,0.00001,3) ~ "0-3 kgDS/koe/dag",
                                   voedermiddel == "Vers gras" & between(opnamePkoe,3,Inf) ~ ">3 kgDS/koe/dag"),
         versGrasGroep = factor(versGrasGroep, levels = c("Geen","0-3 kgDS/koe/dag", ">3 kgDS/koe/dag")),
         voedermiddel = factor(voedermiddel, levels = voeders.order)) %>% 
  group_by(bedrijfID, jaar) %>% 
  fill(fracSM,versGrasGroep, .direction = "downup") %>% 
  group_by(bedrijfID) 

#---------------------------------opbouw RE-gehalte ~ grondsoort--------------------------------
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

plot_data <- data  %>% 
  mutate(voedermiddel = factor(voedermiddel,voeders.orderPlot)) %>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(rantsoenRE = round(mean(rantsoenRE, na.rm = T)),
          REcontr = mean(REcontr,na.rm = T),
          fpcmProductie = mean(fpcmProductie, na.rm = T),
          nrFarms = length(unique(bedrijfID))) %>% 
  group_by(grondsoort) %>% 
  mutate(lbl = case_when(voedermiddel == "Bijproducten" ~ round(rantsoenRE)),
         nrFarms = first(nrFarms[jaar == 2024]),
         nrFarms = case_when(voedermiddel == "Bijproducten" & jaar == 2022 ~ paste(nrFarms,"bedrijven"))) %>%
  filter(!is.na(voedermiddel))



ggplot(plot_data, aes(x = jaar, y = REcontr, fill = voedermiddel)) + theme_bw() +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = voeders.color) +
  geom_line(aes(x = jaar, y = (fpcmProductie - 8000)/20), color="#BAD3B4", size=1.5) +
  geom_point(aes(x = jaar, y = (fpcmProductie - 8000)/20),
             shape = 21,             # Allows fill and stroke
             fill = "#1c6c30",       # Inside color
             color = "white",        # Border color
             size = 2) +
  geom_text(aes(label = lbl,y = 185), size=3.5) +
  geom_text(aes(label = nrFarms,y = 210),nudge_x = -0.5, size=3.5) +
  facet_wrap(~ grondsoort) +
  scale_x_continuous(breaks = c(2020:2024),labels = c("2020","2021","2022","2023", "2024")) +
  scale_y_continuous(limits = c(0,220), expand = c(0,0), sec.axis = sec_axis(~. * 20 + 8000, name = "Meetmelkproductie [kg/koe/jr]")) +
  scale_color_manual(values = colors) +
  labs(x = element_blank(), y = "RE-gehalte [g/kgDS]", fill = "Voeder",caption = "Data 2020-2024") +
  theme(legend.position = "right", strip.placement = "outer",
        legend.title = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "#1c6c30", fill = NA),
        strip.background = element_rect(fill = "#F0CF80", color = "#F0CF80", size = 2),
        axis.text.y =element_text(color="black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(),
        plot.title = element_text(),
        strip.text = element_text(color="black"))


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/voedermiddelpergrondsoort1.pdf", width = 7, height = 5, dpi = 1500)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Tussenrapportage/voedermiddelpergrondsoort.pdf", width = 7.2, height = 5, dpi = 1500)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/voedermiddelREpergrondsoort1.jpg", width = 9, height = 4, dpi = 1500)


#Heatplot RE groups###############################################################################################
df_sorted <- datameans1 %>%
  group_by(naam)%>%
  mutate(naam = ifelse(naam == "Jan van Dieren (Aeres Landbouwbedrijf Dronten)", "Jan van Dieren", naam)) %>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%  # Arrange first by REgroup and then by jaar
  ungroup() %>%
  mutate(naam = factor(naam, levels = unique(naam)))%>%
  distinct(naam, jaar, .keep_all = TRUE)

for (testsss in unique(df_sorted$groep)) {
  
  # Filter data for the current group (safe character conversion)
  df_grp <- df_sorted %>% filter(groep == testsss)
  
  # Create the heatmap for the current group
  p <- ggplot(df_grp, aes(x = jaar, y = naam, fill = REgroup)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),
              color = "white", fontface = "bold", size = 4) +
    labs(title = paste("RE rantsoen (2020-2024) -", testsss),
         x = "Year",
         y = "Kleur per boer") +
    scale_fill_manual(values = REgroup.colors) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 16)
    )
  
  # Save plot
  ggsave(filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/REheatplot/REheatplot_", testsss, ".jpg"),
         plot = p, width = 9, height = 5.5, dpi = 300)
}

df_groep <- df_sorted %>%
  group_by(jaar, grondsoort, intensiteitcat) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(REgroup = case_when(
    `rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
    `rantsoenRE gehalte (g/kg DS)` >= 156 & `rantsoenRE gehalte (g/kg DS)` < 161 ~ "RE 156-160",  # Change condition to < 161 for better handling of 160.x
    `rantsoenRE gehalte (g/kg DS)` >= 161 & `rantsoenRE gehalte (g/kg DS)` <= 165 ~ "RE 161-165",
    `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"),
    REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156"))
    ,
    intensiteit = factor(intensiteitcat, 
                         levels = c("< 14000 kg/ha", "14000 - 20000 kg/ha", "> 20000 kg/ha"))
  )

ggplot(df_groep, aes(x = jaar, y = intensiteitcat, fill = REgroup)) +
  geom_tile()  +  # Creates the heatmap tiles
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),  # Format the average values with 0 digits after the decimal
            color = "white", fontface = "bold", size = 4) +  # Creates the heatmap tiles
  facet_wrap(. ~ grondsoort)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = REgroup.colors) +
  ylab("Kleur per boer")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/REheatplot/REheatplot_grondsoort_intensiteit.jpg", width = 9, height = 3.5, dpi = 1500)


df_groep <- df_sorted %>%
  group_by(jaar, groep) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(REgroup = case_when(
    `rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
    `rantsoenRE gehalte (g/kg DS)` >= 156 & `rantsoenRE gehalte (g/kg DS)` < 161 ~ "RE 156-160",  # Change condition to < 161 for better handling of 160.x
    `rantsoenRE gehalte (g/kg DS)` >= 161 & `rantsoenRE gehalte (g/kg DS)` <= 165 ~ "RE 161-165",
    `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"),
    REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156"))
  )

ggplot(df_groep, aes(x = jaar, y = groep, fill = REgroup)) +
  geom_tile()  +  # Creates the heatmap tiles
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),  # Format the average values with 0 digits after the decimal
            color = "white", fontface = "bold", size = 4) +  # Creates the heatmap tiles
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size=11),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = REgroup.colors) +
  ylab("Kleur per boer")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/REheatplot/REheatplot_groep.jpg", width = 8, height = 4.3, dpi = 1500)

#calculate dalers, stijgers, etc.----------------------------------------------------------------
countname <- df_sorted %>%
  group_by(naam)%>%
  count()%>% rename(Ntotal = n)

Stabiel hoog <- df_sorted %>%
  select(naam, bedrijfID, grondsoort, intensiteitcat, REgroup, jaar, `rantsoenRE gehalte (g/kg DS)`)%>%
  filter(REgroup %in% c("RE 161-165", "RE > 165"))  %>%
  add_count(naam) %>%  # Adds a column `n` while keeping all others
  left_join(countname, by=c("naam")) %>%
  mutate(perc = n/Ntotal)%>%
  filter(perc > 0.8) %>%
  mutate(Typebedrijf = "Stabiel hoog") %>%
  distinct(naam, Typebedrijf)

Stabiel laag <- df_sorted %>%
  select(naam, bedrijfID, grondsoort, intensiteitcat, REgroup, jaar, `rantsoenRE gehalte (g/kg DS)`)%>%
  filter(REgroup %in% c("RE 156-160", "RE < 156"))  %>%
  add_count(naam) %>%  # Adds a column `n` while keeping all others
  left_join(countname, by=c("naam")) %>%
  mutate(perc = n/Ntotal)%>%
  filter(perc > 0.8) %>%
  mutate(Typebedrijf = "Stabiel laag") %>%
  distinct(naam, Typebedrijf)

Stabiel <- rbind(Stabiel hoog,Stabiel laag)

Bewegers <- df_sorted %>%
  anti_join(Stabiel, by = "naam")%>%
  group_by(bedrijfID, grondsoort, naam) %>%
  filter(jaar == max(jaar)) %>%  # Houdt alleen de laatste "jaar" per groep
  ungroup() %>%           
  mutate(Typebedrijf = case_when(`rantsoenRE gehalte (g/kg DS)` <155 ~ "Doel gehaald", 
                                 `rantsoenRE gehalte (g/kg DS)` == 155 ~ "Doel gehaald",
                                 `rantsoenRE gehalte (g/kg DS)` >155 ~ "Doel niet gehaald",
                                 TRUE ~ "KLW 2020 (referentie) ontbreekt"))%>%
  select(naam, Typebedrijf) %>%
  filter(Typebedrijf != "KLW 2020 (referentie) ontbreekt")

final_df <- bind_rows(Bewegers, Stabiel) %>%
  right_join(datameans, by=c("naam"))%>%
  filter(!is.na(Typebedrijf)) %>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"),
                              labels = c("Stabiel laag n=29", "Doel gehaald n=35", "Doel niet gehaald n=58", "Stabiel hoog n=27")))

heatplots <- final_df %>% 
  distinct(naam, jaar, REgroup, `rantsoenRE gehalte (g/kg DS)`, Typebedrijf, grondsoort, bedrijfID)%>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%
  mutate(naam = factor(naam, levels = unique(naam))) %>% 
  left_join(final_df %>% 
              distinct(naam, Typebedrijf) %>% 
              group_by(Typebedrijf)%>% 
              mutate(n = n())%>%
              ungroup()%>%
              mutate(Typebedrijflabel = paste0(Typebedrijf, n)) %>%
              distinct(Typebedrijflabel, Typebedrijf),
            by=c("Typebedrijf")
  ) %>%
  mutate(REgroup = factor(REgroup, levels=c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")),
         Naam_ID = paste(bedrijfID, naam,  sep = "_")
  )

ggplot(heatplots, aes(x = jaar, y = naam, fill = REgroup)) +
  geom_tile()+ 
  theme_minimal()+
  facet_wrap(.~Typebedrijf, scales = "free", nrow=1)+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        legend.text = element_text(size = 12))+
  scale_fill_manual(values = REgroup.colors)+
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)), color="White")

ggplot(heatplots, aes(x = jaar, y = naam, fill = REgroup)) +
  geom_tile()+ 
  theme_minimal()+
  facet_wrap(.~Typebedrijf, scales = "free", nrow=1)+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size=11),
        axis.text.y = element_blank(),
        strip.text = element_text(size=12),
        legend.text = element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_manual(values = REgroup.colors)

ggsave(
  filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/heatplotsgroups/figures14_5/heatplot_145.jpg"),
  width = 9.5,
  height = 4.5,
  dpi = 300
)

# Create list of unique 'Typebedrijf' values
type_v <- unique(heatplots$Typebedrijf)

# Loop over each Typebedrijf and create & save a separate plot
for (type_v in type_v) {
  plot_data <- subset(heatplots, Typebedrijf == type_v)
  
  p <- ggplot(plot_data, aes(x = jaar, y = Naam_ID, fill = REgroup)) +
    geom_tile() + 
    theme_minimal() +
    facet_wrap(. ~ Typebedrijf, scales = "free", nrow = 1) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.title = element_blank(),
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 15),
      strip.text = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 15),
      legend.position = "right"
    ) +
    scale_fill_manual(values = REgroup.colors) #+
    #geom_text(
      #aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),
      #color = "white", fontface = "bold", size = 5
    #)
  
  # Safe filename-friendly version of Typebedrijf
  filename_type <- gsub(" ", "_", type_v)
  
  # Save the plot
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/heatplotsgroups/figures14_5/heatplot_", filename_type, ".jpg"),
    plot = p,
    width = 10,
    height = 10,
    dpi = 300
  )
}

Excelfile <- heatplots %>%
  distinct(naam, Typebedrijf) %>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag n=29", "Doel gehaald n=35", "Doel niet gehaald n=58", "Stabiel hoog n=27"),
                              labels = c("Stabiel laag", "Wel gehaald", "Niet gehaald", "Stabiel hoog")))%>%
  arrange(Typebedrijf)

write_xlsx(Excelfile, "C:/Rfiles/WR/Koe en eiwit 2025/Graphs/heatplotsgroups/Namen_stabiel_Bewegers.xlsx")

#----------------------------------------Line graphs per frarm----------------------------------------------------------------
#lines intensity, per grondsoort
grondsoorten <- unique(datameans1$grondsoort)

# Loop over each grondsoort and create & save a separate plot
for (gs in grondsoorten) {
  
  # Filter and prepare data
  Graph <- datameans1 %>%
    filter(grondsoort == gs) %>%
    filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")) %>%
    group_by(jaar, intensiteitcat, voedermiddel) %>%
    reframe(
      `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
      `fpcmProductie per koe (kg)`   = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
      nrFarms = length(unique(bedrijfID)),
      opname  = mean(`opname (kg DS/koe/dag)`, na.rm = TRUE),
      RE      = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE)
    ) %>%
    mutate(
      voedermiddel   = factor(voedermiddel, levels = c("Vers gras","Graskuil","Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
      intensiteitcat = factor(intensiteitcat, levels = c("< 14000 kg/ha","14000 - 20000 kg/ha","> 20000 kg/ha"))
    )
  
  # --- Plot 1 ---
  p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = intensiteitcat, color = intensiteitcat, linetype = intensiteitcat)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(~ voedermiddel, nrow = 1) +
    ylab("Voeropname (kgDS/dier/dag)") +
    scale_y_continuous(limits = c(0, 9)) +
    scale_color_manual(values = c("> 20000 kg/ha" = "#E29E00","14000 - 20000 kg/ha" = "#293684","< 14000 kg/ha" = "#1A6B05")) +
    scale_linetype_manual(values = c("> 20000 kg/ha" = "solid","14000 - 20000 kg/ha" = "dashed","< 14000 kg/ha" = "solid")) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )
  
  # --- Plot 2 ---
  p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = intensiteitcat, color=intensiteitcat, linetype = intensiteitcat)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(~ voedermiddel, nrow = 1) +
    ylab("RE gehalte (g/kg DS)") +
    scale_color_manual(values = c("> 20000 kg/ha" = "#E29E00","14000 - 20000 kg/ha" = "#293684","< 14000 kg/ha" = "#1A6B05")) +
    scale_linetype_manual(values = c("> 20000 kg/ha" = "solid","14000 - 20000 kg/ha" = "dashed","< 14000 kg/ha" = "solid")) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 0, color = "white"),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )
  
  # --- Combine + annotate ---
  p3 <- ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")
  p3 <- annotate_figure(p3, top = text_grob(gs, face = "bold", size = 14))
  
  # Safe filename
  filename_gs <- gsub(" ", "_", gs)
  
  # --- Save ---
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/29_08/groeplinevoedermiddelen2_", filename_gs, ".jpg"),
    plot = p3,
    width = 12,
    height = 5.5,
    dpi = 300
  )
}

#line RE,  per grondsoort###############################################################################################
grondsoorten <- unique(datameans1$grondsoort)

# Loop over each grondsoort and create & save a separate plot
for (gs in grondsoorten) {
  
  # Filter and prepare data
  Graph <- datameans1 %>%
    filter(grondsoort == gs) %>%
    filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")) %>%
    group_by(jaar, REgroup, voedermiddel) %>%
    reframe(
      `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
      `fpcmProductie per koe (kg)`   = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
      nrFarms = length(unique(bedrijfID)),
      opname  = mean(`opname (kg DS/koe/dag)`, na.rm = TRUE),
      RE      = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE)
    ) %>%
    mutate(
      voedermiddel   = factor(voedermiddel, levels = c("Vers gras","Graskuil","Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
      REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156")))
  
  # --- Plot 1 ---
  p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = REgroup, color = REgroup)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(~ voedermiddel, nrow = 1) +
    scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
    ylab("Voeropname (kgDS/dier/dag)") +
    #scale_y_continuous(limits = c(0, 9)) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )
  
  # --- Plot 2 ---
  p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = REgroup, color=REgroup)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1)  +
    scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
    facet_wrap(~ voedermiddel, nrow = 1) +
    ylab("RE gehalte (g/kg DS)") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 0, color = "white"),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )
  
  # --- Combine + annotate ---
  p3 <- ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")
  p3 <- annotate_figure(p3, top = text_grob(gs, face = "bold", size = 14))
  
  # Safe filename
  filename_gs <- gsub(" ", "_", gs)
  
  # --- Save ---
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/29_08/groeplinevoedermiddelenRE2_", filename_gs, ".jpg"),
    plot = p3,
    width = 12,
    height = 5.5,
    dpi = 300
  )
}

# Summarise the data
Graph <- datameans1 %>%
  group_by(jaar, Typebedrijf) %>% 
  reframe(
    `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
    `VEM/kg DS` = round(mean(`VEM/kg DS`, na.rm = TRUE)),
    `REkVEM (g/kVEM)` = mean(`REkVEM (g/kVEM)`, na.rm = TRUE),
    nkoeien = mean(nkoeien, na.rm = TRUE),
    `Melk per koe (kg)` = round(mean(`Melk per koe (kg)`, na.rm = TRUE)),
    jongvee_per_10_melkkoe = mean(jongvee_per_10_melkkoe, na.rm = TRUE)
  ) %>%
  mutate(Typebedrijf = factor(Typebedrijf, 
                              levels = c("Stabiel laag", "Doel gehaald", 
                                         "Doel niet gehaald", "Stabiel hoog")))

#Linegraphs multiple variables per year#Typerbedrijf###############################################################
# Remove duplicates
# Clean data
dat_clean <- datameans1 %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE)%>%
  mutate(
    Typebedrijf = factor(
      Typebedrijf, 
      levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")
    )
  )

# Compute pairwise comparisons per year
comparisons <- dat_clean %>%
  group_by(jaar) %>%
  group_modify(~ compare_means(nkoeien ~ Typebedrijf, data = ., method = "t.test")) %>%
  ungroup() %>%
  # Add y-position BEFORE filtering
  group_by(jaar) %>%
  mutate(y.position = max(dat_clean$nkoeien[dat_clean$jaar == unique(jaar)], na.rm = TRUE) + 
           row_number() * 2) %>%
  ungroup() %>%
  # Keep only significant comparisons
  filter(p.adj < 0.05)

# Plot
ggplot(dat_clean, aes(x = Typebedrijf, y = nkoeien, color = Typebedrijf)) +
  geom_boxplot(aes(fill = Typebedrijf), alpha = 0.2, outliers = FALSE) +
  geom_jitter(width = 0.2, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  facet_wrap(~jaar, nrow = 1, scales = "free") +
  stat_pvalue_manual(comparisons, label = "p.signif", tip.length = 0.01) +
  labs(x = "Type of Farm", y = "Number of Cows", color = "Type of Farm", fill = "Type of Farm") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),            # remove x-axis title
    axis.text.x = element_text(angle = 45, hjust = 1,),
    legend.position = "none"
  )+
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  scale_fill_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) 

# Plot
ggplot(dat_clean, aes(x = `rantsoenRE gehalte (g/kg DS)`, y = nkoeien, color = Typebedrijf)) +
  geom_point(width = 0.2, size = 2) +
  facet_wrap(~jaar, nrow = 1, scales = "free") +
  labs(x = "rantsoenRE gehalte (g/kg DS)", y = "Number of Cows", color = "Type of Farm", fill = "Type of Farm") +
  theme_minimal()+
  theme(
    legend.position = "none"
  )+
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) 

# Step 1: create Graskuil columns first
datameans1_with_newcolumns <- datameans1 %>%
  group_by(jaar, bedrijfID) %>%
  mutate(
    `Graskuil_RE gehalte (g/kg DS)` = mean(`RE gehalte (g/kg DS)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Graskuil_VEM/kg DS` = mean(`VEM/kg DS`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Graskuil_REkVEM (g/kVEM)` = mean(`REkVEM (g/kVEM)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Krachtvoer_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Krachtvoer"], na.rm = TRUE),
    `Graskuil_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Snijmais_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Snijmais"], na.rm = TRUE)
  ) %>%
  ungroup()


# Variable lists
vars_to_round <- c("VEM/kg DS", "Melk per koe (kg)")

vars_selected <- c(
  "fpcmProductie per koe (kg)", 
  "REkVEM (g/kVEM)", 
  "nkoeien", 
  "jongvee_per_10_melkkoe", 
  "efficientie_N", 
  "oppervlakte totaal (ha)", 
  "oppbraak", 
  "oppervlakte gras (ha)", 
  "oppervlakte mais (ha)",
  "oppoverig",
  "VEM/kg DS", 
  "REkVEM (g/kVEM)",
  "Graskuil_RE gehalte (g/kg DS)",
  "Graskuil_VEM/kg DS",
  "Graskuil_REkVEM (g/kVEM)",
  'Saldo incl. jongvee (euro/100kg FPCM)',
  'Saldo incl. jongvee (euro/koe)',
  'Krachtvoer_P gehalte (g/kg)',
  'Graskuil_P gehalte (g/kg)',
  'Snijmais_P gehalte (g/kg)',
  'rantsoenP gehalte (g/kg)'
)

# Step 2: compute counts per variable per jaar x Typebedrijf
counts_df <- datameans1_with_newcolumns %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE) %>%
  group_by(jaar, Typebedrijf) %>%
  summarise(
    across(
      all_of(vars_selected),
      ~ sum(!is.na(.x))/5,   # non-NA counts
      .names = "{.col}_n"
    ),
    .groups = "drop"
  )

# Step 3: summarise means and join counts
Graph <- datameans1_with_newcolumns %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE) %>%
  group_by(jaar, Typebedrijf) %>%
  summarise(
    across(all_of(vars_selected), ~ mean(.x, na.rm = TRUE)),
    across(all_of(vars_to_round), ~ round(mean(.x, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  left_join(counts_df, by = c("jaar","Typebedrijf")) %>%
  mutate(
    Typebedrijf = factor(
      Typebedrijf, 
      levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")
    )
  )


# --- Step 3: variable groups ---
graph1 <- c("Graskuil_RE gehalte (g/kg DS)", "Graskuil_VEM/kg DS", "Graskuil_REkVEM (g/kVEM)")
graph2 <- c("oppervlakte totaal (ha)", "oppervlakte gras (ha)", "oppervlakte mais (ha)")
graph4 <- c("fpcmProductie per koe (kg)", "nkoeien", "jongvee_per_10_melkkoe", "efficientie_N")
graph5 <- c("nkoeien", "jongvee_per_10_melkkoe", "efficientie_N")
graph6 <- c('Saldo incl. jongvee (euro/100kg FPCM)','Saldo incl. jongvee (euro/koe)')
graph7 <- c('Krachtvoer_P gehalte (g/kg)',  'Snijmais_P gehalte (g/kg)', 'Graskuil_P gehalte (g/kg)', 'rantsoenP gehalte (g/kg)')
graph8 <- c("VEM/kg DS", "REkVEM (g/kVEM)", 'rantsoenP gehalte (g/kg)')



graph_groups <- list(
  Graskuil = graph1,
  Oppervlakte = graph2,
  General1 = graph4,
  General2 = graph5,
  Saldo = graph6,
  Pgehalte = graph7,
  overig = graph8
)
# --- Step 4: plotting function ---
make_plot <- function(var) {
  count_var <- paste0(var, "_n")
  
  counts <- Graph %>%
    group_by(Typebedrijf) %>%
    summarise(n = sum(.data[[count_var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(Typebedrijf, " (n=", n, ")")) %>%
    pull(label)
  
  caption_text <- paste(counts, collapse = "\n")  # multiple lines
  
  ggplot(Graph, aes(x = jaar, y = .data[[var]], 
                    group = Typebedrijf, color = Typebedrijf)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    ylab(var) +
    #labs(caption = caption_text) +  # 👈 put counts at the bottom
    scale_color_manual(values = c(
      "Stabiel hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c"
    )) +
    theme_minimal() +
    #scale_y_continuous(limits = c(0, NA)) +   # 👈 start y-axis at 0
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11),
      plot.caption = element_text(size = 8, face = "italic", color = "gray", hjust = 0.5, lineheight = 1.1)
    )
}

# --- Step 5: loop and save ---
for (gname in names(graph_groups)) {
  plots <- lapply(graph_groups[[gname]], make_plot)
  
  arranged <- ggarrange(
    plotlist = plots, 
    ncol = length(plots), 
    common.legend = TRUE, legend = "bottom"
  )
  
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/TypeBedrijf2/", gname, ".jpg"),
    plot = arranged, width = 10, height = 3.5, dpi = 300
  )
}

#oppervlakte############################################################################################
# Step 3: summarise means and join counts
Graph <- datameans1_with_newcolumns %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE) %>%
  group_by(jaar, Typebedrijf) %>%
  summarise(
    across(c("oppervlakte totaal (ha)", 
             "oppervlakte gras (ha)", 
             "oppervlakte mais (ha)"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Typebedrijf = factor(
      Typebedrijf, 
      levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")
    )) %>%
      pivot_longer(
        cols = c("oppervlakte totaal (ha)", "oppervlakte gras (ha)", "oppervlakte mais (ha)"),
        names_to = "variable",
        values_to = "value"
      ) 
    
    # 2️⃣ Plot with facets, free y-scale
ggplot(Graph , aes(x = jaar, y = value, group = Typebedrijf, color = Typebedrijf)) +
      geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      facet_wrap(~variable, scales = "free") +  # 👈 free y-scale per facet
      scale_color_manual(values = c(
        "Stabiel hoog" = "#ff4d4d",
        "Doel niet gehaald" = "#ffc14d",
        "Doel gehaald" = "#3385ff",
        "Stabiel laag" = "#00b33c"
      )) +
  ylab("Oppervlakte (ha)")+
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.ticks.x = element_line(color = "black"),
        legend.text = element_text(size = 11)
      )+ scale_y_continuous(limits = c(0, NA))

ggsave(("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/TypeBedrijf2/opp.jpg"), width = 8, height = 3.5, dpi = 300
)


ggplot(Graph, aes(x = jaar, y = .data[[var]], 
                  group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  ylab(var) +
  #labs(caption = caption_text) +  # 👈 put counts at the bottom
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  #scale_y_continuous(limits = c(0, NA)) +   # 👈 start y-axis at 0
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    plot.caption = element_text(size = 8, face = "italic", color = "gray", hjust = 0.5, lineheight = 1.1)
  )


##############Grondsoorten##############################################################################
# Step 1: create Graskuil columns first
datameans1_with_newcolumns <- datameans1 %>%
  group_by(jaar, bedrijfID) %>%
  mutate(
    `Graskuil_RE gehalte (g/kg DS)` = mean(`RE gehalte (g/kg DS)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Graskuil_VEM/kg DS` = mean(`VEM/kg DS`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Graskuil_REkVEM (g/kVEM)` = mean(`REkVEM (g/kVEM)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Krachtvoer_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Krachtvoer"], na.rm = TRUE),
    `Graskuil_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Graskuil"], na.rm = TRUE),
    `Snijmais_P gehalte (g/kg)` = mean(`P gehalte (g/kg)`[voedermiddel == "Snijmais"], na.rm = TRUE)
  ) %>%
  ungroup()

# Variable lists
vars_to_round <- c("VEM/kg DS", "Melk per koe (kg)")

vars_selected <- c(
  "fpcmProductie per koe (kg)", 
  "REkVEM (g/kVEM)", 
  "nkoeien", 
  "jongvee_per_10_melkkoe", 
  "efficientie_N", 
  "oppervlakte totaal (ha)", 
  "oppbraak", 
  "oppervlakte gras (ha)", 
  "oppervlakte mais (ha)",
  "oppoverig",
  "VEM/kg DS", 
  "REkVEM (g/kVEM)",
  "Graskuil_RE gehalte (g/kg DS)",
  "Graskuil_VEM/kg DS",
  "Graskuil_REkVEM (g/kVEM)",
  'Saldo incl. jongvee (euro/100kg FPCM)',
  'Saldo incl. jongvee (euro/koe)',
  'Krachtvoer_P gehalte (g/kg)',
  'Graskuil_P gehalte (g/kg)',
  'Snijmais_P gehalte (g/kg)',
  'rantsoenP gehalte (g/kg)'
)

# Step 2: compute counts per variable per jaar x grondsoort
counts_df <- datameans1_with_newcolumns %>%
  group_by(jaar, grondsoort) %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE) %>%
  summarise(
    across(
      all_of(vars_selected),
      ~ sum(!is.na(.x))/5,   # non-NA counts
      .names = "{.col}_n"
    ),
    .groups = "drop"
  )

# Step 3: summarise means and join counts
Graph <- datameans1_with_newcolumns %>%
  distinct(jaar, bedrijfID, .keep_all = TRUE) %>%
  group_by(jaar, grondsoort) %>%
  summarise(
    across(all_of(vars_selected), ~ mean(.x, na.rm = TRUE)),
    across(all_of(vars_to_round), ~ round(mean(.x, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  left_join(counts_df, by = c("jaar","grondsoort")) %>%
  mutate(
    grondsoort = factor(
      grondsoort, 
      levels = c("Klei", "Veen", "Zand")
    )
  )


# --- Step 3: variable groups ---
graph1 <- c("Graskuil_RE gehalte (g/kg DS)", "Graskuil_VEM/kg DS", "Graskuil_REkVEM (g/kVEM)")
graph2 <- c("oppervlakte totaal (ha)", "oppervlakte gras (ha)", "oppervlakte mais (ha)", "oppbraak", "oppoverig")
graph4 <- c("fpcmProductie per koe (kg)", "REkVEM (g/kVEM)", "VEM/kg DS")
graph5 <- c("nkoeien", "jongvee_per_10_melkkoe", "efficientie_N")
graph6 <- c('Saldo incl. jongvee (euro/100kg FPCM)','Saldo incl. jongvee (euro/koe)')
graph7 <- c('Krachtvoer_P gehalte (g/kg)',  'Snijmais_P gehalte (g/kg)', 'Graskuil_P gehalte (g/kg)', 'rantsoenP gehalte (g/kg)')
graph8 <- c("VEM/kg DS", "REkVEM (g/kVEM)", 'rantsoenP gehalte (g/kg)')



graph_groups <- list(
  Graskuil = graph1,
  Oppervlakte = graph2,
  General1 = graph4,
  General2 = graph5,
  Saldo = graph6,
  Pgehalte = graph7,
  overig = graph8
)


# --- Step 4: plotting function ---
make_plot <- function(var) {
  count_var <- paste0(var, "_n")
  
  counts <- Graph %>%
    group_by(grondsoort) %>%
    summarise(n = sum(.data[[count_var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(grondsoort, " (n=", n, ")")) %>%
    pull(label)
  
  caption_text <- paste(counts, collapse = "\n")  # multiple lines
  
  ggplot(Graph, aes(x = jaar, y = .data[[var]], 
                    group = grondsoort, color = grondsoort)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    ylab(var) +
    labs(caption = caption_text) +  # 👈 put counts at the bottom
    scale_color_manual(values = c(
      "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
    )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11),
      plot.caption = element_text(size = 8, face = "italic", color = "gray", hjust = 0.5, lineheight = 1.1)
    )
}

# --- Step 5: loop and save ---
for (gname in names(graph_groups)) {
  plots <- lapply(graph_groups[[gname]], make_plot)
  
  arranged <- ggarrange(
    plotlist = plots, 
    ncol = length(plots), 
    common.legend = TRUE, legend = "bottom"
  )
  
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/Grondsoort2/", gname, ".jpg"),
    plot = arranged, width = 9, height = 3.5, dpi = 300
  )
}



#cutoff######################################################################################################
# List of variables to plot
var_list <- c(
  "fpcmProductie per koe (kg)",
  "VEM/kg DS",
  "REkVEM (g/kVEM)",
  "nkoeien",
  "Melk per koe (kg)",
  "jongvee_per_10_melkkoe"
)

# Loop over each variable and create a separate plot
for (var in var_list) {
  
  p <- ggplot(Graph, aes(x = jaar, y = .data[[var]], 
                         group = Typebedrijf, color = Typebedrijf)) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    ylab(var) +
    scale_color_manual(values = c(
      "Stabiel hoog" = "#ff4d4d",
      "Doel niet gehaald" = "#ffc14d",
      "Doel gehaald" = "#3385ff",
      "Stabiel laag" = "#00b33c"
    )) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )
  
  # Make filename safe
  filename_var <- gsub("[ /()]", "_", var)
  
  # Save each plot
  ggsave(
    filename = paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/29_08/plot_", filename_var, ".jpg"),
    plot = p,
    width = 5,
    height = 4,
    dpi = 300
  )
}




Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean(`opname (kg DS/koe/dag)`, na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"),
                              labels = c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Hoog"))) %>%
  filter(!voedermiddel %in% c("Overig ruwvoer", "Bijproducten"))


p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(limits = c(0, 8)) +
  scale_color_manual(values = c(
    "Hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 


# 1️⃣ Compute the largest range and new y-limits per facet
# Step 1: Compute min and max per group
facet_limits <- Graph %>%
  group_by(voedermiddel) %>%
  summarise(
    ymin = min(RE, na.rm = TRUE),
    ymax = max(RE, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Compute largest range across all facets
max_range <- facet_limits %>%
  mutate(range = ymax - ymin) %>%
  summarise(max_range = max(range)) %>%
  pull(max_range)

# Step 3: Compute midpoint and preliminary limits
facet_limits <- facet_limits %>%
  mutate(
    midpoint = (ymin + ymax)/2,
    new_ymin = midpoint - max_range/2,
    new_ymax = midpoint + max_range/2
  ) %>%
  # Step 4: Round ymin down to nearest multiple of 20 and adjust ymax accordingly
  mutate(
    new_ymin_rounded = floor(new_ymin / 0.5) * 0.5,
    new_ymax_rounded = new_ymax + (new_ymin_rounded - new_ymin)
  )

# 2️⃣ Join new y-limits back to original data
Graph2 <- Graph %>%
  left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")

# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph2, aes(x = jaar, y = RE, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  # Extend y-axis per facet to same height
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  ylab("RE gehalte (g/kgDS)") +
  scale_color_manual(values = c(
    "Hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/groeplinevoedermiddelen2.jpg", plot = p3, width = 9, height = 5.5, dpi = 300)

#P-gehalte#################################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(`P gehalte (g/kg)` = (mean(`P gehalte (g/kg)`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")))

# 1️⃣ Compute the largest range and new y-limits per facet
# Step 1: Compute min and max per group
facet_limits <- Graph %>%
  group_by(voedermiddel) %>%
  summarise(
    ymin = min(`P gehalte (g/kg)`, na.rm = TRUE),
    ymax = max(`P gehalte (g/kg)`, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Compute largest range across all facets
max_range <- facet_limits %>%
  mutate(range = ymax - ymin) %>%
  summarise(max_range = max(range)) %>%
  pull(max_range)

# Step 3: Compute midpoint and preliminary limits
facet_limits <- facet_limits %>%
  mutate(
    midpoint = (ymin + ymax)/2,
    new_ymin = midpoint - max_range/2,
    new_ymax = midpoint + max_range/2
  ) %>%
  # Step 4: Round ymin down to nearest multiple of 20 and adjust ymax accordingly
  mutate(
    new_ymin_rounded = floor(new_ymin / 0.5) * 0.5,
    new_ymax_rounded = new_ymax + (new_ymin_rounded - new_ymin)
  )

# 2️⃣ Join new y-limits back to original data
Graph2 <- Graph %>%
  left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")

# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph2, aes(x = jaar, y = `P gehalte (g/kg)`, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  # Extend y-axis per facet to same height
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  ylab("P gehalte (g/kg)") +
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenpgehalte.jpg", plot = p2, width = 12, height =4, dpi = 300)

#P-gehalte grondsoort#################################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`P gehalte (g/kg)` = (mean(`P gehalte (g/kg)`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))) %>%
  bind_rows(
    datameans1 %>%
      filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
      group_by(jaar, voedermiddel) %>% 
      reframe(`P gehalte (g/kg)` = (mean(`P gehalte (g/kg)`, na.rm = T))) %>%
      mutate(grondsoort = "Koe & Eiwit") 
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))%>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenP gehalte (g/kg)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenP gehalte (g/kg)` = mean(`rantsoenP gehalte (g/kg)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenP gehalte (g/kg)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenP gehalte (g/kg)` = mean(`rantsoenP gehalte (g/kg)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenP gehalte (g/kg)`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, y = 5, label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_point(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenP gehalte (g/kg)`, color = grondsoort),size = 2) +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenP gehalte (g/kg)`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "P gehalte rantsoen (g/kg)") +
  theme_minimal() +
  facet_wrap(.~"Rantsoen")+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(1.6,5.5)


# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar, y = `P gehalte (g/kg)`, group = grondsoort, color = grondsoort,linetype = grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  # Extend y-axis per facet to same height
  #geom_blank(aes(y = new_ymin_rounded)) +
  #geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(
    values = c(
      "Klei"        = "#213b73",
      "Veen"        = "#1c6c30",
      "Zand"        = "#e29f02",
      "Koe & Eiwit" = "#cf641c"
    ),
    breaks = c("Klei", "Veen", "Zand", "Koe & Eiwit")  # gewenste volgorde
  )+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid")) +
  ylab("P gehalte (g/kg)") +
  ylim(1.6,5.5)+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

p2 <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  widths = c(0.3, 0.8),       # 20% and 80%
  common.legend = TRUE,
  legend = "bottom"            # place legend under both plots
)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenpgehalte2.jpg", plot = p2, width = 11.5, height =4.5, dpi = 300)

# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 
           # %>% filter(grondsoort == "Koe & Eiwit") %>%
            #  filter(grondsoortlable == "Koe & Eiwit")
            , aes(x = jaar, y = `rantsoenP gehalte (g/kg)`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, 
           y = max(data2$`rantsoenP gehalte (g/kg)`)-0.02, 
           label = "Startjaar\nKoe & Eiwit", 
           hjust = -0.1, 
           vjust = 0, 
           color = "gray60",
           size=3.5) +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenP gehalte (g/kg)`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "solid", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "P gehalte rantsoen (g/kg)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  )   +
  geom_text(
    data = labels_df 
    #%>% filter(grondsoort == "Koe & Eiwit")
    ,
    aes(
      x = jaar - 0.2, 
      y = `rantsoenP gehalte (g/kg)`,
      label = grondsoortlable,
      color = grondsoort
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE,
    position = position_nudge(y = c(0, 0,0, 0,0))  # give each line a different nudge
  )+
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  ylim(3.4, 3.72)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenpgehalte2.jpg", 
       plot = p2, width = 6, height =3.5, dpi = 300)



#P-gehalte grondsoort################################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))) %>%
  bind_rows(
    datameans1 %>%
      filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
      group_by(jaar, voedermiddel) %>% 
      reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
      mutate(grondsoort = "Koe & Eiwit") 
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))%>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenVEM/kg DS`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, y = 1105, label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_point(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenVEM/kg DS`, color = grondsoort),size = 2) +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenP gehalte (g/kg)`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "VEM/kg DS rantsoen") +
  theme_minimal() +
  facet_wrap(.~"Rantsoen")+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(880,1125)


# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar, y = `VEM/kg DS`, group = grondsoort, color = grondsoort,linetype = grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  # Extend y-axis per facet to same height
  #geom_blank(aes(y = new_ymin_rounded)) +
  #geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(
    values = c(
      "Klei"        = "#213b73",
      "Veen"        = "#1c6c30",
      "Zand"        = "#e29f02",
      "Koe & Eiwit" = "#cf641c"
    ),
    breaks = c("Klei", "Veen", "Zand", "Koe & Eiwit")  # gewenste volgorde
  )+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid")) +
  ylab("VEM/kg DS") +
  ylim(880,1125)+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

p2 <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  widths = c(0.3, 0.8),       # 20% and 80%
  common.legend = TRUE,
  legend = "bottom"            # place legend under both plots
)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenVEMgehalte2.jpg", plot = p2, width = 11.5, height =4.5, dpi = 300)


#VEMKRE########################################################################################################return
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`REkVEM (g/kVEM)` = (mean(`REkVEM (g/kVEM)`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))) %>%
  bind_rows(
    datameans1 %>%
      filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
      group_by(jaar, voedermiddel) %>% 
      reframe(`REkVEM (g/kVEM)` = (mean(`REkVEM (g/kVEM)`, na.rm = T))) %>%
      mutate(grondsoort = "Koe & Eiwit") 
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))%>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenREkVEM (g/kVEM)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenREkVEM (g/kVEM)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenREkVEM (g/kVEM)` = mean(`rantsoenREkVEM (g/kVEM)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenREkVEM (g/kVEM)`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, y = 220, label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  geom_point(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenREkVEM (g/kVEM)`, color = grondsoort),size = 2) +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenP gehalte (g/kg)`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "REkVEM rantsoen (g/kVEM)") +
  theme_minimal() +
  facet_wrap(.~"Rantsoen")+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(67,235)


# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar, y = `REkVEM (g/kVEM)`, group = grondsoort, color = grondsoort,linetype = grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  # Extend y-axis per facet to same height
  #geom_blank(aes(y = new_ymin_rounded)) +
  #geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(
    values = c(
      "Klei"        = "#213b73",
      "Veen"        = "#1c6c30",
      "Zand"        = "#e29f02",
      "Koe & Eiwit" = "#cf641c"
    ),
    breaks = c("Klei", "Veen", "Zand", "Koe & Eiwit")  # gewenste volgorde
  )+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid")) +
  ylab("REkVEM (g/kVEM)") +
  ylim(67,235)+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

p2 <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  widths = c(0.3, 0.8),       # 20% and 80%
  common.legend = TRUE,
  legend = "bottom"            # place legend under both plots
)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenVEMkREgehalte2.jpg", plot = p2, width = 11.5, height =4.5, dpi = 300)


# 3️⃣ Plot with geom_blank() to enforce y-limits########################################################
p2 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 
            #%>% filter(grondsoort == "Koe & Eiwit") %>%
             #filter(grondsoortlable == "Koe & Eiwit")
            , 
            aes(x = jaar, y = `rantsoenREkVEM (g/kVEM)`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, 
           y = 174, 
           label = "Startjaar\nKoe & Eiwit", 
           hjust = -0.1, 
           vjust = 0, 
           color = "gray60",
           size=3.5) +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenP gehalte (g/kg)`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "solid", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "REkVEM rantsoen (g/kVEM)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  )   +
  geom_text(
    data = labels_df 
    #%>% filter(grondsoort == "Koe & Eiwit")
    ,
    aes(
      x = jaar - 0.2, 
      y = `rantsoenREkVEM (g/kVEM)`,
      label = grondsoortlable,
      color = grondsoort
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE,
    position = position_nudge(y = c(-0.5, 0.5,0, 0,0))  # give each line a different nudge
  )+
  xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  ylim(155,175.5)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenVEMkREgehalte.jpg", 
       plot = p2, width = 6, height =3.5, dpi = 300)






#P-gehalte grondsoort#################################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))) %>%
  bind_rows(
    datameans1 %>%
      filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
      group_by(jaar, voedermiddel) %>% 
      reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
      mutate(grondsoort = "Koe & Eiwit") 
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))%>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(grondsoort == "Koe & Eiwit" ~ 0.8, TRUE ~ 1)) %>%
  mutate(linetype_group = case_when(grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
                                    TRUE ~ "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = case_when(grondsoort == "Klei" ~ "Klei",grondsoort == "Veen" ~ "Veen",grondsoort == "Zand" ~ "Zand",grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"))



labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_line(data = data2 %>% filter(grondsoort != "Koe & Eiwit"), aes(x = jaar,y = `rantsoenVEM/kg DS`, color = grondsoort,linetype = grondsoortlable),size = 1.2) +
  annotate("text", x = 2022, y = 1100, label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0, color = "gray60") +
  #geom_text(data = labels_df, aes(x = jaar - 0.2, y = `rantsoenVEM/kg DS`,label = grondsoortlable,color = grondsoort), hjust = 0,vjust = 0.5, size = 3.5, fontface = "bold",show.legend = FALSE,position = position_nudge(y = c(0, 0,0, -1)))+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(values = c("Koe & Eiwit" = "dashed", "Veen" = "solid", "Klei" = "dashed", "Zand" = "solid"),name = NULL,guide = "none" ) +
  labs(y = "VEM/kg DS gehalte rantsoen") +
  theme_minimal() +
  facet_wrap(.~"Rantsoen")+
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(875,1125)


# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph %>% filter(grondsoort != "Koe & Eiwit"),
             aes(x = jaar, y = `VEM/kg DS`,
                 group = grondsoort,
                 color = grondsoort,
                 linetype = grondsoort)) +  # map linetype to same variable
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  scale_color_manual(
    values = c(
      "Klei"        = "#213b73",
      "Veen"        = "#1c6c30",
      "Zand"        = "#e29f02",
      "Koe & Eiwit" = "#cf641c"
    )
  ) +
  scale_linetype_manual(
    values = c("Koe & Eiwit" = "dashed", 
               "Veen" = "solid", 
               "Klei" = "dashed", 
               "Zand" = "solid")
  ) +
  ylab("VEM/kg DS") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(875,1125)
 p2 <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  widths = c(0.3, 0.8),       # 20% and 80%
  common.legend = TRUE,
  legend = "bottom"            # place legend under both plots
)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenVEM2.jpg", plot = p2, width = 11.5, height =4, dpi = 300)



#VEM-gehalte#Meeting!!!##############################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")))

# 1️⃣ Compute the largest range and new y-limits per facet
# Step 1: Compute min and max per group
facet_limits <- Graph %>%
  group_by(voedermiddel) %>%
  summarise(
    ymin = min(`VEM/kg DS`, na.rm = TRUE),
    ymax = max(`VEM/kg DS`, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Compute largest range across all facets
max_range <- facet_limits %>%
  mutate(range = ymax - ymin) %>%
  summarise(max_range = max(range)) %>%
  pull(max_range)

# Step 3: Compute midpoint and preliminary limits
facet_limits <- facet_limits %>%
  mutate(
    midpoint = (ymin + ymax)/2,
    new_ymin = midpoint - max_range/2,
    new_ymax = midpoint + max_range/2
  ) %>%
  # Step 4: Round ymin down to nearest multiple of 20 and adjust ymax accordingly
  mutate(
    new_ymin_rounded = floor(new_ymin / 10) * 10,
    new_ymax_rounded = new_ymax + (new_ymin_rounded - new_ymin)
  )

# 2️⃣ Join new y-limits back to original data
Graph2 <- Graph %>%
  left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")

# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph2, aes(x = jaar, y = `VEM/kg DS`, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  # Extend y-axis per facet to same height
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  ylab("VEM/kg DS") +
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groeplinevoedermiddelenvemds2.jpg", plot = p2, width = 12, height =4, dpi = 300)

ggplot(datameans1 %>% filter(voedermiddel != "Melkproducten") %>% filter(`VEM/kg DS` < 3000)
       %>% filter(voedermiddel != "Overig ruwvoer"), aes(x=jaar, y=`VEM/kg DS`, group= jaar)) +
  geom_boxplot(outliers = FALSE)+
  geom_jitter(width=0.2, alpha=0.6)+
  facet_wrap(.~voedermiddel, nrow=1) +
  theme_minimal()
  
ggplot(datameans1 %>% distinct(jaar, bedrijfID, .keep_all=TRUE), aes(x=jaar, y=`rantsoenVEM/kg DS`, group= jaar)) +
  geom_boxplot(outliers = FALSE)+
  geom_jitter(width=0.2, alpha=0.6)+
  theme_minimal()



#P-gehalte#################################################################################################################################
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`VEM/kg DS` = (mean(`VEM/kg DS`, na.rm = T))) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")))

# 1️⃣ Compute the largest range and new y-limits per facet
# Step 1: Compute min and max per group
facet_limits <- Graph %>%
  group_by(voedermiddel) %>%
  summarise(
    ymin = min(`VEM/kg DS`, na.rm = TRUE),
    ymax = max(`VEM/kg DS`, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Compute largest range across all facets
max_range <- facet_limits %>%
  mutate(range = ymax - ymin) %>%
  summarise(max_range = max(range)) %>%
  pull(max_range)

# Step 3: Compute midpoint and preliminary limits
facet_limits <- facet_limits %>%
  mutate(
    midpoint = (ymin + ymax)/2,
    new_ymin = midpoint - max_range/2,
    new_ymax = midpoint + max_range/2
  ) %>%
  # Step 4: Round ymin down to nearest multiple of 20 and adjust ymax accordingly
  mutate(
    new_ymin_rounded = floor(new_ymin / 5) * 5,
    new_ymax_rounded = new_ymax + (new_ymin_rounded - new_ymin)
  )

# 2️⃣ Join new y-limits back to original data
Graph2 <- Graph %>%
  left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")

# 3️⃣ Plot with geom_blank() to enforce y-limits
p2 <- ggplot(Graph2, aes(x = jaar, y = `VEM/kg DS`, group = grondsoort, color = grondsoort, linetype=grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  # Extend y-axis per facet to same height
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  ylab("VEM/kg DS") +
  scale_color_manual(values = c(
    "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
  )) +
  scale_linetype_manual(values = c(
    "Zand" = "solid", "Klei" = "dashed", "Veen" = "solid"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Regiodagen/10_09/groepgrondsoortlijnvoedermiddelVEM.jpg", plot = p2, width = 12, height = 4, dpi = 300)


Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(bedrijfID, jaar, Typebedrijf, voedermiddel) %>% 
  summarise(RE_per_farm = mean(`aandeel (%)` / 100 * `RE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  summarise(`RE (g/kg DS)` = mean(RE_per_farm, na.rm = TRUE), .groups = "drop") %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")))



p3 <- ggplot(Graph, aes(x = jaar, y = `RE (g/kg DS)`, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "fixed", nrow=1) +
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/10_09/groeplinevoedermiddelen excl.jpg", plot = p3, width = 12, height = 4, dpi = 300)

##grondsoorten#########################################################################################################################
Graph <- final_df %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean(`opname (kg DS/koe/dag)`, na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")))

p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = grondsoort, color = grondsoort, linetype = grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(limits = c(0, 8)) +
  scale_color_manual(values = c(
    "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
  )) +
  scale_linetype_manual(values = c(
    "Zand" = "solid", "Klei" = "dashed", "Veen" = "solid"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 

p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = grondsoort, color=grondsoort, linetype = grondsoort)) + theme_bw() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("RE gehalte (kg/gDS)")+
  scale_color_manual(values = c(
    "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
  )) +
  scale_linetype_manual(values = c(
    "Zand" = "solid", "Klei" = "dashed", "Veen" = "solid"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/groeplinegrondsoorten.jpg", plot = p3, width = 12, height = 5.5, dpi = 300)
##grondsoorten#########################################################################################################################
REgroup_2024 <- final_df %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, REgroup) %>%
  distinct()

Graph <- final_df %>%
  left_join(REgroup_2024, by = "bedrijfID", suffix = c("", "_2024")) %>%
  mutate(REgroup = REgroup_2024)  %>%# Overwrite original REgroup with 2024 version
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, REgroup, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean(`opname (kg DS/koe/dag)`, na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
         REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156")))

p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = REgroup, color = REgroup)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(limits = c(0, 8.5)) +
  scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 

p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = REgroup, color=REgroup)) + theme_bw() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("RE gehalte (kg/gDS)")+
  scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/groeplineREgroup.jpg", plot = p3, width = 11, height = 6.5, dpi = 300)

######################################################################################################################################################################


bedrijfID_list <- c(92,85,81,55,246,227,214,199,193,189,187,181,178,156,149,145,140,115,220)

library(dplyr)
library(ggplot2)
library(ggpubr)

bedrijfID_list <- c(92,123,164,104)

# Prepare data
final_df <- datameans1 %>% 
  mutate(
    voedermiddel = factor(voedermiddel, levels = c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
    Typebedrijf = factor(Typebedrijf, c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
  )

Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")) %>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(
    `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
    `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
    nrFarms = length(unique(bedrijfID)),
    opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm = TRUE),
    RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE)
  ) %>%
  mutate(
    voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
    Typebedrijf = factor(Typebedrijf, c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"))
  )

# Define background colors
bg_colors <- c(
  "Stabiel laag" = "#A9E3BD",
  "Doel gehaald" = "#A9CAFB",
  "Doel niet gehaald" = "#FFE5BE",
  "Stabiel hoog" = "#FCC8CA"
)


for (bedrijfID_val in bedrijfID_list) {
  
  naam_label <- farmer_rows %>%
    mutate(Naam_ID = paste(naam, bedrijfID, sep = " & ")) %>%
    pull(Naam_ID) %>%
    unique() %>%
    paste(collapse = ", ")
  
  Typebedrijf_label <- farmer_rows %>%
    mutate(Typebedrijf= Typebedrijf) %>%
    pull(Typebedrijf) %>%
    unique() 
  
  grond_label <- farmer_rows %>%
    mutate(grondsoort = grondsoort) %>%
    pull(grondsoort) %>%
    unique() 
  
  intens_label <- farmer_rows %>%
    mutate(intensiteitcat = intensiteitcat) %>%
    pull(intensiteitcat) %>%
    unique() 
  
  RE_values <- sapply(2020:2024, function(y) {
    val <- datameans1 %>% filter(bedrijfID == bedrijfID_val, jaar == y) %>% 
      pull(`rantsoenRE gehalte (g/kg DS)`) %>% unique()
    if(length(val)==0) return(NA) else return(val)
  })
  
  # Create a legend label for the farmer line
  final_df_plot <- final_df %>% 
    filter(bedrijfID == bedrijfID_val) %>% 
    mutate(LegendLabel = paste("Bedrijf:", naam_label))
  
  Graph_plot <- Graph %>% mutate(LegendLabel = Typebedrijf)
  
  # Combine background and farmer line colors
  fg_colors <- c(bg_colors, setNames("black", paste("Bedrijf:", naam_label)))
  
  # --- Compute new y-limits per facet for RE ---
  facet_limits <- Graph_plot %>%
    group_by(voedermiddel) %>%
    summarise(
      ymin = min(RE, na.rm = TRUE),
      ymax = max(RE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      range = ymax - ymin,
      max_range = max(range),
      midpoint = (ymin + ymax)/2,
      new_ymin = midpoint - max_range/2,
      new_ymax = midpoint + max_range/2,
      new_ymin_rounded = floor(new_ymin / 100) * 100,
      new_ymax_rounded = new_ymax + (new_ymin_rounded - new_ymin)
    )
  
# --- Selecteer de voedermiddelen die je wilt tonen + 
  selected_voedermiddel <- c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten") # pas aan wat je wilt

Graph_plot <- Graph_plot %>% filter(voedermiddel %in% selected_voedermiddel) 
final_df_plot <- final_df_plot %>% filter(voedermiddel %in% selected_voedermiddel) 
facet_limits <- facet_limits %>% filter(voedermiddel %in% selected_voedermiddel)
  
  # p1: Voeropname
  p1 <- ggplot() +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_line(data = Graph_plot, aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel), size = 1) +
    geom_point(data = Graph_plot, aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel), size = 2) +
    geom_line(data = final_df_plot, aes(x = jaar, y = `opname (kg DS/koe/dag)`, group = LegendLabel, color = LegendLabel), size = 1.2) +
    geom_point(data = final_df_plot, aes(x = jaar, y = `opname (kg DS/koe/dag)`, group = LegendLabel, color = LegendLabel), size = 2.5) +
    facet_wrap(~ voedermiddel, nrow = 1) +
    ylab("Voeropname (kgDS/dier/dag)") +
    ylim(0,11) +
    scale_color_manual(values = fg_colors) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11)
    )

  
  # --- Join facet-specific y-limits to the plotting data ---
  Graph_plot <- Graph_plot %>%
    left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")
  final_df_plot <- final_df_plot %>%
    left_join(facet_limits %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel")
  
  # --- Updated p2 plot ---
  p2 <- ggplot() +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
    geom_line(data = Graph_plot, aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel), size = 1) +
    geom_point(data = Graph_plot, aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel), size = 2) +
    geom_line(data = final_df_plot, aes(x = jaar, y = `RE gehalte (g/kg DS)`, group = LegendLabel, color = LegendLabel), size = 1.2) +
    geom_point(data = final_df_plot, aes(x = jaar, y = `RE gehalte (g/kg DS)`, group = LegendLabel, color = LegendLabel), size = 2.5) +
    # Use geom_blank with the joined limits
    geom_blank(data = Graph_plot, aes(y = new_ymin_rounded)) +
    geom_blank(data = Graph_plot, aes(y = new_ymax_rounded)) +
    facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
    ylab("RE gehalte (g/kg DS)") +
    scale_color_manual(values = fg_colors) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 11),
      axis.ticks.x = element_line(color = "black"),
      legend.text = element_text(size = 11),
      legend.position = "bottom"
    )
  
  # Combine plots
  combined_plot <- ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")
  
  # Add annotation
  final_plot <- 
    annotate_figure(combined_plot, 
                    top = text_grob( paste0(naam_label, "  |  ", 
                                             Typebedrijf_label, "  |  Klasse: ", 
                                             grond_label, " & ", intens_label, 
                                             "\nRE-trend (2020 t/m 2024): ", paste(RE_values, collapse="; ") ), size = 12, ) )
  
  # Save
  file_path <- paste0("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/individuelboer/grafieken/bedrijfID_", bedrijfID_val, "_voedergrafieken.jpg")
  ggsave(file_path, final_plot, width = 11, height = 6.5)
}



#get the data____________________________________________________________________________________________
datameans1 <- datameans %>% 
  left_join(
    heatplots %>% select(naam, jaar, Typebedrijf),
    by = c("naam", "jaar")
  )
tussenrapportage <- datameans1 %>%
  filter(jaar == 2024) %>%
  select(`rantsoenRE gehalte (g/kg DS)`,
         `rantsoenVEM/kg DS`,
         ureum,
         `fpcmProductie per koe (kg)`,
         `vet (%)`, 
         `eiwit (%)`,
         jongvee_per_10_melkkoe,
         `Zomerstalvoeren (kgDS/koe/dag)`,
         `Weidegras (kgDS/koe/dag)`,
         `Saldo (euro/100kg FPCM)`,
         `Saldo (euro/koe/dag)`,
         `Voerkosten (euro/koe/dag)`,
         `prijsvoer (euro/kg ds)`,
         `Melkopbrengst (euro/koe/day)`,
         intensiteit,
         `Totaal_beweiding(uur)`,
         voedermiddel,
         `bedrijfID`,
         `aandeel (%)`,
         `RE gehalte (g/kg DS)`,
         `Melk per koe (kg)`,
         grondsoort,
         Typebedrijf,
         nkoeien,
         `rantsoen opname incl. jongvee (kg DS)`)%>%
  mutate(`aandeel (%)` = ifelse(is.na(`aandeel (%)`), 0, `aandeel (%)`),
         `RE gehalte (g/kg DS)` = ifelse(`aandeel (%)`==0, NA, `RE gehalte (g/kg DS)`),
         `prijsvoer (euro/kg ds)` = ifelse(`aandeel (%)`==0, NA, `prijsvoer (euro/kg ds)`))%>%
  pivot_wider(
    names_from = voedermiddel,
    values_from = c(`aandeel (%)`, `RE gehalte (g/kg DS)`,`prijsvoer (euro/kg ds)`),
    names_glue = "{voedermiddel}_{.value}")%>%
  select(-bedrijfID)

#calculate means---------------------------------------------------------------------------------
Total <- tussenrapportage
Total1 <- Total  


#List of column names to process
re_column <- c('rantsoenRE gehalte (g/kg DS)',
               'Saldo (euro/100kg FPCM)',
               'Saldo (euro/koe/dag)',
               'Snijmais_aandeel (%)', 
               'Vers gras_aandeel (%)',
               'ureum',
               'fpcmProductie per koe (kg)',
               'Typebedrijf') #select grouping variables
digitone <- c("ureum", "vet (%)", "eiwit (%)", "jongvee_per_10_melkkoe") #select variables with one digit after the decimal point

ordertabletussenraportage <- c('rantsoenRE gehalte (g/kg DS)',
                               'rantsoenVEM/kg DS',
                               'ureum',
                               'fpcmProductie per koe (kg)',
                               'Melk per koe (kg)',
                               'vet (%)', 
                               'eiwit (%)',
                               'jongvee_per_10_melkkoe',
                               'intensiteit',
                               'Totaal_beweiding(uur)',
                               'Zomerstalvoeren (kgDS/koe/dag)',
                               'Weidegras (kgDS/koe/dag)',
                               'Vers gras_aandeel (%)',
                               'Graskuil_aandeel (%)',
                               'Snijmais_aandeel (%)',
                               'Krachtvoer_aandeel (%)',
                               'Bijproducten_aandeel (%)',
                               'Klei (n)',
                               'Zand (n)',
                               'Veen (n)',
                               'Vers gras_RE gehalte (g/kg DS)',
                               'Graskuil_RE gehalte (g/kg DS)',
                               'Snijmais_RE gehalte (g/kg DS)',
                               'Krachtvoer_RE gehalte (g/kg DS)',
                               'Bijproducten_RE gehalte (g/kg DS)',
                               "Stabiel laag n=29 (n)", 
                               "Doel gehaald n=35 (n)", 
                               "Doel niet gehaald n=58 (n)", 
                               "Stabiel hoog n=27 (n)",
                               "nkoeien",
                               "rantsoen opname incl. jongvee (kg DS)",
                               'Saldo (euro/100kg FPCM)',
                               'Saldo (euro/koe/dag)',
                               'Voerkosten (euro/koe/dag)',
                               'prijsvoer (euro/kg ds)',
                               'Melkopbrengst (euro/koe/day)') #select teh other variables, and determine order.

# Function to process each column using the statistical tests (ANOVA/Kruskal-Wallis/Chi-square)
process_column <- function(re_column, Total) {
  Total1 <- Total %>%
    filter(!is.na(.data[[re_column]])) %>%
    mutate(Group = if (is.numeric(.data[[re_column]])) {
      ntile(.data[[re_column]], 4) %>%
        factor(labels = c("Laag", "Laag-Middel", "Middel-Hoog", "Hoog"))
    } else {
      as.factor(.data[[re_column]])
    }) %>%
    filter(!is.na(Group))
  
  Total1$Group <- factor(Total1$Group)
  
  Counts_v <- Total1 %>%
    group_by(Group) %>%           # Group by 'Group' variable
    count() %>%                   # Count the number of observations in each group
    ungroup() %>%                 # Ungroup to return to the original data structure
    pivot_wider(names_from = Group,   # Pivot data to wide format, creating new columns based on 'Group'
                values_from = n,       # Fill columns with the count values (n)
                values_fill = list(n = 0)) %>%  # Replace missing values with 0
    mutate(variable = "Aantal observaties (n)") 
  
  all_results <- list()
  
  for (thisdoesnotwork in setdiff(names(Total1), c("Group"))) {
    if (is.numeric(Total1[[thisdoesnotwork]])) {
      result <- Total1 %>%
        group_by(Group)  %>%
        summarise(mean_value = mean(.data[[thisdoesnotwork]], na.rm = TRUE)) %>% 
        pivot_wider(names_from = Group, values_from = mean_value) %>%
        mutate(variable = thisdoesnotwork)
      
      normality_tests <- Total1 %>%
        group_by(Group) %>%
        summarise(shapiro_p = shapiro.test(.data[[thisdoesnotwork]])$p.value) %>%
        pull(shapiro_p)
      
      levene_p <- leveneTest(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)$`Pr(>F)`[1]
      
      if (any(normality_tests < 0.05) || levene_p < 0.05) {
        test_result <- kruskal.test(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- test_result$p.value
        test_type <- "Kruskal-Wallis"
      } else {
        anova_result <- aov(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
        test_type <- "ANOVA"
      }
      
    } else {
      result <- Total1 %>%
        group_by(Group, .data[[thisdoesnotwork]]) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = Group, values_from = count, values_fill = list(count = 0)) %>%
        mutate(variable = paste(.data[[thisdoesnotwork]], " (n)", sep = "")) %>%
        select(-c(1))
      
      chi_square_test <- chisq.test(table(Total1$Group, Total1[[thisdoesnotwork]]))
      p_value <- chi_square_test$p.value
      test_type <- "Chi-squared"
    }
    
    significance <- ifelse(p_value < 0.05, "*", "")
    
    result <- result %>%
      mutate(p_waarde = round(p_value, 2), significant = significance, test_type = test_type) %>%
      select(variable, everything(), p_waarde, significant, test_type)
    
    all_results[[thisdoesnotwork]] <- result
  }
  
  final_result <- bind_rows(all_results)
  final_result <- bind_rows(final_result,   Counts_v)
  final_result <- final_result %>%
    mutate(
      across(
        .cols = where(is.numeric) & !matches("p_waarde"),
        .fns = ~case_when(
          variable %in% digitone ~ round(.x, 1),
          TRUE ~ round(.x, 0)
        )
      ),
      p_waarde = round(p_waarde, 2)
    ) %>%
    filter(variable %in% c("Aantal observaties (n)", ordertabletussenraportage)) %>%
    mutate(
      variable = factor(variable, levels = c("Aantal observaties (n)", re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  
  
  return(final_result)
}

# Function to process only numeric columns for linear regression analysis
process_column_lm <- function(re_column, Total) {
  Total1 <- Total %>% filter(!is.na(.data[[re_column]]))
  
  if (!is.numeric(Total1[[re_column]])) {
    message(paste("Skipping linear regression for non-numeric column:", re_column))
    return(tibble(variable = re_column, p_waarde_lin = NA, r_squared = NA))
  }
  
  all_results <- list()
  
  for (response_col in setdiff(names(Total1), re_column)) {
    Total2 <- Total1 %>% filter(!is.na(.data[[response_col]]))
    
    if (nrow(Total2) < 10) next
    
    if (is.numeric(Total2[[response_col]])) {
      formula <- as.formula(paste0("`", response_col, "` ~ `", re_column, "`"))
      model <- lm(formula, data = Total2)
      summary_model <- summary(model)
      
      estimate <- coef(summary_model)[2, "Estimate"]
      p_value <- coef(summary_model)[2, "Pr(>|t|)"]
      r_squared <- summary_model$r.squared
      
      result <- tibble(
        variable = response_col,
        p_waarde = round(p_value, 3),
        r_squared = round(r_squared, 3),
        significant = ifelse(p_value < 0.05, "*", ""),
        test_type = "Linear Regression"
      )
    } else {
      result <- tibble(
        variable = response_col,
        p_waarde = NA,
        r_squared = NA,
        significant = "",
        test_type = "Non-numeric"
      )
    }
    
    all_results[[response_col]] <- result
  }
  
  final_result <- bind_rows(all_results) %>%
    filter(variable %in% ordertabletussenraportage) %>%
    mutate(variable = factor(variable, levels = c(setdiff(ordertabletussenraportage, re_column)))) %>%
    arrange(variable) %>%
    select(variable, p_waarde, r_squared) %>%
    rename(p_waarde_lin = p_waarde)
  
  missing_vars <- setdiff(ordertabletussenraportage, final_result$variable)
  
  if (length(missing_vars) > 0) {
    missing_rows <- tibble(
      variable = missing_vars,
      p_waarde_lin = NA,
      r_squared = NA
    )
    
    final_result <- bind_rows(final_result, missing_rows)
  }
  
  final_result <- final_result %>%
    mutate(
      variable = factor(variable, levels = c(re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  return(final_result)
}

# Merge results and handle non-numeric columns
merge_results <- function(col, Total) {
  stats_result <- process_column(col, Total)
  
  # Skip linear regression for non-numeric columns
  if (is.numeric(Total[[col]])) {
    lm_result <- process_column_lm(col, Total)
    merged_result <- left_join(stats_result, lm_result, by = "variable")
  } else {
    merged_result <- stats_result
  }
  
  return(merged_result)
}

# Process all selected columns and merge results
merged_results_list <- lapply(re_column, function(col) merge_results(col, Total1))

# Function to clean invalid characters in sheet names
clean_sheet_name <- function(name) {
  gsub("[[:punct:][:space:]]+", "_", name)  # Replace spaces and punctuation with underscores
}

# Clean sheet names for saving in Excel
sheet_names <- sapply(re_column, clean_sheet_name)

# Output path for the merged Excel file
output_path <- "C:/Rfiles/WR/Koe en eiwit 2025/R code/Analyzes Eiwit monitor/Tables means and counts all data/tussenrapportage_merged_23_7.xlsx"

# Save to Excel
write_xlsx(setNames(merged_results_list, sheet_names), path = output_path)

#PCA analyze____________________________________________________________________________________________________________________________________________________________
# ===========================
# 1️⃣ Prepare numeric data
# ===========================
numeric_cols <- datameans1 %>%
  mutate(Project_effect = ifelse(jaar %in% 2020:2022, 1, 2)) %>%
  select(where(is.numeric)) %>%
  mutate(across(
    c(contains("gehalte"),
      contains("opname"),
      any_of(c("VEM/kg DS", "REkVEM (g/kVEM)"))),
    ~ replace_na(., 0)
  )) %>%
  
  select(where(~ all(!is.na(.)))) %>%
  
  select(where(~ sd(.) != 0))


complete_cases <- complete.cases(numeric_cols)
pca_data <- numeric_cols[complete_cases, ]


pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)


explained_variance <- summary(pca_result)$importance[2, ]
scree_data <- tibble(
  PC = 1:length(explained_variance),
  Variance_Explained = explained_variance
)

# Plot
p4 <- ggplot(scree_data, aes(x = PC, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(x = PC, y = cumsum(Variance_Explained)), color = "red", size = 1) +
  geom_point(aes(x = PC, y = cumsum(Variance_Explained)), color = "red", size = 3) +
  ggtitle("Scree Plot with Cumulative Variance") +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  theme_minimal()

p1 <- fviz_pca_var(pca_result,
                   col.var = "black",
                   label = "all",
                   alpha.var = 0.3, 
                   labelsize = 3,
                   repel = TRUE,
                   title = "Variable Contributions")

# ===========================
# 6️⃣ Individual biplot colored by grouping variable
# ===========================
# Make sure the grouping vector matches rows in pca_data
habillage_vector <- datameans1$grondsoort[complete_cases]

p2 <- fviz_pca_ind(pca_result,
                   geom = c("point", "arrow"),
                   habillage = habillage_vector,
                   palette = c("#ff4d4d", "#ffc14d", "#3385ff"),
                   addEllipses = TRUE,
                   ellipse.level = 0.95,
                   legend.title = "Typebedrijf",
                   title = "Bedrijven en clusters")

# ===========================
# 7️⃣ Display plots
# ===========================
print(p4)
print(p1)
print(p2)


numeric_cols <- datameans1 %>%
  mutate(Project_effect = ifelse(jaar %in% 2020:2022, 
                                 1, 
                                 2)) %>%
  select(where(is.numeric)) %>%
  mutate(across(
    c(
      contains("gehalte"),      # all columns containing "gehalte"
      contains("opname"),       # all columns containing "opname"
      any_of(c("VEM/kg DS", "REkVEM (g/kVEM)"))  # exact column names
    ),
    ~ replace_na(., 0)          # replace NA with 0
  )) %>%
  select(where(~ all(!is.na(.)))) %>%
  select(where(~ sd(.) != 0)) 

complete_cases <- complete.cases(numeric_cols) 

# Filter the data to include only complete cases
pca_data <- numeric_cols[complete_cases, ]

# Run PCA (scale and center the data)
pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)

# Calculate explained variance
explained_variance <- summary(pca_result)$importance[2, ]

# Create a data frame for the scree plot
scree_data <- tibble(
  PC = 1:length(explained_variance),
  Variance_Explained = explained_variance
)

# Create a scree plot using ggplot2
p4<- ggplot(scree_data, aes(x = PC, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(x = PC, y = cumsum(Variance_Explained)), color = "red", size = 1) +
  geom_point(aes(x = PC, y = cumsum(Variance_Explained)), color = "red", size = 3) +
  ggtitle("Scree Plot with Cumulative Variance") +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  theme_minimal()

# Create a biplot without numbers using factoextra
p1 <- fviz_pca_var(pca_result,
             col.var = "black",
             label = "all",
             alpha.var = 0.3, 
             labelsize = 3,         # Smaller text (default is 4 or 5)
             repel = TRUE,          # Prevent overlapping
             title = "Variable")

p2 <- fviz_pca_ind(pca_result,
             geom = c("point", "arrow"), 
             habillage = datameans1$grondsoort[complete.cases(datameans1 %>% select(where(is.numeric)))],  # Grouping variable
             palette = c("#ff4d4d", "#ffc14d", "#3385ff"),  # Color palette for groups
             addEllipses = TRUE,
             ellipse.level = 0.95,
             legend.title = "Typebedrijf",
             title = "Bedrijven en clusters")

p3<- fviz_pca_biplot(pca_result,
                col.var = "black",      # Color for variable arrows
                habillage = datameans1$grondsoort[complete.cases(datameans1 %>% select(where(is.numeric)))],  # Grouping variable
                palette = c("#ff4d4d", "#ffc14d", "#3385ff"),  # Color palette for groups
                addEllipses = TRUE,              # Add ellipses around groups
                ellipse.level = 0.95,            # Confidence level for ellipses
                alpha.var = 0.3, 
                labelsize = 2,
                title = "Variable en clusters",
                label = "var",                  # Keep variable labels
                pointsize = NA)         

p5<- ggarrange(p2, p3, 
               nrow = 1, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/PCA/Explainvariance.jpg", plot = p1, width = 11, height = 6.5, dpi = 300)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/PCA/Variable.jpg", plot = p4, width = 11, height = 6.5, dpi = 300)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/PCA/Cluster.jpg", plot = p5, width = 11, height = 6.5, dpi = 300)





#get the data____________________________________________________________________________________________
datameans1 <- datameans %>% 
  left_join(
    heatplots %>% select(naam, jaar, Typebedrijf),
    by = c("naam", "jaar")
  ) 

tussenrapportage <- datameans1 %>%
  select(-Zomerstalvoeding,
         -`VEM/kg DS`, 
         -`REkVEM (g/kVEM)`, 
         -`opname incl. jongvee (kg DS)`, 
         -`opnamekoe (kg DS)`, 
         -`opname per koe`,
         -`opname (kg DS/koe/dag)`,
         -`Aantalbeweiding_onbeperkt_zomerstal(uur)`,
         -`Aantalbeweiding_beperkt_zomerstal(uur)`,
         -REgroup,
         -intensiteitcat,
         -naam) %>%
  mutate(
    `aandeel (%)` = ifelse(is.na(`aandeel (%)`), 0, `aandeel (%)`),  # Fix the closing parenthesis here
    `RE gehalte (g/kg DS)` = ifelse(`aandeel (%)` == 0, NA, `RE gehalte (g/kg DS)`)
  ) %>%
  pivot_wider(
    names_from = voedermiddel,
    values_from = c(`aandeel (%)`, `RE gehalte (g/kg DS)`),
    names_glue = "{voedermiddel}_{.value}"
  )
  
  
  
  
  #calculate means---------------------------------------------------------------------------------
Total <- tussenrapportage
Total1 <- Total  


#List of column names to process
re_column <- c('jaar') #select grouping variables
digitone <- c("ureum", "vet (%)", "eiwit (%)", "jongvee_per_10_melkkoe") #select variables with one digit after the decimal point
digittwo <- 
ordertabletussenraportage1 <- c('rantsoenRE gehalte (g/kg DS)',
                                'rantsoenVEM/kg DS',
                                'ureum',
                                'fpcmProductie per koe (kg)',
                                'melkpkoe',
                                'vet (%)', 
                                'eiwit (%)',
                                'jongvee_per_10_melkkoe',
                                'intensiteit',
                                'Totaal_beweiding(uur)',
                                'Vers gras_aandeel (%)',
                                'Graskuil_aandeel (%)',
                                'Snijmais_aandeel (%)',
                                'Krachtvoer_aandeel (%)',
                                'Bijproducten_aandeel (%)',
                                'Klei (n)',
                                'Zand (n)',
                                'Veen (n)',
                                'Vers gras_RE gehalte (g/kg DS)',
                                'Graskuil_RE gehalte (g/kg DS)',
                                'Snijmais_RE gehalte (g/kg DS)',
                                'Krachtvoer_RE gehalte (g/kg DS)',
                                'Bijproducten_RE gehalte (g/kg DS)',
                                "Stabiel laag n=29 (n)", 
                                "Doel gehaald n=35 (n)", 
                                "Doel niet gehaald n=58 (n)", 
                                "Stabiel hoog n=27 (n)",
                                "nkoeien",
                                "rantsoen opname incl. jongvee (kg DS)")
colnames_v <- colnames(tussenrapportage)

ordertabletussenraportage <- c(ordertabletussenraportage1, setdiff(colnames_v, c(ordertabletussenraportage1)))


# Function to process each column using the statistical tests (ANOVA/Kruskal-Wallis/Chi-square)
process_column <- function(re_column, Total) {
  Total1 <- Total %>%
    filter(!is.na(.data[[re_column]])) %>%
    mutate(Group = if (is.numeric(.data[[re_column]])) {
      ntile(.data[[re_column]], 4) %>%
        factor(labels = c("Laag", "Laag-Middel", "Middel-Hoog", "Hoog"))
    } else {
      as.factor(.data[[re_column]])
    }) %>%
    filter(!is.na(Group))
  
  Total1$Group <- factor(Total1$Group)
  
  Counts_v <- Total1 %>%
    group_by(Group) %>%           # Group by 'Group' variable
    count() %>%                   # Count the number of observations in each group
    ungroup() %>%                 # Ungroup to return to the original data structure
    pivot_wider(names_from = Group,   # Pivot data to wide format, creating new columns based on 'Group'
                values_from = n,       # Fill columns with the count values (n)
                values_fill = list(n = 0)) %>%  # Replace missing values with 0
    mutate(variable = "Aantal observaties (n)") 
  
  all_results <- list()
  
  for (thisdoesnotwork in setdiff(names(Total1), c("Group"))) {
    if (is.numeric(Total1[[thisdoesnotwork]])) {
      result <- Total1 %>%
        group_by(Group)  %>%
        summarise(mean_value = mean(.data[[thisdoesnotwork]], na.rm = TRUE)) %>% 
        pivot_wider(names_from = Group, values_from = mean_value) %>%
        mutate(variable = thisdoesnotwork)
      
      normality_tests <- Total1 %>%
        group_by(Group) %>%
        summarise(shapiro_p = shapiro.test(.data[[thisdoesnotwork]])$p.value) %>%
        pull(shapiro_p)
      
      levene_p <- leveneTest(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)$`Pr(>F)`[1]
      
      if (any(normality_tests < 0.05) || levene_p < 0.05) {
        test_result <- kruskal.test(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- test_result$p.value
        test_type <- "Kruskal-Wallis"
      } else {
        anova_result <- aov(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
        test_type <- "ANOVA"
      }
      
    } else {
      result <- Total1 %>%
        group_by(Group, .data[[thisdoesnotwork]]) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = Group, values_from = count, values_fill = list(count = 0)) %>%
        mutate(variable = paste(.data[[thisdoesnotwork]], " (n)", sep = "")) %>%
        select(-c(1))
      
      chi_square_test <- chisq.test(table(Total1$Group, Total1[[thisdoesnotwork]]))
      p_value <- chi_square_test$p.value
      test_type <- "Chi-squared"
    }
    
    significance <- ifelse(p_value < 0.05, "*", "")
    
    result <- result %>%
      mutate(p_waarde = round(p_value, 2), significant = significance, test_type = test_type) %>%
      select(variable, everything(), p_waarde, significant, test_type)
    
    all_results[[thisdoesnotwork]] <- result
  }
  
  final_result <- bind_rows(all_results)
  final_result <- bind_rows(final_result,   Counts_v)
  final_result <- final_result %>%
    mutate(
      across(
        .cols = where(is.numeric) & !matches("p_waarde"),
        .fns = ~case_when(
          variable %in% digitone ~ round(.x, 1),
          variable %in% digittwo ~ round(.x, 2),
          TRUE ~ round(.x, 0)
        )
      ),
      p_waarde = round(p_waarde, 2)
    ) %>%
    filter(variable %in% c("Aantal observaties (n)", ordertabletussenraportage)) %>%
    mutate(
      variable = factor(variable, levels = c("Aantal observaties (n)", re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  
  
  return(final_result)
}

# Function to process only numeric columns for linear regression analysis
process_column_lm <- function(re_column, Total) {
  Total1 <- Total %>% filter(!is.na(.data[[re_column]]))
  
  if (!is.numeric(Total1[[re_column]])) {
    message(paste("Skipping linear regression for non-numeric column:", re_column))
    return(tibble(variable = re_column, p_waarde_lin = NA, r_squared = NA))
  }
  
  all_results <- list()
  
  for (response_col in setdiff(names(Total1), re_column)) {
    Total2 <- Total1 %>% filter(!is.na(.data[[response_col]]))
    
    if (nrow(Total2) < 10) next
    
    if (is.numeric(Total2[[response_col]])) {
      formula <- as.formula(paste0("`", response_col, "` ~ `", re_column, "`"))
      model <- lm(formula, data = Total2)
      summary_model <- summary(model)
      
      estimate <- coef(summary_model)[2, "Estimate"]
      p_value <- coef(summary_model)[2, "Pr(>|t|)"]
      r_squared <- summary_model$r.squared
      
      result <- tibble(
        variable = response_col,
        p_waarde = round(p_value, 3),
        r_squared = round(r_squared, 3),
        significant = ifelse(p_value < 0.05, "*", ""),
        test_type = "Linear Regression"
      )
    } else {
      result <- tibble(
        variable = response_col,
        p_waarde = NA,
        r_squared = NA,
        significant = "",
        test_type = "Non-numeric"
      )
    }
    
    all_results[[response_col]] <- result
  }
  
  final_result <- bind_rows(all_results) %>%
    filter(variable %in% ordertabletussenraportage) %>%
    mutate(variable = factor(variable, levels = c(setdiff(ordertabletussenraportage, re_column)))) %>%
    arrange(variable) %>%
    select(variable, p_waarde, r_squared) %>%
    rename(p_waarde_lin = p_waarde)
  
  missing_vars <- setdiff(ordertabletussenraportage, final_result$variable)
  
  if (length(missing_vars) > 0) {
    missing_rows <- tibble(
      variable = missing_vars,
      p_waarde_lin = NA,
      r_squared = NA
    )
    
    final_result <- bind_rows(final_result, missing_rows)
  }
  
  final_result <- final_result %>%
    mutate(
      variable = factor(variable, levels = c(re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  return(final_result)
}

# Merge results and handle non-numeric columns
merge_results <- function(col, Total) {
  stats_result <- process_column(col, Total)
  
  # Skip linear regression for non-numeric columns
  if (is.numeric(Total[[col]])) {
    lm_result <- process_column_lm(col, Total)
    merged_result <- left_join(stats_result, lm_result, by = "variable")
  } else {
    merged_result <- stats_result
  }
  
  return(merged_result)
}

# Process all selected columns and merge results
merged_results_list <- lapply(re_column, function(col) merge_results(col, Total1))

# Function to clean invalid characters in sheet names
clean_sheet_name <- function(name) {
  gsub("[[:punct:][:space:]]+", "_", name)  # Replace spaces and punctuation with underscores
}

# Clean sheet names for saving in Excel
sheet_names <- sapply(re_column, clean_sheet_name)

# Output path for the merged Excel file
output_path <- "C:/Rfiles/WR/Koe en eiwit 2025/R code/Analyzes Eiwit monitor/Tables means and counts all data/tussenrapportage_merged_23_8.xlsx"

# Save to Excel
write_xlsx(setNames(merged_results_list, sheet_names), path = output_path)




#maybe delete###############################################################################
#get the data____________________________________________________________________________________________
datameans1 <- datameans %>% 
  left_join(
    heatplots %>% select(naam, jaar, Typebedrijf),
    by = c("naam", "jaar")
  )
tussenrapportage <- datameans1 %>%
  filter(jaar == 2024) %>%
  select(`rantsoenRE gehalte (g/kg DS)`,
         `rantsoenVEM/kg DS`,
         ureum,
         `fpcmProductie per koe (kg)`,
         `vet (%)`, 
         `eiwit (%)`,
         jongvee_per_10_melkkoe,
         `Zomerstalvoeren (kgDS/koe)`,
         `Weidegras (kgDS/koe)`,
         `Saldo (euro/100kg FPCM)`,
         `Saldo excl. jongvee (euro/koe)`,
         `Voerkosten  excl. jongvee (euro/koe)`,
         `prijsvoer (euro/kg ds)`,
         `Melkopbrengst (euro/koe)`,
         intensiteit,
         `Totaal_beweiding(uur)`,
         voedermiddel,
         `bedrijfID`,
         `aandeel (%)`,
         `RE gehalte (g/kg DS)`,
         `Melk per koe (kg)`,
         grondsoort,
         Typebedrijf,
         nkoeien,
         `rantsoen opname incl. jongvee (kg DS)`)%>%
  mutate(`aandeel (%)` = ifelse(is.na(`aandeel (%)`), 0, `aandeel (%)`),
         `RE gehalte (g/kg DS)` = ifelse(`aandeel (%)`==0, NA, `RE gehalte (g/kg DS)`),
         `prijsvoer (euro/kg ds)` = ifelse(`aandeel (%)`==0, NA, `prijsvoer (euro/kg ds)`))%>%
  pivot_wider(
    names_from = voedermiddel,
    values_from = c(`aandeel (%)`, `RE gehalte (g/kg DS)`,`prijsvoer (euro/kg ds)`),
    names_glue = "{voedermiddel}_{.value}")%>%
  select(-bedrijfID,`Melkproducten_prijsvoer (euro/kg ds)`)

#calculate means---------------------#calculate means---------------------------------------------------------------------------------
Total <- tussenrapportage
Total1 <- Total[, colSums(!is.na(Total)) > 0]


#List of column names to process
re_column <- c('rantsoenRE gehalte (g/kg DS)',
               'Saldo (euro/100kg FPCM)',
               'Saldo excl. jongvee (euro/koe)',
               'Snijmais_aandeel (%)', 
               'Vers gras_aandeel (%)',
               'ureum',
               'fpcmProductie per koe (kg)',
               'Typebedrijf',
               'grondsoort') #select grouping variables
digitone <- c("ureum", "vet (%)", "eiwit (%)", "jongvee_per_10_melkkoe") #select variables with one digit after the decimal point
digittwo <- c('Vers gras_prijsvoer (euro/kg ds)',
                                             'Graskuil_prijsvoer (euro/kg ds)',
                                             'Snijmais_prijsvoer (euro/kg ds)',
                                             'Krachtvoer_prijsvoer (euro/kg ds)',
                                             'Bijproducten_prijsvoer (euro/kg ds)')
ordertabletussenraportage <- c('rantsoenRE gehalte (g/kg DS)',
                               'rantsoenVEM/kg DS',
                               'ureum',
                               'fpcmProductie per koe (kg)',
                               'Melk per koe (kg)',
                               'vet (%)', 
                               'eiwit (%)',
                               'jongvee_per_10_melkkoe',
                               'intensiteit',
                               'Totaal_beweiding(uur)',
                               'Zomerstalvoeren (kgDS/koe)',
                               'Weidegras (kgDS/koe)',
                               'Vers gras_aandeel (%)',
                               'Graskuil_aandeel (%)',
                               'Snijmais_aandeel (%)',
                               'Krachtvoer_aandeel (%)',
                               'Bijproducten_aandeel (%)',
                               'Klei (n)',
                               'Zand (n)',
                               'Veen (n)',
                               'Vers gras_RE gehalte (g/kg DS)',
                               'Graskuil_RE gehalte (g/kg DS)',
                               'Snijmais_RE gehalte (g/kg DS)',
                               'Krachtvoer_RE gehalte (g/kg DS)',
                               'Bijproducten_RE gehalte (g/kg DS)',
                               "Stabiel laag n=29 (n)", 
                               "Doel gehaald n=35 (n)", 
                               "Doel niet gehaald n=58 (n)", 
                               "Stabiel hoog n=27 (n)",
                               "nkoeien",
                               "rantsoen opname incl. jongvee (kg DS)",
                               'Saldo (euro/100kg FPCM)',
                               'Saldo excl. jongvee (euro/koe)',
                               'Voerkosten (euro/koe)',
                               'prijsvoer (euro/kg ds)',
                               'Melkopbrengst (euro/koe)',
                               'Vers gras_prijsvoer (euro/kg ds)',
                               'Graskuil_prijsvoer (euro/kg ds)',
                               'Snijmais_prijsvoer (euro/kg ds)',
                               'Krachtvoer_prijsvoer (euro/kg ds)',
                               'Bijproducten_prijsvoer (euro/kg ds)') #select teh other variables, and determine order.



# Function to process each column using the statistical tests (ANOVA/Kruskal-Wallis/Chi-square)
process_column <- function(re_column, Total) {
  Total1 <- Total %>%
    filter(!is.na(.data[[re_column]])) %>%
    mutate(Group = if (is.numeric(.data[[re_column]])) {
      ntile(.data[[re_column]], 4) %>%
        factor(labels = c("Laag", "Laag-Middel", "Middel-Hoog", "Hoog"))
    } else {
      as.factor(.data[[re_column]])
    }) %>%
    filter(!is.na(Group))
  
  Total1$Group <- factor(Total1$Group)
  
  Counts_v <- Total1 %>%
    group_by(Group) %>%           # Group by 'Group' variable
    count() %>%                   # Count the number of observations in each group
    ungroup() %>%                 # Ungroup to return to the original data structure
    pivot_wider(names_from = Group,   # Pivot data to wide format, creating new columns based on 'Group'
                values_from = n,       # Fill columns with the count values (n)
                values_fill = list(n = 0)) %>%  # Replace missing values with 0
    mutate(variable = "Aantal observaties (n)") 
  
  all_results <- list()
  
  for (thisdoesnotwork in setdiff(names(Total1), c("Group"))) {
    if (is.numeric(Total1[[thisdoesnotwork]])) {
      result <- Total1 %>%
        group_by(Group)  %>%
        summarise(mean_value = mean(.data[[thisdoesnotwork]], na.rm = TRUE)) %>% 
        pivot_wider(names_from = Group, values_from = mean_value) %>%
        mutate(variable = thisdoesnotwork)
      
      normality_tests <- Total1 %>%
        group_by(Group) %>%
        summarise(shapiro_p = tryCatch(
          shapiro.test(.data[[thisdoesnotwork]])$p.value,
          error = function(e) NA_real_
        )) %>%
        pull(shapiro_p)
      
      levene_p <- leveneTest(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)$`Pr(>F)`[1]
      
      normality_flag <- any(normality_tests < 0.05, na.rm = TRUE)
      levene_flag <- !is.na(levene_p) && levene_p < 0.05
      
      if (normality_flag || levene_flag) {
        test_result <- kruskal.test(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- test_result$p.value
        test_type <- "Kruskal-Wallis"
      } else {
        anova_result <- aov(as.formula(paste0("`", thisdoesnotwork, "` ~ Group")), data = Total1)
        p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
        test_type <- "ANOVA"
      }
      
    } else {
      result <- Total1 %>%
        group_by(Group, .data[[thisdoesnotwork]]) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = Group, values_from = count, values_fill = list(count = 0)) %>%
        mutate(variable = paste(.data[[thisdoesnotwork]], " (n)", sep = "")) %>%
        select(-c(1))
      
      chi_square_test <- chisq.test(table(Total1$Group, Total1[[thisdoesnotwork]]))
      p_value <- chi_square_test$p.value
      test_type <- "Chi-squared"
    }
    
    significance <- ifelse(p_value < 0.05, "*", "")
    
    result <- result %>%
      mutate(p_waarde = round(p_value, 2), significant = significance, test_type = test_type) %>%
      select(variable, everything(), p_waarde, significant, test_type)
    
    all_results[[thisdoesnotwork]] <- result
  }
  
  final_result <- bind_rows(all_results)
  final_result <- bind_rows(final_result,   Counts_v)
  final_result <- final_result %>%
    mutate(
      across(
        .cols = where(is.numeric) & !matches("p_waarde"),
        .fns = ~case_when(
          variable %in% digitone ~ round(.x, 1),
          variable %in% digittwo ~ round(.x, 2),
          TRUE ~ round(.x, 0)
        )
      ),
      p_waarde = round(p_waarde, 2)
    ) %>%
    filter(variable %in% c("Aantal observaties (n)", ordertabletussenraportage)) %>%
    mutate(
      variable = factor(variable, levels = c("Aantal observaties (n)", re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  
  
  return(final_result)
}

# Function to process only numeric columns for linear regression analysis
process_column_lm <- function(re_column, Total) {
  Total1 <- Total %>% filter(!is.na(.data[[re_column]]))
  
  if (!is.numeric(Total1[[re_column]])) {
    message(paste("Skipping linear regression for non-numeric column:", re_column))
    return(tibble(variable = re_column, p_waarde_lin = NA, r_squared = NA))
  }
  
  all_results <- list()
  
  for (response_col in setdiff(names(Total1), re_column)) {
    Total2 <- Total1 %>% filter(!is.na(.data[[response_col]]))
    
    if (nrow(Total2) < 10) next
    
    if (is.numeric(Total2[[response_col]])) {
      formula <- as.formula(paste0("`", response_col, "` ~ `", re_column, "`"))
      model <- lm(formula, data = Total2)
      summary_model <- summary(model)
      
      estimate <- coef(summary_model)[2, "Estimate"]
      p_value <- coef(summary_model)[2, "Pr(>|t|)"]
      r_squared <- summary_model$r.squared
      
      result <- tibble(
        variable = response_col,
        p_waarde = round(p_value, 3),
        r_squared = round(r_squared, 3),
        significant = ifelse(p_value < 0.05, "*", ""),
        test_type = "Linear Regression"
      )
    } else {
      result <- tibble(
        variable = response_col,
        p_waarde = NA,
        r_squared = NA,
        significant = "",
        test_type = "Non-numeric"
      )
    }
    
    all_results[[response_col]] <- result
  }
  
  final_result <- bind_rows(all_results) %>%
    filter(variable %in% ordertabletussenraportage) %>%
    mutate(variable = factor(variable, levels = c(setdiff(ordertabletussenraportage, re_column)))) %>%
    arrange(variable) %>%
    select(variable, p_waarde, r_squared) %>%
    rename(p_waarde_lin = p_waarde)
  
  missing_vars <- setdiff(ordertabletussenraportage, final_result$variable)
  
  if (length(missing_vars) > 0) {
    missing_rows <- tibble(
      variable = missing_vars,
      p_waarde_lin = NA,
      r_squared = NA
    )
    
    final_result <- bind_rows(final_result, missing_rows)
  }
  
  final_result <- final_result %>%
    mutate(
      variable = factor(variable, levels = c(re_column, setdiff(ordertabletussenraportage, re_column)))
    ) %>%
    arrange(variable)
  
  return(final_result)
}

# Merge results and handle non-numeric columns
merge_results <- function(col, Total) {
  stats_result <- process_column(col, Total)
  
  # Skip linear regression for non-numeric columns
  if (is.numeric(Total[[col]])) {
    lm_result <- process_column_lm(col, Total)
    merged_result <- left_join(stats_result, lm_result, by = "variable")
  } else {
    merged_result <- stats_result
  }
  
  return(merged_result)
}

# Process all selected columns and merge results
merged_results_list <- lapply(re_column, function(col) merge_results(col, Total1))

# Function to clean invalid characters in sheet names
clean_sheet_name <- function(name) {
  gsub("[[:punct:][:space:]]+", "_", name)  # Replace spaces and punctuation with underscores
}

# Clean sheet names for saving in Excel
sheet_names <- sapply(re_column, clean_sheet_name)

# Output path for the merged Excel file
output_path <- "C:/Rfiles/WR/Koe en eiwit 2025/R code/Analyzes Eiwit monitor/Tables means and counts all data/total_6_4.xlsx"

# Save to Excel
write_xlsx(setNames(merged_results_list, sheet_names), path = output_path)


##grondsoorten#########################################################################################################################
Graph <- final_df %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")))

library(scales)

p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = grondsoort, color = grondsoort, linetype = grondsoort)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(
    limits = c(0, 10),
    labels = label_number(accuracy = 1)  # afgerond naar hele getallen
  ) +
  scale_color_manual(values = c(
    "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
  )) +
  scale_linetype_manual(values = c(
    "Zand" = "solid", "Klei" = "dashed", "Veen" = "solid"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 


p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = grondsoort, color=grondsoort, linetype = grondsoort)) + theme_bw() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("RE gehalte (kg/gDS)")+
  scale_color_manual(values = c(
    "Zand" = "#E29E00", "Klei" = "#293684", "Veen" = "#1A6B05"
  )) +
  scale_linetype_manual(values = c(
    "Zand" = "solid", "Klei" = "dashed", "Veen" = "solid"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/groeplinegrondsoorten_incl.jpg", plot = p3, width = 11, height = 5.5, dpi = 300)
##grondsoorten#########################################################################################################################
REgroup_2024 <- final_df %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, REgroup) %>%
  distinct()

Graph <- final_df %>%
  left_join(REgroup_2024, by = "bedrijfID", suffix = c("", "_2024")) %>%
  mutate(REgroup = REgroup_2024)  %>%# Overwrite original REgroup with 2024 version
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, REgroup, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")),
         REgroup = factor(REgroup, levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156")))

p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = REgroup, color = REgroup)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 

p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = REgroup, color=REgroup)) + theme_bw() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("RE gehalte (kg/gDS)")+
  scale_color_manual(values = c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c"))+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/groeplineREgroup_incl.jpg", plot = p3, width = 11, height = 6.5, dpi = 300)




#----------------------------------------Line graphs per frarm----------------------------------------------------------------
Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten"))%>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(`rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = T)),
          `fpcmProductie per koe (kg)` = mean(`fpcmProductie per koe (kg)`, na.rm = T),
          nrFarms = length(unique(bedrijfID)),
          opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm=T),
          RE = mean(`RE gehalte (g/kg DS)`, na.rm=T)) %>%
  mutate(voedermiddel = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais","Overig ruwvoer","Krachtvoer","Bijproducten")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel hoog n=27","Doel niet gehaald n=58","Doel gehaald n=35","Stabiel laag n=29"),
                              labels = c("Stabiel hoog", "Doel niet gehaald", "Doel gehaald", "Stabiel laag")))%>%
  mutate(Typebedrijf = factor(Typebedrijf,c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")))

p1 <- ggplot(Graph, aes(x = jaar, y = opname, group = Typebedrijf, color = Typebedrijf)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  ) 

p2 <- ggplot(Graph, aes(x = jaar, y = RE, group = Typebedrijf, color=Typebedrijf)) + theme_bw() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ voedermiddel, scales = "free", nrow=1) +
  ylab("RE gehalte (kg/gDS)")+
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 0, color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.ticks.x = element_line(color = "black"),
    legend.text = element_text(size = 11)
  )

p3<- ggarrange(p1, p2, 
               nrow = 2, 
               common.legend = TRUE,   # Zorgt voor gedeelde legenda
               legend = "bottom",       # Plaatst de legenda rechts
               align = "v")  


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/groeplinevoedermiddelenincl.jpg", plot = p3, width = 12, height = 5.5, dpi = 300)




library(ggrepel)

datameans1 <- datameans #data sluis
#graph voerkosten barplot:
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")

graph <- datameans %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten  incl. jongvee (euro/koe)` = `prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`,
         `Voerkosten  incl. jongvee (euro/koe)` = ifelse(is.na(`Voerkosten  incl. jongvee (euro/koe)`), 0, `Voerkosten  incl. jongvee (euro/koe)`)) %>%
  group_by(grondsoort, voedermiddel) %>%
  summarise(`Voerkosten  incl. jongvee (euro/koe)` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten  incl. jongvee (euro/koe)` > 70, paste0("€", round(`Voerkosten  incl. jongvee (euro/koe)`)), NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)))

totals <- graph %>%
  group_by(grondsoort) %>%
  summarise(total = sum(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", round(total)))

p1 <- ggplot(graph, aes(x = grondsoort, y = `Voerkosten  incl. jongvee (euro/koe)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = grondsoort, y = total, label = label), vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  ylab("Voerkosten  incl. jongvee (euro/koe/jaar)")+
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.placement = "outer",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/voerskosten_incl.jpg", plot = p1, width = 6, height = 3.7, dpi = 300)

graph <- datameans %>% 
  filter(jaar == "2024") %>%
  filter(voedermiddel != "Melkproducten") %>%
  mutate(`Voerkosten incl. jongvee (euro/100 kg melk)` = 
           (`prijsvoer (euro/kg ds)` * `opname incl. jongvee (kg DS/koe)`) / `fpcmProductie per koe (kg)` * 100) %>%
  group_by(grondsoort, voedermiddel) %>%
  summarise(`Voerkosten incl. jongvee (euro/100 kg melk)` = mean(`Voerkosten incl. jongvee (euro/100 kg melk)`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label = ifelse(`Voerkosten incl. jongvee (euro/100 kg melk)` > 0.8, 
                        paste0("€", format(round(`Voerkosten incl. jongvee (euro/100 kg melk)`, 1), nsmall = 2)), 
                        NA),
         voedermiddel = factor(voedermiddel, levels = c(voeders.orderPlot)))

totals <- graph %>%
  group_by(grondsoort) %>%
  summarise(total = sum(`Voerkosten incl. jongvee (euro/100 kg melk)`, na.rm = TRUE)) %>%
  mutate(label = paste0("€", format(round(total, 2), nsmall = 2)))

p1<-ggplot(graph, aes(x = grondsoort, y = `Voerkosten incl. jongvee (euro/100 kg melk)`, fill = voedermiddel)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold", na.rm = TRUE) +
  geom_text(data = totals, aes(x = grondsoort, y = total, label = label), 
            vjust = -0.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = voeders.color) +
  scale_color_manual(values = voeders.color) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/voerskosten_melkperkoe2_incl.jpg", plot = p1, width = 6, height = 3.7, dpi = 300)

#Further analyzes########################################################################################
#########################################################################################################
#########################################################################################################
test <- data %>%
  filter(jaar == 2024) %>%
  select(bedrijfID, `Melkopbrengst (euro/koe)`, `Voerkosten  incl. jongvee (euro/koe)`, `Saldo incl. jongvee (euro/koe)`, grondsoort) %>%
  distinct() %>%
  group_by(grondsoort) %>%
  summarise(
    `Voerkosten  incl. jongvee` = mean(`Voerkosten  incl. jongvee (euro/koe)`, na.rm = TRUE),
    `Melkopbrengst` = mean(`Melkopbrengst (euro/koe)`, na.rm = TRUE),
    `Saldo incl. jongvee` = mean(`Saldo incl. jongvee (euro/koe)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -grondsoort, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("Voerkosten  incl. jongvee", "Melkopbrengst", "Saldo incl. jongvee")))

my_colors <- c("#213b73", "#1c6c30", "#e29f02")

p1 <- ggplot(test, aes(x = grondsoort, y = Value, fill = grondsoort)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~Variable) +
  geom_text(aes(label = paste0("€", round(Value, 0))),  
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5, color = "black", fontface = "bold", na.rm = TRUE) +
  scale_fill_manual(values = my_colors) +
  labs(x = NULL, y = "Euro/koe/jaar") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 11, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/bar_saldomelkvoer_incl.jpg", plot = p1, width = 7, height = 4, dpi = 300)


data2 <- datameans %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)) %>%
  ungroup() %>%
  bind_rows(data.frame(
    jaar = c(2020, 2021, 2022, 2023, 2024),
    `rantsoenRE gehalte (g/kg DS)` = c(167, 161, 161, 163, 161),
    grondsoort = "NL-gemiddelde"
  )) %>%
  mutate( `rantsoenRE gehalte (g/kg DS)` = ifelse(grondsoort == "NL-gemiddelde", rantsoenRE.gehalte..g.kg.DS., `rantsoenRE gehalte (g/kg DS)`))%>%
  mutate(alpha_group = ifelse(grondsoort == "NL-gemiddelde", 0.6, 1))%>%
  mutate(linetype_group = ifelse(grondsoort == "NL-gemiddelde", "NL-gemiddelde", "Koe & Eiwit deelnemers")) %>%
  mutate(grondsoortlable = ifelse(grondsoort == "Klei", "Klei (n=57)", 
                                  ifelse(grondsoort == "Veen", "Veen (n=31)",
                                         ifelse(grondsoort == "Zand", "Zand (n=57)", "NL-gemiddelde"))))


labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar+0.1) 

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

p1<- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "lightgray", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenRE gehalte (g/kg DS)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = linetype_group),
            size = 1.2) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size=1) +
  annotate("rect", xmin = 2024.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white")+
  annotate("text", x = 2022, y = max(data2$`rantsoenRE gehalte (g/kg DS)`), 
           label = "Startjaar Koe & Eiwit", hjust = -0.1, vjust = 0, color = "gray",
           fontface = "bold", size=3.5) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = 0, vjust = 0, color = "Black",
           fontface = "bold", size=3.5) +
  geom_text(
    data = labels_df,
    aes(x = jaar, y = `rantsoenRE gehalte (g/kg DS)`, 
        label = grondsoortlable, color = grondsoort),
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "NL-gemiddelde" = "gray"),
    labels = c("Klei (n=11)", "Veen (n=22)", "Zand (n=34)", "NL-gemiddelde"),
    guide = "none")+
  scale_linetype_manual(
    values = c("NL-gemiddelde" = "dashed", "Koe & Eiwit deelnemers" = "solid"),
    name = NULL ) +
  labs(y = "Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  #xlim(min(data2$jaar), max(data2$jaar) + 0.6) +
  ylim(150, 172) +
  theme_minimal(base_size = 12.5) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.width = unit(1.5, "cm")
  )+
  xlim(min(data2$jaar), max(data2$jaar) + 0.7) 

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Artikelgrondsoorten/linegraph_REeiwitgrondsoortvsnederland.jpg", plot = p1, width = 7, height = 4.5, dpi = 300)




Rrumdf$hia1b13a
#final graphs Koe en Eiwit:

dontdelete <- read_excel("C:/Rfiles/WR/Koe en eiwit 2025/datameans2.xlsx")
datameans1 <- dontdelete
dontdelete <- read_excel("C:/Rfiles/WR/Koe en eiwit 2025/datameans2.xlsx")
datameans1 <- dontdelete

tamara <- datameans1 %>%
  group_by(Typebedrijf, jaar) %>%
  summarise(
    number_bedrijven_in_group = n_distinct(bedrijfID),
    rantsoenRE_gehalte_mean = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)
  )

ggplot(datameans1 %>% 
         mutate(Typebedrijf = factor(Typebedrijf, levels=c("Stabiel laag",
                                                           "Doel gehaald",
                                                           "Doel niet gehaald",
                                                           "Stabiel hoog")))%>%
         distinct(bedrijfID, jaar, Typebedrijf, `rantsoenRE gehalte (g/kg DS)`), 
       aes(x = factor(jaar), y = `rantsoenRE gehalte (g/kg DS)`, fill = Typebedrijf,
           color = Typebedrijf)) +
  geom_jitter(width = 0.2, size=1.2) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color="Black") +  # boxplot zonder uitbijters (optioneel)
  labs(
    x = "Jaar",
    y = "RantsoenRE gehalte (g/kg DS)",
  ) +
  theme_minimal()+
  facet_wrap(.~Typebedrijf, nrow=1)+
  scale_fill_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  scale_color_manual(values = c(
    "Stabiel hoog" = "#ff4d4d",
    "Doel niet gehaald" = "#ffc14d",
    "Doel gehaald" = "#3385ff",
    "Stabiel laag" = "#00b33c"
  )) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size=9),
        axis.ticks.x = element_line(),
        panel.grid.major.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) 


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Tamara_132.jpg"
       , width = 7, height = 3.5, dpi = 300)


#----------------------------------RE K & E vs Nederland-----------------------------------------------------
data2 <- data.frame(
  Jaar = c(2020, 2021, 2022, 2023, 2024),
  Waarde1 = c(166, 162, 159, 158, 156),
  Waarde2 = c(167, 161, 161, 163, 161)
)%>%
  pivot_longer(cols = -Jaar, names_to = "Categorie", values_to = "Waarde") %>%
  mutate(Categorie = ifelse(Categorie == "Waarde1", "Koe & Eiwit", "NL-gemiddelde"))

# Plot de gegevens
ggplot(data2, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  geom_line(size = 1.2) +  # Lijn dikte
  #annotate("text", x = 2022, y = max(data$Waarde), label = "Startjaar", vjust = -3, hjust=-0.2, color = "gray") +
  labs(title = "Eiwitgehalte rantsoen Koe & Eiwit daalt t.o.v. NL-gemiddelde
       ",
       x = "Jaar",
       y = "Ruw eiwitgehalte rantsoen
  (g RE/kg ds)",
       color = "Legenda",
       text = element_text(family = "Dosis")) +
  theme_minimal()+
  scale_color_manual(values = c("#1A6B04", "#E29E00")) +  # Kleuren handmatig instellen
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_text(size =12),
        axis.text.x = element_text(size =12),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size = 14),
        plot.title = element_blank())+
  geom_text(
    data = data2 %>% filter(Jaar == max(Jaar)),
    aes(label = Categorie),
    hjust = -0.05,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(150, 172), clip = "off") +
  theme(
    plot.margin = margin(5.5, 75, 5.5, 5.5)
  )+
  ylim(150,172) +
  annotate("text", x = 2022, y = 172, 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0.7, color = "gray",
           fontface = "bold", size=4) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black",
           fontface = "bold", size=4) +
  scale_x_continuous(
    limits = c(2020, 2024.2),
    expand = c(0, 0)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/RE K & E vs Nederland.jpg"
       , width = 7, height = 4.5, dpi = 300)


#----------------------------------RE K & E vs grondsoort-----------------------------------------------------
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(data.frame(
    jaar = c(2020, 2021, 2022, 2023, 2024),
    `rantsoenRE gehalte (g/kg DS)` = c(167, 161, 161, 163, 161),
    grondsoort = "NL-gemiddelde",
    check.names = FALSE   # <---- voorkomt dat R de naam vervormt
  )) %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "NL-gemiddelde" ~ 0.6,
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "NL-gemiddelde" ~ "NL-gemiddelde",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Klei",
    grondsoort == "Veen" ~ "Veen",
    grondsoort == "Zand" ~ "Zand",
    grondsoort == "NL-gemiddelde" ~ "NL-gemiddelde",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))


labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenRE gehalte (g/kg DS)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  geom_text(
    data = labels_df, 
    aes(
      x = jaar - 0.2,
      y = `rantsoenRE gehalte (g/kg DS)`,
      label = grondsoortlable,
      color = grondsoort
    ), 
    hjust = 0,          # start label nét rechts van de lijn
    vjust = 0.5,        # verticaal gecentreerd op de lijn
    size = 4, 
    fontface = "bold", 
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("NL-gemiddelde" = "dashed", "Koe & Eiwit" = "dashed", "Klei"="solid", "Veen"= "solid", "Zand"="solid"),
    name = NULL, guide = "none" ) +
  labs(y = "Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  ylim(150, 172) +
  theme_minimal(base_size = 11) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.text.y = element_text(size =12),
    axis.text.x = element_text(size =12),
    panel.grid.minor.x = element_blank(),
    axis.title.y=element_text(size = 14),
    plot.title = element_blank()
  )  +
  coord_cartesian(ylim = c(150, 172), clip = "off") +
  theme(
    plot.margin = margin(5.5, 75, 5.5, 5.5)
  )+
  annotate("text", x = 2022, y = 172, 
           label = "Startjaar\nKoe & Eiwit", hjust = -0.1, vjust = 0.7, color = "gray",
           fontface = "bold", size=4) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Doel: 155 RE", hjust = -0.1, vjust = 0, color = "Black",
           fontface = "bold", size=4) +
  scale_x_continuous(
    limits = c(2020, 2024.2),
    expand = c(0, 0)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/RE K & E vs grondsoort.jpg"
       , width = 7, height = 4.5, dpi = 300)

#english##################################################################33333
#----------------------------------RE K & E vs Nederland-----------------------------------------------------
data2 <- data.frame(
  Jaar = c(2020, 2021, 2022, 2023, 2024),
  Waarde1 = c(166, 162, 159, 158, 156),
  Waarde2 = c(167, 161, 161, 163, 161)
)%>%
  pivot_longer(cols = -Jaar, names_to = "Categorie", values_to = "Waarde") %>%
  mutate(Categorie = ifelse(Categorie == "Waarde1", "Koe & Eiwit", "NL average"))

# Plot de gegevens
ggplot(data2, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  geom_line(size = 1.2) +  # Lijn dikte
  #annotate("text", x = 2022, y = max(data$Waarde), label = "Startjaar", vjust = -3, hjust=-0.2, color = "gray") +
  labs(title = "Eiwitgehalte rantsoen Koe & Eiwit daalt t.o.v. NL-gemiddelde
       ",
       x = "Jaar",
       y = "Crude protein content of ration\n(g CP/kg DM)",
       color = "Legenda",
       text = element_text(family = "Dosis")) +
  theme_minimal()+
  scale_color_manual(values = c("#1A6B04", "#E29E00")) +  # Kleuren handmatig instellen
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_text(size =12),
        axis.text.x = element_text(size =12),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size = 14),
        plot.title = element_blank())+
  geom_text(
    data = data2 %>% filter(Jaar == max(Jaar)),
    aes(label = Categorie),
    hjust = -0.05,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(150, 172), clip = "off") +
  theme(
    plot.margin = margin(5.5, 70, 5.5, 5.5)
  )+
  ylim(150,172) +
  annotate("text", x = 2022, y = 172, 
           label = "Start year\nKoe & Eiwit", hjust = -0.1, vjust = 0.7, color = "gray",
           fontface = "bold", size = 4) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Target: 155 RE", hjust = -0.1, vjust = 0, color = "black",
           fontface = "bold", size = 4) +
  scale_x_continuous(
    limits = c(2020, 2024.2),
    expand = c(0, 0)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/RE K & E vs Nederland.jpg"
       , width = 7, height = 4.5, dpi = 300)


#----------------------------------RE K & E vs grondsoort-----------------------------------------------------
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(data.frame(
    jaar = c(2020, 2021, 2022, 2023, 2024),
    `rantsoenRE gehalte (g/kg DS)` = c(167, 161, 161, 163, 161),
    grondsoort = "NL-gemiddelde",
    check.names = FALSE   # <---- voorkomt dat R de naam vervormt
  )) %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) %>%
  mutate(alpha_group = case_when(
    grondsoort == "NL-gemiddelde" ~ 0.6,
    grondsoort == "Koe & Eiwit" ~ 0.8,
    TRUE ~ 1
  )) %>%
  mutate(linetype_group = case_when(
    grondsoort == "NL-gemiddelde" ~ "NL-gemiddelde",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit deelnemers",
    TRUE ~ "Koe & Eiwit deelnemers"
  )) %>%
  mutate(grondsoortlable = case_when(
    grondsoort == "Klei" ~ "Clay",
    grondsoort == "Veen" ~ "Peat",
    grondsoort == "Zand" ~ "Sand",
    grondsoort == "NL-gemiddelde" ~ "NL average",
    grondsoort == "Koe & Eiwit" ~ "Koe & Eiwit"
  ))


labels_df <- data2 %>%
  group_by(grondsoort) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  mutate(jaar = jaar + 0.3) 

# Plot de gegevens
cols <- c("NL-gemiddelde" = "gray60", "Veen" = "#1c6c30", "Zand" = "#e29f02", "Klei" = "#213b73")
types <- c("NL-gemiddelde" = "dashed", "Veen" = "solid", "Zand" = "solid", "Klei" = "solid")

ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size=1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size=1) +
  geom_line(data = data2,
            aes(x = jaar,
                y = `rantsoenRE gehalte (g/kg DS)`,
                color = grondsoort,       # kleuren blijven zichtbaar
                linetype = grondsoortlable),
            size = 1.2) +
  geom_text(
    data = labels_df, 
    aes(
      x = jaar - 0.2,
      y = `rantsoenRE gehalte (g/kg DS)`,
      label = grondsoortlable,
      color = grondsoort
    ), 
    hjust = 0,          # start label nét rechts van de lijn
    vjust = 0.5,        # verticaal gecentreerd op de lijn
    size = 4, 
    fontface = "bold", 
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02",
      "Koe & Eiwit" = "#cf641c",
      "NL-gemiddelde" = "gray60"),
    guide = "none")+
  scale_linetype_manual(
    values = c("NL average" = "dashed", "Koe & Eiwit" = "dashed", "Clay"="solid", "Peat"= "solid", "Sand"="solid"),
    name = NULL, guide = "none" ) +
  labs(y = "Crude protein content of ration\n(g CP/kg DM)") +
  ylim(150, 172) +
  theme_minimal(base_size = 11) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.text.y = element_text(size =12),
    axis.text.x = element_text(size =12),
    panel.grid.minor.x = element_blank(),
    axis.title.y=element_text(size = 14),
    plot.title = element_blank()
  )  +
  coord_cartesian(ylim = c(150, 172), clip = "off") +
  theme(
    plot.margin = margin(5.5, 70, 5.5, 5.5)
  )+
  annotate("text", x = 2022, y = 172, 
           label = "Start year\nKoe & Eiwit", hjust = -0.1, vjust = 0.7, color = "gray",
           fontface = "bold", size = 4) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Target: 155 RE", hjust = -0.1, vjust = 0, color = "black",
           fontface = "bold", size = 4) +
  scale_x_continuous(
    limits = c(2020, 2024.2),
    expand = c(0, 0)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/RE K & E vs grondsoort.jpg"
       , width = 7, height = 4.5, dpi = 300)

