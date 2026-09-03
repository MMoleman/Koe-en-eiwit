library(ggpubr)
library(dplyr)
library(tidyr)
library(car)
library(writexl)
library(factoextra)
library(readxl)
library(ggcorrplot)
library(grid)
library("ggrepel")
library(ggalluvial)
library(ggpmisc)
library(modelsummary)

dontdelete <- read_excel("C:/Rfiles/WR/Koe en eiwit 2025/datameans3.xlsx")
datameans1 <- dontdelete

check <- datameans1 %>%
  select(provincie)
  dplyr::distinct(bedrijfID, jaar, provincie, `rantsoenRE gehalte (g/kg DS)`) %>%
  group_by(jaar, provincie) %>%
  summarise(RE_rantsoen = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm=TRUE)) %>%
  ungroup()

writexl::write_xlsx(
  check,
  "C:/Rfiles/WR/Koe en eiwit 2025/REperProvincie.xlsx"
)

voeders.abbr = c("gr","gk","sm","rv","bp","kv","mp","ov")
voeders.dict = c("gr" = "Vers gras", "gk" = "Graskuil", "sm" = "Snijmais", "rv" = "Overig ruwvoer", "bp" = "Bijproducten", "kv" = "Krachtvoer", "mp" = "Melkproducten", "ov" = "Overig")
voeders.order = c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
voeders.orderPlot <- c("Bijproducten","Krachtvoer","Overig ruwvoer","Snijmais","Graskuil", "Vers gras")
kenmerk.abbr = c("gve","melkperha","rants","efficientie","fpcmpkoe","melkpkoe","vet","eiwit","ureum",'opbGrasprDs',"aanleg")
kenmerk.dict = c("gve" = "nrMK","melkperha" = "intensiteit","rants" = "rantsoen","efficientie" = "Nefficientie","fpcmpkoe" = "fpcmProductie","melkpkoe" = "melkProductie","vet" = "vet","eiwit" = "eiwit","ureum" = "ureum", 'opbGrasprDs' = "Grasopbrengst", "aanleg" = "aanleg")
property.abbr = c("aandeel","verbruik","re","kvem","vem","ds")
property.dict = c("aandeel" = "aandeel","verbruik" = "opname","re" = "RE", "kvem" = "REkVEM","vem" = "VEM", "ds" = "DS")
fracSM.colors = c("< 5%" = "#548235", "5-25%" = "#843C0C", "> 25%" =	"#FFC000")
REgroup.colors <- c("RE > 165" = "#ff4d4d", "RE 161-165" = "#ffc14d", "RE 156-160" ="#3385ff", "RE < 156" = "#00b33c", "NA" = "darkgray")
REgroup.breaks <- c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")
prodCat.colors <- c("6000 - 8000 kg" = "#ff4d4d", "8000 - 10000 kg" = "#ffc14d", "10000 - 12000 kg" ="#3385ff", "12000 - 14000 kg" = "#00b33c")
voeders.color = colors <- c("Vers gras" = "#1c6c30", 
                            "Graskuil" =	"#3fa535", 
                            "Snijmais" =	"#cf641c", 
                            "Overig ruwvoer" ="#F0CF80", 
                            "Bijproducten"	= "#e29f02", 
                            "Krachtvoer" =	"#213b73")


dataaaa <- datameans1 %>%
  filter(jaar == 2025) %>%
  select(
    bedrijfID, jaar, gve_melkvee,
    `oppervlakte gras (ha)`,
    `oppervlakte totaal (ha)`,
    oppoverig,
    intensiteitcat
  ) %>%
  distinct() %>%
  mutate(
    `GVE per ha gras` = gve_melkvee / `oppervlakte gras (ha)`,
    `GVE per ha voedergewassen` = gve_melkvee / (`oppervlakte totaal (ha)` - oppoverig),
    intensiteitcat = factor(intensiteitcat, levels=c("< 14000 kg/ha",
                                                     "14000 - 20000 kg/ha",
                                                     "> 20000 kg/ha"))
  ) 

ggplot(
  dataaaa,
  aes(
    x = `GVE per ha gras`,
    y = `GVE per ha voedergewassen`,
    color = intensiteitcat
  )
) +
  geom_point(size = 2.2, alpha = 0.85) +
  
  geom_hline(yintercept = 1.5, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 2.85, linetype = "dashed", colour = "grey40") +
  
  annotate("text", x = Inf, y = 1.5,
           label = "1.5 GVE/ha",
           hjust = 1.1, vjust = -0.3, size = 4) +
  
  annotate("text", x = 2.85, y = 7.5,
           label = "2.85 GVE/ha",
           angle = 90, vjust = -0.3, size = 4) +
  
  labs(
    x = "GVE per ha gras",
    y = "GVE per ha voedergewassen"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )+
  scale_color_manual(values = c("#00b33c", "#ffc14d","#ff4d4d"))

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/GVE_ha.jpg"
       , width = 8, height = 4, dpi = 300)



library(nlme)
library(emmeans)
library(multcomp)
library(rstatix)


vars <- c(
  "nkoeien",
  "jongvee_per_10_melkkoe",
  "fpcmProductie per koe (kg)",
  "oppervlakte gras (ha)",
  "oppervlakte mais (ha)",
  "oppervlakte totaal (ha)",
  "fpcmProductie per ha",
  "Totaal_beweiding(uur)"
)

results_list <- list()

for (v in vars) {
  
  cat("Running variable:", v, "\n")
  
  temp <- check
  temp$y <- temp[[v]]
  
  # clean data
  temp <- temp %>%
    filter(!is.na(y), is.finite(y))
  
  if (nrow(temp) < 10) next
  
  try({
    
    model <- lme(
      fixed = y ~ jaar,
      random = ~1 | bedrijfID,
      data = temp
    )
    
    # ✅ SAVE PLOTS
    
    file_name <- paste0("residuals_", gsub("[^A-Za-z0-9]", "_", v), ".png")
    
    png(file_name, width = 800, height = 800)
    
    par(mfrow = c(2, 2))
    
    print(plot(model))   # ✅ IMPORTANT: use print()
    
    dev.off()
    
    
    # p-value
    pval <- anova(model)["jaar", "p-value"]
    
    # emmeans
    emm <- emmeans(model, ~ jaar)
    cld_df <- as.data.frame(cld(emm, Letters = letters))
    
    # clean group column
    cld_df$.group <- gsub(" ", "", cld_df$.group)
    
    # add info
    cld_df$variable <- v
    cld_df$p_jaar <- ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
    
    
    cld_df <- cld_df[, c("variable", "p_jaar", "jaar",
                         "emmean", "SE", "df",
                         "lower.CL", "upper.CL", ".group")]
    
    results_list[[v]] <- cld_df
    
  }, silent = TRUE)
}

final_table <- bind_rows(results_list) %>%
  filter(p_jaar < 0.05)
final_table
getwd()



vars <- c(
  "nkoeien",
  "jongvee_per_10_melkkoe",
  "fpcmProductie per koe (kg)",
  "oppervlakte gras (ha)",
  "oppervlakte mais (ha)",
  "oppervlakte totaal (ha)",
  "fpcmProductie per ha",
  "Totaal_beweiding(uur)"
)

results_list <- list()
trend_list <- list()   # ✅ extra table for slopes

for (v in vars) {
  
  cat("Running variable:", v, "\n")
  
  temp <- check
  temp$y <- temp[[v]]
  
  # ✅ ensure numeric year
  temp$jaar <- as.numeric(temp$jaar)
  
  # clean data
  temp <- temp %>%
    filter(!is.na(y), is.finite(y))
  
  if (nrow(temp) < 10) next
  
  try({
    
    model <- lme(
      fixed = y ~ jaar,
      random = ~1 | bedrijfID,
      data = temp
    )
    
    # ============================
    # ✅ Residual plots
    # ============================
    
    file_name <- paste0("residuals_", gsub("[^A-Za-z0-9]", "_", v), ".png")
    
    png(file_name, width = 800, height = 800)
    par(mfrow = c(2, 2))
    print(plot(model))
    dev.off()
    
    
    # ============================
    # ✅ Linear trend extraction
    # ============================
    
    coef_table <- summary(model)$tTable
    
    slope <- coef_table["jaar", "Value"]
    slope_se <- coef_table["jaar", "Std.Error"]
    slope_t <- coef_table["jaar", "t-value"]
    slope_p <- coef_table["jaar", "p-value"]
    
    ci <- intervals(model)$fixed
    slope_lower <- ci["jaar", "lower"]
    slope_upper <- ci["jaar", "upper"]
    
    
    # ============================
    # ✅ P-value from ANOVA
    # ============================
    
    pval <- anova(model)["jaar", "p-value"]
    
    
    # ============================
    # ✅ Estimated marginal means
    # ============================
    
    emm <- emmeans(model, ~ jaar)
    cld_df <- as.data.frame(cld(emm, Letters = letters))
    
    # clean grouping column
    cld_df$.group <- gsub(" ", "", cld_df$.group)
    
    
    # ============================
    # ✅ Add info to table
    # ============================
    
    cld_df$variable <- v
    cld_df$p_jaar <- ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
    
    # ✅ Add slope info (same per variable)
    cld_df$slope <- slope
    cld_df$slope_SE <- slope_se
    cld_df$slope_p <- ifelse(slope_p < 0.001, "<0.001", sprintf("%.3f", slope_p))
    cld_df$slope_lower <- slope_lower
    cld_df$slope_upper <- slope_upper
    
    
    # reorder columns
    cld_df <- cld_df[, c(
      "variable", "p_jaar", "jaar",
      "emmean", "SE", "df",
      "lower.CL", "upper.CL", ".group",
      "slope", "slope_SE", "slope_p",
      "slope_lower", "slope_upper"
    )]
    
    results_list[[v]] <- cld_df
    
    
    # ============================
    # ✅ Separate trend table
    # ============================
    
    trend_df <- data.frame(
      variable = v,
      slope = slope,
      slope_SE = slope_se,
      slope_t = slope_t,
      slope_p = slope_p,
      lower_CI = slope_lower,
      upper_CI = slope_upper
    )
    
    trend_list[[v]] <- trend_df
    
  }, silent = TRUE)
}

# ============================
# ✅ Final tables
# ============================

final_table <- bind_rows(results_list) %>%
  filter(p_jaar < 0.05)

trend_table <- bind_rows(trend_list)

# view results
final_table
trend_table

# working directory (where plots are saved)
getwd()


#Variables you want to analyse
vars <- c(
  "nkoeien",
  "jongvee_per_10_melkkoe",
  "fpcmProductie per koe (kg)",
  "oppervlakte gras (ha)",
  "oppervlakte mais (ha)",
  "oppervlakte totaal (ha)",
  "fpcmProductie per ha",
  "Totaal_beweiding(uur)"
)

# Empty list to store results
results_list <- list()

for (v in vars) {
  
  # tijdelijke kolom (handig ivm spaties)
  check$y <- check[[v]]
  
  # model
  model <- lme(
    fixed = y ~ jaar,
    random = ~1 | bedrijfID,
    data = check
  )
  
  # p-value
  pval <- anova(model)["jaar", "p-value"]
  
  # emmeans + groepen
  emm <- emmeans(model, ~ jaar)
  cld_df <- as.data.frame(cld(emm, Letters = letters))
  
  # toevoegen info
  cld_df$variable <- v
  cld_df$p_jaar <- pval
  
  # selecteer kolommen
  cld_df <- cld_df[, c("variable", "p_jaar", "jaar", 
                       "emmean", "SE", "df", 
                       "lower.CL", "upper.CL", ".group")]
  
  # opslaan
  results_list[[v]] <- cld_df
}

# alles samenvoegen
final_table <- bind_rows(results_list)

final_table



check <- datameans1 %>%
  distinct(bedrijfID, 
           jaar, 
           nkoeien, 
           jongvee_per_10_melkkoe, 
           `fpcmProductie per koe (kg)`, 
           `oppervlakte gras (ha)`, 
           `oppervlakte mais (ha)`,
           `oppervlakte totaal (ha)`,
           `fpcmProductie per ha`,
           `Totaal_beweiding(uur)`)

check$jaar <- as.factor(check$jaar)

# model
model <- aov(`fpcmProductie per koe (kg)` ~ jaar + Error(bedrijfID/jaar), data = check)
summary(model)

check$fpcm <- check$`fpcmProductie per koe (kg)`

model <- lme(
  fixed = fpcm ~ jaar,
  random = ~1 | bedrijfID,
  data = check
)
anova(model)

emmeans(model, pairwise ~ jaar, adjust = "tukey")
emm <- emmeans(model, ~ jaar)
cld_res <- cld(emm, Letters = letters)
cld_df <- as.data.frame(cld_res)

plot(model)


#alluvium (Karstens grafiek)###################################################################################################################
plot_data <- datameans1 %>% 
  group_by(bedrijfID,jaar,REgroup) %>% 
  slice(1) %>% 
  group_by(jaar) %>% 
  mutate(nrFarmsTotal = length(unique(bedrijfID))) %>% 
  group_by(jaar, REgroup) %>% 
  mutate(nrFarms = paste0(length(unique(bedrijfID))," bedrijven"),
         nrFarmsPerc = paste0(round(length(unique(bedrijfID)) / nrFarmsTotal * 100),"%"),
         farmFreq = 1 / nrFarmsTotal,
         nrFarmsTotal = case_when(bedrijfID == min(bedrijfID, na.rm = T) ~ nrFarmsTotal)) %>%
  mutate(REgroup = factor(REgroup, levels= c("RE > 165","RE 161-165","RE 156-160", "RE < 156")))

ggplot(plot_data, aes(x = as.factor(jaar), y = farmFreq, stratum = REgroup, fill = REgroup)) +
  geom_flow(aes(alluvium = bedrijfID)) +
  geom_stratum(color="black") + 
  geom_text(stat = "stratum", aes(label = nrFarmsPerc), color = "white",fontface = "bold", size=3)+
  geom_text(aes(label = ifelse(!is.na(nrFarmsTotal), paste("n=", nrFarmsTotal), "")),  # Label only if nrFarmsTotal is not NA
            y = 1.07, color = "black", size =3.5)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size =11),
        axis.text.x = element_text(size =11),
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(size = 12),
        plot.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size =10)
  ) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.1), labels = scales::percent) +
  labs(y = "Aandeel bedrijven (%)", x = "Jaar", fill = "RE groep") +
  scale_fill_manual(values = REgroup.colors, breaks = REgroup.breaks)+
  ggtitle(" ")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/alluvium.jpg"
       , width = 8, height = 4, dpi = 300)

#tabel 3: RE-gehalte (gram/kg ds) totale rantsoen Koe en Eiwit-deelnemers ten opzichte van Nederland ###################################################################################################################
tabel3 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenRE gehalte (g/kg DS)`) %>%
  group_by(jaar) %>%
  summarise(`rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE), .groups = "drop")

#tabel3 <- datameans1 %>%
#  distinct(jaar, bedrijfID,grondsoort, intensiteitcat, `rantsoenRE gehalte (g/kg DS)`) %>%
#  filter(jaar == 2025) %>%
#  group_by(grondsoort, intensiteitcat) %>%
#  summarise(
#    n_bedrijven = n(),
#    `rantsoenRE gehalte (g/kg DS)` = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE),
#    .groups = "drop"
#  )

data2 <- data.frame(
  Jaar = c(2020, 2021, 2022, 2023, 2024, 2025),
  Waarde1 = c(166.0694, 161.5616, 158.8681, 158.3217, 156.5857, 157.9790),
  Waarde2 = c(167, 161, 161, 163, 161, 158))%>%
  pivot_longer(cols = -Jaar, names_to = "Categorie", values_to = "Waarde") %>%
  mutate(Categorie = ifelse(Categorie == "Waarde1", "Koe & Eiwit", "NL-gemiddelde"))

#figuur lijngrafiek
ggplot(data2, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  geom_line(size = 1.2) +  # Lijn dikte
  #annotate("text", x = 2022, y = max(data$Waarde), label = "Startjaar", vjust = -3, hjust=-0.2, color = "gray") +
  labs(title = "Eiwitgehalte rantsoen Koe & Eiwit daalt t.o.v. NL-gemiddelde
       ",
       x = "Jaar",
       y =  "Ruw eiwitgehalte rantsoen\n(g RE/kg ds)",
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
    data = data2 %>% 
      group_by(Categorie) %>%
      filter(!is.na(Waarde)) %>%
      slice_max(Jaar),  # pick the latest year with a value
    aes(label = Categorie),
    hjust = -0.05,
    nudge_y = c(-0.5, 0.5),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  )+
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
    limits = c(2020, 2025.2),
    expand = c(0, 0)
  )

# Sla de grafiek op als een jpg-bestand
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/RE K & E vs Nederland.jpg"
       , width = 7, height = 4, dpi = 300)

#Heatplot grondsoort###################################################################################################################
df_sorted <- datameans1 %>% 
  group_by(bedrijfID,jaar,REgroup) %>% 
  slice(1) %>% 
  group_by(jaar) %>% 
  mutate(nrFarmsTotal = length(unique(bedrijfID))) %>% 
  group_by(jaar, REgroup) %>% 
  mutate(nrFarms = paste0(length(unique(bedrijfID))," bedrijven"),
         nrFarmsPerc = paste0(round(length(unique(bedrijfID)) / nrFarmsTotal * 100),"%"),
         farmFreq = 1 / nrFarmsTotal,
         nrFarmsTotal = case_when(bedrijfID == min(bedrijfID, na.rm = T) ~ nrFarmsTotal)) %>%
  mutate(REgroup = factor(REgroup, levels= c("RE > 165","RE 161-165","RE 156-160", "RE < 156")))

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
    intensiteitcat = factor(intensiteitcat, 
                            levels = c("< 14000 kg/ha", "14000 - 20000 kg/ha", "> 20000 kg/ha"))
  )

ggplot(df_groep, aes(x = jaar, y = intensiteitcat, fill = REgroup)) +
  geom_tile(color="white")  +  # Creates the heatmap tiles
  geom_text(aes(label = sprintf("%.0f", `rantsoenRE gehalte (g/kg DS)`)),  # Format the average values with 0 digits after the decimal
            color = "white", fontface = "bold", size = 4) +  # Creates the heatmap tiles
  facet_wrap(. ~ grondsoort)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = sort(unique(df_sorted$jaar))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size=12, color="black"),
        strip.text = element_text(size=12, color="black"),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = REgroup.colors) +
  ylab("Kleur per boer")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Heatplot klasse.jpg", width = 10, height = 3.7, dpi = 1500)

#jitterplot chapter ugh something##################################################################################################################################
farm_level <- datameans1 %>%
  filter(voedermiddel == "Snijmais") %>%
  mutate(
    `Snijmais aandeel (%)` = `aandeel (%)`) %>%
  mutate(
    `Snijmais groep` = ntile(`Snijmais aandeel (%)`, 3),
    `Snijmais groep` = as.factor(`Snijmais groep`),
    doelgehaald = ifelse(`rantsoenRE gehalte (g/kg DS)` < 155, "Yes", "No")
  ) %>%
  dplyr::select(
    grondsoort,
    `Snijmais groep`,
    intensiteitcat,
    jaar,
    `rantsoenRE gehalte (g/kg DS)`,
    bedrijfID,
    doelgehaald
  )

farm_long <- farm_level %>%
  mutate(
    grondsoort = as.character(grondsoort),
    intensiteitcat = as.character(intensiteitcat),
    `Snijmais groep` = as.character(`Snijmais groep`),
  ) %>%
  pivot_longer(
    cols = c(grondsoort, intensiteitcat, `Snijmais groep`),
    names_to = "factor_type",
    values_to = "factor_value"
  ) %>%
  mutate(
    factor_type = case_when(
      factor_type == "intensiteitcat" ~ "Intensiteit",
      factor_type == "Snijmais groep" ~ "Aandeel Snijmais",
      TRUE ~ "Grondsoort"
    ),
    factor_type = factor(factor_type,
                         levels = c("Grondsoort", "Intensiteit", "Aandeel Snijmais"))
  )

summary_df <- farm_long %>%
  group_by(factor_type, factor_value, jaar, bedrijfID) %>%
  summarise(
    doelgehaald = any(doelgehaald == "Yes"),
    .groups = "drop"
  ) %>%
  group_by(factor_type, factor_value, jaar) %>%
  summarise(
    n = n(),
    pct_below_155 = mean(doelgehaald) * 100,
    .groups = "drop"
  ) %>%
  dplyr::select(-n) %>%
  pivot_wider(
    names_from = jaar,
    values_from = pct_below_155
  )%>%
  mutate(across(
    where(is.numeric),
    ~ paste0(round(.x, 1), "%")
  ))


plot_df <- summary_df %>%
  pivot_longer(
    cols = -c(factor_type, factor_value),
    names_to = "jaar",
    values_to = "pct"
  ) %>%
  mutate(
    pct = as.numeric(gsub("%", "", pct)),
    jaar = as.factor(jaar)
  )

p1 <- ggplot(farm_long %>% filter(factor_type == "Grondsoort"),
       aes(x = jaar, y = `rantsoenRE gehalte (g/kg DS)`, group=factor(jaar), fill =factor_value)) +
  scale_fill_manual(values = c("Klei" = "#213b73","Zand" = "#e29f02","Veen" ="#1c6c30")) +
  facet_wrap(~factor_value) +
  geom_boxplot(width = 0.65, alpha = 0.8) +
  coord_cartesian(ylim = c(122, 193)) +
  labs(x = NULL,
       y = "RE rantsoen (g RE/kg ds)") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=11, color="black"),
        axis.ticks.x = element_line(),
        panel.grid.minor.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Boxplotsgroepen_grondsoort.jpg",  width = 8, height = 3, dpi = 1500)


p1 <- ggplot(farm_long %>% filter(factor_type == "Intensiteit") %>%
               mutate(factor_value = factor(factor_value,
                                       levels = c("< 14000 kg/ha",
                                                  "14000 - 20000 kg/ha",
                                                  "> 20000 kg/ha"))),
             aes(x = jaar, y = `rantsoenRE gehalte (g/kg DS)`, group=factor(jaar), fill =factor_value)) +
  facet_wrap(~factor_value) +
  scale_fill_manual(values = c("< 14000 kg/ha" = "#213b73","14000 - 20000 kg/ha" = "#1c6c30","> 20000 kg/ha" ="#e29f02")) +
  facet_wrap(~factor_value) +
  geom_boxplot(width = 0.65, alpha = 0.8) +
  coord_cartesian(ylim = c(122, 193)) +
  labs(x = NULL,
       y = "RE rantsoen (g RE/kg ds)") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=11, color="black"),
        axis.ticks.x = element_line(),
        panel.grid.minor.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Boxplotsgroepen_intens.jpg",  width = 8, height = 3, dpi = 1500)


p3 <- ggplot(farm_long %>%                
               filter(factor_type == "Aandeel Snijmais") %>% 
               mutate(factor_value = factor(factor_value,
                                            levels = c("1","2","3"),
                                            labels = c("0-13%", "13-21%","21-52%"))),
             aes(x = jaar, y = `rantsoenRE gehalte (g/kg DS)`, group=factor(jaar), fill =factor_value)) +
  facet_wrap(~factor_value) +
  scale_fill_manual(values = c("0-13%" = "#213b73","13-21%" = "#1c6c30","21-52%" ="#e29f02")) +
  facet_wrap(~factor_value) +
  geom_boxplot(width = 0.65, alpha = 0.8) +
  coord_cartesian(ylim = c(122, 193)) +
  labs(x = NULL,
       y = "RE rantsoen (g RE/kg ds)") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=11, color="black"),
        axis.ticks.x = element_line(),
        panel.grid.minor.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) 

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Boxplotsgroepen_snijmais.jpg",  width = 7, height = 2.7, dpi = 1500)


table_base <- base_2025 %>%
  mutate(Doelgehaald = `rantsoenRE gehalte (g/kg DS)` <= 155)

per_grondsoort <- table_base %>%
  group_by(grondsoort,jaar) %>%
  summarise(
    perc_gehaald = mean(Doelgehaald) * 100,
    n = n(),
    .groups = "drop"
  )

per_intensiteit <- table_base %>%
  group_by(intensiteitcat) %>%
  summarise(
    perc_gehaald = mean(Doelgehaald) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(intensiteitcat = factor(
    intensiteitcat,
    levels = c('< 14000 kg/ha',
               '14000 - 20000 kg/ha',
               '> 20000 kg/ha')
  ))

per_mais <- table_base %>%
  group_by(`Snijmais groep`) %>%
  summarise(
    perc_gehaald = mean(Doelgehaald) * 100,
    n = n(),
    .groups = "drop"
  )


test_long <- datameans1 %>%
  filter(jaar == 2025) %>%
  group_by(jaar, bedrijfID) %>%
  mutate(`Snijmais aandeel (%)` = mean(`aandeel (%)`[voedermiddel == "Snijmais"], na.rm = TRUE)) %>%
  select(bedrijfID, `rantsoenRE gehalte (g/kg DS)`, grondsoort, `Snijmais aandeel (%)`, `fpcmProductie per ha`, intensiteitcat, jaar) %>%
  distinct() %>%
  ungroup() %>%
  mutate(`Snijmais groep` = ntile(`Snijmais aandeel (%)`, 3)) %>%
  group_by(`Snijmais groep`) %>%
  mutate(label = paste0(round(min(`Snijmais aandeel (%)`),0), "–", round(max(`Snijmais aandeel (%)`),0), "%")) %>%
  ungroup() %>%
  mutate(`Snijmais groep` = label) %>%
  select(-label) %>%
  mutate(grondsoort = as.character(grondsoort), intensiteitcat = as.character(intensiteitcat)) %>%
  pivot_longer(cols = c(grondsoort, `Snijmais groep`, intensiteitcat),
               names_to = "factor_type", values_to = "factor_value") %>%
  mutate(factor_type = case_when(factor_type == "intensiteitcat" ~ "Intensiteit",
                                 factor_type == "Snijmais groep" ~ "Aandeel Snijmais",
                                 TRUE ~ "Grondsoort"),
         factor_type = factor(factor_type, levels=c("Grondsoort","Intensiteit","Aandeel Snijmais")),
         factor_value = factor(factor_value, levels=c("Klei","Veen","Zand",
                                                      "< 14000 kg/ha","14000 - 20000 kg/ha","> 20000 kg/ha",
                                                      "0–13%","13–21%","21–50%"))) %>%
  filter(factor_type != "Aandeel Snijmais")

# n en % <155 per groep
summary_df <- test_long %>%
  group_by(factor_type, factor_value) %>%
  summarise(n = n(),
            pct_below_155 = round(mean(`rantsoenRE gehalte (g/kg DS)` < 155) * 100, 1),
            y_pos = max(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE) + 3,
            .groups = "drop")


ggplot(test_long, aes(x = factor_value, y = `rantsoenRE gehalte (g/kg DS)`, color = factor_type)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  facet_wrap(~factor_type, scales = "free")+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  geom_text(
    data = summary_df, 
    aes(x = factor_value, y = 189, label = paste0("n = ", n
                                                  #, "\n", pct_below_155, "% <155"
                                                  )),
    inherit.aes = FALSE,
    size = 3.2,      # iets groter dan 3.5
    color = "gray30" 
  ) +
  scale_color_manual(values = c("Grondsoort" = "#213b73","Intensiteit" = "#e29f02","Aandeel Snijmais" ="#1c6c30")) +
  ylab("Ruw eiwitgehalte rantsoen\n(g RE/kg ds)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),       # y-as titel groter
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1), # x-as labels groter
    axis.text.y = element_text(size = 11),        # y-as labels groter
    strip.text = element_text(size = 12),         # facet labels groter
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(122, 193)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Ruw eiwitgehalte rantsoen per groep – 2025.jpg", width = 9, height =4, dpi = 300)


####grafiek relatie mais grondsoort en RE##########################################################################################
ggplot(datameans1 %>% 
         filter(voedermiddel == "Snijmais"),
       aes(x = `aandeel (%)`, 
           y = `rantsoenRE gehalte (g/kg DS)`, 
           color = grondsoort, shape=grondsoort, size=grondsoort)) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Klei" = 17,
      "Veen" = 3,
      "Zand" = 16
    )
  ) +
  geom_point(
    data = datameans1 %>% 
      filter(voedermiddel == "Snijmais") %>% filter(grondsoort %in% c("Klei", "Zand")),
    aes(color = grondsoort, shape = grondsoort),
    size = 1.5,
    stroke = 2
  ) +
  geom_point(
    data = datameans1 %>% 
      filter(voedermiddel == "Snijmais") %>% filter(grondsoort == "Veen"),
    aes(color = grondsoort, shape = grondsoort),
    size = 1,    # hier kleiner
    stroke = 1.5
  ) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  #ylab("RE gehalte graskuil") +
  xlab("Aandeel snijmais (%)") +
  #xlab("Maisoppervlakte\n(% van totaal)") +
  ylab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  # theme tuning
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(),
    
    
    axis.title = element_text(),
    axis.text = element_text(color = "black")
  )+
  facet_wrap(.~jaar, nrow=1)


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/snijmaisaandeel vs rantsoen.jpg", width =11, height = 4.5, dpi = 300)

####grafiek relatie ammoniak RE##########################################################################################
test <- datameans1 %>%
  filter(`bruikbaar voor alle andere analyses?` != "nee") %>%
  select(bedrijfID, jaar, `rantsoenRE gehalte (g/kg DS)`, grondsoort,`Ammoniakemissie per ha (kg/ha)`, `Ammoniakemissie uit stal en mestopslag (kg/gve)`) %>%
  distinct(bedrijfID, jaar, `rantsoenRE gehalte (g/kg DS)`, grondsoort,`Ammoniakemissie per ha (kg/ha)`, `Ammoniakemissie uit stal en mestopslag (kg/gve)`) 

p1 <- ggplot(test, aes(x = `rantsoenRE gehalte (g/kg DS)`,
                 y = `Ammoniakemissie per ha (kg/ha)`)) +
  geom_point(color="#e29f02", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color="#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color= "#213b73"
  ) +
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)")+
  ylab("Ammoniakemissie\ntotaal (kg/ha)") +
  theme_minimal()

p2 <- ggplot(test, aes(x=`rantsoenRE gehalte (g/kg DS)`, y= `Ammoniakemissie uit stal en mestopslag (kg/gve)`, color=grondsoort))+
  geom_point(#color="#e29f02", 
    alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color="#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color= "#213b73"
  ) +
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  ylab("Ammoniakemissie\nstal en mestopslag (kg/gve)") +
  theme_minimal()


ggarrange(p1,p2)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/ammoniak relatie.jpg", width =8, height = 3.5, dpi = 300)

####grafiek relatie ammoniak RE##########################################################################################
test <- datameans1 %>%
  filter(`bruikbaar voor alle andere analyses?` != "nee") %>%
  select(
    bedrijfID, jaar,
    `rantsoenRE gehalte (g/kg DS)`,
    grondsoort,
    `Ammoniakemissie per ha (kg/ha)`,
    `Ammoniakemissie uit stal en mestopslag (kg/gve)`
  ) %>%
  distinct()

test_diff <- test %>%
  arrange(bedrijfID, jaar) %>%
  group_by(bedrijfID) %>%
  mutate(
    diff_RE = `rantsoenRE gehalte (g/kg DS)` -
      lag(`rantsoenRE gehalte (g/kg DS)`),
    
    diff_NH3_ha = `Ammoniakemissie per ha (kg/ha)` -
      lag(`Ammoniakemissie per ha (kg/ha)`),
    
    diff_NH3_stal = `Ammoniakemissie uit stal en mestopslag (kg/gve)` -
      lag(`Ammoniakemissie uit stal en mestopslag (kg/gve)`)
  ) %>%
  ungroup()
library(fixest)
model_stal_fe <- feols(
  `Ammoniakemissie uit stal en mestopslag (kg/gve)` ~ 
    `rantsoenRE gehalte (g/kg DS)` | bedrijfID + jaar,
  data = test
)
summary(model_stal_fe)
model <- lmer(
  diff_NH3_ha ~ diff_RE + (1 | bedrijfID),
  data = test_diff
)

newdat <- data.frame(
  diff_RE = seq(
    min(test_diff$diff_RE, na.rm = TRUE),
    max(test_diff$diff_RE, na.rm = TRUE),
    length.out = 100
  )
)

newdat$pred <- predict(model, newdata = newdat, re.form = NA)

ggplot(test_diff, aes(x = diff_RE, y = diff_NH3_ha)) +
  geom_point(color = "#e29f02", alpha = 0.3) +
  
  geom_line(data = newdat,
            aes(x = diff_RE, y = pred),
            color = "#213b73",
            linewidth = 1) +
  
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  ylab("Ammoniakemissie\ntotaal (kg/ha)") +
  theme_minimal()

model <- lmer(
  `Ammoniakemissie uit stal en mestopslag (kg/gve)` | bedrijfID + jaar,
  data = test_diff
)

newdat <- data.frame(
  diff_RE = seq(
    min(test_diff$diff_RE, na.rm = TRUE),
    max(test_diff$diff_RE, na.rm = TRUE),
    length.out = 100
  )
)

newdat$pred <- predict(model, newdata = newdat, re.form = NA)

ggplot(test_diff, aes(x = diff_RE, y = diff_NH3_stal)) +
  geom_point(color = "#e29f02", alpha = 0.3) +
  
  geom_line(data = newdat,
            aes(x = diff_RE, y = pred),
            color = "#213b73",
            linewidth = 1) +
  
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  ylab("Ammoniakemissie\ntotaal (kg/ha)") +
  theme_minimal()

ggplot(test_diff, aes(x = diff_RE,
                       y = diff_NH3_ha)) +
  geom_point(color="#e29f02", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color="#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color= "#213b73"
  ) +
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)")+
  ylab("Ammoniakemissie\ntotaal (kg/ha)") +
  theme_minimal()

ggplot(test_diff, aes(x = diff_RE,
                      y = diff_NH3_stal)) +
  geom_point(color="#e29f02", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color="#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color= "#213b73"
  ) +
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)")+
  ylab("Ammoniakemissie\ntotaal (kg/ha)") +
  theme_minimal()

p2 <- ggplot(test, aes(x=`rantsoenRE gehalte (g/kg DS)`, y= `Ammoniakemissie uit stal en mestopslag (kg/gve)`, color=grondsoort))+
  geom_point(#color="#e29f02", 
    alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color="#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color= "#213b73"
  ) +
  xlab("Ruw eiwitgehalte rantsoen\n(g RE/kg DS)") +
  ylab("Ammoniakemissie\nstal en mestopslag (kg/gve)") +
  theme_minimal()


ggarrange(p1,p2)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/ammoniak relatie.jpg", width =8, height = 3.5, dpi = 300)

######Hoofdstuk 4#################################################################################################
df_sorted <- datameans1 %>%
  group_by(naam)%>%
  mutate(naam = ifelse(naam == "Jan van Dieren (Aeres Landbouwbedrijf Dronten)", "Jan van Dieren", naam)) %>%
  arrange(jaar, desc(`rantsoenRE gehalte (g/kg DS)`)) %>%  # Arrange first by REgroup and then by jaar
  ungroup() %>%
  mutate(naam = factor(naam, levels = unique(naam)))%>%
  distinct(naam, jaar, .keep_all = TRUE) %>%
  mutate(Typebedrijf = factor(Typebedrijf, levels= c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"),
                              labels = c("Laag (< 155 RE)", "Bewogen (< 155 RE)", "Bewogen (> 155 RE)", "Hoog (> 160 RE)")),
         REgroup = factor(REgroup, levels= c("RE < 156", "RE 156-160", "RE 161-165", "RE > 165")))

facet_labels <- df_sorted %>%
  distinct(naam, Typebedrijf) %>%
  count(Typebedrijf) %>%
  mutate(label = paste0(Typebedrijf
                        #, "\n (n = ", n, ")"
                        )) %>%
  {setNames(.$label, .$Typebedrijf)}

ggplot(df_sorted, aes(x = jaar, y = naam, fill = REgroup)) +
  geom_tile(color = "white") + 
  theme_minimal() +
  scale_x_continuous(breaks = sort(unique(df_sorted$jaar))) +
  facet_wrap(
    . ~ Typebedrijf,
    scales = "free",
    nrow = 1,
    labeller = labeller(Typebedrijf = facet_labels)
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(
      size = 11
    ),
    axis.text.y = element_blank(),
    strip.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = REgroup.colors)+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/heatplot.jpg", width =8, height = 3.5, dpi = 300)

#count typebedrijf per grondsoort#################################################################################
Tabellll_perc <- datameans1 %>% 
  mutate(naam = factor(naam, levels = unique(naam))) %>% 
  distinct(naam, Typebedrijf, grondsoort, .keep_all = TRUE) %>%
  count(Typebedrijf, grondsoort) %>%
  group_by(grondsoort) %>%   # 👈 dit is de key change
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup() %>%
  select(Typebedrijf, grondsoort, percentage) %>%
  tidyr::pivot_wider(
    names_from = Typebedrijf,
    values_from = percentage,
    values_fill = 0
  )


#lijngrafieken####################################################################################################

#----------------------------------Boxplots type bedrijf-----------------------------------------------------

ggplot(datameans1 %>% 
         mutate(Typebedrijf = factor(Typebedrijf, levels= c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog"),
                              labels = c("Laag (RE < 155)", "Bewogen (< 155 RE)", "Bewogen (> 155 RE)", "Hoog (RE > 160)")))%>%
         distinct(bedrijfID, jaar, Typebedrijf, `rantsoenRE gehalte (g/kg DS)`), 
       aes(x = factor(jaar), y = `rantsoenRE gehalte (g/kg DS)`, fill = Typebedrijf,
           color = Typebedrijf)) +
  geom_jitter(width = 0.2, size=1.2) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color="Black") +  # boxplot zonder uitbijters (optioneel)
  labs(
    x = "Jaar",
    y = "RantsoenRE gehalte\n(g/kg DS)",
  ) +
  theme_minimal()+
  facet_wrap(.~Typebedrijf, nrow=1)+
  scale_fill_manual(values = c(
    "Hoog (RE > 160)" = "#ff4d4d",
    "Bewogen (> 155 RE)" = "#ffc14d",
    "Bewogen (< 155 RE)" = "#3385ff",
    "Laag (RE < 155)" = "#00b33c"
  )) +
  scale_color_manual(values = c(
    "Hoog (RE > 160)" = "#ff4d4d",
    "Bewogen (> 155 RE)" = "#ffc14d",
    "Bewogen (< 155 RE)" = "#3385ff",
    "Laag (RE < 155)" = "#00b33c"
  )) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=11, color="black"),
        axis.text.x = element_text(angle = 45, hjust = 1,size=9),
        axis.ticks.x = element_line(),
        panel.grid.major.x = element_blank())+
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) 


ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Boxplot.jpg"
       , width = 8, height = 3, dpi = 300)


#Lijngrafiek voedermiddelen########################################################################################
final_df_plot <- datameans1 %>% 
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>%
  mutate(
    voedermiddel = factor(voedermiddel, 
                          levels = c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")),
    Typebedrijf = factor(Typebedrijf, 
                         c("Stabiel laag", "Doel gehaald", 
                           "Doel niet gehaald", "Stabiel hoog")),
    LegendLabel = paste("Bedrijf:", naam),
    opname = (`opname incl. jongvee (kg DS)`/nkoeien/365),
    RE = `RE gehalte (g/kg DS)`,
    grondsoort = grondsoort, 
  ) %>%
  dplyr::select(naam, Typebedrijf, bedrijfID, grondsoort, jaar, voedermiddel,
         `rantsoenRE gehalte (g/kg DS)`, opname, intensiteitcat, RE)

# --- Add group averages ---
Graph_plot <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>%
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(
    `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
    opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm = TRUE),
    RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE)
  ) %>%
  mutate(voedermiddel = factor(voedermiddel, levels = levels(final_df_plot$voedermiddel)),
         LegendLabel = Typebedrijf,
         naam = "All",
         grondsoort = "All",
         intensiteitcat = "All",
         bedrijfID = 0)


# --- Combine individual + group data ---
final_df_plot <- bind_rows(Graph_plot) %>%
  mutate(opname = replace_na(opname, 0)) %>%
  mutate(`rantsoenRE gehalte (g/kg DS)` = replace_na(`rantsoenRE gehalte (g/kg DS)`, "-"))

# --- Colors with alpha ---
bg_colors <- c(
  "Hoog (RE >160)" = alpha("#ff4d4d"),
  "Bewogen (RE >155)" = alpha("#ffc14d"),
  "Bewogen (RE <155)" = alpha("#3385ff"),
  "Laag (RE <155)" = alpha("#00b33c")
)


legend_levels <- c(
  "Stabiel laag", 
  "Doel gehaald", 
  "Doel niet gehaald", 
  "Stabiel hoog"
)

final_df_plot <- final_df_plot %>%
  mutate(LegendLabel = factor(LegendLabel, levels = legend_levels)) 

# Step 2: Calculate midpoint using only subset
# Step 1: Midpoint per facet
midpoints <- final_df_plot %>%
  filter(Typebedrijf %in% c("Stabiel laag", "Doel gehaald", "Doel niet gehaald", "Stabiel hoog")) %>%
  group_by(voedermiddel) %>%
  summarise(midpoint = mean(RE, na.rm = TRUE))

# Step 2: Half-range per facet
half_ranges <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(
    dev_up = max(RE - midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)], na.rm = TRUE),
    dev_down = max(midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)] - RE, na.rm = TRUE),
    half_range = pmax(dev_up, dev_down)
  )

# Step 3: Maximum half-range across all facets
max_half_range <- max(half_ranges$half_range, na.rm = TRUE)

# Step 4: Compute symmetric axes
facet_limits_adj <- midpoints %>%
  mutate(
    half_range = max_half_range,
    new_ymin_rounded = floor((midpoint - half_range) / 5) * 5,
    new_ymax_rounded = ceiling((midpoint + half_range) / 5) * 5
  )

# Step 5: Optional fine adjustment to include actual min/max
bottom_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(min_RE = min(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymin_rounded), by = "voedermiddel") %>%
  mutate(dist_to_min = min_RE - new_ymin_rounded)

top_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(max_RE = max(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(dist_to_max = max_RE - new_ymax_rounded)

# Adjust all facets to include real min/max
facet_limits_adj <- facet_limits_adj %>%
  mutate(
    new_ymin_rounded = new_ymin_rounded + min(bottom_devs$dist_to_min, na.rm = TRUE),
    new_ymax_rounded = new_ymax_rounded + max(top_devs$dist_to_max, na.rm = TRUE)
  )

# Step 8: Join back to final_df_plot
final_df_plot <- final_df_plot %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(
    LegendLabel = case_when(
      LegendLabel == "Stabiel laag" ~ "Laag (RE <155)",
      LegendLabel == "Doel gehaald" ~ "Bewogen (RE <155)",
      LegendLabel == "Doel niet gehaald" ~ "Bewogen (RE >155)",
      LegendLabel == "Stabiel hoog" ~ "Hoog (RE >160)",
      TRUE ~ LegendLabel
    )
  )%>%
  mutate(LegendLabel = factor(LegendLabel, 
                              levels = c("Laag (RE <155)",
                                         "Bewogen (RE <155)",
                                         "Bewogen (RE >155)",
                                         "Hoog (RE >160)")))

# --- Plot Voeropname ---
p1 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Laag (RE <155)", "Bewogen (RE <155)", "Bewogen (RE >155)", "Hoog (RE >160)")),
            aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Laag (RE <155)", "Bewogen (RE <155)", "Bewogen (RE >155)", "Hoog (RE >160)")),
             aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel),
             size = 1.7) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_color_manual(values = bg_colors) +
  expand_limits(y = c(0, max(final_df_plot$opname, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11))+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Plot RE gehalte ---
p2 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Laag (RE <155)", "Bewogen (RE <155)", "Bewogen (RE >155)", "Hoog (RE >160)")),
            aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Laag (RE <155)", "Bewogen (RE <155)", "Bewogen (RE >155)", "Hoog (RE >160)")),
             aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel),
             size = 1.7) +
  #geom_point(data = groep_avg_line,aes(x = jaar, y = RE, group = voedermiddel, color = paste("Groep:", groep_val)), size = 1.5, alpha=0.7 )+
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(values = bg_colors) +
  ylab("RE gehalte (g/kgDS)") +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Combine and annotate plots ---
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Lijnengrafiektypebedrijf.jpg"
       , width = 9, height = 4.5, dpi = 300)


# ================================
# Plot per grondsoort
# ================================
final_df_plot <- datameans1 %>% 
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>%
  mutate(
    voedermiddel = factor(voedermiddel, 
                          levels = c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")),
    Typebedrijf = factor(Typebedrijf, 
                         c("Stabiel laag", "Doel gehaald", 
                           "Doel niet gehaald", "Stabiel hoog")),
    LegendLabel = paste("Bedrijf:", naam),
    opname = (`opname incl. jongvee (kg DS)`/nkoeien/365),
    RE = `RE gehalte (g/kg DS)`,
    grondsoort = grondsoort, 
  ) %>%
  dplyr::select(naam, Typebedrijf, bedrijfID, grondsoort, jaar, voedermiddel,
         `rantsoenRE gehalte (g/kg DS)`, opname, intensiteitcat, RE)

# --- Add group averages ---
Graph_plot <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>%
  group_by(jaar, grondsoort, voedermiddel) %>% 
  reframe(
    `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
    opname = mean((`opname incl. jongvee (kg DS)`/nkoeien/365), na.rm = TRUE),
    RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE)
  ) %>%
  mutate(voedermiddel = factor(voedermiddel, levels = levels(final_df_plot$voedermiddel)),
         LegendLabel = grondsoort)


# --- Combine individual + group data ---
final_df_plot <- bind_rows(Graph_plot) %>%
  mutate(opname = replace_na(opname, 0)) %>%
  mutate(`rantsoenRE gehalte (g/kg DS)` = replace_na(`rantsoenRE gehalte (g/kg DS)`, "-"))

# --- Colors with alpha ---
bg_colors <- c(
  "Klei" = alpha("#213b73"),
  "Veen" = alpha("#1c6c30"),
  "Zand" = alpha("#e29f02")
)

legend_levels <- c(
  "Klei", 
  "Veen", 
  "Zand"
)

final_df_plot <- final_df_plot %>%
  mutate(LegendLabel = factor(LegendLabel, levels = legend_levels)) 

# Step 2: Calculate midpoint using only subset
# Step 1: Midpoint per facet
midpoints <- final_df_plot %>%
  filter(grondsoort %in% c("Klei", "Veen", "Zand")) %>%
  group_by(voedermiddel) %>%
  summarise(midpoint = mean(RE, na.rm = TRUE), .groups = "drop")

# Step 2: Half-range per facet
half_ranges <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(
    dev_up = max(RE - midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)], na.rm = TRUE),
    dev_down = max(midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)] - RE, na.rm = TRUE),
    half_range = pmax(dev_up, dev_down)
  )

# Step 3: Maximum half-range across all facets
max_half_range <- max(half_ranges$half_range, na.rm = TRUE)

# Step 4: Compute symmetric axes
facet_limits_adj <- midpoints %>%
  mutate(
    half_range = max_half_range,
    new_ymin_rounded = floor((midpoint - half_range) / 5) * 5,
    new_ymax_rounded = ceiling((midpoint + half_range) / 5) * 5
  )

# Step 5: Optional fine adjustment to include actual min/max
bottom_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(min_RE = min(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymin_rounded), by = "voedermiddel") %>%
  mutate(dist_to_min = min_RE - new_ymin_rounded)

top_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(max_RE = max(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(dist_to_max = max_RE - new_ymax_rounded)

# Adjust all facets to include real min/max
facet_limits_adj <- facet_limits_adj %>%
  mutate(
    new_ymin_rounded = new_ymin_rounded + min(bottom_devs$dist_to_min, na.rm = TRUE),
    new_ymax_rounded = new_ymax_rounded + max(top_devs$dist_to_max, na.rm = TRUE)
  )

# Step 8: Join back to final_df_plot
final_df_plot <- final_df_plot %>%
  left_join(facet_limits_adj %>% dplyr::select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(LegendLabel = factor(LegendLabel, 
                              levels = c("Klei",
                                         "Veen",
                                         "Zand")))

# --- Plot Voeropname ---
p1 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Klei",
                                        "Veen",
                                        "Zand")),
            aes(x = as.numeric(jaar), y = opname, group = LegendLabel, color = LegendLabel,
                linetype = LegendLabel),
            size = 1.2)+
  scale_linetype_manual(values = c(
    "Klei" = "dashed",
    "Veen" = "solid",
    "Zand" = "solid"
  ))+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Klei",
                                         "Veen",
                                         "Zand")),
             aes(x = as.numeric(jaar), y = opname, group = LegendLabel, color = LegendLabel),
             size = 1.7) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_color_manual(values = bg_colors) +
  expand_limits(y = c(0, max(final_df_plot$opname, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11))+
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Plot RE gehalte ---
p2 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Klei",
                                        "Veen",
                                        "Zand")),
            aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel,
                linetype = LegendLabel),
            size = 1.7)+
  scale_linetype_manual(values = c(
    "Klei" = "dashed",
    "Veen" = "solid",
    "Zand" = "solid"
  ))+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Klei",
                                         "Veen",
                                         "Zand")),
             aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel),
             size = 1.7) +
  #geom_point(data = groep_avg_line,aes(x = jaar, y = RE, group = voedermiddel, color = paste("Groep:", groep_val)), size = 1.5, alpha=0.7 )+
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(values = bg_colors) +
  ylab("RE gehalte (g/kgDS)") +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Combine and annotate plots ---
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Lijnengrafiektypebedrijf_grondsoort.jpg"
       , width = 9, height = 4.5, dpi = 300)


# ================================
# Plot per Maisgroup
# ================================
snijmais_df <- datameans1 %>%
  group_by(jaar, bedrijfID) %>%
  summarise(
    snijmais_aandeel = mean(`aandeel (%)`[voedermiddel == "Snijmais"], na.rm = TRUE),
    .groups = "drop"
  )

snijmais_df <- snijmais_df %>%
  mutate(`Snijmais groep` = ntile(snijmais_aandeel, 3)) %>%
  group_by(`Snijmais groep`) %>%
  mutate(label = paste0(
    round(min(snijmais_aandeel), 0), "–",
    round(max(snijmais_aandeel), 0), "%"
  )) %>%
  ungroup() %>%
  mutate(`Snijmais groep` = label)

Graph_plot <- datameans1 %>%
  inner_join(snijmais_df, by = c("jaar", "bedrijfID")) %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais", "Krachtvoer", "Bijproducten")) %>%
  group_by(jaar, voedermiddel, `Snijmais groep`) %>%
  summarise(
    `rantsoenRE gehalte (g/kg DS)` = round(mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE)),
    opname = mean(`opname incl. jongvee (kg DS)`/nkoeien/365, na.rm = TRUE),
    RE = mean(`RE gehalte (g/kg DS)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    LegendLabel = `Snijmais groep`
  )
# --- Combine individual + group data ---
final_df_plot <- bind_rows(Graph_plot) %>%
  mutate(opname = replace_na(opname, 0)) %>%
  mutate(`rantsoenRE gehalte (g/kg DS)` = replace_na(`rantsoenRE gehalte (g/kg DS)`, "-"))

# --- Colors with alpha ---
bg_colors <- c(
  "0–13%" = alpha("#213b73"),
  "13–21%" = alpha("#1c6c30"),
  "21–52%" = alpha("#e29f02")
)

legend_levels <- c(
  "0–13%", 
  "13–21%", 
  "21–52%"
)

final_df_plot <- final_df_plot %>%
  mutate(LegendLabel = factor(LegendLabel, levels = legend_levels)) 

# Step 2: Calculate midpoint using only subset
# Step 1: Midpoint per facet
midpoints <- final_df_plot %>%
  filter(`Snijmais groep` %in% c("0–13%", "13–21%", "21–52%")) %>%
  group_by(voedermiddel) %>%
  summarise(midpoint = mean(RE, na.rm = TRUE), .groups = "drop")

# Step 2: Half-range per facet
half_ranges <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(
    dev_up = max(RE - midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)], na.rm = TRUE),
    dev_down = max(midpoints$midpoint[match(voedermiddel, midpoints$voedermiddel)] - RE, na.rm = TRUE),
    half_range = pmax(dev_up, dev_down)
  )

# Step 3: Maximum half-range across all facets
max_half_range <- max(half_ranges$half_range, na.rm = TRUE)

# Step 4: Compute symmetric axes
facet_limits_adj <- midpoints %>%
  mutate(
    half_range = max_half_range,
    new_ymin_rounded = floor((midpoint - half_range) / 5) * 5,
    new_ymax_rounded = ceiling((midpoint + half_range) / 5) * 5
  )

# Step 5: Optional fine adjustment to include actual min/max
bottom_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(min_RE = min(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% select(voedermiddel, new_ymin_rounded), by = "voedermiddel") %>%
  mutate(dist_to_min = min_RE - new_ymin_rounded)

top_devs <- final_df_plot %>%
  group_by(voedermiddel) %>%
  summarise(max_RE = max(RE, na.rm = TRUE)) %>%
  left_join(facet_limits_adj %>% select(voedermiddel, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(dist_to_max = max_RE - new_ymax_rounded)

# Adjust all facets to include real min/max
facet_limits_adj <- facet_limits_adj %>%
  mutate(
    new_ymin_rounded = new_ymin_rounded + min(bottom_devs$dist_to_min, na.rm = TRUE),
    new_ymax_rounded = new_ymax_rounded + max(top_devs$dist_to_max, na.rm = TRUE)
  )

# Step 8: Join back to final_df_plot
final_df_plot <- final_df_plot %>%
  left_join(facet_limits_adj %>% select(voedermiddel, new_ymin_rounded, new_ymax_rounded), by = "voedermiddel") %>%
  mutate(LegendLabel = factor(LegendLabel, 
                              levels = c("0–13%",
                                         "13–21%",
                                         "21–52%")))

# --- Plot Voeropname ---
p1 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("0–13%",
                                        "13–21%",
                                        "21–52%")),
            aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("0–13%",
                                         "13–21%",
                                         "21–52%")),
             aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel),
             size = 1.5) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  ylab("Voeropname (kgDS/dier/dag)") +
  scale_color_manual(values = bg_colors) +
  expand_limits(y = c(0, max(final_df_plot$opname, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11))+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# --- Plot RE gehalte ---
p2 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("0–13%",
                                        "13–21%",
                                        "21–52%")),
            aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("0–13%",
                                         "13–21%",
                                         "21–52%")),
             aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel),
             size = 1.5) +
  #geom_point(data = groep_avg_line,aes(x = jaar, y = RE, group = voedermiddel, color = paste("Groep:", groep_val)), size = 1.5, alpha=0.7 )+
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(values = bg_colors) +
  ylab("RE gehalte (g/kgDS)") +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")

# --- Combine and annotate plots ---
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Lijnengrafiektypebedrijf.jpg"
       , width = 10, height = 5.5, dpi = 300)

#Ureum############################################################################################################
#----------------------------------Ureum-----------------------------------------------------------------
data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, ureum, grondsoort) %>%
  group_by(grondsoort, jaar) %>%
  summarise(ureum = mean(ureum, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, ureum) %>%
      group_by(jaar) %>%
      summarise(ureum = mean(ureum, na.rm = TRUE), .groups = "drop") %>%
      mutate(grondsoort = "Koe & Eiwit")
  ) 

ggplot(datameans1 %>% filter(jaar == 2025), 
       aes(x = `rantsoenRE gehalte (g/kg DS)`,
           y = ureum,
           color = grondsoort,
           shape = grondsoort)) +
  
  # Klei & Zand points (bigger)
  geom_point(
    data = datameans1 %>% filter(jaar == 2025, grondsoort %in% c("Klei", "Zand")),
    size = 1.5,
    stroke = 1.5
  ) +
  
  # Veen points (smaller)
  geom_point(
    data = datameans1 %>% filter(jaar == 2025, grondsoort == "Veen"),
    size = 1,
    stroke = 1
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.text =  element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    legend.title = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "Klei" = "#213b73",
      "Veen" = "#1c6c30",
      "Zand" = "#e29f02"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Klei" = 17,   # triangle
      "Veen" = 3,    # cross
      "Zand" = 16    # circle
    )
  ) +
  ylab("Ureum (mg/dL)")+
  xlab("Ruw eiwitgehalte (g RE/kg ds)")

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/Ureum vs RE.jpg"
       , width = 6, height = 3.5, dpi = 300)
####data paul##########################################################################################
data_paul <- datameans1 %>%
  filter(voedermiddel == "Graskuil") %>%
  group_by(jaar) %>%
  summarise(grassillage = mean(`RE gehalte (g/kg DS)`, na.rm=TRUE))

####VEM##########################################################################################################
datameans1 <- datameans1 %>%
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag",
                                         "Doel gehaald",
                                         "Doel niet gehaald",
                                         "Stabiel hoog")
  ))


type_labels <- c(
  "Stabiel laag" = "Laag (RE <155)",
  "Doel gehaald" = "Bewogen (RE <155)",
  "Doel niet gehaald" = "Bewogen (RE >155)",
  "Stabiel hoog" = "Hoog (RE >160)",
  "Alle bedrijven" = "Alle bedrijven"
)

cols <- c(
  "Stabiel laag" = alpha("#00b33c", 0.9),
  "Doel gehaald" = alpha("#ffc14d", 0.9),
  "Doel niet gehaald" = alpha("#3385ff", 0.9),
  "Stabiel hoog" = alpha("#ff4d4d", 0.9),
  "Alle bedrijven" = "gray60"
)

types <- c(
  "Stabiel laag" = "solid",
  "Doel gehaald" = "solid",
  "Doel niet gehaald" = "solid",
  "Stabiel hoog" = "solid",
  "Alle bedrijven" = "dashed"
)

Graph <- datameans1 %>%
  filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>% 
  group_by(jaar, Typebedrijf, voedermiddel) %>% 
  reframe(`VEM/kg DS` = mean(`VEM/kg DS`, na.rm = TRUE)) %>%
  mutate(voedermiddel = factor(voedermiddel,
                               levels = c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten"))) %>%
  
  bind_rows(
    datameans1 %>%
      filter(voedermiddel %in% c("Vers gras", "Graskuil", "Snijmais","Krachtvoer","Bijproducten")) %>% 
      group_by(jaar, voedermiddel) %>% 
      reframe(`VEM/kg DS` = mean(`VEM/kg DS`, na.rm = TRUE)) %>%
      mutate(Typebedrijf = "Alle bedrijven")
  ) %>%
  
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag",
                                         "Doel gehaald",
                                         "Doel niet gehaald",
                                         "Stabiel hoog",
                                         "Alle bedrijven")
  ),
  voedermiddel = factor(voedermiddel,
                        levels = c("Vers gras",
                                   "Graskuil",
                                   "Snijmais",
                                   "Krachtvoer",
                                   "Bijproducten")))

data2 <- datameans1 %>%
  distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`, Typebedrijf) %>%
  group_by(Typebedrijf, jaar) %>%
  summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE),
            .groups = "drop") %>%
  
  bind_rows(
    datameans1 %>%
      distinct(jaar, bedrijfID, `rantsoenVEM/kg DS`) %>%
      group_by(jaar) %>%
      summarise(`rantsoenVEM/kg DS` = mean(`rantsoenVEM/kg DS`, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Typebedrijf = "Alle bedrijven")
  ) %>%
  
  mutate(Typebedrijf = factor(Typebedrijf,
                              levels = c("Stabiel laag",
                                         "Doel gehaald",
                                         "Doel niet gehaald",
                                         "Stabiel hoog",
                                         "Alle bedrijven")
  )) 

p1 <- ggplot() +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  
  geom_line(
    data = data2 %>% filter(Typebedrijf != "Alle bedrijven"),
    aes(x = as.numeric(jaar), y = `rantsoenVEM/kg DS`,
        color = Typebedrijf, linetype = Typebedrijf),
    size = 1.2
  ) +
  
  geom_point(
    data = data2 %>% filter(Typebedrijf != "Alle bedrijven"),
    aes(x = jaar, y = `rantsoenVEM/kg DS`, color = Typebedrijf),
    size = 2
  ) +
  
  scale_color_manual(values = cols, labels = type_labels, breaks = names(type_labels)) +
  scale_linetype_manual(values = types, labels = type_labels, breaks = names(type_labels)) +
  
  labs(y = "VEM/kg DS rantsoen") +
  theme_minimal() +
  facet_wrap(. ~ "Rantsoen") +
  
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  
  coord_cartesian(ylim = c(880, 1125))

p2 <- ggplot(
  Graph %>% filter(Typebedrijf != "Alle bedrijven")%>%  filter(voedermiddel != "Bijproducten"),
  aes(x = jaar,
      y = `VEM/kg DS`,
      group = Typebedrijf,
      color = Typebedrijf,
      linetype = Typebedrijf)
) +
  
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  
  facet_wrap(~ voedermiddel, nrow = 1) +
  
  scale_color_manual(values = cols, labels = type_labels, breaks = names(type_labels)) +
  scale_linetype_manual(values = types, labels = type_labels, breaks = names(type_labels)) +
  
  ylab("VEM/kg DS") +
  coord_cartesian(ylim = c(880, 1125)) +
  
  theme_minimal() +
  
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

p_final <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  widths = c(0.25, 0.8),
  common.legend = TRUE,
  legend = "bottom"
)

ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report//groeplinevoedermiddelenVEMgehalte2.jpg", plot = p_final, width = 8, height =3, dpi = 300)

#prijzen voer#####################################################################################################
ggplot(datameans1 %>% filter(jaar == "2024") %>% filter(voedermiddel != "Melkproducten") %>% distinct(bedrijfID, voedermiddel, .keep_all=TRUE), 
       aes(x=factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")),
           y=`prijsvoer (euro/kg ds)`, color = factor(voedermiddel, levels=c("Vers gras", "Graskuil", "Snijmais", "Overig ruwvoer", "Krachtvoer","Bijproducten")))) +
  geom_boxplot(outlier.shape = NA, color= "#e29f02") +
  geom_jitter(width = 0.2, alpha = 0.7, color= "#e29f02") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 3)), vjust = -1, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "black", size = 3, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  ylab("Prijs voer (euro/kg DS)")



ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/costvoer.jpg", width = 6, height =3, dpi = 300)
####grafiek neveneffecten##########################################################################################
test <- datameans1 %>%
  filter(`bruikbaar voor alle andere analyses?` != "nee") %>%
  select(
    bedrijfID,
    jaar,
    `rantsoenRE gehalte (g/kg DS)`,
    `fpcmProductie per koe (kg)`,
    `Saldo incl. jongvee (euro/koe)`,
    `eiwit eigen land DZK (%)`,
    `emissie totaal per ton meetmelk (kg CO2-eq/ton FPCM)`,
    `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)`,
  ) %>%
  distinct()


test_avg <- test %>%
  rename(
    RE = `rantsoenRE gehalte (g/kg DS)`,
    milk = `fpcmProductie per koe (kg)`,
    saldo = `Saldo incl. jongvee (euro/koe)`,
    protein = `eiwit eigen land DZK (%)`,
    co2pens = `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)`,
    co2 = `emissie totaal per ton meetmelk (kg CO2-eq/ton FPCM)`
  )


####grafiek neveneffecten##########################################################################################
test <- datameans1 %>%
  filter(`bruikbaar voor alle andere analyses?` != "nee") %>%
  select(
    bedrijfID,
    jaar,
    `rantsoenRE gehalte (g/kg DS)`,
    `fpcmProductie per koe (kg)`,
    `Saldo incl. jongvee (euro/koe)`,
    `eiwit eigen land DZK (%)`,
    `emissie totaal per ton meetmelk (kg CO2-eq/ton FPCM)`,
    `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)`
  ) %>%
  distinct()


test_avg <- test %>%
  rename(
    RE = `rantsoenRE gehalte (g/kg DS)`,
    milk = `fpcmProductie per koe (kg)`,
    saldo = `Saldo incl. jongvee (euro/koe)`,
    protein = `eiwit eigen land DZK (%)`,
    co2pens = `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)`,
    co2 = `emissie totaal per ton meetmelk (kg CO2-eq/ton FPCM)`
  )


library(fixest)

m_b_milk   <- feols(milk   ~ RE | bedrijfID, data = test_avg, cluster = ~bedrijfID)
m_b_saldo  <- feols(saldo  ~ RE | bedrijfID, data = test_avg, cluster = ~bedrijfID)
m_b_protein<- feols(protein~ RE | bedrijfID , data = test_avg, cluster = ~bedrijfID)
m_b_pens   <- feols(co2pens~ RE | bedrijfID, data = test_avg, cluster = ~bedrijfID)
m_b_co2    <- feols(co2    ~ RE | bedrijfID, data = test_avg, cluster = ~bedrijfID)


library(modelsummary)
library(performance)

modelsummary(
  list(
    "Milk production" = m_b_milk,
    "Saldo" = m_b_saldo,
    "Protein" = m_b_protein,
    "CO2 (pens)" = m_b_pens,
    "CO2 (total)" = m_b_co2
  ),
  coef_map = c("RE" = "Ration protein (RE)"),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log",   # keeps table clean
  output = "html"  # or "word"
)

###############################################################################
# PANEL DIAGNOSTICS FOR ALL MODELS
################################################################################

library(plm)
library(fixest)
library(lmtest)
library(dplyr)

################################################################################
# PANEL DATA
################################################################################

pdata <- pdata.frame(test_avg, index = c("bedrijfID"))

# dependent variables
dep_vars <- c("milk", "saldo", "protein", "co2pens", "co2")

################################################################################
# 1. SERIAL CORRELATION TESTS (WOOLDRIDGE / BREUSCH-GODFREY)
################################################################################

serial_results <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(paste(y, "~ RE"))
  
  test_result <- pbgtest(fmla, data = pdata)
  
  data.frame(
    dependent_variable = y,
    chisq = as.numeric(test_result$statistic),
    df = as.numeric(test_result$parameter),
    p_value = test_result$p.value
  )
})

serial_results_df <- bind_rows(serial_results)

cat("\n================ SERIAL CORRELATION TESTS ================\n")
print(serial_results_df)

################################################################################
# 2. FIXED EFFECTS VS POOLED OLS (F-TEST)
################################################################################

pf_results <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(paste(y, "~ RE"))
  
  # FE model
  fe_model <- plm(
    fmla,
    data = pdata,
    model = "within"
  )
  
  # pooled model
  pool_model <- plm(
    fmla,
    data = pdata,
    model = "pooling"
  )
  
  # F-test
  test_result <- pFtest(fe_model, pool_model)
  
  data.frame(
    dependent_variable = y,
    F_statistic = as.numeric(test_result$statistic),
    df1 = test_result$parameter[1],
    df2 = test_result$parameter[2],
    p_value = test_result$p.value
  )
})

pf_results_df <- bind_rows(pf_results)

cat("\n================ FIXED EFFECTS VS POOLED OLS ================\n")
print(pf_results_df)

################################################################################
# 3. CROSS-SECTIONAL DEPENDENCE TESTS
################################################################################

cd_results <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(paste(y, "~ RE"))
  
  model <- plm(
    fmla,
    data = pdata,
    model = "within"
  )
  
  test_result <- pcdtest(model, test = "cd")
  
  data.frame(
    dependent_variable = y,
    z_value = as.numeric(test_result$statistic),
    p_value = test_result$p.value
  )
})

cd_results_df <- bind_rows(cd_results)

cat("\n================ CROSS-SECTIONAL DEPENDENCE TESTS ================\n")
print(cd_results_df)

################################################################################
# 4. QUADRATIC MODELS
################################################################################

quad_models <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(
    paste(y, "~ RE + I(RE^2) | bedrijfID + jaar")
  )
  
  model <- feols(
    fmla,
    data = test_avg,
    cluster = ~bedrijfID
  )
  
  return(model)
})

names(quad_models) <- dep_vars

################################################################################
# 5. QUADRATIC MODEL SUMMARIES
################################################################################

cat("\n================ QUADRATIC MODEL RESULTS ================\n")

for(i in seq_along(dep_vars)) {
  
  cat("\n--------------------------------------------------\n")
  cat("DEPENDENT VARIABLE:", dep_vars[i], "\n")
  cat("--------------------------------------------------\n")
  
  print(summary(quad_models[[i]]))
}

################################################################################
# 6. HETEROSKEDASTICITY TESTS (BREUSCH-PAGAN)
################################################################################

bp_results <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(paste(y, "~ RE"))
  
  test_result <- bptest(fmla, data = test_avg)
  
  data.frame(
    dependent_variable = y,
    BP_statistic = as.numeric(test_result$statistic),
    df = as.numeric(test_result$parameter),
    p_value = test_result$p.value
  )
})

bp_results_df <- bind_rows(bp_results)

cat("\n================ BREUSCH-PAGAN TESTS ================\n")
print(bp_results_df)

################################################################################
# 7. HAUSMAN TESTS (FIXED VS RANDOM EFFECTS)
################################################################################

hausman_results <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(paste(y, "~ RE"))
  
  fe_model <- plm(
    fmla,
    data = pdata,
    model = "within"
  )
  
  re_model <- plm(
    fmla,
    data = pdata,
    model = "random"
  )
  
  test_result <- phtest(fe_model, re_model)
  
  data.frame(
    dependent_variable = y,
    chisq = as.numeric(test_result$statistic),
    df = as.numeric(test_result$parameter),
    p_value = test_result$p.value
  )
})

hausman_results_df <- bind_rows(hausman_results)

cat("\n================ HAUSMAN TESTS ================\n")
print(hausman_results_df)

################################################################################
# 8. MAIN FIXED EFFECTS MODELS
################################################################################

main_models <- lapply(dep_vars, function(y) {
  
  fmla <- as.formula(
    paste(y, "~ RE | bedrijfID")
  )
  
  feols(
    fmla,
    data = test_avg,
    cluster = ~bedrijfID
  )
})

names(main_models) <- dep_vars

################################################################################
# 9. MODEL TABLE
################################################################################

etable(
  main_models,
  se = "cluster",
  headers = dep_vars,
  digits = 3
)

check_collinearity(m_b_milk)
check_collinearity(m_b_saldo)
check_collinearity(m_b_protein)
check_collinearity(m_b_pens)
check_collinearity(m_b_co2)
library(lmtest)

bptest(milk ~ RE, data = test_avg)
bptest(saldo ~ RE, data = test_avg)
bptest(protein ~ RE, data = test_avg)
bptest(co2pens ~ RE, data = test_avg)
bptest(co2 ~ RE, data = test_avg)

library(plm)

pdata <- pdata.frame(test_avg, index = c("bedrijfID", "jaar"))

pbgtest(milk ~ RE, data = pdata)
pbgtest(saldo ~ RE, data = pdata)
pbgtest(protein ~ RE, data = pdata)
pbgtest(co2pens ~ RE, data = pdata)
pbgtest(co2 ~ RE, data = pdata)

# panel data object
pdata <- pdata.frame(test_avg, index = c("bedrijfID", "jaar"))

# dependent variables
dep_vars <- c("milk", "saldo", "protein", "co2pens", "co2")

# loop over all variables
pf_results <- lapply(dep_vars, function(y) {
  
  # formula
  fmla <- as.formula(paste(y, "~ RE"))
  
  # fixed effects model
  fe_model <- plm(fmla,
                  data = pdata,
                  model = "within")
  
  # pooled OLS model
  pool_model <- plm(fmla,
                    data = pdata,
                    model = "pooling")
  
  # pF test
  test_result <- pFtest(fe_model, pool_model)
  
  # return tidy output
  data.frame(
    dependent_variable = y,
    F_statistic = test_result$statistic,
    df1 = test_result$parameter[1],
    df2 = test_result$parameter[2],
    p_value = test_result$p.value
  )
})

# combine results
pf_results_df <- do.call(rbind, pf_results)

# print
print(pf_results_df)

pcdtest(
  plm(co2 ~ RE, data = pdata, model = "within"),
  test = "cd"
)

m_quad <- feols(
  co2 ~ RE + I(RE^2) | bedrijfID + jaar,
  data = test_avg,
  cluster = ~bedrijfID
)

summary(m_quad)




test_avg <- test_avg %>%
  group_by(bedrijfID) %>%
  arrange(jaar) %>%
  mutate(RE_lag = lag(RE))

m_lag <- feols(
  co2 ~ RE_lag | bedrijfID + jaar,
  data = test_avg,
  cluster = ~bedrijfID
)

etable(
  m_b_milk,
  m_b_saldo,
  m_b_protein,
  m_b_pens,
  m_b_co2,
  se = "cluster"
)



test_avg %>%
  group_by(bedrijfID) %>%
  summarise(sd_RE = sd(RE, na.rm = TRUE)) %>%
  summary()


ggplot(test_avg, aes(x = RE, y = milk)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

results <- bind_rows(
  tidy(m_b_milk)   %>% mutate(model = "Milk"),
  tidy(m_b_saldo)  %>% mutate(model = "Saldo"),
  tidy(m_b_protein)%>% mutate(model = "Protein"),
  tidy(m_b_pens)   %>% mutate(model = "CO2 pens"),
  tidy(m_b_co2)    %>% mutate(model = "CO2 total")
) %>%
  filter(term == "RE")




ggplot(results, aes(x = estimate, y = model)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect van RE",
    y = "",
    title = "Neveneffecten van rantsoen eiwit (RE)"
  ) +
  theme_minimal()



library(modelsummary)

modelsummary(
  list(
    "Milk production" = m_b_milk,
    "Saldo" = m_b_saldo,
    "Protein" = m_b_protein,
    "CO2 (pens)" = m_b_pens,
    "CO2 (total)" = m_b_co2
  ),
  coef_map = c("RE" = "Ration protein (RE)"),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Adj|AIC|BIC|Log",   # keeps table clean
  output = "html"  # or "word"
)



results <- bind_rows(
  tidy(m_b_milk) %>% mutate(model = "Milk"),
  tidy(m_b_saldo) %>% mutate(model = "Saldo"),
  tidy(m_b_protein) %>% mutate(model = "Protein"),
  tidy(m_b_pens) %>% mutate(model = "CO2 pens"),
  tidy(m_b_co2) %>% mutate(model = "CO2 total")
)



results %>%
  filter(term == "RE", p.value < 0.05) %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*"
  )) %>%
  ggplot(aes(x = model, y = estimate)) +
  geom_point() +
  geom_text(aes(label = sig), hjust = -0.3) +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * std.error,
    ymax = estimate + 1.96 * std.error
  )) +
  coord_flip() 




between_table <- bind_rows(
  tidy(m_b_milk)   %>% filter(term == "RE") %>% mutate(outcome = "Milk"),
  tidy(m_b_costs)  %>% filter(term == "RE") %>% mutate(outcome = "Costs"),
  tidy(m_b_protein)%>% filter(term == "RE") %>% mutate(outcome = "Protein"),
  tidy(m_b_co2)    %>% filter(term == "RE") %>% mutate(outcome = "CO2")
) %>%
  select(outcome, estimate, std.error, p.value)



r2_between <- data.frame(
  outcome = c("Milk","Costs","Protein","CO2"),
  r2 = c(
    summary(m_b_milk)$r.squared,
    summary(m_b_costs)$r.squared,
    summary(m_b_protein)$r.squared,
    summary(m_b_co2)$r.squared
  )
)

between_table <- between_table %>%
  left_join(r2_between, by = "outcome") %>%
  mutate(across(c(estimate, std.error, p.value, r2), ~ round(.x, 3)))

test_change <- test %>%
  arrange(bedrijfID, jaar) %>%
  group_by(bedrijfID) %>%
  mutate(
    d_RE = `rantsoenRE gehalte (g/kg DS)` - lag(`rantsoenRE gehalte (g/kg DS)`),
    d_milk = `fpcmProductie per koe (kg)` - lag(`fpcmProductie per koe (kg)`),
    d_costs = `Voerkosten  incl. jongvee (euro/koe)` - lag(`Voerkosten  incl. jongvee (euro/koe)`),
    d_protein = `eiwit eigen land DZK (%)` - lag(`eiwit eigen land DZK (%)`),
    d_co2 = `CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)` -
      lag(`CO2 pensfermentatie: koeien, excl. reductie (g CO2-eq/kg FPCM)`)
  ) %>%
  ungroup()

test_change2 <- test_change %>%
  filter(!is.na(d_RE))

m_w_milk <- lm(d_milk ~ d_RE+ factor(jaar), data = test_change2)
m_w_costs <- lm(d_costs ~ d_RE+ factor(jaar), data = test_change2)
m_w_protein <- lm(d_protein ~ d_RE+ factor(jaar), data = test_change2)
m_w_co2 <- lm(d_co2 ~ d_RE+ factor(jaar), data = test_change2)

within_table <- bind_rows(
  tidy(m_w_milk)   %>% filter(term == "d_RE") %>% mutate(outcome = "Milk"),
  tidy(m_w_costs)  %>% filter(term == "d_RE") %>% mutate(outcome = "Costs"),
  tidy(m_w_protein)%>% filter(term == "d_RE") %>% mutate(outcome = "Protein"),
  tidy(m_w_co2)    %>% filter(term == "d_RE") %>% mutate(outcome = "CO2")
) %>%
  select(outcome, estimate, std.error, p.value)

r2_within <- data.frame(
  outcome = c("Milk","Costs","Protein","CO2"),
  r2 = c(
    summary(m_w_milk)$r.squared,
    summary(m_w_costs)$r.squared,
    summary(m_w_protein)$r.squared,
    summary(m_w_co2)$r.squared
  )
)

within_table <- within_table %>%
  left_join(r2_within, by = "outcome") %>%
  mutate(across(c(estimate, std.error, p.value, r2), ~ round(.x, 3)))




test_avg <- test %>%
  group_by(bedrijfID) %>%
  summarise(
    RE = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE),
    milk = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
    .groups = "drop"
  )

p_between <- ggplot(test_avg, aes(RE, milk)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    x = "Average RE (g/kg DS)",
    y = "Average milk production (kg FPCM)",
    title = "Between-farm relationship"
  )

p_pooled <- ggplot(test, aes(
  x = `rantsoenRE gehalte (g/kg DS)`,
  y = `fpcmProductie per koe (kg)`
)) +
  geom_point(color = "#e29f02", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#213b73") +
  facet_wrap(~ jaar) +
  theme_minimal() +
  labs(
    x = "RE (g/kg DS)",
    y = "Milk production (kg FPCM)",
    title = "Pooled relationship by year"
  )

test_change <- test %>%
  arrange(bedrijfID, jaar) %>%
  group_by(bedrijfID) %>%
  mutate(
    d_RE = `rantsoenRE gehalte (g/kg DS)` - lag(`rantsoenRE gehalte (g/kg DS)`),
    d_milk = `fpcmProductie per koe (kg)` - lag(`fpcmProductie per koe (kg)`)
  ) %>%
  ungroup()

test_change2 <- test_change %>%
  filter(!is.na(d_RE), !is.na(d_milk))

model <- lm(d_milk ~ d_RE, data = test_change2)
summary(model)

p_within <- ggplot(test_change2, aes(x = d_RE, y = d_milk)) +
  geom_point(color = "#e29f02", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color = "#213b73"
  ) +
  theme_minimal() +
  labs(
    x = "Change in RE (Δ g/kg DS)",
    y = "Change in milk production (Δ kg FPCM)",
    title = "Within-farm changes"
  )

ggarrange(p_between, p_within, ncol = 2)




test_change <- test %>%
  arrange(bedrijfID, jaar) %>%
  group_by(bedrijfID) %>%
  mutate(
    d_RE = `rantsoenRE gehalte (g/kg DS)` - lag(`rantsoenRE gehalte (g/kg DS)`),
    d_milk = `fpcmProductie per koe (kg)` - lag(`fpcmProductie per koe (kg)`)
  ) %>%
  ungroup()

test_change2 <- test_change %>%
  filter(!is.na(d_RE), !is.na(d_milk))

model <- lm(d_milk ~ d_RE, data = test_change2)
summary(model)

ggplot(test_change2, aes(x = d_RE, y = d_milk)) +
  geom_point(color = "#e29f02", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#213b73") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color = "#213b73"
  ) +
  xlab("Change in RE (g/kg DS)") +
  ylab("Change in milk production (kg FPCM)") +
  theme_minimal()

ggarrange(p1,p2)
ggsave("C:/Rfiles/WR/Koe en eiwit 2025/Graphs/Final/Report/ammoniak relatie.jpg", width =8, height = 3.5, dpi = 300)

# BETWEEN-FARM RELATIE
test_mean <- test %>%
  group_by(bedrijfID) %>%
  summarise(
    RE = mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE),
    milk = mean(`fpcmProductie per koe (kg)`, na.rm = TRUE),
    .groups = "drop"
  )

p_between <- ggplot(test_mean, aes(RE, milk)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  theme_minimal() +
  labs(
    x = "Gemiddeld RE-gehalte (g/kg DS)",
    y = "Gemiddelde melkproductie (kg FPCM)",
    title = "Tussen bedrijven"
  )+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    color = "#213b73"
  ) 


