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
library("languageserver")
library(ggalluvial)
library(ggpmisc)
library(modelsummary)

dontdelete <- read_excel("Koe_en_eiwit/Koe en eiwit_2026/Data/datameans3.xlsx")
datameans1 <- dontdelete

REgroup.colors <- c("CP > 165" = "#ff4d4d", "CP 161-165" = "#ffc14d", "CP 156-160" ="#3385ff", "CP < 156" = "#00b33c", "NA" = "darkgray")
REgroup.breaks <- c("CP < 156", "CP 156-160", "CP 161-165", "CP > 165")

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
  mutate(REgroup = factor(REgroup, levels= c("RE > 165","RE 161-165","RE 156-160", "RE < 156"),
  labels= c("CP > 165","CP 161-165","CP 156-160", "CP < 156")))

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
  labs(y = "Percentage of farms (%)", x = "Jaar", fill = "RE groep") +
  scale_fill_manual(values = REgroup.colors, breaks = REgroup.breaks)+
  ggtitle(" ")

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/alluvium.png"
       , width = 8, height = 4, dpi = 600)

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
mutate(
  Categorie = ifelse(Categorie == "Waarde1",
                    "Cow & Protein",
                    "National average")
)
#figuur lijngrafiek
ggplot(data2, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "Black", size = 1) +
  geom_line(size = 1.2) +  # Lijn dikte
  #annotate("text", x = 2022, y = max(data$Waarde), label = "Startjaar", vjust = -3, hjust=-0.2, color = "gray") +
  labs(title = "Eiwitgehalte rantsoen Koe & Eiwit daalt t.o.v. NL-gemiddelde
       ",
       x = "Jaar",
       y =  "Dietary crude protein content\n(g CP/kg DM)",
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
           label = "Cow & Protein\n start year", hjust = -0.1, vjust = 0.7, color = "gray",
           fontface = "bold", size=4) +
  annotate("text", x = 2020, y = 155.5, 
           label = "Goal: 155 CP", hjust = -0.1, vjust = 0, color = "Black",
           fontface = "bold", size=4) +
  scale_x_continuous(
    limits = c(2020, 2025.3),
    expand = c(0, 0)
  )

data2_2024 <- data2 %>%
  filter(Jaar <= 2024)

ggplot(data2_2024, aes(x = Jaar, y = Waarde, color = Categorie)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_hline(yintercept = 155, linetype = "dashed", color = "black", size = 1) +
  geom_line(size = 1.2) +
  labs(
    title = "Dietary crude protein content in Koe & Eiwit decreases compared with the national average",
    x = "Year",
    y = "Dietary crude protein content\n(g CP/kg DM)",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#1A6B04", "#E29E00")) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_text(size = 14),
    plot.title = element_blank()
  ) +
  geom_text(
    data = data2_2024 %>%
      group_by(Categorie) %>%
      filter(!is.na(Waarde)) %>%
      slice_max(Jaar),
    aes(label = Categorie),
    hjust = -0.05,
    nudge_y = c(-0.5, 0.5),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_cartesian(
    ylim = c(150, 172),
    clip = "off"
  ) +
  theme(
    plot.margin = margin(5.5, 75, 5.5, 5.5)
  ) +
  annotate(
    "text",
    x = 2022,
    y = 172,
    label = "Cow & Protein\n start year",
    hjust = -0.1,
    vjust = 0.7,
    color = "gray",
    fontface = "bold",
    size = 4
  ) +
  annotate(
    "text",
    x = 2020,
    y = 155.5,
    label = "Goal: 155 CP",
    hjust = -0.1,
    vjust = 0,
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(
    limits = c(2020, 2025.3),
    expand = c(0, 0)
  )

ggsave(
  "Koe_en_eiwit/Koe en eiwit_2026/Graphs/RE K & E vs Nederland_2024.png",
  width = 7,
  height = 4,
  dpi = 600
)
`

# Sla de grafiek op als een jpg-bestand
ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/RE K & E vs Nederland.png"
       , width = 7, height = 4, dpi = 600)

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
  mutate(REgroup = factor(REgroup, levels= c("RE > 165","RE 161-165","RE 156-160", "RE < 156"),
  labels= c("CP > 165","CP 161-165","CP 156-160", "CP < 156"))) %>%
  mutate(grondsoort = factor(grondsoort, levels= c("Klei","Veen","Zand"),
  labels= c("Clay","Peat","Sand")))

df_groep <- df_sorted %>%
  group_by(jaar, grondsoort, intensiteitcat) %>%
  summarise(
    `rantsoenRE gehalte (g/kg DS)` =
      mean(`rantsoenRE gehalte (g/kg DS)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    REgroup = case_when(
      `rantsoenRE gehalte (g/kg DS)` < 156 ~ "RE < 156",
      `rantsoenRE gehalte (g/kg DS)` >= 156 &
        `rantsoenRE gehalte (g/kg DS)` < 161 ~ "RE 156-160",
      `rantsoenRE gehalte (g/kg DS)` >= 161 &
        `rantsoenRE gehalte (g/kg DS)` <= 165 ~ "RE 161-165",
      `rantsoenRE gehalte (g/kg DS)` > 165 ~ "RE > 165"
    ),
    
    REgroup = factor(
      REgroup,
      levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156"),
      labels = c("CP > 165", "CP 161-165", "CP 156-160", "CP < 156")
    ),

    intensiteitcat = factor(
      intensiteitcat,
      levels = c(
        "< 14000 kg/ha",
        "14000 - 20000 kg/ha",
        "> 20000 kg/ha"
      )
    )
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

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Heatplot klasse.png", width = 10, height = 3.7, dpi = 1500)

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
                              labels = c( "Low (<155 RE)",
  "Moved (<155 RE)",
  "Moved (>155 RE)",
  "High (>160 RE)")),
             REgroup = factor(
      REgroup,
      levels = c("RE > 165", "RE 161-165", "RE 156-160", "RE < 156"),
      labels = c("CP > 165", "CP 161-165", "CP 156-160", "CP < 156")
    ))

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
  scale_x_continuous(breaks = seq(min(df_sorted$jaar, na.rm = TRUE),
                                  max(df_sorted$jaar, na.rm = TRUE),
                                  by = 2))

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/heatplot.png", width =8, height = 3.5, dpi = 600)

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
  "High (RE >160)" = alpha("#ff4d4d"),
  "Moved (RE >155)" = alpha("#ffc14d"),
  "Moved (RE <155)" = alpha("#3385ff"),
  "Low (RE <155)" = alpha("#00b33c")
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
      LegendLabel == "Stabiel laag" ~ "Low (RE <155)",
      LegendLabel == "Doel gehaald" ~ "Moved (RE <155)",
      LegendLabel == "Doel niet gehaald" ~ "Moved (RE >155)",
      LegendLabel == "Stabiel hoog" ~ "High (RE >160)",
      TRUE ~ LegendLabel
    )
  )%>%
  mutate(LegendLabel = factor(LegendLabel, 
                              levels = c("Low (RE <155)",
                                         "Moved (RE <155)",
                                         "Moved (RE >155)",
                                         "High (RE >160)"))) %>%
mutate(
  voedermiddel = case_when(
    voedermiddel == "Vers gras" ~ "Fresh grass",
    voedermiddel == "Graskuil" ~ "Grass silage",
    voedermiddel == "Snijmais" ~ "Maize silage",
    voedermiddel == "Krachtvoer" ~ "Concentrates",
    voedermiddel == "Bijproducten" ~ "By-products",
    TRUE ~ voedermiddel
  )
)  %>%
mutate(voedermiddel = factor(voedermiddel, levels = c("Fresh grass", "Grass silage", "Maize silage", "Concentrates", "By-products")))                            

                           

# --- Plot Voeropname ---
p1 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Low (RE <155)", "Moved (RE <155)", "Moved (RE >155)", "High (RE >160)")),
            aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel, alpha = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Low (RE <155)", "Moved (RE <155)", "Moved (RE >155)", "High (RE >160)")),
             aes(x = jaar, y = opname, group = LegendLabel, color = LegendLabel, alpha = LegendLabel),
             size = 1.7) +
  facet_wrap(~ voedermiddel, nrow = 1) +
  ylab("Feed intake\n(kg DM/animal/day)") +
  scale_color_manual(values = bg_colors) +
  geom_line(
  data = final_df_plot %>%
    filter(LegendLabel %in% c("Low (RE <155)",
                              "Moved (RE <155)",
                              "Moved (RE >155)",
                              "High (RE >160)")),
  aes(
    x = jaar,
    y = opname,
    group = LegendLabel,
    color = LegendLabel,
    alpha = LegendLabel
  ),
  size = 1.2
) +
scale_alpha_manual(
  values = c(
    "Low (RE <155)" = 0.3,
    "Moved (RE <155)" = 1,
    "Moved (RE >155)" = 0.3,
    "High (RE >160)" = 0.3
  )
)+
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
        legend.text = element_text(size = 11),
        legend.position = "right")+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Plot RE gehalte ---
p2 <- ggplot(final_df_plot) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray", size = 1) +
  geom_line(data = final_df_plot %>% 
              filter(LegendLabel %in% c("Low (RE <155)", "Moved (RE <155)", "Moved (RE >155)", "High (RE >160)")),
            aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel, alpha = LegendLabel),
            size = 1.2)+
  geom_point(data = final_df_plot %>% 
               filter(LegendLabel %in% c("Low (RE <155)", "Moved (RE <155)", "Moved (RE >155)", "High (RE >160)")),
             aes(x = jaar, y = RE, group = LegendLabel, color = LegendLabel, alpha = LegendLabel),
             size = 1.7) +
  #geom_point(data = groep_avg_line,aes(x = jaar, y = RE, group = voedermiddel, color = paste("Groep:", groep_val)), size = 1.5, alpha=0.7 )+
  facet_wrap(~ voedermiddel, scales = "free_y", nrow = 1) +
  geom_blank(aes(y = new_ymin_rounded)) +
  geom_blank(aes(y = new_ymax_rounded)) +
  scale_color_manual(values = bg_colors) +
  ylab("Crude protein level\n(g/kg DM)") +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 12, color = "White"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "right")+
        scale_alpha_manual(
  values = c(
    "Low (RE <155)" = 0.3,
    "Moved (RE <155)" = 1,
    "Moved (RE >155)" = 1,
    "High (RE >160)" = 0.3
  )
)+
  scale_x_continuous(breaks = seq(min(final_df_plot$jaar, na.rm = TRUE),
                                  max(final_df_plot$jaar, na.rm = TRUE),
                                  by = 2))

# --- Combine and annotate plots ---
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "right")

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Lijnengrafiektypebedrijf3.png"
       , width = 11, height = 4.5, dpi = 600)



# ============================================================
# CHOOSE WHICH GROUP TO HIGHLIGHT
# ============================================================
library(ggpubr)

plot_groups <- c(
  "Low (RE <155)",
  "Moved (RE <155)",
  "Moved (RE >155)",
  "High (RE >160)"
)

make_plot <- function(highlight_group = NULL){

  plot_data_all <- final_df_plot %>%
    filter(LegendLabel %in% plot_groups)

  if(is.null(highlight_group)){
    plot_data_highlight <- plot_data_all[0, ]
  } else {
    plot_data_highlight <- plot_data_all %>%
      filter(LegendLabel == highlight_group)
  }

  # ======================================================
  # P1 FEED INTAKE
  # ======================================================

  p1 <- ggplot() +

    geom_vline(
      xintercept = 2022,
      linetype = "dashed",
      color = "gray"
    ) +

    geom_line(
      data = plot_data_all,
      aes(
        jaar,
        opname,
        group = LegendLabel,
        color = LegendLabel
      ),
      alpha = 0.3,
      size = 1.2
    ) +

    geom_line(
      data = plot_data_highlight,
      aes(
        jaar,
        opname,
        group = LegendLabel,
        color = LegendLabel
      ),
      alpha = 1,
      size = 1.6
    ) +

    geom_point(
      data = plot_data_highlight,
      aes(
        jaar,
        opname,
        color = LegendLabel
      ),
      size = 2.5
    ) +

    facet_wrap(~voedermiddel, nrow = 1) +

    scale_color_manual(values = bg_colors) +

    ylab("Feed intake\n(kg DM/animal/day)") +

    scale_x_continuous(
      breaks = seq(
        min(final_df_plot$jaar, na.rm = TRUE),
        max(final_df_plot$jaar, na.rm = TRUE),
        by = 2
      )
    ) +

    theme_minimal() +

    theme(
      legend.title = element_blank(),
      legend.position = "right",
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
              axis.line = element_line(color = "black"),
              strip.text = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12)
    )

  # ======================================================
  # P2 CP CONTENT
  # ======================================================

  p2 <- ggplot() +

    geom_vline(
      xintercept = 2022,
      linetype = "dashed",
      color = "gray"
    ) +

    geom_line(
      data = plot_data_all,
      aes(
        jaar,
        RE,
        group = LegendLabel,
        color = LegendLabel
      ),
      alpha = 0.3,
      size = 1.2
    ) +

    geom_line(
      data = plot_data_highlight,
      aes(
        jaar,
        RE,
        group = LegendLabel,
        color = LegendLabel
      ),
      alpha = 1,
      size = 1.6
    ) +

    geom_point(
      data = plot_data_highlight,
      aes(
        jaar,
        RE,
        color = LegendLabel
      ),
      size = 2.5
    ) +

    facet_wrap(
      ~voedermiddel,
      scales = "free_y",
      nrow = 1
    ) +

    geom_blank(
      data = final_df_plot,
      aes(y = new_ymin_rounded)
    ) +

    geom_blank(
      data = final_df_plot,
      aes(y = new_ymax_rounded)
    ) +

    scale_color_manual(values = bg_colors) +

    ylab("Crude protein content\n(g CP/kg DM)") +

    scale_y_continuous(
      breaks = scales::breaks_width(20)
    ) +

    scale_x_continuous(
      breaks = seq(
        min(final_df_plot$jaar, na.rm = TRUE),
        max(final_df_plot$jaar, na.rm = TRUE),
        by = 2
      )
    ) +

    theme_minimal() +

    theme(
      legend.title = element_blank(),
      legend.position = "right",
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
              axis.line = element_line(color = "black"),
              strip.text = element_text(size = 13, color = "White"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12)
    )

  ggarrange(
    p1,
    p2,
    nrow = 2,
    common.legend = TRUE,
    legend = "right"
  )
}

fig_background <- make_plot()

fig_low <- make_plot("Low (RE <155)")

fig_moved_low <- make_plot("Moved (RE <155)")

fig_moved_high <- make_plot("Moved (RE >155)")

fig_high <- make_plot("High (RE >160)")

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Background.png", fig_background,
       width = 11, height = 4.5, dpi = 600)

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Low.png", fig_low,
       width = 11, height = 4.5, dpi = 600)

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Moved_low.png", fig_moved_low,
       width = 11, height = 4.5, dpi = 600)

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Moved_high.png", fig_moved_high,
       width = 11, height = 4.5, dpi = 600)

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/High.png", fig_high,
       width = 11, height = 4.5, dpi = 600)



ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/Lijnengrafiektypebedrijf3.png"
       , width = 11, height = 4.5, dpi = 600)


#############################################################################################
#Final 
#############################################################################################

# --- Calculate total ammonia emissions per year ---

# --- Calculate total ammonia emissions per year ---

NH3_year <- datameans1 %>%
  filter(`bruikbaar voor alle andere analyses?` != "nee") %>%
  select(
    bedrijfID, jaar,
    `Ammoniakemissie per ha (kg/ha)`
  ) %>%
  distinct() %>%
  group_by(jaar) %>%
  summarise(
    mean_NH3_ha = sum(`Ammoniakemissie per ha (kg/ha)`, na.rm = TRUE),
    .groups = "drop"
  )


# --- Plot ammonia emissions ---

p1<-ggplot(NH3_year, aes(x = as.factor(jaar), y = mean_NH3_ha)) +


  # Bars
  geom_col(
    width = 0.7,
    fill = "#186C30"
  ) +

  # Reference year
geom_hline(
  yintercept = 7761.9,
  linetype = "dashed",
  color = "grey50",
  linewidth = 0.9
)+

  # Labels above bars
  geom_text(
    aes(label = round(mean_NH3_ha, 1)),
    vjust = -0.4,
    size = 3.5
  ) +

  # Axis labels
  labs(
    y = "Ammonia emissions (kg NH₃/ha)",
  ) +

  # Leave space for labels
  expand_limits(
    y = max(NH3_year$mean_NH3_ha, na.rm = TRUE) * 1.1
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.title = element_text(
      size = 11
    ),
    axis.text = element_text(
      size = 10
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(
      color = "black"
    ),
    axis.title.x = element_blank()
  )

ggsave("Koe_en_eiwit/Koe en eiwit_2026/Graphs/line.png", p1,
       width = 6, height = 3.5, dpi = 600)
