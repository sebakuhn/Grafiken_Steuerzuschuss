library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(aokaux)
library(ggsci)

# Path to updated raw data
raw_path <- "00_raw_data/260129_Aktualisierung_Grafiken_Steuerzuschuss.xlsx"

# =============================================================================
# 1) Kuchendiagramm – Einnahmen des Gesundheitsfonds 2024
# =============================================================================

# The sheet "Einnahmen_GF_2024" has pre-aggregated categories in columns E/F:
# Row 18: Beiträge        | 0.85287
# Row 26: Zusatzbeiträge  | 0.09760
# Row 29: Steuerzuschuss  | 0.04639
# Row 30: Weitere Einnahmen | 0.00314

data_pie <- tibble(
  Posten    = c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere Einnahmen"),
  Prozentual = c(0.85287, 0.09760, 0.04639, 0.00314)
)

data_pie <- data_pie %>%
  mutate(Posten = factor(Posten, levels = rev(Posten))) %>%
  arrange(desc(Posten)) %>%
  mutate(ypos = cumsum(Prozentual) - 0.5 * Prozentual)

pie <- ggplot(data_pie, aes(x = "", y = Prozentual, fill = Posten)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c(
    "Beiträge"           = "#000080",
    "Zusatzbeiträge"     = "#0000CD",
    "Steuerzuschuss"     = "#0000FF",
    "Weitere Einnahmen"  = "#00BFFF"
  )) +
  geom_text(x = 1.3, aes(
    family = "AOK Buenos Aires",
    y = ypos,
    label = scales::percent(Prozentual, accuracy = 0.1)
  ), color = "white", size = 12) +
  labs(
    subtitle = "",
    fill = ""
  ) +
  theme_aok() +
  theme(
    text = element_text(family = "AOK Buenos Aires"),
    legend.position = "right",
    legend.text = element_text(family = "Frutiger for AOK", size = 30),
    plot.title = element_text(hjust = 0, size = 25, face = "italic", color = "#000080"),
    plot.subtitle = element_text(hjust = 0, size = 20),
    plot.caption = element_text(hjust = 0, size = 25, color = "black")
  )

jpeg("06_graphs/Einnahmen_Pie_Seba.jpeg",
     units = "cm", width = 50, height = 30, res = 500)
pie + scale_fill_d3()
dev.off()

jpeg("06_graphs/Einnahmen_Pie_Seba_bw.jpeg",
     units = "cm", width = 50, height = 30, res = 500)
pie + scale_fill_grey()
dev.off()

# =============================================================================
# 2) Kipppunkt – Einnahmen- und Ausgabenentwicklung (2010–2026)
# =============================================================================

kipppunkt <- read_excel(raw_path, sheet = "Tab_Ausgaben_Beitragseinnahmen")

# Keep only the two relevant rows and gather years
kipppunkt <- kipppunkt %>%
  rename(Wert = 1) %>%
  gather(Jahr, "Value", 2:18) %>%
  filter(
    Wert == "Beitragseinnahmen ohne Zusatzbeiträge" |
    Wert == "Ausgaben GKV"
  ) %>%
  mutate(
    Wert = as.factor(Wert),
    Value = as.numeric(Value)
  )

# Annotations for key events
label <- data.frame(
  Jahr  = c(2, 6, 11, 16),
  Value = c(145, 160, 197, 310),
  label = c(
    "Erhöhung allg. \n Beitragssatz von\n 14,9% auf 15,5%",
    "Absenkung allg. \n Beitragssatz von\n 15,5% auf 14,6%",
    "Geringere\n Grundlohnsteigerung\n aufgrund von Covid-19",
    "Strukturelle\n Deckungslücke\n wächst weiter"
  )
)

kipp_graph <- ggplot(kipppunkt, aes(x = Jahr, y = Value, group = Wert)) +
  geom_line(aes(colour = Wert), linewidth = 3.5) +
  scale_color_manual(values = c(
    "Ausgaben GKV" = "#000080",
    "Beitragseinnahmen ohne Zusatzbeiträge" = "#0000FF"
  )) +
  labs(
    subtitle = "",
    caption = "*Prognose des Schätzerkreises",
    colour = ""
  ) +
  ylab("in Mrd. Euro") +
  ylim(140, 390) +
  theme_aok() +
  theme(
    text = element_text(family = "AOK Buenos Aires"),
    legend.position = c(0.3, 0.9),
    legend.direction = "vertical",
    legend.key.size = unit(3, "lines"),
    legend.text = element_text(size = 34, color = "black"),
    plot.title = element_text(hjust = 0, size = 33, face = "italic", color = "#000080"),
    plot.subtitle = element_text(hjust = 0, size = 33),
    plot.caption = element_text(hjust = 0, size = 25, face = "italic", color = "black"),
    axis.text.x = element_text(size = 32, color = "black", angle = 45, hjust = 1),
    axis.title.x = element_text(size = 35),
    axis.text.y = element_text(size = 32, color = "black"),
    axis.title.y = element_text(size = 35),
    panel.grid = element_blank()
  ) +
  geom_text(data = label,
            aes(family = "AOK Buenos Aires", x = Jahr, y = Value, label = label),
            size = 10.5, inherit.aes = FALSE) +
  geom_segment(aes(x = 2, y = 155, xend = 2, yend = 167),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
  geom_segment(aes(x = 6, y = 170, xend = 6, yend = 182),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
  geom_segment(aes(x = 11, y = 207, xend = 11, yend = 219),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
  geom_segment(aes(x = 16, y = 320, xend = 16, yend = 338),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"))

jpeg("06_graphs/Einnahmen_Ausagben_Seba.jpeg",
     units = "cm", width = 50, height = 40, res = 600)
kipp_graph + scale_color_d3()
dev.off()

jpeg("06_graphs/Einnahmen_Ausagben_Seba_bw.jpeg",
     units = "cm", width = 50, height = 40, res = 600)
kipp_graph + scale_color_grey()
dev.off()

# =============================================================================
# 3) Balkendiagramm – Entwicklung des Steuerzuschusses (2004–2026)
# =============================================================================

balken <- read_excel(raw_path, sheet = "Tab_Höhe_Anteil", skip = 1)

# Rename first column, select §221 and §221a rows, gather 23 year columns
balken <- balken %>%
  rename(Wert = 1) %>%
  gather(Jahr, "Value", 2:24) %>%
  filter(
    Wert == "Steuerzuschuss nach § 221 SGB V" |
    Wert == "Steuerzuschuss nach § 221a SGB V"
  ) %>%
  mutate(
    Wert = as.factor(Wert),
    Value = replace_na(as.numeric(Value), 0)
  )

barplot <- ggplot(balken, aes(x = Jahr, y = Value, fill = Wert)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(aes(
    family = "AOK Buenos Aires",
    x = Jahr, y = Value,
    label = ifelse(Value == 0, "", format(Value, decimal.mark = ",")),
    group = Wert
  ),
  position = position_stack(vjust = 0.5, reverse = TRUE),
  color = "white", size = 7) +
  guides(fill = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values = c("#000080", "#0000FF")) +
  labs(
    subtitle = "",
    caption = c(
      "* Zusätzlicher Steuerzuschuss i. H. v. 3,5 Mrd. EUR abweichend nach §12a Haushaltsgesetz 2020",
      "\n** einschl. 0,3 Mrd. EUR (2021 und 2022) bzw. 0,15 Mrd. EUR (2023) an die Liquiditätsreserve des GF für Kinderkrankengeld"
    ),
    fill = ""
  ) +
  ylab("in Mrd. Euro") +
  theme_aok() +
  theme(
    text = element_text(family = "AOK Buenos Aires"),
    legend.position = c(0.25, 0.865),
    legend.direction = "vertical",
    legend.key.size = unit(3, "lines"),
    legend.text = element_text(size = 30),
    plot.title = element_text(hjust = 0, size = 25, face = "italic", color = "#000080"),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    plot.caption = element_text(hjust = c(0, 0), size = 20, face = "italic", color = "black"),
    axis.text.x = element_text(size = 24, color = "black", angle = 45, hjust = 1),
    axis.title.x = element_text(size = 35),
    axis.text.y = element_text(size = 27, color = "black"),
    axis.title.y = element_text(size = 35),
    panel.grid = element_blank()
  )

jpeg("06_graphs/Säulen_Seba.jpeg",
     units = "cm", width = 55, height = 30, res = 600)
barplot + scale_fill_d3()
dev.off()

jpeg("06_graphs/Säulen_Seba_bw.jpeg",
     units = "cm", width = 55, height = 30, res = 600)
barplot + scale_fill_grey()
dev.off()
