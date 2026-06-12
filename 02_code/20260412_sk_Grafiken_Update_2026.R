# =============================================================================
# Visualisierungen – Steuerzuschuss Buchpublikation (Kuhn/Illgen)
# Minimalistisches, publikationsreifes Design
# Autor: Sebastian Kuhn | 2026-04-12
# =============================================================================

library(tidyverse)
library(readxl)
library(scales)
library(ggtext)
library(systemfonts)

# --- Konfiguration ------------------------------------------------------------

raw_path <- "00_raw_data/260129_Aktualisierung_Grafiken_Steuerzuschuss.xlsx"
out_path <- "06_graphs/"

# Farbpalette: kontrastreich, drucksicher
col_primary   <- "#5D2A9D"   # kräftiges Violett (Signature)        – Kontrast 9.2 ✓
col_secondary <- "#C2298A"   # leuchtendes Fuchsia/Magenta           – Kontrast 5.3 ✓
col_accent    <- "#00B39B"   # Teal (Original)
col_accent2   <- "#3BA99C"   # abgedunkeltes Türkis – Kontrast ~3.5 (besser, aber nicht ideal für Text)
col_highlight <- "#E02D52"   # Warnsignal-Rot (für Deckungslücken-Ribbon) – Kontrast 4.5 ✓
col_grey      <- "#6E6E6E"   # Grau – etwas dunkler (war #8C8C8C, Kontrast 3.4 ✗ → jetzt 5.7 ✓)
col_lightgrey <- "#E8E8E8"
col_bg        <- "#FFFFFF"

# Diagramm-spezifische Paletten
einnahmen_colors <- c(col_primary, col_secondary, col_accent, col_accent2)

# Typografie: Systemschriften mit Fallback
font_body  <- "Helvetica Neue"

# Minimales Basis-Theme für alle Diagramme
theme_publication <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = font_body) +
    theme(
      # Hintergrund
      plot.background    = element_rect(fill = col_bg, color = NA),
      panel.background   = element_rect(fill = col_bg, color = NA),
      # Rasterlinien
      panel.grid.major.y = element_line(color = col_lightgrey, linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      # Achsen
      axis.line.x        = element_line(color = "#333333", linewidth = 0.4),
      axis.ticks.x       = element_line(color = "#333333", linewidth = 0.3),
      axis.ticks.length  = unit(3, "pt"),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = rel(0.85), color = "#555555",
                                        hjust = 0.5, margin = margin(r = 8)),
      axis.text          = element_text(color = "#444444"),
      # Legende
      legend.position     = "top",
      legend.justification = "left",
      legend.title        = element_blank(),
      legend.key.size     = unit(12, "pt"),
      legend.text         = element_text(size = rel(0.8), color = "#444444"),
      legend.margin       = margin(b = -5),
      # Titel / Quellenangabe
      plot.title          = element_text(face = "bold", size = rel(1.25),
                                         color = "#1a1a1a", margin = margin(b = 4)),
      plot.subtitle       = element_text(size = rel(0.9), color = "#666666",
                                         margin = margin(b = 16)),
      plot.caption = element_text(size = rel(0.7), color = "#777777",
                             hjust = 0, margin = margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin         = margin(20, 20, 15, 15)
    )
}


# =============================================================================
# 1a) Horizontaler Balken – Einnahmen des Gesundheitsfonds 2024
# =============================================================================

data_einnahmen <- tibble(
  Posten     = c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere\nEinnahmen"),
  Absolut    = c(266.50, 30.50, 14.49, 0.98),
  Prozentual = c(0.85287, 0.09760, 0.04639, 0.00314)
) %>%
  mutate(
    Posten = fct_inorder(Posten),
    # Label im Balken: Prozentanteil
    pct_label = paste0(format(round(Prozentual * 100, 1), nsmall = 1, decimal.mark = ","), " %"),
    # Label unter dem Balken: Absolutwert
    abs_label = paste0(trimws(format(round(Absolut, 1), nsmall = 1, decimal.mark = ",")), " Mrd. €"),
    # Kumulative Positionen für Label-Platzierung
    xmax = cumsum(Prozentual),
    xmin = lag(xmax, default = 0),
    xmid = (xmin + xmax) / 2
  )

# Label-Positionen für kleine Segmente versetzt, um Überlappung zu vermeiden
data_einnahmen <- data_einnahmen %>%
  mutate(
    # Kategorie-Labels: vertikal versetzt für die kleinen Segmente
    y_cat = case_when(
      Posten == "Beiträge"                        ~ 1.12,
      str_detect(Posten, "Zusatz")                ~ 1.22,
      str_detect(Posten, "Steuer")                ~ 1.12,
      TRUE                                        ~ 1.22  # Weitere Einnahmen
    ),
    # Absolutwert-Labels unten: gleicher Versatz
    y_abs = case_when(
      Posten == "Beiträge"                        ~ 0.18,
      str_detect(Posten, "Zusatz")                ~ 0.08,
      str_detect(Posten, "Steuer")                ~ 0.18,
      TRUE                                        ~ 0.08
    )
  )

p_einnahmen <- ggplot(data_einnahmen) +
  # Horizontaler gestapelter Balken
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 1, fill = Posten),
            color = col_bg, linewidth = 0.8) +
  # Prozent-Labels im Balken (nur Beiträge passt komfortabel)
  geom_text(data = data_einnahmen %>% filter(Prozentual > 0.08),
            aes(x = xmid, y = 0.65, label = pct_label),
            color = "white", fontface = "bold", size = 4.5, family = font_body) +
  # Kategorie-Labels über dem Balken (versetzt)
  geom_text(aes(x = xmid, y = y_cat, label = Posten),
            color = "#333333", size = 3, family = font_body, lineheight = 0.85) +
  # Dünne Verbindungslinien vom Label zum Segment
  geom_segment(data = data_einnahmen %>% filter(Prozentual < 0.5),
               aes(x = xmid, xend = xmid, y = 1.02, yend = y_cat - 0.03),
                              color = "#aaaaaa", linewidth = 0.25) +
  # Absolutwerte unter dem Balken (versetzt)
  geom_text(aes(x = xmid, y = y_abs, label = abs_label),
                        color = "#666666", size = 2.8, family = font_body) +
  scale_fill_manual(values = einnahmen_colors) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(-0.1, 1.5)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Einnahmen des Gesundheitsfonds 2024",
    subtitle = "Verteilung nach Einnahmeart (Gesamteinnahmen: 312,5 Mrd. €)",
    caption  = "Quelle: Eigene Berechnungen auf Basis der KJ1 des Gesundheitsfonds 2024."
  ) +
  theme_void(base_family = font_body) +
  theme(
    plot.background       = element_rect(fill = col_bg, color = NA),
    legend.position       = "none",
    plot.title            = element_text(face = "bold", size = 14, color = "#1a1a1a",
                                         margin = margin(b = 4)),
    plot.subtitle         = element_text(size = 10, color = "#666666",
                                         margin = margin(b = 20)),
    plot.caption = element_text(size = 8, color = "#777777", hjust = 0,
                             margin = margin(t = 20)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(20, 20, 15, 15)
  )

p_einnahmen

# --- 1b) Donut-Variante mit Direkt-Beschriftung und Callout für Mini-Segment ---

data_donut <- tibble(
  Posten     = c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere Einnahmen"),
  Absolut    = c(266.50, 30.50, 14.49, 0.98),
  Prozentual = c(0.85287, 0.09760, 0.04639, 0.00314)
) %>%
  mutate(
    Posten    = fct_inorder(Posten),
    ymax      = cumsum(Prozentual),
    ymin      = lag(ymax, default = 0),
    ymid      = (ymin + ymax) / 2,
    pct_label = paste0(format(round(Prozentual * 100, 1), nsmall = 1, decimal.mark = ","), " %")
  )

kleine_segment <- filter(data_donut, Posten == "Weitere Einnahmen")

p_donut <- ggplot(data_donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.6, fill = Posten)) +
  geom_rect(color = col_bg, linewidth = 1.2) +
  # Prozent-Labels am Außenrand des Rings (> 4 %: Beiträge, Zusatzbeiträge, Steuerzuschuss)
  geom_text(data = filter(data_donut, Prozentual > 0.04),
            aes(x = 3.72, y = ymid, label = pct_label),
            color = "white", fontface = "bold", size = 3.5, family = font_body,
            inherit.aes = FALSE) +
  # Callout-Linie für "Weitere Einnahmen" (radial nach außen, Pfeil zeigt auf Segment)
  annotate("segment",
           x = 4.02, xend = 4.18,
           y = kleine_segment$ymid, yend = kleine_segment$ymid,
                      color = "#666666", linewidth = 0.4,
           arrow = arrow(length = unit(0.1, "cm"), type = "open", ends = "first")) +
  # Callout-Label außerhalb des Donuts
  annotate("text",
           x = 4.22, y = kleine_segment$ymid,
           label = paste0("Weitere Einnahmen\n", kleine_segment$pct_label),
           hjust = 0, size = 3.0, color = "#555555", family = font_body, lineheight = 0.9) +
  coord_polar(theta = "y") +
  xlim(c(1.5, 4.45)) +
  scale_fill_manual(values = einnahmen_colors) +
  labs(
    title    = "Einnahmen des Gesundheitsfonds 2024",
    subtitle = "Verteilung nach Einnahmeart (Gesamteinnahmen: 312,5 Mrd. €)",
    caption  = "Quelle: Eigene Berechnungen auf Basis der KJ1 des Gesundheitsfonds 2024."
  ) +
  theme_void(base_family = font_body) +
  theme(
    plot.background       = element_rect(fill = col_bg, color = NA),
    legend.position       = "bottom",
    legend.direction      = "horizontal",
    legend.justification  = "center",
    legend.text           = element_text(size = 11, color = "#333333", margin = margin(b = 4)),
    legend.key.size       = unit(20, "pt"),
    legend.key            = element_rect(color = NA),
    legend.spacing.y      = unit(4, "pt"),
    legend.title          = element_blank(),
    legend.box.margin     = margin(t = -10),
    plot.title            = element_text(face = "bold", size = 16, color = "#1a1a1a",
                                         margin = margin(b = 2)),
    plot.subtitle         = element_text(size = 11, color = "#666666",
                                         margin = margin(b = -5)),
    plot.caption = element_text(size = 8, color = "#777777", hjust = 0,
                             margin = margin(t = 20)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(10, 10, 5, 10)
  ) +
  guides(fill = guide_legend(byrow = TRUE, ncol = 2))

p_donut

# --- 1c) Horizontaler Balken mit Legende unterhalb ---

# Legenden-Labels ohne Zeilenumbrüche
legend_labels_clean <- paste0(
  c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere Einnahmen"),
  " \u2013 ",
  data_einnahmen$pct_label, " (",
  data_einnahmen$abs_label, ")"
) %>% str_replace_all("\n", " ")

p_einnahmen_legend <- ggplot(data_einnahmen) +
  # Gleicher geom_rect-Ansatz wie Direkt-Label-Variante
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 1, fill = Posten),
            color = col_bg, linewidth = 0.8) +
  # Prozent-Labels im Balken (nur Beiträge – Rest zu schmal)
  geom_text(data = data_einnahmen %>% filter(Prozentual > 0.08),
            aes(x = xmid, y = 0.65, label = pct_label),
            color = "white", fontface = "bold", size = 5, family = font_body) +
  scale_fill_manual(
    values = einnahmen_colors,
    labels = legend_labels_clean
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0.1, 1.15)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Einnahmen des Gesundheitsfonds 2024",
    subtitle = "Verteilung nach Einnahmeart (Gesamteinnahmen: 312,5 Mrd. \u20AC)",
    caption  = "Quelle: Eigene Berechnungen auf Basis der KJ1 des Gesundheitsfonds 2024."
  ) +
  theme_void(base_family = font_body) +
  theme(
    plot.background       = element_rect(fill = col_bg, color = NA),
    legend.position       = "bottom",
    legend.direction      = "vertical",
    legend.justification  = "left",
    legend.text           = element_text(size = 9, color = "#444444", margin = margin(b = 3)),
    legend.key.size       = unit(10, "pt"),
    legend.key            = element_rect(color = NA),
    legend.title          = element_blank(),
    legend.margin         = margin(t = 10),
    plot.title            = element_text(face = "bold", size = 14, color = "#1a1a1a",
                                         margin = margin(b = 4)),
    plot.subtitle         = element_text(size = 10, color = "#666666",
                                         margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "#777777", hjust = 0,
                             margin = margin(t = 20)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(15, 20, 10, 15)
  ) +
  guides(fill = guide_legend(byrow = TRUE))


# =============================================================================
# 2) Kipppunkt – Einnahmen- vs. Ausgabenentwicklung (2010–2026)
# =============================================================================

kipppunkt_raw <- read_excel(raw_path, sheet = "Tab_Ausgaben_Beitragseinnahmen")

kipppunkt <- kipppunkt_raw %>%
  rename(Wert = 1) %>%
  pivot_longer(cols = -Wert, names_to = "Jahr_raw", values_to = "Value") %>%
  filter(Wert %in% c("Beitragseinnahmen ohne Zusatzbeiträge", "Ausgaben GKV")) %>%
  mutate(
    # Numerisches Jahr extrahieren und Prognose-Flag setzen
    Prognose = str_detect(Jahr_raw, "\\*"),
    Jahr     = as.integer(str_remove_all(Jahr_raw, "\\*")),
    Value    = as.numeric(Value),
    # Lesbarere Legenden-Labels
    Wert = case_when(
      Wert == "Beitragseinnahmen ohne Zusatzbeiträge" ~ "Beitragseinnahmen (ohne Zusatzbeiträge)",
      Wert == "Ausgaben GKV"                          ~ "Ausgaben der GKV",
      TRUE ~ Wert
    ),
    Wert = fct_relevel(Wert, "Ausgaben der GKV")
  )

# Aufteilen in Ist- und Prognosedaten für separate Linien-Layer
kipp_actual   <- kipppunkt %>% filter(!Prognose)
# Prognose: letztes Ist-Jahr (2024) einschließen für nahtlosen Übergang
kipp_forecast <- kipppunkt %>% filter(Prognose | Jahr == 2024)

# Lücke zwischen den Linien für Ribbon-Schattierung berechnen
kipp_wide <- kipppunkt %>%
  select(Jahr, Wert, Value) %>%
  pivot_wider(names_from = Wert, values_from = Value) %>%
  rename(Ausgaben = `Ausgaben der GKV`,
         Beitraege = `Beitragseinnahmen (ohne Zusatzbeiträge)`)

# Annotationen: beziehen sich auf Beitragseinnahmen (untere Linie) – knapp darunter platziert
# Einheitliche Offsets relativ zur echten Einnahmenlinie (linear interpoliert)
arrow_gap  <- 5    # Abstand Pfeilspitze <-> Linie (Mrd. €)
label_drop <- 20   # Abstand Label-Oberkante <-> Linie (Mrd. €)

annotations <- tibble(
  Jahr  = c(2011.5, 2015, 2020),
  label = c(
    "Erhöhung allg. Beitragssatz\nvon 14,9 % auf 15,5 %",
    "Absenkung allg. Beitragssatz\nvon 15,5 % auf 14,6 %",
    "Geringere Grundlohn-\nsteigerung (Covid-19)"
  )
) %>%
  mutate(
    # Linearinterpolation auf der Beitragseinnahmen-Linie
    line_y  = approx(x = kipp_wide$Jahr, y = kipp_wide$Beitraege, xout = Jahr)$y,
    y_arrow = line_y - arrow_gap,
    y_label = line_y - label_drop
  )

p_kipppunkt <- ggplot() +
  # Ribbon: Lücke zwischen den Linien
  geom_ribbon(data = kipp_wide,
              aes(x = Jahr, ymin = Beitraege, ymax = Ausgaben),
              fill = "#b5b5b5", alpha = 0.4) +
  # Durchgezogene Linien: Ist-Daten
  geom_line(data = kipp_actual,
            aes(x = Jahr, y = Value, color = Wert),
            linewidth = 1.1) +
  # Gestrichelte Linien: Prognose (Überlappung bei 2024 für nahtlosen Übergang)
  geom_line(data = kipp_forecast,
            aes(x = Jahr, y = Value, color = Wert),
            linewidth = 1.1, linetype = "21") +
  # Direkt-Labels am Linienende
  geom_text(data = kipppunkt %>% filter(Jahr == max(Jahr)),
            aes(x = Jahr + 0.3, y = Value, label = paste0(round(Value, 0)),
                color = Wert),
            hjust = 0, size = 3.2, fontface = "bold", family = font_body,
            show.legend = FALSE) +
  # Annotations-Text (unterhalb der Einnahmenlinie, vjust = 1 damit Text nach oben wächst)
  geom_text(data = annotations,
            aes(x = Jahr, y = y_label, label = label),
            size = 2.6, color = "#666666", lineheight = 0.9,
            family = font_body, vjust = 1) +
  # Annotations-Pfeile (vom Text nach oben zur Einnahmenlinie)
  geom_segment(data = annotations,
               aes(x = Jahr, xend = Jahr,
                   y = y_label + 2,
                   yend = y_arrow),
               color = "#999999", linewidth = 0.3,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  # Skalen
  scale_color_manual(values = c("Ausgaben der GKV" = col_primary,
                                "Beitragseinnahmen (ohne Zusatzbeiträge)" = col_accent)) +
  scale_x_continuous(breaks = seq(2010, 2026, 1),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_y_continuous(limits = c(100, 400),
                     breaks = seq(100, 400, 50),
                     labels = label_comma(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Einnahmen- und Ausgabenentwicklung in der GKV",
    subtitle = "Strukturelle Deckungslücke wächst – 2026 beträgt die Lücke rd. 77 Mrd. €",
    y        = "in Mrd. €",
    caption  = "Gestrichelte Linie = Prognose des Schätzerkreises (10/2025). | Quelle: Eigene Berechnungen."
  ) +
  theme_publication() +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = alpha(col_bg, 0.9), color = NA),
    legend.key.width     = unit(22, "pt"),
    legend.spacing.y     = unit(2, "pt"),
    plot.margin          = margin(20, 25, 15, 15)
  )

p_kipppunkt

# =============================================================================
# 3) Balkendiagramm – Entwicklung des Steuerzuschusses (2004–2026)
# =============================================================================

balken_raw <- read_excel(raw_path, sheet = "Tab_Höhe_Anteil")

balken <- balken_raw %>%
  rename(Wert = 1) %>%
  # Untertitel-Zeile "in Mrd. Euro" überspringen
  filter(!is.na(Wert), Wert != "NA") %>%
  # Alle Jahresspalten vor dem Pivotieren auf numerisch erzwingen (gemischte Typen aus Excel)
  mutate(across(-Wert, as.numeric)) %>%
  pivot_longer(cols = -Wert, names_to = "Jahr_raw", values_to = "Value") %>%
  filter(Wert %in% c("Steuerzuschuss nach § 221 SGB V",
                      "Steuerzuschuss nach § 221a SGB V")) %>%
  mutate(
    Jahr  = as.integer(str_remove_all(Jahr_raw, "\\*")),
    Value = replace_na(as.numeric(Value), 0),
    # Kürzere Legenden-Labels
    Wert  = case_when(
      str_detect(Wert, "221a") ~ "§ 221a SGB V (Sonderzuschüsse)",
      str_detect(Wert, "221")  ~ "§ 221 SGB V (Regelzuschuss)",
      TRUE ~ Wert
    ),
    Wert = fct_relevel(Wert, "§ 221 SGB V (Regelzuschuss)")
  )

# Summe pro Jahr für Labels über den Balken
balken_total <- balken %>%
  group_by(Jahr) %>%
  summarise(Total = sum(Value), .groups = "drop")

p_barplot <- ggplot(balken, aes(x = Jahr, y = Value, fill = Wert)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  # Summen-Label über jedem Balken
  geom_text(data = balken_total,
            aes(x = Jahr, y = Total, fill = NULL,
                label = format(round(Total, 1), nsmall = 1, decimal.mark = ",")),
            vjust = -0.5, size = 2.5, color = "#555555", family = font_body) +
  scale_fill_manual(values = c(col_primary, col_accent)) +
  scale_x_continuous(breaks = 2004:2026,
                     labels = function(x) {
                       case_when(
                         x == 2020 ~ "2020*",
                         x %in% c(2021, 2022, 2023) ~ paste0(x, "**"),
                         TRUE ~ as.character(x)
                       )
                     }) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    title    = "Entwicklung des Bundeszuschusses seit dessen Einführung 2004",
    subtitle = "Aufschlüsselung nach Regelzuschuss (§ 221 SGB V) und Sonderzuschüssen (§ 221a SGB V)",
    y        = "in Mrd. €",
    caption  = paste0(
      "* Zusätzlicher Steuerzuschuss i. H. v. 3,5 Mrd. € abweichend nach § 12a Haushaltsgesetz 2020\n",
      "** Einschl. 0,3 Mrd. € (2021/2022) bzw. 0,15 Mrd. € (2023) an die Liquiditätsreserve des GF für Kinderkrankengeld\n",
      "Quelle: Eigene Darstellung auf Basis der verschiedenen Fassungen der angegebenen Paragrafen."
    )
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)),
    legend.position = "top",
    legend.justification = "left"
  )


# =============================================================================
# Graustufen-Varianten
# =============================================================================

col_bw_dark  <- "#2d2d2d"   # Kontrast 13.8 ✓
col_bw_mid   <- "#636363"   # Kontrast  5.9 ✓ (war #7a7a7a → 4.3 ✗)
col_bw_light <- "#c8c8c8"
col_bw_pale  <- "#a0a0a0"
bw_vals      <- c(col_bw_dark, col_bw_mid, col_bw_light, col_bw_pale)

p_einnahmen_bw        <- p_einnahmen + scale_fill_manual(values = bw_vals)
p_donut_bw            <- p_donut + scale_fill_manual(values = bw_vals)
p_einnahmen_legend_bw <- p_einnahmen_legend + scale_fill_manual(values = bw_vals, labels = legend_labels_clean)
p_kipppunkt_bw <- p_kipppunkt + 
  scale_color_manual(values = c("Ausgaben der GKV" = "#1a1a1a",
                                "Beitragseinnahmen (ohne Zusatzbeiträge)" = "#888888"))
p_barplot_bw          <- p_barplot + scale_fill_manual(values = c(col_bw_dark, col_bw_light))


# =============================================================================
# Export – alle Grafiken gebündelt
# =============================================================================

# 1) Einnahmen GF 2024 – Varianten
ggsave(paste0(out_path, "einnahmen_anteile_2024.png"), p_einnahmen,
       width = 26, height = 8, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "einnahmen_donut_2024.png"), p_donut,
       width = 16, height = 16, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "einnahmen_balken_legende_2024.png"), p_einnahmen_legend,
       width = 24, height = 10, units = "cm", dpi = 600, bg = col_bg)

# 2) Kipppunkt
ggsave(paste0(out_path, "kipppunkt_2010_2026.png"), p_kipppunkt,
       width = 24, height = 16, units = "cm", dpi = 600, bg = col_bg)

# 3) Balkendiagramm Steuerzuschuss
ggsave(paste0(out_path, "steuerzuschuss_2004_2026.png"), p_barplot,
       width = 26, height = 14, units = "cm", dpi = 600, bg = col_bg)

# Graustufen
ggsave(paste0(out_path, "einnahmen_anteile_2024_bw.png"), p_einnahmen_bw,
       width = 26, height = 8, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "einnahmen_donut_2024_bw.png"), p_donut_bw,
       width = 16, height = 16, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "einnahmen_balken_legende_2024_bw.png"), p_einnahmen_legend_bw,
       width = 24, height = 10, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "kipppunkt_2010_2026_bw.png"), p_kipppunkt_bw,
       width = 24, height = 16, units = "cm", dpi = 600, bg = col_bg)
ggsave(paste0(out_path, "steuerzuschuss_2004_2026_bw.png"), p_barplot_bw,
       width = 26, height = 14, units = "cm", dpi = 600, bg = col_bg)


# =============================================================================
# Export – TIFF-Varianten (LZW-komprimiert, druckfertig)
# =============================================================================

# Helper: einheitlicher TIFF-Export mit verlustfreier LZW-Kompression.
# ragg::agg_tiff sorgt fuer sauberes Antialiasing und Schriftrendering.
save_tiff <- function(file, plot, width, height) {
  ggsave(paste0(out_path, file), plot,
         width = width, height = height, units = "cm", dpi = 600, bg = col_bg,
         device = ragg::agg_tiff, compression = "lzw")
}

# 1) Einnahmen GF 2024 – Varianten
save_tiff("einnahmen_anteile_2024.tiff",         p_einnahmen,        26,  8)
save_tiff("einnahmen_donut_2024.tiff",           p_donut,            16, 16)
save_tiff("einnahmen_balken_legende_2024.tiff",  p_einnahmen_legend, 24, 10)

# 2) Kipppunkt
save_tiff("kipppunkt_2010_2026.tiff",            p_kipppunkt,        24, 16)

# 3) Balkendiagramm Steuerzuschuss
save_tiff("steuerzuschuss_2004_2026.tiff",       p_barplot,          26, 14)

# Graustufen
save_tiff("einnahmen_anteile_2024_bw.tiff",        p_einnahmen_bw,        26,  8)
save_tiff("einnahmen_donut_2024_bw.tiff",          p_donut_bw,            16, 16)
save_tiff("einnahmen_balken_legende_2024_bw.tiff", p_einnahmen_legend_bw, 24, 10)
save_tiff("kipppunkt_2010_2026_bw.tiff",           p_kipppunkt_bw,        24, 16)
save_tiff("steuerzuschuss_2004_2026_bw.tiff",      p_barplot_bw,          26, 14)
