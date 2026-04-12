# =============================================================================
# High-End Visualisierungen – Steuerzuschuss Buchpublikation (Kuhn/Illgen)
# Minimalist, publication-ready design
# Author: Sebastian Kuhn | 2026-04-12
# =============================================================================

library(tidyverse)
library(readxl)
library(scales)
library(ggtext)
library(systemfonts)

# --- Config -------------------------------------------------------------------

raw_path <- "00_raw_data/260129_Aktualisierung_Grafiken_Steuerzuschuss.xlsx"
out_path <- "06_graphs/"

# Color palette: muted, high-contrast, print-safe
col_primary   <- "#1B2A4A"
col_secondary <- "#3D6098"
col_accent    <- "#7EA1C4"
col_accent2   <- "#B8CDE0"
col_highlight <- "#C44E52"
col_grey      <- "#8C8C8C"
col_lightgrey <- "#E8E8E8"
col_bg        <- "#FFFFFF"

# Typography: use system fonts with fallback
font_title <- "Helvetica Neue"
font_body  <- "Helvetica Neue"

# Minimal base theme for all charts
theme_publication <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = font_body) +
    theme(
      # Background
      plot.background    = element_rect(fill = col_bg, color = NA),
      panel.background   = element_rect(fill = col_bg, color = NA),
      # Grid
      panel.grid.major.y = element_line(color = col_lightgrey, linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      # Axes
      axis.line.x        = element_line(color = "#333333", linewidth = 0.4),
      axis.ticks.x       = element_line(color = "#333333", linewidth = 0.3),
      axis.ticks.length  = unit(3, "pt"),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = rel(0.85), color = "#555555",
                                        hjust = 1, margin = margin(r = 8)),
      axis.text          = element_text(color = "#444444"),
      # Legend
      legend.position     = "top",
      legend.justification = "left",
      legend.title        = element_blank(),
      legend.key.size     = unit(12, "pt"),
      legend.text         = element_text(size = rel(0.8), color = "#444444"),
      legend.margin       = margin(b = -5),
      # Title / Caption
      plot.title          = element_text(face = "bold", size = rel(1.25),
                                         color = "#1a1a1a", margin = margin(b = 4)),
      plot.subtitle       = element_text(size = rel(0.9), color = "#666666",
                                         margin = margin(b = 16)),
      plot.caption        = element_text(size = rel(0.7), color = "#999999",
                                         hjust = 0, margin = margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin         = margin(20, 20, 15, 15)
    )
}


# =============================================================================
# 1) Donut Chart – Einnahmen des Gesundheitsfonds 2024
# =============================================================================

data_einnahmen <- tibble(
  Posten     = c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere\nEinnahmen"),
  Absolut    = c(266.50, 30.50, 14.49, 0.98),
  Prozentual = c(0.85287, 0.09760, 0.04639, 0.00314)
) %>%
  mutate(
    Posten = fct_inorder(Posten),
    # Label inside bar: percentage
    pct_label = paste0(format(round(Prozentual * 100, 1), nsmall = 1, decimal.mark = ","), " %"),
    # Label below bar: absolute value
    abs_label = paste0(format(round(Absolut, 1), nsmall = 1, decimal.mark = ","), " Mrd. €"),
    # Cumulative positions for label placement
    xmax = cumsum(Prozentual),
    xmin = lag(xmax, default = 0),
    xmid = (xmin + xmax) / 2
  )

einnahmen_colors <- c(col_primary, col_secondary, col_accent, col_accent2)

# Stagger label positions for small segments to avoid overlap
data_einnahmen <- data_einnahmen %>%
  mutate(
    # Category labels: stagger vertically for the 3 small segments
    y_cat = case_when(
      Posten == "Beiträge"                        ~ 1.12,
      str_detect(Posten, "Zusatz")                ~ 1.22,
      str_detect(Posten, "Steuer")                ~ 1.12,
      TRUE                                        ~ 1.22  # Weitere Einnahmen
    ),
    # Absolute labels below: same stagger
    y_abs = case_when(
      Posten == "Beiträge"                        ~ 0.18,
      str_detect(Posten, "Zusatz")                ~ 0.08,
      str_detect(Posten, "Steuer")                ~ 0.18,
      TRUE                                        ~ 0.08
    )
  )

p_einnahmen <- ggplot(data_einnahmen) +
  # Horizontal stacked bar
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 1, fill = Posten),
            color = col_bg, linewidth = 0.8) +
  # Percentage labels inside bars (only Beiträge fits comfortably)
  geom_text(data = data_einnahmen %>% filter(Prozentual > 0.08),
            aes(x = xmid, y = 0.65, label = pct_label),
            color = "white", fontface = "bold", size = 4.5, family = font_body) +
  # Category labels above bar (staggered)
  geom_text(aes(x = xmid, y = y_cat, label = Posten),
            color = "#333333", size = 3, family = font_body, lineheight = 0.85) +
  # Thin connector lines from label to segment
  geom_segment(data = data_einnahmen %>% filter(Prozentual < 0.5),
               aes(x = xmid, xend = xmid, y = 1.02, yend = y_cat - 0.03),
               color = "#cccccc", linewidth = 0.25) +
  # Absolute values below bar (staggered)
  geom_text(aes(x = xmid, y = y_abs, label = abs_label),
            color = "#888888", size = 2.8, family = font_body) +
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
    plot.caption          = element_text(size = 8, color = "#999999", hjust = 0,
                                         margin = margin(t = 20)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(20, 20, 15, 15)
  )

ggsave(paste0(out_path, "einnahmen_anteile_2024.png"), p_einnahmen,
       width = 26, height = 8, units = "cm", dpi = 600, bg = col_bg)

# --- 1b) Donut-Variante mit separater Legende ---

data_donut <- tibble(
  Posten     = c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere Einnahmen"),
  Absolut    = c(266.50, 30.50, 14.49, 0.98),
  Prozentual = c(0.85287, 0.09760, 0.04639, 0.00314)
) %>%
  mutate(
    Posten = fct_inorder(Posten),
    ymax   = cumsum(Prozentual),
    ymin   = lag(ymax, default = 0),
    ymid   = (ymin + ymax) / 2,
    # Legend-style label: "Beiträge – 85,3 % (266,5 Mrd. €)"
    legend_label = paste0(
      Posten, " \u2013 ",
      format(round(Prozentual * 100, 1), nsmall = 1, decimal.mark = ","), " % (",
      format(round(Absolut, 1), nsmall = 1, decimal.mark = ","), " Mrd. \u20AC)"
    )
  )

p_donut <- ggplot(data_donut, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.6, fill = Posten)) +
  geom_rect(color = col_bg, linewidth = 1.2) +
  coord_polar(theta = "y") +
  xlim(c(1, 4.5)) +
  scale_fill_manual(
    values = einnahmen_colors,
    labels = data_donut$legend_label
  ) +
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
    legend.spacing.y      = unit(2, "pt"),
    legend.title          = element_blank(),
    legend.margin         = margin(t = 5),
    plot.title            = element_text(face = "bold", size = 14, color = "#1a1a1a",
                                         margin = margin(b = 4)),
    plot.subtitle         = element_text(size = 10, color = "#666666",
                                         margin = margin(b = 5)),
    plot.caption          = element_text(size = 8, color = "#999999", hjust = 0,
                                         margin = margin(t = 12)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(15, 15, 10, 15)
  ) +
  guides(fill = guide_legend(byrow = TRUE))

ggsave(paste0(out_path, "einnahmen_donut_2024.png"), p_donut,
       width = 18, height = 18, units = "cm", dpi = 600, bg = col_bg)

# --- 1c) Horizontaler Balken mit Legende unterhalb ---

# Build legend labels without linebreaks
legend_labels_clean <- paste0(
  c("Beiträge", "Zusatzbeiträge", "Steuerzuschuss", "Weitere Einnahmen"),
  " \u2013 ",
  data_einnahmen$pct_label, " (",
  data_einnahmen$abs_label, ")"
) %>% str_replace_all("\n", " ")

p_einnahmen_legend <- ggplot(data_einnahmen) +
  # Same geom_rect approach as direct-label variant (no geom_col issues)
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 1, fill = Posten),
            color = col_bg, linewidth = 0.8) +
  # Percentage labels inside bars (only Beiträge – rest too narrow)
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
    plot.caption          = element_text(size = 8, color = "#999999", hjust = 0,
                                         margin = margin(t = 12)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(15, 20, 10, 15)
  ) +
  guides(fill = guide_legend(byrow = TRUE))

ggsave(paste0(out_path, "einnahmen_balken_legende_2024.png"), p_einnahmen_legend,
       width = 24, height = 10, units = "cm", dpi = 600, bg = col_bg)


# =============================================================================
# 2) Kipppunkt – Einnahmen- vs. Ausgabenentwicklung (2010–2026)
# =============================================================================

kipppunkt_raw <- read_excel(raw_path, sheet = "Tab_Ausgaben_Beitragseinnahmen")

kipppunkt <- kipppunkt_raw %>%
  rename(Wert = 1) %>%
  pivot_longer(cols = -Wert, names_to = "Jahr_raw", values_to = "Value") %>%
  filter(Wert %in% c("Beitragseinnahmen ohne Zusatzbeiträge", "Ausgaben GKV")) %>%
  mutate(
    # Extract numeric year and flag forecast
    Prognose = str_detect(Jahr_raw, "\\*"),
    Jahr     = as.integer(str_remove_all(Jahr_raw, "\\*")),
    Value    = as.numeric(Value),
    # Cleaner legend labels
    Wert = case_when(
      Wert == "Beitragseinnahmen ohne Zusatzbeiträge" ~ "Beitragseinnahmen (ohne Zusatzbeiträge)",
      Wert == "Ausgaben GKV"                          ~ "Ausgaben der GKV",
      TRUE ~ Wert
    ),
    Wert = fct_relevel(Wert, "Ausgaben der GKV")
  )

# Split into actual and forecast data for separate line layers
kipp_actual   <- kipppunkt %>% filter(!Prognose)
# Forecast: include last actual year (2024) to connect lines seamlessly
kipp_forecast <- kipppunkt %>% filter(Prognose | Jahr == 2024)

# Calculate the gap for ribbon shading
kipp_wide <- kipppunkt %>%
  select(Jahr, Wert, Value) %>%
  pivot_wider(names_from = Wert, values_from = Value) %>%
  rename(Ausgaben = `Ausgaben der GKV`,
         Beitraege = `Beitragseinnahmen (ohne Zusatzbeiträge)`)

# Annotations: key policy events – positioned above both lines, arrows point down
annotations <- tibble(
  Jahr    = c(2011.5, 2015, 2020),
  y_label = c(192, 222, 272),
  y_arrow = c(176, 210, 258),
  label   = c(
    "Erhöhung allg. Beitragssatz\nvon 14,9 % auf 15,5 %",
    "Absenkung allg. Beitragssatz\nvon 15,5 % auf 14,6 %",
    "Geringere Grundlohn-\nsteigerung (Covid-19)"
  )
)

p_kipppunkt <- ggplot() +
  # Ribbon: gap between lines
  geom_ribbon(data = kipp_wide,
              aes(x = Jahr, ymin = Beitraege, ymax = Ausgaben),
              fill = col_highlight, alpha = 0.08) +
  # Solid lines: actual data
  geom_line(data = kipp_actual,
            aes(x = Jahr, y = Value, color = Wert),
            linewidth = 1.1) +
  # Dashed lines: forecast (overlapping at 2024 for seamless connection)
  geom_line(data = kipp_forecast,
            aes(x = Jahr, y = Value, color = Wert),
            linewidth = 1.1, linetype = "21") +
  # End-of-line direct labels
  geom_text(data = kipppunkt %>% filter(Jahr == max(Jahr)),
            aes(x = Jahr + 0.3, y = Value, label = paste0(round(Value, 0), " Mrd."),
                color = Wert),
            hjust = 0, size = 3.2, fontface = "bold", family = font_body,
            show.legend = FALSE) +
  # Annotation text (above both lines, vjust = 0 so text grows upward)
  geom_text(data = annotations,
            aes(x = Jahr, y = y_label, label = label),
            size = 2.6, color = "#666666", lineheight = 0.9,
            family = font_body, vjust = 0) +
  # Annotation arrows (from label downward to the line)
  geom_segment(data = annotations,
               aes(x = Jahr, xend = Jahr,
                   y = y_label - 2,
                   yend = y_arrow),
               color = "#999999", linewidth = 0.3,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  # Scales
  scale_color_manual(values = c("Ausgaben der GKV" = col_primary,
                                "Beitragseinnahmen (ohne Zusatzbeiträge)" = col_secondary)) +
  scale_x_continuous(breaks = seq(2010, 2026, 2),
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(limits = c(150, NA),
                     labels = label_comma(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Einnahmen- und Ausgabenentwicklung in der GKV",
    subtitle = "Strukturelle Deckungslücke wächst – 2026 beträgt die Differenz rd. 77 Mrd. €",
    y        = "in Mrd. Euro",
    caption  = "Gestrichelte Linie = Prognose des Schätzerkreises. | Quelle: Eigene Berechnungen."
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

ggsave(paste0(out_path, "kipppunkt_2010_2026.png"), p_kipppunkt,
       width = 24, height = 16, units = "cm", dpi = 600, bg = col_bg)


# =============================================================================
# 3) Balkendiagramm – Entwicklung des Steuerzuschusses (2004–2026)
# =============================================================================

balken_raw <- read_excel(raw_path, sheet = "Tab_Höhe_Anteil")

balken <- balken_raw %>%
  rename(Wert = 1) %>%
  # Skip the "in Mrd. Euro" subtitle row
  filter(!is.na(Wert), Wert != "NA") %>%
  # Force all year columns to numeric before pivoting (mixed types from Excel)
  mutate(across(-Wert, as.numeric)) %>%
  pivot_longer(cols = -Wert, names_to = "Jahr_raw", values_to = "Value") %>%
  filter(Wert %in% c("Steuerzuschuss nach § 221 SGB V",
                      "Steuerzuschuss nach § 221a SGB V")) %>%
  mutate(
    Jahr  = as.integer(str_remove_all(Jahr_raw, "\\*")),
    Value = replace_na(as.numeric(Value), 0),
    # Shorter legend labels
    Wert  = case_when(
      str_detect(Wert, "221a") ~ "§ 221a SGB V (Sonderzuschüsse)",
      str_detect(Wert, "221")  ~ "§ 221 SGB V (Regelzuschuss)",
      TRUE ~ Wert
    ),
    Wert = fct_relevel(Wert, "§ 221 SGB V (Regelzuschuss)")
  )

# Total per year for top labels
balken_total <- balken %>%
  group_by(Jahr) %>%
  summarise(Total = sum(Value), .groups = "drop")

p_barplot <- ggplot(balken, aes(x = Jahr, y = Value, fill = Wert)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  # Total label on top of each bar
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
    subtitle = "Aufschlüsselung nach Regelzuschuss (§ 221) und Sonderzuschüssen (§ 221a SGB V)",
    y        = "in Mrd. Euro",
    caption  = paste0(
      "* Zusätzlicher Steuerzuschuss i. H. v. 3,5 Mrd. € abweichend nach § 12a Haushaltsgesetz 2020\n",
      "** Einschl. 0,3 Mrd. € (2021/2022) bzw. 0,15 Mrd. € (2023) an die Liquiditätsreserve des GF für Kinderkrankengeld\n",
      "Quelle: Eigene Berechnungen."
    )
  ) +
  theme_publication() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)),
    legend.position = "top",
    legend.justification = "left"
  )

ggsave(paste0(out_path, "steuerzuschuss_2004_2026.png"), p_barplot,
       width = 26, height = 14, units = "cm", dpi = 600, bg = col_bg)


# =============================================================================
# Bonus: Grayscale variants for print
# =============================================================================

col_bw_dark  <- "#2d2d2d"
col_bw_mid   <- "#7a7a7a"
col_bw_light <- "#b5b5b5"
col_bw_pale  <- "#d9d9d9"

# Einnahmen BW (alle drei Varianten)
bw_vals <- c(col_bw_dark, col_bw_mid, col_bw_light, col_bw_pale)

p_einnahmen_bw <- p_einnahmen +
  scale_fill_manual(values = bw_vals)
ggsave(paste0(out_path, "einnahmen_anteile_2024_bw.png"), p_einnahmen_bw,
       width = 26, height = 8, units = "cm", dpi = 600, bg = col_bg)

p_donut_bw <- p_donut +
  scale_fill_manual(values = bw_vals, labels = data_donut$legend_label)
ggsave(paste0(out_path, "einnahmen_donut_2024_bw.png"), p_donut_bw,
       width = 18, height = 18, units = "cm", dpi = 600, bg = col_bg)

p_einnahmen_legend_bw <- p_einnahmen_legend +
  scale_fill_manual(values = bw_vals, labels = legend_labels_clean)
ggsave(paste0(out_path, "einnahmen_balken_legende_2024_bw.png"), p_einnahmen_legend_bw,
       width = 24, height = 10, units = "cm", dpi = 600, bg = col_bg)

# Kipppunkt BW
p_kipppunkt_bw <- p_kipppunkt +
  scale_color_manual(values = c("Ausgaben der GKV" = col_bw_dark,
                                "Beitragseinnahmen (ohne Zusatzbeiträge)" = col_bw_mid))
ggsave(paste0(out_path, "kipppunkt_2010_2026_bw.png"), p_kipppunkt_bw,
       width = 24, height = 16, units = "cm", dpi = 600, bg = col_bg)

# Barplot BW
p_barplot_bw <- p_barplot +
  scale_fill_manual(values = c(col_bw_dark, col_bw_light))
ggsave(paste0(out_path, "steuerzuschuss_2004_2026_bw.png"), p_barplot_bw,
       width = 26, height = 14, units = "cm", dpi = 600, bg = col_bg)
