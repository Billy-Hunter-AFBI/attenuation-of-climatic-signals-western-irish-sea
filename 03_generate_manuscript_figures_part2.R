# ============================================================
# 03_generate_drawdown_figures.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Generate Figure 2 and Supplementary Figure 2 examining
# climatic and nutrient controls on seasonal nutrient
# drawdown dynamics.
#
# Outputs:
#   Figure2_drawdown_controls.pdf
#   Supplementary_Figure2_drawdown_diagnostics.pdf
#
# ============================================================

# ============================================================
# Package requirements
# ============================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "patchwork",
  "readr",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if(length(missing_packages) > 0){

  stop(
    paste(
      "Missing packages:",
      paste(missing_packages, collapse = ", ")
    )
  )

}

# ----------------------------
# 0. Libraries & theme
# ----------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(readr)
library(scales)

theme_set(
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
)

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

# ============================================================
# Read analysis dataset
# ============================================================

df <- read_csv(
  file.path(
    data_dir,
    "western_irish_sea_ecosystem_metrics.csv"
  ),
  show_col_types = FALSE
)

# ============================================================
# Prepare Figure 2 dataset
# ============================================================

figure2_data <- df %>%
  filter(
    nutrient %in% c(
      "NO3",
      "NH4",
      "SRP",
      "SIO2"
    )
  )

# ============================================================
# Figure 2
# Climatic controls on drawdown timing and nutrient-supply
# controls on drawdown magnitude
# ============================================================

# ------------------------------------------------------------
# Panel A — Climate control of drawdown timing
# Hypothesis: climate regulates WHEN drawdown occurs
# ------------------------------------------------------------

p_timing <- ggplot(
  >figure2_data,
  aes(x = temp_mean, y = drawdown_doy)
) +
  geom_point(size = 1.6, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.6,
    colour = "black"
  ) +
  facet_wrap(~ nutrient, scales = "free_x") +
  labs(
    x = "Mean sea surface temperature (°C)",
    y = "Drawdown timing (DOY)",
    title = "a  Climate control of nutrient drawdown timing"
  )

# ------------------------------------------------------------
# Panel B — Supply control of drawdown magnitude
# Hypothesis: winter nutrient pool controls HOW MUCH is consumed
# ------------------------------------------------------------

p_magnitude <- ggplot(
  >figure2_data,
  aes(x = winter_mean_nut, y = drawdown_mag)
) +
  geom_point(size = 1.6, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.6,
    colour = "black"
  ) +
  facet_wrap(~ nutrient, scales = "free_x") +
  labs(
    x = "Winter nutrient concentration",
    y = "Drawdown magnitude",
    title = "b  Nutrient‑supply control of drawdown magnitude"
  )

# ============================================================
# Assemble Figure 2
# ============================================================

fig2 <- (
  p_timing /
  p_magnitude
) +
  plot_layout(
    heights = c(
      1,
      1
    )
  ) +
  plot_annotation(
    tag_levels = "a"
  )

# ============================================================
# Export Figure 2
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Figure2_drawdown_controls.pdf"
  ),
  plot = fig2,
  width = 180,
  height = 160,
  units = "mm",
  device = cairo_pdf
)

message("Figure 2 exported")

# ============================================================
# SUPPLEMENTARY FIGURE 2 | Additional analyses of nutrient drawdown dynamics.
# ============================================================

# ============================================================
# Panel A
# Climate controls on nutrient drawdown timing
# ============================================================

df_timing_long <- figure2_data %>%
  select(
    year,
    nutrient,
    drawdown_doy,
    temp_mean,
    hw_int_cum,
    nao_DJFM
  ) %>%
  pivot_longer(
    cols = c(
      temp_mean,
      hw_int_cum,
      nao_DJFM
    ),
    names_to = "climate_var",
    values_to = "value"
  ) %>%
  mutate(
    climate_var = recode(
      climate_var,
      temp_mean  = "Mean SST (°C)",
      hw_int_cum = "Heatwave intensity",
      nao_DJFM   = "Winter NAO (DJFM)"
    ),
    facet_label = paste(
      nutrient,
      climate_var,
      sep = " | "
    )
  )

p_sup_timing <- ggplot(
  df_timing_long,
  aes(
    x = value,
    y = drawdown_doy
  )
) +
  geom_point(
    size = 1.3,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.5,
    colour = "black"
  ) +
  facet_wrap(
    ~ facet_label,
    scales = "free_x",
    ncol = 4
  ) +
  labs(
    x = "Climate variable",
    y = "Drawdown timing (DOY)",
    title = "Climate controls on nutrient drawdown timing"
  )

# ============================================================
# Panel B
# Consequences of drawdown magnitude
# ============================================================

supp_mag_data <- figure2_data %>%
  filter(
    !is.na(drawdown_mag),
    !is.na(spring_min_nut)
  )

p_sup_mag <- ggplot(
  supp_mag_data,
  aes(
    x = drawdown_mag,
    y = spring_min_nut
  )
) +
  geom_point(
    size = 1.6,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.6,
    colour = "black"
  ) +
  facet_wrap(
    ~ nutrient,
    scales = "free",
    ncol = 2
  ) +
  labs(
    x = "Drawdown magnitude",
    y = "Spring minimum nutrient concentration",
    title = "Consequences of drawdown magnitude"
  )

# ============================================================
# Assemble Supplementary Figure 2
# ============================================================

figS2 <- (
  p_sup_timing /
  p_sup_mag
) +
  plot_layout(
    heights = c(
      1.2,
      1
    )
  ) +
  plot_annotation(
    tag_levels = "a"
  )

# ============================================================
# Export Supplementary Figure 2
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Supplementary_Figure2_drawdown_diagnostics.pdf"
  ),
  plot = figS2,
  width = 180,
  height = 220,
  units = "mm",
  device = cairo_pdf
)

message("Supplementary Figure 2 exported")

# ============================================================
# END OF WORKFLOW
# ============================================================
``