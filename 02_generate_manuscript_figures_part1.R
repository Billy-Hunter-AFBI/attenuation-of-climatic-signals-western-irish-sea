# ============================================================
# 02_generate_figures.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Generate manuscript and supplementary figures from the
# derived ecosystem metrics dataset.
#
# Outputs:
#   Figure1_main_time_series.pdf
#   Supplementary_Figure1_time_series.pdf
#
# ============================================================


# ============================================================
# Package requirements
# ============================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "patchwork",
  "scales",
  "readr"
)

missing_packages <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if(length(missing_packages) > 0){

  stop(
    paste(
      "Missing packages:",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )

}

# ============================================================
# Load libraries
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)
library(readr)

# ============================================================
# Global plotting theme
# ============================================================

theme_set(
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
)

# ============================================================
# Project directories
# ============================================================

data_dir <- "data"
fig_dir  <- "figures"

dir.create(
  fig_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

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
# Prepare Figure 1 dataset
# ============================================================

figure1_data <- df %>%
  filter(
    nutrient == "NO3",
    !is.na(bloom_intensity)
  )

# ============================================================
# Figure 1
# Interannual variability in climate, nutrient dynamics,
# and phytoplankton bloom intensity
# ============================================================

message("Generating Figure 1")

# ============================================================
# Panel A
# Physical climate context
# ============================================================

p_climate <- ggplot(
  figure1_data,
  aes(x = year)
) +
  geom_line(
    aes(y = temp_mean),
    linewidth = 0.5,
    colour = "black"
  ) +
  geom_col(
    aes(
      y = hw_int_cum /
        max(hw_int_cum, na.rm = TRUE) *
        max(temp_mean, na.rm = TRUE)
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  labs(
    y = "Mean SST (°C)",
    title = "Physical climate context"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6)
  )

# ============================================================
# Panel B
# Nitrate availability and seasonal drawdown
# ============================================================

p_nutrient <- ggplot(
  figure1_data,
  aes(x = year)
) +
  geom_point(
    aes(y = winter_mean_nut),
    size = 1.5
  ) +
  geom_line(
    aes(y = winter_mean_nut),
    linewidth = 0.4
  ) +
  geom_point(
    aes(y = drawdown_mag),
    colour = "red",
    size = 1.5
  ) +
  geom_line(
    aes(y = drawdown_mag),
    colour = "red",
    linewidth = 0.4
  ) +
  scale_y_continuous(
    name = expression(
      paste(
        "Winter NO"[3],
        " (µmol L"^-1, ")"
      )
    ),
    sec.axis = sec_axis(
      ~ .,
      name = expression(
        paste(
          "NO"[3],
          " drawdown magnitude"
        )
      )
    )
  ) +
  labs(
    title = "Nitrate availability and drawdown"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6)
  )

# ============================================================
# Panel C
# Spring bloom intensity
# ============================================================

p_bloom <- ggplot(
  figure1_data,
  aes(x = year)
) +
  geom_point(
    aes(y = bloom_intensity),
    size = 1.8
  ) +
  geom_line(
    aes(y = bloom_intensity),
    linewidth = 0.4
  ) +
  labs(
    y = expression(
      paste(
        "Bloom intensity (ΔChl-a)"
      )
    ),
    x = "Year",
    title = "Bloom intensity"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6)
  )

# ============================================================
# Assemble Figure 1
# ============================================================

fig1 <- (
  p_climate /
  p_nutrient /
  p_bloom
) +
  plot_layout(
    heights = c(
      1,
      1.1,
      1
    )
  ) +
  plot_annotation(
    tag_levels = "a"
  )

# ============================================================
# Export Figure 1
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Figure1_main_time_series.pdf"
  ),
  plot = fig1,
  width = 180,
  height = 180,
  units = "mm",
  device = cairo_pdf
)

message("Figure 1 exported")

# ============================================================
# Supplementary Figure 1
# Interannual variability in nutrient dynamics and
# climate forcing
# ============================================================

message("Generating Supplementary Figure 1")

# ============================================================
# Panel A
# Nutrient availability and seasonal drawdown
# ============================================================

supp_nutrient_data <- df %>%
  filter(
    !is.na(drawdown_mag)
  ) %>%
  select(
    year,
    nutrient,
    winter_mean_nut,
    drawdown_mag
  ) %>%
  pivot_longer(
    cols = c(
      winter_mean_nut,
      drawdown_mag
    ),
    names_to = "metric",
    values_to = "value"
  )

p_sup_nutrients <- ggplot(
  supp_nutrient_data,
  aes(
    x = year,
    y = value
  )
) +
  geom_line(
    linewidth = 0.4
  ) +
  geom_point(
    size = 1
  ) +
  facet_grid(
    metric ~ nutrient,
    scales = "free_y"
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Nutrient availability and drawdown"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6)
  )

# ============================================================
# Panel B
# Physical climate variability
# ============================================================

supp_climate_data <- figure1_data %>%
  select(
    year,
    temp_mean,
    hw_int_cum,
    cs_int_cum,
    nao_DJFM
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

p_sup_climate <- ggplot(
  supp_climate_data,
  aes(
    x = year,
    y = value
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Climate variability"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6)
  )

# ============================================================
# Assemble Supplementary Figure 1
# ============================================================

figS1 <- (
  p_sup_nutrients /
  p_sup_climate
) +
  plot_annotation(
    tag_levels = "a"
  )

# ============================================================
# Export Supplementary Figure 1
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Supplementary_Figure1_time_series.pdf"
  ),
  plot = figS1,
  width = 180,
  height = 240,
  units = "mm",
  device = cairo_pdf
)

message("Supplementary Figure 1 exported")