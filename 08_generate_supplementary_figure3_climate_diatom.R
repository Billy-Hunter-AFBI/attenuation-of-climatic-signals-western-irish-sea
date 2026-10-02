# =============================================================================
# Supplementary Figure: Climate Associations with Maximum Diatom Abundance
#
# Author: William Ross Hunter
#
# Description:
# Examines relationships between annual maximum diatom abundance and:
#   1. Mean sea-surface temperature (SST)
#   2. Cumulative marine heatwave intensity
#   3. Winter North Atlantic Oscillation (NAO; DJFM)
#
# Input:
#   analysis_all_sst_plankton_nao.csv
#
# Outputs:
#   figures/Supplementary_Figure_Climate_Diatom_Associations.pdf
#   figures/Supplementary_Figure_Climate_Diatom_Associations.tiff
# =============================================================================

# -----------------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# -----------------------------------------------------------------------------
# Global plotting theme
# -----------------------------------------------------------------------------

theme_set(
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "grey90",
        linewidth = 0.3
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
)

# -----------------------------------------------------------------------------
# Output directory
# -----------------------------------------------------------------------------

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Import data
# -----------------------------------------------------------------------------

df <- read_csv(
  "analysis_all_sst_plankton_nao.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Create annual climate–diatom dataset
#
# Nutrient records are duplicated within years. NO3 is used as the reference
# row to extract a single annual observation for climate metrics and diatom
# abundance.
# -----------------------------------------------------------------------------

df_year <- df %>%
  filter(nutrient == "NO3") %>%
  select(
    year,
    temp_mean,
    hw_int_cum,
    nao_DJFM,
    Diatom_max
  )

# -----------------------------------------------------------------------------
# Convert climate variables to long format for faceting
# -----------------------------------------------------------------------------

df_diatom_long <- df_year %>%
  pivot_longer(
    cols = c(
      temp_mean,
      hw_int_cum,
      nao_DJFM
    ),
    names_to = "climate_var",
    values_to = "climate_value"
  ) %>%
  mutate(
    climate_var = recode(
      climate_var,
      temp_mean  = "Mean SST",
      hw_int_cum = "Heatwave intensity",
      nao_DJFM   = "Winter NAO (DJFM)"
    )
  )

# -----------------------------------------------------------------------------
# Panel labels
# -----------------------------------------------------------------------------

panel_labels <- tibble::tibble(
  climate_var = c(
    "Heatwave intensity",
    "Mean SST",
    "Winter NAO (DJFM)"
  ),
  label = c("a", "b", "c")
)

# -----------------------------------------------------------------------------
# Plot: climate drivers of maximum diatom abundance
# -----------------------------------------------------------------------------

p_climate_diatom <- ggplot(
  df_diatom_long,
  aes(
    x = climate_value,
    y = Diatom_max
  )
) +
  geom_point(
    size = 1.6,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.2,
    vjust = 1.3,
    fontface = "bold",
    size = 3
  ) +
  facet_wrap(
    ~climate_var,
    scales = "free_x",
    ncol = 3
  ) +
  labs(
    x = "Climate variable",
    y = "Maximum diatom abundance"
  )

# -----------------------------------------------------------------------------
# Export figure
# -----------------------------------------------------------------------------

ggsave(
  filename = file.path(
    fig_dir,
    "Supplementary_Figure_Climate_Diatom_Associations.pdf"
  ),
  plot = p_climate_diatom,
  width = 180,
  height = 70,
  units = "mm",
  device = cairo_pdf
)

ggsave(
  filename = file.path(
    fig_dir,
    "Supplementary_Figure_Climate_Diatom_Associations.tiff"
  ),
  plot = p_climate_diatom,
  width = 180,
  height = 70,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

# =============================================================================
# End of script
# =============================================================================