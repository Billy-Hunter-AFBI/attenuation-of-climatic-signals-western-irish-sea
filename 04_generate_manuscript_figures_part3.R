# ============================================================
# 04_generate_bloom_response_figures.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Generate Figure 3 and Supplementary Figure 4 examining
# relationships between seasonal nutrient drawdown and
# phytoplankton bloom intensity.
#
# Figure 3 tests whether variability in bloom intensity is
# related to nutrient drawdown magnitude, with observations
# coloured by maximum diatom biomass.
#
# Supplementary Figure 4 provides equivalent analyses for
# nitrate (NO3) and phosphate (SRP) drawdown to facilitate
# comparison with the silicate-focused results shown in
# Figure 3.
#
# Outputs:
#   Figure3_Nutrient_Drawdown_Diatom_Biomass.pdf
#   Figure3_Nutrient_Drawdown_Diatom_Biomass.tiff
#   Supplementary_Figure4_NO3_SRP_Bloom_Response.pdf
#
# ============================================================

# ============================================================
# Package requirements
# ============================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "scales",
  "tibble"
)

missing_packages <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if (length(missing_packages) > 0) {

  stop(
    paste(
      "Missing packages:",
      paste(missing_packages, collapse = ", ")
    )
  )

}

# ============================================================
# Libraries and theme
# ============================================================

library(dplyr)
library(ggplot2)
library(readr)
library(scales)

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
        hjust = 0
      ),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
)

fig_dir <- "figures"
dir.create(
  fig_dir,
  showWarnings = FALSE
)

# ============================================================
# Read analysis dataset
# ============================================================

df <- read_csv(
  file.path(
    data_dir,
    "western_irish_sea_

# ============================================================
# Prepare Figure 3 dataset
# ============================================================

figure3_data <- df %>%
  filter(
    nutrient %in% c(
      "NO3",
      "SIO2",
      "SRP"
    ),
    !is.na(drawdown_mag),
    !is.na(bloom_intensity)
  ) %>%
  mutate(
    diatom_scaled = rescale(Diatom_max)
  )

# ============================================================
# Figure 3
# Bloom intensity responses to nutrient drawdown magnitude
# ============================================================

p_fig3 <- ggplot(
  figure3_data,
  aes(
    x = drawdown_mag,
    y = bloom_intensity
  )
) +

  # Observations with diatom biomass available
  geom_point(
    data = figure3_data %>%
      filter(!is.na(Diatom_max)),
    aes(colour = diatom_scaled),
    size = 2,
    alpha = 0.85
  ) +

  # Observations without diatom biomass
  geom_point(
    data = figure3_data %>%
      filter(is.na(Diatom_max)),
    shape = 21,
    fill = "white",
    colour = "black",
    size = 2,
    stroke = 0.6
  ) +

  # Linear relationship
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    colour = "black"
  ) +

  scale_colour_gradient(
    low = "grey70",
    high = "darkred"
  ) +

  # Panel labels
  geom_text(
    data = tibble::tibble(
      nutrient = c(
        "NO3",
        "SIO2",
        "SRP"
      ),
      label = c(
        "a",
        "b",
        "c"
      )
    ),
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
    ~ nutrient,
    scales = "free_x",
    ncol = 3,
    labeller = as_labeller(
      c(
        "NO3"  = "NO₃",
        "SIO2" = "SiO₂",
        "SRP"  = "SRP"
      )
    )
  ) +

  labs(
    x = "Drawdown magnitude",
    y = expression(
      "Bloom intensity (" * Delta * "Chl-a)"
    )
  ) +

  guides(colour = "none") +

  theme(
    strip.text = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

# ============================================================
# Export Figure 3
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Figure3_Nutrient_Drawdown_Diatom_Biomass.pdf"
  ),
  plot = p_fig3,
  width = 180,
  height = 90,
  units = "mm",
  device = cairo_pdf
)

ggsave(
  filename = file.path(
    fig_dir,
    "Figure3_Nutrient_Drawdown_Diatom_Biomass.tiff"
  ),
  plot = p_fig3,
  width = 180,
  height = 90,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

message("Figure 3 exported")



# ============================================================
# SUPPLEMENTARY FIGURE 4 | Bloom intensity responses to
# nitrate and phosphate drawdown
#
# Relationships between bloom intensity and the magnitude of
# seasonal nitrate (NO3) and phosphate (SRP) drawdown.
# Analyses are presented for comparison with the silicate-
# focused results shown in Figure 3, allowing assessment of
# whether bloom expression responds similarly across major
# nutrient pools.
# ============================================================


# ============================================================
# Prepare Supplementary Figure 4 dataset
# ============================================================

figureS4_data <- df %>%
  filter(
    nutrient %in% c(
      "NO3",
      "SRP"
    ),
    !is.na(drawdown_mag),
    !is.na(bloom_intensity)
  )

# ============================================================
# Supplementary Figure 4
# Bloom intensity responses to nitrate and phosphate drawdown
# ============================================================

p_sup_no3_srp <- ggplot(
  figureS4_data,
  aes(
    x = drawdown_mag,
    y = bloom_intensity
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
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    x = "Drawdown magnitude",
    y = expression(
      "Bloom intensity (" * Delta * "Chl-a)"
    )
  )

# ============================================================
# Export Supplementary Figure 4
# ============================================================

ggsave(
  filename = file.path(
    fig_dir,
    "Supplementary_Figure4_NO3_SRP_Bloom_Response.pdf"
  ),
  plot = p_sup_no3_srp,
  width = 180,
  height = 80,
  units = "mm",
  device = cairo_pdf
)

message("Supplementary Figure 4 exported")

# ============================================================
# END OF WORKFLOW
# ============================================================