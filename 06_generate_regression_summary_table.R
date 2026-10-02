# =============================================================================
# 03_generate_regression_summary_table.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Fit all linear models reported in the manuscript and
# supplementary analyses and export a consolidated summary
# table of regression statistics.
#
# Outputs:
#   Supplementary_Table_Regression_Summaries.csv
#   Supplementary_Table_Regression_Summaries.xlsx
#
# =============================================================================

# =============================================================================
# Package requirements
# =============================================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "broom",
  "readr",
  "purrr",
  "stringr",
  "writexl"
)

missing_packages <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if (length(missing_packages) > 0) {

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

# =============================================================================
# Load libraries
# =============================================================================

library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(purrr)
library(stringr)
library(writexl)

# =============================================================================
# Project directories
# =============================================================================

data_dir <- "data"
fig_dir  <- "figures"

dir.create(
  fig_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# =============================================================================
# Read analysis dataset
# =============================================================================

df <- read_csv(
  file.path(
    data_dir,
    "analysis_all_sst_plankton_nao.csv"
  ),
  show_col_types = FALSE
)

# =============================================================================
# Helper function
# Extract regression statistics from linear models
# =============================================================================

extract_lm_stats <- function(
  model,
  response,
  predictor,
  group_label
) {

  smry <- summary(model)

  tibble(
    response  = response,
    predictor = predictor,
    group     = group_label,
    slope     = smry$coefficients[2, 1],
    intercept = smry$coefficients[1, 1],
    p_value   = smry$coefficients[2, 4],
    r_squared = smry$r.squared,
    adj_r2    = smry$adj.r.squared,
    n         = nobs(model)
  )

}

# =============================================================================
# Analysis 1
# Climate effects on nutrient drawdown timing
# Figure 2a
# =============================================================================

message("Fitting drawdown timing models")

df_timing <- df %>%
  filter(!is.na(drawdown_doy)) %>%
  filter(
    nutrient %in% c(
      "NO3",
      "NH4",
      "SIO2",
      "SRP"
    )
  )

timing_models <- df_timing %>%
  group_by(nutrient) %>%
  group_map(~ {

    mod <- lm(
      drawdown_doy ~ temp_mean,
      data = .x
    )

    extract_lm_stats(
      mod,
      response = "Drawdown timing (DOY)",
      predictor = "Mean SST",
      group_label = unique(.x$nutrient)
    )

  }) %>%
  bind_rows()

# =============================================================================
# Analysis 2
# Winter nutrient concentration versus drawdown magnitude
# Figure 2b
# =============================================================================

message("Fitting drawdown magnitude models")

df_mag <- df %>%
  filter(!is.na(drawdown_mag)) %>%
  filter(
    nutrient %in% c(
      "NO3",
      "NH4",
      "SIO2",
      "SRP"
    )
  )

magnitude_models <- df_mag %>%
  group_by(nutrient) %>%
  group_map(~ {

    mod <- lm(
      drawdown_mag ~ winter_mean_nut,
      data = .x
    )

    extract_lm_stats(
      mod,
      response = "Drawdown magnitude",
      predictor = "Winter nutrient concentration",
      group_label = unique(.x$nutrient)
    )

  }) %>%
  bind_rows()

# =============================================================================
# Analysis 3
# Nutrient drawdown magnitude versus bloom intensity
# Figure 3 and supplementary analyses
# =============================================================================

message("Fitting bloom intensity models")

df_bloom <- df %>%
  filter(
    !is.na(drawdown_mag),
    !is.na(bloom_intensity)
  ) %>%
  filter(
    nutrient %in% c(
      "NO3",
      "SIO2",
      "SRP"
    )
  )

bloom_models <- df_bloom %>%
  group_by(nutrient) %>%
  group_map(~ {

    mod <- lm(
      bloom_intensity ~ drawdown_mag,
      data = .x
    )

    extract_lm_stats(
      mod,
      response = "Bloom intensity",
      predictor = "Drawdown magnitude",
      group_label = unique(.x$nutrient)
    )

  }) %>%
  bind_rows()

# =============================================================================
# Analysis 4
# Climate influences on biological structure
# Supplementary Figure 5
# =============================================================================

message("Fitting climate-biology models")

df_bio <- df %>%
  filter(
    nutrient == "NO3"
  ) %>%
  select(
    year,
    temp_mean,
    hw_int_cum,
    nao_DJFM,
    Diatom_max,
    Zoop_richness
  )

climate_vars <- c(
  "temp_mean",
  "hw_int_cum",
  "nao_DJFM"
)

bio_vars <- c(
  "Diatom_max",
  "Zoop_richness"
)

bio_models <- crossing(
  climate = climate_vars,
  biology = bio_vars
) %>%
  pmap_dfr(function(
    climate,
    biology
  ) {

    mod <- lm(
      as.formula(
        paste(
          biology,
          "~",
          climate
        )
      ),
      data = df_bio
    )

    extract_lm_stats(
      mod,
      response = biology,
      predictor = climate,
      group_label = "Climate to biology diagnostic"
    )

  })

# =============================================================================
# Assemble final regression summary table
# =============================================================================

message("Compiling regression summary table")

regression_summary <- bind_rows(
  timing_models,
  magnitude_models,
  bloom_models,
  bio_models
) %>%
  mutate(
    slope     = round(slope, 3),
    intercept = round(intercept, 3),
    r_squared = round(r_squared, 3),
    adj_r2    = round(adj_r2, 3),
    p_value   = signif(p_value, 3)
  )

# =============================================================================
# Export outputs
# =============================================================================

write_csv(
  regression_summary,
  file.path(
    fig_dir,
    "Supplementary_Table_Regression_Summaries.csv"
  )
)

write_xlsx(
  list(
    "Regression summaries" = regression_summary
  ),
  file.path(
    fig_dir,
    "Supplementary_Table_Regression_Summaries.xlsx"
  )
)

message("Regression summary table exported")

# =============================================================================
# End of script
# =============================================================================