# =============================================================================
# 07_gam_diagnostics.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Compare linear models (LMs) and generalized additive models
# (GAMs) for selected nutrient-bloom relationships and assess
# evidence for nonlinearity using AIC and explained deviance.
#
# Outputs:
#   Supplementary_Table_GAM_Summaries.csv
#
# =============================================================================

# =============================================================================
# Package requirements
# =============================================================================

required_packages <- c(
  "dplyr",
  "mgcv",
  "broom",
  "purrr",
  "readr",
  "flextable",
  "officer"
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
library(mgcv)
library(broom)
library(purrr)
library(readr)
library(flextable)
library(officer)

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
# Fit linear and GAM models and extract diagnostics
# =============================================================================

fit_lm_gam <- function(
  data,
  response,
  predictor,
  k = 4
) {

  form_lm <- as.formula(
    paste(
      response,
      "~",
      predictor
    )
  )

  form_gam <- as.formula(
    paste(
      response,
      "~ s(",
      predictor,
      ", k =",
      k,
      ")"
    )
  )

  lm_fit <- lm(
    form_lm,
    data = data
  )

  gam_fit <- gam(
    form_gam,
    data = data,
    method = "REML"
  )

  tibble(
    response    = response,
    predictor   = predictor,
    n           = nobs(lm_fit),

    lm_r2       = summary(lm_fit)$r.squared,
    lm_p        = summary(lm_fit)$coefficients[2, 4],
    lm_AIC      = AIC(lm_fit),

    gam_edf     = summary(gam_fit)$s.table[1, "edf"],
    gam_dev_exp = summary(gam_fit)$dev.expl,
    gam_AIC     = AIC(gam_fit),

    delta_AIC   = AIC(lm_fit) - AIC(gam_fit)
  )

}

# =============================================================================
# Analysis 1
# Silicate drawdown versus bloom intensity
# Primary nonlinear diagnostic
# =============================================================================

message("Evaluating silicate-bloom relationship")

df_si <- df %>%
  filter(
    nutrient == "SIO2"
  ) %>%
  filter(
    !is.na(drawdown_mag),
    !is.na(bloom_intensity)
  )

gam_si_diag <- fit_lm_gam(
  data = df_si,
  response = "bloom_intensity",
  predictor = "drawdown_mag",
  k = 4
)

# =============================================================================
# GAM basis dimension check
# =============================================================================

message("Running GAM basis check: silicate")

gam.check(
  gam(
    bloom_intensity ~ s(
      drawdown_mag,
      k = 4
    ),
    data = df_si,
    method = "REML"
  )
)

# =============================================================================
# Analysis 2
# Nitrate drawdown versus bloom intensity
# Negative control analysis
# =============================================================================

message("Evaluating nitrate-bloom relationship")

df_no3 <- df %>%
  filter(
    nutrient == "NO3"
  ) %>%
  filter(
    !is.na(drawdown_mag),
    !is.na(bloom_intensity)
  )

gam_no3_diag <- fit_lm_gam(
  data = df_no3,
  response = "bloom_intensity",
  predictor = "drawdown_mag",
  k = 4
)

# =============================================================================
# GAM basis dimension check
# =============================================================================

message("Running GAM basis check: nitrate")

gam.check(
  gam(
    bloom_intensity ~ s(
      drawdown_mag,
      k = 4
    ),
    data = df_no3,
    method = "REML"
  )
)

# =============================================================================
# Assemble diagnostic summary table
# =============================================================================

message("Compiling GAM diagnostic summary")

gam_summary <- bind_rows(
  gam_si_diag %>%
    mutate(
      case = "Silicate to bloom"
    ),

  gam_no3_diag %>%
    mutate(
      case = "Nitrate to bloom (control)"
    )
) %>%
  mutate(
    lm_r2       = round(lm_r2, 3),
    gam_dev_exp = round(gam_dev_exp, 3),
    gam_edf     = round(gam_edf, 2),
    delta_AIC   = round(delta_AIC, 2),
    lm_p        = signif(lm_p, 3)
  )

# =============================================================================
# Export outputs
# =============================================================================

write_csv(
  gam_summary,
  file.path(
    fig_dir,
    "Supplementary_Table_GAM_Summaries.csv"
  )
)

message("GAM diagnostic summary exported")

# =============================================================================
# End of script
# =============================================================================