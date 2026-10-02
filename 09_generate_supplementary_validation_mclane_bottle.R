# =============================================================================
# Validation of Nutrient Climatology
#
# Author: William Ross Hunter
#
# Purpose:
# Compare nutrient concentrations measured by the automated sampler and
# traditional bottle sampling programme during periods of temporal overlap.
#
# Nutrients evaluated:
#   - NO3
#   - NH4
#   - SRP
#   - SIO2
#   - NO2
#
# Outputs:
#   Bottle_vs_Sampler_regression_summary.csv
#   Bottle_vs_Sampler_regressions.pdf
#   Bottle_vs_Sampler_residuals.pdf
#   Bottle_vs_Sampler_QQplots.pdf
#   Corresponding TIFF versions for publication.
# =============================================================================

# -----------------------------------------------------------------------------
# Load packages
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)

# -----------------------------------------------------------------------------
# Nutrients to evaluate
# -----------------------------------------------------------------------------

nutrients <- c(
  "NO3",
  "NH4",
  "SRP",
  "SIO2",
  "NO2"
)

# -----------------------------------------------------------------------------
# Construct overlap dataset
#
# Retain dates where both sampler and bottle measurements are available for
# the same nutrient.
# -----------------------------------------------------------------------------

overlap_long <- map_dfr(nutrients, function(nut) {

  sampler_nut <- sampler_ts %>%
    select(
      D_DATE,
      sampler = all_of(nut)
    ) %>%
    filter(!is.na(sampler))

  bottle_nut <- Bottle_Nuts %>%
    select(
      D_DATE,
      bottle = all_of(nut)
    ) %>%
    filter(!is.na(bottle))

  sampler_nut %>%
    inner_join(
      bottle_nut,
      by = "D_DATE"
    ) %>%
    mutate(nutrient = nut)
})

# Optional sanity check
overlap_long %>%
  count(nutrient)

# -----------------------------------------------------------------------------
# Fit nutrient-specific regression models
#
# Model:
#   Bottle concentration ~ Sampler concentration
# -----------------------------------------------------------------------------

regression_results <- overlap_long %>%
  group_by(nutrient) %>%
  group_modify(~ {

    df <- .x

    if (nrow(df) < 6) {

      return(
        tibble(
          n = nrow(df),
          intercept = NA_real_,
          slope = NA_real_,
          r2 = NA_real_,
          rmse = NA_real_
        )
      )
    }

    mod <- lm(
      bottle ~ sampler,
      data = df
    )

    tibble(
      n = nrow(df),
      intercept = coef(mod)[1],
      slope = coef(mod)[2],
      r2 = summary(mod)$r.squared,
      rmse = sqrt(mean(resid(mod)^2))
    )
  }) %>%
  ungroup()

print(regression_results)

# -----------------------------------------------------------------------------
# Export regression summary table
# -----------------------------------------------------------------------------

write.csv(
  regression_results,
  "Bottle_vs_Sampler_regression_summary.csv",
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# Diagnostic plot 1:
# Bottle versus sampler concentrations
# -----------------------------------------------------------------------------

p_obs <- ggplot(
  overlap_long,
  aes(
    sampler,
    bottle
  )
) +
  geom_point(alpha = 0.7) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "black"
  ) +
  facet_wrap(
    ~nutrient,
    scales = "free"
  ) +
  theme_bw() +
  labs(
    x = "Sampler concentration",
    y = "Bottle concentration",
    title = "Bottle versus sampler nutrient measurements"
  )

ggsave(
  file.path(
    fig_dir,
    "Bottle_vs_Sampler_regressions.pdf"
  ),
  p_obs,
  width = 180,
  height = 120,
  units = "mm",
  device = cairo_pdf
)

# -----------------------------------------------------------------------------
# Build residual diagnostics dataset
# -----------------------------------------------------------------------------

diag_df <- overlap_long %>%
  group_by(nutrient) %>%
  group_modify(~ {
    mod <- lm(
      bottle ~ sampler,
      data = .x
    )
    augment(mod)
  }) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Diagnostic plot 2:
# Residuals versus fitted values
# -----------------------------------------------------------------------------

p_resid <- ggplot(
  diag_df,
  aes(
    .fitted,
    .resid
  )
) +
  geom_point(alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~nutrient,
    scales = "free"
  ) +
  theme_bw() +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residual diagnostics"
  )

ggsave(
  file.path(
    fig_dir,
    "Bottle_vs_Sampler_residuals.pdf"
  ),
  p_resid,
  width = 180,
  height = 120,
  units = "mm",
  device = cairo_pdf
)

# -----------------------------------------------------------------------------
# Diagnostic plot 3:
# Normality assessment using QQ plots
# -----------------------------------------------------------------------------

p_qq <- ggplot(
  diag_df,
  aes(sample = .std.resid)
) +
  stat_qq(alpha = 0.5) +
  stat_qq_line() +
  facet_wrap(
    ~nutrient,
    scales = "free"
  ) +
  theme_bw() +
  labs(
    title = "QQ plots of standardised residuals"
  )

ggsave(
  file.path(
    fig_dir,
    "Bottle_vs_Sampler_QQplots.pdf"
  ),
  p_qq,
  width = 180,
  height = 120,
  units = "mm",
  device = cairo_pdf
)

# -----------------------------------------------------------------------------
# Assess evidence for systematic bias
#
# Criteria:
#   - Slope differs from unity by > 0.2
#   - Intercept exceeds 20% of mean bottle concentration
# -----------------------------------------------------------------------------

regression_results <- regression_results %>%
  mutate(
    slope_flag =
      abs(slope - 1) > 0.2,

    intercept_flag =
      abs(intercept) >
      0.2 * mean(
        overlap_long$bottle,
        na.rm = TRUE
      ),

    bias_flag =
      slope_flag | intercept_flag
  )

print(regression_results)

write.csv(
  regression_results,
  "Bottle_Sampler_regression_summary.csv",
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# Export high-resolution TIFF figures
# -----------------------------------------------------------------------------

tiff(
  filename = file.path(
    fig_dir,
    "Bottle_vs_Sampler_regressions.tiff"
  ),
  width = 180,
  height = 120,
  units = "mm",
  res = 600,
  compression = "lzw"
)
print(p_obs)
dev.off()

tiff(
  filename = file.path(
    fig_dir,
    "Bottle_vs_Sampler_residuals.tiff"
  ),
  width = 180,
  height = 120,
  units = "mm",
  res = 600,
  compression = "lzw"
)
print(p_resid)
dev.off()

tiff(
  filename = file.path(
    fig_dir,
    "Bottle_vs_Sampler_QQplots.tiff"
  ),
  width = 180,
  height = 120,
  units = "mm",
  res = 600,
  compression = "lzw"
)
print(p_qq)
dev.off()

# =============================================================================
# End of script
# =============================================================================