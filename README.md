# Attenuation of climatic signals limits prediction of phytoplankton blooms in a temperate shelf sea

[![R](https://img.shields.io/badge/R-4.4+-blue.svg[License](https://img.shields.io/badge/license-M]()

## Overview

This repository contains the data, analysis workflows, statistical models, and figure-generation scripts used in:

> Hunter, W.R., O'Kane, E., Smyth, C., McClements, D., Service, M. & Mellor, A.
>
> **Attenuation of climatic signals limits prediction of phytoplankton blooms in a temperate shelf sea**

The study uses a multi-decadal observational record from the western Irish Sea gyre (Station 38A) to examine how climatic variability propagates through nutrient dynamics and ultimately influences phytoplankton bloom intensity.

Results demonstrate that relationships between climate forcing and ecosystem function weaken across successive ecological levels, with strong relationships observed for nutrient inventories and nutrient drawdown, but substantially weaker relationships for phytoplankton bloom intensity.

---

## Study Site

**Station 38A, Western Irish Sea**

- Latitude: 53° 46.830′ N
- Longitude: 05° 38.030′ W
- Water depth: ~94 m
- Region: seasonally stratified western Irish Sea gyre

---

## Repository Structure

```text
.
├── README.md
│
├── data/
│   ├── western_irish_sea_climate_nutrient_bloom_metrics.csv
│   └── western_irish_sea_variable_dictionary.csv
│
├── figures/
│
└── scripts/
    ├── 01_generate_analysis_dataset.R
    ├── 02_generate_manuscript_figures_part1.R
    ├── 03_generate_manuscript_figures_part2.R
    ├── 04_generate_manuscript_figures_part3.R
    ├── 05_generate_manuscript_figures_part4.R
    ├── 06_generate_regression_summary_table.R
    ├── 07_gam_diagnostics.R
    ├── 08_generate_supplementary_figure3_climate_diatom.R
    └── 09_generate_supplementary_validation_mclane_bottle.R
```

---

## Workflow

The analytical workflow progresses from data preparation through figure generation and supplementary analyses.

```text
Raw nutrient, climate and plankton observations
                    │
                    ▼
      01_generate_analysis_dataset.R
                    │
                    ▼
     analysis_all_sst_plankton_nao.csv
                    │
     ┌──────────────┼──────────────┐
     ▼              ▼              ▼
Figures        Statistics      Diagnostics
     │              │              │
02–05          06             07–09
```

---

## Script Descriptions

### 01_generate_analysis_dataset.R

Creates the annual analysis dataset used throughout the study.

Derived metrics include:

- Winter nutrient inventories
- Nutrient drawdown timing
- Nutrient drawdown magnitude
- Bloom intensity metrics
- Climate forcing descriptors
- Phytoplankton community metrics
- Zooplankton richness

Output:

```text
analysis_all_sst_plankton_nao.csv
```

---

### 02_generate_manuscript_figures_part1.R

Generates:

- Figure 1
- Supplementary Figure 1

Focus:

- Climate variability
- Nutrient availability
- Seasonal nutrient drawdown
- Bloom intensity time series

---

### 03_generate_manuscript_figures_part2.R

Generates:

- Figure 2

Focus:

- Climate controls on nutrient drawdown timing
- Winter nutrient inventories and drawdown magnitude

---

### 04_generate_manuscript_figures_part3.R

Generates:

- Figure 3

Focus:

- Nutrient drawdown versus phytoplankton bloom intensity
- Diatom abundance relationships

---

### 05_generate_manuscript_figures_part4.R

Generates:

- Figure 4

Focus:

- Conceptual attenuation framework
- Progressive weakening of explanatory power across ecosystem levels

---

### 06_generate_regression_summary_table.R

Fits all linear regression models used in the manuscript and supplementary analyses.

Outputs:

```text
Supplementary_Table_Regression_Summaries.csv
Supplementary_Table_Regression_Summaries.xlsx
```

Used to generate:

- Supplementary Table 1

---

### 07_gam_diagnostics.R

Compares linear models and restricted Generalised Additive Models (GAMs).

Purpose:

- Evaluate potential nonlinear relationships.
- Quantify changes in explanatory power.
- Compare model support using AIC.

Output:

```text
Supplementary_Table_GAM_Summaries.csv
```

Used to generate:

- Supplementary Table 2

---

### 08_generate_supplementary_figure3_climate_diatom.R

Generates:

- Supplementary Figure 3

Focus:

- Climate variability
- Diatom abundance
- Marine heatwaves
- Sea surface temperature
- North Atlantic Oscillation

---

### 09_generate_supplementary_validation_mclane_bottle.R

Validates reconstructed nutrient time series by comparing:

- McLane® Remote Access Sampler observations
- Ship-based CTD bottle measurements

Outputs:

- Regression diagnostics
- Residual analyses
- QQ plots
- Validation summary tables

Used to generate:

- Supplementary Figure 4
- Supplementary Table 3

---

## Main Variables

Climate variables:

| Variable | Description |
|-----------|-------------|
| temp_mean | Mean sea-surface temperature |
| hw_int_cum | Annual cumulative marine heatwave intensity |
| cs_int_cum | Annual cumulative cold-spell intensity |
| nao_DJFM | Winter North Atlantic Oscillation index |

Nutrient metrics:

| Variable | Description |
|-----------|-------------|
| winter_mean_nut | Mean winter concentration |
| drawdown_doy | Day of year drawdown threshold reached |
| drawdown_mag | Seasonal drawdown magnitude |

Biological variables:

| Variable | Description |
|-----------|-------------|
| bloom_intensity | Spring bloom intensity |
| Diatom_max | Maximum annual diatom abundance |
| Zoop_richness | Annual zooplankton richness |

---

## Software Requirements

Analyses were conducted in:

```text
R 4.4+
```

Primary packages include:

```r
dplyr
ggplot2
tidyr
readr
patchwork
mgcv
purrr
broom
writexl
```

---

## Reproducing the Analysis

1. Clone repository

```bash
git clone https://github.com/USERNAME/REPOSITORY.git
```

2. Open R project.

3. Run scripts sequentially:

```r
01_generate_analysis_dataset.R
02_generate_manuscript_figures_part1.R
03_generate_manuscript_figures_part2.R
04_generate_manuscript_figures_part3.R
05_generate_manuscript_figures_part4.R
06_generate_regression_summary_table.R
07_gam_diagnostics.R
08_generate_supplementary_figure3_climate_diatom.R
09_generate_supplementary_validation_mclane_bottle.R
```

All figures and supplementary outputs will be written automatically to the `figures/` directory.

---

## Citation

If you use these data or analysis workflows, please cite:

```text
Hunter, W.R., O'Kane, E., Smyth, C.,
McClements, D., Service, M. & Mellor, A.

Attenuation of climatic signals limits prediction
of phytoplankton blooms in a temperate shelf sea.
```

---

## Contact

**William Ross (Billy) Hunter**

Senior Scientific Officer  
Agri-Food and Biosciences Institute (AFBI)  
Belfast, Northern Ireland

Email: Billy.Hunter@afbini.gov.uk

---

## License

MIT License

See LICENSE file for details.
