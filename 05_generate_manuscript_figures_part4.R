# ============================================================
# 05_generate_attenuation_framework.R
#
# Author: William Ross Hunter
#
# Study:
# Attenuation of climatic signals limits prediction of
# phytoplankton blooms in a temperate shelf sea
#
# Purpose:
# Generate Figure 4, a conceptual attenuation framework
# illustrating the progressive weakening of climate signals
# from environmental forcing through nutrient dynamics to
# phytoplankton bloom expression.
#
# Outputs:
#   Figure4_Attenuation_Framework.svg
#   Figure4_Attenuation_Framework.pdf
#   Figure4_Attenuation_Framework.png
#
# ============================================================

# ============================================================
# Package requirements
# ============================================================

required_packages <- c(
  "DiagrammeR",
  "DiagrammeRsvg",
  "rsvg"
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
# Load libraries
# ============================================================

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

fig_dir <- "figures"

dir.create(
  fig_dir,
  showWarnings = FALSE
)

# ============================================================
# Figure 5
# Attenuation framework linking climate forcing,
# nutrient dynamics and bloom intensity
# ============================================================

fig5 <- grViz("

digraph attenuation {

graph [
  layout = dot,
  rankdir = TB,
  bgcolor = white,
  nodesep = 0.6,
  ranksep = 1.0
]

node [
  shape = box,
  style = 'rounded,filled',
  fontname = Helvetica,
  fontsize = 14,
  penwidth = 1.5,
  color = grey30
]

Climate [
  label = 'Climate forcing\n(SST, MHW, NAO)',
  fillcolor = '#2171b5',
  fontcolor = white
]

Timing [
  label = 'Nutrient drawdown\ntiming',
  fillcolor = '#6baed6'
]

Inventory [
  label = 'Winter nutrient\ninventory',
  fillcolor = '#9ecae1'
]

Drawdown [
  label = 'Seasonal nutrient\ndrawdown',
  fillcolor = '#c6dbef'
]

Bloom [
  label = 'Bloom intensity',
  fillcolor = '#fdd0a2'
]

Climate -> Timing [
  label = 'R² = 0.57',
  penwidth = 6
]

Timing -> Inventory [
  label = 'R² = 0.43',
  penwidth = 5
]

Inventory -> Drawdown [
  label = 'R² = 0.31',
  penwidth = 3.5
]

Drawdown -> Bloom [
  label = 'R² = 0.08',
  penwidth = 1.2
]

}

")

# ============================================================
# Export Figure 4
# ============================================================

svg <- export_svg(fig5)

writeLines(
  svg,
  file.path(
    fig_dir,
    "Figure4_Attenuation_Framework.svg"
  )
)

rsvg_pdf(
  charToRaw(svg),
  file.path(
    fig_dir,
    "Figure4_Attenuation_Framework.pdf"
  ),
  width = 8,
  height = 10
)

rsvg_png(
  charToRaw(svg),
  file.path(
    fig_dir,
    "Figure4_Attenuation_Framework.png"
  ),
  width = 2400,
  height = 3000
)

