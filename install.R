# Dit bestand wordt door Render (of elke andere host) gebruikt om alle
# benodigde R packages te installeren vóór de app start.
# Render-buildcommand: Rscript install.R

pkgs <- c(
  "shiny",
  "shinydashboard",
  "DBI",
  "duckdb",
  "plotly",
  "dplyr",
  "googleAnalyticsR"
)

nieuwe_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if (length(nieuwe_pkgs) > 0) {
  install.packages(nieuwe_pkgs, repos = "https://cloud.r-project.org")
}
