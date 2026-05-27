###############################################################################
#  _targets.R — Reproducible pipeline for NBCS 2026 Thika Rangers analysis
#
#  Pipeline steps:
#    1. Read & clean raw CSV (Nairobi Baseball Community data)
#    2. Compute team summary statistics
#    3. Generate all visualisations (12 ggplot2/patchwork objects)
#    4. Build interactive gt table
#    5. Save processed outputs (PNG + RDS)
#
#  Usage:
#    targets::tar_make()              # run full pipeline
#    targets::tar_load(everything())  # load results into global env
#    targets::tar_visnetwork()        # visualise pipeline graph
#
#  Notes:
#    - Quarto rendering is manual:  quarto render report.qmd
#    - Logo goes in shiny/www/rebranded-logo.png for auto colour extraction
#    - Data source: Nairobi Baseball Community  |  Analysis: Keith Karani
###############################################################################

library(targets)

# Source custom functions (suppress bslib namespace mask messages)
suppressPackageStartupMessages({
  source("R/team_theme.R")
  source("R/functions.R")
})

tar_option_set(
  packages = c("dplyr", "tidyr", "ggplot2", "scales", "gt"),
  format   = "rds",
  memory   = "transient",
  garbage_collection = TRUE
)

# ── Pipeline ────────────────────────────────────────────────────────────────

list(
  tar_target(raw_data_path, "tr-stats26.csv", format = "file"),

  tar_target(batting_data, read_batting_data(raw_data_path)),

  tar_target(team_summary, compute_team_summary(batting_data)),

  tar_target(plots, generate_all_plots(batting_data), iteration = "list"),

  tar_target(gt_table, build_gt_batting_table(batting_data, team_summary)),

  tar_target(save_plots, {
    dir.create("output", showWarnings = FALSE)
    paths <- file.path("output", paste0(names(plots), ".png"))
    for (i in seq_along(plots)) {
      ggplot2::ggsave(paths[i], plots[[i]], width = 12, height = 7, dpi = 150)
    }
    paths
  }, format = "file"),

  tar_target(export_data, {
    dir.create("output", showWarnings = FALSE)
    saveRDS(batting_data, "output/batting_data.rds")
    saveRDS(team_summary, "output/team_summary.rds")
    saveRDS(gt_table,     "output/gt_table.rds")
    c("output/batting_data.rds",
      "output/team_summary.rds",
      "output/gt_table.rds")
  }, format = "file")
)
