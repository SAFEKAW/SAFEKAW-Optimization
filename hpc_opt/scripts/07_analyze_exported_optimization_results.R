suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(scales)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

root <- normalizePath(get_arg("--root", "."), mustWork = TRUE)
outdir <- get_arg("--output", file.path(root, "local_optimization_analysis"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

pareto_files <- list.files(
  root,
  pattern = "^pareto_front_.*[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (!length(pareto_files)) stop("No Pareto-front CSV files found under: ", root)

read_front <- function(path) {
  x <- read_csv(path, show_col_types = FALSE)
  if (!"run_tag" %in% names(x)) {
    normalized <- gsub("\\\\", "/", path)
    x$run_tag <- sub(".*/outputs/runs/([^/]+)/.*", "\\1", normalized)
  }
  if (!"phase" %in% names(x)) x$phase <- "unconstrained"
  if (!"irrigation_technology_name" %in% names(x)) {
    x$irrigation_technology_name <- "current"
  }
  if (!"feasible" %in% names(x)) x$feasible <- TRUE
  x$source_file <- path
  x
}

solutions <- map_dfr(pareto_files, read_front) %>%
  filter(feasible) %>%
  filter(if_all(c(nitrate, irrigation, profit), is.finite))

required <- c(
  "run_tag", "scenario_name", "seed", "phase",
  "irrigation_technology_name", "nitrate", "irrigation", "profit"
)
missing <- setdiff(required, names(solutions))
if (length(missing)) stop("Pareto outputs missing: ", paste(missing, collapse = ", "))

inventory <- solutions %>%
  group_by(
    run_tag, phase, irrigation_technology_name, scenario_name, seed
  ) %>%
  summarise(
    solutions = n(),
    nitrate_min = min(nitrate),
    nitrate_max = max(nitrate),
    irrigation_min = min(irrigation),
    irrigation_max = max(irrigation),
    profit_min = min(profit),
    profit_max = max(profit),
    .groups = "drop"
  )
write_csv(inventory, file.path(outdir, "optimization_solution_inventory.csv"))

phase_summary <- solutions %>%
  group_by(run_tag, phase, irrigation_technology_name) %>%
  summarise(
    scenarios = n_distinct(scenario_name),
    seeds = n_distinct(seed),
    solutions = n(),
    nitrate_min = min(nitrate),
    nitrate_max = max(nitrate),
    irrigation_min = min(irrigation),
    irrigation_max = max(irrigation),
    profit_min = min(profit),
    profit_max = max(profit),
    .groups = "drop"
  )
write_csv(phase_summary, file.path(outdir, "optimization_summary_by_run.csv"))

plot_data <- solutions %>%
  mutate(
    nitrate_million_kg = nitrate / 1e6,
    irrigation_million_m3 = irrigation / 1e6,
    profit_million_usd = profit / 1e6,
    period = factor(period, levels = c("early", "mid", "late")),
    climate_pathway = factor(
      climate_pathway,
      levels = c("rcp45", "rcp85"),
      labels = c("RCP 4.5", "RCP 8.5")
    ),
    landuse_name = factor(
      landuse_name,
      levels = c("fixed", "bau"),
      labels = c("Fixed land use", "BAU land use")
    ),
    irrigation_technology_name = factor(
      irrigation_technology_name,
      levels = c("current", "efficient"),
      labels = c("Current technology", "Efficient technology")
    ),
    phase = factor(
      phase,
      levels = c(
        "unconstrained", "crop_bounds", "crop_fert", "crop_fert_irrigfrac"
      ),
      labels = c(
        "Unconstrained crop allocation",
        "Crop allocation",
        "Crop + fertilizer",
        "Crop + fertilizer + irrigation extent"
      )
    )
  )

nitrate_limits <- range(plot_data$nitrate_million_kg, na.rm = TRUE)

for (tag in unique(plot_data$run_tag)) {
  tag_data <- filter(plot_data, run_tag == tag)
  safe_tag <- gsub("[^A-Za-z0-9._-]", "_", tag)

  p <- ggplot(
    tag_data,
    aes(nitrate_million_kg, profit_million_usd, color = irrigation_million_m3)
  ) +
    geom_point(alpha = 0.65, size = 1.25) +
    facet_grid(
      rows = vars(landuse_name, irrigation_technology_name),
      cols = vars(climate_pathway, period)
    ) +
    scale_color_viridis_c(
      option = "C",
      direction = -1,
      limits = range(plot_data$irrigation_million_m3, na.rm = TRUE),
      oob = scales::squish
    ) +
    labs(
      title = paste("SAFE-KAW Pareto solutions:", tag),
      subtitle = paste("Decision formulation:", first(as.character(tag_data$phase))),
      x = "Nitrate load (million kg/year)",
      y = "Net return (million USD/year)",
      color = "Irrigation\n(million m3/year)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 8.5)
    )

  ggsave(
    file.path(outdir, paste0("pareto_", safe_tag, ".pdf")),
    p,
    width = 18,
    height = 11,
    limitsize = FALSE
  )
}

if (any(as.character(plot_data$phase) != "Unconstrained crop allocation")) {
  for (land_use in levels(plot_data$landuse_name)) {
    comparison <- plot_data %>%
      filter(landuse_name == land_use, !is.na(phase))
    if (!nrow(comparison)) next
    safe_land_use <- gsub("[^A-Za-z0-9._-]", "_", land_use)

    p_comparison <- ggplot(
      comparison,
      aes(nitrate_million_kg, profit_million_usd, color = irrigation_million_m3)
    ) +
      geom_point(alpha = 0.58, size = 1.05) +
      facet_grid(
        rows = vars(phase, irrigation_technology_name),
        cols = vars(climate_pathway, period)
      ) +
      scale_color_viridis_c(
        option = "C", direction = -1,
        limits = range(plot_data$irrigation_million_m3, na.rm = TRUE),
        oob = scales::squish
      ) +
      labs(
        title = paste("Staged constrained optimization —", land_use),
        subtitle = "Each successive row adds a decision lever; the final phase is fully integrated",
        x = "Nitrate load (million kg/year)",
        y = "Net return (million USD/year)",
        color = "Irrigation\n(million m3/year)"
      ) +
      theme_minimal(base_size = 9.5) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 8)
      )

    ggsave(
      file.path(
        outdir,
        paste0("constrained_phase_comparison_", safe_land_use, ".pdf")
      ),
      p_comparison, width = 18, height = 15, limitsize = FALSE
    )
  }
}

benchmark_files <- list.files(
  root,
  pattern = "^optimization_benchmark_summary[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(benchmark_files)) {
  benchmark <- map_dfr(benchmark_files, read_csv, show_col_types = FALSE) %>%
    mutate(evaluations_proxy = popsize * generations)
  write_csv(benchmark, file.path(outdir, "benchmark_summary_combined.csv"))

  p_benchmark <- ggplot(
    benchmark,
    aes(evaluations_proxy, dominated_hypervolume)
  ) +
    geom_line() +
    geom_point(aes(size = elapsed_seconds), alpha = 0.8) +
    geom_text(
      aes(label = paste0("p", popsize, " g", generations)),
      nudge_y = 0.01,
      check_overlap = TRUE
    ) +
    scale_x_log10(labels = label_number()) +
    labs(
      title = "Optimizer-budget benchmark",
      x = "Population x generations (log scale)",
      y = "Normalized dominated hypervolume",
      size = "Runtime (seconds)"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    file.path(outdir, "optimizer_budget_benchmark.pdf"),
    p_benchmark, width = 8, height = 5.5
  )
}

cumulative_files <- list.files(
  root,
  pattern = "^cumulative_seed_stability[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(cumulative_files)) {
  cumulative <- map_dfr(cumulative_files, read_csv, show_col_types = FALSE)
  write_csv(cumulative, file.path(outdir, "cumulative_seed_stability_combined.csv"))

  p_stability <- ggplot(
    cumulative,
    aes(seeds_included, dominated_hypervolume)
  ) +
    geom_line() +
    geom_point() +
    facet_wrap(~scenario_name, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(cumulative$seeds_included))) +
    labs(
      title = "Cumulative Pareto-front stability across optimizer seeds",
      x = "Seeds pooled",
      y = "Normalized dominated hypervolume"
    ) +
    theme_minimal(base_size = 10)
  ggsave(
    file.path(outdir, "cumulative_seed_stability.pdf"),
    p_stability, width = 12, height = 8
  )
}

leave_one_out_files <- list.files(
  root,
  pattern = "^leave_one_seed_out_stability[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(leave_one_out_files)) {
  leave_one_out <- map_dfr(
    leave_one_out_files, read_csv, show_col_types = FALSE
  )
  write_csv(
    leave_one_out,
    file.path(outdir, "leave_one_seed_out_stability_combined.csv")
  )
}

sufficiency_files <- list.files(
  root,
  pattern = "^five_seed_sufficiency[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(sufficiency_files)) {
  sufficiency <- map_dfr(
    sufficiency_files, read_csv, show_col_types = FALSE
  )
  write_csv(
    sufficiency,
    file.path(outdir, "five_seed_sufficiency_combined.csv")
  )
}

message("Read ", length(pareto_files), " Pareto files and ", nrow(solutions), " feasible solutions.")
message("Wrote local summaries and plots to: ", outdir)
