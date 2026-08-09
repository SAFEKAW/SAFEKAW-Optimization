suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
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

root <- normalizePath(get_arg("--root", "hpc_opt/local_exports/clippedcdf-constrained"), mustWork = TRUE)
historical_path <- normalizePath(
  get_arg("--historical", "hpc_opt/outputs/deterministic_summary/historical_objective_reference.csv"),
  mustWork = TRUE
)
outdir <- get_arg("--output", file.path(root, "final_integrated_visualization"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

front_files <- list.files(root, "^pareto_front_.*[.]csv$", recursive = TRUE, full.names = TRUE)
if (!length(front_files)) stop("No Pareto-front CSV files found under: ", root)

read_front <- function(path) {
  read_csv(path, show_col_types = FALSE) %>% mutate(source_file = path)
}

solutions <- map_dfr(front_files, read_front) %>%
  filter(phase == "crop_fert_irrigfrac", feasible) %>%
  filter(if_all(c(nitrate, irrigation, profit), is.finite))
if (!nrow(solutions)) stop("No feasible crop_fert_irrigfrac solutions found.")

required <- c(
  "Corn", "Soybeans", "Sorghum", "Wheat", "fert_factor", "irrig_frac_factor",
  "climate_pathway", "period", "landuse_name", "irrigation_technology_name",
  "nitrate", "irrigation", "profit"
)
missing <- setdiff(required, names(solutions))
if (length(missing)) stop("Final Pareto outputs missing: ", paste(missing, collapse = ", "))

historical_long <- read_csv(historical_path, show_col_types = FALSE)
historical <- historical_long %>%
  mutate(objective = recode(objective, net_returns = "profit")) %>%
  select(objective, historical_mean) %>%
  pivot_wider(names_from = objective, values_from = historical_mean)
if (!all(c("nitrate", "irrigation", "profit") %in% names(historical))) {
  stop("Historical reference must contain nitrate, irrigation, and net_returns rows.")
}
if (any(unlist(historical[c("nitrate", "irrigation", "profit")]) <= 0)) {
  stop("Historical objective references must be positive.")
}
write_csv(historical_long, file.path(outdir, "historical_objective_reference_used.csv"))

period_levels <- c("early", "mid", "late")
solutions <- solutions %>%
  mutate(
    period = factor(period, period_levels, labels = c("Early", "Mid", "Late")),
    climate = factor(climate_pathway, c("rcp45", "rcp85"), c("RCP 4.5", "RCP 8.5")),
    land_use = factor(landuse_name, c("fixed", "bau"), c("Fixed land use", "BAU land use")),
    technology = factor(
      irrigation_technology_name, c("current", "efficient"),
      c("Current irrigation", "Efficient irrigation")
    ),
    nitrate_mkg = nitrate / 1e6,
    irrigation_mm3 = irrigation / 1e6,
    profit_musd = profit / 1e6,
    nitrate_change_pct = 100 * (nitrate / historical$nitrate - 1),
    irrigation_change_pct = 100 * (irrigation / historical$irrigation - 1),
    profit_change_pct = 100 * (profit / historical$profit - 1),
    nitrate_good = historical$nitrate / (historical$nitrate + nitrate),
    irrigation_good = historical$irrigation / (historical$irrigation + irrigation),
    profit_good = profit / (historical$profit + profit)
  )

common_theme <- theme_test(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(), legend.position = "bottom",
    strip.text = element_text(size = 8.5)
  )

p_all <- ggplot(solutions, aes(nitrate_mkg, profit_musd, color = irrigation_mm3)) +
  geom_point(alpha = 0.68, size = 1.25) +
  facet_grid(rows = vars(land_use, technology), cols = vars(climate, period)) +
  scale_color_viridis_c(option = "C", direction = -1) +
  labs(
    title = "Final integrated constrained optimization",
    subtitle = "Crop allocation, fertilizer rate, and irrigation extent are optimized jointly",
    x = "Nitrate load (million kg N/year)", y = "Net return (million USD/year)",
    color = "Irrigation\n(million m3/year)"
  ) + common_theme
ggsave(file.path(outdir, "final_integrated_pareto_all_scenarios.pdf"), p_all,
       width = 18, height = 11, limitsize = FALSE)

efficient <- filter(solutions, irrigation_technology_name == "efficient")
p_efficient <- ggplot(efficient, aes(nitrate_mkg, profit_musd, color = irrigation_mm3)) +
  geom_point(alpha = 0.72, size = 1.35) +
  facet_grid(rows = vars(land_use), cols = vars(climate, period)) +
  scale_color_viridis_c(option = "C", direction = -1) +
  labs(
    title = "Efficient-irrigation Pareto solutions",
    subtitle = "Final integrated decision formulation only",
    x = "Nitrate load (million kg N/year)", y = "Net return (million USD/year)",
    color = "Irrigation\n(million m3/year)"
  ) + common_theme
ggsave(file.path(outdir, "final_integrated_pareto_efficient_focus.pdf"), p_efficient,
       width = 18, height = 7, limitsize = FALSE)

p_historical <- ggplot(
  efficient,
  aes(nitrate_change_pct, profit_change_pct, color = irrigation_change_pct)
) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.35) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.35) +
  geom_point(alpha = 0.72, size = 1.35) +
  facet_grid(rows = vars(land_use), cols = vars(climate, period)) +
  scale_color_viridis_c(option = "C", direction = -1, labels = label_percent(scale = 1)) +
  labs(
    title = "Efficient-irrigation solutions relative to historical conditions",
    subtitle = "All panels use the same 2006–2023 reference; negative is preferred for nitrate/irrigation, positive for NR",
    x = "Nitrate change from historical mean (%)  ← lower is better",
    y = "Net-return change from historical mean (%)  → higher is better",
    color = "Irrigation change\nfrom historical (%)\n(lower is better)"
  ) + common_theme
ggsave(file.path(outdir, "final_integrated_historical_normalized_efficient.pdf"), p_historical,
       width = 18, height = 7, limitsize = FALSE)

context_summary <- solutions %>%
  group_by(climate_pathway, period = as.character(period), landuse_name, irrigation_technology_name) %>%
  summarise(
    solutions = n(),
    nitrate_min = min(nitrate), nitrate_median = median(nitrate), nitrate_max = max(nitrate),
    irrigation_min = min(irrigation), irrigation_median = median(irrigation), irrigation_max = max(irrigation),
    profit_min = min(profit), profit_median = median(profit), profit_max = max(profit),
    .groups = "drop"
  )
write_csv(context_summary, file.path(outdir, "final_frontier_summary_by_context.csv"))

technology_shift <- context_summary %>%
  pivot_wider(
    names_from = irrigation_technology_name,
    values_from = c(nitrate_min, nitrate_median, nitrate_max, irrigation_min,
                    irrigation_median, irrigation_max, profit_min, profit_median, profit_max)
  ) %>%
  mutate(
    irrigation_median_change_pct = 100 * (irrigation_median_efficient / irrigation_median_current - 1),
    nitrate_median_change_pct = 100 * (nitrate_median_efficient / nitrate_median_current - 1),
    profit_median_change_pct = 100 * (profit_median_efficient / profit_median_current - 1)
  )
write_csv(technology_shift, file.path(outdir, "current_vs_efficient_frontier_shift.csv"))

p_shift <- technology_shift %>%
  mutate(
    period = factor(period, c("Early", "Mid", "Late")),
    climate = factor(climate_pathway, c("rcp45", "rcp85"), c("RCP 4.5", "RCP 8.5")),
    land_use = factor(landuse_name, c("fixed", "bau"), c("Fixed land use", "BAU land use"))
  ) %>%
  ggplot(aes(period, -irrigation_median_change_pct, fill = land_use)) +
  geom_hline(yintercept = 15, linetype = 2, color = "grey35") +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(vars(climate)) +
  scale_fill_manual(values = c("Fixed land use" = "#5B8E7D", "BAU land use" = "#D17A22")) +
  labs(
    title = "Irrigation reduction on the optimized efficient frontier",
    subtitle = "Median-front comparison; dashed line is the modeled 15% technology saving holding decisions fixed",
    x = NULL, y = "Reduction versus current optimized frontier median (%)", fill = "Land use"
  ) + common_theme
ggsave(file.path(outdir, "current_vs_efficient_irrigation_reduction.pdf"), p_shift,
       width = 10, height = 6.5)

# Ternary composition of three baseline-relative, directionally oriented scores.
# Each score equals 0.5 at the historical reference; shares show trade-off composition,
# not absolute percent change.
ternary <- efficient %>%
  mutate(
    score_sum = nitrate_good + profit_good + irrigation_good,
    share_nitrate = nitrate_good / score_sum,
    share_profit = profit_good / score_sum,
    share_irrigation = irrigation_good / score_sum,
    ternary_x = share_profit + 0.5 * share_irrigation,
    ternary_y = sqrt(3) / 2 * share_irrigation
  )
triangle <- tibble(x = c(0, 1, 0.5, 0), y = c(0, 0, sqrt(3) / 2, 0))
p_ternary <- ggplot(ternary, aes(ternary_x, ternary_y, color = period)) +
  geom_path(data = triangle, aes(x, y), inherit.aes = FALSE, linewidth = 0.45) +
  geom_point(alpha = 0.55, size = 1.05) +
  annotate("text", x = 0, y = -0.035, label = "Low nitrate", hjust = 0, size = 3) +
  annotate("text", x = 1, y = -0.035, label = "High NR", hjust = 1, size = 3) +
  annotate("text", x = 0.5, y = sqrt(3) / 2 + 0.035, label = "Low irrigation", size = 3) +
  facet_grid(rows = vars(land_use), cols = vars(climate)) +
  coord_equal(xlim = c(-0.03, 1.03), ylim = c(-0.06, 0.93), clip = "off") +
  scale_color_manual(values = c(Early = "#1B9E77", Mid = "#D95F02", Late = "#7570B3")) +
  labs(
    title = "Efficient-irrigation objective balance",
    subtitle = "Ternary shares use common historical-reference scores; position shows balance, not total performance",
    color = "Period"
  ) + theme_test(base_size = 10) +
  theme(
    panel.grid = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", strip.text = element_text(size = 9)
  )
ggsave(file.path(outdir, "final_integrated_ternary_efficient.pdf"), p_ternary,
       width = 11, height = 8.5)

# Select one balanced solution per context using the geometric mean of the
# three historical-reference scores, then show score differences from 0.5.
balanced <- efficient %>%
  mutate(balance_score = (nitrate_good * profit_good * irrigation_good)^(1 / 3)) %>%
  group_by(climate, period, land_use) %>%
  slice_max(balance_score, n = 1, with_ties = FALSE) %>%
  ungroup()
write_csv(balanced, file.path(outdir, "balanced_efficient_solution_by_context.csv"))

radial <- balanced %>%
  select(climate, period, land_use, nitrate_good, profit_good, irrigation_good) %>%
  pivot_longer(ends_with("_good"), names_to = "objective", values_to = "score") %>%
  mutate(
    objective = factor(
      objective,
      c("nitrate_good", "profit_good", "irrigation_good"),
      c("Low nitrate", "High NR", "Low irrigation")
    ),
    angle = as.numeric(objective),
    delta_from_historical = score - 0.5
  )
p_radial <- ggplot(radial, aes(angle, delta_from_historical, color = period, group = period)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_polygon(fill = NA, linewidth = 0.65) +
  geom_point(size = 1.5) +
  facet_grid(rows = vars(land_use), cols = vars(climate)) +
  coord_polar() +
  scale_x_continuous(breaks = 1:3, labels = levels(radial$objective), limits = c(0.5, 3.5)) +
  scale_color_manual(values = c(Early = "#1B9E77", Mid = "#D95F02", Late = "#7570B3")) +
  labs(
    title = "Balanced efficient-irrigation solutions relative to history",
    subtitle = "Positive radial values outperform the common historical reference; negative values underperform",
    x = NULL, y = "Score difference from historical reference", color = "Period"
  ) + common_theme
ggsave(file.path(outdir, "final_integrated_radial_balanced_efficient.pdf"), p_radial,
       width = 11, height = 8.5)

# -------------------------------------------------------------------------
# Legacy-style figures matching the original exploratory visualization.
# These are projections/summaries of the same three-objective Pareto sets and
# are retained alongside, not substituted for, the nitrate-vs-NR frontiers.
# -------------------------------------------------------------------------
climate_colors <- c("RCP 4.5" = "steelblue", "RCP 8.5" = "maroon")
land_shapes <- c("Fixed land use" = 21, "BAU land use" = 24)
period_alpha <- c("Early" = 0.45, "Mid" = 0.7, "Late" = 1)

make_legacy_pareto <- function(data, technology_label, filename) {
  p <- ggplot(
    data,
    aes(
      profit_musd, irrigation_mm3, size = nitrate_mkg,
      color = climate, fill = climate, shape = land_use, alpha = period
    )
  ) +
    geom_point(stroke = 0.55) +
    scale_size_continuous(
      name = expression("Nitrate export"~("million kg N yr"^{-1})),
      range = c(1.5, 6)
    ) +
    scale_color_manual(name = "Climate pathway", values = climate_colors) +
    scale_fill_manual(values = climate_colors, guide = "none") +
    scale_shape_manual(name = "Land-use scenario", values = land_shapes) +
    scale_alpha_manual(
      name = "Period", values = period_alpha,
      labels = c(
        Early = "Early (2025–2049)", Mid = "Mid (2050–2074)",
        Late = "Late (2075–2099)"
      )
    ) +
    labs(
      title = paste("Pareto frontiers by scenario —", technology_label),
      subtitle = "Preferred outcomes move toward higher NR, lower irrigation, and smaller point size",
      x = expression("Annual net return (million USD yr"^{-1}*")"),
      y = expression("Irrigation water use (million m"^3*" yr"^{-1}*")")
    ) +
    guides(
      size = guide_legend(order = 1), color = guide_legend(order = 2),
      shape = guide_legend(order = 3), alpha = guide_legend(order = 4)
    ) +
    theme_test(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 12),
      legend.position = "right", legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  ggsave(file.path(outdir, filename), p, width = 13, height = 9)
}

make_legacy_pareto(efficient, "efficient irrigation", "legacy_style_pareto_efficient.pdf")
make_legacy_pareto(
  filter(solutions, irrigation_technology_name == "current"),
  "current irrigation", "legacy_style_pareto_current.pdf"
)

make_legacy_ternary_density <- function(data, technology_label, filename) {
  ranges <- data %>% summarise(
    n_min = min(nitrate), n_max = max(nitrate),
    p_min = min(profit), p_max = max(profit),
    i_min = min(irrigation), i_max = max(irrigation)
  )
  td <- data %>% mutate(
    sN = 1 - (nitrate - ranges$n_min) / (ranges$n_max - ranges$n_min),
    sP = (profit - ranges$p_min) / (ranges$p_max - ranges$p_min),
    sI = 1 - (irrigation - ranges$i_min) / (ranges$i_max - ranges$i_min),
    total = sN + sP + sI,
    pN = sN / total, pP = sP / total, pI = sI / total,
    ternary_x = pP + 0.5 * pN,
    ternary_y = sqrt(3) / 2 * pN
  ) %>% filter(is.finite(ternary_x), is.finite(ternary_y))

  p <- ggplot() +
    stat_density_2d(
      data = td,
      aes(ternary_x, ternary_y, fill = after_stat(level)),
      geom = "polygon", contour = TRUE, bins = 6, alpha = 0.65,
      color = NA, h = c(0.06, 0.06)
    ) +
    scale_fill_gradient(low = "grey80", high = "grey20", name = "Solution\ndensity") +
    geom_path(data = triangle, aes(x, y), linewidth = 0.8) +
    geom_point(
      data = td, aes(ternary_x, ternary_y, color = climate),
      size = 2.1, alpha = 0.72
    ) +
    annotate("text", x = 0.5, y = sqrt(3) / 2 + 0.06, label = "Low nitrate", size = 4) +
    annotate("text", x = -0.03, y = -0.04, label = "Low irrigation", hjust = 0, size = 4) +
    annotate("text", x = 1.03, y = -0.04, label = "High NR", hjust = 1, size = 4) +
    facet_grid(rows = vars(period), cols = vars(land_use)) +
    coord_equal(
      xlim = c(-0.08, 1.08), ylim = c(-0.08, sqrt(3) / 2 + 0.1),
      expand = FALSE
    ) +
    scale_color_manual(values = climate_colors, name = "Climate pathway") +
    labs(
      title = paste("Pareto frontier density in normalized tradeoff space —", technology_label),
      x = NULL, y = NULL
    ) +
    theme_test(base_size = 14) +
    theme(
      panel.grid = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank(), plot.title.position = "plot",
      legend.position = "right"
    )
  ggsave(file.path(outdir, filename), p, width = 13.5, height = 13)
}

make_legacy_ternary_density(
  efficient, "efficient irrigation", "legacy_style_ternary_density_efficient.pdf"
)
make_legacy_ternary_density(
  filter(solutions, irrigation_technology_name == "current"),
  "current irrigation", "legacy_style_ternary_density_current.pdf"
)

write_csv(
  solutions %>% select(
    scenario_name, seed, solution_id, climate_pathway, period, landuse_name,
    irrigation_technology_name, nitrate, irrigation, profit,
    nitrate_change_pct, irrigation_change_pct, profit_change_pct,
    nitrate_good, irrigation_good, profit_good,
    Corn, Soybeans, Sorghum, Wheat, fert_factor, irrig_frac_factor
  ),
  file.path(outdir, "final_integrated_solutions_normalized.csv")
)

message("Wrote final integrated optimization visualizations to: ", normalizePath(outdir))
