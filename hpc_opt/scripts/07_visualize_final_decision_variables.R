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

input <- normalizePath(
  get_arg(
    "--input",
    "hpc_opt/local_exports/analysis-clippedcdf-constrained/final-integrated-theme-test/final_integrated_solutions_normalized.csv"
  ),
  mustWork = TRUE
)
outdir <- get_arg("--output", file.path(dirname(input), "decision-variables"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

x <- read_csv(input, show_col_types = FALSE) %>%
  mutate(
    period = factor(period, c("Early", "Mid", "Late")),
    climate = factor(climate_pathway, c("rcp45", "rcp85"), c("RCP 4.5", "RCP 8.5")),
    land_use = factor(landuse_name, c("fixed", "bau"), c("Fixed land use", "BAU expansion")),
    technology = factor(
      irrigation_technology_name, c("current", "efficient"),
      c("Current irrigation", "Efficient irrigation")
    ),
    fertilizer_reduction_pct = 100 * (1 - fert_factor),
    irrigation_extent_change_pct = 100 * (irrig_frac_factor - 1),
    balance_score = (nitrate_good * profit_good * irrigation_good)^(1 / 3)
  )

groups <- c("climate_pathway", "period", "landuse_name")
scale01 <- function(value, lower, upper) {
  if (upper > lower) (value - lower) / (upper - lower) else rep(0, length(value))
}
efficient <- x %>%
  filter(irrigation_technology_name == "efficient") %>%
  group_by(across(all_of(groups))) %>%
  mutate(
    nitrate_loss = scale01(nitrate, min(nitrate), max(nitrate)),
    profit_loss = scale01(max(profit) - profit, 0, max(profit) - min(profit)),
    irrigation_loss = scale01(irrigation, min(irrigation), max(irrigation)),
    ideal_distance = sqrt(nitrate_loss^2 + profit_loss^2 + irrigation_loss^2)
  ) %>%
  ungroup()

select_archetype <- function(data, objective, direction, label) {
  grouped <- group_by(data, across(all_of(groups)))
  selected <- if (direction == "max") {
    slice_max(grouped, order_by = .data[[objective]], n = 1, with_ties = FALSE)
  } else {
    slice_min(grouped, order_by = .data[[objective]], n = 1, with_ties = FALSE)
  }
  selected %>% ungroup() %>% mutate(archetype = label)
}

representative <- bind_rows(
  select_archetype(efficient, "profit", "max", "Maximum NR"),
  select_archetype(efficient, "nitrate", "min", "Minimum nitrate"),
  select_archetype(efficient, "irrigation", "min", "Minimum irrigation"),
  select_archetype(efficient, "ideal_distance", "min", "Closest to ideal")
) %>%
  mutate(
    archetype = factor(
      archetype,
      c("Maximum NR", "Minimum nitrate", "Minimum irrigation", "Closest to ideal")
    )
  )

write_csv(representative, file.path(outdir, "representative_efficient_solutions.csv"))

crop_colors <- c(
  Corn = "#F2C500", Soybeans = "#548B54",
  Sorghum = "darkorange", Wheat = "#8B7355"
)
archetype_colors <- c(
  "Maximum NR" = "#0072B2", "Minimum nitrate" = "#009E73",
  "Minimum irrigation" = "#D55E00", "Closest to ideal" = "#CC79A7"
)
technology_colors <- c(
  "Current irrigation" = "#7A7A7A", "Efficient irrigation" = "#0072B2"
)

base_theme <- theme_test(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(), legend.position = "bottom",
    strip.text = element_text(size = 8.5), axis.text.x = element_text(angle = 25, hjust = 1)
  )

crop_rep <- representative %>%
  pivot_longer(c(Corn, Soybeans, Sorghum, Wheat), names_to = "crop", values_to = "share") %>%
  mutate(crop = factor(crop, c("Corn", "Soybeans", "Sorghum", "Wheat")))

p_crop_rep <- ggplot(crop_rep, aes(archetype, share, fill = crop)) +
  geom_col(width = 0.76) +
  facet_grid(rows = vars(climate, period), cols = vars(land_use)) +
  scale_fill_manual(values = crop_colors, name = "Crop") +
  scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Crop composition of representative efficient-irrigation solutions",
    subtitle = "Final integrated phase; closest-to-ideal minimizes normalized three-objective distance within each context",
    x = NULL, y = "Share of cultivated area"
  ) + base_theme
ggsave(file.path(outdir, "representative_crop_composition_efficient.pdf"), p_crop_rep,
       width = 13, height = 17, limitsize = FALSE)

management_rep <- representative %>%
  select(climate, period, land_use, archetype, fertilizer_reduction_pct, irrigation_extent_change_pct) %>%
  pivot_longer(
    c(fertilizer_reduction_pct, irrigation_extent_change_pct),
    names_to = "lever", values_to = "change_pct"
  ) %>%
  mutate(
    lever = factor(
      lever,
      c("fertilizer_reduction_pct", "irrigation_extent_change_pct"),
      c("Fertilizer reduction from baseline", "Irrigated-extent change from baseline")
    )
  )

p_management_rep <- ggplot(management_rep, aes(archetype, change_pct, fill = archetype)) +
  geom_col(width = 0.72) +
  facet_grid(rows = vars(lever, land_use), cols = vars(climate, period), scales = "free_y") +
  scale_fill_manual(values = archetype_colors, guide = "none") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Management decisions in representative efficient-irrigation solutions",
    subtitle = "Positive fertilizer values mean reduced application; positive irrigation values mean expanded irrigated extent",
    x = NULL, y = "Change from baseline (%)"
  ) + base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5))
ggsave(file.path(outdir, "representative_management_levers_efficient.pdf"), p_management_rep,
       width = 18, height = 10, limitsize = FALSE)

crop_all <- efficient %>%
  pivot_longer(c(Corn, Soybeans, Sorghum, Wheat), names_to = "crop", values_to = "share") %>%
  mutate(crop = factor(crop, c("Corn", "Soybeans", "Sorghum", "Wheat")))

p_crop_all <- ggplot(crop_all, aes(crop, share, fill = crop)) +
  geom_violin(scale = "width", trim = TRUE, alpha = 0.72, linewidth = 0.3) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", linewidth = 0.3) +
  facet_grid(rows = vars(land_use), cols = vars(climate, period)) +
  scale_fill_manual(values = crop_colors, guide = "none") +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Crop decisions across complete efficient-irrigation Pareto sets",
    subtitle = "Violin width shows solution density; interior boxes show median and interquartile range",
    x = NULL, y = "Share of cultivated area"
  ) + base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5))
ggsave(file.path(outdir, "full_frontier_crop_distributions_efficient.pdf"), p_crop_all,
       width = 18, height = 8, limitsize = FALSE)

management_all <- efficient %>%
  select(climate, period, land_use, fertilizer_reduction_pct, irrigation_extent_change_pct) %>%
  pivot_longer(
    c(fertilizer_reduction_pct, irrigation_extent_change_pct),
    names_to = "lever", values_to = "change_pct"
  ) %>%
  mutate(
    lever = factor(
      lever,
      c("fertilizer_reduction_pct", "irrigation_extent_change_pct"),
      c("Fertilizer reduction", "Irrigated-extent expansion")
    )
  )

p_management_all <- ggplot(management_all, aes(period, change_pct, fill = land_use)) +
  geom_violin(position = position_dodge(width = 0.82), scale = "width", trim = TRUE, alpha = 0.65) +
  geom_boxplot(
    position = position_dodge(width = 0.82), width = 0.16,
    outlier.shape = NA, alpha = 0.85, linewidth = 0.3
  ) +
  facet_grid(rows = vars(lever), cols = vars(climate), scales = "free_y") +
  scale_fill_manual(values = c("Fixed land use" = "#56B4E9", "BAU expansion" = "#E69F00")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Management decisions across complete efficient-irrigation Pareto sets",
    subtitle = "Distributions reveal whether management levers converge or remain flexible across optimal solutions",
    x = NULL, y = "Change from baseline (%)", fill = "Land use"
  ) + base_theme + theme(axis.text.x = element_text(angle = 0))
ggsave(file.path(outdir, "full_frontier_management_distributions_efficient.pdf"), p_management_all,
       width = 11, height = 8, limitsize = FALSE)

technology_management <- x %>%
  select(climate, period, land_use, technology, fertilizer_reduction_pct, irrigation_extent_change_pct) %>%
  pivot_longer(
    c(fertilizer_reduction_pct, irrigation_extent_change_pct),
    names_to = "lever", values_to = "change_pct"
  ) %>%
  mutate(
    lever = factor(
      lever,
      c("fertilizer_reduction_pct", "irrigation_extent_change_pct"),
      c("Fertilizer reduction", "Irrigated-extent expansion")
    )
  )

p_technology <- ggplot(technology_management, aes(period, change_pct, color = technology, fill = technology)) +
  geom_boxplot(
    position = position_dodge(width = 0.76), width = 0.62,
    alpha = 0.22, outlier.shape = NA, linewidth = 0.45
  ) +
  facet_grid(rows = vars(lever, land_use), cols = vars(climate), scales = "free_y") +
  scale_color_manual(values = technology_colors) +
  scale_fill_manual(values = technology_colors) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Do optimal management decisions shift with irrigation technology?",
    subtitle = "Current and efficient technology scenarios are optimized independently",
    x = NULL, y = "Change from baseline (%)", color = NULL, fill = NULL
  ) + base_theme + theme(axis.text.x = element_text(angle = 0))
ggsave(file.path(outdir, "management_decisions_current_vs_efficient.pdf"), p_technology,
       width = 11, height = 10, limitsize = FALSE)

decision_summary <- x %>%
  group_by(climate_pathway, period, landuse_name, irrigation_technology_name) %>%
  summarise(
    across(
      c(Corn, Soybeans, Sorghum, Wheat, fertilizer_reduction_pct, irrigation_extent_change_pct),
      list(min = min, q25 = ~ quantile(.x, 0.25), median = median, q75 = ~ quantile(.x, 0.75), max = max),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
write_csv(decision_summary, file.path(outdir, "decision_variable_summary_by_context.csv"))

message("Wrote decision-variable exploration to: ", normalizePath(outdir))
