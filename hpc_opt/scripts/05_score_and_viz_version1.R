# hpc_opt/scripts/04_score_and_viz.R
suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source(here("hpc_opt","R","score_helpers.R"))

fig_dir <- here("hpc_opt","figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 0) Read feasible space + scenario outputs
# ---------------------------

# A) Global feasible space (recommended): union of LHS clouds representing app bounds.
# If you don't have this yet, point it to a big baseline LHS file for now.
feasible_path <- here("hpc_opt","outputs","feasible_space.csv")

# B) Scenario outputs (optional, if you have them). If not, you can just plot feasible space.
# Example: a folder where each scenario writes results as CSV:
scenario_glob <- here("hpc_opt","outputs","scenarios","*.csv")

has_feasible <- file.exists(feasible_path)
if (!has_feasible) stop("Missing feasible_space.csv at: ", feasible_path)

feasible <- readr::read_csv(feasible_path, show_col_types = FALSE) %>%
  mutate(Scenario = coalesce(Scenario, "feasible_space"))

scenario_files <- Sys.glob(scenario_glob)
scenarios <- if (length(scenario_files) > 0) {
  bind_rows(lapply(scenario_files, readr::read_csv, show_col_types = FALSE))
} else {
  tibble::tibble()
}


# --- baseline point (single reference row) ------------------------------
baseline_path <- here("hpc_opt","outputs","baseline_point.csv")
stopifnot(file.exists(baseline_path))

baseline_row <- readr::read_csv(baseline_path, show_col_types = FALSE) %>%
  dplyr::mutate(Scenario = "baseline")

# Safety: keep only the objective cols you need (+ Scenario)
# (adjust names here if your csv uses different ones)
baseline_row <- baseline_row %>%
  dplyr::select(dplyr::any_of(c("Scenario","Nitrate_kgyr","NegProfit_usd","Irr_m3",
                                "Corn","Soybeans","Sorghum","Wheat",
                                "cult_area","fert_eff","irr_area","irr_eff")))

# Combine for scoring/plotting
all_df <- bind_rows(feasible, scenarios)

all_df <- dplyr::bind_rows(feasible, baseline_row)
stopifnot(any(all_df$Scenario == "baseline"))


# Require these cols (rename here if your outputs use different names)
req <- c("Scenario","Nitrate_kgyr","NegProfit_usd","Irr_m3")
missing <- setdiff(req, names(all_df))
if (length(missing)) stop("Missing required columns in combined data: ", paste(missing, collapse=", "))

# ---------------------------
# 1) Build global scorers from feasible space ONLY
#    (this ensures every scenario is scored against the same app-feasible domain)
# ---------------------------
ecdfs <- build_global_ecdfs(feasible, cols = c("Nitrate_kgyr","NegProfit_usd","Irr_m3"))

scored <- all_df %>%
  score_objectives(ecdfs) %>%
  to_ternary_shares()

# ---------------------------
# 2) Identify baseline row (the "current conditions" point)
# ---------------------------
# You can tag baseline when you generate it (recommended).
# For now we’ll guess: Scenario == "baseline" and (optionally) run_id contains "baseline".
baseline_candidates <- scored %>%
  filter(Scenario == "baseline") %>%
  arrange(abs(Corn - Corn[1])) # harmless; just makes stable ordering if duplicates

if (nrow(baseline_candidates) == 0) {
  message("No Scenario=='baseline' found. Radial deltas will be skipped.")
  baseline_row <- NULL
} else {
  baseline_row <- baseline_candidates[1, ]  # pick one baseline point (or define a rule)
}

# ---------------------------
# 3) TERNARY (ggplot-only)
# ---------------------------
xy <- ternary_to_xy(scored$pN, scored$pP, scored$pI)
ternary_df <- scored %>% mutate(x = xy$x, y = xy$y)

tri <- ternary_axes_df()

p_tern <- ggplot() +
  geom_path(data = tri, aes(x, y), linewidth = 0.9) +
  geom_point(data = ternary_df,
             aes(x, y, color = Scenario),
             alpha = 0.45, size = 1.8) +
  # baseline point (if present)
  {if (!is.null(baseline_row)) {
    xy_b <- ternary_to_xy(baseline_row$pN, baseline_row$pP, baseline_row$pI)
    geom_point(aes(x = xy_b$x, y = xy_b$y),
               inherit.aes = FALSE,
               shape = 21, fill = "white", color = "black",
               size = 4, stroke = 1.1)
  }} +
  # axis labels (corners)
  annotate("text", x = 0.5, y = sqrt(3)/2 + 0.04, label = "Nitrate (better ↑)", size = 4.2) +
  annotate("text", x = -0.02, y = -0.03, label = "Profit (better ↑)", hjust = 0, size = 4.2) +
  annotate("text", x = 1.02, y = -0.03, label = "Irrigation (better ↑)", hjust = 1, size = 4.2) +
  coord_equal(xlim = c(-0.05, 1.05), ylim = c(-0.05, sqrt(3)/2 + 0.08), expand = FALSE) +
  labs(
    title = "Tradeoff composition in global feasible space (ternary shares)",
    subtitle = "Scores are global-percentile based (relative to app-feasible LHS space); points show how each solution allocates 'goodness' across objectives.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

#ggsave(file.path(fig_dir, "ternary_tradeoff_space.png"), p_tern, width = 9, height = 7, dpi = 300)
print(p_tern)

# ---------------------------
# 4) RADIAL (relative to baseline)
# ---------------------------
if (!is.null(baseline_row)) {
  scored2 <- scored %>% add_baseline_deltas(baseline_row)
  
  rad_long <- radial_long(scored2, id_cols = c("Scenario","run_id","sol_id"))
  
  # A simple, stakeholder-friendly view: distribution of deltas by scenario
  p_rad_violin <- ggplot(rad_long, aes(x = metric, y = delta, fill = metric)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_violin(alpha = 0.35, width = 0.9, color = NA) +
    geom_boxplot(width = 0.18, outlier.alpha = 0.15) +
    facet_wrap(~Scenario, ncol = 1) +
    labs(
      title = "Scenario performance relative to baseline (global-percentile deltas)",
      subtitle = "Above 0 = better than baseline; below 0 = worse. Scores are computed vs the app-feasible space.",
      x = NULL, y = "Δ score (0–1 scale)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title.position = "plot", legend.position = "none")
  
  ggsave(file.path(fig_dir, "radial_deltas_violin.png"), p_rad_violin, width = 9, height = 9, dpi = 300)
  print(p_rad_violin)
  
  # Optional: a “radial” spider-style plot for selected points (e.g., knee/corners)
  # This expects you've already created a 'special' df with a 'type' column.
  # If you want, we’ll wire this to your knee/corners table next.
}
