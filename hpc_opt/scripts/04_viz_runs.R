# ==========================================
# 04_viz_runs.R
# Read + summarize + visualize optimization runs
# ==========================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

# ---- optional packages (nice-to-have) ----
has_ggtern <- requireNamespace("ggtern", quietly = TRUE)
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

# ------------------------------------------
# Palettes (reuse your familiar ones)
# ------------------------------------------
pal_landcover <- c(
  "Grass/Pasture/Hay" = "#C1FFC1",
  "Cultivated"        = "#8B8B00",
  "Developed"         = "darkgrey"
)

pal_crops <- c(
  "Corn"     = "#EEC900",
  "Soybeans" = "#548B54",
  "Wheat"    = "#8B7355",
  "Sorghum"  = "darkorange"
)

scale_fill_landcover <- function(...) ggplot2::scale_fill_manual(values = pal_landcover, ...)
scale_color_landcover <- function(...) ggplot2::scale_color_manual(values = pal_landcover, ...)
scale_fill_crops <- function(...) ggplot2::scale_fill_manual(values = pal_crops, ...)
scale_color_crops <- function(...) ggplot2::scale_color_manual(values = pal_crops, ...)

# ------------------------------------------
# Helpers
# ------------------------------------------

# Nondominated filter (minimize all columns in obj_cols)
nondominated <- function(df, obj_cols) {
  X <- as.matrix(df[, obj_cols, drop = FALSE])
  keep <- rep(TRUE, nrow(X))
  for (i in seq_len(nrow(X))) {
    if (!keep[i]) next
    # j dominates i if all <= and at least one <
    dom_i <- apply(X, 1, function(r) all(r <= X[i, ]) && any(r < X[i, ]))
    dom_i[i] <- FALSE
    if (any(dom_i)) keep[i] <- FALSE
  }
  df[keep, , drop = FALSE]
}

# Knee on a 2D frontier: maximize distance to chord
knee_by_max_distance <- function(df, xcol, ycol) {
  d <- df %>%
    mutate(.x = as.numeric(.data[[xcol]]), .y = as.numeric(.data[[ycol]])) %>%
    filter(is.finite(.x), is.finite(.y)) %>%
    arrange(.x, desc(.y))
  if (nrow(d) < 3) return(d[0, , drop = FALSE])
  
  x1 <- d$.x[1]; y1 <- d$.y[1]
  x2 <- d$.x[nrow(d)]; y2 <- d$.y[nrow(d)]
  num <- abs((y2 - y1) * d$.x - (x2 - x1) * d$.y + x2*y1 - y2*x1)
  den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
  d$.knee_dist <- if (den > 0) num / den else 0
  d %>% slice(which.max(.knee_dist)) %>% select(-.x, -.y, -.knee_dist)
}

# Convert your solution crop shares (Corn,Soy,Sorghum,Wheat) -> long table
crop_long <- function(df) {
  df %>%
    select(run_id, Scenario, Corn, Soybeans, Sorghum, Wheat) %>%
    pivot_longer(cols = c(Corn, Soybeans, Sorghum, Wheat),
                 names_to = "Crop", values_to = "Share") %>%
    mutate(Crop = factor(Crop, levels = c("Corn","Soybeans","Wheat","Sorghum")))
}

# Landcover composition implied by policy (cult_area_factor) + baseline LU by year
# Mirrors eval logic: Cult scaled then clamped to non-developed; grass is remainder.
landcover_from_policy <- function(lu_baseline, years_vec, cult_area_factor = 1) {
  lu <- lu_baseline %>%
    filter(Year %in% years_vec) %>%
    mutate(
      avail_nondeveloped_m2 = LC_total - LC_dev_base,
      Cult_m2 = pmax(0, pmin(avail_nondeveloped_m2, LC_cult_base * cult_area_factor)),
      Grass_m2 = avail_nondeveloped_m2 - Cult_m2,
      Dev_m2 = LC_dev_base
    ) %>%
    summarise(
      Cult_m2 = mean(Cult_m2, na.rm = TRUE),
      Grass_m2 = mean(Grass_m2, na.rm = TRUE),
      Dev_m2 = mean(Dev_m2, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "Class", values_to = "area_m2") %>%
    mutate(
      Class = recode(Class,
                     Cult_m2 = "Cultivated",
                     Grass_m2 = "Grass/Pasture/Hay",
                     Dev_m2 = "Developed"),
      frac = area_m2 / sum(area_m2, na.rm = TRUE)
    )
  lu
}

# ------------------------------------------
# Paths
# ------------------------------------------
runs_root <- here("hpc_opt", "outputs", "runs")
fig_dir   <- here("hpc_opt", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------
# 1) Load baseline/current point from historical integration
# ------------------------------------------
df_int_basin <- read_csv(here("hpc_opt","outputs","integration","int_basin_annual.csv"),
                         show_col_types = FALSE)

years_vec <- intersect(2006:2023, df_int_basin$Year)

baseline_point <- df_int_basin %>%
  filter(Year %in% years_vec) %>%
  summarise(
    Scenario = "Current/Baseline",
    Nitrate_kgyr = mean(NitrateFluxPredicted_kg, na.rm = TRUE),
    Profit_usd   = mean(NetReturn_total_usd, na.rm = TRUE),
    Irr_m3       = mean(Irrigation_total_m3, na.rm = TRUE)
  ) %>%
  mutate(
    NegProfit_usd = -Profit_usd
  )

# ------------------------------------------
# 2) Read all runs (*.rds) across scenarios
# ------------------------------------------
rds_files <- list.files(runs_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
stopifnot(length(rds_files) > 0)


read_one_run <- function(path) {
  res <- readRDS(path)
  Scenario <- basename(dirname(path))
  run_id <- paste0(Scenario, "::", tools::file_path_sans_ext(basename(path)))
  
  obj <- as.data.frame(res$value)
  par <- as.data.frame(res$par)
  colnames(obj) <- c("Nitrate_kgyr","NegProfit_usd","Irr_m3")
  colnames(par) <- c("Corn","Soybeans","Sorghum")
  
  bind_cols(obj, par) %>%
    mutate(
      sol_id = row_number(),                 # <- UNIQUE within run
      Wheat = 1 - (Corn + Soybeans + Sorghum),
      Profit_usd = -NegProfit_usd,
      Scenario = Scenario,
      run_id = run_id,
      sol_key = paste0(run_id, "::", sol_id) # <- globally unique
    )
}

solutions <- bind_rows(lapply(rds_files, read_one_run)) %>%
  # guard against tiny numeric drift
  mutate(
    across(c(Corn, Soybeans, Sorghum, Wheat), ~pmax(0, .x)),
    s = Corn + Soybeans + Sorghum + Wheat,
    across(c(Corn, Soybeans, Sorghum, Wheat), ~ .x / ifelse(s > 0, s, 1))
  ) %>%
  select(-s)

# ------------------------------------------
# 3) Frontier per scenario (nondominated)
# ------------------------------------------
frontiers <- solutions %>%
  group_by(Scenario) %>%
  group_modify(~ nondominated(.x, obj_cols = c("Nitrate_kgyr","NegProfit_usd","Irr_m3"))) %>%
  ungroup()

# ------------------------------------------
# 4) Identify corners + knee per scenario
# ------------------------------------------
# corners (min N, max Profit, min Irr) on frontier
corners <- frontiers %>%
  group_by(Scenario) %>%
  summarise(
    sol_key_minN = sol_key[which.min(Nitrate_kgyr)],
    sol_key_maxP = sol_key[which.max(Profit_usd)],
    sol_key_minI = sol_key[which.min(Irr_m3)],
    .groups = "drop"
  ) %>%
  pivot_longer(-Scenario, names_to = "type", values_to = "sol_key") %>%
  left_join(frontiers %>% select(Scenario, sol_key, everything()),
            by = c("Scenario","sol_key")) %>%
  mutate(type = recode(type,
                       sol_key_minN = "Corner: min nitrate",
                       sol_key_maxP = "Corner: max profit",
                       sol_key_minI = "Corner: min irrigation"))

# knee (use 2D plane: N vs Profit, on the frontier)
knees <- frontiers %>%
  group_by(Scenario) %>%
  group_modify(~ knee_by_max_distance(.x, xcol = "Nitrate_kgyr", ycol = "Profit_usd")) %>%
  ungroup() %>%
  mutate(type = "Knee")

# collect “special points”
special_pts <- bind_rows(
  corners %>% select(Scenario, type, sol_key, Corn, Soybeans, Sorghum, Wheat),
  knees %>%
    mutate(type = "Knee") %>%
    select(Scenario, type, sol_key, Corn, Soybeans, Sorghum, Wheat)
)

# ------------------------------------------
# 5) Plot A: Pareto (N vs Profit), color = Irr
# ------------------------------------------
p_np <- ggplot(frontiers, aes(Nitrate_kgyr, NegProfit_usd, color = Irr_m3)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_point(data = special_pts, aes(Nitrate_kgyr, NegProfit_usd),
             inherit.aes = FALSE, shape = 21, fill = "red", color = "black",
             size = 2.6, stroke = 0.6, alpha = 0.9) +
  facet_wrap(~Scenario, scales = "free") +
  scale_color_viridis_c(trans = "log10", name = "Irrigation (m³/yr)") +
  labs(title = "Pareto frontiers by scenario: Nitrate vs Profit (color = Irrigation)",
       x = "Mean nitrate flux (kg/yr, ↓)",
       y = "Mean net returns ($/yr, ↑)") +
  theme_minimal(base_size = 14) +
  theme(plot.title.position = "plot")

if (has_ggrepel) {
  p_np <- p_np +
    ggrepel::geom_text_repel(
      data = special_pts,
      aes(Nitrate_kgyr, NegProfit_usd, label = type),
      inherit.aes = FALSE,
      size = 3.3, seed = 42, max.overlaps = Inf
    )
}
p_np
#ggsave(file.path(fig_dir, "pareto_N_vs_profit_byScenario.png"), p_np, width = 12, height = 7, dpi = 300)

# ------------------------------------------
# 6) Plot B: Crop composition along frontier + mark knee/corners
# ------------------------------------------
front_long <- crop_long(frontiers)
special_long <- crop_long(special_pts) %>% mutate(is_special = TRUE)
special_crop_long <- special_pts %>%
  tidyr::pivot_longer(
    cols = c(Corn, Soybeans, Sorghum, Wheat),
    names_to = "Crop",
    values_to = "Share"
  )


p_crop_density <- ggplot(front_long, aes(Share, after_stat(density), fill = Crop)) +
  geom_histogram(position = "identity", alpha = 0.55, bins = 25) +
  facet_grid(Scenario ~ Crop) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_crops() +
  labs(title = "Crop-share distributions along each scenario frontier",
       x = "Share of cultivated area", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title.position = "plot")

#ggsave(file.path(fig_dir, "crop_share_density_byScenario.png"),
 #      p_crop_density, width = 12, height = 8, dpi = 300)
print(p_crop_density)

# Crop bars for knee + corners
p_crop_special <- special_crop_long %>%
  ggplot(aes(x = type, y = Share, fill = Crop)) +
  geom_col(width = 0.75) +
  facet_wrap(~Scenario, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
  scale_fill_crops() +
  labs(title = "Crop composition at knee + corner solutions",
       x = NULL, y = "Share of cultivated area") +
  theme_minimal(base_size = 13) +
  theme(plot.title.position = "plot")

#ggsave(file.path(fig_dir, "crop_comp_knee_and_corners.png"),
 #      p_crop_special, width = 10, height = 8, dpi = 300)
print(p_crop_special)

# ------------------------------------------
# 7) Landcover composition (Cult/Grass/Dev) at knee + corners
#     (requires knowing each scenario’s cult_area_factor)
# ------------------------------------------

# For now we hardcode policy levers here.
# Once you move to YAML, this will read per scenario automatically.
policy_tbl <- list(
  baseline = list(cult_area_factor = 1),
  conservation = list(cult_area_factor = 0.85)
)

# Build lu_baseline from basin input (mirrors your eval assumptions)
common_input_basin <- read_csv(here("hpc_opt","outputs","common_inputs_basin.csv"), show_col_types = FALSE)

lu_baseline <- common_input_basin %>%
  filter(Year %in% years_vec) %>%
  transmute(
    Year,
    LC_cult_base = `LandCover_m2_all_cult`,
    LC_grass_base = LandCover_m2_grassPastureHay,
    LC_dev_base = LandCover_m2_developed,
    LC_total = LC_cult_base + LC_grass_base + LC_dev_base
  )

landcover_special <- special_pts %>%
  distinct(Scenario, type) %>%
  rowwise() %>%
  do({
    sc <- .$Scenario
    cult_fac <- policy_tbl[[sc]]$cult_area_factor %||% 1
    lc <- landcover_from_policy(lu_baseline, years_vec, cult_area_factor = cult_fac)
    lc$type <- .$type
    lc$Scenario <- sc
    lc
  }) %>%
  ungroup()

`%||%` <- function(a, b) if (!is.null(a)) a else b

p_lc_special <- landcover_special %>%
  ggplot(aes(x = type, y = frac, fill = Class)) +
  geom_col(width = 0.75) +
  facet_wrap(~Scenario, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
  scale_fill_landcover() +
  labs(title = "Landcover composition implied by scenario (knee + corners)",
       x = NULL, y = "Share of basin area") +
  theme_minimal(base_size = 13) +
  theme(plot.title.position = "plot")

#ggsave(file.path(fig_dir, "landcover_knee_and_corners.png"),
 #      p_lc_special, width = 10, height = 8, dpi = 300)
print(p_lc_special)

# ------------------------------------------
# 8) Ternary plot of the 3 objectives by scenario + baseline/current
# ------------------------------------------
# Use a ternary on normalized objective “weights” (all in minimize form):
#   nitrate (min), neg_profit (min), irrigation (min)
# Convert each solution to proportions that sum to 1.

ternary_df <- frontiers %>%
  mutate(
    O1 = Nitrate_kgyr,
    O2 = NegProfit_usd,
    O3 = Irr_m3,
    # shift to positive so proportions are meaningful
    O1p = O1 - min(O1, na.rm = TRUE) + 1e-9,
    O2p = O2 - min(O2, na.rm = TRUE) + 1e-9,
    O3p = O3 - min(O3, na.rm = TRUE) + 1e-9,
    s = O1p + O2p + O3p,
    pN = O1p / s,
    pP = O2p / s,
    pI = O3p / s
  )

# baseline/current in the same “objective-proportion” space
baseline_obj <- baseline_point %>%
  transmute(
    Scenario = Scenario,
    Nitrate_kgyr = Nitrate_kgyr,
    NegProfit_usd = NegProfit_usd,
    Irr_m3 = Irr_m3
  ) %>%
  mutate(
    # match the same shifts used above (global min from ternary_df)
    O1p = Nitrate_kgyr - min(ternary_df$Nitrate_kgyr, na.rm = TRUE) + 1e-9,
    O2p = NegProfit_usd - min(ternary_df$NegProfit_usd, na.rm = TRUE) + 1e-9,
    O3p = Irr_m3       - min(ternary_df$Irr_m3, na.rm = TRUE) + 1e-9,
    s = O1p + O2p + O3p,
    pN = O1p / s, pP = O2p / s, pI = O3p / s
  )

if (has_ggtern) {
  
  p_tern <- ggtern::ggtern(
    data = ternary_df,
    ggplot2::aes(x = pN, y = pP, z = pI, color = Scenario)
  ) +
    geom_point(alpha = 0.6, size = 2) +  # ggplot2 geom is correct
    geom_point(
      data = baseline_obj,
      ggplot2::aes(x = pN, y = pP, z = pI),
      inherit.aes = FALSE,
      shape = 21,
      fill = "white",
      color = "black",
      size = 4,
      stroke = 1
    ) +
    ggplot2::labs(
      title = "Objective tradeoff space (ternary) by scenario",
      T = "Nitrate (minimize share)",
      L = "Neg Profit (minimize share)",
      R = "Irrigation (minimize share)"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title.position = "plot")
  
  ggsave(file.path(fig_dir, "ternary_objectives_byScenario.png"),
         p_tern, width = 9, height = 7, dpi = 300)
  
  print(p_tern)
  
}
