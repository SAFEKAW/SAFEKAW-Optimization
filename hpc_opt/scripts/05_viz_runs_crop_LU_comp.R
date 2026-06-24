# ==========================================
# 05_viz_runs.R
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
    select(run_id, Scenario_raw, Scenario_label, Corn, Soybeans, Sorghum, Wheat) %>%
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
# ------------------------------------------
# 2) Read all Pareto CSV outputs across scenarios
# ------------------------------------------
pareto_files <- list.files(
  runs_root,
  pattern = "^pareto_front_.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(pareto_files) > 0)

read_one_pareto <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      Scenario = .data$scenario_name,
      run_id = paste0(
        Scenario, "::",
        tools::file_path_sans_ext(basename(path))
      ),
      sol_id = row_number(),
      sol_key = paste0(run_id, "::", sol_id),
      Nitrate_kgyr = nitrate,
      Irr_m3 = irrigation,
      Profit_usd = profit,
      NegProfit_usd = profit_minimized
    )
}

# ------------------------------------------
# Scenario labels + context helper
# ------------------------------------------

scenario_labels <- c(
  baseline = "Historical baseline",
  fixed_ensemble_rcp45_early = "RCP4.5 Early Century Fixed",
  fixed_ensemble_rcp45_mid   = "RCP4.5 Mid Century Fixed",
  fixed_ensemble_rcp45_late  = "RCP4.5 Late Century Fixed",
  fixed_ensemble_rcp85_early = "RCP8.5 Early Century Fixed",
  fixed_ensemble_rcp85_mid   = "RCP8.5 Mid Century Fixed",
  fixed_ensemble_rcp85_late  = "RCP8.5 Late Century Fixed",
  
  bau_ensemble_rcp45_early   = "RCP4.5 Early Century BAU",
  bau_ensemble_rcp45_mid     = "RCP4.5 Mid Century BAU",
  bau_ensemble_rcp45_late    = "RCP4.5 Late Century BAU",
  bau_ensemble_rcp85_early   = "RCP8.5 Early Century BAU",
  bau_ensemble_rcp85_mid     = "RCP8.5 Mid Century BAU",
  bau_ensemble_rcp85_late    = "RCP8.5 Late Century BAU"
)

add_scenario_context <- function(df) {
  df %>%
    mutate(
      Scenario_raw = as.character(Scenario),
      
      landuse_name = stringr::str_extract(Scenario_raw, "^(fixed|bau)"),
      climate_pathway = stringr::str_extract(Scenario_raw, "rcp45|rcp85"),
      period = stringr::str_extract(Scenario_raw, "early|mid|late"),
      
      Scenario_label = dplyr::recode(
        Scenario_raw,
        !!!scenario_labels,
        .default = Scenario_raw
      ),
      
      landuse_name = factor(
        landuse_name,
        levels = c("fixed", "bau"),
        labels = c("Fixed cultivated area", "BAU expansion")
      ),
      climate_pathway = factor(
        climate_pathway,
        levels = c("rcp45", "rcp85"),
        labels = c("RCP 4.5", "RCP 8.5")
      ),
      period = factor(
        period,
        levels = c("early", "mid", "late"),
        labels = c("Early century", "Mid century", "Late century")
      ),
      Scenario_label = factor(
        Scenario_label,
        levels = scenario_labels
      )
    )
}


solutions <- bind_rows(lapply(pareto_files, read_one_pareto)) %>%
  filter(feasible) %>%
  mutate(
    across(c(Corn, Soybeans, Sorghum, Wheat), ~pmax(0, .x)),
    s = Corn + Soybeans + Sorghum + Wheat,
    across(c(Corn, Soybeans, Sorghum, Wheat), ~.x / s)
  ) %>%
  select(-s) %>%
  add_scenario_context()


# ------------------------------------------
# 3) Frontier per scenario (nondominated)
# ------------------------------------------
frontiers <- solutions %>%
  filter(Scenario_raw != "baseline") %>%
  group_by(Scenario_raw) %>%
  group_modify(~ nondominated(.x, obj_cols = c("Nitrate_kgyr", "NegProfit_usd", "Irr_m3"))) %>%
  ungroup() %>%
  add_scenario_context()

frontiers_context <- frontiers

p_profit_irr <- ggplot(
  frontiers,
  aes(
    x = Profit_usd,
    y = Irr_m3,
    size = Nitrate_kgyr,
    fill = climate_pathway,
    color = climate_pathway,
    shape = landuse_name, 
    alpha = period
  )
) +
  geom_point() +
  
  
  scale_size_continuous(
    name = expression("Nitrate export\n(million kg N yr"^{-1}*")"),
    labels = label_number(scale = 1e-6)
  ) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "RCP 4.5" = "steelblue",
      "RCP 8.5" = "maroon"
    )
  ) +
  
  scale_color_manual(
    name = "Climate pathway",
    values = c(
      "RCP 4.5" = "steelblue",
      "RCP 8.5" = "maroon"
    )
  ) +
  
  scale_shape_manual(
    name = "Land-use scenario",
    values = c(
      "Fixed cultivated area" = 21,
      "BAU expansion" = 24
    )
  ) +
  
  scale_alpha_manual(
    name = "Period",
    values = c(
      "Early century" = 0.5,
      "Late century" = 1
    ),
    labels = c(
      "Early century" = "Early (2025–2049)",
      "Late century" = "Late (2075–2099)"
    )
  ) +

  scale_x_continuous(
    labels = label_number(scale = 1e-6)
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6)
  ) +
  labs(
    title = "Pareto frontiers by scenario",
    subtitle = "Preferred outcomes move toward higher profit, lower irrigation, and smaller point size",
    x = expression("Annual profit (million USD yr"^{-1}*")"),
    
    y = expression("Irrigation water use (million m"^3*" yr"^{-1}*")"),
   # shape = "Land-use scenario"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 3),
    fill = "none"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

p_profit_irr



# ------------------------------------------
# 4) Identify corners + knee per scenario
# ------------------------------------------
# corners (min N, max Profit, min Irr) on frontier
corners <- frontiers_context %>%
  group_by(Scenario_label) %>%
  summarise(
    sol_key_minN = sol_key[which.min(Nitrate_kgyr)],
    sol_key_maxP = sol_key[which.max(Profit_usd)],
    sol_key_minI = sol_key[which.min(Irr_m3)],
    .groups = "drop"
  ) %>%
  pivot_longer(-Scenario_label, names_to = "type", values_to = "sol_key") %>%
  left_join(frontiers %>% select(Scenario_label, sol_key, everything()),
            by = c("Scenario_label","sol_key")) %>%
  mutate(type = recode(type,
                       sol_key_minN = "Corner: min nitrate",
                       sol_key_maxP = "Corner: max profit",
                       sol_key_minI = "Corner: min irrigation"))

# knee (use 2D plane: N vs Profit, on the frontier)
knees <- frontiers %>%
  group_by(Scenario_label) %>%
  group_modify(~ knee_by_max_distance(.x, xcol = "Nitrate_kgyr", ycol = "Profit_usd")) %>%
  ungroup() %>%
  mutate(type = "Knee")

# collect “special points”
special_pts <- bind_rows(
  corners %>% select(Scenario_label, type, sol_key, Corn, Soybeans, Sorghum, Wheat),
  knees %>%
    mutate(type = "Knee") %>%
    select(Scenario_label, type, sol_key, Corn, Soybeans, Sorghum, Wheat)
)


# ------------------------------------------
# 5) Plot A: Pareto (N vs Profit), color = Irr
# ------------------------------------------
p_np <- ggplot(frontiers, aes(Profit_usd, Nitrate_kgyr, color = Irr_m3)) +
  geom_point(alpha = 0.85, size = 3) +
  facet_wrap(~Scenario_label)+
  scale_color_viridis_c(name = "Irrigation (m³/yr)") +
  labs(
    title = "Pareto frontiers: Profit vs nitrate",
    x = "Profit ($/yr, ↑)",
    y = "Nitrate export (kg/yr, ↓)"
  ) +
  theme_test(base_size = 15)

p_np
#ggsave(file.path(fig_dir, "pareto_N_vs_profit_byScenario.png"), p_np, width = 12, height = 7, dpi = 300)


# ------------------------------------------
# 6) Plot B: Crop composition along frontier + mark knee/corners
# ------------------------------------------
front_long <- crop_long(frontiers)
#special_long <- crop_long(special_pts) %>% mutate(is_special = TRUE)
special_crop_long <- special_pts %>%
  tidyr::pivot_longer(
    cols = c(Corn, Soybeans, Sorghum, Wheat),
    names_to = "Crop",
    values_to = "Share"
  )


p_crop_density <- ggplot(front_long, aes(Share, after_stat(density), fill = Crop)) +
  geom_histogram(position = "identity", alpha = 0.55, bins = 25) +
  facet_grid(Scenario_label ~ Crop) +
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
  filter(type != "Knee") %>%
  ggplot(aes(x = type, y = Share, fill = Crop)) +
  geom_col(width = 0.75) +
  facet_wrap(~ Scenario_label, ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1.01)) +
  scale_fill_crops() +
  labs(title = "Crop composition at knee + corner solutions",
       x = NULL, y = "Share of cultivated area") +
  theme_minimal(base_size = 13) +
  theme(plot.title.position = "plot")

p_crop_special

#ggsave(file.path(fig_dir, "crop_comp_knee_and_corners.png"),
 #      p_crop_special, width = 10, height = 8, dpi = 300)




frontier_comp_long <- frontiers_context %>%
  group_by(Scenario_raw) %>%
  arrange(Profit_usd, .by_group = TRUE) %>%
  mutate(frontier_position = row_number() / n()) %>%
  ungroup() %>%
  select(
    Scenario_raw, Scenario_label,
    landuse_name, climate_pathway, period,
    frontier_position, Profit_usd,
    Corn, Soybeans, Sorghum, Wheat
  ) %>%
  pivot_longer(
    cols = c(Corn, Soybeans, Sorghum, Wheat),
    names_to = "Crop",
    values_to = "Share"
  ) %>%
  mutate(
    Crop = factor(Crop, levels = c("Corn", "Soybeans", "Sorghum", "Wheat"))
  )


p_crop_trajectory <- ggplot(
  frontier_comp_long,
  aes(
    x = frontier_position,
    y = Share,
    fill = Crop
  )
) +
  geom_area(alpha = 0.95) +
  facet_grid(landuse_name ~ climate_pathway + period) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1)
  ) +
  scale_fill_crops() +
  labs(
    title = "Crop composition shifts along each Pareto frontier",
    subtitle = "Frontier position increases from low-profit to high-profit solutions",
    x = "Position along frontier",
    y = "Share of cultivated area"
  ) +
  theme_test(base_size = 15) +
  theme(plot.title.position = "plot")

p_crop_trajectory

# ------------------------------------------
# 7) Landcover composition from actual scenario_path files
# ------------------------------------------

scenario_contexts <- unique(frontiers_context$Scenario_raw)
scenario_contexts <- scenario_contexts[grepl("^(fixed|bau)_ensemble_rcp(45|85)_(early|mid|late)$", scenario_contexts)]

scenario_contexts

read_scenario_landcover <- function(Scenario_raw) {
  
  parts <- strsplit(Scenario_raw, "_")[[1]]
  
  landuse_name <- parts[1]
  gcm_name <- parts[2]
  climate_pathway <- parts[3]
  period_name <- parts[4]
  
  scenario_path_file <- here(
    "hpc_opt", "outputs", "factorial_runs",
    sprintf("%s_current_current_%s_%s", landuse_name, gcm_name, climate_pathway),
    sprintf("scenario_path_%s_current_current_%s_%s.csv", landuse_name, gcm_name, climate_pathway)
  )
  
  if (!file.exists(scenario_path_file)) {
    warning("Missing scenario path: ", scenario_path_file)
    return(tibble())
  }
  
  read_csv(scenario_path_file, show_col_types = FALSE) %>%
    filter(period == period_name) %>%
    summarise(
      Cultivated = mean(Cult_m2, na.rm = TRUE),
      `Grass/Pasture/Hay` = mean(grass_mean_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(Cultivated, `Grass/Pasture/Hay`),
      names_to = "Class",
      values_to = "area_m2"
    ) %>%
    mutate(
      Scenario_raw = Scenario_raw,
      Scenario_label = scenario_labels[Scenario_raw],
      frac = area_m2 / sum(area_m2, na.rm = TRUE)
    )
}
  
 

landcover_scenario <- purrr::map_dfr(
  scenario_contexts,
  read_scenario_landcover
)

p_lc_scenario <- landcover_scenario %>%
  ggplot(aes(x = Scenario_label, y = frac, fill = Class)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_landcover() +
  labs(
    title = "Landcover composition by optimization context",
    x = NULL,
    y = "Share of non-developed basin area"
  ) +
  theme_test(base_size = 14) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

p_lc_scenario




# 8 ternary plots
# ------------------------------------------
# Ternary frontier density: objective tradeoff space
# ------------------------------------------

frontiers_context %>%
  count(Scenario_raw, Scenario_label, landuse_name, climate_pathway, period)

# Normalize objectives globally across all plotted frontiers
obj_ranges <- frontiers_context %>%
  summarise(
    n_min = min(Nitrate_kgyr, na.rm = TRUE),
    n_max = max(Nitrate_kgyr, na.rm = TRUE),
    p_min = min(Profit_usd, na.rm = TRUE),
    p_max = max(Profit_usd, na.rm = TRUE),
    i_min = min(Irr_m3, na.rm = TRUE),
    i_max = max(Irr_m3, na.rm = TRUE)
  )

ternary_obj <- frontiers_context %>%
  mutate(
    # goodness scores; higher = better
    sN = 1 - (Nitrate_kgyr - obj_ranges$n_min) / (obj_ranges$n_max - obj_ranges$n_min),
    sP =     (Profit_usd   - obj_ranges$p_min) / (obj_ranges$p_max - obj_ranges$p_min),
    sI = 1 - (Irr_m3       - obj_ranges$i_min) / (obj_ranges$i_max - obj_ranges$i_min),
    
    sN = pmax(0, pmin(1, sN)),
    sP = pmax(0, pmin(1, sP)),
    sI = pmax(0, pmin(1, sI)),
    
    total = sN + sP + sI,
    pN = sN / total,
    pP = sP / total,
    pI = sI / total,
    
    x = pP + 0.5 * pN,
    y = (sqrt(3) / 2) * pN
  ) %>%
  filter(is.finite(x), is.finite(y))

tri <- tibble::tibble(
  x = c(0, 1, 0.5, 0),
  y = c(0, 0, sqrt(3) / 2, 0)
)

p_tern_density <- ggplot() +
  
  stat_density_2d(
    data = ternary_obj,
    aes(x = x, y = y, fill = after_stat(level)),
    geom = "polygon",
    contour = TRUE,
    bins = 6,
    alpha = 0.65,
    color = NA,
    h = c(0.06, 0.06)
  ) +
  scale_fill_gradient(
    low = "grey80",
    high = "grey20",
    name = "Solution\ndensity"
  ) +
  
  geom_path(data = tri, aes(x, y), linewidth = 0.8) +
  geom_point(
    data = ternary_obj,
    aes(x, y, color = climate_pathway),
    size = 2.4,
    alpha = 0.75
  ) +
  
  annotate("text", x = 0.5, y = sqrt(3) / 2 + 0.06, label = "Low nitrate", size = 4) +
  annotate("text", x = -0.03, y = -0.04, label = "Low irrigation", hjust = 0, size = 4) +
  annotate("text", x = 1.03, y = -0.04, label = "High profit", hjust = 1, size = 4) +
  facet_wrap(
     period ~ landuse_name,
    labeller = labeller(
      landuse_name = c(
        fixed = "Fixed cultivated area",
        bau = "BAU expansion"
      ),
      period = c(
        early = "Early century",
      #  mid = "Mid century",
        late = "Late century"
      )
    )
  ) +
  coord_equal(
    xlim = c(-0.08, 1.08),
    ylim = c(-0.08, sqrt(3) / 2 + 0.1),
    expand = FALSE
  ) +
  scale_color_manual(
    values = c(
      "RCP 4.5" = "steelblue",
      "RCP 8.5" = "maroon"
    )
  ) +
  labs(
    title = "Pareto frontier density in normalized tradeoff space",
    x = NULL,
    y = NULL,
    color = "Climate pathway"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

p_tern_density


