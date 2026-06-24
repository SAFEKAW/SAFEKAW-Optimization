suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(purrr)
})

# ---- historical reference ----
hist_annual <- read_csv(
  here("hpc_opt", "outputs", "integration", "int_basin_annual.csv"),
  show_col_types = FALSE
)

hist_basin_common <- readr::read_csv(
  here("hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"),
  show_col_types = FALSE
)
hist_irrig <- hist_basin_common %>%
  summarise(hist_irrig = mean(Irrigation_total_m3, na.rm = TRUE)) %>%
  pull(hist_irrig)

hist_ref <- hist_annual %>%
  summarise(
    hist_nitrate_kgyr = mean(NitrateFluxPredicted_kg, na.rm = TRUE),
    hist_profit_usdyr = mean(NetReturn_total_usd, na.rm = TRUE)
  ) %>%
  mutate(
    hist_irrigation_m3yr = hist_irrig
  )


hist_nitrate <- hist_ref$hist_nitrate_kgyr[[1]]
hist_irrig   <- hist_ref$hist_irrigation_m3yr[[1]]
hist_profit  <- hist_ref$hist_profit_usdyr[[1]]

# ---- locate all factorial run folders ----
run_root <- here("hpc_opt", "outputs", "factorial_runs")
run_dirs <- list.dirs(run_root, recursive = FALSE, full.names = TRUE)

#climate_pathway <- "rcp85"

#if (!is.null(climate_pathway) && nzchar(climate_pathway)) {
 # run_dirs <- run_dirs[str_detect(basename(run_dirs), paste0("__", climate_pathway, "$"))]
#}

#if (length(run_dirs) == 0) stop("No factorial run folders found.")

climate_source <- "ensemble"
climate_pathway <- NULL

if (!is.null(climate_source) && nzchar(climate_source)) {
  run_dirs <- run_dirs[str_detect(basename(run_dirs), paste0("_", climate_source, "_"))]
}

if (is.null(climate_pathway)) {
  run_dirs <- run_dirs[str_detect(basename(run_dirs), "_(rcp45|rcp85)$")]
} else {
  run_dirs <- run_dirs[str_detect(basename(run_dirs), paste0("_", climate_pathway, "$"))]
}

if (length(run_dirs) == 0) stop("No factorial run folders found.")

# ---- helper to read one combo ----
read_one_combo <- function(dir_path) {
  files <- list.files(dir_path, full.names = TRUE)
  
  res_file <- files[str_detect(basename(files), "^deterministic_results_.*\\.csv$")]
  sum_period_file <- files[str_detect(basename(files), "^deterministic_summary_by_period_.*\\.csv$")]
  sum_overall_file <- files[str_detect(basename(files), "^deterministic_summary_overall_.*\\.csv$")]
  path_file <- files[str_detect(basename(files), "^scenario_path_.*\\.csv$")]
  
  if (length(res_file) != 1 || length(sum_period_file) != 1 ||
      length(sum_overall_file) != 1 || length(path_file) != 1) {
    stop("Expected exactly one results / by-period summary / overall summary / path file in: ", dir_path)
  }
  
  list(
    results = read_csv(res_file, show_col_types = FALSE),
    summary_by_period = read_csv(sum_period_file, show_col_types = FALSE),
    summary_overall = read_csv(sum_overall_file, show_col_types = FALSE),
    path = read_csv(path_file, show_col_types = FALSE)
  )
}

combo_list <- map(run_dirs, read_one_combo)

results_all <- bind_rows(map(combo_list, "results"))
summary_period_all <- bind_rows(map(combo_list, "summary_by_period"))
summary_overall_all <- bind_rows(map(combo_list, "summary_overall"))
path_all <- bind_rows(map(combo_list, "path"))

# ---- add combo labels if missing ----
add_combo_name <- function(df) {
  if (!"combo_name" %in% names(df)) {
    df %>%
      mutate(combo_name = paste(landuse_name, irrigation_name, fertilizer_name, climate_pathway, sep = "__"))
  } else {
    df
  }
}

results_all        <- add_combo_name(results_all)
summary_period_all <- add_combo_name(summary_period_all)
summary_overall_all <- add_combo_name(summary_overall_all)



# ---- use by-period summary for plotting ----
summary_plot <- summary_period_all
summary_overall_plot <- summary_overall_all

results_plot <- results_all %>%
  mutate(profit_usdyr = -neg_profit_usdyr)

# ---- factor ordering ----
landuse_levels <- c("fixed", "bau", "crp")
irrig_levels   <- c("current", "efficient", "expand_area", "contract_area")
fert_levels    <- c("current", "efficient")
climate_levels <- c("rcp45", "rcp85")
period_levels  <- c("early", "mid", "late")

summary_plot <- summary_plot %>%
  mutate(
    landuse_name = factor(landuse_name, levels = intersect(landuse_levels, unique(landuse_name))),
    irrigation_name = factor(irrigation_name, levels = intersect(irrig_levels, unique(irrigation_name))),
    fertilizer_name = factor(fertilizer_name, levels = intersect(fert_levels, unique(fertilizer_name))),
    climate_pathway = factor(climate_pathway, levels = intersect(climate_levels, unique(climate_pathway))),
    period = factor(period, levels = intersect(period_levels, unique(period)))
  )

results_plot <- results_plot %>%
  mutate(
    landuse_name = factor(landuse_name, levels = levels(summary_plot$landuse_name)),
    irrigation_name = factor(irrigation_name, levels = levels(summary_plot$irrigation_name)),
    fertilizer_name = factor(fertilizer_name, levels = levels(summary_plot$fertilizer_name)),
    climate_pathway = factor(climate_pathway, levels = levels(summary_plot$climate_pathway)),
    period = factor(period, levels = levels(summary_plot$period))
  )

profit_area_plot <- results_plot %>%
  mutate(
    mgmt = paste(irrigation_name, fertilizer_name, sep = " | "),
    profit_per_ha = profit_usdyr / (Cult_m2 / 10000)
  )

p_profit_per_ha <- ggplot(
  profit_area_plot,
  aes(
    x = Year,
    y = profit_per_ha,
    color = mgmt,
    group = interaction(combo_name, mgmt)
  )
) +
  geom_line(alpha = 0.85, linewidth = 0.8) +
  geom_vline(xintercept = 2049, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 2074, linetype = "dashed", color = "grey70") +
  facet_grid(climate_pathway ~ landuse_name, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Net returns per cultivated hectare",
    subtitle = "Area-normalized profit separates climate/yield effects from cultivated-area expansion",
    x = "Year",
    y = "Net return ($/ha)",
    color = "Management"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")

p_profit_per_ha



# ---- helpers ----
safe_ratio <- function(x, ref) {
  ifelse(is.finite(x) & is.finite(ref) & ref != 0, x / ref, NA_real_)
}

clamp01 <- function(x) {
  pmax(0, pmin(1, x))
}

# ---- normalized tables ----
summary_norm <- summary_plot %>%
  mutate(
    norm_n = safe_ratio(mean_nitrate_kgyr, hist_nitrate),
    norm_i = safe_ratio(mean_irrigation_m3yr, hist_irrig),
    norm_p = safe_ratio(mean_profit_usdyr, hist_profit),
    norm_n_01 = clamp01(norm_n),
    norm_i_01 = clamp01(norm_i),
    norm_p_01 = clamp01(norm_p)
  )

summary_scores <- summary_norm %>%
  mutate(
    score_n = 1 - norm_n_01,
    score_i = 1 - norm_i_01,
    score_p = norm_p_01,
    score_sum = score_n + score_i + score_p,
    pN = if_else(score_sum > 0, score_n / score_sum, NA_real_),
    pI = if_else(score_sum > 0, score_i / score_sum, NA_real_),
    pP = if_else(score_sum > 0, score_p / score_sum, NA_real_)
  )

results_norm <- results_plot %>%
  mutate(
    norm_n = safe_ratio(nitrate_kgyr, hist_nitrate),
    norm_i = safe_ratio(irrigation_m3yr, hist_irrig),
    norm_p = safe_ratio(profit_usdyr, hist_profit),
    norm_n_01 = clamp01(norm_n),
    norm_i_01 = clamp01(norm_i),
    norm_p_01 = clamp01(norm_p)
  )


#plots
traj_long <- results_plot %>%
  select(
    Year, combo_name, climate_pathway, period,
    landuse_name, irrigation_name, fertilizer_name,
    nitrate_kgyr, irrigation_m3yr, profit_usdyr
  ) %>%
  pivot_longer(
    cols = c(nitrate_kgyr, irrigation_m3yr, profit_usdyr),
    names_to = "objective",
    values_to = "value"
  ) %>%
  mutate(
    objective = recode(
      objective,
      nitrate_kgyr = "Nitrate",
      irrigation_m3yr = "Irrigation",
      profit_usdyr = "Net return"
    ),
    mgmt = paste(irrigation_name, fertilizer_name, sep = " | ")
  )

p_traj <- ggplot(
  traj_long,
  aes(x = Year, y = value, color = mgmt, group = interaction(combo_name, objective))
) +
  geom_line(alpha = 0.8) +
  geom_vline(xintercept = 2049, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 2074, linetype = "dashed", color = "grey70") +
  facet_grid(objective ~ climate_pathway + landuse_name, scales = "free_y") +
  labs(
    title = "Annual trajectories across future scenarios",
    x = "Year",
    y = NULL,
    color = "Management"
  ) +
  theme_test(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  here("hpc_opt","outputs","factorial_runs","figures","traj.png"),
  p_traj, width = 8, height = 6, dpi = 300
)
p_traj


# ---- historical annual objectives ----
hist_obj <- readr::read_csv(
  here("hpc_opt", "outputs", "integration", "int_basin_annual.csv"),
  show_col_types = FALSE
)

hist_irrig_common <- readr::read_csv(
  here("hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  select(Year, irrigation_m3yr_hist = Irrigation_total_m3)



summary_df <- results_plot %>%
  group_by(climate_pathway, landuse_name, Year) %>%
  summarise(
    irrigation_mean = mean(irrigation_m3yr, na.rm = TRUE),
    irrigation_min  = min(irrigation_m3yr, na.rm = TRUE),
    irrigation_max  = max(irrigation_m3yr, na.rm = TRUE),
    
    profit_mean = mean(profit_usdyr, na.rm = TRUE),
    profit_min  = min(profit_usdyr, na.rm = TRUE),
    profit_max  = max(profit_usdyr, na.rm = TRUE),
    
    nitrate_mean = mean(nitrate_kgyr, na.rm = TRUE),
    nitrate_min  = min(nitrate_kgyr, na.rm = TRUE),
    nitrate_max  = max(nitrate_kgyr, na.rm = TRUE),
    
    .groups = "drop"
  )


summary_long <- summary_df %>%
  pivot_longer(
    cols = -c(climate_pathway, landuse_name, Year),
    names_to = c("metric", ".value"),
    names_pattern = "(.*)_(mean|min|max)"
  )

summary_long %>%
  group_by(metric) %>%
  summarise(
    min_value = min(.data$mean, na.rm = TRUE),
    mean_value = mean(.data$mean, na.rm = TRUE),
    max_value = max(.data$mean, na.rm = TRUE),
    .groups = "drop"
  )


hist_obj_plot <- hist_obj %>%
  left_join(hist_irrig_common, by = "Year") %>%
  transmute(
    Year,
    irrigation = irrigation_m3yr_hist,
    nitrate = NitrateFluxPredicted_kg,
    profit = NetReturn_total_usd
  ) %>%
  pivot_longer(
    cols = c(irrigation, nitrate, profit),
    names_to = "metric",
    values_to = "value"
  ) %>%
  tidyr::crossing(
    climate_pathway = unique(summary_long$climate_pathway)
  )

hist_obj_plot %>%
  group_by(metric) %>%
  summarise(
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )



p_traj_lumped <-
  ggplot(summary_long,
            aes(x = Year, y = mean, color = climate_pathway, fill = climate_pathway)) +
  
  geom_ribbon(aes(ymin = min, ymax = max),
              alpha = 0.2,
              color = NA) +
  
  geom_line(linewidth = 1.2) +
  
  facet_grid(metric ~ landuse_name, scales = "free_y") +
  
  geom_vline(xintercept = c(2025, 2050, 2075),
             linetype = "dashed", color = "grey60") +
  
  scale_color_manual(values = c(
   # fixed = "#1B9E77",
  #  bau   = "#D95F02"
   rcp45 =  "skyblue",
   rcp85 =  "maroon"
  )) +
  
  scale_fill_manual(values = c(
   # fixed = "#1B9E77",
  #  bau   = "#D95F02"
    rcp45 =  "skyblue",
    rcp85 =  "maroon"
  )) +
  
 # geom_line(
#    data = hist_obj_plot,
#    aes(x = Year, y = value),
#    inherit.aes = FALSE,
#    color = "black",
#    linewidth = 0.8
#  ) +
  
  labs(
    x = "Year",
    y = NULL,
    color = "Climate pathway",
    fill = "Climate pathway",
    title = "Mean trajectories with management bounds"
  ) +
  
  theme_test(base_size = 13) +
  theme(
    legend.position = "bottom"
  )






landuse_labels <- c(
  fixed = "Fixed cultivated area",
  bau = "BAU expansion"
)

climate_labels <- c(
  rcp45 = "RCP 4.5",
  rcp85 = "RCP 8.5"
)


summary_long_plot <- summary_long %>%
  mutate(
    across(
      c(mean, min, max),
      ~ .x / 1e6
    ),
    metric = dplyr::recode(
      as.character(metric),
      "irrigation" = "Irrigation water use\n(million m³ yr⁻¹)",
      "nitrate"    = "Nitrate export\n(million kg N yr⁻¹)",
      "profit"     = "Net returns\n(million USD yr⁻¹)"
    )
  )


summary_long_plot <- summary_long_plot %>%
  mutate(
    metric = dplyr::recode(
      as.character(metric),
      "irrigation" = "Irrigation water use\n(million m³ yr⁻¹)",
      "nitrate"    = "Nitrate export\n(million kg N yr⁻¹)",
      "profit"     = "Net returns\n(million USD yr⁻¹)"
    ),
    metric = factor(
      metric,
      levels = c(
        "Nitrate export\n(million kg N yr⁻¹)",
        "Irrigation water use\n(million m³ yr⁻¹)",
        "Net returns\n(million USD yr⁻¹)"
      )
    )
  )

p_traj_lumped <-
  ggplot(
    summary_long_plot,
    aes(
      x = Year,
      y = mean,
      color = climate_pathway,
      fill = climate_pathway
    )
  ) +
  geom_ribbon(
    aes(ymin = min, ymax = max),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(linewidth = 1.3) +
  
  geom_vline(
    xintercept = c(2025, 2050, 2075),
    linetype = "dashed",
    color = "grey55",
    linewidth = 0.5
  ) +
  
  annotate("text", x = 2037.5, y = Inf, label = "Early century",
           vjust = 1.4, size = 3.5, color = "grey25") +
  annotate("text", x = 2062.5, y = Inf, label = "Mid century",
           vjust = 1.4, size = 3.5, color = "grey25") +
  annotate("text", x = 2087.5, y = Inf, label = "Late century",
           vjust = 1.4, size = 3.5, color = "grey25") +
  
  facet_grid(
    metric ~ landuse_name,
    scales = "free_y",
    labeller = labeller(
      landuse_name = landuse_labels
    )
  ) +
  
  scale_color_manual(
    name = "Climate pathway",
    values = c(
      rcp45 = "skyblue",
      rcp85 = "maroon"
    ),
    labels = climate_labels
  ) +
  scale_fill_manual(
    name = "Climate pathway",
    values = c(
      rcp45 = "skyblue",
      rcp85 = "maroon"
    ),
    labels = climate_labels
  ) +
  
  scale_x_continuous(
    breaks = c(2025, 2050, 2075, 2100),
    limits = c(2025, 2100),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  labs(
    x = NULL,
    y = NULL#,
  #  title = "Projected trajectories across climate and land-use scenarios",
  #  subtitle = "Lines show mean outcomes; shaded ribbons show management scenario bounds"
  ) +
  
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(linewidth = 1.8, alpha = 1)
    )
  ) +
  
  theme_test(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.spacing = unit(1.1, "lines"),
    axis.text.x = element_text(
      angle = 45,      # Rotation angle in degrees
      hjust = 1,       # Horizontal justification
      vjust = 1        # Vertical justification
    )
  )


p_traj_lumped

ggsave(
  here("hpc_opt","outputs","factorial_runs","figures","traj_lumped.png"),
  p_traj_lumped, width = 7, height = 6, dpi = 300
)
p_traj_lumped



#raw
profile_raw <- summary_plot %>%
  select(combo_name, climate_pathway, period, landuse_name, irrigation_name, fertilizer_name,
         mean_nitrate_kgyr, mean_irrigation_m3yr, mean_profit_usdyr) %>%
  pivot_longer(
    cols = c(mean_nitrate_kgyr, mean_irrigation_m3yr, mean_profit_usdyr),
    names_to = "objective",
    values_to = "value"
  ) %>%
  mutate(
    objective = recode(
      objective,
      mean_nitrate_kgyr = "Nitrate",
      mean_irrigation_m3yr = "Irrigation",
      mean_profit_usdyr = "Net return"
    )
  )

p_profile_raw <- ggplot(
  profile_raw,
  aes(x = landuse_name, y = value, color = fertilizer_name)
) +
  geom_point(aes(shape = irrigation_name),
             size = 3,
             position = position_dodge(width = 0.45)) +
  facet_grid(objective ~ period + climate_pathway, scales = "free_y") +
 # facet_grid(objective ~ climate_pathway + landuse_name, scales = "free_y") +
  
  labs(
    x = NULL,
    y = "Mean value",
    title = "Mean objective values by deterministic scenario",
    subtitle = "Raw units by period"
  ) +
  theme_bw(base_size = 13)

ggsave(
  here("hpc_opt","outputs","factorial_runs","figures","p_profile_raw.png"),
  p_profile_raw, width = 8, height = 6, dpi = 300
)


#norm
profile_norm <- summary_norm %>%
  select(
    combo_name, climate_pathway, period,
    landuse_name, irrigation_name, fertilizer_name,
    norm_n, norm_i, norm_p
  ) %>%
  pivot_longer(
    cols = c(norm_n, norm_i, norm_p),
    names_to = "objective",
    values_to = "value"
  ) %>%
  mutate(
    objective = recode(
      objective,
      norm_n = "Nitrate (↓ better)",
      norm_i = "Irrigation (↓ better)",
      norm_p = "Profit (↑ better)"
    )
  )



profile_norm_summary <- profile_norm %>%
  group_by(climate_pathway, period, objective, landuse_name) %>%
  summarise(
    value_min = min(value, na.rm = TRUE),
    value_med = median(value, na.rm = TRUE),
    value_max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

pd <- position_dodge(width = 0.45)

p_profile_norm <- ggplot(
  profile_norm_summary,
  aes(x = period, color = landuse_name)
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_linerange(
    aes(ymin = value_min, ymax = value_max),
    linewidth = 1,
    position = pd
  ) +
  geom_point(
    aes(y = value_med),
    size = 3,
    position = pd
  ) +
  scale_color_manual(values = c(
    fixed = "#1B9E77",
    bau   = "#D95F02"
  )) +
  facet_grid(objective ~ climate_pathway, scales = "free_y") +
  labs(
    x = NULL,
    y = "Relative to historical",
    title = "Future outcomes relative to historical conditions",
    subtitle = "Points = median; lines = management range"
  ) +
  theme_bw(base_size = 13)

ggsave(
  here("hpc_opt","outputs","factorial_runs","figures","p_profile_norm.png"),
  p_profile_norm, width = 8, height = 6, dpi = 300
)

p_profile_norm
