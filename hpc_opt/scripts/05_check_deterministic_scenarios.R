suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(purrr)
  library(stringr)
})

# ---- load scenario paths from factorial runs ----
run_root <- here("hpc_opt", "outputs", "factorial_runs")

path_files <- list.files(
  run_root,
  pattern = "^scenario_path_.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# ---- remove old / broken paths ----
path_files <- path_files[file.exists(path_files)]

# ---- OPTIONAL: only keep ensemble runs ----
path_files <- path_files[str_detect(path_files, "ensemble")]

length(path_files)

path_df <- purrr::map_dfr(path_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      source_file = f,
      combo_name = str_remove(basename(f), "^scenario_path_"),
      combo_name = str_remove(combo_name, "\\.csv$")
    )
})

# ---- load deterministic results ----
res_files <- list.files(
  run_root,
  pattern = "^deterministic_results_.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

res_files <- res_files[file.exists(res_files)]
res_files <- res_files[str_detect(res_files, "ensemble")]  # optional but recommended

res_df <- purrr::map_dfr(res_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      source_file = f,
      combo_name = str_remove(basename(f), "^deterministic_results_"),
      combo_name = str_remove(combo_name, "\\.csv$")
    )
})

# ---- cultivated area trajectory ----
p_cult <- ggplot(
  path_df %>% distinct(combo_name, landuse_name, Year, Cult_m2),
  aes(x = Year, y = Cult_m2 / 1e6, color = landuse_name)
) +
  geom_line(linewidth = 1.2) +
  labs(
    x = NULL,
    y = expression("Cultivated area (km"^2*")"),
    title = "Cultivated area trajectory"
  ) +
  theme_bw(base_size = 13)

# ---- crop share trajectories ----
crop_long <- path_df %>%
  select(combo_name, landuse_name, Year, corn, soy, sor, wheat) %>%
  distinct() %>%
  pivot_longer(
    cols = c(corn, soy, sor, wheat),
    names_to = "crop",
    values_to = "share"
  ) %>%
  mutate(
    crop = recode(
      crop,
      corn = "Corn",
      soy = "Soybeans",
      sor = "Sorghum",
      wheat = "Wheat"
    )
  )

p_crop <- ggplot(crop_long, aes(x = Year, y = share, color = landuse_name)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~crop, ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Crop share",
    title = "Crop composition trajectory"
  ) +
  theme_bw(base_size = 13)

# ---- annual objective trajectories ----
res_long <- res_df %>%
  transmute(
    landuse_name,
    Year,
    `Nitrate flux` = nitrate_kgyr,
    `Irrigation volume` = irrigation_m3yr,
    `Net returns` = -neg_profit_usdyr
  ) %>%
  pivot_longer(cols = c(`Nitrate flux`, `Irrigation volume`, `Net returns`),
               names_to = "metric", values_to = "value")

p_obj <- ggplot(res_long, aes(x = Year, y = value, color = landuse_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    x = NULL,
    y = NULL,
    title = "Annual objective trajectories"
  ) +
  theme_bw(base_size = 13)

# ---- summary comparison ----
summary_df <- res_df %>%
  group_by(landuse_name) %>%
  summarise(
    `Mean nitrate flux` = mean(nitrate_kgyr, na.rm = TRUE),
    `Mean irrigation volume` = mean(irrigation_m3yr, na.rm = TRUE),
    `Mean net returns` = mean(-neg_profit_usdyr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -landuse_name,
    names_to = "metric",
    values_to = "value"
  )

p_sum <- ggplot(summary_df, aes(x = landuse_name, y = value, fill = landuse_name)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    x = NULL,
    y = NULL,
    title = "Scenario mean outcomes"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# ---- print / combine ----
print(p_cult)
print(p_crop)
print(p_obj)
print(p_sum)

(p_cult | p_sum) / (p_crop | p_obj)


#checks on YAML
path_df %>%
  filter(landuse_name == "bau") %>%
  summarise(
    soy_start = first(soy[order(Year)]),
    soy_end   = last(soy[order(Year)]),
    corn_start = first(corn[order(Year)]),
    corn_end   = last(corn[order(Year)])
  )

bau_area_check <- path_df %>%
  filter(landuse_name == "bau") %>%
  distinct(Year, corn, soy, sor, wheat, Cult_m2) %>%
  mutate(
    Corn_m2 = corn * Cult_m2,
    Soybeans_m2 = soy * Cult_m2,
    Sorghum_m2 = sor * Cult_m2,
    Wheat_m2 = wheat * Cult_m2
  ) %>%
  select(Year, Corn_m2, Soybeans_m2, Sorghum_m2, Wheat_m2) %>%
  pivot_longer(-Year, names_to = "crop", values_to = "area_m2")


ggplot(bau_area_check, aes(Year, area_m2 / 1e6, color = crop)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = NULL,
    y = expression("Crop area (km"^2*")"),
    title = "BAU crop area trajectory"
  ) +
  theme_bw(base_size = 13)



