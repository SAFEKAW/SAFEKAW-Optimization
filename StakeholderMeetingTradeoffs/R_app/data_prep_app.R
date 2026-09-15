
library(dplyr)
library(tibble)
library(here)
library(readr)
library(tidyr)

# Keep this app-preparation step independent of the repository-wide package
# bootstrap. The app only needs the economic parameters defined below; loading
# paths+packages.R also requires unrelated GIS and plotting packages.
source(here("StakeholderMeetingTradeoffs/R_app/econ.R"))


# ---- 0) Basic settings ----
years_curr <- 2006:2023
crops4     <- c("Corn","Soybeans","Sorghum","Wheat")

get_area_m2 <- function(df, area_m2_col = "Crop_area_m2", area_ha_col = "Crop_area_ha") {
  if (area_m2_col %in% names(df)) {
    df$area_m2 <- df[[area_m2_col]]
  } else if (area_ha_col %in% names(df)) {
    df$area_m2 <- df[[area_ha_col]] * 1e4
  } else {
    df$area_m2 <- NA_real_
  }

  df
}


# read inputs
df_int_crop_areanorm <- read_csv(here("data/int_crop_areanorm_annual.csv"), show_col_types = FALSE)
df_basin_input_forWQ <- read_csv(here("data/common_inputs_basin.csv"), show_col_types = FALSE) %>%
  mutate(Management_FertilizerUse_kgm2 = Management_FertilizerUse_kg / `LandCover_m2_all cult`)

# ---- 1) Land-use history ----
lu_hist <- df_int_crop_areanorm %>%
  filter(Crop %in% crops4) %>%
  get_area_m2() %>%
  drop_na(area_m2)

# Current-period mean crop mix (for baseline sliders)
obs_mix <- lu_hist %>%
  filter(Year %in% years_curr) %>%
  group_by(Crop) %>%
  summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = area_m2 / sum(area_m2, na.rm = TRUE))

obs_x <- obs_mix %>%
  filter(Crop %in% c("Corn","Soybeans","Sorghum")) %>%
  mutate(Crop = factor(Crop, levels = c("Corn","Soybeans","Sorghum"))) %>%
  arrange(Crop) %>%
  pull(share)

obs_x <- setNames(obs_x, c("Corn","Soybeans","Sorghum"))

saveRDS(obs_mix, here("StakeholderMeetingTradeoffs/data_app/obs_mix.rds"))
saveRDS(obs_x,   here("StakeholderMeetingTradeoffs/data_app/obs_x.rds"))

# ---- 2) hist_mix (irrig fractions by crop) ----
# If you can compute these from data, do it here.
# Otherwise, your placeholder is fine:
hist_mix <- tibble(
  Crop = crops4,
  irrig_frac = c(0.40, 0.55, 0.10, 0.10)
)

saveRDS(hist_mix, here("StakeholderMeetingTradeoffs/data_app/hist_mix.rds"))


# ---- 3) lu_baseline (basin landcover totals by year) ----
lu_baseline <- df_basin_input_forWQ %>%
  select(
    Year,
    LC_cult_base  = `LandCover_m2_all cult`,
    LC_grass_base = LandCover_m2_grassPastureHay,
    LC_dev_base   = LandCover_m2_developed
  ) %>%
  mutate(LC_total = LC_cult_base + LC_grass_base + LC_dev_base)

saveRDS(lu_baseline, here("StakeholderMeetingTradeoffs/data_app/lu_baseline.rds"))

# ---- 3.5) fert_ref_by_year (baseline fertilizer totals for scaling) ----

# baseline fert intensity on cultivated land (kg/ha), implied by obs_mix
fert_intensity_baseline_kgha <- obs_mix %>%
  left_join(crop_params %>% select(Crop, fert_kgha), by = "Crop") %>%
  summarise(fert_kgha = sum(share * fert_kgha, na.rm = TRUE)) %>%
  pull(fert_kgha)

fert_ref_by_year <- lu_baseline %>%
  dplyr::filter(Year %in% years_curr) %>%
  transmute(
    Year,
    Cult_ha = LC_cult_base / 1e4,
    Fert_ref_kg = Cult_ha * fert_intensity_baseline_kgha
  )

stopifnot(nrow(fert_ref_by_year) == length(years_curr))
stopifnot(!anyNA(fert_ref_by_year$Fert_ref_kg))

saveRDS(fert_ref_by_year, here("StakeholderMeetingTradeoffs/data_app/fert_ref_by_year.rds"))


# ---- 4) df_opt (app-ready table used by eval) ----
# This should include anything eval needs year-by-year.
# If eval expects crop-level econ columns in df_opt, join them once here:
df_opt <- lu_hist %>%
  left_join(crop_params, by = "Crop")  # only if needed/used inside eval

saveRDS(df_opt, here("StakeholderMeetingTradeoffs/data_app/df_opt.rds"))




# starting from df_basin_input_forWQ (must include Climate_precip_m and Climate_Tmean_C or Tmin/Tmax)

wq_base_by_year <- df_basin_input_forWQ %>%
  transmute(
    Year,
    Climate_precip_m,
    Climate_Tmean_C = dplyr::coalesce(Climate_Tmean_C, (Climate_Tmin_C + Climate_Tmax_C)/2)
  ) %>%
  arrange(Year)

ec <- ecdf(wq_base_by_year$Climate_precip_m)

wq_base_by_year <- wq_base_by_year %>%
  mutate(
    precip_percentile       = ec(Climate_precip_m),
    precip_percentileChange = dplyr::coalesce(precip_percentile - dplyr::lag(precip_percentile), 0)
  ) %>%
  select(Year, Climate_precip_m, Climate_Tmean_C, precip_percentileChange)

saveRDS(wq_base_by_year, here("StakeholderMeetingTradeoffs/data_app/wq_base_by_year_from_common_inputs.rds"))


