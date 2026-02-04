library(dplyr)
library(readr)
library(here)

# load basin context (same as before)
years_curr <- 2006:2023

df_basin <- read_csv(here("data/common_inputs_basin.csv"), show_col_types = FALSE) %>%
  filter(Year %in% years_curr)

df_nitrate <- read_csv(here("data/RiverNitrateData_EKSRB.csv"), show_col_types = FALSE)

# join obs nitrate
wq_context <- df_basin %>%
  left_join(df_nitrate, by = "Year") %>%
  arrange(Year)

# precip percentile features (same logic)
ec <- ecdf(wq_context$Climate_precip_m)
wq_context <- wq_context %>%
  mutate(
    precip_percentile       = ec(Climate_precip_m),
    precip_percentileChange = tidyr::replace_na(precip_percentile - lag(precip_percentile), 0)
  )

# compute BasinArea and fertilizer density consistent with new model
wq_context <- wq_context %>%
  mutate(
    Climate_Tmean_C = dplyr::coalesce(Climate_Tmean_C, (Climate_Tmin_C + Climate_Tmax_C)/2),
    BasinArea_m2 = `LandCover_m2_all cult` + LandCover_m2_grassPastureHay + LandCover_m2_developed,
    Management_FertilizerUse_kgm2 = Management_FertilizerUse_kg / BasinArea_m2,
    dev_frac  = LandCover_m2_developed / BasinArea_m2,
    cult_frac = `LandCover_m2_all cult` / BasinArea_m2
  )

req <- c(
  "NitrateFlux_kg",
  "BasinArea_m2","dev_frac","cult_frac",
  "Climate_precip_m","Climate_Tmean_C","precip_percentileChange",
  "Management_FertilizerUse_kgm2"
)

wq_train <- wq_context %>% tidyr::drop_na(dplyr::all_of(req))

wq_model_mlr <- lm(
  NitrateFlux_kg ~ cult_frac +
    Climate_precip_m + Climate_Tmean_C + precip_percentileChange,
  data = wq_train
)

saveRDS(wq_model_mlr, here("StakeholderMeetingTradeoffs/models_app/wq_model_mlr.rds"))

# base-by-year for app (climate-only; landcover is built from lu_baseline inside eval_candidate)
wq_base_by_year <- wq_context %>%
  distinct(Year, .keep_all = TRUE) %>%
  transmute(Year, Climate_precip_m, Climate_Tmean_C, precip_percentileChange)

saveRDS(wq_base_by_year, here("StakeholderMeetingTradeoffs/data_app/wq_base_by_year.rds"))
