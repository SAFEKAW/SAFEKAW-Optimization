library(dplyr)
library(tidyr)
library(readr)
library(here)

years_curr <- 2006:2023  # or whatever you use

# --- Load basin inputs (the same object you use elsewhere) ---
df_basin_input_forWQ <- read_csv(here("data/common_inputs_basin.csv"), show_col_types = FALSE) %>%
  mutate(Management_FertilizerUse_kgm2 = Management_FertilizerUse_kg / `LandCover_m2_all cult`) %>%
  filter(Year %in% years_curr)

# --- Join observed nitrate flux ---
df_nitrate <- read_csv(here("data/RiverNitrateData_EKSRB.csv"), show_col_types = FALSE)

wq_context <- df_basin_input_forWQ %>%
  left_join(df_nitrate, by = "Year") %>%
  arrange(Year)

# --- Precip percentile features (training-consistent) ---
ec <- ecdf(wq_context$Climate_precip_m)

wq_context <- wq_context %>%
  mutate(
    precip_percentile       = ec(Climate_precip_m),
    precip_percentileChange = replace_na(precip_percentile - lag(precip_percentile), 0)
  )

# --- Training rows: require full predictors + response ---
req_wq <- c(
  "NitrateFlux_kg","LandCover_m2_developed","LandCover_m2_grassPastureHay",
  "LandCover_m2_all cult","Climate_precip_m","Climate_Tmean_C",
  "precip_percentileChange","Management_irrigation_m","Management_FertilizerUse_kgm2"
)

wq_train <- wq_context %>% drop_na(all_of(req_wq))

# --- Fit log1p model (positivity at predict time) ---
wq_model_log1p <- lm(
  log1p(NitrateFlux_kg) ~ LandCover_m2_developed + LandCover_m2_grassPastureHay + `LandCover_m2_all cult` +
    Climate_precip_m + Climate_Tmean_C + precip_percentileChange +
    Management_irrigation_m + Management_FertilizerUse_kgm2,
  data = wq_train
)

# --- Save model for app ---
saveRDS(wq_model_log1p, here("StakeholderMeetingTradeoffs/models_app/wq_model_log1p.rds"))

# --- Save per-year base predictors for app (NO response) ---
wq_base_by_year <- wq_context %>%
  distinct(Year, .keep_all = TRUE) %>%
  transmute(
    Year,
    Climate_precip_m,
    Climate_Tmean_C,
    precip_percentileChange
  )

saveRDS(wq_base_by_year, here("StakeholderMeetingTradeoffs/data_app/wq_base_by_year.rds"))

# --- Optional: save required vars list for validation in app ---
wq_required_vars <- all.vars(formula(wq_model_log1p))[-1]
saveRDS(
  list(required_vars = wq_required_vars),
  here("StakeholderMeetingTradeoffs/models_app/wq_context.rds")
)

