#01_build_historical_baseline_inputs.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

# -------------------------------------------------------------------------
# Build county historical baseline common inputs
# -------------------------------------------------------------------------

years_hist <- 2006:2023

county_base <- readr::read_csv(
  here::here("data", "common_inputs_county.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist) %>%
  mutate(
    FIPS = as.character(FIPS),
    LandCoverGroup = dplyr::recode(
      LandCoverGroup,
      "all cult" = "all_cult"
    )
  ) %>%
  select(
    -any_of(c(
      "spatial_scale",
      "GDD",
      "precip_gs_mm",
      "precip_m",
      "precip_m_avg",
      "tmin_C_avg",
      "tmax_C_avg"
    ))
  )

climate_daily <- readr::read_csv(
  here::here(
    "data",
    "ClimateData_County_hist_baseline.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(FIPS = as.character(FIPS))

climate_annual <- climate_daily %>%
  group_by(FIPS, Year) %>%
  summarise(
    precip_m = sum(precip_mm_gridmet, na.rm = TRUE) / 1000,
    precip_m_avg = mean(precip_mm_gridmet, na.rm = TRUE) / 1000,
    tmin_C_avg = mean(tmmn_C_gridmet, na.rm = TRUE),
    tmax_C_avg = mean(tmmx_C_gridmet, na.rm = TRUE),
    .groups = "drop"
  )

crop_climate <- readr::read_csv(
  here::here(
    "data",
    "crop_climate_gs_hist_baseline.csv"
  ),
  show_col_types = FALSE
) %>%
  transmute(
    FIPS = as.character(FIPS),
    Year,
    Crop = crop,
    GDD,
    precip_gs_mm
  )

county_hist <- county_base %>%
  left_join(
    crop_climate,
    by = c("FIPS", "Year", "Crop")
  ) %>%
  left_join(
    climate_annual,
    by = c("FIPS", "Year")
  )


## validation
expected_county_cols <- c(
  "Year",
  "FIPS",
  "LandCover",
  "area_m2",
  "area_ha",
  "area_prc",
  "LandCoverGroup",
  "Crop",
  "irrigation_WaterUse_m",
  "n_pdiv",
  "fertilizer_kgHa",
  "fertilizer_kgCrop",
  "GDD",
  "precip_gs_mm",
  "precip_m",
  "precip_m_avg",
  "tmin_C_avg",
  "tmax_C_avg"
)

county_hist <- county_hist %>%
  select(all_of(expected_county_cols))

stopifnot(
  nrow(county_hist) > 0,
  identical(sort(unique(county_hist$Year)), years_hist),
  !anyNA(county_hist$FIPS),
  !anyNA(county_hist$LandCover),
  all(county_hist$LandCoverGroup != "all cult", na.rm = TRUE)
)

## write 
write_csv(
  county_hist,
  here::here(
    "hpc_opt",
    "outputs",
    "common_inputs_county_hist_baseline.csv"
  )
)
  