library(tidyr)
library(readr)
library(dplyr)
library(here)

maca_params <- crossing(
  scenario_tag = c(
    "rcp45_early", "rcp45_mid", "rcp45_late",
    "rcp85_early", "rcp85_mid", "rcp85_late"
  ),
  maca_model = c(
    "CCSM4",
    "BCC-CSM1-1",
    "GFDL-ESM2M",
    "CanESM2",
    "CNRM-CM5"
  )
) %>%
  mutate(
    gcm_name = case_when(
      maca_model == "CCSM4"     ~ "ccsm4",
      maca_model == "BCC-CSM1-1" ~ "bcc_csm1_1",
      maca_model == "GFDL-ESM2M" ~ "gfdl_esm2m",
      maca_model == "CanESM2"   ~ "canesm2",
      maca_model == "CNRM-CM5"  ~ "cnrm_cm5"
    )
  )

write_csv(maca_params, here("hpc_opt/config/climate", "maca_param_grid.csv"))
