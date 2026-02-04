message("✅ global.R is running")


library(dplyr)
library(tidyr)
library(purrr)
library(here)

source(here("StakeholderMeetingTradeoffs/R_app/eval_candidate.R"))
source(here("StakeholderMeetingTradeoffs/R_app/eval_candidate_all_years.R"))
source(here("StakeholderMeetingTradeoffs/R_app/irr_allocation.R"))
#source(here("StakeholderMeetingTradeoffs/models_app/wq_predict.R"))
source(here("StakeholderMeetingTradeoffs/models_app/wq_refined_predict.R"))
source(here("StakeholderMeetingTradeoffs/R_app/policy.R"))


years_vec <- 2006:2023

df_opt      <- readRDS(here("StakeholderMeetingTradeoffs/data_app/df_opt.rds"))
lu_baseline <- readRDS(here("StakeholderMeetingTradeoffs/data_app/lu_baseline.rds"))
hist_mix <- readRDS(here("StakeholderMeetingTradeoffs/data_app/hist_mix.rds"))  # if you want it
fert_ref_by_year <- readRDS(here("StakeholderMeetingTradeoffs/data_app/fert_ref_by_year.rds"))

stopifnot(all(years_vec %in% fert_ref_by_year$Year))
stopifnot(!anyNA(fert_ref_by_year$Fert_ref_kg))

#wq_model_log1p  <- readRDS(here("StakeholderMeetingTradeoffs/models_app/wq_model_log1p.rds"))
wq_model_mlr   <- readRDS(here("StakeholderMeetingTradeoffs/models_app/wq_model_mlr.rds"))
wq_base_by_year <- readRDS(here("StakeholderMeetingTradeoffs/data_app/wq_base_by_year.rds"))

baseline_irrig_frac <- 0.48

