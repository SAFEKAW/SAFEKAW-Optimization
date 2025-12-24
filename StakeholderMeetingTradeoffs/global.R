message("✅ global.R is running")


library(dplyr)
library(tidyr)
library(purrr)

source("StakeholderMeetingTradeoffs/R_app/eval_candidate.R")
source("StakeholderMeetingTradeoffs/R_app/eval_candidate_all_years.R")
source("StakeholderMeetingTradeoffs/R_app/irr_allocation.R")
source("StakeholderMeetingTradeoffs/models_app/wq_predict.R")

years_vec <- 2006:2023

df_opt      <- readRDS("StakeholderMeetingTradeoffs/data_app/df_opt.rds")
lu_baseline <- readRDS("StakeholderMeetingTradeoffs/data_app/lu_baseline.rds")
hist_mix <- readRDS("StakeholderMeetingTradeoffs/data_app/hist_mix.rds")  # if you want it

wq_model_log1p  <- readRDS("StakeholderMeetingTradeoffs/models_app/wq_model_log1p.rds")
wq_base_by_year <- readRDS("StakeholderMeetingTradeoffs/data_app/wq_base_by_year.rds")
wq_context      <- readRDS("StakeholderMeetingTradeoffs/models_app/wq_context.rds")  # optional

baseline_irrig_frac <- 0.48
