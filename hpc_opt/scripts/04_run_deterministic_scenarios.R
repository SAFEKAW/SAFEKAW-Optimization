suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tibble)
})

# ---- source engine + helpers ----
source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","11_models_load.R"))
source(here("hpc_opt","R","17_fert_multiplier.R"))
source(here("hpc_opt","R","18_irrig_allocation.R"))
source(here("hpc_opt","R","33_eval_engine.R"))
source(here("hpc_opt","R","34_read_scenario_yaml.R"))
source(here("hpc_opt","R","35_build_scenario_path.R"))

# ---- args ----
#args <- commandArgs(trailingOnly = TRUE)
#get_arg <- function(flag, default = NULL) {
#  i <- match(flag, args)
#  if (is.na(i) || i == length(args)) return(default)
#  args[[i + 1]]
#}
#scenario_file <- get_arg("--scenario", here("hpc_opt","config","flatline_hist.yaml"))

#cfg <- read_scenario_yaml(scenario_file)
#message("Running deterministic scenario: ", cfg$scenario_name)

#scenario_file <- here("hpc_opt","config","flatline_hist.yaml")
scenario_file <- here("hpc_opt","config","bau_expansion_hist.yaml")
cfg <- read_scenario_yaml(scenario_file)

# ---- load inputs ----
df_int_crop_areanorm <- read_csv(
  here("hpc_opt","outputs","integration","int_crop_areanorm_annual.csv"),
  show_col_types = FALSE
)

models <- load_models(here("hpc_opt","models"))
stopifnot(!is.null(models$wq_lm))

precomp <- readRDS(here("hpc_opt","outputs","precompute","precomp_baseline.rds"))
baseline_reference <- readRDS(here("hpc_opt","outputs","precompute","baseline_reference.rds"))

years_vec           <- precomp$years_vec
lu_baseline         <- precomp$lu_baseline
wq_base_by_year     <- precomp$wq_base_by_year
fert_ref_by_year    <- precomp$fert_ref_by_year
baseline_irrig_frac <- precomp$baseline_irrig_frac
hist_mix            <- precomp$hist_mix

# ---- economics ----
universal_costs <- list(
  irr_cost_per_m3 = 0.029,
  fert_share_direct = 0.2
)

crop_params <- tibble::tribble(
  ~Crop,      ~income_per_kg, ~direct_cost_per_kg, ~fixed_cost_per_kg, ~total_cost_per_kg, ~fert_kgha,
  "Wheat",     0.2,           0.12,                0.05,               0.17,               90,
  "Corn",      0.18,          0.11,                0.05,               0.15,              250,
  "Sorghum",   0.17,          0.09,                0.06,               0.15,              110,
  "Soybeans",  0.37,          0.18,                0.14,               0.32,               55
) %>% mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)

df_opt <- df_int_crop_areanorm %>%
  left_join(crop_params, by = "Crop") %>%
  rename(
    Yield_kgHa   = Yield_kgHa,
    Irrigation_m = Irrigation_m
  )

# ---- build scenario path ----
scenario_path <- build_scenario_path(
  cfg = cfg,
  lu_baseline = lu_baseline,
  baseline_reference = baseline_reference
)

# ---- evaluate one year at a time ----
res_list <- lapply(seq_len(nrow(scenario_path)), function(i) {
  s <- scenario_path[i, ]
  
  shares_override <- c(
    Corn     = s$corn,
    Soybeans = s$soy,
    Sorghum  = s$sor,
    Wheat    = s$wheat
  )
  
  policy <- list(
    cult_area_factor  = s$cult_area_factor,
    irrig_frac_factor = s$irrig_frac_factor,
    irr_eff           = 1,
    fert_factor       = s$fert_factor,
    fert_gamma        = 0.1
  )
  
  vals <- eval_candidate_year(
    x = c(0, 0, 0),   # ignored when shares_override is supplied
    Y = s$Year,
    df_opt = df_opt,
    wq_model_mlr = models$wq_lm,
    wq_base_by_year = wq_base_by_year,
    lu_baseline = lu_baseline,
    universal_costs = universal_costs,
    crop_params = crop_params,
    baseline_irrig_frac = baseline_irrig_frac,
    fert_ref_by_year = fert_ref_by_year,
    policy = policy,
    hist_mix = hist_mix,
    shares_override = shares_override
  )
  
  tibble(
    scenario_name = s$scenario_name,
    Year = s$Year,
    cult_area_factor = s$cult_area_factor,
    Cult_m2 = s$Cult_m2,
    cap_cult_m2 = s$cap_cult_m2,
    corn = s$corn,
    soy = s$soy,
    sor = s$sor,
    wheat = s$wheat,
    irrig_frac_factor = s$irrig_frac_factor,
    fert_factor = s$fert_factor,
    nitrate_kgyr = vals[1],
    neg_profit_usdyr = vals[2],
    irrigation_m3yr = vals[3]
  )
})

res_df <- bind_rows(res_list)

summary_df <- res_df %>%
  summarise(
    scenario_name = first(scenario_name),
    mean_nitrate_kgyr = mean(nitrate_kgyr, na.rm = TRUE),
    mean_profit_usdyr = mean(-neg_profit_usdyr, na.rm = TRUE),
    mean_irrigation_m3yr = mean(irrigation_m3yr, na.rm = TRUE)
  )

# ---- write outputs ----
outdir <- here("hpc_opt","outputs","runs", cfg$scenario_name)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

message("About to write outputs to: ", outdir)
print(head(res_df))
print(summary_df)

write_csv(scenario_path, file.path(outdir, sprintf("scenario_path_%s.csv", cfg$scenario_name)))
write_csv(res_df, file.path(outdir, sprintf("deterministic_results_%s.csv", cfg$scenario_name)))
write_csv(summary_df, file.path(outdir, sprintf("deterministic_summary_%s.csv", cfg$scenario_name)))

message("DONE. Scenario=", cfg$scenario_name, " -> ", outdir)