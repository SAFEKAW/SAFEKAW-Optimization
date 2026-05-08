suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tibble)
})

# ---- source helpers + engine ----
source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","11_models_load.R"))
source(here("hpc_opt","R","12_irrigation_predict.R"))
source(here("hpc_opt","R","13_yield_predict.R"))
source(here("hpc_opt","R","17_fert_multiplier.R"))
source(here("hpc_opt","R","18_irrig_allocation.R"))
source(here("hpc_opt","R","33_eval_engine.R"))
source(here("hpc_opt","R","34_read_config_yaml.R"))
source(here("hpc_opt","R","35_build_scenario_path.R"))
source(here("hpc_opt","R","36_build_policy_from_configs.R"))


# ---- helper: map climate pathway to periods ----
assign_period <- function(Y) {
  if (Y >= 2025 && Y <= 2049) return("early")
  if (Y >= 2050 && Y <= 2074) return("mid")
  if (Y >= 2075 && Y <= 2099) return("late")
  stop("Year outside expected future range: ", Y)
}

get_precomp_for_year <- function(Y, precomp_early, precomp_mid, precomp_late) {
  p <- assign_period(Y)
  if (p == "early") return(precomp_early)
  if (p == "mid")   return(precomp_mid)
  if (p == "late")  return(precomp_late)
  stop("Could not assign precomp for Year = ", Y)
}

if (!exists("landuse_file")) {
  landuse_file <- here("hpc_opt","config","landuse","bau.yaml")
}
if (!exists("irrigation_file")) {
  irrigation_file <- here("hpc_opt","config","irrigation","current.yaml")
}
if (!exists("fertilizer_file")) {
  fertilizer_file <- here("hpc_opt","config","fertilizer","current.yaml")
}
if (!exists("climate_pathway") || is.null(climate_pathway) || climate_pathway == "") {
  climate_pathway <- "rcp45"
}
#climate_pathway <- "rcp45"
#climate_pathway <- "rcp85"


if (!exists("gcm_name") || is.null(gcm_name) || gcm_name == "") {
  gcm_name <- "ensemble"
}

message("Using landuse_file = ", landuse_file)
message("Using irrigation_file = ", irrigation_file)
message("Using fertilizer_file = ", fertilizer_file)
message("Using gcm_name = ", gcm_name)
message("Using climate_pathway = ", climate_pathway)


landuse_cfg    <- read_config_yaml(landuse_file)
irrigation_cfg <- read_config_yaml(irrigation_file)
fertilizer_cfg <- read_config_yaml(fertilizer_file)

combo_name <- paste(
  landuse_cfg$name,
  irrigation_cfg$name,
  fertilizer_cfg$name,
  gcm_name,
  climate_pathway,
  sep = "_"
)

message("Running deterministic factorial combo: ", combo_name)

# ---- load inputs ----
county_early <- read_csv(
  here("hpc_opt","outputs", paste0("common_inputs_county_", gcm_name, "_", climate_pathway, "_early.csv")),
  show_col_types = FALSE
)

county_mid <- read_csv(
  here("hpc_opt","outputs", paste0("common_inputs_county_", gcm_name, "_", climate_pathway, "_mid.csv")),
  show_col_types = FALSE
)

county_late <- read_csv(
  here("hpc_opt","outputs", paste0("common_inputs_county_", gcm_name, "_", climate_pathway, "_late.csv")),
  show_col_types = FALSE
)

df_county <- bind_rows(county_early, county_mid, county_late) %>%
  distinct() %>%
  mutate(FIPS = as.character(FIPS))


models <- load_models(here("hpc_opt","models"))
stopifnot(!is.null(models$wq_lm))
stopifnot(!is.null(models$irrigation_lm))
stopifnot(length(models$yield_kg) > 0)

# historical anchor only
baseline_reference <- readRDS(
  here("hpc_opt","outputs","precompute","baseline_reference.rds")
)

precomp_hist <- readRDS(
  here("hpc_opt","outputs","precompute","precomp_hist_baseline.rds")
)

# future climate precomp bundles by period
precomp_early <- readRDS(
  here("hpc_opt","outputs","precompute", paste0("precomp_", gcm_name, "_", climate_pathway, "_early.rds"))
)

precomp_mid <- readRDS(
  here("hpc_opt","outputs","precompute", paste0("precomp_", gcm_name, "_", climate_pathway, "_mid.rds"))
)

precomp_late <- readRDS(
  here("hpc_opt","outputs","precompute", paste0("precomp_", gcm_name, "_", climate_pathway, "_late.rds"))
)

# build one continuous future baseline table from period-specific precomp objects
lu_baseline <- bind_rows(
  precomp_early$lu_baseline,
  precomp_mid$lu_baseline,
  precomp_late$lu_baseline
) %>%
  distinct(Year, .keep_all = TRUE) %>%
  arrange(Year) %>%
  filter(Year >= 2025, Year <= 2099)

stopifnot(nrow(lu_baseline) > 0)

stopifnot(
  all(precomp_early$years_vec %in% 2025:2049),
  all(precomp_mid$years_vec   %in% 2050:2074),
  all(precomp_late$years_vec  %in% 2075:2099)
)

stopifnot(
  min(lu_baseline$Year) == 2025,
  max(lu_baseline$Year) == 2099,
  all(sort(unique(lu_baseline$Year)) == 2025:2099)
)

lu_baseline %>%
  select(Year, LC_cult_base, LC_dev_base, LC_total) %>%
  print(n = 100)

# use historical structural references
hist_mix            <- precomp_hist$hist_mix
baseline_irrig_frac <- precomp_hist$baseline_irrig_frac



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
) %>%
  mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)



# ---- build cumulative scenario path + policy ----
baseline_reference$cult_start_m2 <- precomp_hist$lu_baseline %>%
  arrange(Year) %>%
  filter(Year >= max(Year, na.rm = TRUE) - 4) %>%
  summarise(cult_start_m2 = mean(LC_cult_base, na.rm = TRUE)) %>%
  pull(cult_start_m2)

scenario_path <- build_scenario_path(
  landuse_cfg = landuse_cfg,
  lu_baseline = lu_baseline,
  baseline_reference = baseline_reference
) %>%
  mutate(period = vapply(Year, assign_period, character(1)))

policy_base <- build_policy_from_configs(
  irrigation_cfg = irrigation_cfg,
  fertilizer_cfg = fertilizer_cfg
)

stopifnot(all(abs(with(scenario_path, corn + soy + sor + wheat) - 1) < 1e-8))
stopifnot(all(is.finite(scenario_path$cult_area_factor)))
stopifnot(all(scenario_path$Cult_m2 <= scenario_path$cap_cult_year_m2 + 1e-8))

# ---- evaluate one future year at a time, switching precomp by period ----
res_list <- lapply(seq_len(nrow(scenario_path)), function(i) {
  s <- scenario_path[i, ]
  
  pc <- get_precomp_for_year(
    Y = s$Year,
    precomp_early = precomp_early,
    precomp_mid   = precomp_mid,
    precomp_late  = precomp_late
  )
  
  shares_override <- c(
    Corn     = s$corn,
    Soybeans = s$soy,
    Sorghum  = s$sor,
    Wheat    = s$wheat
  )
  
 # policy <- c(
#    list(cult_area_factor = s$cult_area_factor),
#    policy_base
#  )
  
  policy <- c(
    list(
      cult_area_factor = s$cult_area_factor,
      debug = i == 1
    ),
    policy_base
  )
  
  vals <- eval_candidate_year(
    x = c(0, 0, 0),
    Y = s$Year,
    df_county = df_county,
    irrigation_lm = models$irrigation_lm,
    yield_kg_models = models$yield_kg,
    yield_kcal_models = models$yield_kcal,
    wq_model_mlr = models$wq_lm,
    wq_base_by_year = pc$wq_base_by_year,
    lu_baseline = pc$lu_baseline,
    universal_costs = universal_costs,
    crop_params = crop_params,
    baseline_irrig_frac = pc$baseline_irrig_frac,
    fert_ref_by_year = pc$fert_ref_by_year,
    policy = policy,
    hist_mix = pc$hist_mix,
    shares_override = shares_override
  )
  
  tibble(
    combo_name = combo_name,
    climate_pathway = climate_pathway,
    period = s$period,
    landuse_name = landuse_cfg$name,
    irrigation_name = irrigation_cfg$name,
    fertilizer_name = fertilizer_cfg$name,
    Year = s$Year,
    cult_area_factor = s$cult_area_factor,
    Cult_m2 = s$Cult_m2,
    corn = s$corn,
    soy = s$soy,
    sor = s$sor,
    wheat = s$wheat,
    irrig_frac_factor = policy$irrig_frac_factor,
    irr_eff = policy$irr_eff,
    irrig_area_cap_m2 = as.numeric(policy$irrig_area_cap_m2),
    cap_rule = policy$cap_rule,
    cap_hit = all(vals == 1e12),
    fert_factor = policy$fert_factor,
    nitrate_kgyr = vals[1],
    neg_profit_usdyr = vals[2],
    irrigation_m3yr = vals[3]
  )
})

res_df <- bind_rows(res_list)

# ---- summaries ----
summary_period_df <- res_df %>%
  group_by(combo_name, climate_pathway, period,
           landuse_name, irrigation_name, fertilizer_name) %>%
  summarise(
    mean_nitrate_kgyr = mean(nitrate_kgyr, na.rm = TRUE),
    mean_profit_usdyr = mean(-neg_profit_usdyr, na.rm = TRUE),
    mean_irrigation_m3yr = mean(irrigation_m3yr, na.rm = TRUE),
    .groups = "drop"
  )

summary_overall_df <- res_df %>%
  summarise(
    combo_name = first(combo_name),
    climate_pathway = first(climate_pathway),
    landuse_name = first(landuse_name),
    irrigation_name = first(irrigation_name),
    fertilizer_name = first(fertilizer_name),
    mean_nitrate_kgyr = mean(nitrate_kgyr, na.rm = TRUE),
    mean_profit_usdyr = mean(-neg_profit_usdyr, na.rm = TRUE),
    mean_irrigation_m3yr = mean(irrigation_m3yr, na.rm = TRUE)
  )

# ---- write outputs ----
outdir <- here("hpc_opt","outputs","factorial_runs", combo_name)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

message("About to write outputs to: ", outdir)
print(head(res_df))
print(summary_period_df)
print(summary_overall_df)

write_csv(
  scenario_path,
  file.path(outdir, paste0("scenario_path_", combo_name, ".csv"))
)

write_csv(
  res_df,
  file.path(outdir, paste0("deterministic_results_", combo_name, ".csv"))
)

write_csv(
  summary_period_df,
  file.path(outdir, paste0("deterministic_summary_by_period_", combo_name, ".csv"))
)

write_csv(
  summary_overall_df,
  file.path(outdir, paste0("deterministic_summary_overall_", combo_name, ".csv"))
)

message("DONE. Combo = ", combo_name, " -> ", outdir)

