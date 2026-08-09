#CONSTRAINED
suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(mco)
})

# ---- source engine + helpers ----
source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","11_models_load.R"))
source(here("hpc_opt","R","12_irrigation_predict.R"))
source(here("hpc_opt","R","13_yield_predict.R"))
source(here("hpc_opt","R","17_fert_multiplier.R"))
source(here("hpc_opt","R","18_irrig_allocation.R"))
source(here("hpc_opt","R","32_precomp_build.R"))
source(here("hpc_opt","R","33_eval_engine.R"))
source(here("hpc_opt","R","34_read_config_yaml.R"))
source(here("hpc_opt","R","36_build_policy_from_configs.R"))
source(here("hpc_opt","R","41_objective_wrapper_constrained.R"))

# ---- args (HPC friendly) ----
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

gcm_name <- get_arg("--gcm", "ensemble")
climate_pathway <- get_arg("--climate", "rcp45")
period <- get_arg("--period", "late")
landuse_name <- get_arg("--landuse", "fixed")
seed <- as.integer(get_arg("--seed", "1"))
popsize <- as.integer(get_arg("--popsize", "8"))
generations <- as.integer(get_arg("--generations", "3"))
phase <- get_arg("--phase", "crop_bounds")
irrigation_name <- tolower(get_arg("--irrigation", "current"))
run_tag <- get_arg("--run-tag", phase)
baseline_extent_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "irrigation_extent", "baseline.yaml"
))
expanded_extent_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "irrigation_extent", "expanded.yaml"
))
current_fertilizer_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "fertilizer", "current.yaml"
))
efficient_fertilizer_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "fertilizer", "efficient.yaml"
))
fert_factor_min <- as.numeric(get_arg(
  "--fert-factor-min", as.character(efficient_fertilizer_cfg$fert_factor)
))
fert_factor_max <- as.numeric(get_arg(
  "--fert-factor-max", as.character(current_fertilizer_cfg$fert_factor)
))
irrig_frac_factor_min <- as.numeric(get_arg(
  "--irrig-frac-min", as.character(baseline_extent_cfg$irrig_frac_factor)
))
irrig_frac_factor_max <- as.numeric(get_arg(
  "--irrig-frac-max", as.character(expanded_extent_cfg$irrig_frac_factor)
))

supported_phases <- c("crop_bounds", "crop_fert", "crop_fert_irrigfrac")
if (!phase %in% supported_phases) {
  stop(
    "Unknown phase: ", phase, ". Supported phases are: ",
    paste(supported_phases, collapse = ", "), "."
  )
}
if (!irrigation_name %in% c("current", "efficient")) {
  stop("--irrigation must be either 'current' or 'efficient'.")
}
if (!grepl("^[A-Za-z0-9._-]+$", run_tag)) {
  stop("--run-tag may contain only letters, numbers, dot, underscore, and hyphen.")
}

irrigation_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "irrigation_technology", paste0(irrigation_name, ".yaml")
))
scenario_irr_eff <- as.numeric(irrigation_cfg$irr_eff)
if (!is.finite(scenario_irr_eff) || scenario_irr_eff < 1) {
  stop("Irrigation scenario config must define finite irr_eff >= 1.")
}
scenario_water_savings <- 1 - 1 / scenario_irr_eff
irrigation_extent_name <- if (
  phase == "crop_fert_irrigfrac"
) "optimized" else "baseline"
fertilizer_name <- if (phase == "crop_bounds") "current" else "optimized"

if (!is.finite(fert_factor_min) || !is.finite(fert_factor_max) ||
    fert_factor_min <= 0 || fert_factor_min > fert_factor_max) {
  stop("Invalid fertilizer-factor bounds.")
}
if (!is.finite(irrig_frac_factor_min) || !is.finite(irrig_frac_factor_max) ||
    irrig_frac_factor_min < 0 ||
    irrig_frac_factor_min > irrig_frac_factor_max) {
  stop("Invalid irrigated-fraction factor bounds.")
}
annual_crop_change <- as.numeric(get_arg("--annual-crop-change", "0.01"))

scenario_name <- paste(
  landuse_name, irrigation_name, gcm_name, climate_pathway, period, sep = "_"
)

message(
  "Running optimization scenario: ", scenario_name,
  " | phase=", phase,
  " | irrigation savings=", round(100 * scenario_water_savings, 1), "%"
)

# ---- load models ----
models <- load_models(here("hpc_opt", "models"))

stopifnot(!is.null(models$irrigation_lm))
stopifnot(!is.null(models$yield_kg))
stopifnot(!is.null(models$yield_kcal))
stopifnot(!is.null(models$wq_lm))

# ---- economics ----
universal_costs <- list(
  irr_cost_per_m3 = 0.029,
  fert_share_direct = 0.2,
  nutrient_mgmt_full_cost_per_acre = 34.30,
  nutrient_mgmt_max_reduction = 0.30
)

crop_params <- tibble::tribble(
  ~Crop,      ~income_per_kg, ~direct_cost_per_kg, ~fixed_cost_per_kg, ~total_cost_per_kg, ~fert_kgha,
  "Wheat",     0.2,           0.12,                0.05,               0.17,               90,
  "Corn",      0.18,          0.11,                0.05,               0.16,              250,
  "Sorghum",   0.17,          0.09,                0.06,               0.15,              110,
  "Soybeans",  0.37,          0.18,                0.14,               0.32,               55
) %>%
  mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)

# ---- context-specific input files ----
df_county_file <- here(
  "hpc_opt", "outputs",
  sprintf("common_inputs_county_%s_%s_%s.csv", gcm_name, climate_pathway, period)
)

precomp_file <- here(
  "hpc_opt", "outputs", "precompute",
  sprintf("precomp_%s_%s_%s.rds", gcm_name, climate_pathway, period)
)

scenario_path_file <- resolve_baseline_scenario_path(
  landuse_name, gcm_name, climate_pathway, require_existing = FALSE
)

if (!file.exists(df_county_file)) stop("Missing county input file: ", df_county_file)
if (!file.exists(precomp_file)) stop("Missing precompute file: ", precomp_file)
if (!file.exists(scenario_path_file)) stop("Missing scenario path file: ", scenario_path_file)

# ---- load county inputs ----
df_county <- read_csv(df_county_file, show_col_types = FALSE) %>%
  mutate(
    FIPS = as.character(FIPS),
    Year = as.integer(Year)
  )

# ---- load precompute ----
precomp <- readRDS(precomp_file)

# ---- load land-use trajectory ----
scenario_path <- read_csv(scenario_path_file, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year))

# ---- overlapping years ----
years_vec <- Reduce(
  intersect,
  list(
    as.integer(precomp$years_vec),
    unique(as.integer(df_county$Year)),
    unique(as.integer(scenario_path$Year))
  )
)

if (length(years_vec) == 0) {
  stop("No overlapping years among precomp, df_county, and scenario_path.")
}

wq_base_by_year <- precomp$wq_base_by_year
fert_ref_by_year <- precomp$fert_ref_by_year
baseline_irrig_frac <- precomp$baseline_irrig_frac
hist_irrig_mix <- precomp$hist_mix
irrigation_reference <- precomp$irrigation_reference

if (is.null(irrigation_reference)) {
  stop("Precompute is missing the dual-domain irrigation reference.")
}

alluvial_reference_totals <- irrigation_reference$totals_by_year %>%
  filter(Domain == "alluvial") %>%
  summarise(
    expansion_eligible_area_m2 = mean(expansion_eligible_area_m2, na.rm = TRUE),
    all_reported_irrigated_area_m2 =
      mean(irrigated_area_all_reported_m2, na.rm = TRUE),
    modeled_crop_irrigated_area_m2 =
      mean(irrigated_area_modeled_crops_m2, na.rm = TRUE)
  )

alluvial_non_target_irrigated_area_m2 <-
  alluvial_reference_totals$all_reported_irrigated_area_m2 -
  alluvial_reference_totals$modeled_crop_irrigated_area_m2

alluvial_target_irrig_area_cap_m2 <- pmax(
  0,
  alluvial_reference_totals$expansion_eligible_area_m2 -
    alluvial_non_target_irrigated_area_m2
)

required_hist_mix_cols <- c("Crop", "irrig_frac")
if (!is.data.frame(hist_irrig_mix) || nrow(hist_irrig_mix) == 0 ||
    !all(required_hist_mix_cols %in% names(hist_irrig_mix))) {
  stop(
    "The precompute file has no valid crop-specific historical irrigation mix. ",
    "Rebuild it with hpc_opt/scripts/03_precompute_inputs.R after the historical-reference fix."
  )
}

# ---- crop-composition reference for dynamic transition bounds ----
crop_names <- c("Corn", "Soybeans", "Sorghum", "Wheat")
baseline_reference_file <- here(
  "hpc_opt", "outputs", "precompute", "baseline_reference.rds"
)
if (!file.exists(baseline_reference_file)) {
  stop("Missing crop-composition baseline reference: ", baseline_reference_file)
}
baseline_reference <- readRDS(baseline_reference_file)

historical_crop_mix <- baseline_reference$obs_tbl %>%
  transmute(Crop = as.character(Crop), ref_share = as.numeric(obs_share)) %>%
  tidyr::complete(Crop = crop_names, fill = list(ref_share = 0)) %>%
  arrange(match(Crop, crop_names))

scenario_period_mixes <- scenario_path %>%
  filter(period %in% c("early", "mid", "late")) %>%
  group_by(period) %>%
  summarise(
    Corn = mean(corn, na.rm = TRUE),
    Soybeans = mean(soy, na.rm = TRUE),
    Sorghum = mean(sor, na.rm = TRUE),
    Wheat = mean(wheat, na.rm = TRUE),
    .groups = "drop"
  )

reference_period <- switch(
  period,
  early = "historical",
  mid = "early",
  late = "mid",
  stop("Unsupported optimization period: ", period)
)

if (reference_period == "historical") {
  crop_ref_mix <- setNames(historical_crop_mix$ref_share, historical_crop_mix$Crop)
} else {
  reference_row <- scenario_period_mixes %>% filter(period == reference_period)
  if (nrow(reference_row) != 1) {
    stop("Could not find one deterministic crop reference for period: ", reference_period)
  }
  crop_ref_mix <- unlist(reference_row[1, crop_names], use.names = TRUE)
}

transition_years <- 25
crop_bounds <- make_dynamic_crop_bounds(
  ref_mix = crop_ref_mix,
  annual_relative_change = annual_crop_change,
  transition_years = transition_years
) %>%
  mutate(
    optimization_period = period,
    reference_period = reference_period
  )

# ---- land-use baseline for optimization context ----
LC_dev_base_ref <- mean(precomp$lu_baseline$LC_dev_base, na.rm = TRUE)

lu_baseline <- scenario_path %>%
  filter(Year %in% years_vec) %>%
  transmute(
    Year,
    LC_cult_base = Cult_m2,
    LC_grass_base = grass_mean_m2,
    LC_dev_base = LC_dev_base_ref,
    LC_total = LC_cult_base + LC_grass_base + LC_dev_base
  )

# ---- fixed/current management policy for Step 1 ----
policy <- list(
  cult_area_factor = 1,
  irrig_frac_factor = as.numeric(baseline_extent_cfg$irrig_frac_factor),
  irr_eff = scenario_irr_eff,
  fert_factor = 1,
  fert_gamma = 0.1,
  alluvial_target_irrig_area_cap_m2 = alluvial_target_irrig_area_cap_m2,
  debug_crop_alloc = FALSE
)


# ---- objective fn for nsga2 (minimize all 3) ----
fn <- make_objective_wrapper(
  years_vec = years_vec,
  df_county = df_county,
  irrigation_lm = models$irrigation_lm,
  yield_kg_models = models$yield_kg,
  yield_kcal_models = models$yield_kcal,
  wq_model_mlr = models$wq_lm,
  wq_base_by_year = wq_base_by_year,
  lu_baseline = lu_baseline,
  universal_costs = universal_costs,
  crop_params = crop_params,
  baseline_irrig_frac = baseline_irrig_frac,
  hist_mix = hist_irrig_mix,
  irrigation_reference = irrigation_reference,
  fert_ref_by_year = fert_ref_by_year,
  policy = policy,
  crop_bounds = crop_bounds,
  phase = phase
)


# ---- output directory ----
outdir <- here("hpc_opt", "outputs", "runs", run_tag, scenario_name)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

set.seed(seed)

# ---- decision-variable dimensions and bounds by optimization phase ----
if (phase == "crop_bounds") {
  idim <- 4
  lower <- rep(0, 4)
  upper <- rep(1, 4)
}

if (phase == "crop_fert") {
  idim <- 5
  lower <- c(rep(0, 4), fert_factor_min)
  upper <- c(rep(1, 4), fert_factor_max)
}

if (phase == "crop_fert_irrigfrac") {
  idim <- 6
  lower <- c(
    rep(0, 4), fert_factor_min, irrig_frac_factor_min
  )
  upper <- c(
    rep(1, 4), fert_factor_max, irrig_frac_factor_max
  )
}

if (!exists("idim")) {
  stop("Unknown optimization phase: ", phase)
}

res <- mco::nsga2(
  fn,
  idim = idim,
  odim = 3,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize = popsize,
  generations = generations
)

params <- as.data.frame(t(apply(
  res$par[, 1:4, drop = FALSE],
  1,
  decode_bounded_crop_shares,
  crop_bounds = crop_bounds
))) %>%
  mutate(
    scenario_name = scenario_name,
    seed = seed
  )

if (idim >= 5) params$fert_factor <- res$par[, 5]
if (phase == "crop_fert_irrigfrac") {
  params$irrig_frac_factor <- res$par[, 6]
}
if (!"irrig_frac_factor" %in% names(params)) params$irrig_frac_factor <- 1
params$irrigation_name <- irrigation_name
params$irrigation_technology_name <- irrigation_name
params$irrigation_extent_name <- irrigation_extent_name
params$fertilizer_name <- fertilizer_name
params$irr_eff <- scenario_irr_eff
params$irrigation_water_savings_fraction <- scenario_water_savings

if (idim >= 5) params$fertilizer_reduction_fraction <- 1 - params$fert_factor

if (idim >= 5) {
  params$nutrient_management_cost_usdyr <-
    (mean(lu_baseline$LC_cult_base, na.rm = TRUE) / 4046.8564224) *
    universal_costs$nutrient_mgmt_full_cost_per_acre *
    pmin(
      1,
      params$fertilizer_reduction_fraction /
        universal_costs$nutrient_mgmt_max_reduction
    )
}

mean_cultivated_area_m2 <- mean(lu_baseline$LC_cult_base, na.rm = TRUE)
irrigation_area_out <- bind_rows(lapply(seq_len(nrow(params)), function(i) {
  shares_i <- unlist(params[i, c("Corn", "Soybeans", "Sorghum", "Wheat")])
  summarize_candidate_irrigation_area(
    crop_shares = shares_i,
    cultivated_area_m2 = mean_cultivated_area_m2,
    irrigated_fraction = baseline_irrig_frac * params$irrig_frac_factor[i],
    hist_mix = hist_irrig_mix,
    irrigation_reference = irrigation_reference
  ) %>%
    mutate(solution_id = i, .before = 1)
}))

irrigation_area_totals <- irrigation_area_out %>%
  group_by(solution_id) %>%
  summarise(
    basin_irrigated_area_m2 = sum(basin_irrigated_area_m2, na.rm = TRUE),
    basin_irrigated_fraction_of_model_cultivated =
      sum(basin_irrigated_fraction_of_model_cultivated, na.rm = TRUE),
    estimated_alluvial_irrigated_area_m2 =
      sum(estimated_alluvial_irrigated_area_m2, na.rm = TRUE),
    estimated_alluvial_irrigated_fraction_of_cdl_cultivated =
      sum(estimated_alluvial_irrigated_fraction_of_cdl_cultivated, na.rm = TRUE),
    estimated_alluvial_irrigated_fraction_of_expansion_eligible =
      sum(estimated_alluvial_irrigated_fraction_of_expansion_eligible, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    alluvial_target_irrig_area_cap_m2 = alluvial_target_irrig_area_cap_m2,
    alluvial_target_irrig_area_headroom_m2 =
      alluvial_target_irrig_area_cap_m2 -
      estimated_alluvial_irrigated_area_m2,
    alluvial_target_irrig_area_cap_fraction =
      estimated_alluvial_irrigated_area_m2 /
      alluvial_target_irrig_area_cap_m2
  )

params <- params %>%
  mutate(solution_id = row_number(), .before = 1) %>%
  left_join(irrigation_area_totals, by = "solution_id")

objectives <- as.data.frame(res$value) %>%
  setNames(c("nitrate", "profit_minimized", "irrigation")) %>%
  mutate(
    profit = -profit_minimized,
    scenario_name = scenario_name,
    seed = seed
  )


pareto_out <- bind_cols(
  params,
  objectives %>% select(nitrate, irrigation, profit, profit_minimized)
) %>%
  mutate(
    crop_bounds_feasible =
      Corn >= crop_bounds$lower[crop_bounds$Crop == "Corn"] &
      Corn <= crop_bounds$upper[crop_bounds$Crop == "Corn"] &
      Soybeans >= crop_bounds$lower[crop_bounds$Crop == "Soybeans"] &
      Soybeans <= crop_bounds$upper[crop_bounds$Crop == "Soybeans"] &
      Sorghum >= crop_bounds$lower[crop_bounds$Crop == "Sorghum"] &
      Sorghum <= crop_bounds$upper[crop_bounds$Crop == "Sorghum"] &
      Wheat >= crop_bounds$lower[crop_bounds$Crop == "Wheat"] &
      Wheat <= crop_bounds$upper[crop_bounds$Crop == "Wheat"],
    irrigation_extent_feasible =
      irrig_frac_factor >= irrig_frac_factor_min - 1e-10 &
      irrig_frac_factor <= irrig_frac_factor_max + 1e-10,
    alluvial_area_feasible =
      estimated_alluvial_irrigated_area_m2 <=
        alluvial_target_irrig_area_cap_m2 + 1e-6,
    feasible =
      crop_bounds_feasible &
      irrigation_extent_feasible &
      alluvial_area_feasible,
    phase = phase,
    run_tag = run_tag,
    popsize = popsize,
    generations = generations,
    scenario_name = scenario_name,
    gcm_name = gcm_name,
    climate_pathway = climate_pathway,
    period = period,
    landuse_name = landuse_name,
    irrigation_name = irrigation_name,
    irrigation_technology_name = irrigation_name,
    irrigation_extent_name = irrigation_extent_name,
    fertilizer_name = fertilizer_name,
    irr_eff = scenario_irr_eff,
    irrigation_water_savings_fraction = scenario_water_savings,
    irrig_frac_factor_scenario_baseline =
      as.numeric(baseline_extent_cfg$irrig_frac_factor),
    irrig_frac_factor_scenario_expanded =
      as.numeric(expanded_extent_cfg$irrig_frac_factor),
    seed = seed,

    crop_bound_type = "dynamic_relative_transition",
    annual_relative_change = annual_crop_change,
    transition_years = transition_years,
    crop_reference_period = reference_period,
    irrig_frac_factor_min = irrig_frac_factor_min,
    irrig_frac_factor_max = irrig_frac_factor_max
  )

saveRDS(
  res,
  file.path(outdir, sprintf("nsga2_res_%s_seed%d.rds", scenario_name, seed))
)


required_cols <- c(
  "scenario_name", "gcm_name", "climate_pathway", "period", "landuse_name",
  "irrigation_name", "irrigation_technology_name", "irrigation_extent_name",
  "fertilizer_name", "irr_eff", "irrigation_water_savings_fraction", "seed",
  "phase", "run_tag", "popsize", "generations",
  "Corn", "Soybeans", "Sorghum", "Wheat",
  "nitrate", "irrigation", "profit", "profit_minimized",
  "feasible"
)

missing_cols <- setdiff(required_cols, names(pareto_out))

if (length(missing_cols) > 0) {
  stop("Missing required output columns: ", paste(missing_cols, collapse = ", "))
}

if (any(is.na(pareto_out[required_cols]))) {
  stop("Required metadata/objective columns contain NA values.")
}

write_csv(
  pareto_out,
  file.path(outdir, sprintf("pareto_front_%s_seed%d.csv", scenario_name, seed))
)

write_csv(
  crop_bounds,
  file.path(outdir, sprintf("crop_bounds_%s_seed%d.csv", scenario_name, seed))
)

write_csv(
  irrigation_area_out,
  file.path(outdir, sprintf("irrigation_area_by_domain_crop_%s_seed%d.csv", scenario_name, seed))
)

message("DONE. Scenario=", scenario_name, " seed=", seed, " -> ", outdir)
