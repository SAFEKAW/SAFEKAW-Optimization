#UNCONSTRAINED
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
source(here("hpc_opt","R","41_objective_wrapper.R"))

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
run_tag <- get_arg("--run-tag", "")

if (!is.finite(popsize) || popsize < 4 || popsize %% 4 != 0) {
  stop("--popsize must be a positive multiple of 4 (minimum 4).")
}
if (!is.finite(generations) || generations < 1) {
  stop("--generations must be a positive integer.")
}
if (nzchar(run_tag) && !grepl("^[A-Za-z0-9._-]+$", run_tag)) {
  stop("--run-tag may contain only letters, numbers, dot, underscore, and hyphen.")
}

scenario_name <- paste(landuse_name, gcm_name, climate_pathway, period, sep = "_")

message("Running optimization scenario: ", scenario_name)

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
hist_mix <- precomp$hist_mix
irrigation_reference <- precomp$irrigation_reference

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

# ---- shared baseline management definitions for theoretical benchmark ----
irrigation_technology_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "irrigation_technology", "current.yaml"
))
irrigation_extent_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "irrigation_extent", "baseline.yaml"
))
fertilizer_cfg <- read_config_yaml(here(
  "hpc_opt", "config", "fertilizer", "current.yaml"
))
policy <- c(
  list(cult_area_factor = 1, debug_crop_alloc = FALSE),
  build_policy_from_configs(
    irrigation_technology_cfg = irrigation_technology_cfg,
    irrigation_extent_cfg = irrigation_extent_cfg,
    fertilizer_cfg = fertilizer_cfg
  )
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
  hist_mix = hist_mix,
  irrigation_reference = irrigation_reference,
  fert_ref_by_year = fert_ref_by_year,
  policy = policy
)


# ---- output directory ----
outdir <- if (nzchar(run_tag)) {
  here("hpc_opt", "outputs", "runs", run_tag, scenario_name)
} else {
  here("hpc_opt", "outputs", "runs", scenario_name)
}
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

set.seed(seed)

lower <- rep(0, 4)
upper <- rep(1, 4)

res <- mco::nsga2(
  fn,
  idim = 4,
  odim = 3,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize = popsize,
  generations = generations
)

params <- as.data.frame(t(apply(res$par, 1, decode_crop_shares))) %>%
  mutate(
    solution_id = row_number(),
    scenario_name = scenario_name,
    seed = seed
  )

mean_cultivated_area_m2 <- mean(lu_baseline$LC_cult_base, na.rm = TRUE)
irrigation_area_out <- bind_rows(lapply(seq_len(nrow(params)), function(i) {
  shares_i <- unlist(params[i, c("Corn", "Soybeans", "Sorghum", "Wheat")])
  summarize_candidate_irrigation_area(
    crop_shares = shares_i,
    cultivated_area_m2 = mean_cultivated_area_m2,
    irrigated_fraction = baseline_irrig_frac,
    hist_mix = hist_mix,
    irrigation_reference = irrigation_reference
  ) %>%
    mutate(solution_id = i, .before = 1)
}))

params <- params %>%
  left_join(
    irrigation_area_out %>%
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
      ),
    by = "solution_id"
  )

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
    feasible = Corn >= 0 & Soybeans >= 0 & Sorghum >= 0 & Wheat >= 0,
    scenario_name = scenario_name,
    gcm_name = gcm_name,
    climate_pathway = climate_pathway,
    period = period,
    landuse_name = landuse_name,
    irrigation_technology_name = irrigation_technology_cfg$name,
    irrigation_extent_name = irrigation_extent_cfg$name,
    fertilizer_name = fertilizer_cfg$name,
    irr_eff = as.numeric(irrigation_technology_cfg$irr_eff),
    irrigation_water_savings_fraction =
      1 - 1 / as.numeric(irrigation_technology_cfg$irr_eff),
    irrig_frac_factor = as.numeric(irrigation_extent_cfg$irrig_frac_factor),
    fert_factor = as.numeric(fertilizer_cfg$fert_factor),
    seed = seed,
    popsize = popsize,
    generations = generations,
    run_tag = run_tag
  )

saveRDS(
  res,
  file.path(outdir, sprintf("nsga2_res_%s_seed%d.rds", scenario_name, seed))
)


required_cols <- c(
  "scenario_name", "gcm_name", "climate_pathway", "period", "landuse_name",
  "irrigation_technology_name", "irrigation_extent_name", "fertilizer_name", "seed",
  "irr_eff", "irrigation_water_savings_fraction", "irrig_frac_factor", "fert_factor",
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
  irrigation_area_out,
  file.path(outdir, sprintf("irrigation_area_by_domain_crop_%s_seed%d.csv", scenario_name, seed))
)

message("DONE. Scenario=", scenario_name, " seed=", seed, " -> ", outdir)
