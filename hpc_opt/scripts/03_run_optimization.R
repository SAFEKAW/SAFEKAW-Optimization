suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(mco)      # nsga2
})

# ---- source engine + helpers ----
source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","11_models_load.R"))
source(here("hpc_opt","R","17_fert_multiplier.R"))
source(here("hpc_opt","R","18_irrig_allocation.R"))
source(here("hpc_opt","R","32_precomp_build.R"))
source(here("hpc_opt","R","33_eval_engine.R"))

# ---- args (HPC friendly) ----
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}
scenario_name <- get_arg("--scenario", "baseline")
seed <- as.integer(get_arg("--seed", "1"))


# ---- load inputs (HPC version) ----
common_input_basin <- read_csv(here("hpc_opt","outputs","common_inputs_basin.csv"), show_col_types = FALSE)

df_int_crop_areanorm <- read_csv(
  here("hpc_opt","outputs","integration","int_crop_areanorm_annual.csv"),
  show_col_types = FALSE
)

# ---- load models ----
models <- load_models(here("hpc_opt","models"))
stopifnot(!is.null(models$wq_lm))

# ---- economics (put this into hpc_opt/config later if you want) ----
universal_costs <- list(
  irr_cost_per_m3 = 0.029,
  fert_share_direct = 0.2   # used by eval engine
)

crop_params <- tibble::tribble(
  ~Crop,      ~income_per_kg, ~direct_cost_per_kg, ~fixed_cost_per_kg, ~total_cost_per_kg, ~fert_kgha,
  "Wheat",     0.2,           0.12,                0.05,               0.17,               90,
  "Corn",      0.18,          0.11,                0.05,               0.15,              250,
  "Sorghum",   0.17,          0.09,                0.06,               0.15,              110,
  "Soybeans",  0.37,          0.18,                0.14,               0.32,               55
) %>% mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)

# ---- df_opt exactly as your original workflow ----
df_opt <- df_int_crop_areanorm %>%
  left_join(crop_params, by = "Crop") %>%
  # engine expects these names:
  rename(
    Yield_kgHa   = Yield_kgHa,
    Irrigation_m = Irrigation_m
  )

# ---- build/load precomp ----
#Load precomp (this is the single source of truth)
precomp <- readRDS(here("hpc_opt","outputs","precompute","precomp_baseline.rds"))

years_vec          <- precomp$years_vec
lu_baseline         <- precomp$lu_baseline
wq_base_by_year      <- precomp$wq_base_by_year
fert_ref_by_year     <- precomp$fert_ref_by_year
baseline_irrig_frac  <- precomp$baseline_irrig_frac
hist_mix             <- precomp$hist_mix

# ---- scenario policy knobs ----
# For now: keep scenarios as simple R lists. We'll move to YAML next.
policy_tbl <- list(
  baseline = list(
    cult_area_factor = 1,
    irrig_frac_factor = 1,
    irr_eff = 1,
    fert_factor = 1,
    fert_gamma = 0.1
  )
  )


policy <- policy_tbl[[scenario_name]]
if (is.null(policy)) stop("Unknown scenario: ", scenario_name)

# ---- objective fn for nsga2 (minimize all 3) ----
fn <- function(x) {
  as.numeric(eval_candidate_all_years(
    x = x,
    years_vec = precomp$years_vec,
    df_opt = df_opt,
    wq_model_mlr = models$wq_lm,
    wq_base_by_year = precomp$wq_base_by_year,
    lu_baseline = precomp$lu_baseline,
    universal_costs = universal_costs,
    crop_params = crop_params %>% rename(fert_kgha = fert_kgha), # ensure name
    baseline_irrig_frac = precomp$baseline_irrig_frac,
    hist_mix = precomp$hist_mix,
    fert_ref_by_year = precomp$fert_ref_by_year,
    policy = policy
  ))
}

# ---- bounds for x = (corn, soy, sor); wheat is remainder ----
lower <- c(0, 0, 0)
upper <- c(1, 1, 1)

set.seed(seed)

res <- mco::nsga2(
  fn,
  idim = 3, odim = 3,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize = 40, #adjust
  generations = 30 #adjust
  
)

# ---- write outputs (scenario + seed) ----
outdir <- here("hpc_opt","outputs","runs", scenario_name)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

saveRDS(res, file.path(outdir, sprintf("nsga2_res_%s_seed%d.rds", scenario_name, seed)))
write.csv(res$value, file.path(outdir, sprintf("nsga2_objectives_%s_seed%d.csv", scenario_name, seed)), row.names = FALSE)
write.csv(res$par,   file.path(outdir, sprintf("nsga2_params_%s_seed%d.csv", scenario_name, seed)), row.names = FALSE)

message("DONE. Scenario=", scenario_name, " seed=", seed, " -> ", outdir)