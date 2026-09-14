install.packages("lhs")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

# 1) Load precomp (this is the single source of truth)
precomp <- readRDS(here("hpc_opt","outputs","precompute","precomp_baseline.rds"))

years_vec          <- precomp$years_vec
lu_baseline         <- precomp$lu_baseline
wq_base_by_year      <- precomp$wq_base_by_year
fert_ref_by_year     <- precomp$fert_ref_by_year
baseline_irrig_frac  <- precomp$baseline_irrig_frac
hist_mix             <- precomp$hist_mix

wq_model_mlr <- readRDS(here("hpc_opt","models","wq_lm.rds"))  # or wherever you saved them

# ---- LHS sampler for ALL levers (7 vars like collaborator) ----
sample_lhs_levers <- function(n_init = 20000,
                              ranges = list(
                                corn      = c(0, 100),
                                soy       = c(0, 100),
                                sor       = c(0, 100),
                                cult_area = c(-30, 30),  # percent change
                                fert_eff  = c(-10, 10),  # percent (positive = more efficient)
                                irr_area  = c(-30, 30),  # percent change in irrig frac
                                irr_eff   = c(-10, 10)   # percent change in irr efficiency
                              ),
                              seed = 1) {
  if (!requireNamespace("lhs", quietly = TRUE)) {
    stop("Package 'lhs' not installed. Install with install.packages('lhs').")
  }
  set.seed(seed)
  
  # 7-D latin hypercube in [0,1]
  lhs_init <- lhs::randomLHS(n_init, 7)
  
  # scale to ranges
  nm <- names(ranges)
  mins <- vapply(ranges, function(x) min(x), numeric(1))
  maxs <- vapply(ranges, function(x) max(x), numeric(1))
  spans <- maxs - mins
  
  scaled <- sweep(lhs_init, 2, spans, `*`)
  scaled <- sweep(scaled, 2, mins, `+`)
  colnames(scaled) <- nm
  
  # enforce crop constraint: corn+soy+sor <= 100
  keep <- rowSums(scaled[, c("corn","soy","sor"), drop = FALSE]) <= 100
  scaled <- scaled[keep, , drop = FALSE]
  
  as_tibble(scaled)
}

# ---- Convert one sample row -> (x, policy) exactly like app ----
row_to_x_policy <- function(corn, soy, sor, cult_area, fert_eff, irr_area, irr_eff,
                            fert_gamma = 0.1) {
  x <- c(corn, soy, sor) / 100
  
  policy <- list(
    cult_area_factor  = max(0, 1 + cult_area/100),
    
    # fert_eff: +10 means "10% more efficient" => less fertilizer => factor < 1
    fert_factor       = max(0, 1 - fert_eff/100),
    fert_gamma        = fert_gamma,
    
    irrig_frac_factor = max(0, 1 + irr_area/100),
    
    # irr_eff: +10 means "10% more efficient" => irr_eff > 1 in your eval (you divide by irr_eff)
    irr_eff           = max(1, 1 + irr_eff/100)
  )
  
  list(x = x, policy = policy)
}

# ---- Evaluate feasible space ----
evaluate_feasible_space_lhs <- function(samples,
                                        years_vec,
                                        df_opt,
                                        wq_model_mlr, wq_base_by_year,
                                        lu_baseline,
                                        universal_costs, crop_params,
                                        baseline_irrig_frac,
                                        hist_mix,
                                        fert_ref_by_year,
                                        scenario_name = "feasible_space") {
  
  purrr::pmap_dfr(
    samples,
    function(corn, soy, sor, cult_area, fert_eff, irr_area, irr_eff) {
      
      xp <- row_to_x_policy(
        corn = corn, soy = soy, sor = sor,
        cult_area = cult_area,
        fert_eff = fert_eff,
        irr_area = irr_area,
        irr_eff = irr_eff,
        fert_gamma = 0.1
      )
      
      vals <- eval_candidate_all_years(
        x = xp$x,
        years_vec = years_vec,
        df_opt = df_opt,
        wq_model_mlr = wq_model_mlr,
        wq_base_by_year = wq_base_by_year,
        lu_baseline = lu_baseline,
        universal_costs = universal_costs,
        crop_params = crop_params,
        baseline_irrig_frac = baseline_irrig_frac,
        hist_mix = hist_mix,
        fert_ref_by_year = fert_ref_by_year,
        policy = xp$policy
      )
      
      tibble(
        Scenario = scenario_name,
        
        # levers (keep for plotting later)
        corn = corn, soy = soy, sor = sor,
        wheat = 100 - corn - soy - sor,
        cult_area = cult_area,
        fert_eff  = fert_eff,
        irr_area  = irr_area,
        irr_eff   = irr_eff,
        
        # shares (0-1) also handy
        Corn = xp$x[1],
        Soybeans = xp$x[2],
        Sorghum = xp$x[3],
        Wheat = 1 - sum(xp$x),
        
        # objectives (note: neg profit)
        Nitrate_kgyr  = vals[["mean_n_flux_kgyr"]],
        NegProfit_usd = vals[["mean_neg_profit_usdyr"]],
        Irr_m3        = vals[["mean_irr_m3_yr"]]
      )
    }
  )
}

# ---- Driver ----
n_samples_init <- 20000
samples <- sample_lhs_levers(n_init = n_samples_init, seed = 1)

message(nrow(samples), "/", n_samples_init, " samples retained after crop constraint")

# OPTIONAL: run baseline point once (for checking)
baseline_policy <- list(
  cult_area_factor  = 1,
  fert_factor       = 1,
  fert_gamma        = 0.1,
  irrig_frac_factor = 1,
  irr_eff           = 1
)

# Build baseline x from observed crop mix 
# obs_x should be c(corn, soy, sor) fractions. 
# baseline_x <- obs_x
# If not:
baseline_x <- c(0.4, 0.4, 0.05)  # <-- placeholder; replace with actual baseline shares!

baseline_out <- eval_candidate_all_years(
  x = baseline_x,
  years_vec = years_vec,
  df_opt = df_opt,
  wq_model_mlr = wq_model_mlr,
  wq_base_by_year = wq_base_by_year,
  lu_baseline = lu_baseline,
  universal_costs = universal_costs,
  crop_params = crop_params,
  baseline_irrig_frac = baseline_irrig_frac,
  hist_mix = hist_mix,
  fert_ref_by_year = fert_ref_by_year,
  policy = baseline_policy
)

baseline_row <- tibble(
  Scenario = "baseline",
  Corn = baseline_x[1], Soybeans = baseline_x[2], Sorghum = baseline_x[3], Wheat = 1 - sum(baseline_x),
  Nitrate_kgyr  = baseline_out[["mean_n_flux_kgyr"]],
  NegProfit_usd = baseline_out[["mean_neg_profit_usdyr"]],
  Irr_m3        = baseline_out[["mean_irr_m3_yr"]]
)

# Now evaluate feasible space
feas <- evaluate_feasible_space_lhs(
  samples = samples,
  years_vec = years_vec,
  df_opt = df_opt,
  wq_model_mlr = wq_model_mlr,
  wq_base_by_year = wq_base_by_year,
  lu_baseline = lu_baseline,
  universal_costs = universal_costs,
  crop_params = crop_params,
  baseline_irrig_frac = baseline_irrig_frac,
  hist_mix = hist_mix,
  fert_ref_by_year = fert_ref_by_year,
  scenario_name = "feasible_space"
)

# Save
out_dir <- here::here("hpc_opt","outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(feas, file.path(out_dir, "feasible_space.rds"))
readr::write_csv(feas, file.path(out_dir, "feasible_space.csv"))
readr::write_csv(baseline_row, file.path(out_dir, "baseline_point.csv"))