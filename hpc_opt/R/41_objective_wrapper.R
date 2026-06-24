# hpc_opt/R/41_objective_wrapper.R

decode_crop_shares <- function(x) {
  x <- pmax(as.numeric(x), 0)
  
  if (sum(x) == 0) {
    x <- rep(1, length(x))
  }
  
  shares <- x / sum(x)
  names(shares) <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  shares
}

make_objective_wrapper <- function(
    years_vec,
    df_county,
    irrigation_lm,
    yield_kg_models,
    yield_kcal_models,
    wq_model_mlr,
    wq_base_by_year,
    lu_baseline,
    universal_costs,
    crop_params,
    baseline_irrig_frac,
    hist_mix,
    fert_ref_by_year,
    policy,
    PEN = 1e12
) {
  
  function(x) {
    
    shares <- decode_crop_shares(x)
    
    x_eval <- c(
      Corn     = unname(shares["Corn"]),
      Soybeans = unname(shares["Soybeans"]),
      Sorghum  = unname(shares["Sorghum"])
    )
    
    out <- tryCatch(
      eval_candidate_all_years(
        x = x_eval,
        years_vec = years_vec,
        df_county = df_county,
        irrigation_lm = irrigation_lm,
        yield_kg_models = yield_kg_models,
        yield_kcal_models = yield_kcal_models,
        wq_model_mlr = wq_model_mlr,
        wq_base_by_year = wq_base_by_year,
        lu_baseline = lu_baseline,
        universal_costs = universal_costs,
        crop_params = crop_params,
        baseline_irrig_frac = baseline_irrig_frac,
        hist_mix = hist_mix,
        fert_ref_by_year = fert_ref_by_year,
        policy = policy
      ),
      error = function(e) {
        message("Objective wrapper error: ", conditionMessage(e))
        c(nitrate = PEN, profit_minimized = PEN, irrigation = PEN)
      }
    )
    
    out <- as.numeric(out)
    
    
    if (length(out) != 3 || any(!is.finite(out))) {
      return(c(nitrate = PEN, profit_minimized = PEN, irrigation = PEN))
    }
    
    out
  }
}