decode_crop_shares <- function(x) {
  x <- pmax(as.numeric(x), 0)
  
  if (sum(x) == 0) {
    x <- rep(1, length(x))
  }
  
  shares <- x / sum(x)
  names(shares) <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  shares
}

make_dynamic_crop_bounds <- function(
    ref_mix,
    annual_relative_change = 0.01,
    transition_years = 25
) {
  crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  ref_mix <- as.numeric(ref_mix[crops])
  names(ref_mix) <- crops
  if (any(!is.finite(ref_mix)) || any(ref_mix < 0) || sum(ref_mix) <= 0) {
    stop("Invalid crop-composition reference supplied to dynamic bounds.")
  }
  ref_mix <- ref_mix / sum(ref_mix)
  relative_window <- annual_relative_change * transition_years
  allowed_change <- ref_mix * relative_window

  bounds <- tibble::tibble(
    Crop = crops,
    ref_share = unname(ref_mix),
    allowed_change = unname(allowed_change),
    lower = pmax(0, ref_share - allowed_change),
    upper = pmin(1, ref_share + allowed_change)
  ) %>%
    mutate(annual_relative_change = annual_relative_change,
           transition_years = transition_years)

  if (sum(bounds$lower) > 1 + 1e-10 || sum(bounds$upper) < 1 - 1e-10) {
    stop("Dynamic crop bounds are jointly infeasible.")
  }
  bounds
}

check_crop_constraints <- function(shares, crop_bounds) {
  
  checks <- purrr::map_lgl(names(shares), function(crop) {
    
    lower_i <- crop_bounds$lower[crop_bounds$Crop == crop]
    upper_i <- crop_bounds$upper[crop_bounds$Crop == crop]
    
    shares[[crop]] >= lower_i && shares[[crop]] <= upper_i
  })
  
  all(checks)
}

decode_bounded_crop_shares <- function(x, crop_bounds, tol = 1e-12) {
  crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  bounds <- crop_bounds[match(crops, crop_bounds$Crop), , drop = FALSE]
  lower <- bounds$lower
  upper <- bounds$upper

  if (anyNA(lower) || anyNA(upper) || sum(lower) > 1 + tol || sum(upper) < 1 - tol) {
    stop("Crop bounds are missing or jointly infeasible.")
  }

  y <- decode_crop_shares(x[1:4])
  lo <- min(y - upper)
  hi <- max(y - lower)
  for (i in seq_len(100)) {
    lambda <- (lo + hi) / 2
    projected <- pmin(upper, pmax(lower, y - lambda))
    if (sum(projected) > 1) lo <- lambda else hi <- lambda
  }
  projected <- pmin(upper, pmax(lower, y - (lo + hi) / 2))
  names(projected) <- crops
  projected
}

decode_management <- function(x, phase) {
  
  mgmt <- list(
    fert_factor = 1,
    irrig_frac_factor = 1
  )
  
  if (phase %in% c("crop_fert", "crop_fert_irrigfrac")) {
    mgmt$fert_factor <- x[5]
  }
  
  if (phase == "crop_fert_irrigfrac") {
    mgmt$irrig_frac_factor <- x[6]
  }
  
  mgmt
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
    irrigation_reference = NULL,
    fert_ref_by_year,
    policy,
    crop_bounds,
    phase = "crop_bounds",
    PEN = 1e12
) {
  
  function(x) {
    
    # only first 4 values are crop-share variables
    shares <- decode_bounded_crop_shares(x[1:4], crop_bounds)
    
    if (!check_crop_constraints(shares, crop_bounds)) {
      return(c(nitrate = PEN, profit_minimized = PEN, irrigation = PEN))
    }
    
    # Irrigation technology is fixed by the scenario policy, not optimized.
    mgmt <- decode_management(x, phase)
    
    # make a candidate-specific copy of policy
    policy_i <- policy
    
    policy_i$fert_factor <- mgmt$fert_factor
    policy_i$irrig_frac_factor <- mgmt$irrig_frac_factor
    
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
        irrigation_reference = irrigation_reference,
        fert_ref_by_year = fert_ref_by_year,
        policy = policy_i
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
