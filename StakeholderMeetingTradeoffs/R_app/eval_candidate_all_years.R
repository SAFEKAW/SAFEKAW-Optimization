eval_candidate_all_years <- function(
    x, years_vec, df_opt, wq_model_log1p, wq_context,
    lu_baseline, universal_costs, crop_params,
    baseline_irrig_frac, hist_mix = NULL,
    weights = NULL, na_penalty = 1e9
){
  stopifnot(length(years_vec) > 0)
  
  vals <- vapply(
    years_vec,
    \(Y) eval_candidate(
      x = x, Y = Y,
      df_opt = df_opt,
      wq_model_log1p = wq_model_log1p,
      wq_context = wq_context,
      lu_baseline = lu_baseline,
      universal_costs = universal_costs,
      crop_params = crop_params,
      baseline_irrig_frac = baseline_irrig_frac,
      hist_mix = hist_mix
    ),
    numeric(3)
  )
  
  vals <- t(vals)
  colnames(vals) <- c("n_flux_kgyr", "neg_profit_usdyr", "irr_m3_yr")
  
  if (is.null(weights)) weights <- rep(1, nrow(vals))
  if (length(weights) != nrow(vals)) stop("weights must match length(years_vec).")
  
  wmean <- function(v, w) {
    keep <- is.finite(v) & is.finite(w) & w > 0
    if (!any(keep)) return(NA_real_)
    wk <- w[keep] / sum(w[keep])
    sum(v[keep] * wk)
  }
  
  out <- c(
    mean_n_flux_kgyr      = wmean(vals[, "n_flux_kgyr"],      weights),
    mean_neg_profit_usdyr = wmean(vals[, "neg_profit_usdyr"], weights),
    mean_irr_m3_yr        = wmean(vals[, "irr_m3_yr"],        weights)
  )
  
  if (any(!is.finite(out))) out[] <- na_penalty
  out
}
