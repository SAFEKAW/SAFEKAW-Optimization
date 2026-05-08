# hpc_opt/R/13_yield_predict.R
suppressPackageStartupMessages({
  library(dplyr)
})

# helper: predict from either lm or lmer
predict_one_yield_model <- function(m, newdata, fixed_only = TRUE) {
  if (is.null(m)) return(rep(NA_real_, nrow(newdata)))
  
  # mixed model
  if (inherits(m, "lmerMod") || inherits(m, "lmerModLmerTest")) {
    return(as.numeric(stats::predict(
      m,
      newdata = newdata,
      re.form = if (fixed_only) NA else NULL,
      allow.new.levels = TRUE
    )))
  }
  
  # simple regression
  if (inherits(m, "lm")) {
    return(as.numeric(stats::predict(
      m,
      newdata = newdata
    )))
  }
  
  stop("Unsupported model class: ", paste(class(m), collapse = ", "))
}

# Predict from already-fit yield models
# df must contain: Crop, totalWater_m, GDD (and any other fixed-effect terms)
predict_yields <- function(df, yield_kg_models, yield_kcal_models,
                           fixed_only = TRUE) {
  
  df_out <- df
  df_out$yield_kgHa_pred   <- NA_real_
  df_out$yield_kcalHa_pred <- NA_real_
  
  crops <- intersect(unique(df_out$Crop), union(names(yield_kg_models), names(yield_kcal_models)))
  
  for (cr in crops) {
    idx <- which(df_out$Crop == cr)
    if (length(idx) == 0) next
    
    if (!is.null(yield_kg_models[[cr]])) {
      m <- yield_kg_models[[cr]]
      df_out$yield_kgHa_pred[idx] <- predict_one_yield_model(
        m = m,
        newdata = df_out[idx, , drop = FALSE],
        fixed_only = fixed_only
      )
    }
    
    if (!is.null(yield_kcal_models[[cr]])) {
      m <- yield_kcal_models[[cr]]
      df_out$yield_kcalHa_pred[idx] <- predict_one_yield_model(
        m = m,
        newdata = df_out[idx, , drop = FALSE],
        fixed_only = fixed_only
      )
    }
  }
  
  df_out
}
