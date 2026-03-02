# hpc_opt/R/13_yield_predict.R
suppressPackageStartupMessages({
  library(dplyr)
})

# Predict from already-fit lmer models
# df must contain: Crop, totalWater_m, GDD (and any other fixed-effect terms)
predict_yields <- function(df, yield_kg_models, yield_kcal_models,
                           fixed_only = TRUE) {
  
  # we keep original df order
  df_out <- df
  df_out$yield_kgHa_pred   <- NA_real_
  df_out$yield_kcalHa_pred <- NA_real_
  
  crops <- intersect(unique(df_out$Crop), union(names(yield_kg_models), names(yield_kcal_models)))
  
  for (cr in crops) {
    idx <- which(df_out$Crop == cr)
    if (length(idx) == 0) next
    
    if (!is.null(yield_kg_models[[cr]])) {
      m <- yield_kg_models[[cr]]
      df_out$yield_kgHa_pred[idx] <- as.numeric(stats::predict(
        m, newdata = df_out[idx, , drop = FALSE],
        re.form = if (fixed_only) NA else NULL,
        allow.new.levels = TRUE
      ))
    }
    
    if (!is.null(yield_kcal_models[[cr]])) {
      m <- yield_kcal_models[[cr]]
      df_out$yield_kcalHa_pred[idx] <- as.numeric(stats::predict(
        m, newdata = df_out[idx, , drop = FALSE],
        re.form = if (fixed_only) NA else NULL,
        allow.new.levels = TRUE
      ))
    }
  }
  
  df_out
}
