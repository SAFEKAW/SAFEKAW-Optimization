# hpc_opt/R/13_yield_predict.R
suppressPackageStartupMessages({
  library(dplyr)
})

# Add any model-specific derived predictors using preprocessing metadata saved
# with the fitted model. This keeps future scenarios on the historical training
# scale instead of re-standardizing each projection period.
prepare_yield_newdata <- function(m, newdata) {
  scaling <- attr(m, "yield_scaling", exact = TRUE)
  if (is.null(scaling)) return(newdata)

  required_scaling <- c("water_center", "water_scale", "gdd_center", "gdd_scale")
  missing_scaling <- setdiff(required_scaling, names(scaling))
  if (length(missing_scaling) > 0) {
    stop(
      "Yield model has incomplete scaling metadata: ",
      paste(missing_scaling, collapse = ", ")
    )
  }

  required_data <- c("totalWater_m", "GDD", "Year")
  missing_data <- setdiff(required_data, names(newdata))
  if (length(missing_data) > 0) {
    stop(
      "Yield prediction data are missing: ",
      paste(missing_data, collapse = ", ")
    )
  }

  if (!is.finite(scaling$water_scale) || scaling$water_scale <= 0 ||
      !is.finite(scaling$gdd_scale) || scaling$gdd_scale <= 0) {
    stop("Yield model scaling standard deviations must be finite and positive.")
  }

  newdata %>%
    mutate(
      ZTotalWater = (totalWater_m - scaling$water_center) / scaling$water_scale,
      ZGDD = (GDD - scaling$gdd_center) / scaling$gdd_scale,
      Year_centered = Year - if (is.null(scaling$year_center)) 2005 else scaling$year_center
    )
}

# helper: predict from either lm or lmer
predict_one_yield_model <- function(m, newdata, fixed_only = TRUE) {
  if (is.null(m)) return(rep(NA_real_, nrow(newdata)))

  newdata <- prepare_yield_newdata(m, newdata)
  
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
# df must contain the predictors required by each saved crop model. Current
# models use totalWater_m or precip_gs_m plus irrigation_WaterUse_m, and GDD.
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
