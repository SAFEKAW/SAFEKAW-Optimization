# R/wq_predict.R

required_wq_vars <- function() {
  c(
    "BasinArea_m2",
    "dev_frac",
    "cult_frac",
    "Climate_precip_m",
    "Climate_Tmean_C",
    "precip_percentileChange",
    "Management_FertilizerUse_kgm2"
  )
}

validate_pred_row <- function(pred_row) {
  miss <- setdiff(required_wq_vars(), names(pred_row))
  if (length(miss) > 0) stop("pred_row missing: ", paste(miss, collapse = ", "))
  if (anyNA(pred_row[required_wq_vars()])) stop("pred_row has NA values in required fields.")
  invisible(TRUE)
}

predict_nitrate_flux_kgyr <- function(pred_row, wq_model_mlr) {
  validate_pred_row(pred_row)
  pred <- as.numeric(stats::predict(wq_model_mlr, newdata = pred_row))
  max(0, pred)
}
