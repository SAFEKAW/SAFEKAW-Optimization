# R/wq_predict.R

required_wq_vars <- function() {
  c("LandCover_m2_developed",
    "LandCover_m2_grassPastureHay",
    "LandCover_m2_all cult",
    "Climate_precip_m",
    "Climate_Tmean_C",
    "precip_percentileChange",
    "Management_irrigation_m",
    "Management_FertilizerUse_kgm2")
}

validate_pred_row <- function(pred_row) {
  miss <- setdiff(required_wq_vars(), names(pred_row))
  if (length(miss) > 0) stop("pred_row missing: ", paste(miss, collapse = ", "))
  if (anyNA(pred_row))  stop("pred_row has NA values.")
  invisible(TRUE)
}

predict_nitrate_flux_kgyr <- function(pred_row, wq_model_log1p) {
  validate_pred_row(pred_row)
  pred_log1p <- as.numeric(stats::predict(wq_model_log1p, newdata = pred_row))
  max(0, exp(pred_log1p) - 1)
}

