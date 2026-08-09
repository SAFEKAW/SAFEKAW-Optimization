# hpc_opt/R/32_build_precomp.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

build_precomp <- function(common_input_basin,
                          years_vec,
                          precip_reference_m,
                          precip_context = common_input_basin,
                          fert_ref_by_year = NULL,
                          baseline_irrig_frac,
                          hist_mix,
                          irrigation_reference = NULL) {
  
  df <- common_input_basin %>%
    filter(Year %in% years_vec) %>%
    arrange(Year)
  
  stopifnot(all(c("LandCover_m2_all_cult","LandCover_m2_grassPastureHay","LandCover_m2_developed") %in% names(df)))
  
  lu_baseline <- df %>%
    transmute(
      Year,
      LC_cult_base = LandCover_m2_all_cult,
      LC_dev_base  = LandCover_m2_developed,
      LC_total     = LandCover_m2_all_cult + LandCover_m2_grassPastureHay + LandCover_m2_developed
    )
  
  if (length(precip_reference_m) < 2L || any(!is.finite(precip_reference_m))) {
    stop("precip_reference_m must contain at least two finite historical values.")
  }

  # Always evaluate precipitation ranks against the fixed historical ECDF.
  # For future scenarios, precip_context contains the continuous 2025-2099
  # pathway so changes at 2050 and 2075 are retained before period subsetting.
  precip_context <- precip_context %>%
    transmute(Year, Climate_precip_m, Climate_Tmean_C) %>%
    distinct(Year, .keep_all = TRUE) %>%
    arrange(Year)
  if (anyDuplicated(precip_context$Year) ||
      any(!is.finite(precip_context$Climate_precip_m))) {
    stop("precip_context must contain one finite precipitation value per year.")
  }

  ec <- stats::ecdf(precip_reference_m)
  wq_context <- precip_context %>%
    mutate(
      precip_percentile       = ec(Climate_precip_m),
      precip_percentileChange = dplyr::coalesce(precip_percentile - dplyr::lag(precip_percentile), 0)
    )

  wq_base_by_year <- wq_context %>%
    filter(Year %in% years_vec)
  if (nrow(wq_base_by_year) != length(years_vec)) {
    stop("precip_context does not cover every requested precompute year.")
  }
  
  if (is.null(fert_ref_by_year)) {
    if (!("Management_FertilizerUse_kg" %in% names(df))) {
      stop("Need fert_ref_by_year OR Management_FertilizerUse_kg in common_input_basin.")
    }
    fert_ref_by_year <- df %>%
      transmute(Year, Fert_ref_kg = Management_FertilizerUse_kg)
  } else {
    stopifnot(all(c("Year","Fert_ref_kg") %in% names(fert_ref_by_year)))
    fert_ref_by_year <- fert_ref_by_year %>% filter(Year %in% years_vec)
  }
  
  list(
    years_vec = years_vec,
    lu_baseline = lu_baseline,
    wq_base_by_year = wq_base_by_year,
    precip_reference = list(
      source = "historical_common_inputs_basin_2006_2023",
      years = 2006:2023,
      values_m = as.numeric(precip_reference_m)
    ),
    fert_ref_by_year = fert_ref_by_year,
    baseline_irrig_frac = baseline_irrig_frac,
    hist_mix = hist_mix,
    irrigation_reference = irrigation_reference
  )
}
