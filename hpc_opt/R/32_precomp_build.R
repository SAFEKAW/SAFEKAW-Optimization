# hpc_opt/R/32_build_precomp.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

build_precomp <- function(common_input_basin,
                          years_vec,
                          fert_ref_by_year = NULL,
                          baseline_irrig_frac,
                          hist_mix) {
  
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
  
  wq_base_by_year <- df %>%
    transmute(Year, Climate_precip_m, Climate_Tmean_C) %>%
    arrange(Year)
  
  ec <- stats::ecdf(wq_base_by_year$Climate_precip_m)
  wq_base_by_year <- wq_base_by_year %>%
    mutate(
      precip_percentile       = ec(Climate_precip_m),
      precip_percentileChange = dplyr::coalesce(precip_percentile - dplyr::lag(precip_percentile), 0)
    )
  
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
    fert_ref_by_year = fert_ref_by_year,
    baseline_irrig_frac = baseline_irrig_frac,
    hist_mix = hist_mix
  )
}