# hpc_opt/R/14_wq_features.R
suppressPackageStartupMessages({
  library(dplyr)
})

add_wq_features <- function(df) {
  
  # Ensure required columns exist
  if (!("Climate_Tmean_C" %in% names(df))) df$Climate_Tmean_C <- NA_real_
  if (!("Management_FertilizerUse_kgm2" %in% names(df))) df$Management_FertilizerUse_kgm2 <- NA_real_
  if (!("Management_FertilizerUse_kg"   %in% names(df))) df$Management_FertilizerUse_kg   <- NA_real_
  
  # Expect these landcover columns from common_inputs_basin.csv (HPC version)
  # LandCover_m2_all_cult, LandCover_m2_grassPastureHay, LandCover_m2_developed
  need_lc <- c("LandCover_m2_all_cult", "LandCover_m2_grassPastureHay", "LandCover_m2_developed")
  missing <- setdiff(need_lc, names(df))
  if (length(missing) > 0) {
    stop("add_wq_features(): missing landcover cols: ", paste(missing, collapse = ", "))
  }
  
  # Basin area
  if (!("BasinArea_m2" %in% names(df))) {
    df <- df %>%
      mutate(BasinArea_m2 = LandCover_m2_all_cult + LandCover_m2_grassPastureHay + LandCover_m2_developed)
  }
  
  # Optional Tmean fallback
  if (all(c("Climate_Tmin_C", "Climate_Tmax_C") %in% names(df))) {
    df <- df %>%
      mutate(Climate_Tmean_C = coalesce(Climate_Tmean_C, (Climate_Tmin_C + Climate_Tmax_C) / 2))
  }
  
  df %>%
    mutate(
      grass_frac = if_else(BasinArea_m2 > 0, LandCover_m2_grassPastureHay / BasinArea_m2, NA_real_),
      cult_frac  = if_else(BasinArea_m2 > 0, LandCover_m2_all_cult       / BasinArea_m2, NA_real_),
      Management_FertilizerUse_kgm2 = coalesce(
        Management_FertilizerUse_kgm2,
        if_else(!is.na(Management_FertilizerUse_kg) & BasinArea_m2 > 0,
                Management_FertilizerUse_kg / BasinArea_m2, NA_real_)
      )
    )
}
