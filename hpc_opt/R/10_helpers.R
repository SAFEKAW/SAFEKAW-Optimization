# hpc_opt/R/10_helpers.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

mae  <- function(a, b, na.rm = TRUE) mean(abs(a - b), na.rm = na.rm)
rmse <- function(a, b, na.rm = TRUE) sqrt(mean((a - b)^2, na.rm = na.rm))

wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

# --- Robust irrigation indicator from available columns -----------------
# Returns a logical vector (one per row of df)
build_irrig_flags <- function(df) {
  lc   <- if ("LandCover" %in% names(df)) tolower(as.character(df$LandCover)) else ""
  lcg  <- if ("LandCoverGroup" %in% names(df)) tolower(as.character(df$LandCoverGroup)) else ""
  irrm <- if ("irrigation_WaterUse_m" %in% names(df)) suppressWarnings(as.numeric(df$irrigation_WaterUse_m)) else NA_real_
  
  is_irrig_text  <- grepl("irrig", lc) | grepl("irrig", lcg)
  is_irrig_depth <- is.finite(irrm) & irrm > 0
  
  is_irrig_depth | is_irrig_text
}

# --- Crop-specific historical irrigated fraction (used by allocate_irrigation) ---
build_hist_mix_from_county <- function(df_county, years_vec) {
  stopifnot(all(c("Year","Crop") %in% names(df_county)))
  
  area_col <- dplyr::case_when(
    "area_m2" %in% names(df_county) ~ "area_m2",
    "area_ha" %in% names(df_county) ~ "area_ha",
    TRUE ~ NA_character_
  )
  if (is.na(area_col)) stop("No area column found (expected area_m2 or area_ha).")
  
  df <- df_county %>%
    filter(Year %in% years_vec, !is.na(Crop)) %>%
    mutate(
      Crop = as.character(Crop),
      is_irrig = build_irrig_flags(cur_data_all())
    )
  
  df %>%
    group_by(Crop, is_irrig) %>%
    summarise(area = sum(.data[[area_col]], na.rm = TRUE), .groups = "drop") %>%
    group_by(Crop) %>%
    summarise(
      irrig_area = sum(area[is_irrig], na.rm = TRUE),
      total_area = sum(area, na.rm = TRUE),
      irrig_frac = if_else(total_area > 0, irrig_area / total_area, 0),
      .groups = "drop"
    ) %>%
    mutate(irrig_frac = pmax(0, pmin(1, irrig_frac)))
}

# --- Basin-wide baseline irrigated fraction across all crops -------------
build_baseline_irrig_frac_from_county <- function(df_county, years_vec) {
  area_col <- dplyr::case_when(
    "area_m2" %in% names(df_county) ~ "area_m2",
    "area_ha" %in% names(df_county) ~ "area_ha",
    TRUE ~ NA_character_
  )
  if (is.na(area_col)) stop("No area column found (expected area_m2 or area_ha).")
  
  df <- df_county %>%
    filter(Year %in% years_vec, !is.na(Crop)) %>%
    mutate(is_irrig = build_irrig_flags(cur_data_all()))
  
  tot <- df %>%
    summarise(
      irrig_area = sum(.data[[area_col]][is_irrig], na.rm = TRUE),
      cult_area  = sum(.data[[area_col]], na.rm = TRUE)
    )
  
  if (!is.finite(tot$cult_area) || tot$cult_area <= 0) return(0)
  pmax(0, pmin(1, tot$irrig_area / tot$cult_area))
}

# --- Mean crop mix over historical period (baseline x for eval) ----------
get_baseline_obs_x <- function(int_crop_areanorm_path,
                               crops = c("Corn","Soybeans","Sorghum","Wheat")) {
  
  df_int_crop <- readr::read_csv(int_crop_areanorm_path, show_col_types = FALSE)
  
  df_crop <- df_int_crop %>%
    dplyr::filter(.data$Crop %in% crops) %>%
    dplyr::mutate(Crop_prc = suppressWarnings(as.numeric(.data$Crop_prc))) %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::mutate(
      Crop_frac = {
        # IMPORTANT: base if/else (scalar condition per Year group)
        cond_pct <- max(Crop_prc, na.rm = TRUE) > 1.01
        if (isTRUE(cond_pct)) Crop_prc / 100 else Crop_prc
      },
      share_year = {
        s <- sum(Crop_frac, na.rm = TRUE)
        if (is.finite(s) && s > 0) Crop_frac / s else NA_real_
      }
    ) %>%
    dplyr::ungroup()
  
  obs_tbl <- df_crop %>%
    dplyr::group_by(.data$Crop) %>%
    dplyr::summarise(obs_share = mean(.data$share_year, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(Crop = crops, fill = list(obs_share = 0)) %>%
    dplyr::mutate(Crop = factor(.data$Crop, levels = crops)) %>%
    dplyr::arrange(.data$Crop)
  
  obs_x <- c(
    Corn     = obs_tbl$obs_share[obs_tbl$Crop == "Corn"],
    Soybeans = obs_tbl$obs_share[obs_tbl$Crop == "Soybeans"],
    Sorghum  = obs_tbl$obs_share[obs_tbl$Crop == "Sorghum"]
  )
  
  obs_x <- pmax(0, pmin(1, as.numeric(obs_x)))
  if (sum(obs_x) > 1) obs_x <- obs_x / sum(obs_x) * 0.999999
  
  list(
    obs_tbl = obs_tbl,
    obs_x = obs_x,
    wheat = 1 - sum(obs_x)
  )
}