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



# --- Basin-wide baseline irrigated fraction from water-use area ----------
build_baseline_irrig_frac_from_wateruse <- function(wu_by_crop, lu_baseline, years_vec,
                                                    crops = c("Corn", "Soybeans")) {
  
  wu_sum <- wu_by_crop %>%
    filter(Year %in% years_vec, Crop %in% crops) %>%
    group_by(Year) %>%
    summarise(
      irrig_area_m2 = sum(irrArea_ha, na.rm = TRUE) * 1e4,
      .groups = "drop"
    )
  
  df <- wu_sum %>%
    left_join(
      lu_baseline %>% select(Year, LC_cult_base),
      by = "Year"
    ) %>%
    mutate(
      irrig_frac = irrig_area_m2 / LC_cult_base
    )
  
  out <- mean(df$irrig_frac, na.rm = TRUE)
  pmax(0, pmin(1, out))
}


# --- Crop-specific historical irrigation mix from water-use area ----------
build_hist_mix_from_wateruse <- function(wu_by_crop, df_county, years_vec,
                                         crops = c("Corn", "Soybeans", "Sorghum", "Wheat")) {
  required_wu <- c("Year", "Crop", "irrArea_ha")
  required_county <- c("Year", "Crop", "area_m2")
  if (!all(required_wu %in% names(wu_by_crop))) {
    stop("Water-use data are missing: ", paste(setdiff(required_wu, names(wu_by_crop)), collapse = ", "))
  }
  if (!all(required_county %in% names(df_county))) {
    stop("Historical county data are missing: ", paste(setdiff(required_county, names(df_county)), collapse = ", "))
  }
  
  irrig_by_crop <- wu_by_crop %>%
    filter(Year %in% years_vec, Crop %in% crops) %>%
    group_by(Crop) %>%
    summarise(
      irrig_area = sum(irrArea_ha, na.rm = TRUE) * 1e4,
      .groups = "drop"
    )
  
  total_by_crop <- df_county %>%
    filter(Year %in% years_vec, Crop %in% crops) %>%
    group_by(Crop) %>%
    summarise(
      total_area = sum(area_m2, na.rm = TRUE),
      .groups = "drop"
    )
  
  out <- total_by_crop %>%
    left_join(irrig_by_crop, by = "Crop") %>%
    tidyr::complete(Crop = crops, fill = list(total_area = 0, irrig_area = 0)) %>%
    mutate(
      irrig_area = coalesce(irrig_area, 0),
      irrig_frac = if_else(total_area > 0, irrig_area / total_area, 0),
      irrig_frac = pmax(0, pmin(1, irrig_frac))
    ) %>%
    arrange(match(Crop, crops))

  if (nrow(out) != length(crops) || any(out$total_area <= 0)) {
    stop(
      "Could not build crop-specific historical irrigation fractions for: ",
      paste(out$Crop[out$total_area <= 0], collapse = ", "),
      ". Check the historical county input and irrigation reference years."
    )
  }

  out
}

# --- Dual-domain historical irrigation reference -------------------------
# Keeps geographic domain (whole basin vs alluvial corridor), numerator
# scope (all reported vs four modeled crops), and denominator definition
# explicit. The evaluator-compatible fraction uses its broad modeled
# cultivated-area denominator, while reporting fractions retain direct CDL
# denominators for both domains.
build_dual_domain_irrigation_reference <- function(
    wu_basin,
    wu_alluvial,
    lu_basin,
    lu_alluvial,
    model_basin_cultivated,
    years_vec = 2006:2023,
    modeled_crops = c("Corn", "Soybeans", "Sorghum", "Wheat"),
    cultivated_classes = c(
      "Corn", "Soybeans", "Sorghum", "Wheat", "Other Crops", "Double Crop"
    ),
    expansion_classes = c(
      "Corn", "Soybeans", "Sorghum", "Wheat", "Other Crops", "Double Crop",
      "Grass/Pasture/Shrubland", "Fallow/Barren"
    )) {

  prep_wu <- function(x, domain) {
    stopifnot(all(c("Year", "Crop", "irrArea_ha", "waterUse_m3") %in% names(x)))
    x %>%
      filter(Year %in% years_vec) %>%
      transmute(
        Domain = domain,
        Year = as.integer(Year),
        Crop = as.character(Crop),
        irrigated_area_m2 = pmax(0, as.numeric(irrArea_ha)) * 1e4,
        withdrawal_m3 = pmax(0, as.numeric(waterUse_m3))
      )
  }

  prep_lu <- function(x, domain) {
    stopifnot(all(c("Year", "LandCover", "area_ha") %in% names(x)))
    x %>%
      filter(Year %in% years_vec) %>%
      transmute(
        Domain = domain,
        Year = as.integer(Year),
        LandCover = as.character(LandCover),
        area_m2 = as.numeric(area_ha) * 1e4
      )
  }

  wu <- bind_rows(
    prep_wu(wu_basin, "basin"),
    prep_wu(wu_alluvial, "alluvial")
  )
  lu <- bind_rows(
    prep_lu(lu_basin, "basin"),
    prep_lu(lu_alluvial, "alluvial")
  )

  land_by_year <- lu %>%
    group_by(Domain, Year) %>%
    summarise(
      total_domain_area_m2 = sum(area_m2, na.rm = TRUE),
      cdl_cultivated_area_m2 = sum(
        area_m2[LandCover %in% cultivated_classes], na.rm = TRUE
      ),
      expansion_eligible_area_m2 = sum(
        area_m2[LandCover %in% expansion_classes], na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    left_join(
      model_basin_cultivated %>%
        filter(Year %in% years_vec) %>%
        transmute(Year = as.integer(Year), model_cultivated_area_m2 = LC_cult_base),
      by = "Year"
    ) %>%
    mutate(
      model_cultivated_area_m2 = if_else(
        Domain == "basin", model_cultivated_area_m2, NA_real_
      )
    )

  totals_by_year <- wu %>%
    group_by(Domain, Year) %>%
    summarise(
      irrigated_area_all_reported_m2 = sum(irrigated_area_m2, na.rm = TRUE),
      irrigated_area_modeled_crops_m2 = sum(
        irrigated_area_m2[Crop %in% modeled_crops], na.rm = TRUE
      ),
      withdrawal_all_reported_m3 = sum(withdrawal_m3, na.rm = TRUE),
      withdrawal_modeled_crops_m3 = sum(
        withdrawal_m3[Crop %in% modeled_crops], na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    left_join(land_by_year, by = c("Domain", "Year")) %>%
    mutate(
      frac_all_reported_of_domain =
        irrigated_area_all_reported_m2 / total_domain_area_m2,
      frac_all_reported_of_cdl_cultivated =
        irrigated_area_all_reported_m2 / cdl_cultivated_area_m2,
      frac_modeled_crops_of_cdl_cultivated =
        irrigated_area_modeled_crops_m2 / cdl_cultivated_area_m2,
      frac_modeled_crops_of_model_cultivated =
        irrigated_area_modeled_crops_m2 / model_cultivated_area_m2
    )

  crop_by_year <- wu %>%
    filter(Crop %in% modeled_crops) %>%
    group_by(Domain, Year, Crop) %>%
    summarise(
      irrigated_area_m2 = sum(irrigated_area_m2, na.rm = TRUE),
      withdrawal_m3 = sum(withdrawal_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      Domain = c("basin", "alluvial"),
      Year = years_vec,
      Crop = modeled_crops,
      fill = list(irrigated_area_m2 = 0, withdrawal_m3 = 0)
    ) %>%
    left_join(
      lu %>%
        filter(LandCover %in% modeled_crops) %>%
        group_by(Domain, Year, Crop = LandCover) %>%
        summarise(crop_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop"),
      by = c("Domain", "Year", "Crop")
    ) %>%
    mutate(irrigated_fraction_of_crop = irrigated_area_m2 / crop_area_m2)

  crop_summary <- crop_by_year %>%
    group_by(Domain, Crop) %>%
    summarise(
      irrigated_area_m2 = sum(irrigated_area_m2, na.rm = TRUE),
      crop_area_m2 = sum(crop_area_m2, na.rm = TRUE),
      irrigated_fraction_of_crop = irrigated_area_m2 / crop_area_m2,
      withdrawal_m3 = sum(withdrawal_m3, na.rm = TRUE),
      .groups = "drop"
    )

  basin_eval_fraction <- totals_by_year %>%
    filter(Domain == "basin") %>%
    summarise(value = mean(frac_modeled_crops_of_model_cultivated, na.rm = TRUE)) %>%
    pull(value)

  basin_crop_mix <- crop_summary %>%
    filter(Domain == "basin") %>%
    transmute(
      Crop,
      total_area = crop_area_m2,
      irrig_area = irrigated_area_m2,
      irrig_frac = irrigated_fraction_of_crop
    )

  list(
    reference_years = years_vec,
    modeled_crops = modeled_crops,
    totals_by_year = totals_by_year,
    crop_by_year = crop_by_year,
    crop_summary = crop_summary,
    evaluator = list(
      domain = "basin",
      numerator = "four modeled crops",
      denominator = "model LC_cult_base",
      baseline_irrig_frac = basin_eval_fraction,
      hist_mix = basin_crop_mix
    )
  )
}

summarize_candidate_irrigation_area <- function(
    crop_shares,
    cultivated_area_m2,
    irrigated_fraction,
    hist_mix,
    irrigation_reference = NULL) {
  alloc <- allocate_irrigation(
    crop_shares = crop_shares,
    theta_irrig = irrigated_fraction,
    hist_mix = hist_mix,
    mode = "preserve_hist_mix"
  )
  out <- tibble::tibble(
    Crop = names(alloc$irrig),
    basin_irrigated_area_m2 = as.numeric(alloc$irrig) * cultivated_area_m2,
    basin_model_cultivated_area_m2 = cultivated_area_m2,
    basin_irrigated_fraction_of_model_cultivated =
      basin_irrigated_area_m2 / cultivated_area_m2
  )
  if (!is.null(irrigation_reference)) {
    capture <- irrigation_reference$crop_summary %>%
      select(Domain, Crop, irrigated_area_m2) %>%
      tidyr::pivot_wider(names_from = Domain, values_from = irrigated_area_m2) %>%
      mutate(alluvial_capture = pmin(1, alluvial / basin)) %>%
      select(Crop, alluvial_capture)
    alluvial_totals <- irrigation_reference$totals_by_year %>%
      filter(Domain == "alluvial") %>%
      summarise(
        alluvial_cdl_cultivated_area_m2 = mean(cdl_cultivated_area_m2, na.rm = TRUE),
        alluvial_expansion_eligible_area_m2 = mean(expansion_eligible_area_m2, na.rm = TRUE)
      )
    out <- out %>%
      left_join(capture, by = "Crop") %>%
      mutate(
        alluvial_capture = coalesce(alluvial_capture, 0),
        estimated_alluvial_irrigated_area_m2 =
          basin_irrigated_area_m2 * alluvial_capture,
        alluvial_cdl_cultivated_area_m2 = alluvial_totals$alluvial_cdl_cultivated_area_m2,
        alluvial_expansion_eligible_area_m2 = alluvial_totals$alluvial_expansion_eligible_area_m2,
        estimated_alluvial_irrigated_fraction_of_cdl_cultivated =
          estimated_alluvial_irrigated_area_m2 / alluvial_cdl_cultivated_area_m2,
        estimated_alluvial_irrigated_fraction_of_expansion_eligible =
          estimated_alluvial_irrigated_area_m2 / alluvial_expansion_eligible_area_m2
      )
  }
  out
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
