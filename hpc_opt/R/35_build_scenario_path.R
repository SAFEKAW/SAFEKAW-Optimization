suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

build_scenario_path <- function(landuse_cfg, lu_baseline, baseline_reference) {
  yrs <- sort(unique(lu_baseline$Year))
  
  baseline_crop_shares <- c(
    Corn     = baseline_reference$obs_x[1],
    Soybeans = baseline_reference$obs_x[2],
    Sorghum  = baseline_reference$obs_x[3],
    Wheat    = baseline_reference$wheat
  )
  baseline_crop_shares <- baseline_crop_shares[c("Corn", "Soybeans", "Sorghum", "Wheat")]
  
  
  
  if (abs(sum(baseline_crop_shares) - 1) > 1e-8) {
    stop("Baseline crop shares do not sum to 1.")
  }
  
  if (!is.null(baseline_reference$cult_start_m2)) {
    cult_mean <- baseline_reference$cult_start_m2
  } else {
    cult_mean <- mean(lu_baseline$LC_cult_base, na.rm = TRUE)
  }
  
  if (!is.finite(cult_mean) || cult_mean <= 0) {
    stop("cult_mean must be finite and > 0, got: ", cult_mean)
  }
  
  if ("LC_grass_base" %in% names(lu_baseline)) {
    grass_mean <- mean(lu_baseline$LC_grass_base, na.rm = TRUE)
    cap_cult_mean <- cult_mean + grass_mean
  } else {
    nondev_mean <- mean(lu_baseline$LC_total - lu_baseline$LC_dev_base, na.rm = TRUE)
    grass_mean <- nondev_mean - cult_mean
    cap_cult_mean <- nondev_mean
  }
  
  mode <- landuse_cfg$mode
  
  # initialize state from historical baseline
  Cult_m2_prev <- cult_mean
  crop_area_prev <- cult_mean * baseline_crop_shares
  
  out <- vector("list", length(yrs))
  
  for (i in seq_along(yrs)) {
    Y <- yrs[i]
    
    lu_y <- lu_baseline %>% filter(Year == Y)
    if (nrow(lu_y) != 1L) stop("Expected exactly one lu_baseline row for Year = ", Y)
    
    LC_total <- lu_y$LC_total
    LC_dev_base <- lu_y$LC_dev_base
    avail_nondeveloped_y <- LC_total - LC_dev_base
    cap_cult_year <- min(cap_cult_mean, avail_nondeveloped_y) #cap is currently based on historical mean cap, not evolving grass pool
    
    if (identical(mode, "fixed")) {
      Cult_m2 <- cult_mean
      crop_area <- cult_mean * baseline_crop_shares
      shares <- baseline_crop_shares
      
    } else if (identical(mode, "linear_expansion")) {
      inc_val <- landuse_cfg$annual_increment$value
      
      alloc <- c(
        Corn     = landuse_cfg$new_area_allocation$corn,
        Soybeans = landuse_cfg$new_area_allocation$soybean,
        Sorghum  = landuse_cfg$new_area_allocation$sorghum,
        Wheat    = landuse_cfg$new_area_allocation$wheat
      )
      
      if (abs(sum(alloc) - 1) > 1e-8) {
        stop("new_area_allocation must sum to 1 in land-use YAML.")
      }
      
      delta_area_m2_annual <- cult_mean * inc_val
      Cult_m2_raw <- Cult_m2_prev + delta_area_m2_annual
      
      Cult_m2 <- min(Cult_m2_raw, cap_cult_year)
      
      delta_area_m2 <- max(Cult_m2 - Cult_m2_prev, 0)
      
      crop_area <- crop_area_prev + delta_area_m2 * alloc
      shares <- crop_area / sum(crop_area)
      
    } else {
      stop("Unsupported land-use mode: ", mode)
    }
    
    if (!is.finite(Cult_m2) || !is.finite(cult_mean) || cult_mean <= 0) {
      stop("Invalid cultivated area scaling at Year = ", Y,
           ": Cult_m2 = ", Cult_m2,
           ", cult_mean = ", cult_mean)
    }
    
    out[[i]] <- tibble(
      landuse_name = landuse_cfg$name,
      Year = Y,
      cult_area_factor = Cult_m2 / cult_mean, #u_y$LC_cult_base,
      Cult_m2 = Cult_m2,
      cult_mean_m2 = cult_mean,
      grass_mean_m2 = grass_mean,
      cap_cult_mean_m2 = cap_cult_mean,
      cap_cult_year_m2 = cap_cult_year,
      corn = unname(shares["Corn"]),
      soy = unname(shares["Soybeans"]),
      sor = unname(shares["Sorghum"]),
      wheat = unname(shares["Wheat"])
    )
    
    # update state for next year
    Cult_m2_prev <- Cult_m2
    crop_area_prev <- crop_area
  }
  
  bind_rows(out)
}