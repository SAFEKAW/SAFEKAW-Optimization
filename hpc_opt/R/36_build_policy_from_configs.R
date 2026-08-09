build_policy_from_configs <- function(
    irrigation_technology_cfg = NULL,
    irrigation_extent_cfg = NULL,
    fertilizer_cfg,
    irrigation_cfg = NULL
) {
  # Legacy combined irrigation configs remain supported for older scripts.
  if (!is.null(irrigation_cfg)) {
    irrigation_technology_cfg <- irrigation_technology_cfg %||% irrigation_cfg
    irrigation_extent_cfg <- irrigation_extent_cfg %||% irrigation_cfg
  }
  if (is.null(irrigation_technology_cfg) || is.null(irrigation_extent_cfg)) {
    stop("Supply irrigation technology and extent configs.")
  }

  cap_val <- irrigation_extent_cfg$irrig_area_cap_m2 %||% Inf
  cap_val <- as.numeric(cap_val)
  
  list(
    irrig_frac_factor = irrigation_extent_cfg$irrig_frac_factor %||% 1,
    irr_eff = irrigation_technology_cfg$irr_eff %||% 1,
    irrig_area_cap_m2 = cap_val,
    cap_rule = irrigation_extent_cfg$cap_rule %||% NA_character_,
    fert_factor       = fertilizer_cfg$fert_factor %||% 1,
    fert_gamma        = fertilizer_cfg$fert_gamma %||% 0.1,
    irr_depth_floor_m = irrigation_extent_cfg$irr_depth_floor_m %||%
      irrigation_technology_cfg$irr_depth_floor_m %||% 0.033
  )
}
