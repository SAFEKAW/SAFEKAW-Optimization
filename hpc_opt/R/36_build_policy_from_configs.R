build_policy_from_configs <- function(irrigation_cfg, fertilizer_cfg) {
  
  cap_val <- irrigation_cfg$irrig_area_cap_m2 %||% Inf
  cap_val <- as.numeric(cap_val)
  
  list(
    irrig_frac_factor = irrigation_cfg$irrig_frac_factor %||% 1,
    irr_eff           = irrigation_cfg$irr_eff %||% 1,
    irrig_area_cap_m2 = cap_val,
    cap_rule          = irrigation_cfg$cap_rule %||% NA_character_,
    fert_factor       = fertilizer_cfg$fert_factor %||% 1,
    fert_gamma        = fertilizer_cfg$fert_gamma %||% 0.1,
    irr_depth_floor_m = irrigation_cfg$irr_depth_floor_m %||% 0.033
  )
}