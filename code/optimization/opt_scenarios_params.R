# Scenario preferences for optimization
# ------------------------------------
# This file defines a compact, typed set of knobs for flipping scenarios on/off
# in your NSGA-II workflow. Call scenario_prefs("bau") etc., and pass the list
# to optimize_all_years_nsga2(..., scenario = scenario_prefs("bau")).

# Helper: deep merge two lists (x overwritten by y)
list_merge <- function(x, y) {
  for (n in names(y)) {
    if (!is.null(x[[n]]) && is.list(x[[n]]) && is.list(y[[n]])) {
      x[[n]] <- list_merge(x[[n]], y[[n]])
    } else {
      x[[n]] <- y[[n]]
    }
  }
  x
}

# Defaults that all scenarios inherit unless overridden
scenario_defaults <- function() {
  list(
    # Max allowed cultivated-area increase (fraction of current cultivated area)
    delta_cult_max = 0.15,
    
    # Per-crop share bounds WITHIN cultivated area (0-1); use NA to disable.
    caps = list(
      corn_max = NA_real_,
      soy_max  = NA_real_
    ),
    mins = list(
      wheat_min = NA_real_,
      sorg_min  = NA_real_
    ),
    
    # Land-use change penalties (relative scale); larger = stronger penalty
    # Ordering: Developed ≫ Grassland > Crop (other crops)
    lambda_convert = list(
      dev   = 10.0,
      grass = 2.5,
      crop  = 1.0
    ),
    
    # Basin-level irrigation share constraints (relative to baseline), NA disables
    irr_share_min = NA_real_,
    irr_share_max = NA_real_,
    
    # Objective weights (tune to your eval function implementation)
    # alpha_Irr: penalty per unit irrigation volume
    # beta_LUchange: penalty for any land-use reallocation (smoothness)
    # gamma_expand: penalty for net cultivated expansion (>0)
    weights = list(
      alpha_Irr     = 0.0,
      beta_LUchange = 0.0,
      gamma_expand  = 0.0
    ),
    
    # Per-crop priority multipliers applied to economic objective (>=0)
    # e.g., effective_net_return = net_return * w_priority[crop]
    w_priority = list(
      corn = 1.0,
      soy  = 1.0,
      wheat= 1.0,
      sorg = 1.0
    ),
    
    # Optional: bias the initial population toward certain crop mixes
    seed_bias = list(
      corn = 0.0, soy = 0.0, wheat = 0.0, sorg = 0.0
    )
  )
}

# Scenario presets
scenario_prefs <- function(name = "bau",
                             overrides = NULL) {
    name <- match.arg(tolower(name), 
                      c("bau","diversified","irrigated","rainfed","conservation"))
    base <- scenario_defaults()
    
  scn <- switch(
    name,
    
    # 1) Business as usual – let 4 crops vary within +15% (grassland >> urban)
    bau = list(),
    
    # 2) Diversified expansion – same expansion (+15%), cap corn/soy at 50%; min 10% for wheat/sorghum
    diversified = list(
      caps = list(corn_max = 0.50, soy_max = 0.50),
      mins = list(wheat_min = 0.10, sorg_min = 0.10)
    ),
    
    # 3) Irrigated expansion – same expansion, prioritized for irrigated crops (corn & soy)
    irrigated = list(
      w_priority = list(corn = 1.2, soy = 1.2, wheat = 1.0, sorg = 1.0),
      weights = list(alpha_Irr = 0.0),           # do not penalize irrigation
      irr_share_min = 0.0                        # allow >= baseline; set to NA to disable
    ),
    
    # 4) Rainfed expansion – same expansion, prioritize rainfed (wheat & sorghum); optionally limit irrigation
    rainfed = list(
      w_priority = list(corn = 1.0, soy = 1.0, wheat = 1.2, sorg = 1.2),
      weights = list(alpha_Irr = 1.0),           # penalize irrigation volume
      irr_share_max = 0.0                        # <= baseline; set to NA to disable
      # Optionally also add soft caps to tilt away from corn/soy:
      # caps = list(corn_max = 0.45, soy_max = 0.45)
    ),
    
    # 5) Conservation – penalize any expansion; can enforce hard no-expansion
    conservation = list(
      delta_cult_max = 0.00,                     # hard no net expansion
      weights = list(gamma_expand = 5.0, beta_LUchange = 0.5) # extra discouragement of reallocation
    )
  )
  
  if (!is.null(overrides) && length(overrides)) {
    scn <- list_merge(scn, overrides)
  }
  
  list_merge(base, scn)
}

# ---------- Example usage ----------
# Pass these into optimize function, e.g.:
# opt <- optimize_all_years_nsga2(
#   years_vec = yrs,
#   pop = 240, gens = 240,
#   share_bounds = list(lb = c(0,0,0,0), ub = c(1,1,1,1)),
#   scenario = scenario_prefs("bau")
# )
#
# With tweaks:
# scn_rainfed_tighter <- scenario_prefs("rainfed", overrides = list(
#   caps = list(corn_max = 0.45, soy_max = 0.45),
#   weights = list(alpha_Irr = 1.5)
# ))
#
# opt2 <- optimize_all_years_nsga2(
#   years_vec = yrs,
#   scenario = scn_rainfed_tighter
# )

# ---------- Integration hooks (suggested) ----------
# 1) In feasibility/repair step, enforce:
#    - delta_cult_max
#    - per-crop caps/mins
#    - irr_share_min / irr_share_max relative to baseline
#    Then renormalize crop shares within cultivated area.
#
# 2) In eval function, add penalties/weights:
#    obj_cost <- obj_cost + weights$alpha_Irr * IrrigationVolume
#    obj_cost <- obj_cost + weights$beta_LUchange * LU_realloc_amount
#    obj_cost <- obj_cost + weights$gamma_expand * max(0, delta_cult)
#    net_return_eff <- net_return_raw * w_priority[[crop]]
#
# 3) In initializer, bias by seed_bias if desired.
