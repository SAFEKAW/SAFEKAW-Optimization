# hpc_opt/R/16_net_returns.R
suppressPackageStartupMessages({
  library(dplyr)
})

compute_net_returns <- function(df_rows, crop_params, universal_costs) {
  
  # harmonize fertilizer column name
  if (!"fert_kgHa" %in% names(df_rows) && "fertilizer_kgHa" %in% names(df_rows)) {
    df_rows <- rename(df_rows, fert_kgHa = fertilizer_kgHa)
  }
  
  # harmonize irrigation m3/ha
  if (!"Irrigation_m3Ha_pred" %in% names(df_rows) && "irr_depth_m_pred" %in% names(df_rows)) {
    df_rows <- mutate(df_rows, Irrigation_m3Ha_pred = coalesce(irr_depth_m_pred, 0) * 10000)
  }
  
  # harmonize yield column name expected by economics
  if (!"yield_kgHa_detrended_fit" %in% names(df_rows) && "yield_kgHa_pred" %in% names(df_rows)) {
    df_rows <- rename(df_rows, yield_kgHa_detrended_fit = yield_kgHa_pred)
  }
  
  cp <- crop_params %>%
    mutate(
      income_per_kg      = coalesce(income_per_kg, 0),
      direct_cost_per_kg = coalesce(direct_cost_per_kg, 0),
      fixed_cost_per_kg  = coalesce(fixed_cost_per_kg, 0),
      total_cost_per_kg  = coalesce(total_cost_per_kg, direct_cost_per_kg + fixed_cost_per_kg),
      net_return_per_kg  = coalesce(net_return_per_kg, income_per_kg - total_cost_per_kg),
      fert_kgHa          = coalesce(fert_kgHa, NA_real_)
    ) %>%
    rename(fert_kgHa_cp = fert_kgHa)
  
  df_rows %>%
    left_join(cp, by = "Crop") %>%
    mutate(
      yield_kgHa_detrended_fit = coalesce(yield_kgHa_detrended_fit, 0),
      Irrigation_m3Ha_pred     = coalesce(Irrigation_m3Ha_pred, 0),
      
      fert_kgHa_eff = coalesce(.data$fert_kgHa, .data$fert_kgHa_cp, 0),
      
      revenue_ha          = yield_kgHa_detrended_fit * income_per_kg,
      base_total_costs_ha = yield_kgHa_detrended_fit * total_cost_per_kg,
      irr_cost_ha         = Irrigation_m3Ha_pred * (universal_costs$irr_cost_per_m3 %||% 0),
      
      total_costs_ha      = base_total_costs_ha + irr_cost_ha,
      NetReturn_dollar_ha = revenue_ha - total_costs_ha
    )
}
