library(dplyr)
library(tibble)

universal_costs <- list(
  irr_cost_per_m3 = 0.029
)

crop_params <- tribble(
  ~Crop,       ~income_per_kg, ~direct_cost_per_kg, ~fixed_cost_per_kg, ~total_cost_per_kg, ~fert_kgha,
  "Wheat",     0.20,           0.12,                0.05,               0.17,               90,
  "Corn",      0.18,           0.11,                0.05,               0.15,               250,
  "Sorghum",   0.17,           0.09,                0.06,               0.15,               110,
  "Soybeans",  0.37,           0.18,                0.14,               0.32,               55
) %>%
  mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)
