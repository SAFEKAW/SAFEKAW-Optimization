# hpc_opt/R/17_fert_multiplier.R
# Fertilizer → nitrate flux multiplier using elasticity gamma
fert_multiplier <- function(Fert_policy_kg, Fert_ref_kg, gamma = 0.1, eps = 1e-9) {
  # ratio r = policy / reference, clipped to avoid log(0)
  r <- (Fert_policy_kg + eps) / (Fert_ref_kg + eps)
  r <- pmax(eps, r)
  as.numeric(exp(gamma * log(r)))   # == r^gamma
}