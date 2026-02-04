`%||%` <- function(a, b) if (is.null(a)) b else a

fert_multiplier <- function(Fert_kg_total, Fert_ref_kg, gamma = 0.1) {
  r <- pmax(1e-6, Fert_kg_total / Fert_ref_kg)
  r^gamma
}
