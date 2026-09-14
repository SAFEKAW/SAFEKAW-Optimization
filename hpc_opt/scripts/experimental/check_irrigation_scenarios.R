suppressPackageStartupMessages({
  library(here)
  library(yaml)
})

source(here("hpc_opt", "R", "41_objective_wrapper_constrained.R"))
`%||%` <- function(a, b) if (!is.null(a)) a else b
source(here("hpc_opt", "R", "36_build_policy_from_configs.R"))

read_irrigation_config <- function(name) {
  yaml::read_yaml(here(
    "hpc_opt", "config", "irrigation_technology", paste0(name, ".yaml")
  ))
}

current <- read_irrigation_config("current")
efficient <- read_irrigation_config("efficient")
baseline_extent <- yaml::read_yaml(here(
  "hpc_opt", "config", "irrigation_extent", "baseline.yaml"
))
expanded_extent <- yaml::read_yaml(here(
  "hpc_opt", "config", "irrigation_extent", "expanded.yaml"
))
fertilizer_current <- yaml::read_yaml(here(
  "hpc_opt", "config", "fertilizer", "current.yaml"
))

tol <- 1e-8
stopifnot(abs(as.numeric(current$irr_eff) - 1) < tol)
stopifnot(abs((1 - 1 / as.numeric(efficient$irr_eff)) - 0.15) < tol)
stopifnot(abs(as.numeric(baseline_extent$irrig_frac_factor) - 1) < tol)
stopifnot(abs(as.numeric(expanded_extent$irrig_frac_factor) - 1.15) < tol)

expanded_efficient_policy <- build_policy_from_configs(
  irrigation_technology_cfg = efficient,
  irrigation_extent_cfg = expanded_extent,
  fertilizer_cfg = fertilizer_current
)
stopifnot(abs(expanded_efficient_policy$irr_eff - as.numeric(efficient$irr_eff)) < tol)
stopifnot(abs(expanded_efficient_policy$irrig_frac_factor - 1.15) < tol)

fert_only <- decode_management(c(rep(0.25, 4), 0.8), "crop_fert")
stopifnot(abs(fert_only$fert_factor - 0.8) < tol)
stopifnot(abs(fert_only$irrig_frac_factor - 1) < tol)
stopifnot(is.null(fert_only$irr_eff))

fert_extent <- decode_management(
  c(rep(0.25, 4), 0.8, 1.1),
  "crop_fert_irrigfrac"
)
stopifnot(abs(fert_extent$fert_factor - 0.8) < tol)
stopifnot(abs(fert_extent$irrig_frac_factor - 1.1) < tol)
stopifnot(is.null(fert_extent$irr_eff))

message("Fixed irrigation-scenario checks passed.")
