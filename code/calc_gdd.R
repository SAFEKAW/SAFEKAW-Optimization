# Compatibility shim.
# The canonical crop-season definitions and GDD implementation now live in
# hpc_opt/R/calc_gdd.R so historical and future climate workflows cannot use
# different crop constants.
source(here::here("hpc_opt", "R", "calc_gdd.R"))
