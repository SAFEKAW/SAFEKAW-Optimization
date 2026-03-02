# hpc_opt/R/00_setup.R
# Minimal setup for batch/HPC runs: packages + helpers only.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(here)
})

# Common year range (centralize it here so scripts stay clean)
yrs_common <- 1990:2023

# Small helper: robust weighted mean
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}
