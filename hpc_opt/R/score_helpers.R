# hpc_opt/R/score_helpers.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---------------------------
# 1) Build global ECDF scorers
# ---------------------------
build_global_ecdfs <- function(df, cols = c("Nitrate_kgyr", "Profit_usd", "Irr_m3")) {
  stopifnot(all(cols %in% names(df)))
  # return a named list of ecdf functions + cached ranges
  ecdfs <- lapply(cols, function(nm) {
    v <- df[[nm]]
    v <- v[is.finite(v)]
    if (length(v) < 10) stop("Not enough finite values to build ECDF for: ", nm)
    stats::ecdf(v)
  })
  names(ecdfs) <- cols
  ecdfs
}

# ------------------------------------------
# 2) Score objectives into [0,1] (higher=better)
# ------------------------------------------
score_objectives <- function(df, ecdfs,
                             nitrate_col = "Nitrate_kgyr",
                             profit_col  = "NegProfit_usd",
                             irr_col     = "Irr_m3") {
  stopifnot(all(c(nitrate_col, profit_col, irr_col) %in% names(df)))
  stopifnot(all(c(nitrate_col, profit_col, irr_col) %in% names(ecdfs)))
  
  df %>%
    mutate(
      # Percentiles in the global feasible space
      p_n_raw = ecdfs[[nitrate_col]](.data[[nitrate_col]]),
      p_p_raw = ecdfs[[profit_col]] (.data[[profit_col]]),
      p_i_raw = ecdfs[[irr_col]]    (.data[[irr_col]]),
      
      # Convert to "goodness" scores (higher=better):
      # minimize nitrate/irrigation => 1 - percentile
      sN = 1 - p_n_raw,
      # maximize profit => percentile
      sP = 1- p_p_raw,
      # minimize irrigation => 1 - percentile
      sI = 1 - p_i_raw
    )
}

# ------------------------------------------
# 3) Convert scores to ternary "shares" (sum=1)
# ------------------------------------------
to_ternary_shares <- function(df, score_cols = c("sN","sP","sI"), eps = 1e-12) {
  stopifnot(all(score_cols %in% names(df)))
  df %>%
    mutate(
      s_sum = pmax(eps, .data[[score_cols[1]]] + .data[[score_cols[2]]] + .data[[score_cols[3]]]),
      pN = .data[[score_cols[1]]] / s_sum,
      pP = .data[[score_cols[2]]] / s_sum,
      pI = .data[[score_cols[3]]] / s_sum
    )
}

# ------------------------------------------
# 4) Baseline deltas (radial wants relative-to-baseline)
# ------------------------------------------
add_baseline_deltas <- function(df, baseline_row) {
  # baseline_row: a 1-row df containing sN,sP,sI
  stopifnot(nrow(baseline_row) == 1)
  stopifnot(all(c("sN","sP","sI") %in% names(df)))
  stopifnot(all(c("sN","sP","sI") %in% names(baseline_row)))
  
  b <- baseline_row[1, c("sN","sP","sI")]
  df %>%
    mutate(
      dN = sN - b$sN,
      dP = sP - b$sP,
      dI = sI - b$sI
    )
}

# ------------------------------------------
# 5) ggplot-only ternary (no ggtern dependency)
#    Map (pN,pP,pI) to 2D barycentric coordinates.
# ------------------------------------------
ternary_to_xy <- function(pN, pP, pI) {
  # triangle vertices:
  # N at top, P bottom-left, I bottom-right
  # x = pI + 0.5*pN, y = (sqrt(3)/2)*pN
  x <- pI + 0.5 * pN
  y <- (sqrt(3)/2) * pN
  list(x = x, y = y)
}

ternary_axes_df <- function() {
  # triangle outline
  tri <- tibble::tibble(
    x = c(0, 1, 0.5, 0),
    y = c(0, 0, sqrt(3)/2, 0)
  )
  tri
}

# ------------------------------------------
# 6) Radial plot data prep (3 spokes)
# ------------------------------------------
radial_long <- function(df, id_cols = c("Scenario","run_id","sol_id"),
                        delta_cols = c(dN = "dN", dP = "dP", dI = "dI")) {
  stopifnot(all(unname(delta_cols) %in% names(df)))
  keep <- intersect(id_cols, names(df))
  df %>%
    select(any_of(keep), all_of(unname(delta_cols))) %>%
    pivot_longer(cols = all_of(unname(delta_cols)),
                 names_to = "metric", values_to = "delta") %>%
    mutate(
      metric = recode(metric,
                      !!delta_cols["dN"] := "Nitrate (better →)",
                      !!delta_cols["dP"] := "Profit (better →)",
                      !!delta_cols["dI"] := "Irrigation (better →)"),
      metric = factor(metric, levels = c("Nitrate (better →)","Profit (better →)","Irrigation (better →)"))
    )
}