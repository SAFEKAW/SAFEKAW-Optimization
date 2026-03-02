# ==========================================
# 04_viz_all.R
# One script: load outputs -> global scoring -> frontiers/knee/corners -> ternary + radial
# ==========================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(purrr)
})

# ---- sources ----
source(here("hpc_opt","R","10_helpers.R"))       # %||% , hist_mix builders, baseline mix helper, etc.
source(here("hpc_opt","R","score_helpers.R"))    # build_global_ecdfs(), score_objectives(), to_ternary_shares(),
# ternary_to_xy(), ternary_axes_df(), add_baseline_deltas(), radial_long()

# ------------------------------------------
# Palettes
# ------------------------------------------
pal_landcover <- c(
  "Grass/Pasture/Hay" = "#C1FFC1",
  "Cultivated"        = "#8B8B00",
  "Developed"         = "darkgrey"
)

pal_crops <- c(
  "Corn"     = "#EEC900",
  "Soybeans" = "#548B54",
  "Wheat"    = "#8B7355",
  "Sorghum"  = "darkorange"
)

scale_fill_landcover <- function(...) ggplot2::scale_fill_manual(values = pal_landcover, ...)
scale_color_landcover <- function(...) ggplot2::scale_color_manual(values = pal_landcover, ...)
scale_fill_crops <- function(...) ggplot2::scale_fill_manual(values = pal_crops, ...)
scale_color_crops <- function(...) ggplot2::scale_color_manual(values = pal_crops, ...)

# ------------------------------------------
# Paths
# ------------------------------------------
fig_dir <- here("hpc_opt","figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

feasible_path <- here("hpc_opt","outputs","feasible_space.csv")
baseline_path <- here("hpc_opt","outputs","baseline_point.csv")

# Option A: scenarios as CSVs (optional)
scenario_glob <- here("hpc_opt","outputs","scenarios","*.csv")

# Option B: optimization runs (NSGA2 rds) (optional)
runs_root <- here("hpc_opt","outputs","runs")

# ------------------------------------------
# 0) Load feasible + baseline (+ optional scenarios/runs)
# ------------------------------------------
stopifnot(file.exists(feasible_path))
stopifnot(file.exists(baseline_path))

feasible <- read_csv(feasible_path, show_col_types = FALSE) %>%
  mutate(Scenario = coalesce(Scenario, "feasible_space"))

baseline_row <- read_csv(baseline_path, show_col_types = FALSE) %>%
  mutate(Scenario = "baseline_point")

# ---- optional scenario csvs ----
scenario_files <- Sys.glob(scenario_glob)
scenarios_csv <- if (length(scenario_files) > 0) {
  bind_rows(lapply(scenario_files, read_csv, show_col_types = FALSE))
} else {
  tibble()
}

# ---- optional NSGA2 runs (.rds) ----
read_one_run <- function(path) {
  res <- readRDS(path)
  Scenario <- basename(dirname(path))
  run_id <- paste0(Scenario, "::", tools::file_path_sans_ext(basename(path)))
  
  obj <- as.data.frame(res$value)
  par <- as.data.frame(res$par)
  
  # Expect optimizer minimizes: (N, NegProfit, Irr) in that order
  colnames(obj) <- c("Nitrate_kgyr","NegProfit_usd","Irr_m3")
  colnames(par) <- c("Corn","Soybeans","Sorghum")
  
  bind_cols(obj, par) %>%
    mutate(
      sol_id = row_number(),
      Wheat = 1 - (Corn + Soybeans + Sorghum),
      Profit_usd = -NegProfit_usd,
      Scenario = Scenario,
      run_id = run_id,
      sol_key = paste0(run_id, "::", sol_id)
    )
}

rds_files <- if (dir.exists(runs_root)) {
  list.files(runs_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
} else character(0)

solutions_rds <- if (length(rds_files) > 0) {
  bind_rows(lapply(rds_files, read_one_run)) %>%
    mutate(
      across(c(Corn, Soybeans, Sorghum, Wheat), ~pmax(0, .x)),
      s = Corn + Soybeans + Sorghum + Wheat,
      across(c(Corn, Soybeans, Sorghum, Wheat), ~ .x / ifelse(s > 0, s, 1))
    ) %>%
    select(-s)
} else {
  tibble()
}

# ------------------------------------------
# 0b) Standardize columns across datasets
# ------------------------------------------
# Require objectives everywhere:
#   Nitrate_kgyr, NegProfit_usd, Irr_m3
# Optional: Profit_usd

standardize_obj_cols <- function(df) {
  out <- df
  
  # profit sign conventions
  if (!("NegProfit_usd" %in% names(out)) && ("Profit_usd" %in% names(out))) {
    out <- out %>% mutate(NegProfit_usd = -Profit_usd)
  }
  if (!("Profit_usd" %in% names(out)) && ("NegProfit_usd" %in% names(out))) {
    out <- out %>% mutate(Profit_usd = -NegProfit_usd)
  }
  
  out
}

feasible     <- standardize_obj_cols(feasible)
baseline_row <- standardize_obj_cols(baseline_row)
scenarios_csv <- standardize_obj_cols(scenarios_csv)
solutions_rds <- standardize_obj_cols(solutions_rds)


# keep only key columns if they exist
keep_cols <- c(
  "Scenario","run_id","sol_id","sol_key",
  "Nitrate_kgyr","NegProfit_usd","Profit_usd","Irr_m3",
  "Corn","Soybeans","Sorghum","Wheat",
  "corn","soy","sor","wheat","cult_area","fert_eff","irr_area","irr_eff"
)

feasible     <- feasible %>% select(any_of(keep_cols))
baseline_row <- baseline_row %>% select(any_of(keep_cols))
scenarios_csv <- scenarios_csv %>% select(any_of(keep_cols))
solutions_rds <- solutions_rds %>% select(any_of(keep_cols))

# Combine everything for scoring/plotting
all_df <- bind_rows(feasible, scenarios_csv, solutions_rds, baseline_row)

req <- c("Scenario","Nitrate_kgyr","NegProfit_usd","Irr_m3")
missing <- setdiff(req, names(all_df))
if (length(missing)) stop("Missing required columns: ", paste(missing, collapse=", "))

stopifnot(any(all_df$Scenario == "baseline_point"))

# ------------------------------------------
# 1) Global scoring (ECDFs built from feasible space only)
# ------------------------------------------
ecdfs <- build_global_ecdfs(feasible, cols = c("Nitrate_kgyr","Profit_usd","Irr_m3"))
names(ecdfs)

score_objectives2 <- function(df, ecdfs,
                              nitrate_col = "Nitrate_kgyr",
                              profit_col  = "Profit_usd",
                              irr_col     = "Irr_m3") {
  stopifnot(all(c(nitrate_col, profit_col, irr_col) %in% names(df)))
  stopifnot(all(c(nitrate_col, profit_col, irr_col) %in% names(ecdfs)))
  
  df %>%
    mutate(
      p_n = ecdfs[[nitrate_col]](.data[[nitrate_col]]),
      p_p = ecdfs[[profit_col]] (.data[[profit_col]]),
      p_i = ecdfs[[irr_col]]    (.data[[irr_col]]),
      
      # higher = better
      sN = 1 - p_n,     # minimize nitrate
      sP = p_p,         # maximize profit
      sI = 1 - p_i      # minimize irrigation
    )
}

scored <- all_df %>%
  score_objectives2(ecdfs) %>%
  to_ternary_shares() %>%
  mutate(
    xy = purrr::pmap(list(pN, pP, pI), ~ ternary_to_xy(..1, ..2, ..3)),
    x = purrr::map_dbl(xy, "x"),
    y = purrr::map_dbl(xy, "y")
  ) %>%
  select(-xy)

baseline_point_scored <- scored %>%
  filter(Scenario == "baseline_point") %>% slice(1)

stopifnot(nrow(baseline_point_scored) == 1)

baseline_point_scored %>%
  transmute(
    sN, sP, sI,
    pN = scales::percent(pN, 1),
    pP = scales::percent(pP, 1),
    pI = scales::percent(pI, 1)
  )

# ------------------------------------------
# 2) Frontier + knee/corners (optional, if you have optimizer runs)
#    Only do this on optimization solutions (not feasible space cloud).
# ------------------------------------------
nondominated <- function(df, obj_cols) {
  if (nrow(df) == 0) return(df)
  X <- as.matrix(df[, obj_cols, drop = FALSE])
  keep <- rep(TRUE, nrow(X))
  for (i in seq_len(nrow(X))) {
    if (!keep[i]) next
    dom_i <- apply(X, 1, function(r) all(r <= X[i, ]) && any(r < X[i, ]))
    dom_i[i] <- FALSE
    if (any(dom_i)) keep[i] <- FALSE
  }
  df[keep, , drop = FALSE]
}

knee_by_max_distance <- function(df, xcol, ycol) {
  d <- df %>%
    mutate(.x = as.numeric(.data[[xcol]]), .y = as.numeric(.data[[ycol]])) %>%
    filter(is.finite(.x), is.finite(.y)) %>%
    arrange(.x, desc(.y))
  if (nrow(d) < 3) return(d[0, , drop = FALSE])
  
  x1 <- d$.x[1]; y1 <- d$.y[1]
  x2 <- d$.x[nrow(d)]; y2 <- d$.y[nrow(d)]
  num <- abs((y2 - y1) * d$.x - (x2 - x1) * d$.y + x2*y1 - y2*x1)
  den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
  d$.knee_dist <- if (den > 0) num / den else 0
  d %>% slice(which.max(.knee_dist)) %>% select(-.x, -.y, -.knee_dist)
}

opt_solutions <- scored %>%
  filter(!Scenario %in% c("feasible_space", "baseline_point")) %>% #""baseline",", 
  filter(!is.na(run_id) | !is.na(sol_id) | !is.na(sol_key))

frontiers <- opt_solutions %>%
  group_by(Scenario) %>%
  group_modify(~ nondominated(.x, obj_cols = c("Nitrate_kgyr","NegProfit_usd","Irr_m3"))) %>%
  ungroup()

corners <- frontiers %>%
  group_by(Scenario) %>%
  summarise(
    sol_key_minN = sol_key[which.min(Nitrate_kgyr)],
    sol_key_maxP = sol_key[which.max(NegProfit_usd)],
    sol_key_minI = sol_key[which.min(Irr_m3)],
    .groups = "drop"
  ) %>%
  pivot_longer(-Scenario, names_to = "type", values_to = "sol_key") %>%
  left_join(frontiers %>% select(Scenario, sol_key, everything()),
            by = c("Scenario","sol_key")) %>%
  mutate(type = recode(type,
                       sol_key_minN = "Corner: min nitrate",
                       sol_key_maxP = "Corner: max profit",
                       sol_key_minI = "Corner: min irrigation"))

knees <- frontiers %>%
  group_by(Scenario) %>%
  group_modify(~ knee_by_max_distance(.x, xcol = "Nitrate_kgyr", ycol = "Profit_usd")) %>%
  ungroup() %>%
  mutate(type = "Knee")

special_pts <- bind_rows(
  corners %>% select(Scenario, type, sol_key, everything()),
  knees   %>% select(Scenario, type, sol_key, everything())
)

# ------------------------------------------
# 3) Ternary (ggplot) with ggtern-like labels + ticks
# ------------------------------------------

ternary_ticks_df <- function(breaks = seq(0.2, 0.8, by = 0.2), tick_len = 0.02) {
  h <- sqrt(3)/2
  
  # pN ticks on left edge
  pN_left <- tibble::tibble(
    axis = "pN", val = breaks,
    x = 0.5 * breaks,
    y = h * breaks,
    lab = scales::percent(breaks, accuracy = 1)
  ) %>%
    dplyr::mutate(
      xend = x + tick_len * 0.8,
      yend = y + tick_len * 0,
      lx = xend + 0.01,
      ly = yend
    )
  
  # pN ticks on right edge
  pN_right <- tibble::tibble(
    axis = "pN", val = breaks,
    x = 1 - 0.5 * breaks,
    y = h * breaks,
    lab = scales::percent(breaks, accuracy = 1)
  ) %>%
    dplyr::mutate(
      xend = x - tick_len * 0.8,
      yend = y + tick_len * 0,
      lx = xend - 0.01,
      ly = yend
    )
  
  # pP ticks on base (x = 1 - pP)
  pP_base <- tibble::tibble(
    axis = "pP", val = breaks,
    x = 1 - breaks,
    y = 0,
    lab = scales::percent(breaks, accuracy = 1)
  ) %>%
    dplyr::mutate(
      xend = x + tick_len * 0,
      yend = y + tick_len * 1,
      lx = xend,
      ly = yend + 0.01
    )
  
  # pI ticks on base (x = pI)
  pI_base <- tibble::tibble(
    axis = "pI", val = breaks,
    x = breaks,
    y = 0,
    lab = scales::percent(breaks, accuracy = 1)
  ) %>%
    dplyr::mutate(
      xend = x + tick_len * 0,
      yend = y + tick_len * 1,
      lx = xend,
      ly = yend + 0.04
    )
  
  dplyr::bind_rows(pN_left, pN_right, pP_base, pI_base)
}


ternary_baseline_guides <- function(baseline_point_scored) {
  b <- baseline_point_scored %>% dplyr::slice(1)
  stopifnot(all(c("pN","pP","pI","x","y") %in% names(b)))
  
  bx <- b$x; by <- b$y
  h <- sqrt(3)/2
  
  # intersections for classic read-off lines
  x_left  <- by / sqrt(3)
  x_right <- 1 - by / sqrt(3)
  
  x_base_I  <- bx - by/sqrt(3)
  y_right_I <- sqrt(3) * (1 - bx)
  
  x_base_P <- bx + by/sqrt(3)
  y_left_P <- sqrt(3) * bx
  
  proj_lines <- dplyr::bind_rows(
    tibble::tibble(metric="Nitrate",    x=x_left,  y=by, xend=x_right, yend=by),
    tibble::tibble(metric="Profit",     x=0, y=y_left_P, xend=x_base_P, yend=0),
    tibble::tibble(metric="Irrigation", x=x_base_I, y=0, xend=1, yend=y_right_I)
  )
  
  arrow_df <- tibble::tibble(
    metric = c("Nitrate","Profit","Irrigation"),
    x0 = c(x_left,  x_base_P, x_base_I),
    y0 = c(by,      0,        0),
    x1 = bx, y1 = by,
    lab = c(
      scales::percent(b$pN, accuracy = 1),
      scales::percent(b$pP, accuracy = 1),
      scales::percent(b$pI, accuracy = 1)
    )
  )
  
  list(proj_lines = proj_lines, arrow_df = arrow_df)
}


tri   <- ternary_axes_df()
ticks <- ternary_ticks_df()
guides <- ternary_baseline_guides(baseline_point_scored)

p_tern <- ggplot() +
  geom_path(data = tri, aes(x, y), linewidth = 0.9) +
  geom_segment(data = ticks, aes(x=x, y=y, xend=xend, yend=yend), linewidth = 0.4) +
  geom_text(data = ticks, aes(x=lx, y=ly, label=lab), size = 3) +
  
  # feasible space / scenarios cloud
  geom_point(data = scored, aes(x, y, color = Scenario), alpha = 0.25, size = 1.4) +
  
  # baseline point
  geom_point(
    data = baseline_point_scored,
    aes(x, y),
    inherit.aes = FALSE,
    shape = 21, fill = "white", color = "black",
    size = 4, stroke = 1.1
  ) +
  
  # read-off lines
 # geom_segment(
#    data = guides$proj_lines,
#    aes(x=x, y=y, xend=xend, yend=yend),
#    inherit.aes = FALSE,
#    linetype = "dashed",
#    linewidth = 0.6,
#    alpha = 0.8
#  ) +
  
  # arrows (edge -> baseline point)
  geom_segment(
    data = guides$arrow_df,
    aes(x=x0, y=y0, xend=x1, yend=y1),
    inherit.aes = FALSE,
    arrow = grid::arrow(length = grid::unit(0.18, "cm")),
    linewidth = 0.8
  ) +
  
  # labels at arrow starts
  geom_label(
    data = guides$arrow_df,
    aes(x=x0, y=y0, label=paste0(metric, ": ", lab)),
    inherit.aes = FALSE,
    size = 3,
    label.size = 0.2,
    alpha = 0.95
  ) +
  
  annotate("text", x = 0.5, y = sqrt(3)/2 + 0.055, label = "T: Nitrate (higher = lower nitrate)", size = 4.0) +
  annotate("text", x = -0.02, y = -0.055, label = "L: Profit (higher = higher profit)", hjust = 0, size = 4.0) +
  annotate("text", x =  1.02, y = -0.055, label = "R: Irrigation (higher = lower irrigation)", hjust = 1, size = 4.0) +
  
  coord_equal(xlim = c(-0.08, 1.08), ylim = c(-0.08, sqrt(3)/2 + 0.10), expand = FALSE) +
  labs(
    title = "Baseline position in feasible tradeoff space (ECDF goodness shares)",
    subtitle = "Shares sum to 100% and reflect relative standing vs feasible space on each objective.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

print(p_tern)


## DO NOT TRUST PLOTS AFTER THIS POINT (YET)
# ------------------------------------------
# 4) Radial deltas (violin) + TRUE spider plot
# ------------------------------------------
scored2 <- scored %>% add_baseline_deltas(baseline_scored)

# ---- violin deltas (what you already have, cleaned) ----
rad_long <- radial_long(scored2, id_cols = intersect(c("Scenario","run_id","sol_id","sol_key"), names(scored2)))

p_rad_violin <- ggplot(rad_long, aes(x = metric, y = delta, fill = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_violin(alpha = 0.35, width = 0.9, color = NA) +
  geom_boxplot(width = 0.18, outlier.alpha = 0.12) +
  facet_wrap(~Scenario, ncol = 1) +
  labs(
    title = "Scenario performance relative to baseline (global-percentile deltas)",
    subtitle = "Above 0 = better than baseline; below 0 = worse. Deltas computed on ECDF-based scores (0–1).",
    x = NULL, y = "Δ score"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title.position = "plot", legend.position = "none")

#ggsave(file.path(fig_dir, "radial_deltas_violin.png"), p_rad_violin, width = 9, height = 9, dpi = 300)
print(p_rad_violin)


# ---- TRUE radial “spider” plot for baseline + special points (knee/corners)
# Uses deltas vs baseline on 3 axes (N, Profit, Irr), so it's directly “relative to baseline”.
make_spider_df <- function(df, id_cols = c("Scenario","type"), metrics = c("dN","dP","dI")) {
  
  long <- df %>%
    select(any_of(c(id_cols, metrics))) %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value") %>%
    mutate(
      metric = recode(metric, dN = "Nitrate", dP = "Profit", dI = "Irrigation"),
      metric = factor(metric, levels = c("Nitrate","Profit","Irrigation")),
      angle  = (as.integer(metric) - 1) / nlevels(metric) * 2*pi,
      x = value * sin(angle),
      y = value * cos(angle)
    )
  
  # Close polygon *within each group*
  long %>%
    arrange(across(all_of(id_cols)), metric) %>%
    group_by(across(all_of(id_cols))) %>%
    reframe(bind_rows(pick(everything()),
                      slice(pick(everything()), 1))) %>%
    ungroup()
}
# pick what to spider:
# - baseline polygon = zero deltas
# - special points: knee + corners if available; else do nothing
if (nrow(special_pts) > 0) {
  # bring in the deltas/scored fields for special points by sol_key
  special_scored <- scored2 %>%
    filter(!is.na(sol_key)) %>%
    semi_join(special_pts %>% select(Scenario, sol_key, type), by = c("Scenario","sol_key")) %>%
    left_join(special_pts %>% select(Scenario, sol_key, type), by = c("Scenario","sol_key")) %>%
    distinct(Scenario, type, sol_key, .keep_all = TRUE)
  
  # baseline as a row with 0 deltas
  baseline_spider <- baseline_scored %>%
    mutate(Scenario = "baseline", type = "Baseline",
           dN = 0, dP = 0, dI = 0) %>%
    select(Scenario, type, dN, dP, dI)
  
  spider_in <- bind_rows(
    baseline_spider,
    special_scored %>% select(Scenario, type, dN, dP, dI)
  )
  
  spider_df <- make_spider_df(spider_in, id_cols = c("Scenario","type"))
  
  spider_in2 <- spider_in %>%
    mutate(
      dN = pmax(dN, 0),
      dP = pmax(dP, 0),
      dI = pmax(dI, 0)
    )
  
  # reference circles
  max_r <- max(c(spider_in2$dN, spider_in2$dP, spider_in2$dI), na.rm = TRUE)
  
  # rings: 0 to max, nice breaks
  ring_levels <- pretty(c(0, max_r), n = 5)
  
  rings <- tidyr::expand_grid(
    r = ring_levels,
    ang = seq(0, 2*pi, length.out = 240)
  ) %>%
    mutate(x = r*sin(ang), y = r*cos(ang))
  
  # axis length a touch beyond max
  axis_len <- max_r * 1.05
  
  axis_df <- tibble(
    metric = factor(c("Nitrate","Profit","Irrigation"),
                    levels = c("Nitrate","Profit","Irrigation")),
    angle  = (as.integer(metric) - 1) / 3 * 2*pi,
    xend   = axis_len*sin(angle),
    yend   = axis_len*cos(angle),
    xlab   = axis_len*1.12*sin(angle),
    ylab   = axis_len*1.12*cos(angle)
  )
  
  ring_labs <- tibble(r = ring_levels) %>%
    mutate(x = 0, y = r, lab = sprintf("%.2f", r))
  
  
  p_spider <- ggplot() +
    geom_path(data = rings, aes(x, y, group = r), linewidth = 0.3, alpha = 0.35) +
    geom_segment(data = axis_df, aes(x = 0, y = 0, xend = xend, yend = yend), linewidth = 0.5) +
    geom_text(data = axis_df, aes(x = xlab, y = ylab, label = as.character(metric)), size = 4) +
    geom_text(data = ring_labs, aes(x, y, label = lab), size = 3, alpha = 0.6)+
    geom_polygon(
      data = spider_df,
      aes(x, y, group = interaction(Scenario, type), fill = type),
      alpha = 0.25, color = "grey30", linewidth = 0.4
    ) +
    geom_path(
      data = spider_df,
      aes(x, y, group = interaction(Scenario, type), color = type),
      linewidth = 0.7
    ) +
    facet_wrap(~Scenario, ncol = 2) +
    coord_equal() +
    labs(
      title = "Radial deltas vs baseline (spider plot)",
     # subtitle = "Axes are Δ scores (ECDF-based): positive = better than baseline; negative = worse. Baseline is the zero polygon.",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
  
  #ggsave(file.path(fig_dir, "radial_spider_specialPoints.png"), p_spider, width = 12, height = 8, dpi = 300)
  print(p_spider)
} else {
  message("No optimization run special points found (knee/corners). Spider plot skipped.")
}



#spider pt 2

baseline_frontier <- scored2 %>% 
  filter(Scenario == "baseline") %>%
  mutate(
    dN = pmax(dN, 0),
    dP = pmax(dP, 0),
    dI = pmax(dI, 0)
  )


env_probs <- c(0.10, 0.90)  # change to c(0,1) for min-max

baseline_env <- baseline_frontier %>%
  summarise(
    dN_lo = quantile(dN, env_probs[1], na.rm = TRUE),
    dN_hi = quantile(dN, env_probs[2], na.rm = TRUE),
    dP_lo = quantile(dP, env_probs[1], na.rm = TRUE),
    dP_hi = quantile(dP, env_probs[2], na.rm = TRUE),
    dI_lo = quantile(dI, env_probs[1], na.rm = TRUE),
    dI_hi = quantile(dI, env_probs[2], na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("metric","bound"),
    names_pattern = "d([NPI])_(lo|hi)",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric, N = "Nitrate", P = "Profit", I = "Irrigation"),
    metric = factor(metric, levels = c("Nitrate","Profit","Irrigation")),
    bound  = factor(bound, levels = c("lo","hi"))
  )

# axis geometry
axis_df <- tibble(
  metric = factor(c("Nitrate","Profit","Irrigation"), levels = c("Nitrate","Profit","Irrigation")),
  angle  = (as.integer(metric) - 1) / 3 * 2*pi
)

# join angles, compute endpoints for lo/hi
baseline_env_xy <- baseline_env %>%
  left_join(axis_df, by = "metric") %>%
  mutate(
    x = value * sin(angle),
    y = value * cos(angle)
  ) %>%
  select(metric, bound, value, x, y) %>%
  tidyr::pivot_wider(
    names_from = bound,
    values_from = c(value, x, y)
  ) %>%
  transmute(
    metric,
    x_lo = x_lo, y_lo = y_lo,
    x_hi = x_hi, y_hi = y_hi,
    r_lo = value_lo, r_hi = value_hi
  )

max_r <- max(baseline_env_xy$r_hi, na.rm = TRUE)
axis_len <- max_r * 1.10

ring_levels <- pretty(c(0, axis_len), n = 5)

rings <- tidyr::expand_grid(
  r = ring_levels,
  ang = seq(0, 2*pi, length.out = 240)
) %>%
  mutate(x = r*sin(ang), y = r*cos(ang))

axis_draw <- axis_df %>%
  mutate(
    xend = axis_len*sin(angle),
    yend = axis_len*cos(angle),
    xlab = axis_len*1.14*sin(angle),
    ylab = axis_len*1.14*cos(angle)
  )

p_spider_base <- ggplot() +
  geom_path(data = rings, aes(x, y, group = r), linewidth = 0.3, alpha = 0.35) +
  geom_segment(data = axis_draw, aes(x = 0, y = 0, xend = xend, yend = yend), linewidth = 0.5) +
  geom_text(data = axis_draw, aes(x = xlab, y = ylab, label = as.character(metric)), size = 4) +
  
  # Baseline envelope as range bars on each axis
  geom_segment(
    data = baseline_env_xy,
    aes(x = x_lo, y = y_lo, xend = x_hi, yend = y_hi),
    linewidth = 6, alpha = 0.25, lineend = "round"
  ) +
  geom_segment(
    data = baseline_env_xy,
    aes(x = x_lo, y = y_lo, xend = x_hi, yend = y_hi),
    linewidth = 1.2, alpha = 0.6, lineend = "round"
  ) +
  
  # Baseline point at origin (Δ=0)
  geom_point(aes(0, 0), shape = 21, fill = "white", color = "black", size = 3, stroke = 1) +
  
  coord_equal() +
  labs(
    title = "Baseline tradeoff envelope (relative to baseline point)",
    subtitle = paste0("Envelope shows ", env_probs[1]*100, "–", env_probs[2]*100,
                      "% range of Δ scores across the baseline frontier."),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    legend.position = "none"
  )

print(p_spider_base)

#overlay in future with
#scenario_poly <- make_spider_df(
#  scenario_df %>% mutate(type = "Scenario X"),   # contains dN/dP/dI
#  id_cols = c("type"),
#  metrics = c("dN","dP","dI")
#)

#p_spider_base +
#  geom_polygon(
#    data = scenario_poly,
#    aes(x, y, group = type),
#    fill = NA, color = "dodgerblue3", linewidth = 1
#  )

# ------------------------------------------
# 5) Optional: crop composition for special points
# ------------------------------------------
if (nrow(special_pts) > 0 && all(c("Corn","Soybeans","Sorghum","Wheat") %in% names(special_pts))) {
  special_crop_long <- special_pts %>%
    select(Scenario, type, Corn, Soybeans, Sorghum, Wheat) %>%
    pivot_longer(cols = c(Corn, Soybeans, Sorghum, Wheat),
                 names_to = "Crop", values_to = "Share") %>%
    mutate(Crop = factor(Crop, levels = c("Corn","Soybeans","Wheat","Sorghum")))
  
  p_crop_special <- ggplot(special_crop_long, aes(x = type, y = Share, fill = Crop)) +
    geom_col(width = 0.75) +
    facet_wrap(~Scenario, ncol = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
    scale_fill_crops() +
    labs(title = "Crop composition at knee + corner solutions",
         x = NULL, y = "Share of cultivated area") +
    theme_minimal(base_size = 13) +
    theme(plot.title.position = "plot")
  
 # ggsave(file.path(fig_dir, "crop_comp_knee_and_corners.png"),
  #       p_crop_special, width = 10, height = 8, dpi = 300)
  print(p_crop_special)
}
p_crop_special # THIS IS WRONG !? 04_viz_run was correct!!

#message("DONE. Figures written to: ", fig_dir)


