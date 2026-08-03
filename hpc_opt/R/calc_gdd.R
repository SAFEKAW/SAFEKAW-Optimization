# hpc_opt/R/calc_gdd.R
# Shared crop-season and growing-degree-day definitions.
#
# `Year` in all returned tables is the harvest/yield year. Cross-calendar
# seasons therefore assign October-December weather to the following year.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

crop_gdd_spec <- function() {
  tibble(
    crop = c("Corn", "Soybeans", "Sorghum", "Wheat"),
    gs_start = c("04-01", "04-01", "04-01", "10-01"),
    gs_end = c("10-31", "10-31", "10-31", "06-30"),
    tbase_C = c(10, 10, 10, 0),
    tupper_C = c(30, 30, 37.7777778, Inf),
    method = c(
      "modified_average",
      "modified_average",
      "modified_average",
      "simple_average"
    )
  )
}

get_crop_gdd_spec <- function(crop) {
  spec <- crop_gdd_spec() %>% filter(.data$crop == .env$crop)
  if (nrow(spec) != 1L) stop("Unknown crop: ", crop)
  spec
}

assign_crop_season <- function(df, date_col, gs_start, gs_end) {
  start_md <- as.integer(gsub("-", "", gs_start))
  end_md <- as.integer(gsub("-", "", gs_end))
  crosses_year <- start_md > end_md

  dat <- df %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      .md = month(.date) * 100L + day(.date),
      .calendar_year = year(.date),
      .yield_year = if (crosses_year) {
        if_else(.md >= start_md, .calendar_year + 1L, .calendar_year)
      } else {
        .calendar_year
      },
      .season_start = as.Date(
        paste0(
          if (crosses_year) .yield_year - 1L else .yield_year,
          "-",
          gs_start
        )
      ),
      .season_end = as.Date(paste0(.yield_year, "-", gs_end))
    ) %>%
    filter(.date >= .season_start, .date <= .season_end)

  dat
}

calc_daily_gdd <- function(tmin, tmax, tbase, tupper, method) {
  valid <- is.finite(tmin) & is.finite(tmax)
  out <- rep(NA_real_, length(tmin))
  if (!any(valid)) return(out)

  if (method == "modified_average") {
    tmin_eff <- pmax(tmin[valid], tbase)
    tmax_eff <- pmax(tmax[valid], tbase)
    if (is.finite(tupper)) {
      tmin_eff <- pmin(tmin_eff, tupper)
      tmax_eff <- pmin(tmax_eff, tupper)
    }
    out[valid] <- pmax(0, (tmin_eff + tmax_eff) / 2 - tbase)
  } else if (method == "simple_average") {
    tavg <- (tmin[valid] + tmax[valid]) / 2
    if (is.finite(tupper)) tavg <- pmin(tavg, tupper)
    out[valid] <- pmax(0, tavg - tbase)
  } else {
    stop("Unknown GDD method: ", method)
  }
  out
}

calc_gdd_county_year <- function(df,
                                 crop = "Corn",
                                 county_col = "FIPS",
                                 date_col = "Date",
                                 tmin_col = "tmin_C",
                                 tmax_col = "tmax_C",
                                 season = c("annual", "growing_season"),
                                 gs_start = NULL,
                                 gs_end = NULL,
                                 target_years = NULL) {
  season <- match.arg(season)
  spec <- get_crop_gdd_spec(crop)
  if (is.null(gs_start)) gs_start <- spec$gs_start[[1]]
  if (is.null(gs_end)) gs_end <- spec$gs_end[[1]]

  req <- c(county_col, date_col, tmin_col, tmax_col)
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }

  dat <- df %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      tmin = as.numeric(.data[[tmin_col]]),
      tmax = as.numeric(.data[[tmax_col]])
    ) %>%
    filter(!is.na(.date))

  if (season == "growing_season") {
    dat <- assign_crop_season(dat, ".date", gs_start, gs_end)
  } else {
    dat <- dat %>%
      mutate(
        .yield_year = year(.date),
        .season_start = as.Date(paste0(.yield_year, "-01-01")),
        .season_end = as.Date(paste0(.yield_year, "-12-31"))
      )
  }

  if (!is.null(target_years)) {
    dat <- dat %>% filter(.yield_year %in% target_years)
  }

  dat %>%
    mutate(
      .daily_gdd = calc_daily_gdd(
        tmin,
        tmax,
        spec$tbase_C[[1]],
        spec$tupper_C[[1]],
        spec$method[[1]]
      )
    ) %>%
    group_by(across(all_of(county_col)), Year = .yield_year) %>%
    summarise(
      crop = crop,
      GDD = if (all(is.na(.daily_gdd))) NA_real_ else sum(.daily_gdd, na.rm = TRUE),
      n_days = sum(is.finite(tmin) & is.finite(tmax)),
      season_start = min(.season_start, na.rm = TRUE),
      season_end = max(.season_end, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_crop_climate_gs <- function(df,
                                 crop,
                                 county_col = "FIPS",
                                 date_col = "Date",
                                 tmin_col = "tmin_C",
                                 tmax_col = "tmax_C",
                                 precip_col = "precip_mm",
                                 gs_start = NULL,
                                 gs_end = NULL,
                                 target_years = NULL) {
  spec <- get_crop_gdd_spec(crop)
  if (is.null(gs_start)) gs_start <- spec$gs_start[[1]]
  if (is.null(gs_end)) gs_end <- spec$gs_end[[1]]

  seasonal <- assign_crop_season(df, date_col, gs_start, gs_end)
  if (!is.null(target_years)) {
    seasonal <- seasonal %>% filter(.yield_year %in% target_years)
  }

  gdd <- calc_gdd_county_year(
    df = df,
    crop = crop,
    county_col = county_col,
    date_col = date_col,
    tmin_col = tmin_col,
    tmax_col = tmax_col,
    season = "growing_season",
    gs_start = gs_start,
    gs_end = gs_end,
    target_years = target_years
  )

  precip <- seasonal %>%
    group_by(across(all_of(county_col)), Year = .yield_year) %>%
    summarise(
      crop = crop,
      precip_gs_mm = if (all(is.na(.data[[precip_col]]))) {
        NA_real_
      } else {
        sum(.data[[precip_col]], na.rm = TRUE)
      },
      ppt_n_days = sum(is.finite(.data[[precip_col]])),
      .groups = "drop"
    )

  gdd %>%
    left_join(precip, by = c(county_col, "Year", "crop"))
}

validate_crop_climate_completeness <- function(df, target_years) {
  expected <- tidyr::crossing(
    FIPS = unique(as.character(df$FIPS)),
    Year = as.integer(target_years),
    crop = crop_gdd_spec()$crop
  )
  missing <- anti_join(
    expected,
    df %>% transmute(FIPS = as.character(FIPS), Year, crop),
    by = c("FIPS", "Year", "crop")
  )
  if (nrow(missing) > 0) {
    stop(
      "Crop-climate output is missing ", nrow(missing),
      " county × yield-year × crop combinations. Ensure the daily input ",
      "starts by October 1 of the year preceding the first target year."
    )
  }
  invisible(TRUE)
}
