# calc_gdd.R
# Purpose: Calculate county × year Growing Degree Days using pollen
# Inputs: daily tmin/tmax, Date, county ID
# Output: tidy county-year GDD

# calc_gdd.R
# Purpose: Calculate county × year Growing Degree Days for a specified crop
# Inputs: daily tmin/tmax, Date, county ID
# Output: tidy county-year GDD

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(pollen)
  library(tibble)
})

calc_gdd_county_year <- function(df,
                                 crop = "Corn",
                                 county_col = "FIPS",
                                 date_col   = "Date",
                                 tmin_col   = "tmin_C",
                                 tmax_col   = "tmax_C",
                                 season = c("annual", "growing_season"),
                                 gs_start = "04-01",
                                 gs_end   = "10-31") {
  
  season <- match.arg(season)
  
  crop_tbl <- tibble(
    crop      = c("Corn", "Sorghum", "Soybeans", "Wheat"),
    tbase     = c(10, 10, 10, 0),   # °C
    tbase_max = c(30, 30, 30, 35)   # °C
  )
  
  cp <- crop_tbl %>% filter(.data$crop == crop)
  if (nrow(cp) != 1) stop("Unknown crop: ", crop)
  
  tbase     <- cp$tbase[[1]]
  tbase_max <- cp$tbase_max[[1]]
  
  req <- c(county_col, date_col, tmin_col, tmax_col)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  dat <- df %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      Year  = lubridate::year(.date),
      tmin  = as.numeric(.data[[tmin_col]]),
      tmax  = as.numeric(.data[[tmax_col]])
    ) %>%
    filter(!is.na(.date))
  
  if (season == "growing_season") {
    dat <- dat %>%
      mutate(
        .start = as.Date(paste0(Year, "-", gs_start)),
        .end   = as.Date(paste0(Year, "-", gs_end))
      ) %>%
      filter(.date >= .start, .date <= .end) %>%
      select(-.start, -.end)
  }
  
  dat %>%
    group_by(across(all_of(county_col)), Year) %>%
    arrange(.date, .by_group = TRUE) %>%
    summarise(
      crop = crop,
      GDD = {
        valid <- is.finite(tmin) & is.finite(tmax)
        
        if (!any(valid)) {
          NA_real_
        } else {
          g <- pollen::gdd(
            tmin      = tmin[valid],
            tmax      = tmax[valid],
            tbase     = tbase,
            tbase_max = tbase_max,
            type      = "C"
          )
          max(g, na.rm = TRUE)
        }
      },
      n_days = sum(is.finite(tmin) & is.finite(tmax)),
      .groups = "drop"
    )
}
