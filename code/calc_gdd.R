# calc_gdd.R
# Purpose: Calculate county × year Growing Degree Days using pollen
# Inputs: daily tmin/tmax, Date, county ID
# Output: tidy county-year GDD

library(dplyr)
library(lubridate)
library(pollen)

calc_gdd_county_year <- function(df,
                                 crop = "corn",
                                 county_col = "FIPS",
                                 date_col   = "Date",
                                 tmin_col   = "tmin_C",
                                 tmax_col   = "tmax_C",
                                 season = c("annual", "growing_season"),
                                 gs_start = "04-01",
                                 gs_end   = "10-31") {
  
  season <- match.arg(season)
  
  crop_tbl <- tibble::tibble(
    crop      = c("Corn","Sorghum" ,"Soybeans", "Wheat"),
    tbase     = c(10, 10, 10, 0),  # °C
    tbase_max = c(30, 30, 30, 28)   # °C
  )
  
  cp <- crop_tbl %>% dplyr::filter(.data$crop == !!crop)
  if (nrow(cp) != 1) stop("Unknown crop: ", crop)
  
  tbase     <- cp$tbase[[1]]
  tbase_max <- cp$tbase_max[[1]]
  
  req <- c(county_col, date_col, tmin_col, tmax_col)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  dat <- df %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      Year  = lubridate::year(.date),
      tmin  = .data[[tmin_col]],
      tmax  = .data[[tmax_col]]
    )
  
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
        g <- pollen::gdd(
          tmax      = tmax,
          tmin      = tmin,
          tbase     = tbase,
          tbase_max = tbase_max,
          type      = "C"   # pollen output behaves cumulative; take endpoint
        )
        max(g, na.rm = TRUE)
      },
      n_days = sum(!is.na(tmin) & !is.na(tmax)),
      .groups = "drop"
    )
}

