# Allocate crop-specific groundwater-irrigated area to county/domain cells.
# This helper is intentionally independent of the optimization evaluator.
allocate_groundwater_area <- function(target_area_ha,
                                      groundwater_reference,
                                      period = "historical_2006_2023",
                                      tolerance = 1e-6) {
  modeled_crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  if (is.null(names(target_area_ha)) ||
      !all(modeled_crops %in% names(target_area_ha))) {
    stop("target_area_ha must be named for Corn, Soybeans, Sorghum, and Wheat.")
  }
  target_area_ha <- pmax(0, as.numeric(target_area_ha[modeled_crops]))
  names(target_area_ha) <- modeled_crops

  detail <- groundwater_reference$baseline_detail %>%
    dplyr::filter(.data$period == !!period, .data$Crop %in% modeled_crops)
  capacity <- groundwater_reference$county_capacity
  if (nrow(detail) == 0 || nrow(capacity) == 0) {
    stop("Groundwater reference lacks baseline detail or county capacity.")
  }

  crop_baseline <- detail %>%
    dplyr::group_by(.data$Crop) %>%
    dplyr::summarise(baseline_area_ha = sum(.data$baseline_area_ha),
                     .groups = "drop") %>%
    tidyr::complete(Crop = modeled_crops, fill = list(baseline_area_ha = 0)) %>%
    dplyr::mutate(target_area_ha = target_area_ha[.data$Crop])

  # Crops below baseline contract proportionally across their observed cells.
  allocation <- detail %>%
    dplyr::left_join(crop_baseline, by = "Crop") %>%
    dplyr::mutate(
      contraction_factor = dplyr::if_else(
        .data$baseline_area_ha.y > 0,
        pmin(1, .data$target_area_ha / .data$baseline_area_ha.y),
        0
      ),
      allocated_area_ha = .data$baseline_area_ha.x * .data$contraction_factor
    ) %>%
    dplyr::select(FIPS, County, Crop, Domain,
                  baseline_area_ha = baseline_area_ha.x, allocated_area_ha)

  # Contraction releases previously occupied alluvial hectares. Added irrigation
  # may use those hectares plus the unoccupied eligible headroom.
  released <- allocation %>%
    dplyr::filter(.data$Domain == "Alluvial") %>%
    dplyr::group_by(.data$FIPS, .data$County) %>%
    dplyr::summarise(
      released_area_ha = sum(.data$baseline_area_ha - .data$allocated_area_ha),
      .groups = "drop"
    )
  available <- capacity %>%
    dplyr::select(FIPS, County, expansion_headroom_ha) %>%
    dplyr::left_join(released, by = c("FIPS", "County")) %>%
    dplyr::mutate(
      released_area_ha = dplyr::coalesce(.data$released_area_ha, 0),
      available_area_ha = .data$expansion_headroom_ha + .data$released_area_ha
    )

  expansion <- crop_baseline %>%
    dplyr::transmute(
      Crop,
      expansion_area_ha = pmax(0, .data$target_area_ha - .data$baseline_area_ha)
    )
  total_expansion <- sum(expansion$expansion_area_ha)
  total_available <- sum(available$available_area_ha)
  if (total_expansion > total_available + tolerance) {
    stop(
      "Groundwater-irrigated area target exceeds shared alluvial capacity by ",
      round(total_expansion - total_available, 2), " ha."
    )
  }

  if (total_expansion > tolerance) {
    additions <- tidyr::crossing(
      available %>%
        dplyr::mutate(county_share = .data$available_area_ha / total_available) %>%
        dplyr::select(FIPS, County, county_share),
      expansion %>% dplyr::filter(.data$expansion_area_ha > 0)
    ) %>%
      dplyr::transmute(
        FIPS, County, Crop, Domain = "Alluvial",
        baseline_area_ha = 0,
        allocated_area_ha = .data$county_share * .data$expansion_area_ha
      )
    allocation <- dplyr::bind_rows(allocation, additions) %>%
      dplyr::group_by(FIPS, County, Crop, Domain) %>%
      dplyr::summarise(
        baseline_area_ha = sum(.data$baseline_area_ha),
        allocated_area_ha = sum(.data$allocated_area_ha),
        .groups = "drop"
      )
  }

  achieved <- allocation %>%
    dplyr::group_by(.data$Crop) %>%
    dplyr::summarise(area_ha = sum(.data$allocated_area_ha), .groups = "drop") %>%
    tidyr::complete(Crop = modeled_crops, fill = list(area_ha = 0))
  if (any(abs(achieved$area_ha - target_area_ha[achieved$Crop]) > tolerance)) {
    stop("Internal error: groundwater allocation did not reproduce crop targets.")
  }

  attr(allocation, "county_capacity") <- available
  allocation
}
