suppressPackageStartupMessages({
  library(dplyr)
  library(exactextractr)
  library(ggplot2)
  library(here)
  library(readr)
  library(sf)
  library(terra)
  library(tidyr)
})

# Build an optimizer-independent reference for historical groundwater irrigation.
# This script does not change scenario or optimization behavior.

years_long <- 2006:2023
years_recent <- 2019:2023
modeled_crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")
target_crops <- c("Corn", "Soybeans")

input_dir <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit", "wimas_allocation"
)
out_dir <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit",
  "groundwater_allocation_reference"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

records_path <- file.path(input_dir, "irrigation_records_1990_2023.csv")
if (!file.exists(records_path)) {
  stop("Run 03_build_wimas_irrigation_allocation_history.R first.")
}

records <- read_csv(records_path, show_col_types = FALSE) %>%
  mutate(FIPS = as.character(FIPS)) %>%
  filter(Year %in% years_long, Source == "Groundwater")

# A diversion-year has one source in the cleaned WIMAS data, so its reported
# irrigated acreage can be assigned to groundwater without source apportionment.
gw_annual <- records %>%
  group_by(Year, FIPS, County, Crop, Domain) %>%
  summarise(
    groundwater_irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    groundwater_withdrawal_m3 = sum(waterUse_m3, na.rm = TRUE),
    n_pdiv = n_distinct(pdiv_id),
    .groups = "drop"
  )

summarize_period <- function(dat, years, period) {
  keys <- dat %>% distinct(FIPS, County, Crop, Domain)
  crossing(Year = years, keys) %>%
    left_join(dat, by = c("Year", "FIPS", "County", "Crop", "Domain")) %>%
    mutate(
      groundwater_irrigated_area_ha = coalesce(groundwater_irrigated_area_ha, 0),
      groundwater_withdrawal_m3 = coalesce(groundwater_withdrawal_m3, 0),
      n_pdiv = coalesce(n_pdiv, 0L)
    ) %>%
    group_by(FIPS, County, Crop, Domain) %>%
    summarise(
      period = period,
      n_years = length(years),
      years_active = sum(groundwater_irrigated_area_ha > 0),
      baseline_area_ha = mean(groundwater_irrigated_area_ha),
      baseline_withdrawal_m3 = mean(groundwater_withdrawal_m3),
      baseline_depth_m = if_else(
        sum(groundwater_irrigated_area_ha) > 0,
        sum(groundwater_withdrawal_m3) /
          (sum(groundwater_irrigated_area_ha) * 10000),
        NA_real_
      ),
      .groups = "drop"
    )
}

baseline_detail <- bind_rows(
  summarize_period(gw_annual, years_long, "historical_2006_2023"),
  summarize_period(gw_annual, years_recent, "recent_2019_2023")
) %>%
  group_by(period) %>%
  mutate(basin_area_share = baseline_area_ha / sum(baseline_area_ha)) %>%
  group_by(period, Crop) %>%
  mutate(crop_area_share = baseline_area_ha / sum(baseline_area_ha)) %>%
  group_by(period, Crop, Domain) %>%
  mutate(crop_domain_county_share = baseline_area_ha / sum(baseline_area_ha)) %>%
  ungroup()

baseline_crop <- baseline_detail %>%
  group_by(period, Crop) %>%
  summarise(
    baseline_area_ha = sum(baseline_area_ha),
    baseline_withdrawal_m3 = sum(baseline_withdrawal_m3),
    baseline_depth_m = baseline_withdrawal_m3 / (baseline_area_ha * 10000),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    groundwater_crop_area_mix = baseline_area_ha / sum(baseline_area_ha),
    groundwater_crop_withdrawal_mix =
      baseline_withdrawal_m3 / sum(baseline_withdrawal_m3)
  ) %>%
  ungroup()

baseline_totals <- baseline_detail %>%
  group_by(period, Domain) %>%
  summarise(
    baseline_area_ha = sum(baseline_area_ha),
    baseline_withdrawal_m3 = sum(baseline_withdrawal_m3),
    baseline_depth_m = baseline_withdrawal_m3 / (baseline_area_ha * 10000),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(domain_area_share = baseline_area_ha / sum(baseline_area_ha)) %>%
  ungroup()

# Derive a current county capacity surface from the repository's 2024 CDL.
# Eligible expansion land follows the previously documented basin rule:
# cultivated crops + grass/pasture/shrubland + fallow/barren. Developed land,
# water, wetlands, and forest are excluded.
cdl_path <- here("data", "raw", "figure1", "CDL_EKSRB_2024_90m.tif")
if (!file.exists(cdl_path)) stop("Missing 2024 CDL raster: ", cdl_path)

alluvial <- st_read(
  here("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE
)
counties <- st_read(
  here("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE
) %>%
  select(FIPS, County = name)

capacity_polygons <- st_intersection(
  st_make_valid(counties), st_make_valid(st_union(alluvial))
) %>%
  select(FIPS, County)

cdl <- rast(cdl_path)
capacity_polygons <- st_transform(capacity_polygons, crs(cdl))
names(cdl) <- "CDLcode"
# The figure raster is in Web Mercator, where nominal 90 m cells overstate
# ground area at Kansas latitude. Use geodesic cell areas, not x/y resolution.
cell_area <- cellSize(cdl, unit = "ha")
names(cell_area) <- "cell_area_ha"
extracted <- exact_extract(c(cdl, cell_area), capacity_polygons)

classify_cdl <- function(code) {
  case_when(
    code %in% c(1, 12, 13, 251) ~ "Corn",
    code == 4 ~ "Sorghum",
    code == 5 ~ "Soybeans",
    code %in% c(23, 24) ~ "Wheat",
    code %in% c(2, 3, 6, 21, 25, 27:58, 67:77, 205:224, 229, 242, 247, 251) ~
      "Other Crops",
    code %in% c(59, 60, 152, 176) ~ "Grass/Pasture/Shrubland",
    code %in% c(26, 225:228, 235:241, 254) ~ "Double Crop",
    code %in% c(61, 131) ~ "Fallow/Barren",
    code %in% c(87, 190, 195) ~ "Wetlands",
    code %in% c(63, 141:143) ~ "Forest",
    code %in% c(121:124) ~ "Developed",
    code %in% c(83, 111) ~ "Water",
    TRUE ~ "Unclassified"
  )
}

capacity_landcover <- bind_rows(lapply(seq_along(extracted), function(i) {
  extracted[[i]] %>%
    transmute(
      FIPS = as.character(capacity_polygons$FIPS[[i]]),
      County = capacity_polygons$County[[i]],
      CDLcode = as.integer(CDLcode),
      area_ha = coverage_fraction * cell_area_ha
    )
})) %>%
  filter(!is.na(CDLcode), CDLcode != 0) %>%
  mutate(LandCover = classify_cdl(CDLcode)) %>%
  group_by(FIPS, County, LandCover) %>%
  summarise(area_ha = sum(area_ha), .groups = "drop")

authoritative_corridor_2024 <- read_csv(
  here("data", "LandCoverData-CDL_AlluvialCorridor.csv"),
  show_col_types = FALSE
) %>%
  filter(Year == 2024) %>%
  select(LandCover, authoritative_area_ha = area_ha)

capacity_reconciliation <- capacity_landcover %>%
  group_by(LandCover) %>%
  summarise(extracted_county_area_ha = sum(area_ha), .groups = "drop") %>%
  full_join(authoritative_corridor_2024, by = "LandCover") %>%
  mutate(
    difference_ha = extracted_county_area_ha - authoritative_area_ha,
    relative_difference = difference_ha / authoritative_area_ha
  )

eligible_classes <- c(
  modeled_crops, "Other Crops", "Double Crop",
  "Grass/Pasture/Shrubland", "Fallow/Barren"
)

county_capacity <- capacity_landcover %>%
  group_by(FIPS, County) %>%
  summarise(
    alluvial_area_ha = sum(area_ha),
    current_cultivated_area_ha = sum(area_ha[LandCover %in%
      c(modeled_crops, "Other Crops", "Double Crop")]),
    expansion_eligible_area_ha = sum(area_ha[LandCover %in% eligible_classes]),
    .groups = "drop"
  ) %>%
  left_join(
    baseline_detail %>%
      filter(period == "historical_2006_2023", Domain == "Alluvial") %>%
      group_by(FIPS, County) %>%
      summarise(
        baseline_alluvial_groundwater_area_ha = sum(baseline_area_ha),
        .groups = "drop"
      ),
    by = c("FIPS", "County")
  ) %>%
  mutate(
    baseline_alluvial_groundwater_area_ha =
      coalesce(baseline_alluvial_groundwater_area_ha, 0),
    expansion_headroom_ha = pmax(
      0, expansion_eligible_area_ha - baseline_alluvial_groundwater_area_ha
    ),
    expansion_allocation_share = expansion_headroom_ha / sum(expansion_headroom_ha)
  )

# Machine-readable rules. The reference period is explicit rather than hidden
# in the evaluator so alternative baselines can later be tested deliberately.
allocation_rules <- tibble(
  rule_order = 1:7,
  rule = c(
    "reference_period",
    "contraction",
    "historical_baseline",
    "expansion_domain",
    "expansion_county_weight",
    "shared_land_capacity",
    "water_objective"
  ),
  definition = c(
    "Use the 2006-2023 mean; retain 2019-2023 as a sensitivity reference.",
    "Below baseline, scale every observed county-crop-domain groundwater area proportionally.",
    "At baseline, reproduce observed mean county-crop-domain groundwater areas.",
    "Above baseline, retain the baseline footprint and place added groundwater irrigation only in the alluvial corridor.",
    "Allocate added area among counties in proportion to remaining eligible alluvial land, subject to each county's headroom.",
    "Corn and soybean expansion draw from one shared county headroom; hectares cannot be claimed twice.",
    "Only groundwater withdrawal enters the water-use objective; total applied irrigation remains available to yield and nitrate models."
  )
)

groundwater_reference <- list(
  metadata = list(
    baseline_period = "historical_2006_2023",
    sensitivity_period = "recent_2019_2023",
    area_units = "ha",
    withdrawal_units = "m3/year",
    capacity_landcover_year = 2024L,
    optimizer_behavior_changed = FALSE
  ),
  annual = gw_annual,
  baseline_detail = baseline_detail,
  baseline_crop = baseline_crop,
  baseline_totals = baseline_totals,
  county_capacity = county_capacity,
  capacity_landcover = capacity_landcover,
  allocation_rules = allocation_rules
)

source(here("hpc_opt", "R", "19_groundwater_allocation.R"))
modeled_baseline_targets <- baseline_crop %>%
  filter(period == "historical_2006_2023", Crop %in% modeled_crops) %>%
  select(Crop, baseline_area_ha) %>%
  tibble::deframe()
modeled_baseline_targets <- modeled_baseline_targets[modeled_crops]

allocation_examples <- bind_rows(
  allocate_groundwater_area(
    modeled_baseline_targets * 0.5, groundwater_reference
  ) %>% mutate(example = "50_percent_of_baseline"),
  allocate_groundwater_area(
    modeled_baseline_targets, groundwater_reference
  ) %>% mutate(example = "historical_baseline"),
  allocate_groundwater_area(
    modeled_baseline_targets * 1.5, groundwater_reference
  ) %>% mutate(example = "150_percent_of_baseline")
)

write_csv(gw_annual, file.path(out_dir, "groundwater_irrigation_annual.csv"))
write_csv(baseline_detail, file.path(out_dir, "groundwater_baseline_county_crop_domain.csv"))
write_csv(baseline_crop, file.path(out_dir, "groundwater_baseline_by_crop.csv"))
write_csv(baseline_totals, file.path(out_dir, "groundwater_baseline_by_domain.csv"))
write_csv(county_capacity, file.path(out_dir, "county_alluvial_expansion_capacity.csv"))
write_csv(capacity_landcover, file.path(out_dir, "county_alluvial_2024_landcover.csv"))
write_csv(capacity_reconciliation, file.path(out_dir, "county_capacity_reconciliation.csv"))
write_csv(allocation_rules, file.path(out_dir, "groundwater_allocation_rules.csv"))
write_csv(allocation_examples, file.path(out_dir, "groundwater_allocation_examples.csv"))
saveRDS(groundwater_reference, file.path(out_dir, "groundwater_allocation_reference.rds"))

p_capacity <- county_capacity %>%
  arrange(expansion_eligible_area_ha) %>%
  mutate(County = factor(County, levels = County)) %>%
  select(
    County,
    `Historical groundwater footprint` = baseline_alluvial_groundwater_area_ha,
    `Additional eligible headroom` = expansion_headroom_ha
  ) %>%
  pivot_longer(-County, names_to = "Component", values_to = "area_ha") %>%
  ggplot(aes(area_ha, County, fill = Component)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Historical groundwater footprint" = "#0072B2",
    "Additional eligible headroom" = "#D9D9D9"
  )) +
  labs(
    title = "County alluvial groundwater-irrigation baseline and land headroom",
    subtitle = "Headroom uses 2024 CDL eligible land and is shared across crops",
    x = "Area (ha)", y = NULL, fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "county_alluvial_groundwater_capacity.png"),
  p_capacity, width = 10, height = 7, dpi = 220
)

# Internal consistency checks.
stopifnot(abs(sum(county_capacity$expansion_allocation_share) - 1) < 1e-10)
stopifnot(all(county_capacity$expansion_headroom_ha >= 0))
stopifnot(all(baseline_detail$baseline_area_ha >= 0))
eligible_reconciliation <- capacity_reconciliation %>%
  filter(LandCover %in% eligible_classes) %>%
  summarise(
    relative_difference =
      (sum(extracted_county_area_ha) - sum(authoritative_area_ha)) /
      sum(authoritative_area_ha)
  ) %>%
  pull(relative_difference)
stopifnot(abs(eligible_reconciliation) < 0.02)

print(baseline_crop %>% filter(period == "historical_2006_2023"), n = Inf)
print(
  county_capacity %>%
    select(County, baseline_alluvial_groundwater_area_ha,
           expansion_eligible_area_ha, expansion_headroom_ha,
           expansion_allocation_share) %>%
    arrange(desc(expansion_allocation_share)),
  n = Inf
)
message("Wrote groundwater allocation reference to: ", out_dir)
