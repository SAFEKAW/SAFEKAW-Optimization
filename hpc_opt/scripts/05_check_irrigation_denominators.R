suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(tidyr)
})

years_ref <- 2006:2023
modeled_crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")
alluvial_cult_classes <- c(
  modeled_crops, "Other Crops", "Double Crop"
)
alluvial_expansion_classes <- c(
  alluvial_cult_classes, "Grass/Pasture/Shrubland", "Fallow/Barren"
)

outdir <- here("hpc_opt", "outputs", "irrigation_diagnostics")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

wu <- read_csv(
  here("data", "WaterUseByCrop_AlluvialCorridor.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_ref) %>%
  mutate(
    Crop_group = if_else(Crop %in% modeled_crops, Crop, "Other/Unclassified"),
    irrig_area_ha = pmax(0, as.numeric(irrArea_ha))
  )

alluvial_lu <- read_csv(
  here("data", "LandCoverData-CDL_AlluvialCorridor.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_ref)

basin_lu <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_ref) %>%
  transmute(
    Year,
    basin_area_ha = (
      LandCover_m2_all_cult +
        LandCover_m2_grassPastureHay +
        LandCover_m2_developed
    ) / 1e4,
    basin_cultivated_ha = LandCover_m2_all_cult / 1e4
  )

alluvial_denominators <- alluvial_lu %>%
  group_by(Year) %>%
  summarise(
    alluvial_area_ha = sum(area_ha, na.rm = TRUE),
    alluvial_cultivated_ha = sum(
      area_ha[LandCover %in% alluvial_cult_classes], na.rm = TRUE
    ),
    alluvial_expansion_eligible_ha = sum(
      area_ha[LandCover %in% alluvial_expansion_classes], na.rm = TRUE
    ),
    alluvial_developed_water_ha = sum(
      area_ha[LandCover %in% c("Developed", "Water")], na.rm = TRUE
    ),
    .groups = "drop"
  )

denominators <- basin_lu %>%
  inner_join(alluvial_denominators, by = "Year")

irrigation_by_year <- wu %>%
  group_by(Year) %>%
  summarise(
    irrigated_all_reported_ha = sum(irrig_area_ha, na.rm = TRUE),
    irrigated_modeled_crops_ha = sum(
      irrig_area_ha[Crop %in% modeled_crops], na.rm = TRUE
    ),
    irrigated_corn_soy_ha = sum(
      irrig_area_ha[Crop %in% c("Corn", "Soybeans")], na.rm = TRUE
    ),
    .groups = "drop"
  )

denominator_long <- denominators %>%
  select(
    Year,
    `Total basin area` = basin_area_ha,
    `Basin cultivated area` = basin_cultivated_ha,
    `Total alluvial area` = alluvial_area_ha,
    `Alluvial cultivated area` = alluvial_cultivated_ha
  ) %>%
  pivot_longer(-Year, names_to = "Denominator", values_to = "denominator_ha")

total_comparison <- irrigation_by_year %>%
  select(Year, irrigated_area_ha = irrigated_all_reported_ha) %>%
  inner_join(denominator_long, by = "Year") %>%
  mutate(percent_irrigated = 100 * irrigated_area_ha / denominator_ha)

crop_comparison <- wu %>%
  group_by(Year, Crop_group) %>%
  summarise(irrigated_area_ha = sum(irrig_area_ha, na.rm = TRUE), .groups = "drop") %>%
  inner_join(denominator_long, by = "Year", relationship = "many-to-many") %>%
  mutate(percent_irrigated = 100 * irrigated_area_ha / denominator_ha)

crop_specific_alluvial <- wu %>%
  filter(Crop %in% modeled_crops) %>%
  group_by(Year, Crop) %>%
  summarise(irrigated_area_ha = sum(irrig_area_ha, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    alluvial_lu %>%
      filter(LandCover %in% modeled_crops) %>%
      group_by(Year, Crop = LandCover) %>%
      summarise(crop_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop"),
    by = c("Year", "Crop")
  ) %>%
  mutate(percent_of_same_crop_irrigated = 100 * irrigated_area_ha / crop_area_ha)

cap_comparison <- denominators %>%
  inner_join(irrigation_by_year, by = "Year") %>%
  transmute(
    Year,
    irrigated_all_reported_ha,
    alluvial_expansion_eligible_ha,
    current_fraction_basin_cultivated =
      irrigated_all_reported_ha / basin_cultivated_ha,
    cap_fraction_basin_cultivated =
      alluvial_expansion_eligible_ha / basin_cultivated_ha,
    current_fraction_basin_area = irrigated_all_reported_ha / basin_area_ha,
    cap_fraction_basin_area = alluvial_expansion_eligible_ha / basin_area_ha
  )

write_csv(denominators, file.path(outdir, "irrigation_denominators_by_year.csv"))
write_csv(irrigation_by_year, file.path(outdir, "irrigated_area_by_year.csv"))
write_csv(total_comparison, file.path(outdir, "irrigation_percent_by_denominator.csv"))
write_csv(crop_comparison, file.path(outdir, "irrigation_percent_by_crop_and_denominator.csv"))
write_csv(crop_specific_alluvial, file.path(outdir, "crop_specific_alluvial_irrigation_percent.csv"))
write_csv(cap_comparison, file.path(outdir, "irrigation_extent_cap_comparison.csv"))

denominator_order <- c(
  "Total basin area", "Basin cultivated area",
  "Total alluvial area", "Alluvial cultivated area"
)

p_total <- total_comparison %>%
  mutate(Denominator = factor(Denominator, levels = denominator_order)) %>%
  ggplot(aes(Year, percent_irrigated)) +
  geom_line(linewidth = 0.9, color = "#2C7FB8") +
  geom_point(size = 1.5, color = "#2C7FB8") +
  facet_wrap(~Denominator, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "The same reported irrigated area under four denominators",
    subtitle = "Numerator includes every crop category reported in the alluvial water-use table",
    x = NULL,
    y = "Reported irrigated area / denominator"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

p_crop <- crop_comparison %>%
  mutate(Denominator = factor(Denominator, levels = denominator_order)) %>%
  ggplot(aes(Year, percent_irrigated, color = Crop_group)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Denominator, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "Contribution of each crop group under four denominators",
    subtitle = "Crop-group percentages add to the total percentage within each panel and year",
    x = NULL,
    y = "Crop irrigated area / denominator",
    color = "Crop group"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p_crop_specific <- crop_specific_alluvial %>%
  ggplot(aes(Year, percent_of_same_crop_irrigated, color = Crop)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +
  labs(
    title = "Crop-specific irrigation within the alluvial corridor",
    subtitle = "Each crop uses its own alluvial land-cover area as the denominator",
    x = NULL,
    y = "Reported irrigated area / same-crop alluvial area",
    color = "Crop"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave(file.path(outdir, "irrigation_total_denominator_comparison.png"), p_total,
       width = 10, height = 7, dpi = 300)
ggsave(file.path(outdir, "irrigation_crop_denominator_comparison.png"), p_crop,
       width = 10, height = 7, dpi = 300)
ggsave(file.path(outdir, "irrigation_crop_specific_alluvial.png"), p_crop_specific,
       width = 9, height = 5.5, dpi = 300)

message("Wrote irrigation diagnostics to: ", outdir)
