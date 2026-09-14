# Historical irrigation audit

## Finding

The historical integration runner previously forced every county-crop row to `Non-Irrigated`. Because `predict_irrigation_depth()` honors that management label, historical modeled irrigation was exactly zero.

Using `build_irrig_flags()` directly is not a valid replacement. That helper detects where a positive county irrigation-depth estimate exists; it does not measure irrigated acreage. Across 2006–2023 it would classify, on average, 73.4% of corn area and 61.8% of soybean area as irrigated. Observed basin irrigated-area fractions are 8.25% and 4.41%, respectively.

## Correction

Corn and soybean county crop rows are now split into irrigated and non-irrigated management rows. The annual split is set by observed crop-specific irrigated area in `data/WaterUseByCrop_EKSRB.csv`. Area-dependent fields are scaled by the management share, so the two rows preserve the original county crop area. Sorghum and wheat remain non-irrigated because the observed yield dataset contains no irrigated management series for those crops.

The irrigation-depth model still supplies county-specific depth variation for the irrigated portions. The resulting modeled withdrawal is therefore a model result, not a forced copy of observed withdrawal.

## Effect on historical results

Relative to the preserved all-rainfed integration:

- Mean modeled corn-plus-soy irrigation changes from 0 to 21.47 million m3/year.
- Mean observed corn-plus-soy withdrawal is 21.41 million m3/year.
- Mean integrated yield decreases by 5.9%.
- Mean integrated net return decreases by 5.7%.
- Historical nitrate predictions are unchanged under the current water-quality model.

Annual modeled withdrawal residuals range from approximately -35% to +101% of observed withdrawal. This variation reflects irrigation-depth model error; the irrigated acreage itself is anchored to the observed crop-year values.

Corn and soybean yield-model sample sizes double because both observed management series are now retained. RMSE increases and R2 decreases for those crops, revealing the performance cost that was hidden when only one management series per county-crop row was selected.

## Yield-management specification comparison

The retained irrigated and non-irrigated observations were compared under three mixed-model specifications: one shared water curve, a shared curve plus a management intercept, and management-specific water curves. Each retains the county random intercept and GDD term. Models were evaluated with forward year-block validation and held-out-county validation; observations receive equal weight during model fitting, while acreage weights remain part of downstream spatial integration.

A model that ignores management is not supported. Adding a management intercept improves likelihood strongly for corn (likelihood-ratio p = 7.7e-85) and soybeans (p = 8.6e-35), and materially improves held-out performance. The irrigated and non-irrigated observations also have substantial common support in total water, so this result is not driven solely by extrapolation between disjoint water ranges.

For corn, management-specific curve shapes provide an additional statistical improvement over the management-intercept model (likelihood-ratio p = 2.4e-5; forward validation RMSE 1,770 versus 1,856 kg/ha; held-out-county RMSE 1,687 versus 1,719 kg/ha). However, the fitted irrigated-corn curve decreases throughout the observed irrigated-water range. The raw irrigated observations have essentially no association between assigned total water and yield (r = -0.05), compared with r = 0.39 for rainfed corn. This is not a scientifically defensible dose-response curve and the interaction model must not be selected from fit statistics alone. The assigned irrigation depths are modeled county-crop-year estimates rather than water measurements attached directly to each yield observation, which limits identification of a separate irrigated response.

For soybeans, the two management-aware models are practically tied in validation (approximately 516--520 kg/ha RMSE); BIC favors the simpler management-intercept model even though the likelihood-ratio test is marginally significant (p = 0.044). This comparison rules out unconstrained management-specific curves but does not establish that a categorical management intercept is necessary; the water-source comparison below tests that question directly. A separate irrigated curve should only be used if it is constrained by agronomic assumptions or supported by irrigation amounts directly matched to the yield observations. These diagnostic comparisons do not themselves overwrite the production models.

A subsequent paired analysis showed that the irrigated yield advantage increases with the modeled irrigation increment and has a near-zero intercept: -219 kg/ha for corn and +43 kg/ha for soybeans. The increment explains 29% and 11% of the paired yield-gap variance, respectively. This supports added water as the primary modeled explanation for the average management difference, rather than an unexplained categorical yield bonus.

Because rainfall and irrigation differ in timing and effective availability, an additional comparison replaced their simple sum with separate terms while retaining one model for each crop and no management-specific curve. The parsimonious specification uses a quadratic growing-season precipitation response and a positive linear irrigation-depth response. It outperforms both the simple total-water curve and total water plus a management intercept in forward-year and held-out-county validation. Adding a quadratic irrigation term does not improve validation or information criteria. The fitted irrigation effects are positive: approximately 3,015 kg/ha per 0.1 m for corn and 634 kg/ha per 0.1 m for soybeans. These are associative effects based on modeled irrigation depth, not field-level causal dose-response estimates.

Reproduce the management comparison with `hpc_opt/scripts/03_compare_yield_management_models.R`. Detailed fold metrics, likelihood tests, water-support summaries, predictions, and a curve plot are stored in `hpc_opt/outputs/historical_irrigation_audit/yield_specifications/`. Reproduce the water-source comparison with `hpc_opt/scripts/03_compare_water_source_yield_models.R`; its outputs are stored in `hpc_opt/outputs/historical_irrigation_audit/water_source_specifications/`.

## Downstream references

The historical precompute bundle is unchanged by the correction. The crop-mix baseline reference is also preserved after making crop-share aggregation invariant to the number of management rows. Existing future precompute bundles therefore remain valid; deterministic scenario outputs must be rerun because the saved corn and soybean yield models changed.

The integration continues to omit some cultivated area because it models four crops and retains a legacy minimum crop-area threshold. That is a separate scope issue and is not caused by the irrigation correction.

Machine-readable comparisons are stored in `hpc_opt/outputs/historical_irrigation_audit/`.
