# SAFEKAW Deterministic Pipeline

This document describes two supported routes through the deterministic SAFEKAW workflow: running the deterministic scenarios from derived inputs packaged in the repository, and regenerating the full set of derived inputs from raw climate data.

## Choose a workflow

### Run deterministic scenarios from packaged derived inputs

This is the recommended route for collaborators who want to reproduce the 32 deterministic scenarios. A fresh clone contains the required ensemble county inputs, historical references, fitted models, and precompute bundles. No climate archive or climate extraction is required.

From an R session at the repository root, run:

```r
source(here::here("hpc_opt", "scripts", "pipeline", "run_deterministic_packaged.R"))
```

The runner performs a preflight check, runs all 32 factorial scenarios, and then runs the paired-counterfactual and factorial summary checks. It does not rebuild upstream inputs or models.

To check the packaged inputs and R dependencies without running the scenarios, set `SAFEKAW_PREFLIGHT_ONLY=true` before sourcing the runner.

The packaged future inputs are:

- `hpc_opt/outputs/common_inputs_county_ensemble_<scenario>.csv`
- `hpc_opt/outputs/precompute/precomp_ensemble_<scenario>.rds`

for the six RCP/period combinations. The precompute objects are the packaged derived climate inputs used directly by the deterministic evaluator; they are generated artifacts, not raw climate observations.

### Regenerate everything from raw climate data

Use this route only when changing climate sources, climate-processing logic, common-input construction, model fitting, or historical integration. It is a computational preprocessing workflow and requires access to the raw GridMET/MACA data or permission to download them.

1. Generate the historical climate products with `00_make_climate_inputs_gridmet.R`.
2. Generate all 30 GCM × scenario MACA products with `00_run_maca_climate_array_worker.R` or the corresponding HPC array jobs.
3. Generate the six ensemble climate products with `00_make_climate_ensemble.R`.
4. Run `scripts/pipeline/run_full_pipeline.R` to rebuild common inputs, models, historical integration, precompute objects, deterministic scenarios, and checks.

The large `SAFEKAW data.zip` archive is a raw/emergency backup and is not required for the packaged deterministic route. Do not extract it over a working repository.

## Workflow at a glance

1. Build historical and future climate inputs.
2. Build county- and basin-scale common inputs.
3. Fit and save the component models.
4. Integrate the historical baseline.
5. Build historical and future precompute bundles.
6. Run the deterministic factorial scenarios.
7. Summarize and quality-check the scenario outputs.

Climate extraction is an expensive preprocessing step and is not run by `hpc_opt/scripts/pipeline/run_full_pipeline.R`. Common-input construction is part of that full rebuild runner.

## Historical baseline common inputs

The canonical entry point that creates the complete historical baseline common-input set is:

`hpc_opt/scripts/01_build_historical_baseline_inputs.R`

That entry point configures and calls `hpc_opt/scripts/01_common_inputs_make.R`, which is the shared implementation for historical and future common inputs.

With `scenario_tag <- "hist_baseline"` (the default), it writes:

- `hpc_opt/outputs/common_inputs_county_hist_baseline.csv`
- `hpc_opt/outputs/common_inputs_basin_hist_baseline.csv`
- `hpc_opt/outputs/county_areas_hist_baseline.csv`

Run it from the repository root with:

```r
source(here::here("hpc_opt", "scripts", "01_build_historical_baseline_inputs.R"))
```

For the historical baseline it reads:

- `data/ClimateData_County.csv`
- `data/crop_climate_gs_hist_baseline.csv`
- `data/LandCoverData-CDL_County.csv`
- `data/WaterUseData_County.csv`
- `data/WaterUseByCrop_AlluvialCorridor.csv`

It combines county climate, land cover, crop-specific irrigation, fertilizer assumptions, and growing-season climate. It then aggregates the basin table, including basin climate, cultivated/developed land cover, fertilizer use, and alluvial-corridor irrigation volume and irrigated area.

### Relationship between the two scripts

`01_build_historical_baseline_inputs.R` is the user-facing historical entry point. It always sets `scenario_tag <- "hist_baseline"` in an isolated environment and calls `01_common_inputs_make.R`.

`01_common_inputs_make.R` contains the actual shared construction logic. It reads climate, land cover, county irrigation, alluvial-corridor water use, and crop growing-season climate; it then writes the county, basin, and county-area outputs. Future scenarios call this engine directly with a scenario and GCM/ensemble name.

These common-input files feed the shared precompute stage, so the same historical baseline anchors both the deterministic and optimization evaluators.

## 0. Build climate inputs

These climate products are generally regenerated only when the underlying climate data or climate-processing logic changes.

### Historical GridMET climate

`hpc_opt/scripts/00_make_climate_inputs_gridmet.R` generates historical daily county climate and crop growing-season summaries. The current full-run output names are:

- `data/ClimateData_County.csv`
- `data/crop_climate_gs_hist_baseline.csv`
- `data/gdd_all_gs.csv` (legacy GDD-only output)

The crop growing-season file supplies `GDD` and `precip_gs_mm`. The daily climate file is summarized into annual precipitation and temperature fields by `01_common_inputs_make.R`.

### Future MACA climate and ensemble

- `hpc_opt/scripts/00_make_climate_inputs_maca.R` creates county climate and crop growing-season climate for each GCM and RCP/time-period combination.
- `hpc_opt/scripts/00_make_climate_ensemble.R` averages the individual GCM products into ensemble files for the six future scenario tags: `rcp45_early`, `rcp45_mid`, `rcp45_late`, `rcp85_early`, `rcp85_mid`, and `rcp85_late`.
- The individual-GCM files are intermediate products when the deterministic run uses `gcm_name <- "ensemble"`.

HPC array-worker and submission scripts for the climate stage are also available in `hpc_opt/scripts/00_run_maca_climate_array_worker.R` and the `00_submit_*climate*.slurm` files.

## 1. Build common inputs

`hpc_opt/scripts/01_common_inputs_make.R` builds county, basin, and county-area files for one scenario at a time.

Historical baseline (preferred entry point):

```r
source(here::here("hpc_opt", "scripts", "01_build_historical_baseline_inputs.R"))
```

Future ensemble scenarios:

```r
future_scenarios <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

for (sc in future_scenarios) {
  scenario_tag <- sc
  gcm_name <- "ensemble"
  source(here::here("hpc_opt", "scripts", "01_common_inputs_make.R"))
}
```

Future outputs follow the pattern:

- `hpc_opt/outputs/common_inputs_county_<gcm>_<scenario>.csv`
- `hpc_opt/outputs/common_inputs_basin_<gcm>_<scenario>.csv`
- `hpc_opt/outputs/county_areas_<gcm>_<scenario>.csv`

For future periods, historical mean land cover and water-use inputs are repeated across the relevant years; future climate varies by GCM/ensemble and scenario. Scenario-specific management changes are applied later by the policy/evaluation code.

## 2. Fit and save component models

`hpc_opt/scripts/01_fit_and_save_models.R` fits the irrigation, crop-yield, and water-quality models.

Primary model outputs:

- `hpc_opt/models/irr_lm.rds`
- `hpc_opt/models/yield_kg_<Crop>.rds`
- `hpc_opt/models/yield_kcal_<Crop>.rds`
- `hpc_opt/models/wq_lm.rds`

Diagnostics, metrics, coefficients, and validation figures are written under `hpc_opt/outputs/model_checks/`.

Optional yield-model checking scripts include:

- `hpc_opt/scripts/diagnostics/01b_compare_yield_models.R`
- `hpc_opt/scripts/diagnostics/01c_validate_yield_models.R`

## 3. Integrate the historical baseline

`hpc_opt/scripts/02_integrate_historical_run.R` reads both historical common-input files, observed crop yield and nitrate data, and the saved models. It writes:

- `hpc_opt/outputs/integration/int_crop_areanorm_annual.csv`
- `hpc_opt/outputs/integration/int_basin_annual.csv`
- `hpc_opt/outputs/integration/irr_frac_annual.csv`

The historical integration outputs provide the historical anchors used by precomputation and deterministic result normalization.

Corn and soybean crop areas are split into irrigated and non-irrigated management rows using observed basin crop-year irrigated acreage. This preserves total crop area while allowing the matching irrigated and non-irrigated yield observations to be used. See `hpc_opt/HISTORICAL_IRRIGATION_AUDIT.md` for the rationale and measured effect of this correction.

## 4. Build precompute bundles

`hpc_opt/scripts/03_precompute_inputs.R` creates scenario-specific objects used by the deterministic evaluator. The historical precompute must be built before future precomputes because future runs reuse its irrigation and land-use references.

```r
# Historical first
scenario_tag <- "hist_baseline"
if (exists("gcm_name")) rm(gcm_name)
source(here::here("hpc_opt", "scripts", "03_precompute_inputs.R"))

# Then future ensemble periods
future_scenarios <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

for (sc in future_scenarios) {
  scenario_tag <- sc
  gcm_name <- "ensemble"
  source(here::here("hpc_opt", "scripts", "03_precompute_inputs.R"))
}
```

Primary outputs:

- `hpc_opt/outputs/precompute/precomp_hist_baseline.rds`
- `hpc_opt/outputs/precompute/precomp_<gcm>_<scenario>.rds`
- `hpc_opt/outputs/precompute/baseline_reference.rds`
- irrigation reference CSVs by domain, crop, and year

## 5. Run deterministic factorial scenarios

`hpc_opt/scripts/04_run_factorial_all.R` loops over the enabled configuration files and calls `04_run_deterministic_factorial.R` for each combination.

The deterministic factorial evaluates 32 complete 2025-2099 trajectories:

- climate source: ensemble
- pathways: RCP 4.5 and RCP 8.5
- land use: fixed and BAU
- irrigation technology: current and efficient (15% withdrawal savings)
- irrigated extent: historical baseline and 15% expanded
- fertilizer: current and efficient

Irrigation technology and irrigated extent are independent configuration axes
under `hpc_opt/config/irrigation_technology/` and
`hpc_opt/config/irrigation_extent/`. This ensures that all four irrigation
combinations are evaluated, including efficient technology on expanded acreage.

Run from an R session at the repository root:

```r
source(here::here("hpc_opt", "scripts", "04_run_factorial_all.R"))
```

The runner first writes the auditable scenario manifest
`hpc_opt/outputs/deterministic_factorial_grid_32_scenarios.csv` and then
evaluates every row.

Each scenario directory under `hpc_opt/outputs/factorial_runs/` contains:

- `scenario_path.csv`
- `deterministic_results.csv`
- `irrigation_by_crop.csv`
- `summary_by_period.csv`
- `summary_overall.csv`

Annual and summary outputs report the three objectives (nitrate export, irrigation withdrawals, and net returns) plus modeled-crop irrigated area and irrigated fractions for the whole basin and estimated alluvial corridor. Alluvial values use historical crop-specific corridor capture rates.

## 6. Check and summarize deterministic runs

`hpc_opt/scripts/diagnostics/05_check_deterministic_factorial.R` reads the 32 run directories declared in the current scenario manifest, compares results with the historical integration baseline, builds normalized summaries, and writes diagnostic figures under:

- `hpc_opt/outputs/factorial_runs/figures/`

Additional targeted checks include:

- `hpc_opt/scripts/diagnostics/05_check_deterministic_counterfactuals.R`, which verifies
  matched current/efficient technology and fertilizer relationships;
- `hpc_opt/scripts/diagnostics/05_check_irrigation_denominators.R`
- `hpc_opt/scripts/diagnostics/06_compare_irrigation_domains.R`

## Full rebuild runner

`hpc_opt/scripts/pipeline/run_full_pipeline.R` builds the historical and future ensemble common inputs, fits models, integrates the historical baseline, builds precompute objects, runs the deterministic factorial scenarios, and performs the checks. It is a rebuild runner, not the collaborator entry point. It assumes the historical GridMET and future ensemble climate files have already been generated from the raw data.

The full rebuild runner and `04_run_factorial_all.R` both use the ensemble climate source. Set the stage flags near the top of `run_full_pipeline.R` to skip outputs that do not need to be regenerated. For a clone containing the packaged derived inputs, use `run_deterministic_packaged.R` instead.
