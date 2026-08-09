# SAFEKAW Optimization Pipeline

This document describes the optimization workflow for the SAFEKAW project.

## Overview

The optimization workflow builds on the deterministic integration framework but replaces fixed crop allocations with a multi-objective optimization routine.

The workflow proceeds in six stages:

	1. Generate climate and common input datasets 
	2. Fit and save component models 
	3. Build scenario-specific precompute objects 
		- see SAFEKAW Deterministic Pipeline for details on how to run steps 1-3 if needed
	4. Run unconstrained crop-allocation optimization
		- The unconstrained optimization results serve as the theoretical benchmark against which all 			constrained and management-aware optimization scenarios are compared.
	5. Summarize and compare Pareto frontiers
	6. Add constraints and management decision variables/ Run constrained crop-management optimization
	n. Summarize and compare Pareto frontiers between unconstrained & constrained

The optimization workflow currently evaluates tradeoffs among:
	- Basin nitrate export (minimize)
	- Agricultural profit (maximize)
	- Irrigation water use (minimize)
using the NSGA-II multi-objective evolutionary algorithm (in R; mco::nsga2()).

All workflows use the same management definitions. Irrigation technology,
irrigated extent, and fertilizer configurations live under
`hpc_opt/config/irrigation_technology/`,
`hpc_opt/config/irrigation_extent/`, and `hpc_opt/config/fertilizer/`.
The unconstrained benchmark fixes all three at current/baseline conditions.
The constrained workflow selects current or efficient irrigation technology as
a fixed scenario and records whether fertilizer and irrigated extent are fixed
or optimized.

Current decision variables:
	- Corn fraction
	- Soybean fraction
	- Sorghum fraction
	- Wheat fraction is calculated as: Wheat = 1 - Corn - Soybeans - Sorghum

Current optimization runs represent a theoretical unconstrained frontier in which crop shares can vary freely.
Management variables (fertilizer, irrigation, efficiency) are currently fixed at baseline conditions and will be added in future optimization phases.


## The HPC optimization workflow can be tested with:

Opt. 1. Command line:
- Open: Windows Key → cmd
- Navigate to your project: cd C:\Users\yourname\Documents\project_folder
- Then: Rscript hpc_opt\scripts\04_run_optimization.R --gcm ensemble --climate rcp45 --period early --landuse

Opt. 2. R console by 
system('Rscript hpc_opt/scripts/04_run_optimization.R --climate rcp45 --period early --landuse fixed --seed 1')

## Unconstrained HPC staging workflow

Generate and validate the 12-context, one-seed smoke grid:

```bash
Rscript hpc_opt/scripts/06_make_optimization_grid.R \
  --seeds 101 --popsize 8 --generations 3 \
  --run-tag unconstrained_smoke \
  --output hpc_opt/outputs/optimization_grid_unconstrained_smoke.csv
Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid hpc_opt/outputs/optimization_grid_unconstrained_smoke.csv
sbatch --array=1-12%3 \
  --export=ALL,GRID_FILE=hpc_opt/outputs/optimization_grid_unconstrained_smoke.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

Generate, validate, and submit the representative three-budget benchmark:

```bash
Rscript hpc_opt/scripts/06_make_benchmark_grid.R
Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid hpc_opt/outputs/optimization_grid_unconstrained_benchmark.csv
sbatch --array=1-3%1 \
  --export=ALL,GRID_FILE=hpc_opt/outputs/optimization_grid_unconstrained_benchmark.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm
Rscript hpc_opt/scripts/06_summarize_optimization_benchmark.R
```

The benchmark summary reports runtime, objective ranges, and dominated
hypervolume after normalizing all benchmark fronts to common objective ranges.
Select production settings only after checking that additional computation
produces a sufficiently small hypervolume/front change. Then generate the
20-seed grid with those settings and submit `--array=1-240` with an appropriate
concurrency throttle. Array workers validate existing outputs before skipping
them and write one status manifest per job under
`hpc_opt/outputs/optimization_manifests/<run_tag>/`.


## Detailed description of scripts 
See SAFEKAW Deterministic Pipeline for details on how to run steps 1-3 if needed: 
	00_make_climate_inputs_gridmet.R for historical period
   	00_make_climate_inputs_maca.R for future scenarios
	01_build_historical_baseline_inputs.R
	01_fit_and_save_models.R

	03_precompute_inputs.R

4. Unconstrained Optimization 
- Scripts:
	- 04_run_optimization.R 
	- 04_run_optimization_batch.R

- UPDATE TO INCLUDE HPC FILES ONCE VALIDATED!!
	- 06_run_optimization_grid.R
	- 06_run_optimization_arrary_worker.R

- Management Assumptions
	- Current optimization uses fixed management assumptions:
		- cult_area_factor = 1
		- irrig_frac_factor = 1
		- irr_eff = 1
		- fert_factor = 1
	- Thus, optimization currently evaluates crop-allocation tradeoffs only.
- Outputs:
	- Location: hpc_opt/outputs/runs/<scenario_name>/
	- Files: nsga2_res_<scenario>_seed<seed>.rds & pareto_front_<scenario>_seed<seed>.csv

5. Summarize and compare Pareto frontiers
- Scripts:
	- (main): 05_viz_runs_crop_LU_comp.R
	- List others
- Outputs:
	- hpc_opt/outputs/optimization_summaries/

6. Constrained optimization:
- Scripts:
- Outputs:

Default management bounds in `04_run_optimization_constrained.R` are:

- fertilizer application factor uses the shared efficient and current scenario
  definitions (currently 0.70 to 1.00, or a 0 to 30% reduction);
- irrigation technology is a fixed scenario selected with `--irrigation`:
  - `current`: `irr_eff = 1.0` (no withdrawal savings);
  - `efficient`: `irr_eff = 1.176470588` (15% withdrawal savings);
- irrigation extent is held at baseline until the
  `crop_fert_irrigfrac` phase.
- irrigation extent in the final phase uses the shared baseline and expanded
  definitions (currently 1.00 to 1.15 times the historical baseline fraction);
- estimated alluvial target-crop irrigation cannot exceed expansion-eligible
  alluvial area after reserving historical non-target irrigation.

Fertilizer bounds can be overridden with `--fert-factor-min` and
`--fert-factor-max`. Fertilizer reduction has the recurring
nutrient-management cost described below. Irrigation technology is not an
optimization decision and has no separate adoption charge. The efficient
scenario holds yield and irrigated extent constant while reducing withdrawals
and their associated pumping cost by 15%.
Irrigation-extent bounds can be overridden with `--irrig-frac-min` and
`--irrig-frac-max`.

Run both irrigation technology scenarios with otherwise identical settings:

```bash
Rscript hpc_opt/scripts/04_run_optimization_constrained.R \
  --climate rcp45 --period early --landuse fixed \
  --phase crop_fert --irrigation current --seed 1
Rscript hpc_opt/scripts/04_run_optimization_constrained.R \
  --climate rcp45 --period early --landuse fixed \
  --phase crop_fert --irrigation efficient --seed 1
```

The constrained batch runner automatically crosses both irrigation scenarios
with climate, period, and land-use contexts. Scenario and output names include
`current` or `efficient` to prevent paired runs from overwriting one another.

For HPC execution, use the same resumable array worker as the unconstrained
workflow. A one-seed smoke grid contains 72 jobs: 12 climate/period/land-use
contexts, two irrigation technologies, and three constraint phases.

```bash
Rscript hpc_opt/scripts/06_make_constrained_optimization_grid.R \
  --seeds 1 --popsize 8 --generations 3 \
  --run-tag-prefix constrained_smoke \
  --output hpc_opt/outputs/optimization_grid_constrained_smoke.csv
Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid hpc_opt/outputs/optimization_grid_constrained_smoke.csv
sbatch --array=1-72%6 \
  --export=ALL,GRID_FILE=hpc_opt/outputs/optimization_grid_constrained_smoke.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

After the array finishes, audit manifests and output presence. The command
returns a nonzero status if any job is missing, failed, or incomplete:

```bash
Rscript hpc_opt/scripts/06_audit_optimization_grid.R \
  --grid hpc_opt/outputs/optimization_grid_constrained_smoke.csv
```

The same generator can then create a production grid with the selected
population, generation, seed, and array-concurrency settings. Completed output
files are validated and skipped automatically when an array is resubmitted.

### Nutrient-management cost

Fertilizer reduction carries a recurring annual nutrient-management cost that
is linear in the selected reduction:

```text
annual cost = treated acres * $34.30/acre * (fertilizer reduction / 30%)
```

The full $34.30/acre cost is applied at the maximum modeled reduction of 30%.
The cost is applied to all four-crop cultivated acreage each modeled year and
is subtracted from agricultural profit in addition to the existing fertilizer
purchase cost. Yield is held constant; the implementation cost is interpreted
as bundling the management burden and any yield-loss cost associated with the
efficiency assumption. The $34.30 value is reported in the USDA NRCS/ERS
Practice 590 nutrient-management assessment (2022 dollars).

## Irrigation domains and denominators

`03_precompute_inputs.R` builds a shared `irrigation_reference` from the
whole-basin (EKSRB) and alluvial-corridor water-use and CDL files. The model
uses the whole-basin four-crop irrigated area divided by model cultivated area
as `baseline_irrig_frac`, while retaining crop-specific historical irrigation
fractions and both domains for diagnostics.

Both `04_run_optimization.R` and `04_run_optimization_constrained.R` write:

- total modeled-crop irrigated area and fraction for the basin;
- estimated alluvial modeled-crop irrigated area and fraction;
- `irrigation_area_by_domain_crop_<scenario>_seed<seed>.csv`, with the same
  accounting by crop.

The alluvial candidate values are estimates based on each crop's historical
alluvial share of whole-basin irrigated area. They are not a spatial allocation
of future crop cells. The expansion-eligible alluvial area is therefore a
physical screening ceiling, not by itself a realistic adoption constraint.





