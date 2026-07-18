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

- fertilizer application factor: 0.70 to 1.00 (0 to 30% reduction);
- irrigation efficiency: 1.00 to 1.176470588 (0 to 15% withdrawal savings);
- irrigation extent is held at baseline until the
  `crop_fert_irreff_irrigfrac` phase.
- irrigation extent in the final phase is limited to 0.85 to 1.15 times the
  historical baseline fraction;
- estimated alluvial target-crop irrigation cannot exceed expansion-eligible
  alluvial area after reserving historical non-target irrigation.

The bounds can be overridden with `--fert-factor-min`, `--fert-factor-max`,
`--irr-eff-min`, and `--irr-eff-max`. Yield is held constant for both levers.
Fertilizer reduction has the recurring nutrient-management cost described
below; irrigation efficiency currently has no technology/adoption cost.
Irrigation-extent bounds can be overridden with `--irrig-frac-min` and
`--irrig-frac-max`.

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





