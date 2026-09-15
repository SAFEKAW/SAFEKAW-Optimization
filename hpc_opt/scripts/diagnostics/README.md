# Diagnostics and scientific audits

Scripts here inspect models, historical irrigation, deterministic results, and
optimization outputs. They are not intended to be run sequentially as a
workflow.

The packaged deterministic runner automatically executes:

- `05_check_deterministic_counterfactuals.R`
- `05_check_deterministic_factorial.R`

Deterministic numerical results are stored by scenario under
`hpc_opt/outputs/factorial_runs/`. The current scenario-to-directory mapping is
recorded in `hpc_opt/outputs/deterministic_factorial_grid_32_scenarios.csv`.
`05_check_deterministic_factorial.R` reads those results and writes the main
figures to `hpc_opt/outputs/factorial_runs/figures/`. It can be sourced again to
regenerate the figures without rerunning the scenarios.

`05_summarize_forward_projection.R` provides a focused historical-versus-future
summary and writes its tables to `hpc_opt/outputs/deterministic_summary/`.
Scripts named `05_viz_*` are specialized analyses associated primarily with
optimization outputs and are not required for the deterministic workflow.

Other scripts require the input products suggested by their names and should be
run only for the corresponding audit. The `03_*irrigation*` and
`03_*yield*` scripts document the current historical irrigation and yield-model
investigation; their outputs are written under
`hpc_opt/outputs/historical_irrigation_audit/` or `model_checks/`.
