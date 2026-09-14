# Diagnostics and scientific audits

Scripts here inspect models, historical irrigation, deterministic results, and
optimization outputs. They are not intended to be run sequentially as a
workflow.

The packaged deterministic runner automatically executes:

- `05_check_deterministic_counterfactuals.R`
- `05_check_deterministic_factorial.R`

Other scripts require the input products suggested by their names and should be
run only for the corresponding audit. The `03_*irrigation*` and
`03_*yield*` scripts document the current historical irrigation and yield-model
investigation; their outputs are written under
`hpc_opt/outputs/historical_irrigation_audit/` or `model_checks/`.
