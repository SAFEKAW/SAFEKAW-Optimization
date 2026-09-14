# Supported workflow entry points

## Packaged deterministic workflow

Run `run_deterministic_packaged.R`. This is the supported collaborator route. It
checks dependencies and packaged inputs, runs the 32-scenario factorial, and runs
the required counterfactual and factorial checks.

Set `SAFEKAW_PREFLIGHT_ONLY=true` to validate prerequisites without running
scenarios.

## Full derived-input rebuild

Run `run_full_pipeline.R` only after the required historical and ensemble climate
products exist. It rebuilds common inputs, models, historical integration,
precompute objects, deterministic scenarios, and checks. It does not download or
extract raw climate data.

Climate generation and HPC submission scripts remain as shared stages in the
parent directory because they are specialist preprocessing operations, not part
of the packaged collaborator workflow.
