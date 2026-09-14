# Script organization

Use the scripts in `pipeline/` as workflow entry points. The numbered scripts
that remain in this directory are shared pipeline stages called by those entry
points; collaborators normally should not run every numbered file manually.

- `pipeline/`: supported deterministic entry points and the full derived-input
  rebuild runner.
- `diagnostics/`: optional scientific audits, validation checks, summaries, and
  figures. Two deterministic checks are called automatically by the packaged
  runner.
- `recovery/`: restart utilities that require outputs from an interrupted run.
- `experimental/`: historical or exploratory scripts that are not part of a
  supported workflow and may require obsolete inputs or manual configuration.

The current optimization scripts remain in this directory until the groundwater
allocation reference is connected to the optimizer and its supported entry point
is finalized.
