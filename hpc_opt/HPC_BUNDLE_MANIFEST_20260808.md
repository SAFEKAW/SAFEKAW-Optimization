# HPC deployment bundle — 2026-08-08

This bundle contains the corrected SAFE-KAW optimization runtime and inputs.

Corrections carried through the packaged artifacts:

- county land-cover areas are derived from county polygons clipped to the EKSRB;
- basin climate aggregation uses watershed-overlap areas;
- wheat uses the climate-only production model;
- corn total production cost is 0.16 USD/kg;
- fertilizer–nitrate elasticity remains the rounded empirical value gamma = 0.1;
- future precipitation percentiles use the historical 2006–2023 empirical CDF;
- future percentile changes are calculated continuously from 2025–2099 before
  early-, mid-, and late-century subsetting.

The bundle includes runtime R files, optimization scripts, scenario
configuration, fitted models, historical and six future common-input files,
seven precompute bundles, the four deterministic scenario paths required as
crop-bound references, preflight scripts, and grid-generation scripts.

Regenerate production optimization grids with the final selected population
size, generation count, phases, and seeds before submission. Any bundled grid
whose name contains `preflight` is for validation only.
