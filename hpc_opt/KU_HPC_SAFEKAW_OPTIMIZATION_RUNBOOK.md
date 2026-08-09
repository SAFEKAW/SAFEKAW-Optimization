# Running SAFE-KAW optimization on the KU HPC

## Purpose and current scientific version

This is the operational handoff guide for running the SAFE-KAW optimization
workflow on the KU `water` partition. Commands are written for a Bash shell in
MobaXterm or another SSH client.

Collaborators using a new personal `$WORK` account should first complete
`hpc_opt/KU_HPC_SAFEKAW_ENVIRONMENT_SETUP.md`. That companion guide creates and
tests the personal Conda/R environment before any production submission.

For archiving completed runs and recreating summaries/figures locally, use
`hpc_opt/KU_HPC_SAFEKAW_RESULTS_EXPORT_AND_LOCAL_ANALYSIS.md`.

Use only the final deployment bundle:

```text
safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip
SHA-256: 92E0F077A76BEF85870448F67B905231B5ABDC0512CEB015FF59BB7A411FF6B3
```

This version includes the watershed-overlap climate weighting, climate-only
wheat model, corrected corn production cost, fertilizer-nitrate elasticity,
historical precipitation CDF, and continuous 2025-2099 future percentile
calculation. Older deployments and their optimization outputs are scientifically
superseded and must not be pooled with this version.

## What is already complete

- The final deployment passed local preflight validation.
- One constrained seed was completed for all 72 combinations:
  - 12 climate/period/land-use contexts;
  - 2 irrigation technologies;
  - 3 constrained phases.
- KU job `25700500` completed all 72 array tasks.
- The output audit passed `72/72`.
- All 4,608 solutions passed crop-bound, irrigation-extent, and alluvial-area
  feasibility checks.

The completed deployment on the original user's allocation is:

```text
/kuhpc/work/water/b923w423/SAFEKAW-Optimization-clippedcdf-20260808
```

Access and write permissions should be confirmed before another user attempts
to run inside that directory. If collaborators do not have write permission,
deploy a separate copy under their own `$WORK` directory.

### Copying the completed constrained seed into a collaborator deployment

The production commands below start constrained replication at seeds 2-3 and
therefore assume that the completed seed-1 outputs remain available for final
pooling. If the collaborator can read the original user's work directory, copy
the completed run directories, manifests, grid, and audit into the personal
deployment:

```bash
SOURCE=/kuhpc/work/water/b923w423/SAFEKAW-Optimization-clippedcdf-20260808
DEST="$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

mkdir -p "$DEST/hpc_opt/outputs/runs"
mkdir -p "$DEST/hpc_opt/outputs/optimization_manifests"

for tag in \
  constrained_clippedcdf_20260808_crop_bounds \
  constrained_clippedcdf_20260808_crop_fert \
  constrained_clippedcdf_20260808_crop_fert_irrigfrac
do
  cp -a "$SOURCE/hpc_opt/outputs/runs/$tag" \
    "$DEST/hpc_opt/outputs/runs/"
  cp -a "$SOURCE/hpc_opt/outputs/optimization_manifests/$tag" \
    "$DEST/hpc_opt/outputs/optimization_manifests/"
done

cp -a \
  "$SOURCE/hpc_opt/outputs/optimization_grid_constrained_clippedcdf_seed1.csv" \
  "$DEST/hpc_opt/outputs/"

cp -a \
  "$SOURCE/hpc_opt/outputs/optimization_grid_constrained_clippedcdf_seed1_audit.csv" \
  "$DEST/hpc_opt/outputs/"
```

Run the 72-job audit in the destination after copying. If group permissions do
not allow these reads, the original account owner must grant group-readable
access or provide a separate archive. If seed-1 outputs cannot be transferred,
the collaborator must regenerate constrained seed 1 before seeds 2-3 are pooled.

## What remains to run

Recommended minimum writing-ready design:

1. Constrained final-phase replication:
   - phase: `crop_fert_irrigfrac`;
   - irrigation: current and efficient;
   - seeds: 2 and 3;
   - 48 new jobs.
2. Corrected unconstrained optimization:
   - seeds: 1, 2, and 3;
   - population: 64;
   - generations: 50;
   - 36 jobs.
3. Assess three-seed stability. Add seeds 4-5 only for contexts that do not
   satisfy the chosen stability criterion.

## Requirements

The operator needs:

- a KU HPC account;
- permission to charge the `water` account and use the `water` partition;
- access to the final deployment ZIP and checksum;
- the SAFE-KAW Conda environment or permission to use the existing shared
  environment;
- write permission in the selected project directory.

Each collaborator should create the personal environment documented in
`KU_HPC_SAFEKAW_ENVIRONMENT_SETUP.md` at:

```text
$WORK/conda-envs/safekaw
```

## Important HPC concepts

- `sbatch` submits work and immediately returns a job ID.
- The work continues after MobaXterm disconnects.
- An array such as `1-48%12` contains 48 tasks, with at most 12 running at once.
- The 12-hour Slurm limit applies separately to every array task, not to the
  whole array.
- A run tag isolates outputs. Never reuse a run tag across scientifically
  different model versions.
- Array workers validate existing outputs. Valid outputs are skipped safely on
  resubmission.

## A. Starting a new MobaXterm session

Every new login starts in the home directory without the Conda environment.
Run:

```bash
module purge
module load conda/latest
conda activate "$WORK/conda-envs/safekaw"
```

For the collaborator's personal final deployment:

```bash
cd "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"
pwd
which Rscript
```

Expected project path:

```text
<the collaborator's $WORK>/SAFEKAW-Optimization-clippedcdf-20260808
```

## B. Deploying a separate collaborator copy

Upload the ZIP and checksum to the collaborator's `$WORK` directory using the
MobaXterm SFTP pane. Then run:

```bash
cd "$WORK"

sed -i 's/\r$//' \
  safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip.sha256

sha256sum -c \
  safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip.sha256
```

The checksum must say `OK`. Extract:

```bash
mkdir -p "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

unzip -o \
  safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip \
  -d "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

cd "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"
```

Update the Slurm script to point to this user's deployment and request 4 GB:

```bash
sed -i \
  's|PROJECT_DIR="${WORK}/SAFEKAW-Optimization"|PROJECT_DIR="${WORK}/SAFEKAW-Optimization-clippedcdf-20260808"|' \
  hpc_opt/scripts/06_submit_optimization_array.slurm

sed -i \
  's/#SBATCH --mem=16G/#SBATCH --mem=4G/' \
  hpc_opt/scripts/06_submit_optimization_array.slurm

sed -i 's/\r$//' \
  hpc_opt/scripts/06_submit_optimization_array.slurm

grep -E 'PROJECT_DIR|#SBATCH --mem|#SBATCH --time' \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

Expected settings:

```text
#SBATCH --mem=4G
#SBATCH --time=12:00:00
PROJECT_DIR="${WORK}/SAFEKAW-Optimization-clippedcdf-20260808"
```

## C. Preflight rule

Always run preflight after generating a grid and before `sbatch`:

```bash
Rscript hpc_opt/scripts/06_preflight_optimization.R --grid GRID_FILE.csv
```

Do not submit if preflight reports an error. Preflight checks models, common
inputs, precomputes, deterministic crop-bound paths, grid structure, packages,
and runtime files. A successful preflight does not assess scientific
convergence; it verifies operational readiness.

## D. Run constrained final-phase seeds 2-3

From the final project directory with the environment active:

```bash
Rscript hpc_opt/scripts/06_make_constrained_optimization_grid.R \
  --seeds 2,3 \
  --popsize 64 \
  --generations 50 \
  --phases crop_fert_irrigfrac \
  --irrigation current,efficient \
  --run-tag-prefix constrained_clippedcdf_20260808 \
  --output hpc_opt/outputs/optimization_grid_constrained_clippedcdf_final_seeds2_3.csv
```

Preflight:

```bash
Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid hpc_opt/outputs/optimization_grid_constrained_clippedcdf_final_seeds2_3.csv
```

Expected: `48 jobs (24 contexts)`.

Submit:

```bash
CON_REP=$(sbatch --parsable \
  --array=1-48%12 \
  --export=ALL,GRID_FILE=hpc_opt/outputs/optimization_grid_constrained_clippedcdf_final_seeds2_3.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm)

echo "Constrained replication job: $CON_REP"
```

Expected execution time is approximately 16-18 hours after tasks begin.

## E. Run corrected unconstrained seeds 1-3

```bash
Rscript hpc_opt/scripts/06_make_optimization_grid.R \
  --seeds 1,2,3 \
  --popsize 64 \
  --generations 50 \
  --run-tag unconstrained_clippedcdf_p64_g50 \
  --output hpc_opt/outputs/optimization_grid_unconstrained_clippedcdf_seeds1_3.csv
```

Preflight:

```bash
Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid hpc_opt/outputs/optimization_grid_unconstrained_clippedcdf_seeds1_3.csv
```

Expected: `36 jobs (12 contexts)`.

Submit:

```bash
UNCON_JOB=$(sbatch --parsable \
  --array=1-36%12 \
  --export=ALL,GRID_FILE=hpc_opt/outputs/optimization_grid_unconstrained_clippedcdf_seeds1_3.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm)

echo "Unconstrained job: $UNCON_JOB"
```

Expected execution time is approximately 9-12 hours after tasks begin.

The constrained and unconstrained arrays may run simultaneously only if the
allocation permits 24 concurrent tasks. Otherwise submit them sequentially.

## F. Monitor jobs

Show the operator's queued/running jobs:

```bash
squeue -u "$USER"
```

Monitor one array:

```bash
squeue -j JOB_ID
```

Accounting and resource use:

```bash
sacct -j JOB_ID \
  --format=JobID,State,Elapsed,Timelimit,MaxRSS,ExitCode
```

Find recent SAFE-KAW jobs after reconnecting:

```bash
sacct -S 2026-08-08 \
  --name=safekaw-opt \
  --format=JobID,State,Elapsed,Timelimit,MaxRSS,ExitCode
```

Inspect error logs:

```bash
grep -iE 'error|failed|killed|oom|traceback' \
  hpc_opt/outputs/logs/optimization_JOBID_*.err
```

No grep output is normally good. Replace `JOBID` with the base array job ID.

## G. Audit completed arrays

Constrained replication:

```bash
Rscript hpc_opt/scripts/06_audit_optimization_grid.R \
  --grid hpc_opt/outputs/optimization_grid_constrained_clippedcdf_final_seeds2_3.csv
```

Target: `Complete jobs: 48/48`.

Unconstrained:

```bash
Rscript hpc_opt/scripts/06_audit_optimization_grid.R \
  --grid hpc_opt/outputs/optimization_grid_unconstrained_clippedcdf_seeds1_3.csv
```

Target: `Complete jobs: 36/36`.

An audit exits with a nonzero status if a job is missing, failed, incomplete,
or lacks its expected output.

## H. Safe restart and resubmission

It is safe to resubmit the same array and grid:

```bash
sbatch --array=1-N%12 \
  --export=ALL,GRID_FILE=THE_SAME_GRID.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

The worker validates existing outputs and marks them `skipped_valid`. Failed or
missing jobs run normally. Do not use `--force` unless there is an explicit
reason to overwrite valid results.

To rerun only selected array rows:

```bash
sbatch --array=3,8,17-20%4 \
  --export=ALL,GRID_FILE=THE_SAME_GRID.csv \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

## I. Cancel a run

Identify the base array job ID:

```bash
squeue -u "$USER"
```

Cancel the whole array:

```bash
scancel JOB_ID
```

Verify:

```bash
squeue -j JOB_ID
sacct -j JOB_ID --format=JobID,State,Elapsed,ExitCode
```

Cancelled and partially completed outputs should be preserved for provenance.
If the model implementation or inputs change, deploy to a new directory and use
a new run tag; never rely on skip-validation across scientific versions.

## J. Output locations

Pareto fronts:

```text
hpc_opt/outputs/runs/<run_tag>/<scenario_name>/pareto_front_<scenario_name>_seed<seed>.csv
```

Raw optimizer objects:

```text
hpc_opt/outputs/runs/<run_tag>/<scenario_name>/nsga2_res_<scenario_name>_seed<seed>.rds
```

Per-job manifests:

```text
hpc_opt/outputs/optimization_manifests/<run_tag>/job_<id>.csv
```

Slurm logs:

```text
hpc_opt/outputs/logs/optimization_<array-job-id>_<task-id>.out
hpc_opt/outputs/logs/optimization_<array-job-id>_<task-id>.err
```

## K. Common problems

### `Rscript: command not found`

The Conda environment is not active:

```bash
module purge
module load conda/latest
conda activate "$WORK/conda-envs/safekaw"
```

### `Missing grid`

Either the grid has not been generated or the shell is not at the project root:

```bash
pwd
ls hpc_opt/outputs/optimization_grid_*.csv
```

### Slurm job immediately fails but preflight passed

Check that `PROJECT_DIR` in the Slurm script points to the deployed directory:

```bash
grep PROJECT_DIR hpc_opt/scripts/06_submit_optimization_array.slurm
```

### Checksum says the ZIP filename contains `$'\r'`

Normalize the checksum file:

```bash
sed -i 's/\r$//' FILE.zip.sha256
sha256sum -c FILE.zip.sha256
```

### Old results are being skipped

Stop the submission and inspect `run_tag` in the grid. Scientifically different
models must use different deployment directories and run tags.

### Task state is `TIMEOUT`

The individual task exceeded its time limit. Do not merely resubmit it unchanged.
Inspect its log and increase the Slurm time request only after confirming that
the process is progressing normally.

### Task state is `OUT_OF_MEMORY`

Inspect `MaxRSS` using `sacct`. Increase `--mem` only enough to provide a
reasonable margin above observed use. Validated runs used less than 0.5 GB;
4 GB currently provides substantial headroom.

## L. Information to record for reproducibility

For every production submission, record:

- deployment directory and bundle SHA-256;
- grid CSV;
- Slurm array job ID;
- run tag;
- seeds;
- population and generations;
- phase and irrigation scenarios;
- audit CSV;
- submission date;
- any resubmitted task IDs.

Never infer these values later from memory; the grid and manifests are the
authoritative execution record.
