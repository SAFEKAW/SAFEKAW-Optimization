# Setting up SAFE-KAW on a collaborator's KU HPC account

## Scope

This guide starts from an empty personal KU `$WORK` directory. It creates a
personal Conda/R environment, deploys the final SAFE-KAW bundle, verifies the
installation, and runs a small scheduler-level environment test.

After this setup passes, use
`hpc_opt/KU_HPC_SAFEKAW_OPTIMIZATION_RUNBOOK.md` for production grids,
submission, monitoring, auditing, and recovery.

## Files to obtain before starting

The collaborator needs these four files:

```text
safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip
safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip.sha256
HPC_BUNDLE_MANIFEST_20260808.md
KU_HPC_SAFEKAW_OPTIMIZATION_RUNBOOK.md
```

Authoritative bundle SHA-256:

```text
92E0F077A76BEF85870448F67B905231B5ABDC0512CEB015FF59BB7A411FF6B3
```

Use no earlier deployment bundle for scientific runs.

## 1. Log in and confirm the personal work directory

Connect to the KU HPC with MobaXterm or SSH. Then run:

```bash
echo "$USER"
echo "$WORK"
pwd
```

`$WORK` should point to a user-specific directory under the KU work
filesystem. The environment and deployment created below belong to this user;
they do not depend on another collaborator's `$WORK` directory.

Create the required parent directories:

```bash
mkdir -p "$WORK/conda-envs"
mkdir -p "$WORK/SAFEKAW-transfer"
```

## 2. Upload the handoff files

Using the MobaXterm SFTP pane, upload the ZIP, checksum, manifest, and runbook
into:

```text
$WORK/SAFEKAW-transfer/
```

Shell variables such as `$WORK` are not expanded by the graphical SFTP pane.
Use the full path printed by `echo "$WORK"`.

Confirm the uploaded files:

```bash
cd "$WORK/SAFEKAW-transfer"
ls -lh
```

## 3. Verify the deployment archive

Files created on Windows may contain carriage returns in the checksum file.
Normalize it and verify the archive:

```bash
sed -i 's/\r$//' \
  safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip.sha256

sha256sum -c \
  safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip.sha256
```

Required result:

```text
safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip: OK
```

Stop if the checksum does not say `OK`. Do not run or extract an unverified
archive.

## 4. Load Conda

Inspect the available module if necessary:

```bash
module spider conda
```

Load the same module used by the validated workflow:

```bash
module purge
module load conda/latest
conda --version
```

## 5. Create the personal SAFE-KAW environment

Create the environment at the exact path expected by the bundled Slurm worker:

```bash
conda create -y \
  -p "$WORK/conda-envs/safekaw" \
  -c conda-forge \
  --strict-channel-priority \
  r-base=4.4 \
  r-here \
  r-readr \
  r-dplyr \
  r-tidyr \
  r-yaml \
  r-mco
```

Activate it:

```bash
conda activate "$WORK/conda-envs/safekaw"
which Rscript
Rscript --version
```

The `Rscript` path should begin with the collaborator's own `$WORK` path.

### If `r-mco` is unavailable from Conda

Create the environment without `r-mco`, activate it, and install `mco` from
CRAN:

```bash
conda create -y \
  -p "$WORK/conda-envs/safekaw" \
  -c conda-forge \
  --strict-channel-priority \
  r-base=4.4 \
  r-here r-readr r-dplyr r-tidyr r-yaml

conda activate "$WORK/conda-envs/safekaw"

Rscript -e 'install.packages("mco", repos = "https://cloud.r-project.org")'
```

Use this fallback only if the first Conda command explicitly reports that
`r-mco` cannot be resolved.

## 6. Verify the R package environment

```bash
Rscript -e '
required <- c("here", "readr", "dplyr", "tidyr", "yaml", "mco")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
cat("All required SAFE-KAW R packages are installed.\n")
print(sessionInfo())
'
```

Stop and resolve any missing package before continuing.

## 7. Extract the final deployment

```bash
mkdir -p "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

unzip -o \
  "$WORK/SAFEKAW-transfer/safekaw-hpc-optimization-deploy-clippedcdf-20260808.zip" \
  -d "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

cd "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"
```

Verify critical files:

```bash
test -f hpc_opt/models/yield_kg_Wheat.rds &&
test -f hpc_opt/models/yield_kcal_Wheat.rds &&
test -f hpc_opt/outputs/precompute/precomp_ensemble_rcp85_late.rds &&
test -f hpc_opt/scripts/06_submit_optimization_array.slurm &&
echo "Critical deployment files are present."
```

## 8. Configure the Slurm wrapper for this account

The archived wrapper contains a generic project path. Change it to the final
deployment name, reduce the validated memory request to 4 GB, and normalize
line endings:

```bash
sed -i \
  's|PROJECT_DIR="${WORK}/SAFEKAW-Optimization"|PROJECT_DIR="${WORK}/SAFEKAW-Optimization-clippedcdf-20260808"|' \
  hpc_opt/scripts/06_submit_optimization_array.slurm

sed -i \
  's/#SBATCH --mem=16G/#SBATCH --mem=4G/' \
  hpc_opt/scripts/06_submit_optimization_array.slurm

sed -i 's/\r$//' \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

Verify the important settings:

```bash
grep -E '#SBATCH --account|#SBATCH --partition|#SBATCH --qos|#SBATCH --mem|#SBATCH --time|PROJECT_DIR|SAFEKAW_CONDA_ENV' \
  hpc_opt/scripts/06_submit_optimization_array.slurm
```

Required values include:

```text
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --mem=4G
#SBATCH --time=12:00:00
PROJECT_DIR="${WORK}/SAFEKAW-Optimization-clippedcdf-20260808"
SAFEKAW_CONDA_ENV="${SAFEKAW_CONDA_ENV:-${WORK}/conda-envs/safekaw}"
```

If the collaborator is not authorized for the `water` account or partition,
they must resolve access with the allocation owner or KU HPC support before
submitting production jobs.

## 9. Run the bundled preflight-validation grid

The bundle contains a small validation grid. It is not a production grid.
Locate it:

```bash
ls hpc_opt/outputs/*preflight*.csv
```

Then run preflight using the returned filename. For example:

```bash
PREFLIGHT_GRID=$(find hpc_opt/outputs -maxdepth 1 -type f \
  -name '*preflight*.csv' | head -n 1)

test -n "$PREFLIGHT_GRID" || {
  echo "No bundled preflight grid found."
  exit 1
}

Rscript hpc_opt/scripts/06_preflight_optimization.R \
  --grid "$PREFLIGHT_GRID"
```

Do not submit the bundled preflight grid as a scientific production run.

## 10. Test the environment through Slurm

A login-node test is not sufficient because production runs execute on compute
nodes. Submit this short scheduler-level environment test:

```bash
mkdir -p hpc_opt/outputs/logs

ENV_TEST_JOB=$(sbatch --parsable \
  --account=water \
  --partition=water \
  --qos=normal \
  --time=00:05:00 \
  --mem=1G \
  --output=hpc_opt/outputs/logs/environment_test_%j.out \
  --error=hpc_opt/outputs/logs/environment_test_%j.err \
  --wrap='module purge; module load conda/latest; conda activate "$WORK/conda-envs/safekaw"; Rscript -e '\''required <- c("here","readr","dplyr","tidyr","yaml","mco"); stopifnot(all(vapply(required, requireNamespace, logical(1), quietly=TRUE))); cat("COMPUTE_NODE_ENVIRONMENT_OK\n")'\''')

echo "Environment test job: $ENV_TEST_JOB"
```

Wait for completion:

```bash
squeue -j "$ENV_TEST_JOB"
sacct -j "$ENV_TEST_JOB" --format=JobID,State,Elapsed,ExitCode
cat "hpc_opt/outputs/logs/environment_test_${ENV_TEST_JOB}.out"
cat "hpc_opt/outputs/logs/environment_test_${ENV_TEST_JOB}.err"
```

Required output:

```text
COMPUTE_NODE_ENVIRONMENT_OK
```

The job state must be `COMPLETED` with exit code `0:0`.

## 11. Preserve an environment record

Record the environment used for the scientific run:

```bash
mkdir -p hpc_opt/outputs/environment

conda env export \
  -p "$WORK/conda-envs/safekaw" \
  > hpc_opt/outputs/environment/conda_environment_full.yml

conda env export --from-history \
  -p "$WORK/conda-envs/safekaw" \
  > hpc_opt/outputs/environment/conda_environment_history.yml

conda list --explicit \
  -p "$WORK/conda-envs/safekaw" \
  > hpc_opt/outputs/environment/conda_explicit_spec.txt

Rscript -e 'writeLines(capture.output(sessionInfo()), "hpc_opt/outputs/environment/R_sessionInfo.txt")'
```

Keep these files with the grid, job ID, run tag, audit, and final outputs.

## 12. Setup completion checklist

Do not submit production optimization until all items are true:

- [ ] Bundle checksum reports `OK`.
- [ ] Deployment is under the collaborator's own `$WORK`.
- [ ] `which Rscript` points into `$WORK/conda-envs/safekaw`.
- [ ] All six required R packages load successfully.
- [ ] Slurm `PROJECT_DIR` points to the collaborator's deployment.
- [ ] Slurm environment path points to the collaborator's Conda environment.
- [ ] The user can charge the `water` account.
- [ ] The bundled preflight-validation grid passes.
- [ ] The compute-node environment test prints
      `COMPUTE_NODE_ENVIRONMENT_OK`.
- [ ] Environment specifications have been saved.

After all checks pass, continue with the production commands in
`KU_HPC_SAFEKAW_OPTIMIZATION_RUNBOOK.md`.
