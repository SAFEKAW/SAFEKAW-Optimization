#!/bin/bash
#SBATCH --job-name=safekaw-gridmet-test
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=hpc_opt/outputs/logs/gridmet_smoke_%j.out
#SBATCH --error=hpc_opt/outputs/logs/gridmet_smoke_%j.err

set -euo pipefail

module purge
module load conda/latest

mkdir -p hpc_opt/outputs/logs
export SAFEKAW_CLIMATE_SCRATCH="${SCRATCH}/SAFEKAW-Optimization/climate"
export SAFEKAW_CONDA_ENV="${WORK}/conda-envs/safekaw"
mkdir -p "${SAFEKAW_CLIMATE_SCRATCH}"

conda activate "${SAFEKAW_CONDA_ENV}"

# One complete historical crop/yield year, beginning in October of the prior
# year so the cross-calendar winter-wheat growing season is exercised.
Rscript hpc_opt/scripts/00_make_climate_inputs_gridmet.R \
  --smoke-year 2023
