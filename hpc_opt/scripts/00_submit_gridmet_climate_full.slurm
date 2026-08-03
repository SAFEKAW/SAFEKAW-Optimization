#!/bin/bash
#SBATCH --job-name=safekaw-gridmet-full
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=08:00:00
#SBATCH --output=hpc_opt/outputs/logs/gridmet_full_%j.out
#SBATCH --error=hpc_opt/outputs/logs/gridmet_full_%j.err

set -euo pipefail

module purge
module load conda/latest

PROJECT_DIR="${WORK}/SAFEKAW-Optimization"
cd "${PROJECT_DIR}"

mkdir -p hpc_opt/outputs/logs
export SAFEKAW_CLIMATE_SCRATCH="${SCRATCH}/SAFEKAW-Optimization/climate"
export SAFEKAW_CONDA_ENV="${WORK}/conda-envs/safekaw"
export SAFEKAW_DOWNLOAD_ATTEMPTS=5
mkdir -p "${SAFEKAW_CLIMATE_SCRATCH}"

conda activate "${SAFEKAW_CONDA_ENV}"

Rscript hpc_opt/scripts/00_make_climate_inputs_gridmet.R
