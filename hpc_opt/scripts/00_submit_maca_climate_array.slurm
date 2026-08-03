#!/bin/bash
#SBATCH --job-name=safekaw-maca
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --array=1-30%3
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --output=hpc_opt/outputs/logs/maca_%A_%a.out
#SBATCH --error=hpc_opt/outputs/logs/maca_%A_%a.err

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

Rscript hpc_opt/scripts/00_run_maca_climate_array_worker.R \
  --task-id "${SLURM_ARRAY_TASK_ID}"
