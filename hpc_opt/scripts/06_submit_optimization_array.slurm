#!/bin/bash
#SBATCH --job-name=safekaw-opt
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=12:00:00
#SBATCH --output=hpc_opt/outputs/logs/optimization_%A_%a.out
#SBATCH --error=hpc_opt/outputs/logs/optimization_%A_%a.err

set -euo pipefail

module purge
module load conda/latest

PROJECT_DIR="${WORK}/SAFEKAW-Optimization"
GRID_FILE="${GRID_FILE:-hpc_opt/outputs/optimization_grid_unconstrained.csv}"
SAFEKAW_CONDA_ENV="${SAFEKAW_CONDA_ENV:-${WORK}/conda-envs/safekaw}"

cd "${PROJECT_DIR}"
mkdir -p hpc_opt/outputs/logs
conda activate "${SAFEKAW_CONDA_ENV}"

Rscript hpc_opt/scripts/06_run_optimization_array_worker.R \
  --task-id "${SLURM_ARRAY_TASK_ID}" \
  --grid "${GRID_FILE}"
