#!/bin/bash
#SBATCH --job-name=safekaw-maca-test
#SBATCH --account=water
#SBATCH --partition=water
#SBATCH --qos=normal
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=hpc_opt/outputs/logs/maca_smoke_%j.out
#SBATCH --error=hpc_opt/outputs/logs/maca_smoke_%j.err

set -euo pipefail

module purge
module load conda/latest

mkdir -p hpc_opt/outputs/logs
export SAFEKAW_CLIMATE_SCRATCH="${SCRATCH}/SAFEKAW-Optimization/climate"
export SAFEKAW_CONDA_ENV="${WORK}/conda-envs/safekaw"
mkdir -p "${SAFEKAW_CLIMATE_SCRATCH}"

conda activate "${SAFEKAW_CONDA_ENV}"

# One complete crop/yield year for one GCM and scenario. The fetch begins in
# October 2024 to exercise the cross-calendar winter-wheat growing season.
Rscript hpc_opt/scripts/00_make_climate_inputs_maca.R \
  --gcm_name ccsm4 \
  --maca_model CCSM4 \
  --scenario_tag rcp45_early \
  --smoke-year 2025
