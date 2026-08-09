#!/bin/bash

set -euo pipefail

PROJECT_DIR="${1:-$PWD}"
EXPORT_LABEL="${2:-safekaw-optimization-results}"
EXPORT_ROOT="${3:-${WORK:-$PWD}/SAFEKAW-exports}"
RUN_TAG_REGEX="${4:-.*}"

if [[ ! "$EXPORT_LABEL" =~ ^[A-Za-z0-9._-]+$ ]]; then
  echo "Invalid export label: $EXPORT_LABEL" >&2
  exit 2
fi

cd "$PROJECT_DIR"
if [[ ! -d hpc_opt/outputs/runs ]]; then
  echo "No optimization run directory under: $PROJECT_DIR" >&2
  exit 2
fi

mkdir -p "$EXPORT_ROOT"
ARCHIVE="$EXPORT_ROOT/${EXPORT_LABEL}.tar.gz"
CHECKSUM="$ARCHIVE.sha256"
CONTENTS="$EXPORT_ROOT/${EXPORT_LABEL}.contents.txt"

items=()

while IFS= read -r item; do
  items+=("$item")
done < <(
  find hpc_opt/outputs/runs -mindepth 1 -maxdepth 1 -type d -print |
    awk -F/ -v pattern="$RUN_TAG_REGEX" '$NF ~ pattern'
)

if [[ ${#items[@]} -eq 0 ]]; then
  echo "No run tags matched regex: $RUN_TAG_REGEX" >&2
  exit 2
fi

optional_dirs=(
  "hpc_opt/outputs/logs"
  "hpc_opt/outputs/optimization_seed_stability"
  "hpc_opt/outputs/factorial_checks"
  "hpc_opt/config"
  "hpc_opt/R"
  "hpc_opt/scripts"
)
for item in "${optional_dirs[@]}"; do
  [[ -e "$item" ]] && items+=("$item")
done

if [[ -d hpc_opt/outputs/optimization_manifests ]]; then
  while IFS= read -r item; do
    items+=("$item")
  done < <(
    find hpc_opt/outputs/optimization_manifests \
      -mindepth 1 -maxdepth 1 -type d -print |
      awk -F/ -v pattern="$RUN_TAG_REGEX" '$NF ~ pattern'
  )
fi

while IFS= read -r -d '' item; do
  items+=("$item")
done < <(
  find hpc_opt/outputs -maxdepth 1 -type f \
    \( -name '*optimization*.csv' \
       -o -name '*constrained*.csv' \
       -o -name '*benchmark*.csv' \
       -o -name '*audit*.csv' \
       -o -name '*preflight*.csv' \) \
    -print0
)

[[ -f hpc_opt/HPC_BUNDLE_MANIFEST_20260808.md ]] && \
  items+=("hpc_opt/HPC_BUNDLE_MANIFEST_20260808.md")
[[ -f hpc_opt/KU_HPC_SAFEKAW_OPTIMIZATION_RUNBOOK.md ]] && \
  items+=("hpc_opt/KU_HPC_SAFEKAW_OPTIMIZATION_RUNBOOK.md")
[[ -f hpc_opt/KU_HPC_SAFEKAW_ENVIRONMENT_SETUP.md ]] && \
  items+=("hpc_opt/KU_HPC_SAFEKAW_ENVIRONMENT_SETUP.md")

tar -czf "$ARCHIVE" "${items[@]}"
tar -tzf "$ARCHIVE" > "$CONTENTS"

cd "$(dirname "$ARCHIVE")"
sha256sum "$(basename "$ARCHIVE")" > "$(basename "$CHECKSUM")"

echo "Created: $ARCHIVE"
echo "Checksum: $CHECKSUM"
echo "Contents: $CONTENTS"
du -h "$ARCHIVE"
