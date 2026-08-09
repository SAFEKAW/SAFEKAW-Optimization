# Exporting SAFE-KAW HPC results and analyzing them locally

## Provenance rule

Keep scientifically valid and superseded results in separate archives.

- `clippedcdf` constrained results are scientifically valid under the final
  implementation.
- Earlier unconstrained overlap-fix results are not valid for substantive
  environmental or economic inference after the wheat/CDF corrections.
- Those earlier results remain valid evidence for computational methods:
  runtime, memory, optimizer-budget convergence, hypervolume improvement, and
  seed-stability behavior. Label all plots and tables accordingly.

Never pool objective values across these two archives.

## 1. Upload the export script to each HPC deployment

Upload `hpc_opt/scripts/07_export_optimization_results.sh` to the matching path
inside each HPC deployment, then normalize its line endings and permissions:

```bash
sed -i 's/\r$//' hpc_opt/scripts/07_export_optimization_results.sh
chmod u+x hpc_opt/scripts/07_export_optimization_results.sh
```

## 2. Export the final clipped-CDF constrained results

```bash
cd "$WORK/SAFEKAW-Optimization-clippedcdf-20260808"

bash hpc_opt/scripts/07_export_optimization_results.sh \
  "$PWD" \
  safekaw-clippedcdf-constrained-results-20260809 \
  "$WORK/SAFEKAW-exports" \
  '^constrained_clippedcdf_20260808_'
```

This archive includes run CSV/RDS files, manifests, logs, grids, audits,
configs, and runtime code but excludes the large fitted-input deployment data.

## 3. Export the superseded unconstrained methods results

The five-seed overlap-fix unconstrained run, optimizer-budget benchmark, and
seed-stability outputs were created in the earlier deployment. Change the path
below if `pwd` shows a different location:

```bash
cd "$WORK/SAFEKAW-Optimization"

bash hpc_opt/scripts/07_export_optimization_results.sh \
  "$PWD" \
  safekaw-superseded-unconstrained-methods-results-20260809 \
  "$WORK/SAFEKAW-exports" \
  '^(unconstrained_overlapfix_p64_g50|unconstrained_benchmark_|unconstrained_smoke)'
```

The archive should contain the run tag
`unconstrained_overlapfix_p64_g50`, benchmark outputs, and
`optimization_seed_stability` files. Verify the contents listing before
downloading.

## 4. Verify export archives on the HPC

```bash
cd "$WORK/SAFEKAW-exports"
ls -lh

sha256sum -c safekaw-clippedcdf-constrained-results-20260809.tar.gz.sha256
sha256sum -c safekaw-superseded-unconstrained-methods-results-20260809.tar.gz.sha256

grep -E 'unconstrained_overlapfix|optimization_seed_stability|optimization_benchmark' \
  safekaw-superseded-unconstrained-methods-results-20260809.contents.txt | head -n 30
```

## 5. Download with MobaXterm

In the SFTP pane, navigate to the full path printed by:

```bash
echo "$WORK/SAFEKAW-exports"
```

Download each `.tar.gz`, `.sha256`, and `.contents.txt` file. Store the valid
and superseded archives in clearly named local directories.

## 6. Verify locally on Windows

In PowerShell, change into the download directory and run:

```powershell
Get-FileHash -Algorithm SHA256 .\safekaw-clippedcdf-constrained-results-20260809.tar.gz
Get-FileHash -Algorithm SHA256 .\safekaw-superseded-unconstrained-methods-results-20260809.tar.gz
```

Compare the values with the downloaded checksum files.

Extract with 7-Zip or PowerShell/tar:

```powershell
tar -xzf .\safekaw-clippedcdf-constrained-results-20260809.tar.gz -C .\clippedcdf-constrained
tar -xzf .\safekaw-superseded-unconstrained-methods-results-20260809.tar.gz -C .\superseded-unconstrained-methods
```

## 7. Run the local analysis script

The local R installation needs:

```r
install.packages(c("readr", "dplyr", "purrr", "ggplot2", "scales"))
```

Analyze each archive separately:

```powershell
Rscript hpc_opt\scripts\07_analyze_exported_optimization_results.R `
  --root .\clippedcdf-constrained `
  --output .\analysis-clippedcdf-constrained

Rscript hpc_opt\scripts\07_analyze_exported_optimization_results.R `
  --root .\superseded-unconstrained-methods `
  --output .\analysis-superseded-unconstrained-methods
```

The script writes:

- an inventory by run, scenario, and seed;
- objective ranges by run/phase/technology;
- Pareto-front PDF figures by run tag;
- optimizer-budget hypervolume figures when benchmark CSVs are present;
- cumulative seed-stability figures when stability CSVs are present.

## 8. Manuscript-safe use of superseded results

Acceptable uses include:

- reporting benchmark runtimes and peak memory;
- documenting why population 64 and 50 generations were selected;
- describing the seed-stability assessment procedure;
- illustrating the computational stopping rule;
- showing normalized convergence diagnostics explicitly labeled as belonging
  to a superseded model build.

Do not use superseded objective values to report nitrate, irrigation, profit,
scenario effects, technology effects, or policy conclusions. Those must come
from the final clipped-CDF deployment.

## 9. Minimum files to preserve permanently

For every final run, preserve:

- Pareto-front CSVs;
- raw optimizer RDS files;
- grid CSVs;
- audit CSVs;
- per-job manifests;
- Slurm logs;
- bundle manifest and SHA-256;
- code and configs included in the export;
- environment exports;
- any hypervolume or seed-stability tables used in the manuscript.
