suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}
grid_file <- get_arg(
  "--grid",
  here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv")
)
if (!file.exists(grid_file)) stop("Missing grid: ", grid_file)

grid <- read_csv(grid_file, show_col_types = FALSE)
manifest_rows <- map_dfr(seq_len(nrow(grid)), function(i) {
  manifest <- here(
    "hpc_opt", "outputs", "optimization_manifests", grid$run_tag[i],
    sprintf("job_%04d.csv", grid$job_id[i])
  )
  if (!file.exists(manifest)) {
    return(tibble(job_id = grid$job_id[i], manifest_status = "missing"))
  }
  read_csv(manifest, show_col_types = FALSE) %>%
    transmute(
      job_id,
      manifest_status = status,
      elapsed_seconds,
      output_file,
      manifest_message = message
    )
})

audit <- grid %>%
  left_join(manifest_rows, by = "job_id") %>%
  mutate(
    manifest_status = coalesce(manifest_status, "missing"),
    output_exists = !is.na(output_file) & file.exists(output_file),
    complete = manifest_status %in% c("complete", "skipped_valid") &
      output_exists
  )

out_file <- file.path(
  dirname(grid_file),
  paste0(tools::file_path_sans_ext(basename(grid_file)), "_audit.csv")
)
write_csv(audit, out_file)
status_summary <- audit %>% count(manifest_status, complete, name = "jobs")
print(status_summary, n = Inf)
message("Complete jobs: ", sum(audit$complete), "/", nrow(audit))
message("Wrote grid audit: ", out_file)

if (!all(audit$complete)) quit(save = "no", status = 2)
