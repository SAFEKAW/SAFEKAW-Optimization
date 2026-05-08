
library(fs)
library(stringr)
library(purrr)
library(dplyr)
library(readr)


proj_root <- "."

key_files <- dir_info(proj_root, recurse = TRUE) %>%
  mutate(
    rel_path = path_rel(path, start = proj_root),
    ext = path_ext(path)
  ) %>%
  filter(type == "file") %>%
  filter(
    ext %in% c("R", "Rmd", "yml", "yaml", "csv", "rds", "gpkg")
  ) %>%
  filter(
    str_detect(rel_path, regex("opt|scen|climate|gridmet|maca|model|input|eval|precomp|yaml", ignore_case = TRUE))
  ) %>%
  select(rel_path, ext, size)

write_csv(key_files, "key_files_inventory.csv")

r_files <- dir_ls(".", recurse = TRUE, regexp = "\\.[Rr](md)?$")

source_map <- map_dfr(r_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  tibble(
    file = f,
    line = seq_along(lines),
    text = lines
  ) %>%
    filter(str_detect(text, "source\\("))
})

write_csv(source_map, "source_calls.csv")
