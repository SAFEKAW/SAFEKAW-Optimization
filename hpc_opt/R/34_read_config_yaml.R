suppressPackageStartupMessages({
  library(yaml)
})

read_config_yaml <- function(path) {
  if (!file.exists(path)) stop("Config YAML does not exist: ", path)
  yaml::read_yaml(path)
}