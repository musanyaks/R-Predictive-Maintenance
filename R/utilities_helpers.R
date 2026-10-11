# File: R/utilities_helpers.R

`%||%` <- function(a, b) if (is.null(a)) b else a

project_root <- function() {
  tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION") | rprojroot::is_git_root),
    error = function(e) getwd()
  )
}

now_utc <- function() lubridate::with_tz(Sys.time(), "UTC")

#' Read app config (config/config.yml) — environment-aware via config pkg
get_app_config <- function() {
  config::get(file = file.path(project_root(), "config", "config.yml"))
}

#' Read YAML config with !expr evaluation and default/production unwrapping
read_yaml_config <- function(file) {
  raw <- yaml::read_yaml(file.path(project_root(), "config", file),
                         eval.expr = TRUE)
  env <- Sys.getenv("R_CONFIG_ACTIVE", "default")
  raw[[env]] %||% raw$default %||% raw
}

get_model_config <- function() read_yaml_config("model_config.yml")
get_db_config    <- function() read_yaml_config("database.yml")

save_model_artifact <- function(model, name, model_type) {
  cfg  <- get_app_config()
  dir  <- file.path(project_root(), cfg$paths$models, model_type, name)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, "model.rds")
  saveRDS(model, path)
  path
}

load_model_artifact <- function(path) readRDS(path)
