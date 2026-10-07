# File: R/utilities/helpers.R
`%||%` <- function(a, b) if (is.null(a)) b else a

project_root <- function() {
  tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION") | rprojroot::is_git_root),
    error = function(e) getwd()
  )
}

now_utc <- function() lubridate::with_tz(Sys.time(), "UTC")

#' Read app config (config/config.yml)
get_app_config <- function() {
  config::get(file = file.path(project_root(), "config", "config.yml"))
}

#' Read arbitrary YAML config with ${ENV_VAR} substitution
read_yaml_config <- function(file) {
  raw <- yaml::read_yaml(file.path(project_root(), "config", file))
  sub_env <- function(x) {
    if (is.list(x)) return(lapply(x, sub_env))
    if (is.character(x)) {
      x <- gsub("\\$\\{([A-Z_][A-Z0-9_]*)\\}",
                function(m) Sys.getenv(m, unset = ""), x)
      num <- suppressWarnings(as.numeric(x))
      if (!is.na(num) && grepl("^-?[0-9.]+$", x)) return(num)
      if (x %in% c("true", "false")) return(x == "true")
    }
    x
  }
  sub_env(raw)
}

get_model_config <- function() read_yaml_config("model_config.yml")
get_db_config    <- function() read_yaml_config("database.yml")

save_model_artifact <- function(model, name, model_type) {
  cfg <- get_app_config()
  dir <- file.path(project_root(), cfg$paths$models, model_type, name)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, "model.rds")
  saveRDS(model, path)
  path
}

load_model_artifact <- function(path) readRDS(path)