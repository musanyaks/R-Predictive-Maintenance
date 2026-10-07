# File: R/utilities/logging.R
#' Configure the logger for the whole application
setup_logging <- function(app_name = "rpredictmaint",
                          level = Sys.getenv("LOG_LEVEL", "INFO")) {
  log_dir <- file.path(project_root(), "logs", "application")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  logger::log_threshold(level)
  logger::log_layout(logger::layout_glue_colors)
  logger::log_appender(logger::appender_tee(
    file.path(log_dir, paste0(app_name, "-", format(Sys.Date(), "%Y%m%d"), ".log"))
  ))
  invisible(TRUE)
}

#' Log a pipeline step with timing
log_step <- function(name, expr) {
  logger::log_info("START: {name}")
  t0 <- Sys.time()
  out <- force(expr)
  logger::log_info("DONE:  {name} ({round(as.numeric(Sys.time() - t0, 'secs'), 1)}s)")
  invisible(out)
}