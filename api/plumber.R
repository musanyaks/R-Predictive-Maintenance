# File: api/plumber.R
# Builds the full API router.
# Run: rpredictmaint::run_api() or
#   pkgload::load_all(); source("api/plumber.R"); plumber::pr_run(build_api(), port = 8000)

for (f in c(list.files("api/middleware", full.names = TRUE),
            list.files("api/endpoints", full.names = TRUE))) source(f)

build_api <- function() {
  setup_logging("api")

  root <- plumber::pr()
  root <- plumber::pr_get(root, "/api/v1/health", function()
    list(status = "ok", time = format(now_utc(), "%Y-%m-%dT%H:%M:%SZ")))
  root <- plumber::pr_mount(root, "/api/v1/predictions",   predictions_api())
  root <- plumber::pr_mount(root, "/api/v1/machines",      machines_api())
  root <- plumber::pr_mount(root, "/api/v1/sensors",       sensors_api())
  root <- plumber::pr_mount(root, "/api/v1/alerts",        alerts_api())
  root <- plumber::pr_mount(root, "/api/v1/maintenance",   maintenance_api())
  root
}

run_api <- function(port = get_app_config()$api$port) {
  pr <- build_api()
  logger::log_info("API listening on port {port}")
  plumber::pr_run(pr, host = "0.0.0.0", port = as.integer(port))
}