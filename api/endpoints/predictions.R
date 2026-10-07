# File: api/endpoints/predictions.R
predictions_api <- function() {
  p <- plumber::pr()
  p <- plumber::pr_get(p, "/latest", function(limit = 500L) {
    get_latest_predictions(limit)
  })
  p <- plumber::pr_post(p, "/run", function() {
    out <- run_batch_predictions()
    list(status = "ok", machines = nrow(out),
         critical = sum(out$risk_level == "critical"),
         high = sum(out$risk_level == "high"))
  })
  p <- plumber::pr_get(p, "/forecast/<machine_name>", function(machine_name,
                                                               horizon = 72L) {
    machines_tbl <- get_machines()
    id <- machines_tbl$machine_id[machines_tbl$machine_name == machine_name]
    if (length(id) == 0) plumber::stop(404, "unknown machine")
    readings <- get_readings_from_db(id, since = now_utc() - lubridate::hours(96))
    feats <- compute_features(readings)
    models <- load_active_models()
    fcols <- grep("^(value_|roll_|lag|delta|dev_|range_|hour_|day_)",
                  names(feats), value = TRUE)
    forecast_failure_risk(feats, models$failure_classifier, fcols, horizon)
  })
  apply_error_handling(apply_request_logging(apply_auth(p)))
}