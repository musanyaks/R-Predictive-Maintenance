# File: R/monitoring/model_monitoring.R
#' Full monitoring pass: drift + performance + alerting
run_model_monitoring <- function(lookback_days = 14) {
  log_step("model_monitoring", {
    mcfg <- get_model_config()$monitoring

    all_preds <- DBI::dbGetQuery(get_pool(),
      "SELECT * FROM predictions ORDER BY predicted_at")
    ref  <- dplyr::filter(all_preds, predicted_at < now_utc() - lubridate::days(30))
    curr <- dplyr::filter(all_preds, predicted_at >= now_utc() - lubridate::days(lookback_days))

    drift <- if (nrow(ref) > 50 && nrow(curr) > 50)
      detect_prediction_drift(ref, curr, mcfg$psi_threshold) else NULL

    if (!is.null(drift) && any(drift$drift, na.rm = TRUE)) {
      machines <- get_machines()
      for (m in drift$metric[which(drift$drift)]) {
        insert_alert(machines$machine_id[1], "drift",
          sprintf("Prediction drift detected in %s (PSI > %.2f)",
                  m, mcfg$psi_threshold))
      }
    }

    perf <- if (nrow(curr) > 0)
      evaluate_predictions(curr, get_maintenance_from_db()) else NULL
    if (!is.null(perf)) write_performance_report(perf)

    list(drift = drift, performance = perf, timestamp = now_utc())
  })
}