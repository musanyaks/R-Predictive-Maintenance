# File: R/prediction/batch_prediction.R
#' Orchestrator: latest readings -> features -> all active models -> DB.
#' Used by the targets pipeline, the API, and scheduled jobs.
run_batch_predictions <- function(lookback_hours = 96) {
  log_step("batch_prediction", {
    # 1. Load active models
    registry <- get_active_models()
    if (nrow(registry) == 0) stop("No active models registered. Train first.")
    models <- setNames(
      lapply(registry$artifact_path, load_model_artifact),
      registry$model_name
    )

    # 2. Features (single source of truth)
    readings <- get_readings_from_db(
      since = now_utc() - lubridate::hours(lookback_hours))
    if (nrow(readings) == 0) stop("No recent readings in DB.")
    feats <- compute_features(readings)

    # 3. Latest row per machine -> inference frame
    latest <- feats |>
      dplyr::group_by(machine_id) |>
      dplyr::slice_max(reading_time, n = 1) |>
      dplyr::ungroup()
    fcols <- grep("^(value_|roll_|lag|delta|dev_|range_|hour_|day_)",
                  names(feats), value = TRUE)

    # 4. Predict with each model
    p_failure <- predict_failure(models[["failure_classifier"]], latest, fcols)
    p_rul     <- predict_rul(models[["rul_model"]], latest, fcols)
    p_anom    <- score_anomaly(models[["anomaly_detector"]], latest, fcols)

    combined <- p_failure |>
      dplyr::left_join(p_rul,  by = c("machine_id", "reading_time")) |>
      dplyr::left_join(p_anom, by = c("machine_id", "reading_time")) |>
      dplyr::mutate(rul_days = tidyr::replace_na(rul_days, 999),
                    anomaly_score = tidyr::replace_na(anomaly_score, 0))
    risk <- compute_risk_score(combined$failure_prob,
                               combined$rul_days, combined$anomaly_score)
    combined <- dplyr::bind_cols(combined, risk)

    # 5. Persist + alert
    machines_tbl <- get_machines()
    out <- combined |>
      dplyr::left_join(machines_tbl[c("machine_id", "machine_name")],
                       by = "machine_id") |>
      dplyr::mutate(
        model_version = registry$version[registry$model_name == "failure_classifier"][1],
        predicted_at = now_utc())

    insert_predictions(out |>
      dplyr::select(machine_id, model_version, failure_prob, rul_days,
                    anomaly_score, risk_score, risk_level, predicted_at))

    for (i in which(out$risk_level %in% c("high", "critical"))) {
      insert_alert(out$machine_id[i], out$risk_level[i],
        sprintf("%s: risk %.0f (%s), RUL %.1f d",
                out$machine_name[i], out$risk_score[i],
                out$risk_level[i], out$rul_days[i]))
    }
    logger::log_info("Batch predictions written for {nrow(out)} machines.")
    invisible(out)
  })
}

#' Load the active artifacts as a named list (API/monitoring helper)
load_active_models <- function() {
  reg <- get_active_models()
  setNames(lapply(reg$artifact_path, load_model_artifact), reg$model_name)
}