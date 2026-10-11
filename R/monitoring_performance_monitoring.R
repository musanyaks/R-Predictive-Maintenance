# File: R/monitoring/performance_monitoring.R
#' Backtest: were high-risk predictions followed by actual events?
evaluate_predictions <- function(predictions_df, maintenance_df,
                                 horizon_days = 30) {
  events <- maintenance_df |>
    dplyr::filter(maintenance_type %in% c("failure", "corrective")) |>
    dplyr::select(machine_id, event_time = performed_at)

  preds <- predictions_df |>
    dplyr::group_by(machine_id) |>
    dplyr::slice_max(predicted_at, n = 1) |>
    dplyr::ungroup()

  preds$outcome <- vapply(seq_len(nrow(preds)), function(i) {
    ev <- events$event_time[events$machine_id == preds$machine_id[i]]
    any(ev > preds$predicted_at[i] &
        ev <= preds$predicted_at[i] + lubridate::days(horizon_days))
  }, logical(1))

  if (length(unique(preds$outcome)) < 2)
    return(dplyr::tibble(metric = "note",
                         value = "insufficient outcome variety"))

  truth <- factor(ifelse(preds$outcome, "failure", "ok"), levels = c("ok", "failure"))
  est   <- factor(ifelse(preds$risk_level %in% c("high", "critical"),
                         "failure", "ok"), levels = c("ok", "failure"))
  dplyr::tibble(
    metric = c("precision", "recall", "n_evaluated"),
    value  = c(
      yardstick::precision_vec(truth, est, event_level = "second")$.estimate,
      yardstick::recall_vec(truth, est, event_level = "second")$.estimate,
      nrow(preds))
  )
}

write_performance_report <- function(metrics_df) {
  dir <- file.path(project_root(), "reports", "model_performance")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(metrics_df, file.path(dir,
    paste0("performance_", format(Sys.Date(), "%Y%m%d"), ".csv")))
  invisible(metrics_df)
}