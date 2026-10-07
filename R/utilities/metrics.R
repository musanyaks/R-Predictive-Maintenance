# File: R/utilities/metrics.R
classification_metrics <- function(truth, prob) {
  truth <- factor(truth, levels = c("ok", "failure"))
  pred  <- factor(ifelse(prob >= 0.5, "failure", "ok"), levels = c("ok", "failure"))
  dplyr::tibble(
    metric = c("roc_auc", "pr_auc", "accuracy", "precision", "recall", "f1"),
    value  = c(
      yardstick::roc_auc_vec(truth, prob, event_level = "second")$.estimate,
      yardstick::pr_auc_vec(truth, prob, event_level = "second")$.estimate,
      yardstick::accuracy_vec(truth, pred)$.estimate,
      yardstick::precision_vec(truth, pred, event_level = "second")$.estimate,
      yardstick::recall_vec(truth, pred, event_level = "second")$.estimate,
      yardstick::f_meas_vec(truth, pred, event_level = "second")$.estimate
    )
  )
}

regression_metrics <- function(truth, estimate) {
  ok <- stats::complete.cases(truth, estimate)
  dplyr::tibble(
    metric = c("rmse", "mae", "rsq"),
    value  = c(
      yardstick::rmse_vec(truth[ok], estimate[ok])$.estimate,
      yardstick::mae_vec(truth[ok], estimate[ok])$.estimate,
      yardstick::rsq_vec(truth[ok], estimate[ok])$.estimate
    )
  )
}