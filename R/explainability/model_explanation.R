# File: R/explainability/model_explanation.R
#' Human-readable explanation for one machine's latest prediction
explain_machine_prediction <- function(pred_row, shap_row = NULL, top_n = 3) {
  reasons <- character()
  if (!is.null(pred_row$failure_prob) && pred_row$failure_prob > 0.5)
    reasons <- c(reasons, sprintf("High failure probability (%.0f%%)",
                                  100 * pred_row$failure_prob))
  if (!is.null(pred_row$rul_days) && pred_row$rul_days < 7)
    reasons <- c(reasons, sprintf("Estimated remaining useful life only %.1f days",
                                  pred_row$rul_days))
  if (!is.null(pred_row$anomaly_score) && pred_row$anomaly_score > 0.8)
    reasons <- c(reasons, "Sensor pattern flagged as anomalous")

  top_factors <- NULL
  if (!is.null(shap_row)) {
    top_factors <- tibble::enframe(shap_row, name = "feature", value = "shap") |>
      dplyr::mutate(abs_shap = abs(shap)) |>
      dplyr::slice_max(abs_shap, n = top_n) |>
      dplyr::select(feature, shap)
    reasons <- c(reasons, sprintf("Main drivers: %s",
                                  paste(top_factors$feature, collapse = ", ")))
  }
  list(
    reasons = reasons,
    top_factors = top_factors,
    summary = if (length(reasons) == 0) "All indicators nominal"
              else paste(reasons, collapse = "; ")
  )
}