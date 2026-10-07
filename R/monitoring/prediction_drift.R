# File: R/monitoring/prediction_drift.R
#' Detect drift in the model's output distribution
detect_prediction_drift <- function(pred_reference, pred_current,
                                    psi_threshold = 0.2) {
  purrr::map_dfr(c("failure_prob", "risk_score", "rul_days"), function(cl) {
    if (!all(c(cl) %in% names(pred_reference))) return(NULL)
    e <- stats::na.omit(pred_reference[[cl]])
    a <- stats::na.omit(pred_current[[cl]])
    if (length(e) < 30 || length(a) < 30)
      return(dplyr::tibble(metric = cl, ref_mean = mean(e), cur_mean = mean(a),
                           psi = NA, drift = FALSE))
    psi <- compute_psi(e, a)
    dplyr::tibble(metric = cl, ref_mean = mean(e), cur_mean = mean(a),
                  psi = psi, drift = psi > psi_threshold)
  })
}