# File: R/models_ensemble_model.R
predict_ensemble <- function(failure_prob, rul_days, anomaly_score,
                             weights = c(failure = 0.5, rul = 0.3, anomaly = 0.2),
                             half_life_days = 14) {
  if (is.null(names(weights)))                       # hardening: positional fallback
    names(weights) <- c("failure", "rul", "anomaly")
  rul_urgency <- 1 / (1 + pmax(rul_days, 0) / half_life_days)
  risk <- (weights[["failure"]] * failure_prob +
           weights[["rul"]]      * rul_urgency +
           weights[["anomaly"]]  * anomaly_score) / sum(weights)
  dplyr::tibble(failure_prob, rul_days, anomaly_score, rul_urgency, risk)
}

fit_ensemble_weights <- function(val_df, half_life_days = 14) {
  grid <- tidyr::crossing(
    failure = seq(0.2, 0.8, 0.1),
    rul     = seq(0.1, 0.5, 0.1)
  ) |>
    dplyr::mutate(anomaly = round(1 - failure - rul, 1)) |>
    dplyr::filter(anomaly > 0)

  brier_for <- function(w) {
    if (is.null(names(w))) names(w) <- c("failure", "rul", "anomaly")  # FIX
    r <- predict_ensemble(val_df$failure_prob, val_df$rul_days,
                          val_df$anomaly_score, w, half_life_days)$risk
    truth <- as.numeric(val_df$failure == "failure")
    mean((r - truth)^2)
  }

  scored <- grid |>
    dplyr::rowwise() |>
    dplyr::mutate(brier = brier_for(c(failure, rul, anomaly))) |>
    dplyr::ungroup() |>
    dplyr::arrange(brier) |>
    dplyr::slice(1)

  c(failure = scored$failure, rul = scored$rul, anomaly = scored$anomaly)
}
