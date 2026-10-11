# File: R/prediction/risk_scoring.R
#' Convert model outputs to a 0-100 risk score + categorical level
compute_risk_score <- function(failure_prob, rul_days, anomaly_score,
                               mcfg = get_model_config()$risk) {
  ens <- predict_ensemble(failure_prob, rul_days, anomaly_score,
                          unlist(mcfg$weights), mcfg$rul_half_life_days)
  score_100 <- round(100 * ens$risk, 1)
  t <- mcfg$thresholds
  dplyr::tibble(
    risk_score = score_100,
    risk_level = dplyr::case_when(
      score_100 >= t$critical ~ "critical",
      score_100 >= t$high     ~ "high",
      score_100 >= t$medium   ~ "medium",
      TRUE                    ~ "low")
  )
}