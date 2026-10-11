# File: R/models/survival_model.R
#' Machine-level survival dataset: time to first failure/corrective event
prepare_survival_data <- function(features_df, maintenance_df) {
  event_types <- c("failure", "corrective")
  events <- maintenance_df |>
    dplyr::filter(maintenance_type %in% event_types) |>
    dplyr::group_by(machine_id) |>
    dplyr::summarise(first_event = min(performed_at), .groups = "drop")

  summary_feats <- features_df |>
    dplyr::group_by(machine_id) |>
    dplyr::summarise(dplyr::across(dplyr::starts_with("value_"), mean, na.rm = TRUE),
                     .groups = "drop")

  first_reading <- features_df |>
    dplyr::group_by(machine_id) |>
    dplyr::summarise(start_time = min(reading_time), .groups = "drop")

  dplyr::left_join(first_reading, events, by = "machine_id") |>
    dplyr::left_join(summary_feats, by = "machine_id") |>
    dplyr::mutate(
      event = !is.na(first_event),
      time  = as.numeric(difftime(dplyr::if_else(event, first_event, now_utc()),
                                  start_time, units = "days")),
      time  = pmax(time, 0.5)
    ) |>
    dplyr::select(-first_event, -start_time)
}

#' Weibull AFT survival model
fit_survival_model <- function(surv_df) {
  covars <- grep("^value_", names(surv_df), value = TRUE)
  fml <- stats::as.formula(paste(
    "survival::Surv(time, event) ~",
    if (length(covars) > 0) paste(covars, collapse = " + ") else "1"))
  survival::survreg(fml, data = surv_df, dist = "weibull")
}

#' P(event within horizon_days); returns survival_prob and hazard
predict_failure_probability <- function(model, newdata, horizon_days = 30) {
  lp    <- stats::predict(model, newdata = newdata, type = "lp")
  scale <- model$scale
  # AFT Weibull: P(T > t) = exp(-exp((log t - lp) / scale))
  surv_h  <- exp(-exp((log(horizon_days) - lp) / scale))
  surv_0  <- exp(-exp((log(1e-3) - lp) / scale))  # ~1
  p_event <- pmin(pmax(1 - surv_h / surv_0, 0), 1)
  dplyr::tibble(
    machine_id        = newdata$machine_id,
    survival_prob_30d = 1 - p_event,
    hazard_30d        = p_event
  )
}