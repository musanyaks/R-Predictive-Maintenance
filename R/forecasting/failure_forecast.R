# File: R/forecasting/failure_forecast.R
#' Project failure risk forward: walk sensor forecasts into the classifier.
#' Rolling/lag features held constant (forecast-level approximation).
forecast_failure_risk <- function(features_df, classifier, feature_cols,
                                  horizon = 72) {
  sensor_fcst <- forecast_all_sensors(features_df, horizon = horizon)
  last <- features_df |>
    dplyr::arrange(dplyr::desc(reading_time)) |>
    dplyr::slice(1)

  steps <- sort(unique(sensor_fcst$step))
  purrr::map_dfr(steps, function(s) {
    row <- as.list(last)
    for (cl in unique(sensor_fcst$sensor)) {
      row[[cl]] <- sensor_fcst |>
        dplyr::filter(step == s, sensor == cl) |>
        dplyr::pull(forecast)
    }
    row$reading_time <- last$reading_time + lubridate::hours(s)
    row_df <- tibble::as_tibble(row)[1, ]
    p <- tryCatch(
      predict_failure(classifier, row_df, feature_cols)$failure_prob,
      error = function(e) NA_real_)
    dplyr::tibble(machine_id = last$machine_id,
                  day = s / 24, failure_prob = p)
  })
}