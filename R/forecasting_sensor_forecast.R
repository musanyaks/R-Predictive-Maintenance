# File: R/forecasting/sensor_forecast.R
#' Forecast one sensor series `horizon` hours ahead (auto-ARIMA, naive fallback)
forecast_sensor <- function(values, horizon = 24, freq = 24) {
  values <- stats::na.omit(values)
  if (length(values) < 2 * freq) {
    last  <- values[length(values)]
    drift <- (values[length(values)] - values[1]) / max(length(values) - 1, 1)
    fc    <- last + drift * (1:horizon)
    return(dplyr::tibble(step = 1:horizon, forecast = fc, lo = fc, hi = fc))
  }
  ts_obj <- stats::ts(values, frequency = freq)
  fit <- tryCatch(
    forecast::auto.arima(ts_obj, seasonal = TRUE, quiet = TRUE),
    error = function(e) forecast::auto.arima(ts_obj, seasonal = FALSE, quiet = TRUE)
  )
  f <- forecast::forecast(fit, h = horizon, level = 90)
  dplyr::tibble(step = 1:horizon,
                forecast = as.numeric(f$mean),
                lo = as.numeric(f$lower), hi = as.numeric(f$upper))
}

#' Forecast every sensor column for one machine's feature table
forecast_all_sensors <- function(features_df, horizon = 24) {
  cols <- grep("^value_", names(features_df), value = TRUE)
  last <- features_df |> dplyr::arrange(dplyr::desc(reading_time)) |> dplyr::slice(1)
  purrr::map(cols, function(cl) {
    series <- features_df |> dplyr::arrange(reading_time) |> dplyr::pull(cl)
    fc <- forecast_sensor(series, horizon = horizon)
    dplyr::mutate(fc, sensor = cl, machine_id = last$machine_id)
  }) |> dplyr::bind_rows()
}