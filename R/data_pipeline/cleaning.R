# File: R/data_pipeline/cleaning.R
#' Clean long-format sensor readings
clean_readings <- function(df) {
  df |>
    dplyr::mutate(
      reading_time = lubridate::with_tz(lubridate::as_datetime(reading_time), "UTC"),
      value = as.numeric(value)
    ) |>
    dplyr::distinct(machine_id, sensor_type, reading_time, .keep_all = TRUE) |>
    dplyr::arrange(machine_id, sensor_type, reading_time) |>
    dplyr::group_by(machine_id, sensor_type) |>
    dplyr::mutate(
      q1   = stats::quantile(value, 0.25, na.rm = TRUE),
      q3   = stats::quantile(value, 0.75, na.rm = TRUE),
      iqr  = q3 - q1,
      value = pmin(pmax(value, q1 - 3 * iqr), q3 + 3 * iqr),
      value = zoo::na.approx(value, x = as.numeric(reading_time),
                             na.rm = FALSE, rule = 2)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-q1, -q3, -iqr)
}

#' Clean machines; assigns machine_id by row order if absent (matches DB serial)
clean_machines <- function(df) {
  df <- df |>
    dplyr::mutate(machine_name = trimws(machine_name),
                  status = tolower(status %||% "active"))
  if (!"machine_id" %in% names(df))
    df$machine_id <- seq_len(nrow(df))
  df
}

clean_maintenance <- function(df) {
  df |>
    dplyr::mutate(
      performed_at = lubridate::with_tz(lubridate::as_datetime(performed_at), "UTC"),
      maintenance_type = tolower(trimws(maintenance_type))
    ) |>
    dplyr::distinct(machine_id, performed_at, maintenance_type, .keep_all = TRUE)
}