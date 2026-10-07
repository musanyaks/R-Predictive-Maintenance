# File: R/data_pipeline/transformation.R
#' Long sensor readings -> one row per machine-hour with value_<type> columns
readings_to_wide <- function(df) {
  df |>
    dplyr::mutate(value = as.numeric(value),
                  hour = lubridate::floor_date(reading_time, "hour")) |>
    dplyr::group_by(machine_id, sensor_type, hour) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = sensor_type, values_from = value,
                       names_prefix = "value_") |>
    dplyr::rename(reading_time = hour) |>
    dplyr::arrange(machine_id, reading_time)
}

add_time_features <- function(df) {
  df |>
    dplyr::mutate(
      hour_of_day = lubridate::hour(reading_time),
      day_of_week = lubridate::wday(reading_time, week_start = 1)
    )
}

#' Time-based split: everything before cutoff = train
time_train_test_split <- function(df, prop = 0.8) {
  cutoff <- df |>
    dplyr::distinct(reading_time) |>
    dplyr::arrange(reading_time) |>
    dplyr::pull(reading_time)
  cutoff <- cutoff[ceiling(prop * length(cutoff))]
  list(
    cutoff = cutoff,
    train  = dplyr::filter(df, reading_time <  cutoff),
    test   = dplyr::filter(df, reading_time >= cutoff)
  )
}

#' Scale numeric columns using train statistics only
scale_train_test <- function(train, test, cols = NULL) {
  if (is.null(cols)) {
    cols <- names(train)[vapply(train, is.numeric, logical(1))]
    cols <- setdiff(cols, c("machine_id", "reading_time"))
  }
  means <- vapply(train[cols], mean, numeric(1), na.rm = TRUE)
  sds   <- vapply(train[cols], stats::sd, numeric(1), na.rm = TRUE)
  sds[sds == 0 | is.na(sds)] <- 1
  scale_one <- function(d) {
    d[cols] <- sweep(sweep(d[cols], 2, means), 2, sds, "/")
    d
  }
  list(train = scale_one(train), test = scale_one(test),
       scaler = list(means = means, sds = sds, cols = cols))
}