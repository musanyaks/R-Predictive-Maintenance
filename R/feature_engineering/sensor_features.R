# File: R/feature_engineering/sensor_features.R
#' THE feature function. Both training AND inference call this.
#' Input: long readings (machine_id, sensor_type, value, reading_time)
#' Output: wide feature table, one row per machine-hour.
compute_features <- function(readings_long,
                             windows = NULL, lags = NULL,
                             min_history = NULL) {
  mcfg <- get_model_config()$features
  windows     <- windows     %||% mcfg$rolling_windows
  lags        <- lags        %||% mcfg$lag_offsets
  min_history <- min_history %||% mcfg$min_history_hours

  readings_long |>
    clean_readings() |>
    readings_to_wide() |>
    add_time_features() |>
    add_rolling_features(windows = windows) |>
    add_lag_features(lags = lags) |>
    add_statistical_features(window = mcfg$stat_window) |>
    # Drop the warm-up period where rolling stats are still NA
    dplyr::group_by(machine_id) |>
    dplyr::filter(dplyr::row_number() > min_history) |>
    dplyr::ungroup()
}

#' Feature column names (excludes keys)
get_feature_names <- function(features_df) {
  exclude <- c("machine_id", "reading_time")
  setdiff(names(features_df), exclude)
}