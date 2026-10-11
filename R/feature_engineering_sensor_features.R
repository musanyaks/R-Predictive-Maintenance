# File: R/feature_engineering_sensor_features.R
compute_features <- function(readings_long, windows = NULL, lags = NULL,
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
    dplyr::group_by(machine_id) |>
    dplyr::filter(dplyr::row_number() > min_history) |>
    dplyr::ungroup()
}

get_feature_names <- function(features_df) {
  exclude <- c("machine_id", "reading_time", "failure", "rul_days")  # FIX: labels out
  cols <- setdiff(names(features_df), exclude)
  cols[vapply(features_df[cols], is.numeric, logical(1))]            # numeric only
}
