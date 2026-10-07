# File: R/feature_engineering/lag_features.R
#' Lagged values (and deltas vs current) per machine
add_lag_features <- function(df, lags = c(1, 6, 12), cols = NULL) {
  if (is.null(cols)) cols <- grep("^value_", names(df), value = TRUE)
  df |>
    dplyr::group_by(machine_id) |>
    dplyr::arrange(reading_time, .by_group = TRUE) |>
    dplyr::group_modify(function(d, key) {
      for (l in lags) {
        for (cl in cols) {
          d[[sprintf("lag%d_%s", l, cl)]]   <- dplyr::lag(d[[cl]], l)
          d[[sprintf("delta%d_%s", l, cl)]] <- d[[cl]] - dplyr::lag(d[[cl]], l)
        }
      }
      d
    }) |>
    dplyr::ungroup()
}