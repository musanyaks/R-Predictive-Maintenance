# File: R/feature_engineering/rolling_features.R
#' Rolling mean/sd per machine over wide sensor columns
add_rolling_features <- function(df, windows = c(6, 24), cols = NULL) {
  if (is.null(cols)) cols <- grep("^value_", names(df), value = TRUE)
  df |>
    dplyr::group_by(machine_id) |>
    dplyr::arrange(reading_time, .by_group = TRUE) |>
    dplyr::group_modify(function(d, key) {
      for (w in windows) {
        for (cl in cols) {
          d[[sprintf("roll_mean_%s_%d", cl, w)]] <-
            zoo::rollmeanr(d[[cl]], k = w, fill = NA)
          d[[sprintf("roll_sd_%s_%d", cl, w)]] <-
            zoo::rollapplyr(d[[cl]], width = w, FUN = stats::sd, fill = NA, partial = TRUE)
        }
      }
      d
    }) |>
    dplyr::ungroup()
}