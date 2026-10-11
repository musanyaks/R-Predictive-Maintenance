# File: R/feature_engineering_statistical_features.R
add_statistical_features <- function(df, window = 24, cols = NULL) {
  if (is.null(cols)) cols <- grep("^value_", names(df), value = TRUE)
  df |>
    dplyr::group_by(machine_id) |>
    dplyr::arrange(reading_time, .by_group = TRUE) |>
    dplyr::group_modify(function(d, key) {
      for (cl in cols) {
        roll_mean <- zoo::rollmeanr(d[[cl]], k = window, fill = NA)
        roll_max  <- zoo::rollmaxr(d[[cl]], k = window, fill = NA)
        # zoo has NO rollmin — use rollapplyr(min) instead
        roll_min  <- zoo::rollapplyr(d[[cl]], width = window, FUN = min,
                                     fill = NA, partial = TRUE)
        d[[sprintf("dev_from_mean_%s", cl)]] <- d[[cl]] - roll_mean
        d[[sprintf("range_ratio_%s", cl)]]   <-
          d[[cl]] / pmax(roll_max - roll_min, 1e-9)
      }
      d
    }) |>
    dplyr::ungroup()
}
