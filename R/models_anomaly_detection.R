# File: R/models_anomaly_detection.R
train_anomaly_detector <- function(train_df, feature_cols, seed = 42) {
  cols <- make.names(feature_cols)
  d <- train_df[stats::complete.cases(train_df[cols]), cols, drop = FALSE]
  isotree::isolation.forest(
    d, seed = seed, ndim = 2, sample_size = min(1024, nrow(d)))
}

score_anomaly <- function(detector, new_df, feature_cols) {
  cols <- make.names(feature_cols)
  ok   <- stats::complete.cases(new_df[cols])
  d    <- new_df[ok, cols, drop = FALSE]
  raw  <- tryCatch(
    predict(detector, d),
    error = function(e) {
      suppressPackageStartupMessages(library(isotree))
      predict(detector, d)
    })
  s <- dplyr::percent_rank(raw)
  dplyr::bind_cols(new_df[c("machine_id", "reading_time")][ok, ],
                   dplyr::tibble(anomaly_score = s))
}
