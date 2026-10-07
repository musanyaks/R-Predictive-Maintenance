# File: R/models/anomaly_detection.R
#' Isolation forest anomaly detector (isotree)
train_anomaly_detector <- function(train_df, feature_cols, seed = 42) {
  cols <- make.names(feature_cols)
  d <- train_df[stats::complete.cases(train_df[cols]), cols, drop = FALSE]
  isotree::isolation.forest(
    d, seed = seed, ndim = 2, sample_size = min(1024, nrow(d)))
}

#' Score new data; returns 0-1 anomaly_score (rank-normalized per batch)
score_anomaly <- function(detector, new_df, feature_cols) {
  cols <- make.names(feature_cols)
  ok   <- stats::complete.cases(new_df[cols])
  d    <- new_df[ok, cols, drop = FALSE]
  raw  <- predict(detector, d)
  s    <- dplyr::percent_rank(raw)  # 0 = normal, 1 = most anomalous in batch
  dplyr::bind_cols(
    new_df[c("machine_id", "reading_time")][ok, ],
    dplyr::tibble(anomaly_score = s)
  )
}