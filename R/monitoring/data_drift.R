# File: R/monitoring/data_drift.R
#' Population Stability Index between two vectors
compute_psi <- function(expected, actual, bins = 10) {
  breaks <- stats::quantile(expected, probs = seq(0, 1, length.out = bins + 1),
                            na.rm = TRUE)
  breaks[1] <- -Inf
  breaks[length(breaks)] <- Inf
  breaks <- unique(breaks)
  e <- table(cut(expected, breaks)) / length(expected)
  a <- table(cut(actual,   breaks)) / max(length(actual), 1)
  e <- pmax(as.numeric(e), 1e-6)
  a <- pmax(as.numeric(a), 1e-6)
  sum((a - e) * log(a / e))
}

#' PSI + KS test per feature: reference (training) vs current
detect_data_drift <- function(train_ref, current, feature_cols = NULL,
                              psi_threshold = 0.2) {
  if (is.null(feature_cols))
    feature_cols <- intersect(names(train_ref), names(current))
  feature_cols <- intersect(feature_cols,
    names(current)[vapply(current, is.numeric, logical(1))])

  purrr::map_dfr(feature_cols, function(cl) {
    e <- stats::na.omit(train_ref[[cl]])
    a <- stats::na.omit(current[[cl]])
    if (length(e) < 30 || length(a) < 30)
      return(dplyr::tibble(feature = cl, psi = NA, ks_stat = NA,
                           ks_p = NA, drift = FALSE))
    ks <- stats::ks.test(e, a)
    psi <- compute_psi(e, a)
    dplyr::tibble(feature = cl, psi = psi,
                  ks_stat = unname(ks$statistic), ks_p = ks$p.value,
                  drift = psi > psi_threshold)
  })
}