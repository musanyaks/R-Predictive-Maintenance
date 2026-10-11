# File: R/feature_engineering/feature_selection.R
#' Drop highly correlated features (keeps first occurrence)
correlation_filter <- function(features_df, cols, threshold = 0.95) {
  cm <- stats::cor(features_df[cols], use = "complete.obs")
  cm[is.na(cm)] <- 0
  drop <- character()
  for (cl in cols[-1]) {
    highly_corr <- names(which(abs(cm[, cl]) > threshold))
    highly_corr <- setdiff(highly_corr, c(cl, drop))
    if (length(highly_corr) > 0) drop <- c(drop, highly_corr)
  }
  setdiff(cols, unique(drop))
}

#' Keep top-k features by XGBoost importance
select_features <- function(features_df, cols, target, k = 25) {
  ok  <- stats::complete.cases(features_df[c(cols, target)])
  x   <- as.matrix(features_df[ok, cols])
  y   <- as.numeric(features_df[[target]][ok] == "failure")
  bst <- xgboost::xgb.train(
    data = xgboost::xgb.DMatrix(x, label = y),
    nrounds = 50, max_depth = 4, eta = 0.1,
    objective = "binary:logistic", verbose = 0
  )
  imp <- xgboost::xgb.importance(model = bst)
  imp$Feature[seq_len(min(k, nrow(imp)))]
}

#' Full selection pipeline
prune_features <- function(features_df, target, threshold = 0.95, k = 25) {
  all_cols <- get_feature_names(features_df)
  cols <- correlation_filter(features_df, all_cols, threshold)
  if (target %in% names(features_df))
    cols <- select_features(features_df, cols, target, k)
  cols
}