# File: R/explainability/shap_analysis.R
#' Compute SHAP values for an xgboost workflow on a sample of rows
compute_shap <- function(model_workflow, features_df, feature_cols, n_sample = 200) {
  cols <- make.names(feature_cols)
  d <- features_df[cols]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) > n_sample) d <- dplyr::slice_sample(d, n = n_sample)

  xgb_fit <- parsnip::extract_fit_engine(model_workflow)
  shapviz::shapviz(
    kernelshap::kernelshap(
      object = function(X) {
        as.numeric(stats::predict(xgb_fit, as.matrix(X)))
      },
      X    = d,
      bg_X = dplyr::slice_sample(d, n = min(50, nrow(d)))
    ),
    X = d
  )
}

plot_shap_summary <- function(sv, max_display = 15) {
  print(shapviz::sv_importance(sv, max_display = max_display))
}

plot_shap_waterfall <- function(sv, row = 1) {
  print(shapviz::sv_waterfall(sv, row = row, max_display = 10))
}