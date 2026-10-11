# File: R/explainability/feature_importance.R
#' Extract XGBoost gain importance from a fitted workflow
get_feature_importance <- function(model_workflow) {
  xgb <- parsnip::extract_fit_engine(model_workflow)
  xgboost::xgb.importance(model = xgb) |>
    dplyr::rename(feature = Feature, gain = Gain, cover = Cover, freq = Frequency)
}

plot_feature_importance <- function(imp, top_n = 15) {
  imp |>
    dplyr::slice_max(gain, n = top_n) |>
    ggplot2::ggplot(ggplot2::aes(gain, reorder(feature, gain))) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::labs(title = "Top feature importance (gain)",
                  x = "Gain", y = NULL) +
    ggplot2::theme_minimal()
}