# File: R/models/model_tuning.R
#' Tune XGBoost hyperparameters with v-fold CV; returns best params + final spec
tune_failure_classifier <- function(train_df, feature_cols, grid_levels = 4) {
  cols <- make.names(feature_cols)
  d <- train_df[stats::complete.cases(train_df[c("failure", cols)]),
                c("failure", cols)]

  spec <- parsnip::boost_tree(
      trees      = parsnip::trees(),
      tree_depth = parsnip::tree_depth(),
      learn_rate = parsnip::learn_rate()
    ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")

  grid <- dials::grid_regular(
    dials::trees(c(100, 500)),
    dials::tree_depth(c(3, 6)),
    dials::learn_rate(c(-3, -1)),
    levels = c(grid_levels, grid_levels, grid_levels)
  )

  folds <- rsample::vfold_cv(d, v = 5, strata = failure)
  wf <- workflows::workflow() |>
        workflows::add_model(spec) |>
        workflows::add_formula(stats::as.formula(
          paste("failure ~", paste(cols, collapse = " + "))))

  tuned <- tune::tune_grid(
    wf, resamples = folds, grid = grid,
    metrics = yardstick::metric_set(yardstick::pr_auc, yardstick::roc_auc),
    control = tune::control_grid(verbose = FALSE)
  )

  best <- tune::select_best(tuned, metric = "pr_auc")
  list(
    best_params = best,
    tuned       = tuned,
    spec        = parsnip::finalize_model(spec, best)
  )
}