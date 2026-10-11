# File: R/models/failure_classifier.R
#' Add binary label: failure-type event within horizon after the feature row?
add_failure_label <- function(features_df, maintenance_df, horizon_hours = 48) {
  failures <- maintenance_df |>
    dplyr::filter(maintenance_type == "failure") |>
    dplyr::select(machine_id, event_time = performed_at)

  features_df |>
    dplyr::group_by(machine_id) |>
    dplyr::group_modify(function(d, key) {
      ev <- failures$event_time[failures$machine_id == key$machine_id]
      d$label <- vapply(d$reading_time, function(t) {
        any(ev > t & ev <= t + lubridate::hours(horizon_hours))
      }, logical(1))
      d
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(failure = factor(ifelse(label, "failure", "ok"),
                                   levels = c("ok", "failure"))) |>
    dplyr::select(-label)
}

#' Train XGBoost failure classifier (tidymodels workflow)
train_failure_classifier <- function(train_df, feature_cols,
                                     trees = 400, tree_depth = 4,
                                     learn_rate = 0.05) {
  cols <- make.names(feature_cols)
  d <- train_df[stats::complete.cases(train_df[c("failure", cols)]),
                c("failure", cols)]
  spec <- parsnip::boost_tree(
      trees = trees, tree_depth = tree_depth, learn_rate = learn_rate
    ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")
  fml <- stats::as.formula(paste("failure ~", paste(cols, collapse = " + ")))
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(fml)
  generics::fit(wf, data = d)
}

#' Predict failure probability; returns machine_id, reading_time, failure_prob
predict_failure <- function(model, new_df, feature_cols) {
  cols <- make.names(feature_cols)
  ok   <- stats::complete.cases(new_df[cols])
  d    <- new_df[ok, cols, drop = FALSE]
  prob <- as.numeric(parsnip::predict(model, d, type = "prob")$.pred_failure)
  dplyr::bind_cols(
    new_df[c("machine_id", "reading_time")][ok, ],
    dplyr::tibble(failure_prob = prob)
  )
}