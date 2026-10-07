# File: pipelines/training_pipeline.R
training_pipeline_plan <- function() {
  list(
    targets::tar_target(labels_data, {
      mcfg <- get_model_config()
      labels <- add_failure_label(features_raw, cleaned$maintenance,
                                  mcfg$labels$failure_horizon_hours)
      add_rul_label(labels, cleaned$maintenance)
    }),
    targets::tar_target(feature_cols, prune_features(
      labels_data, "failure",
      threshold = get_model_config()$training$corr_threshold,
      k = get_model_config()$training$top_k_features)),
    targets::tar_target(splits, time_train_test_split(
      labels_data, get_model_config()$training$test_prop)),
    targets::tar_target(tuned_classifier, {
      t <- tune_failure_classifier(splits$train, feature_cols)
      t$spec  # finalized at best params
    }),
    targets::tar_target(fit_classifier, {
      b <- tuned_classifier$best_params
      train_failure_classifier(splits$train, feature_cols,
        trees = b$trees, tree_depth = b$tree_depth, learn_rate = b$learn_rate)
    }),
    targets::tar_target(fit_rul, train_rul_workflow(splits$train, feature_cols)),
    targets::tar_target(fit_anomaly, train_anomaly_detector(splits$train, feature_cols)),
    targets::tar_target(fit_survival, fit_survival_model(
      prepare_survival_data(features_raw, cleaned$maintenance))),
    targets::tar_target(val_predictions, {
      pf <- predict_failure(fit_classifier, splits$test, feature_cols)
      pr <- predict_rul(fit_rul, splits$test, feature_cols)
      pa <- score_anomaly(fit_anomaly, splits$test, feature_cols)
      splits$test |>
        dplyr::select(machine_id, reading_time, failure, rul_truth = rul_days) |>
        dplyr::left_join(pf, by = c("machine_id", "reading_time")) |>
        dplyr::left_join(pr, by = c("machine_id", "reading_time")) |>
        dplyr::left_join(pa, by = c("machine_id", "reading_time")) |>
        dplyr::mutate(rul_days = tidyr::replace_na(rul_days, 999),
                      anomaly_score = tidyr::replace_na(anomaly_score, 0))
    }),
    targets::tar_target(ensemble_weights, fit_ensemble_weights(val_predictions)),
    targets::tar_target(model_metrics, {
      dplyr::bind_rows(
        classification_metrics(val_predictions$failure, val_predictions$failure_prob),
        regression_metrics(val_predictions$rul_truth, val_predictions$rul_days)
      )
    }),
    targets::tar_target(registry_update, {
      ver <- format(Sys.time(), "%Y%m%d.%H%M%S")
      register_model("failure_classifier", ver, "classification",
                     setNames(model_metrics$value, model_metrics$metric),
                     save_model_artifact(fit_classifier, "xgboost", "classification"))
      register_model("rul_model", ver, "rul", list(),
                     save_model_artifact(fit_rul, "rul_model", "regression"))
      register_model("anomaly_detector", ver, "anomaly", list(),
                     save_model_artifact(fit_anomaly, "isolation_forest", "anomaly"))
      register_model("survival_model", ver, "survival", list(),
                     save_model_artifact(fit_survival, "survreg", "regression"))
      register_model("ensemble", ver, "ensemble",
                     as.list(ensemble_weights),
                     save_model_artifact(ensemble_weights, "ensemble", "classification"))
      TRUE
    }),
    targets::tar_target(train_reference_stats, {
      # Snapshot training feature distributions for drift monitoring
      dir.create("data/processed", showWarnings = FALSE)
      arrow::write_parquet(splits$train, "data/processed/features_test.parquet")
      splits$train
    })
  )
}