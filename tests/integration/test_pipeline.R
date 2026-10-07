# File: tests/integration/test_pipeline.R
# End-to-end smoke test: data -> features -> models -> risk.
# Run manually: Rscript tests/integration/test_pipeline.R
pkgload::load_all()
setup_logging("integration")

readings <- generate_synthetic_readings(n_machines = 5, days = 45, seed = 99)
maint <- tibble::tibble(
  machine_id = rep(1:2, each = 2),
  maintenance_type = c("failure", "preventive", "preventive", "failure"),
  performed_at = rep(c(now_utc() - lubridate::days(3),
                       now_utc() - lubridate::days(20)), 2)
)

feats   <- compute_features(readings)
labeled <- add_failure_label(feats, maint, 48) |> add_rul_label(maint)
cols    <- prune_features(labeled, "failure", 0.99, 5)
splits  <- time_train_test_split(labeled, 0.8)

fit_c <- train_failure_classifier(splits$train, cols, trees = 30, tree_depth = 3)
fit_r <- train_rul_workflow(splits$train, cols)
fit_a <- train_anomaly_detector(splits$train, cols)

pf <- predict_failure(fit_c, splits$test, cols)
pr <- predict_rul(fit_r, splits$test, cols)
pa <- score_anomaly(fit_a, splits$test, cols)

final <- pf |>
  dplyr::left_join(pr, by = c("machine_id", "reading_time")) |>
  dplyr::left_join(pa, by = c("machine_id", "reading_time")) |>
  dplyr::mutate(rul_days = tidyr::replace_na(rul_days, 999),
                anomaly_score = tidyr::replace_na(anomaly_score, 0))
risk <- compute_risk_score(final$failure_prob, final$rul_days, final$anomaly_score)
final <- dplyr::bind_cols(final, risk)

stopifnot(nrow(final) > 0,
          all(final$risk_score >= 0 & final$risk_score <= 100))
print(head(final))
cat("Integration test PASSED\n")