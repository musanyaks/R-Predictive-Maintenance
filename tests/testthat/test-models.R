# File: tests/testthat/test-models.R
test_that("failure classifier trains and predicts probabilities in [0,1]", {
  readings <- generate_synthetic_readings(n_machines = 4, days = 30, seed = 7)
  feats <- compute_features(readings)
  maint <- tibble::tibble(
    machine_id = 1L, maintenance_type = "failure",
    performed_at = max(feats$reading_time) - lubridate::hours(10))
  labeled <- add_failure_label(feats, maint, horizon_hours = 48)
  cols <- prune_features(labeled, "failure", threshold = 0.99, k = 5)
  fit  <- train_failure_classifier(labeled, cols, trees = 20, tree_depth = 3)
  p    <- predict_failure(fit, labeled, cols)
  expect_true(all(p$failure_prob >= 0 & p$failure_prob <= 1))
  expect_true(nrow(p) > 0)
})

test_that("ensemble weights are non-negative and sum to 1", {
  w <- fit_ensemble_weights(tibble::tibble(
    failure_prob = runif(50), rul_days = rexp(50, 0.1),
    anomaly_score = runif(50), failure = factor(
      ifelse(rbinom(50, 1, 0.3) == 1, "failure", "ok"),
      levels = c("ok", "failure"))))
  expect_gte(min(w), 0)
  expect_equal(sum(w), 1, tolerance = 1e-8)
})