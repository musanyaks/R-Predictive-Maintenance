# File: tests/testthat/test-features.R
test_that("compute_features is deterministic and produces expected columns", {
  readings <- generate_synthetic_readings(n_machines = 2, days = 10, seed = 1)
  f1 <- compute_features(readings)
  f2 <- compute_features(readings)
  expect_equal(names(f1), names(f2))
  expect_true(nrow(f1) > 0)
  expect_true(all(c("machine_id", "reading_time", "value_temperature") %in% names(f1)))
  expect_true(any(grepl("^roll_mean_value_", names(f1))))
  expect_true(any(grepl("^delta1_value_", names(f1))))
  expect_true(any(grepl("^dev_from_mean_value_", names(f1))))
  expect_false(any(is.na(f1$value_temperature)))
})