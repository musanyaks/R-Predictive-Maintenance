# File: tests/testthat/test-predictions.R
test_that("risk scoring respects thresholds", {
  hi <- compute_risk_score(0.95, 1, 0.95)
  lo <- compute_risk_score(0.02, 60, 0.05)
  expect_true(hi$risk_score > lo$risk_score)
  expect_true(hi$risk_level %in% c("high", "critical"))
  expect_equal(lo$risk_level, "low")
})

test_that("recommendations escalate with risk", {
  rec_hi <- recommend_maintenance(1L, "PUMP-001", "critical", 90, 1,
                                  list(summary = "test"))
  rec_lo <- recommend_maintenance(1L, "PUMP-001", "low", 5, 90,
                                  list(summary = "test"))
  expect_equal(rec_hi$priority, 1L)
  expect_equal(rec_lo$priority, 4L)
  expect_lt(as.numeric(rec_hi$due_by), as.numeric(rec_lo$due_by))
})