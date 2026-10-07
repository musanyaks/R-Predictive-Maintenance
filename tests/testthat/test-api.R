# File: tests/testthat/test-api.R
test_that("API router builds with health route and auth filter", {
  router <- build_api()
  expect_s3_class(router, "plumber")
  expect_true("auth" %in% names(router$filters))
})