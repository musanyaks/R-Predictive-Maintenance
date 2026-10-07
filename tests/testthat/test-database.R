# File: tests/testthat/test-database.R
skip_if_no_db <- function() {
  if (nchar(Sys.getenv("DB_PASSWORD")) == 0) skip("No DB credentials configured")
}

test_that("pool connects and migrations are idempotent", {
  skip_if_no_db()
  expect_true(db_ping())
  expect_no_error(run_migrations())
  expect_no_error(run_migrations())  # second run: IF NOT EXISTS
})