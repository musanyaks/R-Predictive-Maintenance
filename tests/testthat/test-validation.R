# File: tests/testthat/test-validation.R
test_that("validate_readings catches duplicates, NAs, missing cols", {
  ok <- tibble::tibble(machine_id = 1, sensor_type = "temperature",
                       value = 70, reading_time = Sys.time())
  expect_true(validate_readings(ok)$ok)

  dup <- dplyr::bind_rows(ok, ok)
  res <- validate_readings(dup)
  expect_false(res$ok)
  expect_true("duplicates" %in% res$issues$check)

  bad <- ok |> dplyr::mutate(value = NA)
  expect_true("null_values" %in% validate_readings(bad)$issues$check)

  nocol <- ok |> dplyr::select(-value)
  expect_true("required_columns" %in% validate_readings(nocol)$issues$check)
})

test_that("validate_maintenance rejects unknown types", {
  df <- tibble::tibble(machine_id = 1, maintenance_type = "alien",
                       performed_at = Sys.time())
  expect_true("invalid_type" %in% validate_maintenance(df)$issues$check)
})