# File: R/data_pipeline/validation.R
new_issues <- function() dplyr::tibble(check = character(), detail = character())

add_issue <- function(issues, check, detail) {
  dplyr::bind_rows(issues, dplyr::tibble(check = check, detail = detail))
}

validate_readings <- function(df, value_limits = c(0, 1e6)) {
  issues <- new_issues()
  required <- c("machine_id", "sensor_type", "value", "reading_time")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0)
    issues <- add_issue(issues, "required_columns", paste(missing, collapse = ","))
  if (nrow(df) == 0)
    return(list(ok = FALSE, issues = add_issue(issues, "empty", "0 rows")))

  if (any(is.na(df$value)))
    issues <- add_issue(issues, "null_values", sum(is.na(df$value)))
  oob <- sum(df$value < value_limits[1] | df$value > value_limits[2], na.rm = TRUE)
  if (oob > 0) issues <- add_issue(issues, "out_of_range", paste(oob, "rows"))

  dupes <- df |>
    dplyr::count(machine_id, sensor_type, reading_time) |>
    dplyr::filter(n > 1) |> nrow()
  if (dupes > 0) issues <- add_issue(issues, "duplicates", paste(dupes, "keys"))

  list(ok = nrow(issues) == 0, issues = issues)
}

validate_machines <- function(df) {
  issues <- new_issues()
  missing <- setdiff(c("machine_name", "machine_type"), names(df))
  if (length(missing) > 0)
    issues <- add_issue(issues, "required_columns", paste(missing, collapse = ","))
  if ("machine_name" %in% names(df) && anyDuplicated(df$machine_name) > 0)
    issues <- add_issue(issues, "duplicate_names",
                        paste(which(duplicated(df$machine_name)), collapse = ","))
  list(ok = nrow(issues) == 0, issues = issues)
}

validate_maintenance <- function(df) {
  issues <- new_issues()
  missing <- setdiff(c("machine_id", "maintenance_type", "performed_at"), names(df))
  if (length(missing) > 0)
    issues <- add_issue(issues, "required_columns", paste(missing, collapse = ","))
  if ("maintenance_type" %in% names(df)) {
    bad <- setdiff(unique(df$maintenance_type),
                   c("preventive", "corrective", "failure"))
    if (length(bad) > 0)
      issues <- add_issue(issues, "invalid_type", paste(bad, collapse = ","))
  }
  list(ok = nrow(issues) == 0, issues = issues)
}

stop_if_invalid <- function(result, what) {
  if (!result$ok) {
    logger::log_error("Validation failed for {what}")
    print(result$issues)
    stop(sprintf("Validation failed: %s", what), call. = FALSE)
  }
  invisible(TRUE)
}