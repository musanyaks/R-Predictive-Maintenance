# File: R/database/queries.R
get_machines <- function(active_only = TRUE) {
  sql <- "SELECT * FROM machines"
  if (active_only) sql <- paste(sql, "WHERE status = 'active'")
  DBI::dbGetQuery(get_pool(), sql)
}

get_readings_from_db <- function(machine_ids = NULL, since = NULL) {
  sql <- "SELECT machine_id, sensor_type, value, reading_time FROM sensor_readings WHERE TRUE"
  if (!is.null(machine_ids))
    sql <- paste(sql, sprintf("AND machine_id IN (%s)",
              paste(as.integer(machine_ids), collapse = ",")))
  if (!is.null(since))
    sql <- paste(sql, sprintf("AND reading_time >= '%s'",
              format(lubridate::with_tz(since, "UTC"), "%Y-%m-%d %H:%M:%S")))
  DBI::dbGetQuery(get_pool(), paste(sql, "ORDER BY machine_id, reading_time"))
}

insert_readings_db <- function(df) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbWithTransaction(con, {
    chunk <- 5000
    for (i in seq(1, nrow(df), by = chunk)) {
      end <- min(i + chunk - 1, nrow(df))
      DBI::dbAppendTable(con, "sensor_readings", dplyr::slice(df, i:end))
    }
  })
  invisible(nrow(df))
}

get_maintenance_from_db <- function(machine_ids = NULL) {
  sql <- "SELECT * FROM maintenance"
  if (!is.null(machine_ids))
    sql <- paste(sql, sprintf("WHERE machine_id IN (%s)",
              paste(as.integer(machine_ids), collapse = ",")))
  DBI::dbGetQuery(get_pool(), paste(sql, "ORDER BY machine_id, performed_at"))
}

insert_maintenance_db <- function(df) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbAppendTable(con, "maintenance", df)
  invisible(nrow(df))
}

insert_predictions <- function(df) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbAppendTable(con, "predictions", df)
  invisible(nrow(df))
}

get_latest_predictions <- function(limit = 500) {
  sql <- sprintf("
    SELECT DISTINCT ON (machine_id) *
    FROM predictions ORDER BY machine_id, predicted_at DESC LIMIT %d", limit)
  DBI::dbGetQuery(get_pool(), sql)
}

insert_alert <- function(machine_id, alert_level, message) {
  DBI::dbExecute(get_pool(),
    "INSERT INTO alerts (machine_id, alert_level, message) VALUES ($1, $2, $3)",
    params = list(as.integer(machine_id), alert_level, message))
}

get_open_alerts <- function() {
  DBI::dbGetQuery(get_pool(),
    "SELECT a.*, m.machine_name FROM alerts a
     JOIN machines m USING (machine_id)
     WHERE a.resolved_at IS NULL ORDER BY a.created_at DESC")
}

resolve_alert <- function(alert_id) {
  DBI::dbExecute(get_pool(),
    "UPDATE alerts SET resolved_at = now() WHERE alert_id = $1",
    params = list(as.integer(alert_id)))
}

register_model <- function(model_name, version, model_type, metrics, artifact_path) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO model_registry (model_name, version, model_type, metrics, artifact_path)
    VALUES ($1, $2, $3, $4::jsonb, $5)
    ON CONFLICT (model_name, version) DO UPDATE
      SET metrics = EXCLUDED.metrics, artifact_path = EXCLUDED.artifact_path,
          trained_at = now()",
    params = list(model_name, version, model_type, jsonlite::toJSON(metrics), artifact_path))
  DBI::dbExecute(con, "
    UPDATE model_registry SET active = FALSE
    WHERE model_name = $1 AND version <> $2",
    params = list(model_name, version))
  mirror_registry_csv()
}

get_active_models <- function() {
  DBI::dbGetQuery(get_pool(),
    "SELECT * FROM model_registry WHERE active ORDER BY model_name")
}

# Fallback mirror so the registry is inspectable without a DB client
mirror_registry_csv <- function() {
  df <- DBI::dbGetQuery(get_pool(), "SELECT * FROM model_registry ORDER BY trained_at DESC")
  dir <- file.path(project_root(), "models", "model_metadata")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, file.path(dir, "model_registry.csv"))
  invisible(df)
}