# File: R/data_pipeline/ingestion.R
#' Load the three raw CSVs from data/raw
load_raw_data <- function() {
  cfg <- get_app_config()
  raw <- function(f) file.path(project_root(), cfg$paths$data_raw, f)
  list(
    sensors     = readr::read_csv(raw("sensor_data.csv"), show_col_types = FALSE),
    machines    = readr::read_csv(raw("machine_metadata.csv"), show_col_types = FALSE),
    maintenance = readr::read_csv(raw("maintenance_history.csv"), show_col_types = FALSE)
  )
}

#' Bulk insert long-format sensor readings (chunked, transactional)
ingest_readings <- function(readings, lookup) {
  df <- readings |>
    dplyr::left_join(lookup[c("machine_name", "machine_id")], by = "machine_name") |>
    dplyr::transmute(machine_id, sensor_type, value, reading_time)
  insert_readings_db(df)
}

#' Load maintenance events into DB (machines lookup required for FK)
insert_maintenance <- function(maintenance, lookup) {
  df <- maintenance |>
    dplyr::left_join(lookup[c("machine_name", "machine_id")], by = "machine_name") |>
    dplyr::select(machine_id, maintenance_type, description, downtime_hours, performed_at)
  insert_maintenance_db(df)
}

#' Synthetic data generator — also used by tests
generate_synthetic_readings <- function(n_machines = 5, days = 30, seed = 42) {
  set.seed(seed)
  start <- now_utc() - lubridate::days(days)
  types <- c("temperature", "vibration", "pressure", "rpm")
  tidyr::expand_grid(
    machine_id   = 1:n_machines,
    reading_time = seq(start, start + lubridate::hours(days * 24 - 1), by = "hour"),
    sensor_type  = types
  ) |>
    dplyr::mutate(
      base  = dplyr::case_when(sensor_type == "temperature" ~ 70,
                               sensor_type == "vibration"   ~ 3,
                               sensor_type == "pressure"    ~ 100,
                               TRUE                         ~ 1750),
      value = base + rnorm(dplyr::n(), 0, base * 0.03) +
              ifelse(machine_id == 1 & reading_time > start + lubridate::days(days - 5),
                     5, 0)
    )
}