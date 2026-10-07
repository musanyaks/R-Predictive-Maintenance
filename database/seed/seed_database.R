# File: database/seed/seed_database.R
# Generates a synthetic fleet + history, writes CSVs to data/raw,
# migrates the schema, and loads everything into Postgres.
pkgload::load_all()  # or library(rpredictmaint)

set.seed(42)
n_machines <- 10
days       <- 90
start      <- Sys.time() - lubridate::days(days)

machines <- tibble::tibble(
  machine_name   = sprintf("PUMP-%03d", 1:n_machines),
  machine_type   = sample(c("centrifugal_pump", "compressor", "motor"), n_machines, TRUE),
  location       = sample(c("Plant-A", "Plant-B"), n_machines, TRUE),
  installed_date = Sys.Date() - sample(365:2500, n_machines),
  status         = "active"
)

hours <- days * 24
sensor_data <- tidyr::expand_grid(
  machine_name = machines$machine_name,
  reading_time = seq(start, start + lubridate::hours(hours - 1), by = "hour"),
  sensor_type  = c("temperature", "vibration", "pressure", "rpm")
) |>
  dplyr::mutate(
    base = dplyr::case_when(
      sensor_type == "temperature" ~ 70,
      sensor_type == "vibration"   ~ 3,
      sensor_type == "pressure"    ~ 100,
      TRUE                         ~ 1750
    ),
    drift = as.numeric(reading_time - start) / (24 * 3600) *
            rnorm(dplyr::n(), 0.02, 0.01),
    value = base + drift + rnorm(dplyr::n(), 0, base * 0.03),
    # Inject a degradation ramp in the last 10 days for 3 machines
    value = value + dplyr::if_else(
      machine_name %in% c("PUMP-003", "PUMP-007", "PUMP-010") &
        reading_time > start + lubridate::days(days - 10),
      (as.numeric(reading_time - start - lubridate::days(days - 10)) / 3600) * 0.15,
      0)
  ) |>
  dplyr::select(machine_name, sensor_type, value, reading_time)

maintenance_history <- tidyr::expand_grid(
  machine_name = machines$machine_name,
  month        = 0:2
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    performed_at     = start + lubridate::days(30 * month + sample(1:28, 1)),
    maintenance_type = sample(c("preventive", "preventive", "corrective"), 1),
    description      = maintenance_type,
    downtime_hours   = sample(c(2, 4, 8), 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-month) |>
  dplyr::arrange(machine_name, performed_at)

# 1. Write raw CSVs (source of truth for the offline pipeline)
fs::dir_create("data/raw")
readr::write_csv(machines,            "data/raw/machine_metadata.csv")
readr::write_csv(sensor_data,         "data/raw/sensor_data.csv")
readr::write_csv(maintenance_history, "data/raw/maintenance_history.csv")

# 2. Migrate schema + load into Postgres
rpredictmaint::run_migrations()

con <- pool::poolCheckout(rpredictmaint::get_pool())
on.exit(pool::poolReturn(con), add = TRUE)
DBI::dbWriteTable(con, "machines", machines, append = TRUE)
lookup <- DBI::dbGetQuery(con, "SELECT machine_id, machine_name FROM machines")

rpredictmaint::ingest_readings(sensor_data, lookup)
rpredictmaint::insert_maintenance(maintenance_history, lookup)

logger::log_info("Seed complete: {nrow(machines)} machines, {nrow(sensor_data)} readings.")