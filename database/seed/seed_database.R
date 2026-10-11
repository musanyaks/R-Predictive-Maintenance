# File: database/seed/seed_database.R
pkgload::load_all()

set.seed(42)
n_machines <- 10
days       <- 90
start      <- Sys.time() - lubridate::days(days)

# Degradation episodes: ramp begins at ramp_h, failure (repair) at fail_h.
# Two episodes land in the TRAIN window, one in TEST, so labels exist in both.
episode_spec <- tibble::tibble(
  machine_name = c("PUMP-003", "PUMP-007", "PUMP-010"),
  ramp_h       = c(40, 58, 78) * 24,
  fail_h       = c(48, 66, 89.5) * 24
)

machines <- tibble::tibble(
  machine_name   = sprintf("PUMP-%03d", 1:n_machines),
  machine_type   = sample(c("centrifugal_pump", "compressor", "motor"), n_machines, TRUE),
  location       = sample(c("Plant-A", "Plant-B"), n_machines, TRUE),
  installed_date = Sys.Date() - sample(365:2500, n_machines),
  status         = "active"
)

sensor_data <- tidyr::expand_grid(
  machine_name = machines$machine_name,
  reading_time = seq(start, start + lubridate::hours(days * 24 - 1), by = "hour"),
  sensor_type  = c("temperature", "vibration", "pressure", "rpm")
) |>
  dplyr::mutate(
    base = dplyr::case_when(
      sensor_type == "temperature" ~ 70,
      sensor_type == "vibration"   ~ 3,
      sensor_type == "pressure"    ~ 100,
      TRUE                         ~ 1750),
    elapsed_hours = as.numeric(difftime(reading_time, start, units = "hours")),
    ep       = match(machine_name, episode_spec$machine_name),
    in_ramp  = !is.na(ep) &
               elapsed_hours >= episode_spec$ramp_h[ep] &
               elapsed_hours <  episode_spec$fail_h[ep],
    drift    = (elapsed_hours / 24) * rnorm(dplyr::n(), 0.02, 0.01),
    value    = base + drift + rnorm(dplyr::n(), 0, base * 0.03) +
               dplyr::if_else(in_ramp,
                              0.15 * (elapsed_hours - episode_spec$ramp_h[ep]), 0)
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
    downtime_hours   = sample(c(2, 4, 8), 1)) |>
  dplyr::ungroup() |>
  dplyr::select(-month) |>
  dplyr::bind_rows(episode_spec |>
    dplyr::transmute(machine_name,
                     maintenance_type = "failure",
                     description      = "injected failure (demo)",
                     downtime_hours   = c(12, 8, 16),
                     performed_at     = start + lubridate::seconds(fail_h * 3600))) |>
  dplyr::arrange(machine_name, performed_at)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(machines,            "data/raw/machine_metadata.csv")
readr::write_csv(sensor_data,         "data/raw/sensor_data.csv")
readr::write_csv(maintenance_history, "data/raw/maintenance_history.csv")

rpredictmaint::run_migrations()

con <- pool::poolCheckout(rpredictmaint::get_pool())
DBI::dbWriteTable(con, "machines", machines, append = TRUE)
lookup <- DBI::dbGetQuery(con, "SELECT machine_id, machine_name FROM machines")
pool::poolReturn(con)

rpredictmaint::ingest_readings(sensor_data, lookup)
rpredictmaint::insert_maintenance(maintenance_history, lookup)

logger::log_info("Seed complete: {nrow(machines)} machines, {nrow(sensor_data)} readings.")
