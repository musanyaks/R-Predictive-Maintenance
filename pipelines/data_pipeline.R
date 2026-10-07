# File: pipelines/data_pipeline.R
data_pipeline_plan <- function() {
  list(
    targets::tar_target(raw_data, load_raw_data()),
    targets::tar_target(validated, {
      stop_if_invalid(validate_machines(raw_data$machines), "machines")
      # sensor CSV uses machine_name; add machine_id after machines validated
      machine_map <- clean_machines(validated$machines) |>
        dplyr::select(machine_name, machine_id)
      sensors <- raw_data$sensors |>
        dplyr::left_join(machine_map, by = "machine_name")
      stop_if_invalid(validate_readings(sensors), "sensor_data")
      maint <- raw_data$maintenance |>
        dplyr::left_join(machine_map, by = "machine_name")
      stop_if_invalid(validate_maintenance(maint), "maintenance")
      list(machines = machine_map, sensors = sensors, maintenance = maint)
    }),
    targets::tar_target(cleaned, {
      list(
        machines    = validated$machines,
        sensors     = clean_readings(validated$sensors),
        maintenance = clean_maintenance(validated$maintenance)
      )
    }),
    targets::tar_target(features_raw, compute_features(cleaned$sensors)),
    targets::tar_target(features_parquet, {
      dir.create("data/processed", showWarnings = FALSE)
      arrow::write_parquet(features_raw, "data/processed/features_train.parquet")
      features_raw
    })
  )
}