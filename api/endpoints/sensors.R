# File: api/endpoints/sensors.R
sensors_api <- function() {
  p <- plumber::pr()
  p <- plumber::pr_get(p, "/<machine_id:int>/readings", function(machine_id,
                                                                 hours = 24L) {
    get_readings_from_db(machine_id, since = now_utc() - lubridate::hours(hours))
  })
  p <- plumber::pr_get(p, "/<machine_id:int>/summary", function(machine_id) {
    d <- get_readings_from_db(machine_id, since = now_utc() - lubridate::hours(24))
    d |>
      dplyr::group_by(sensor_type) |>
      dplyr::summarise(mean = mean(value), sd = stats::sd(value),
                       min = min(value), max = max(value), .groups = "drop")
  })
  apply_error_handling(apply_request_logging(apply_auth(p)))
}