# File: api/endpoints/machines.R
machines_api <- function() {
  p <- plumber::pr()
  p <- plumber::pr_get(p, "/", function() get_machines())
  p <- plumber::pr_get(p, "/<machine_id:int>", function(machine_id) {
    m <- get_machines(active_only = FALSE)
    row <- m[m$machine_id == machine_id, ]
    if (nrow(row) == 0) plumber::stop(404, "machine not found")
    row
  })
  apply_error_handling(apply_request_logging(apply_auth(p)))
}