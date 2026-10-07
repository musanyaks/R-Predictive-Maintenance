# File: api/endpoints/maintenance.R
maintenance_api <- function() {
  p <- plumber::pr()
  p <- plumber::pr_get(p, "/schedule", function() {
    p <- get_latest_predictions()
    machines_tbl <- get_machines()
    purrr::pmap_dfr(
      list(p$machine_id, p$risk_level, p$risk_score, p$rul_days),
      function(mid, lvl, score, rul) {
        recommend_maintenance(mid,
          machines_tbl$machine_name[machines_tbl$machine_id == mid],
          lvl, score, rul, list(summary = sprintf("Risk %.0f", score)))
      })
  })
  p <- plumber::pr_get(p, "/history", function(machine_ids = NULL) {
    get_maintenance_from_db(machine_ids)
  })
  apply_error_handling(apply_request_logging(apply_auth(p)))
}