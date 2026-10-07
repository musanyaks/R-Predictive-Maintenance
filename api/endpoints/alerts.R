# File: api/endpoints/alerts.R
alerts_api <- function() {
  p <- plumber::pr()
  p <- plumber::pr_get(p, "/open", function() get_open_alerts())
  p <- plumber::pr_post(p, "/<alert_id:int>/resolve", function(alert_id) {
    resolve_alert(alert_id)
    list(status = "resolved", alert_id = alert_id)
  })
  apply_error_handling(apply_request_logging(apply_auth(p)))
}