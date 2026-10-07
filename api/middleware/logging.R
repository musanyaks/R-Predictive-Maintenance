# File: api/middleware/logging.R
apply_request_logging <- function(router) {
  plumber::pr_hook(router, "postroute", function(req, res) {
    logger::log_info("API {req$REQUEST_METHOD} {req$PATH_INFO} -> {res$status}")
    NULL
  })
}