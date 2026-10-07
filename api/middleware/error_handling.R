# File: api/middleware/error_handling.R
apply_error_handling <- function(router) {
  plumber::pr_set_error(router, function(req, res, err) {
    logger::log_error("API error on {req$PATH_INFO}: {conditionMessage(err)}")
    res$status <- 500L
    list(error = "internal_error", message = conditionMessage(err))
  })
}