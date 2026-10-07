# File: api/middleware/authentication.R
apply_auth <- function(router) {
  plumber::pr_filter(router, "auth", function(req, res) {
    token <- Sys.getenv("API_TOKEN", "")
    if (grepl("/health$", req$PATH_INFO)) return(plumber::forward())
    if (nchar(token) == 0) return(plumber::forward())  # dev mode: open
    auth <- req$HTTP_AUTHORIZATION %||% ""
    if (!identical(auth, paste("Bearer", token))) {
      res$status <- 401L
      return(list(error = "unauthorized"))
    }
    plumber::forward()
  })
}