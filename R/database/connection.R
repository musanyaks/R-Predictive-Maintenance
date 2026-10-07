# File: R/database/connection.R
.rp_pool <- NULL

#' Get (and cache) a DB connection pool
get_pool <- function() {
  if (!is.null(.rp_pool) && DBI::dbIsValid(.rp_pool)) return(.rp_pool)
  cfg <- get_db_config()
  .rp_pool <<- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = cfg$host, port = cfg$port,
    dbname   = cfg$dbname, user = cfg$user,
    password = cfg$password,
    minSize  = 1, maxSize = 10
  )
  logger::log_info("DB pool created: {cfg$host}:{cfg$port}/{cfg$dbname}")
  .rp_pool
}

close_pool <- function() {
  if (!is.null(.rp_pool)) {
    pool::poolClose(.rp_pool)
    .rp_pool <<- NULL
  }
  invisible(TRUE)
}

db_ping <- function() {
  DBI::dbGetQuery(get_pool(), "SELECT 1 AS ok")$ok == 1
}