# File: R/database_connection.R

# Mutable package-level state container.
# (Package namespaces are LOCKED after install — `.rp_pool <<-` is illegal there.
#  Assigning INTO a normal environment created at load time is always allowed.)
.rp_state <- new.env(parent = emptyenv())

#' Get (and cache) a DB connection pool
get_pool <- function() {
  if (!is.null(.rp_state$pool) && DBI::dbIsValid(.rp_state$pool)) {
    return(.rp_state$pool)
  }
  cfg <- get_db_config()
  .rp_state$pool <- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = cfg$host, port = cfg$port,
    dbname   = cfg$dbname, user = cfg$user,
    password = cfg$password,
    minSize  = 1, maxSize = 10
  )
  logger::log_info("DB pool created: {cfg$host}:{cfg$port}/{cfg$dbname}")
  .rp_state$pool
}

close_pool <- function() {
  if (!is.null(.rp_state$pool)) {
    pool::poolClose(.rp_state$pool)
    .rp_state$pool <- NULL
  }
  invisible(TRUE)
}

db_ping <- function() {
  DBI::dbGetQuery(get_pool(), "SELECT 1 AS ok")$ok == 1
}
