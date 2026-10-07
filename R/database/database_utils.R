# File: R/database/database_utils.R
#' Run all migration files in database/tables/ (idempotent)
run_migrations <- function(dir = file.path(project_root(), "database", "tables")) {
  files <- sort(list.files(dir, pattern = "\\.sql$", full.names = TRUE))
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  for (f in files) {
    sql <- paste(readLines(f, warn = FALSE), collapse = "\n")
    DBI::dbExecute(con, sql)
    logger::log_info("Migration applied: {basename(f)}")
  }
  invisible(files)
}

table_exists <- function(table) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbExistsTable(con, table)
}