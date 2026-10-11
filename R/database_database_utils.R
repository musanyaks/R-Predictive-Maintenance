# File: R/database_database_utils.R

#' Run all migration files in database/tables/ (idempotent).
#' RPostgres prepared statements allow ONE command per call, so each file is
#' split into individual statements (comments stripped) and run separately.
run_migrations <- function(dir = file.path(project_root(), "database", "tables")) {
  files <- sort(list.files(dir, pattern = "\\.sql$", full.names = TRUE))
  if (length(files) == 0) stop("No .sql migration files found in ", dir)
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  for (f in files) {
    sql <- paste(readLines(f, warn = FALSE), collapse = "\n")
    sql <- gsub("--[^\n]*", "", sql)                       # strip line comments
    stmts <- trimws(strsplit(sql, ";", fixed = TRUE)[[1]]) # split statements
    stmts <- stmts[nzchar(stmts)]
    for (s in stmts) DBI::dbExecute(con, s)
    logger::log_info("Migration applied: {basename(f)} ({length(stmts)} statements)")
  }
  invisible(files)
}

table_exists <- function(table) {
  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbExistsTable(con, table)
}
