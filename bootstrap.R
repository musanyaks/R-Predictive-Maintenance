# File: bootstrap.R — Windows-safe, run from the repo root:
#   Rscript bootstrap.R 2>&1 | tee bootstrap.log

options(
  repos  = c(CRAN = "https://cloud.r-project.org"),
  pkgType = "binary"   # Windows: never build from source when a binary exists
)

# 0. Clear stale install locks from any previously interrupted install
lib <- .libPaths()[1]
locks <- list.files(lib, pattern = "^00LOCK", full.names = TRUE)
if (length(locks) > 0) {
  message("Removing stale locks: ", paste(basename(locks), collapse = ", "))
  unlink(locks, recursive = TRUE, force = TRUE)
}

# 1. Generate NAMESPACE (no formal docs yet)
if (!file.exists("NAMESPACE"))
  writeLines('exportPattern("^[[:alpha:]]+")', "NAMESPACE")

# 2. Install everything in DESCRIPTION
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_deps(dependencies = TRUE)

# 3. Dev tools + the project package itself
install.packages(c("pkgload", "testthat"))
install.packages(".", repos = NULL, type = "source")

cat("\nDONE. Verify with:  Rscript -e 'rpredictmaint::db_ping()'\n")
# 1. Generate NAMESPACE — ALWAYS overwrite (never trust a stale one)
writeLines('exportPattern("^[[:alpha:]]+")', "NAMESPACE")