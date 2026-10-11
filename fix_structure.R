# File: fix_structure.R — flatten R/ subdirectories into top-level files
# Run from repo root:  Rscript fix_structure.R

files <- list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = FALSE)
stopifnot(length(files) > 0)

for (f in files) {
  parts <- strsplit(f, "/")[[1]]
  if (length(parts) == 1) next   # already top-level
  newname <- paste0(paste(parts[-length(parts)], collapse = "_"),
                    "_", parts[length(parts)])
  ok <- file.rename(file.path("R", f), file.path("R", newname))
  if (is.na(ok) || !ok) {        # fallback: copy+delete
    file.copy(file.path("R", f), file.path("R", newname), overwrite = TRUE)
    unlink(file.path("R", f))
  }
  cat(sprintf("  %-40s -> %s\n", f, newname))
}

# Remove now-empty subdirectories
for (d in setdiff(list.dirs("R", recursive = TRUE), "R"))
  if (length(list.files(d)) == 0) unlink(d, recursive = TRUE, force = TRUE)

# Make NAMESPACE unambiguous
writeLines('exportPattern("^[[:alpha:]]+")', "NAMESPACE")

cat("\nFlattened", length(files), "files. R/ now contains:\n")
print(sort(list.files("R", pattern = "\\.[Rr]$")))