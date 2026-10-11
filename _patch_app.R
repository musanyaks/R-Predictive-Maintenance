x <- readLines("shiny/app.R")

# Fix 1: define small() (htmltools has no small() wrapper)
if (!any(grepl("small <- function", x))) {
  i <- grep("^library\\(DT\\)", x)[1]
  stopifnot(!is.na(i))
  x <- append(x, "small <- function(...) tags$small(...)  # htmltools alias",
              after = i)
}

# Fix 2: sensor legend — key colors by lowercase value (st), not name (nm)
x <- gsub("sensor_colors[[nm]]", "sensor_colors[[st]]", x, fixed = TRUE)

# Fix 3: insights — [[ indexing (fontawesome needs character, not list)
x <- gsub('paste0("insight ", i[1])', 'paste0("insight ", i[[1]])', x, fixed = TRUE)
x <- gsub("icon(i[2])",  "icon(i[[2]])",  x, fixed = TRUE)
x <- gsub("span(i[3])",  "span(i[[3]])",  x, fixed = TRUE)

writeLines(x, "shiny/app.R")

# Verify — hard-fail the block if any patch is missing
y <- readLines("shiny/app.R")
ok1 <- any(grepl("small <- function", y))
ok2 <- any(grepl("sensor_colors[[st]]", y, fixed = TRUE))
ok3 <- any(grepl("icon(i[[2]])", y, fixed = TRUE))
cat("PATCH small:   ", ok1, "\nPATCH legend:  ", ok2,
    "\nPATCH insights:", ok3, "\n")
if (!all(ok1, ok2, ok3)) quit(status = 1)
