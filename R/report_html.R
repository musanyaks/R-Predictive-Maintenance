#' Build the maintenance report as pure HTML (no pandoc/knitr needed)
build_report_html <- function(out_file) {
  preds <- tryCatch(get_latest_predictions(), error = function(e) NULL)
  mach  <- tryCatch(get_machines(), error = function(e) NULL)
  maint <- tryCatch(get_maintenance_from_db(), error = function(e) NULL)

  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    gsub("<", "&lt;", x, fixed = TRUE)
  }

  rows <- ""
  if (!is.null(preds) && nrow(preds) > 0 && !is.null(mach)) {
    d <- merge(preds, mach[, c("machine_id", "machine_name")],
               by = "machine_id")
    d <- d[order(-d$risk_score), ]
    for (i in seq_len(nrow(d))) {
      lvl <- d$risk_level[i]
      col <- switch(lvl, critical = "#d43a3a", high = "#e08a00",
                    medium = "#e0a800", low = "#1a9e55", "#666666")
      rul <- if (is.na(d$rul_days[i]) || d$rul_days[i] > 999) "30+"
             else as.character(round(d$rul_days[i], 1))
      rows <- paste0(rows,
        "<tr><td>", esc(d$machine_name[i]),
        "</td><td style='text-align:right'>",
        round(100 * d$failure_prob[i], 1), "%",
        "</td><td style='text-align:right'>", rul,
        "</td><td style='text-align:right'>",
        round(d$risk_score[i], 1),
        "</td><td style='color:", col, ";font-weight:bold'>",
        toupper(lvl), "</td></tr>")
    }
  }

  mrows <- ""
  if (!is.null(maint) && nrow(maint) > 0) {
    m2 <- maint[order(maint$performed_at, decreasing = TRUE), ]
    for (i in seq_len(min(10, nrow(m2)))) {
      mrows <- paste0(mrows,
        "<tr><td>", m2$machine_id[i],
        "</td><td>", esc(m2$maintenance_type[i]),
        "</td><td>", format(m2$performed_at[i], "%Y-%m-%d %H:%M"),
        "</td></tr>")
    }
  }

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<title>Predictive Maintenance Report</title><style>",
    "body{font-family:'Segoe UI',Arial,sans-serif;margin:32px;color:#1c2b46}",
    "h1{color:#12295c}h2{border-bottom:2px solid #e4eaf3;",
    "padding-bottom:6px;margin-top:32px}",
    "table{border-collapse:collapse;width:100%;margin-top:12px}",
    "th{background:#f0f4fa;text-align:left;padding:9px 12px;font-size:13px}",
    "td{padding:9px 12px;border-bottom:1px solid #eef2f9;font-size:13px}",
    ".meta{color:#7b8aa5}</style></head><body>",
    "<h1>Predictive Maintenance Report</h1>",
    "<p class='meta'>Generated: ",
    format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>",
    "<h2>Executive summary</h2>")
  if (nchar(rows) > 0) {
    html <- paste0(html,
      "<table><tr><th>Machine</th><th>Failure prob</th>",
      "<th>RUL (days)</th><th>Risk score</th><th>Level</th></tr>",
      rows, "</table>")
  } else {
    html <- paste0(html, "<p>No prediction data yet.</p>")
  }
  html <- paste0(html, "<h2>Recent maintenance</h2>")
  if (nchar(mrows) > 0) {
    html <- paste0(html,
      "<table><tr><th>Machine ID</th><th>Type</th><th>Performed</th></tr>",
      mrows, "</table>")
  } else {
    html <- paste0(html, "<p>No maintenance history available.</p>")
  }
  html <- paste0(html, "</body></html>")

  con <- file(out_file, open = "wb")
  writeLines(html, con, useBytes = TRUE)
  close(con)
  invisible(out_file)
}
