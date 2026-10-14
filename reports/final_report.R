#' ---
#' title: "Predictive Maintenance Report"
#' output: html_document
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, error = TRUE)
root <- tryCatch(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
                 error = function(e) NULL)
ok <- FALSE
if (!is.null(root)) {
  ok <- tryCatch({ pkgload::load_all(root, quiet = TRUE); TRUE },
                 error = function(e) FALSE)
}
if (!ok) ok <- tryCatch({ library(rpredictmaint); TRUE }, error = function(e) FALSE)
preds <- tryCatch(get_latest_predictions(), error = function(e) NULL)
mach  <- tryCatch(get_machines(), error = function(e) NULL)
maint <- tryCatch(get_maintenance_from_db(), error = function(e) NULL)
has <- ok && !is.null(preds) && nrow(preds) > 0 && !is.null(mach)

#' # Executive summary
#'
#' Report date: `r format(Sys.Date(), "%Y-%m-%d")`

#+ summary
if (!ok) {
  cat("Package could not load in the render environment.")
} else if (!has) {
  cat("No prediction data yet. Run: make train, then run_batch_predictions().")
} else {
  out <- preds |> dplyr::left_join(mach[c("machine_id", "machine_name")],
                                   by = "machine_id")
  knitr::kable(out[, c("machine_name", "failure_prob", "rul_days",
                       "risk_score", "risk_level")], digits = 3)
}

#' # Risk distribution

#+ riskdist
if (has) barplot(table(preds$risk_level),
                 col = c("#1a9e55", "#f0b429", "#e08a00", "#d43a3a"),
                 main = "Machines by risk level") else cat("No data.")

#' # Recent maintenance

#+ maint
if (!is.null(maint) && nrow(maint) > 0) {
  m2 <- maint[order(maint$performed_at, decreasing = TRUE), ]
  knitr::kable(utils::head(m2[, c("machine_id", "maintenance_type",
                                  "performed_at")], 10))
} else cat("No maintenance history available.")
