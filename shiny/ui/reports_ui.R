# File: shiny/ui/reports_ui.R
reportsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Generate maintenance report", width = 6,
      dateInput(ns("report_date"), "Report date", value = Sys.Date()),
      downloadButton(ns("download_report"), "Download HTML report"),
      p("Report renders from the latest predictions + maintenance data.")
    ),
    box(title = "Recent performance snapshots", width = 6,
        tableOutput(ns("perf_files")))
  )
}