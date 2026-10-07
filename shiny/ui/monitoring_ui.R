# File: shiny/ui/monitoring_ui.R
monitoringUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Prediction drift (PSI by metric)", width = 12,
        plotlyOutput(ns("drift_plot"), height = "300px")),
    box(title = "Backtest performance", width = 6, DTOutput(ns("perf_table"))),
    box(title = "Monitoring alerts", width = 6, DTOutput(ns("alert_drift_table")))
  )
}