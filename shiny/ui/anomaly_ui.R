# File: shiny/ui/anomaly_ui.R
anomalyUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Latest anomaly scores", width = 7, DTOutput(ns("anom_table"))),
    box(title = "Anomaly vs risk", width = 5,
        plotlyOutput(ns("anom_scatter"), height = "320px"))
  )
}