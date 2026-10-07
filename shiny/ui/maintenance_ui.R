# File: shiny/ui/maintenance_ui.R
maintenanceUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Recommended actions", width = 12, DTOutput(ns("reco_table"))),
    box(title = "Maintenance history", width = 12, DTOutput(ns("hist_table")))
  )
}