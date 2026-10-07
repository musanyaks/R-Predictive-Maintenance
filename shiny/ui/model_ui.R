# File: shiny/ui/model_ui.R
modelUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Model registry", width = 12, DTOutput(ns("registry_table"))),
    box(title = "Feature importance (active classifier)", width = 6,
        plotlyOutput(ns("importance"))),
    box(title = "Latest metrics", width = 6, DTOutput(ns("metrics_table")))
  )
}