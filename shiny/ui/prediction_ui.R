# File: shiny/ui/prediction_ui.R
predictionUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Select machine", width = 3,
      selectInput(ns("machine"), "Machine",
                  choices = tryCatch(get_machines()$machine_name,
                                     error = function(e) character(0))),
      actionButton(ns("run"), "Predict now", class = "btn-primary", width = "100%")
    ),
    box(title = "Risk gauge", width = 4, plotlyOutput(ns("gauge"), height = "260px")),
    box(title = "Latest prediction", width = 5, tableOutput(ns("pred_detail"))),
    box(title = "Failure risk projection (72h)", width = 12,
        plotlyOutput(ns("risk_curve"), height = "300px"))
  )
}