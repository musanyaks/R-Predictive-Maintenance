# File: shiny/modules/model_metrics.R
model_metrics_ui <- function(id) DTOutput(NS(id, "metrics"))

model_metrics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$metrics <- renderDT({
      reg <- tryCatch(get_active_models(), error = function(e) NULL)
      req(reg, nrow(reg) > 0)
      f <- reg[reg$model_name == "failure_classifier", ]
      req(nrow(f) > 0)
      datatable(as.data.frame(jsonlite::fromJSON(f$metrics[1])))
    })
  })
}