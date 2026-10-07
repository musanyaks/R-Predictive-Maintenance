# File: shiny/server/model_server.R
modelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$registry_table <- renderDT({
      reg <- tryCatch(get_active_models(), error = function(e) NULL)
      req(reg)
      datatable(reg[, c("model_name", "version", "model_type", "active", "trained_at")],
                options = list(pageLength = 10))
    })
    output$importance <- renderPlotly({
      m <- tryCatch(load_active_models()$failure_classifier, error = function(e) NULL)
      req(m)
      imp <- get_feature_importance(m)
      ggplotly(plot_feature_importance(imp))
    })
    output$metrics_table <- renderDT({
      reg <- tryCatch(get_active_models(), error = function(e) NULL)
      req(reg)
      f <- reg[reg$model_name == "failure_classifier", ]
      req(nrow(f) > 0)
      datatable(as.data.frame(jsonlite::fromJSON(f$metrics[1])))
    })
  })
}