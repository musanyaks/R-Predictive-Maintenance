# File: shiny/server/anomaly_server.R
anomalyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    preds <- reactive({
      invalidateLater(60000, session)
      tryCatch(get_latest_predictions(), error = function(e) NULL)
    })
    output$anom_table <- renderDT({
      p <- preds(); req(p)
      datatable(p[order(-p$anomaly_score),
                  c("machine_id", "anomaly_score", "risk_level", "predicted_at")],
                options = list(pageLength = 10))
    })
    output$anom_scatter <- renderPlotly({
      p <- preds(); req(p, nrow(p) > 0)
      plot_ly(p, x = ~anomaly_score, y = ~risk_score, type = "scatter",
              mode = "markers", color = ~risk_level, text = ~machine_id,
              colors = c(low = "#2c7fb8", medium = "#f0ad4e",
                         high = "#d9534f", critical = "#a71d2a"))
    })
  })
}