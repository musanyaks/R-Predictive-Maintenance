# File: shiny/server/monitoring_server.R
monitoringServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    mon <- reactive({
      invalidateLater(300000, session)  # 5 min
      tryCatch(run_model_monitoring(), error = function(e) NULL)
    })
    output$drift_plot <- renderPlotly({
      d <- mon()$drift
      req(d, nrow(d) > 0)
      plot_ly(d, x = ~metric, y = ~psi, type = "bar",
              color = ~drift, colors = c("#2c7fb8", "#d9534f")) |>
        layout(yaxis = list(title = "PSI"))
    })
    output$perf_table <- renderDT({
      p <- mon()$performance
      req(p, nrow(p) > 0)
      datatable(p)
    })
    output$alert_drift_table <- renderDT({
      a <- tryCatch(get_open_alerts(), error = function(e) NULL)
      req(a)
      datatable(a)
    })
  })
}