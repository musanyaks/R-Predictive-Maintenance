# File: shiny/server/prediction_server.R
predictionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    models <- reactiveVal(NULL)
    observeEvent(input$run, {
      models(tryCatch(load_active_models(), error = function(e) NULL))
      showNotification("Models loaded", type = "message")
    })

    preds <- reactive({
      invalidateLater(60000, session)
      tryCatch(get_latest_predictions(), error = function(e) NULL)
    })
    machines_tbl <- reactive(tryCatch(get_machines(), error = function(e) NULL))

    machine_id_of <- function(name) {
      mt <- machines_tbl()
      req(mt, name)
      mt$machine_id[mt$machine_name == name]
    }

    output$gauge <- renderPlotly({
      p <- preds(); req(p, input$machine)
      id <- machine_id_of(input$machine)
      row <- p[p$machine_id == id, ]
      req(nrow(row) > 0)
      plot_ly(type = "indicator", mode = "gauge+number", value = row$risk_score[1],
              gauge = list(axis = list(range = c(0, 100)),
                           bar = list(color = "darkblue"),
                           steps = list(
                             list(range = c(0, 40),  color = "#e8f5e9"),
                             list(range = c(40, 65), color = "#fff9c4"),
                             list(range = c(65, 85), color = "#ffe0b2"),
                             list(range = c(85, 100), color = "#ffcdd2"))))
    })
    output$pred_detail <- renderTable({
      p <- preds(); req(p, input$machine)
      id <- machine_id_of(input$machine)
      row <- p[p$machine_id == id, ]
      req(nrow(row) > 0)
      data.frame(
        Metric = c("Failure prob", "RUL (days)", "Anomaly", "Risk level"),
        Value  = c(sprintf("%.0f%%", 100 * row$failure_prob[1]),
                   sprintf("%.1f", row$rul_days[1]),
                   sprintf("%.2f", row$anomaly_score[1]),
                   row$risk_level[1]))
    }, width = "100%")
    output$risk_curve <- renderPlotly({
      m <- models(); req(m, input$machine)
      id <- machine_id_of(input$machine)
      feats <- tryCatch(compute_features(get_readings_from_db(
        machine_ids = id, since = now_utc() - lubridate::hours(96))),
        error = function(e) NULL)
      req(feats, nrow(feats) > 0)
      fc <- tryCatch(forecast_failure_risk(feats, m$failure_classifier,
        grep("^(value_|roll_|lag|delta|dev_|range_|hour_|day_)",
             names(feats), value = TRUE)),
        error = function(e) NULL)
      req(fc, nrow(fc) > 0)
      plot_ly(fc, x = ~day, y = ~failure_prob * 100,
              type = "scatter", mode = "lines+markers", name = "Projected risk") |>
        layout(xaxis = list(title = "Days ahead"),
               yaxis = list(title = "Failure prob (%)"))
    })
  })
}