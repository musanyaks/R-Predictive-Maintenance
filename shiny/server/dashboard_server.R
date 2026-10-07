# File: shiny/server/dashboard_server.R
dashboardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    preds <- reactive({
      invalidateLater(60000, session)
      tryCatch(get_latest_predictions(), error = function(e) NULL)
    })
    output$kpi_boxes <- renderUI({
      p <- preds()
      if (is.null(p) || nrow(p) == 0)
        return(h4("No predictions yet — run the training pipeline."))
      fluidRow(
        valueBox(sum(p$risk_level == "critical"), "Critical",
                 color = "red", icon = icon("fire"), width = 3),
        valueBox(sum(p$risk_level == "high"), "High",
                 color = "orange", icon = icon("triangle-exclamation"), width = 3),
        valueBox(sum(p$risk_level == "medium"), "Medium",
                 color = "yellow", icon = icon("eye"), width = 3),
        valueBox(nrow(p), "Machines monitored",
                 color = "blue", icon = icon("robot"), width = 3)
      )
    })
    machines <- tryCatch(get_machines()$machine_name, error = function(e) character(0))
    for (m in machines) {
      machine_card_server(paste0("card_", gsub("\\W", "", m)),
                          machine_name = m, preds = preds)
    }
  })
}