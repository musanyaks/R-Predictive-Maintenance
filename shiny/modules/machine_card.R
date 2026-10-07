# File: shiny/modules/machine_card.R
machine_card_ui <- function(id, machine_name) {
  ns <- NS(id)
  box(title = machine_name, width = 3, status = "primary", solidHeader = TRUE,
      uiOutput(ns("content")))
}

machine_card_server <- function(id, machine_name, preds) {
  moduleServer(id, function(input, output, session) {
    machines_tbl <- tryCatch(get_machines(), error = function(e) NULL)
    output$content <- renderUI({
      p <- preds(); req(p, machines_tbl)
      id_ <- machines_tbl$machine_id[machines_tbl$machine_name == machine_name]
      row <- p[p$machine_id == id_, ]
      if (nrow(row) == 0) return(p("No data"))
      tagList(
        h4(sprintf("Risk: %.0f / 100", row$risk_score[1])),
        p(class = paste0("risk-", row$risk_level[1]),
          sprintf("%s | RUL: %.1f d | Anom: %.2f",
                  toupper(row$risk_level[1]), row$rul_days[1], row$anomaly_score[1])),
        tags$small(format(row$predicted_at[1], "%Y-%m-%d %H:%M UTC"))
      )
    })
  })
}