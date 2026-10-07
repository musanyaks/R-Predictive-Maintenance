# File: shiny/modules/risk_gauge.R
risk_gauge_ui <- function(id) plotlyOutput(NS(id, "gauge"), height = "240px")

risk_gauge_server <- function(id, risk_value) {
  moduleServer(id, function(input, output, session) {
    output$gauge <- renderPlotly({
      v <- risk_value(); req(v)
      plot_ly(type = "indicator", mode = "gauge+number", value = v,
              gauge = list(axis = list(range = c(0, 100)),
                           steps = list(
                             list(range = c(0, 40),  color = "#e8f5e9"),
                             list(range = c(40, 65), color = "#fff9c4"),
                             list(range = c(65, 100), color = "#ffcdd2"))))
    })
  })
}