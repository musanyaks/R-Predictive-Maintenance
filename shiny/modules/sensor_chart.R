# File: shiny/modules/sensor_chart.R
sensor_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sensor"), "Sensor",
                choices = c("temperature", "vibration", "pressure", "rpm")),
    plotlyOutput(ns("chart"), height = "260px")
  )
}

sensor_chart_server <- function(id, readings_reactive) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderPlotly({
      d <- readings_reactive(); req(d)
      ds <- d[d$sensor_type == input$sensor, ]
      req(nrow(ds) > 0)
      plot_ly(ds, x = ~reading_time, y = ~value, type = "scatter", mode = "lines") |>
        layout(yaxis = list(title = input$sensor))
    })
  })
}