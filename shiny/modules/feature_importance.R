# File: shiny/modules/feature_importance.R
feature_importance_ui <- function(id) plotlyOutput(NS(id, "imp"))

feature_importance_server <- function(id, model_reactive, top_n = 10) {
  moduleServer(id, function(input, output, session) {
    output$imp <- renderPlotly({
      m <- model_reactive(); req(m)
      imp <- get_feature_importance(m) |> dplyr::slice_max(gain, n = top_n)
      plot_ly(imp, x = ~gain, y = ~reorder(feature, gain),
              type = "bar", orientation = "h") |>
        layout(yaxis = list(title = NULL))
    })
  })
}