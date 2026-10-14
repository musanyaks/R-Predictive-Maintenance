## File: shiny/pred_ui.R - Predictions tab (mock layout)
pred_tab_ui <- function() {
  head <- div(class = "mon-head",
    div(h2("Predictions"),
        div(class = "sub",
            "AI-powered predictions for failure probability, remaining useful life and maintenance recommendations")),
    div(class = "hspacer"),
    span(class = "sys-online", span(class = "dot"), "System Online"))

  main_card <- div(class = "card", style = "grid-column:span 6",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("chart-line", style = "color:#2563eb"),
            div(h3("Machine Failure Prediction"),
                div(class = "sub",
                    style = "font-size:10.5px;color:#7b8aa5",
                    "Predicted probability of failure within next 30 days"))),
        div(style = "display:flex;gap:8px;align-items:center",
            selectInput("pred_filter", NULL,
                        choices = c("All Machines", "High Risk",
                                    "Medium Risk", "Low Risk"),
                        width = "140px"),
            span(class = "live-tag", "Next 30 Days"))),
    uiOutput("pred_main_table"))

  sel_card <- div(class = "card", style = "grid-column:span 3",
    div(class = "card-head", h3("Selected Machine"),
        uiOutput("pred_sel_badge")),
    div(class = "select-wrap",
        icon("industry", class = "sel-ic"),
        selectInput("pred_machine", NULL, choices = NULL, width = "100%")),
    uiOutput("pred_sel_photo"),
    uiOutput("pred_sel_head"),
    uiOutput("pred_sel_chips"))

  alerts_card <- div(class = "card", style = "grid-column:span 3",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:8px",
            icon("bell", style = "color:#d43a3a"),
            h3("Recent Alerts")),
        actionLink("pred_view_alerts", "View All", class = "link-sm")),
    uiOutput("pred_alerts"))

  trend_card <- div(class = "card", style = "grid-column:span 5",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("chart-line", style = "color:#1a9e55"),
            h3("Failure Probability Trend")),
        span(class = "mini", "from prediction history")),
    plotlyOutput("pred_trend", height = "250px"))

  rul_card <- div(class = "card", style = "grid-column:span 4",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("clock", style = "color:#2563eb"),
            h3("Remaining Useful Life (RUL)"))),
    uiOutput("pred_rul_table"))

  reco_card <- div(class = "card", style = "grid-column:span 3",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("screwdriver-wrench", style = "color:#e08a00"),
            h3("Recommendations")),
        actionLink("pred_view_recos", "View All", class = "link-sm")),
    uiOutput("pred_recos"))

  perf_card <- div(class = "card", style = "grid-column:span 5",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("chart-simple", style = "color:#7c5cff"),
            h3("Model Performance"))),
    uiOutput("perf_table"))

  dist_card <- div(class = "card", style = "grid-column:span 4",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("chart-pie", style = "color:#1a9e55"),
            h3("Prediction Distribution"))),
    div(style = "display:flex;gap:12px;align-items:center",
        div(style = "width:150px;flex-shrink:0",
            plotlyOutput("pred_dist_donut", height = "150px",
                         width = "150px")),
        div(style = "flex:1", uiOutput("pred_dist_legend"))))

  qa_card <- div(class = "card", style = "grid-column:span 3",
    div(class = "card-head", h3("Quick Actions")),
    actionButton("pred_qa_refresh", "Refresh Predictions",
                 icon = icon("rotate"), class = "qa-btn blue"),
    downloadButton("qa_csv", "Export to CSV",
                   class = "qa-btn purple",
                   style = "color:#fff;border:0;display:flex"),
    actionButton("qa_report", "Download Report",
                 icon = icon("download"), class = "qa-btn green"),
    actionButton("qa_alerts", "Configure Alerts",
                 icon = icon("bell"), class = "qa-btn teal"))

  grid1 <- div(class = "grid12", main_card, sel_card, alerts_card)
  grid2 <- div(class = "grid12", trend_card, rul_card, reco_card)
  grid3 <- div(class = "grid12", perf_card, dist_card, qa_card)

  tagList(head, uiOutput("pred_kpis"), tags$br(), grid1,
          tags$br(), grid2, tags$br(), grid3)
}
