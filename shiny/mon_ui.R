## File: shiny/mon_ui.R — Live Monitoring tab (mock layout)
mon_tab_ui <- function() {
  head <- div(class = "mon-head",
    div(h2("Live Monitoring"),
        div(class = "sub", "Real-time equipment health and sensor telemetry")),
    div(class = "hspacer"),
    div(class = "live-badge",
        div(class = "top", span(class = "pulse"), "LIVE - Streaming"),
        div(class = "sub", uiOutput("mon_updated", inline = TRUE))))

  tel_card <- div(class = "card", style = "grid-column:span 8",
    div(class = "card-head",
        div(style = "display:flex;align-items:center;gap:9px",
            icon("wave-square", style = "color:#2563eb"),
            uiOutput("mon_tel_title", inline = TRUE)),
        uiOutput("mon_legend")),
    div(class = "select-wrap",
        icon("industry", class = "sel-ic"),
        selectInput("mon_machine", NULL, choices = NULL, width = "100%")),
    fluidRow(
      column(9, plotlyOutput("mon_chart", height = "260px")),
      column(3, div(class = "legend-side", uiOutput("mon_side")))))

  det_card <- div(class = "card", style = "grid-column:span 4",
    div(class = "card-head", h3("Machine Details"), uiOutput("mon_badge")),
    uiOutput("mon_details"))

  grid1 <- div(class = "grid12", tel_card, det_card)

  grid2 <- div(class = "grid12",
    div(style = "grid-column:span 8;display:grid;grid-template-columns:repeat(12,1fr);gap:14px;align-items:start",
        uiOutput("mon_minis")),
    div(class = "card", style = "grid-column:span 4",
        div(class = "card-head", h3("Machine Status"),
            actionLink("mon_view_all", "View All", class = "link-sm")),
        uiOutput("mon_status_table")))

  grid3 <- div(class = "grid12",
    div(class = "card", style = "grid-column:span 8",
        div(class = "card-head",
            div(style = "display:flex;align-items:center;gap:9px",
                icon("bell", style = "color:#e0a800"),
                h3("Live Alerts"),
                uiOutput("mon_alert_count", inline = TRUE)),
            actionLink("mon_view_alerts", "View All Alerts", class = "link-sm")),
        uiOutput("mon_alerts_table")),
    div(class = "card", style = "grid-column:span 4", uiOutput("mon_banner")))

  tagList(head, uiOutput("mon_kpis"), tags$br(), grid1,
          tags$br(), grid2, tags$br(), grid3)
}
