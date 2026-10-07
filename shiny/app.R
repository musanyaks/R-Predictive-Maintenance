# File: shiny/app.R
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION"))
  pkgload::load_all(quiet = TRUE) else library(rpredictmaint)

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

setup_logging("shiny")

# Source modules, then ui, then server (order matters)
for (f in c(list.files("shiny/modules", full.names = TRUE),
            list.files("shiny/ui",      full.names = TRUE),
            list.files("shiny/server",  full.names = TRUE))) source(f)

ui <- dashboardPage(
  dashboardHeader(title = "Predictive Maintenance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",   tabName = "dashboard",   icon = icon("gauge-high")),
      menuItem("Monitoring",  tabName = "monitoring",  icon = icon("wave-square")),
      menuItem("Predictions", tabName = "prediction",  icon = icon("chart-line")),
      menuItem("Anomalies",   tabName = "anomaly",     icon = icon("triangle-exclamation")),
      menuItem("Models",      tabName = "model",       icon = icon("microchip")),
      menuItem("Maintenance", tabName = "maintenance", icon = icon("wrench")),
      menuItem("Reports",     tabName = "reports",     icon = icon("file-lines"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/dashboard.css")),
    tags$head(tags$script(src = "js/dashboard.js")),
    tabItems(
      tabItem(tabName = "dashboard",   dashboardUI("dashboard")),
      tabItem(tabName = "monitoring",  monitoringUI("monitoring")),
      tabItem(tabName = "prediction",  predictionUI("prediction")),
      tabItem(tabName = "anomaly",     anomalyUI("anomaly")),
      tabItem(tabName = "model",       modelUI("model")),
      tabItem(tabName = "maintenance", maintenanceUI("maintenance")),
      tabItem(tabName = "reports",     reportsUI("reports"))
    )
  )
)

server <- function(input, output, session) {
  dashboardServer("dashboard")
  monitoringServer("monitoring")
  predictionServer("prediction")
  anomalyServer("anomaly")
  modelServer("model")
  maintenanceServer("maintenance")
  reportsServer("reports")
}

shinyApp(ui, server)