# File: shiny/ui/dashboard_ui.R
dashboardUI <- function(id) {
  ns <- NS(id)
  machines <- tryCatch(get_machines()$machine_name, error = function(e) character(0))
  fluidRow(
    uiOutput(ns("kpi_boxes")),
    purrr::map(machines, ~ machine_card_ui(
      ns(paste0("card_", gsub("\\W", "", .x))), machine_name = .x))
  )
}