# File: shiny/server/reports_server.R
reportsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$perf_files <- renderTable({
      dir <- file.path(project_root(), "reports", "model_performance")
      files <- list.files(dir, full.names = FALSE)
      if (length(files) == 0) data.frame(files = "none yet") else data.frame(files)
    })
    output$download_report <- downloadHandler(
      filename = function() sprintf("maintenance_report_%s.html", input$report_date),
      content = function(file) {
        rmarkdown::render(
          file.path(project_root(), "reports", "final_report.Rmd"),
          params = list(report_date = input$report_date),
          output_file = file
        )
      }
    )
  })
}