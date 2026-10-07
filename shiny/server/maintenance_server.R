# File: shiny/server/maintenance_server.R
maintenanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    recos <- reactive({
      invalidateLater(120000, session)
      p <- tryCatch(get_latest_predictions(), error = function(e) NULL)
      req(p, nrow(p) > 0)
      mt <- tryCatch(get_machines(), error = function(e) NULL)
      req(mt)
      purrr::pmap_dfr(
        list(p$machine_id,
             mt$machine_name[match(p$machine_id, mt$machine_id)],
             p$risk_level, p$risk_score, p$rul_days),
        function(mid, mname, lvl, score, rul) {
          recommend_maintenance(mid, mname, lvl, score, rul,
                                list(summary = sprintf("Risk %.0f (%s)", score, lvl)))
        })
    })
    output$reco_table <- renderDT({
      r <- recos(); req(r)
      datatable(r[, c("machine_name", "action", "priority", "due_by", "rationale")],
                options = list(pageLength = 10))
    })
    output$hist_table <- renderDT({
      h <- tryCatch(get_maintenance_from_db(), error = function(e) NULL)
      req(h)
      datatable(h, options = list(pageLength = 10))
    })
  })
}