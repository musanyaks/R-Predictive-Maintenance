## File: shiny/pred_server.R - Predictions tab server (canonical)
pred_server_init <- function(input, output, session, ctx) {
  fleet     <- ctx$fleet
  scored    <- ctx$scored
  alerts_df <- ctx$alerts_df
  go_tab    <- ctx$current_tab

  risk3 <- function(rl) {
    dplyr::case_when(
      rl %in% c("high", "critical") ~ "high",
      rl == "medium" ~ "medium",
      TRUE ~ "low")
  }

  pred_hist <- reactive({
    invalidateLater(120000)
    tryCatch(
      DBI::dbGetQuery(get_pool(), paste(
        "SELECT machine_id, failure_prob, predicted_at",
        "FROM predictions ORDER BY predicted_at")),
      error = function(e) NULL)
  })

  observe({
    x <- scored()
    if (is.null(x) || nrow(x) == 0) return()
    ch <- setNames(x$machine_id,
                   paste0(x$disp_name, " (", x$type_lab, ")"))
    cur <- isolate(input$pred_machine)
    updateSelectInput(session, "pred_machine", choices = ch,
      selected = if (!is.null(cur) && cur %in% unname(ch)) cur
                 else unname(ch)[1])
  })

  pred_sel <- reactive({
    x <- scored()
    req(nrow(x) > 0, input$pred_machine)
    x[x$machine_id == input$pred_machine, ][1, ]
  })

  output$pred_kpis <- renderUI({
    x <- scored()
    req(nrow(x) > 0)
    r3 <- risk3(x$risk_level)
    n_h <- sum(r3 == "high")
    n_m <- sum(r3 == "medium")
    n_l <- sum(r3 == "low")
    m <- fleet()$maint
    cost <- 0
    if (!is.null(m) && nrow(m) > 0) {
      mm <- m[m$performed_at >= (now_utc() - lubridate::days(30)), ]
      if (nrow(mm) > 0) {
        cost <- round(sum(mm$downtime_hours, na.rm = TRUE) * 6500)
      }
    }
    pct <- function(k) paste0(round(100 * k / nrow(x)), "% of fleet")
    big <- function(k) {
      HTML(paste0(k, " <span style='font-size:14px;color:#7b8aa5'>/ ",
                  nrow(x), "</span>"))
    }
    div(class = "grid12",
      kpi_card("span3", "triangle-exclamation", "var(--red)",
               "#fef1f1", "High Risk Machines", big(n_h),
               pct(n_h), "arrow-trend-up"),
      kpi_card("span3", "triangle-exclamation", "var(--amber)",
               "#fffaef", "Medium Risk Machines", big(n_m),
               pct(n_m), "arrow-trend-up"),
      kpi_card("span3", "circle-check", "var(--green)",
               "#f0fbf4", "Low Risk Machines", big(n_l),
               pct(n_l), "arrow-trend-down"),
      kpi_card("span3", "screwdriver-wrench", "var(--purple)",
               "#f6f2ff", "Avg. Maintenance Cost",
               paste0("KSh ", format(cost, big.mark = ",")),
               "last 30 days (downtime-based)", "arrow-trend-down"))
  })

  output$pred_main_table <- renderUI({
    x <- scored()
    req(nrow(x) > 0)
    x$r3 <- risk3(x$risk_level)
    f <- input$pred_filter
    if (!is.null(f) && f != "All Machines") {
      want <- c("High Risk" = "high", "Medium Risk" = "medium",
                "Low Risk" = "low")[[f]]
      x <- x[x$r3 == want, ]
    }
    x <- x[order(-x$failure_prob), ]
    if (nrow(x) == 0) return(empty_card("No machines in this risk band"))
    rows <- lapply(seq_len(nrow(x)), function(i) {
      r <- x[i, ]
      fp <- 100 * r$failure_prob
      fpcol <- if (fp >= 65) "#d43a3a" else
               if (fp >= 40) "#e08a00" else "#1a9e55"
      tic <- if (fp >= 65) "arrow-trend-up" else
             if (fp >= 40) "arrow-right" else "arrow-trend-down"
      tcol <- fpcol
      src <- machine_img(r$machine_name)
      thumb <- if (file.exists(file.path(project_root(), "shiny", "www", src))) {
        tags$img(src = src,
                 style = paste0("width:26px;height:26px;object-fit:cover;",
                                "border-radius:6px;vertical-align:middle"))
      } else {
        tags$span(class = "thumb thumb-fallback", icon("industry"))
      }
      rul_lab <- if (is.na(r$rul_days) || r$rul_days > 999) "30+"
                 else round(r$rul_days)
      pill <- switch(r$r3, high = "critical",
                     medium = "medium", low = "online")
      plab <- switch(r$r3, high = "High",
                     medium = "Medium", low = "Low")
      tags$tr(
        tags$td(thumb),
        tags$td(class = "mini", r$machine_name),
        tags$td(class = "mname", r$disp_name),
        tags$td(pretty_type(r$machine_type)),
        tags$td(style = paste0("color:", fpcol,
                               ";font-weight:800"),
                paste0(round(fp, 1), "%")),
        tags$td(style = "font-weight:700", rul_lab),
        tags$td(span(class = paste0("pill ", pill), plab)),
        tags$td(icon(tic, style = paste0("color:", tcol))))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("", "Machine ID", "Machine Name",
                                 "Type", "Failure Probability",
                                 "RUL (days)", "Risk Level", "Trend"),
                                tags$th))),
      tags$tbody(rows))
  })

  output$pred_sel_badge <- renderUI({
    s <- pred_sel()
    req(s)
    r3 <- risk3(s$risk_level)
    if (r3 == "high") {
      span(class = "badge-critical", "High Risk")
    } else if (r3 == "medium") {
      span(class = "badge-warn", "Medium Risk")
    } else {
      span(class = "badge-run", "Low Risk")
    }
  })

  output$pred_sel_photo <- renderUI({
    s <- pred_sel()
    req(s)
    src <- machine_img(s$machine_name)
    if (file.exists(file.path(project_root(), "shiny", "www", src))) {
      tags$img(src = src, alt = s$machine_name,
               style = paste0("width:100%;height:150px;object-fit:cover;",
                              "border-radius:12px;display:block;",
                              "margin-bottom:10px"))
    } else {
      div(class = "photo", icon("industry"))
    }
  })

  output$pred_sel_head <- renderUI({
    s <- pred_sel()
    req(s)
    r3 <- risk3(s$risk_level)
    pill <- switch(r3, high = "critical",
                   medium = "medium", low = "online")
    plab <- switch(r3, high = "High Risk",
                   medium = "Medium Risk", low = "Low Risk")
    tagList(
      div(style = paste0("display:flex;align-items:center;",
                         "justify-content:space-between;gap:8px"),
          div(style = "font-weight:800;font-size:15px",
              s$disp_name),
          span(class = paste0("pill ", pill), plab)),
      div(class = "mini",
          paste0("Failure probability: ",
                 round(100 * s$failure_prob, 1), "%")))
  })

  output$pred_sel_chips <- renderUI({
    s <- pred_sel()
    req(s)
    tags$div(class = "chips",
      div(class = "chip", "Type", tags$b(s$type_lab)),
      div(class = "chip", "Location",
          tags$b(s$location %||% "---")),
      div(class = "chip", "Model",
          tags$b(paste0("PM-", sprintf("%03d", s$machine_id)))))
  })

  output$pred_alerts <- renderUI({
    a <- alerts_df()
    if (nrow(a) == 0) return(empty_card("No open alerts - all clear."))
    lapply(seq_len(min(5, nrow(a))), function(i) {
      r <- a[i, ]
      am <- alert_meta[[r$alert_level]] %||% alert_meta$info
      div(class = "alert-row",
        div(class = "alert-ic",
            style = paste0("background:", am$col), icon(am$ic)),
        div(style = "flex:1",
            div(class = "t", r$machine_name),
            div(class = "m", r$message)),
        div(class = "when", time_ago(r$created_at),
            tags$br(),
            span(class = paste0("pill ", am$pill),
                 toupper(r$alert_level))))
    })
  })

  output$pred_trend <- renderPlotly({
    h <- pred_hist()
    req(h, nrow(h) > 0)
    x <- scored()
    req(nrow(x) > 0)
    top <- x[order(-x$failure_prob), ][1:min(4, nrow(x)), ]
    cols <- c("#d43a3a", "#f08c1a", "#2563eb", "#1a9e55")
    p <- plot_ly()
    for (k in seq_len(nrow(top))) {
      hh <- h[h$machine_id == top$machine_id[k], ]
      if (nrow(hh) == 0) next
      hh <- hh[order(hh$predicted_at), ]
      p <- plotly::add_trace(p,
        x = hh$predicted_at, y = 100 * hh$failure_prob,
        type = "scatter", mode = "lines+markers",
        line = list(color = cols[k], width = 2.2),
        marker = list(size = 6, color = cols[k]),
        name = paste0(top$disp_name[k], " (",
                      round(100 * top$failure_prob[k], 1), "%)"))
    }
    p |>
      layout(margin = list(20, 10, 10, 40),
             paper_bgcolor = "transparent",
             plot_bgcolor = "transparent",
             legend = list(font = list(size = 10)),
             xaxis = list(title = ""),
             yaxis = list(title = "Probability (%)",
                          gridcolor = "#eef2f9",
                          range = c(0, 100))) |>
      config(displayModeBar = FALSE)
  })

  output$pred_rul_table <- renderUI({
    x <- scored()
    req(nrow(x) > 0)
    x$r3 <- risk3(x$risk_level)
    xx <- x[order(x$rul_days), ][1:min(5, nrow(x)), ]
    rows <- lapply(seq_len(nrow(xx)), function(i) {
      r <- xx[i, ]
      rul <- if (is.na(r$rul_days) || r$rul_days > 999) 30
             else r$rul_days
      frac <- pmin(rul / 30, 1)
      barcol <- if (r$r3 == "high") "#d43a3a" else
                if (r$r3 == "medium") "#e0a800" else "#1a9e55"
      pill <- switch(r$r3, high = "critical",
                     medium = "medium", low = "online")
      plab <- switch(r$r3, high = "Critical",
                     medium = "Medium", low = "Low")
      tags$tr(
        tags$td(class = "mini", r$machine_name),
        tags$td(style = "font-weight:800", round(rul)),
        tags$td(style = "width:40%",
                div(class = "imp-bar",
                    tags$i(style = paste0("width:",
                                          max(frac * 100, 6),
                                          "%;background:",
                                          barcol)))),
        tags$td(span(class = paste0("pill ", pill), plab)))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("Machine ID", "RUL (days)",
                                 "30d Horizon", "Status"), tags$th))),
      tags$tbody(rows))
  })

  output$pred_recos <- renderUI({
    x <- scored()
    req(nrow(x) > 0)
    xx <- x[order(-x$risk_score), ][1:min(4, nrow(x)), ]
    ics <- c(critical = "circle-exclamation", high = "wrench",
             medium = "clock", low = "circle-check")
    cols <- c(critical = "#d43a3a", high = "#f08c1a",
              medium = "#e0a800", low = "#1a9e55")
    lapply(seq_len(nrow(xx)), function(i) {
      r <- xx[i, ]
      rec <- tryCatch(
        recommend_maintenance(r$machine_id, r$disp_name,
                              r$risk_level, r$risk_score,
                              r$rul_days,
                              list(summary = ""))$action,
        error = function(e) "Routine monitoring")
      div(class = "alert-row",
        div(class = "alert-ic",
            style = paste0("background:", cols[[r$risk_level]]),
            icon(ics[[r$risk_level]])),
        div(style = "flex:1",
            div(class = "t", r$disp_name),
            div(class = "m", rec)))
    })
  })

  output$pred_dist_donut <- renderPlotly({
    x <- scored()
    req(nrow(x) > 0)
    r3 <- risk3(x$risk_level)
    n <- c(sum(r3 == "high"), sum(r3 == "medium"),
           sum(r3 == "low"))
    donut(unname(n), c("#d43a3a", "#f0b429", "#1a9e55"),
          paste0("<b>", nrow(x), "</b><br>Machines"),
          w = 150, h = 150, fs = 15)
  })

  output$pred_dist_legend <- renderUI({
    x <- scored()
    req(nrow(x) > 0)
    r3 <- risk3(x$risk_level)
    n <- c(sum(r3 == "high"), sum(r3 == "medium"),
           sum(r3 == "low"))
    labs <- c("High Risk", "Medium Risk", "Low Risk")
    cols <- c("#d43a3a", "#f0b429", "#1a9e55")
    lapply(seq_along(n), function(i) {
      div(class = "dleg",
          span(class = "sw",
               style = paste0("background:", cols[i])),
          labs[i],
          tags$b(paste0(n[i], " (",
                        round(100 * n[i] / nrow(x)), "%)")))
    })
  })

  observeEvent(input$pred_view_recos, go_tab("schedule"))
  observeEvent(input$pred_view_alerts, go_tab("predict"))

  observeEvent(input$pred_qa_refresh, {
    showNotification("Running batch predictions...", type = "message")
    out <- tryCatch({
      run_batch_predictions()
      "ok"
    }, error = function(e) conditionMessage(e))
    if (identical(out, "ok")) {
      showNotification("Predictions updated", type = "message")
    } else {
      showNotification(paste("Failed:", out), type = "error")
    }
  })
}
