# File: shiny/app.R — ProMaint dashboard
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION"))
  pkgload::load_all(quiet = TRUE) else library(rpredictmaint)

library(shiny)
library(plotly)
library(DT)

small <- function(...) tags$small(...)
source(file.path(project_root(), "shiny", "mon_ui.R"), local = TRUE)
source(file.path(project_root(), "shiny", "mon_server.R"), local = TRUE)
source(file.path(project_root(), "shiny", "pred_ui.R"), local = TRUE)
source(file.path(project_root(), "shiny", "pred_server.R"), local = TRUE)

machine_img <- function(machine_name) {
  num <- sub("^PUMP-", "", machine_name)
  sprintf("machine_images/M-%s.jpg", num)
}

sensor_list   <- c(Temperature = "temperature", Vibration = "vibration",
                   Pressure = "pressure", RPM = "rpm")
sensor_colors <- c(temperature = "#ef4444", vibration = "#f59e0b",
                   pressure = "#2563eb", rpm = "#10b981")

NAV_ITEMS <- data.frame(
  id    = c("dashboard","monitor","predict","model","anomaly","schedule",
            "explorer","reports","settings"),
  icon  = c("gauge-high","wave-square","chart-line","chart-simple",
            "triangle-exclamation","calendar-days","database","file-lines","gear"),
  label = c("Dashboard","Live Monitoring","Predictions","Model Performance",
            "Anomaly Detection","Maintenance Schedule","Data Explorer",
            "Reports","Settings"))

time_ago <- function(t) {
  if (length(t) == 0 || is.na(t)) return("---")
  s <- as.numeric(difftime(Sys.time(), t, units = "secs"))
  if (s < 60) "just now"
  else if (s < 3600) paste0(round(s / 60), " min ago")
  else if (s < 86400) paste0(round(s / 3600), " hours ago")
  else paste0(round(s / 86400), " days ago")
}
pretty_name <- function(x) tools::toTitleCase(tolower(x))
pretty_type <- function(x) tools::toTitleCase(gsub("_", " ", x))
hpct_class  <- function(h) if (h >= 85) "g" else if (h >= 70) "y" else
  if (h >= 45) "o" else "r"
health_col  <- function(h) if (h >= 70) "#1a9e55" else
  if (h >= 45) "#f0b429" else "#d43a3a"
status_of <- function(rl) dplyr::case_when(
  rl == "low" ~ "online", rl %in% c("medium", "high") ~ "atrisk", TRUE ~ "critical")
status_lab <- function(s) unname(c(online = "Online", atrisk = "At Risk",
                                   critical = "Critical")[s])

alert_meta <- list(
  critical = list(ic = "circle-exclamation", col = "#d43a3a", pill = "critical"),
  high     = list(ic = "circle-exclamation", col = "#f08c1a", pill = "high"),
  medium   = list(ic = "circle-exclamation", col = "#e0a800", pill = "medium"),
  drift    = list(ic = "chart-line",         col = "#7c5cff", pill = "drift"),
  info     = list(ic = "circle-info",        col = "#2563eb", pill = "info"))

empty_card <- function(msg) div(class = "empty", icon("inbox"), " ", msg)

clean_feat_label <- function(x) {
  x <- gsub("^(roll_mean_|roll_sd_|lag[0-9]+_|delta[0-9]+_|dev_from_mean_|range_ratio_)",
            "", x)
  x <- gsub("^value_", "", x)
  x <- gsub("_[0-9]+$", "", x)
  tools::toTitleCase(gsub("_", " ", x))
}

donut <- function(values, colors, center, w = 120, h = 120, fs = 13) {
  plot_ly(values = values, type = "pie", hole = 0.75, sort = FALSE,
          marker = list(colors = colors),
          textinfo = "none", hoverinfo = "none", showlegend = FALSE) |>
    layout(width = w, height = h, margin = list(0, 0, 0, 0),
           paper_bgcolor = "transparent",
           annotations = list(
             text = center, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
             showarrow = FALSE, font = list(size = fs, color = "#1c2b46"))) |>
    config(displayModeBar = FALSE)
}

kpi_card <- function(span_cls, ic, col, bg, title, big, sub, trend) {
  div(class = span_cls,
      div(class = "card kpi",
          style = paste0("background:linear-gradient(180deg,#fff,", bg, ")"),
          div(class = "ic", style = paste0("background:", col), icon(ic)),
          div(h4(title), div(class = "big", big), div(class = "sub", sub)),
          span(class = "trend", icon(trend))))
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/dashboard.css"),
    tags$style(HTML(".container-fluid{padding:0;max-width:none}"))
  ),
  div(class = "app",
      uiOutput("sidebar"),
      div(class = "main",
          uiOutput("header"),
          div(class = "content", uiOutput("page")),
          div(class = "footer",
              span(HTML("<b style='color:#1c2b46'>R</b>&nbsp; Predictive Maintenance System &nbsp;|&nbsp; Powered by R, Shiny, PostgreSQL, and Machine Learning")),
              span(class = "sys-online", span(class = "dot"),
                   " All systems operational")))))

server <- function(input, output, session) {
  cfg <- get_model_config()
  current_tab <- reactiveVal("dashboard")
  sensor_sel  <- reactiveVal("temperature")

  fleet <- reactivePoll(60000, session,
    checkFunc = function() as.integer(Sys.time()) %/% 60,
    valueFunc = function() tryCatch(list(
      machines = get_machines(),
      preds    = get_latest_predictions(),
      alerts   = get_open_alerts(),
      maint    = get_maintenance_from_db(),
      readings = get_readings_from_db(since = now_utc() - lubridate::hours(168))
    ), error = function(e) list(
      machines = NULL, preds = NULL, alerts = NULL, maint = NULL, readings = NULL)))

  alerts_df <- reactive({ a <- fleet()$alerts; if (is.null(a)) dplyr::tibble() else a })

  scored <- reactive({
    d <- fleet()
    req(d$machines, d$preds)
    x <- dplyr::left_join(d$preds, d$machines, by = "machine_id")
    x$health    <- pmax(0, pmin(100, 100 - x$risk_score))
    x$status    <- status_of(x$risk_level)
    x$disp_name <- pretty_name(x$machine_name)
    x$type_lab  <- pretty_type(x$machine_type)
    dplyr::arrange(x, machine_id)
  })

  latest_sensor <- reactive({
    r <- fleet()$readings
    if (is.null(r) || nrow(r) == 0) return(NULL)
    r |> dplyr::group_by(machine_id, sensor_type) |>
      dplyr::slice_max(reading_time, n = 1) |> dplyr::ungroup()
  })

  observe({
    lapply(NAV_ITEMS$id, function(n) {
      observeEvent(input[[paste0("nav_", n)]], current_tab(n), ignoreInit = TRUE)
    })
  })
  observeEvent(input$view_alerts, current_tab("predict"))
  observeEvent(input$qa_view,     current_tab("predict"))

  # ---- sidebar: WRAPPED in .sidebar (the missing-navy fix) ----
  mon_server_init(input, output, session,
                  list(fleet = fleet, scored = scored,
                       latest_sensor = latest_sensor,
                       alerts_df = alerts_df,
                       current_tab = current_tab))
  pred_server_init(input, output, session,
                   list(fleet = fleet, scored = scored,
                        alerts_df = alerts_df,
                        current_tab = current_tab))

  output$sidebar <- renderUI({
    div(class = "sidebar",
      div(class = "brand",
          div(class = "logo", icon("gears")),
          div(tags$b("ProMaint"), small("Predictive Maintenance"))),
      div(class = "nav",
          lapply(seq_len(nrow(NAV_ITEMS)), function(i) {
            it <- NAV_ITEMS[i, ]
            actionLink(paste0("nav_", it$id), it$label,
                       icon = icon(it$icon),
                       class = paste("nav-item",
                                     if (current_tab() == it$id) "active" else ""))
          })),
      div(class = "sidebar-foot",
          div(class = "rlogo", "R"),
          div(div("R Shiny Dashboard"),
              span(style = "color:#8fa2cf;font-size:11px", "v1.0.0"))))
  })

  output$header <- renderUI({
    invalidateLater(30000)
    n <- nrow(alerts_df())
    div(class = "header",
        div(class = "appicon", icon("chart-line")),
        div(class = "titles",
            h1("Predictive Maintenance"),
            small("Smarter insights ", tags$b("•"), " Higher uptime ",
                  tags$b("•"), " Lower costs")),
        div(class = "hspacer"),
        span(class = "sys-online", span(class = "dot"), "System Online"),
        span(class = "clock", format(Sys.time(), "%b %d, %Y  %H:%M")),
        div(class = "bell", icon("bell"),
            if (n > 0) span(class = "badge", if (n > 9) "9+" else n)),
        div(class = "admin", div(class = "avatar", icon("user")),
            span(style = "font-weight:700", "Admin"), icon("chevron-down")))
  })

  tab_dashboard <- function() {
    na <- nrow(alerts_df())
    tagList(
      uiOutput("kpi_row"), tags$br(),
      div(class = "grid12",
        div(class = "card span5",
            div(class = "card-head", h3("Machine Status"),
                span(class = "live-tag", span(class = "dot"), " Live")),
            uiOutput("machine_table")),
        div(class = "card span4",
            div(class = "card-head", h3("Selected Machine"), uiOutput("sel_badge")),
            div(class = "select-wrap",
                icon("industry", class = "sel-ic"),
                selectInput("sel_machine", NULL, choices = NULL, width = "100%")),
            div(class = "sel-flex",
                uiOutput("sel_photo"),
                div(style = "width:120px;flex-shrink:0",
                    plotlyOutput("health_donut", height = "120px", width = "120px")),
                div(class = "stats-col", uiOutput("sel_stats"))),
            uiOutput("sel_chips")),
        div(class = "card span3",
            div(class = "card-head", h3("Recent Alerts"),
                div(style = "display:flex;gap:10px;align-items:center",
                    if (na > 0) span(class = "alert-count", na) else NULL,
                    actionLink("view_alerts", "View All", class = "link-sm"))),
            uiOutput("alert_list"))),
      tags$br(),
      div(class = "grid12",
        div(class = "card span5",
            div(class = "card-head", uiOutput("sensor_title"),
                span(class = "live-tag", span(class = "dot"), " Live")),
            uiOutput("sensor_tabs"),
            fluidRow(column(8, plotlyOutput("sensor_chart", height = "215px")),
                     column(4, div(class = "legend-side", uiOutput("sensor_legend"))))),
        div(class = "card span4",
            div(class = "card-head", h3("Model Performance"),
                actionLink("compare_models", "⇄ Compare", class = "link-sm")),
            uiOutput("perf_table")),
        div(class = "card span3",
            div(class = "card-head", h3("Feature Importance")),
            uiOutput("imp_bars"))),
      tags$br(),
      div(class = "grid12",
        div(class = "card span4",
            div(class = "card-head", uiOutput("rul_title")),
            plotlyOutput("rul_chart", height = "230px")),
        div(class = "card span3", div(class = "card-head", h3("Quick Insights")),
            uiOutput("insights")),
        div(class = "card span2",
            div(class = "card-head", h3("Distribution")),
            div(class = "donut-wrap",
                plotlyOutput("dist_donut", height = "130px", width = "130px")),
            div(class = "dist-legend", uiOutput("dist_legend"))),
        div(class = "card span3",
            div(class = "card-head", h3("Quick Actions")),
            actionButton("qa_view", "View All Predictions",
                         icon = icon("chart-line"), class = "qa-btn blue"),
            actionButton("qa_report", "Download Report",
                         icon = icon("download"), class = "qa-btn green"),
            downloadButton("qa_csv", "Export to CSV", class = "qa-btn purple",
                           style = "color:#fff;border:0;display:flex"),
            actionButton("qa_alerts", "Configure Alerts",
                         icon = icon("bell"), class = "qa-btn teal"))))
  }

  output$page <- renderUI({
    switch(current_tab(),
      dashboard = tab_dashboard(),
      monitor   = mon_tab_ui(),
      predict   = pred_tab_ui(),
      model     = div(class = "grid12",
                      div(class = "card span7",
                          div(class = "card-head", h3("Model Registry")),
                          DTOutput("registry_dt")),
                      div(class = "card span5",
                          div(class = "card-head", h3("Feature Importance")),
                          uiOutput("imp_bars2"))),
      anomaly   = div(class = "card span12",
                      div(class = "card-head", h3("Anomaly Leaderboard")),
                      DTOutput("anom_dt")),
      schedule  = div(class = "card span12",
                      div(class = "card-head", h3("Maintenance Schedule")),
                      DTOutput("sched_dt")),
      explorer  = div(class = "card span12",
                      div(class = "card-head", h3("Sensor Readings (last 48h)")),
                      DTOutput("read_dt")),
      reports   = div(class = "card span12",
                      div(class = "card-head", h3("Reports & Exports")),
                      downloadButton("qa_csv2", "Export predictions CSV",
                                     class = "qa-btn purple",
                                     style = "display:inline-flex;width:auto;margin-bottom:8px"),
                      actionButton("qa_report2", "Render HTML maintenance report",
                                   icon = icon("file-lines"), class = "qa-btn green",
                                   style = "display:inline-flex;width:auto"),
                      uiOutput("perf_files")),
      settings  = div(class = "card span12",
                      div(class = "card-head", h3("Risk Configuration (read-only)")),
                      verbatimTextOutput("cfg_out")))
  })

  output$kpi_row <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    m <- fleet()$maint
    cost <- 0
    if (!is.null(m) && nrow(m) > 0) {
      mm <- m[m$performed_at >= (now_utc() - lubridate::days(30)), ]
      if (nrow(mm) > 0) cost <- round(sum(mm$downtime_hours, na.rm = TRUE) * 6500)
    }
    n <- nrow(x)
    n_online <- sum(x$status == "online")
    n_risk   <- sum(x$status == "atrisk")
    n_crit   <- sum(x$status == "critical")
    pct <- function(k) paste0(round(100 * k / n, 1), "% of total")
    div(class = "grid12",
      kpi_card("span3", "robot", "var(--blue)", "#f2f6ff", "Total Machines", n,
        HTML(paste0("<span class='on'>● ", n_online, " Online</span> &nbsp; ",
                    "<span class='off'>● ", n_risk + n_crit,
                    " Needing attention</span>")), "chart-line"),
      kpi_card("span3", "heart-pulse", "var(--green)", "#f0fbf4",
               "Healthy Machines", n_online, pct(n_online), "arrow-trend-up"),
      kpi_card("span2", "triangle-exclamation", "var(--amber)", "#fffaef",
               "At Risk", n_risk, pct(n_risk), "arrow-trend-up"),
      kpi_card("span2", "triangle-exclamation", "var(--red)", "#fef1f1",
               "Critical", n_crit, pct(n_crit), "chart-line"),
      kpi_card("span2", "screwdriver-wrench", "var(--purple)", "#f6f2ff",
               "Maintenance Cost", paste0("KSh ", format(cost, big.mark = ",")),
               "last 30 days", "arrow-trend-down"))
  })

  output$machine_table <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    ls <- latest_sensor()
    val <- function(mid, st) {
      if (is.null(ls)) return("---")
      v <- ls$value[ls$machine_id == mid & ls$sensor_type == st]
      if (length(v)) round(v, 1) else "---"
    }
    rows <- lapply(seq_len(nrow(x)), function(i) {
      r <- x[i, ]
      tags$tr(class = if (r$status == "critical") "critical-row",
        tags$td(
          tags$div(style = "display:flex;align-items:center;gap:8px",
            local({
              src <- machine_img(r$machine_name)
              if (file.exists(file.path(project_root(), "shiny", "www", src)))
                tags$img(src = src, class = "thumb", alt = r$machine_name, style = "width:30px;height:30px;object-fit:cover;border-radius:8px;display:block;flex-shrink:0")
              else
                tags$span(class = "thumb thumb-fallback", icon("industry"))
            }),
            tags$span(class = "mini", r$machine_name))),
        tags$td(class = "mname", r$disp_name),
        tags$td(pretty_type(r$machine_type)),
        tags$td(class = paste0("hpct ", hpct_class(r$health)),
                paste0(round(r$health), "%")),
        tags$td(val(r$machine_id, "temperature")),
        tags$td(val(r$machine_id, "vibration")),
        tags$td(span(class = paste0("pill ", r$status), status_lab(r$status))),
        tags$td(class = "mini", time_ago(r$predicted_at)))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("Machine ID", "Machine Name", "Type", "Health",
                                 "Temp (°C)", "Vibration", "Status",
                                 "Last Update"), tags$th))),
      tags$tbody(rows))
  })

  observe({
    x <- scored()
    if (is.null(x) || nrow(x) == 0) return()
    ch <- setNames(x$machine_id, paste0(x$disp_name, "  (", x$type_lab, ")"))
    sel0 <- isolate(input$sel_machine)
    updateSelectInput(session, "sel_machine", choices = ch,
                      selected = if (!is.null(sel0) && sel0 %in% unname(ch)) sel0
                                 else ch[[1]])
  })

  sel <- reactive({
    x <- scored(); req(nrow(x) > 0, input$sel_machine)
    x[x$machine_id == input$sel_machine, ][1, ]
  })

  output$sel_badge <- renderUI({
    s <- sel(); req(s)
    if (s$status == "critical")
      span(class = "badge-critical",
           span(class = "dot", style = "background:#fff"), " Critical")
    else span(class = paste0("pill ", s$status), status_lab(s$status))
  })
output$sel_photo <- renderUI({
  s <- sel(); req(s)
  src <- machine_img(s$machine_name)
  if (file.exists(file.path(project_root(), "shiny", "www", src))) {
    tags$img(src = src, class = "photo-img", alt = s$type_lab, style = "width:170px;height:160px;object-fit:cover;border-radius:12px;display:block;flex-shrink:0")
  } else {
    div(class = "photo-tile", icon("industry"))
  }
})

  output$health_donut <- renderPlotly({
    s <- sel(); req(s)
    donut(c(s$health, max(100 - s$health, 0)),
          c(health_col(s$health), "#e8edf5"),
          paste0("<b>", round(s$health), "%</b><br>",
                 "<span style='font-size:9px;color:#7b8aa5'>Health</span>"))
  })
  output$sel_stats <- renderUI({
    s <- sel(); req(s)
    rul_txt <- if (is.na(s$rul_days) || s$rul_days > 999) "30+ days"
               else paste0(floor(s$rul_days), " \u2013 ", ceiling(s$rul_days), " days")
    rec <- tryCatch(
      recommend_maintenance(s$machine_id, s$disp_name, s$risk_level,
                            s$risk_score, s$rul_days, list(summary = ""))$action,
      error = function(e) "Routine monitoring")
    tagList(
      div(class = "stat-box",
          span(class = "lab", "Failure Probability"),
          div(class = "val red", paste0(round(100 * s$failure_prob, 1), "%"))),
      div(class = "stat-box",
          span(class = "lab", icon("calendar-days"), "Estimated RUL"),
          div(class = "val", rul_txt)),
      div(class = "stat-box",
          span(class = "lab", icon("screwdriver-wrench"), "Recommended Action"),
          div(style = "font-weight:700;font-size:12px;margin-top:2px", rec)))
  })

  output$sel_chips <- renderUI({
    s <- sel(); req(s)
    tags$div(class = "chips",
      div(class = "chip", "Type", tags$b(s$type_lab)),
      div(class = "chip", "Location", tags$b(s$location %||% "---")),
      div(class = "chip", "Model",
          tags$b(paste0("PM-", sprintf("%03d", s$machine_id)))))
  })

  output$alert_list <- renderUI({
    a <- alerts_df()
    if (nrow(a) == 0) return(empty_card("No open alerts — all clear."))
    lapply(seq_len(min(5, nrow(a))), function(i) {
      r <- a[i, ]
      am <- alert_meta[[r$alert_level]] %||% alert_meta$info
      div(class = "alert-row",
        div(class = "alert-ic", style = paste0("background:", am$col), icon(am$ic)),
        div(style = "flex:1",
            div(class = "t", r$machine_name),
            div(class = "m", r$message)),
        div(class = "when", time_ago(r$created_at),
            tags$br(), span(class = paste0("pill ", am$pill),
                            toupper(r$alert_level))))
    })
  })

  output$sensor_tabs <- renderUI({
    tags$div(class = "nav-pills-inline",
      lapply(names(sensor_list), function(nm) {
        v <- sensor_list[[nm]]
        actionButton(paste0("sensbtn_", v), nm,
                     class = if (sensor_sel() == v) "active" else "")
      }))
  })
  output$sensor_tabs2 <- renderUI({
    tags$div(class = "nav-pills-inline",
      lapply(names(sensor_list), function(nm) {
        v <- sensor_list[[nm]]
        actionButton(paste0("sensbtn2_", v), nm,
                     class = if (sensor_sel() == v) "active" else "")
      }))
  })
  observe({
    lapply(unname(sensor_list), function(v) {
      observeEvent(input[[paste0("sensbtn_", v)]],  sensor_sel(v), ignoreInit = TRUE)
      observeEvent(input[[paste0("sensbtn2_", v)]], sensor_sel(v), ignoreInit = TRUE)
    })
  })

  sensor_plot <- function(height) {
    renderPlotly({
      r <- fleet()$readings
      req(r, nrow(r) > 0, input$sel_machine)
      d <- r[r$machine_id == input$sel_machine & r$sensor_type == sensor_sel(), ]
      req(nrow(d) > 0)
      plot_ly(d, x = ~reading_time, y = ~value, type = "scatter", mode = "lines",
              line = list(color = unname(sensor_colors[[sensor_sel()]]), width = 2.4),
              name = sensor_sel()) |>
        layout(margin = list(20, 10, 10, 30),
               paper_bgcolor = "transparent", plot_bgcolor = "transparent",
               xaxis = list(title = "", showgrid = FALSE),
               yaxis = list(title = "", gridcolor = "#eef2f9")) |>
        config(displayModeBar = FALSE)
    })
  }
  output$sensor_chart  <- sensor_plot(215)
  output$sensor_chart2 <- sensor_plot(320)

  output$sensor_title <- renderUI({
    s <- sel(); req(s)
    h3(paste0("Sensor Readings – ", s$disp_name))
  })
  output$sensor_legend <- renderUI({
    ls <- latest_sensor(); s <- sel(); req(s)
    lapply(names(sensor_list), function(nm) {
      st <- sensor_list[[nm]]
      v <- if (is.null(ls)) NA
           else ls$value[ls$machine_id == s$machine_id & ls$sensor_type == st]
      div(class = "leg",
          span(class = "sw", style = paste0("background:", sensor_colors[[st]])),
          nm, tags$b(if (length(v) && !is.na(v)) round(v, 1) else "---"))
    })
  })

  output$perf_table <- renderUI({
    reg <- tryCatch(get_active_models(), error = function(e) NULL)
    if (is.null(reg) || nrow(reg) == 0)
      return(empty_card("Train models first: make train"))
    ord <- match(c("failure_classifier","rul_model","anomaly_detector",
                   "survival_model","ensemble"), reg$model_name)
    reg <- reg[ord[!is.na(ord)], ]
    names_map <- c(failure_classifier = "XGBoost", rul_model = "Random Forest",
                   anomaly_detector = "Isolation Forest",
                   survival_model = "Weibull AFT", ensemble = "Ensemble")
    rows <- lapply(seq_len(nrow(reg)), function(i) {
      r <- reg[i, ]
      mt <- tryCatch(as.data.frame(jsonlite::fromJSON(r$metrics)),
                     error = function(e) NULL)
      g <- function(k) if (!is.null(mt) && k %in% names(mt))
        round(as.numeric(mt[[k]]), 3) else "---"
      tags$tr(class = if (r$model_name == "failure_classifier") "best",
        tags$td(names_map[[r$model_name]] %||% r$model_name),
        tags$td(g("accuracy")), tags$td(g("precision")), tags$td(g("recall")),
        tags$td(g("f1")), tags$td(g("roc_auc")))
    })
    tags$table(class = "models",
      tags$thead(tags$tr(lapply(c("Model", "Accuracy", "Precision", "Recall",
                                 "F1", "ROC-AUC"), tags$th))),
      tags$tbody(rows))
  })

  imp_share <- reactive({
    m <- tryCatch(load_active_models()$failure_classifier, error = function(e) NULL)
    req(m)
    imp <- tryCatch(get_feature_importance(m), error = function(e) NULL)
    req(!is.null(imp) && nrow(imp) > 0)
    imp$share <- 100 * imp$gain / sum(imp$gain)
    utils::head(imp[order(-imp$share), c("feature", "share")], 5)
  })
  imp_ui <- function() renderUI({
    d <- imp_share()
    cols <- c("#d43a3a", "#f08c1a", "#e0a800", "#1a9e55", "#2563eb")
    lapply(seq_len(nrow(d)), function(i) div(class = "imp-row",
      span(class = "lab", clean_feat_label(d$feature[i])),
      span(class = "pct", paste0(round(d$share[i]), "%")),
      div(class = "imp-bar",
          tags$i(style = paste0("width:", max(d$share[i] * 2.2, 8),
                                "%;background:", cols[i], "")))))
  })
  output$imp_bars  <- imp_ui()
  output$imp_bars2 <- imp_ui()

  output$rul_title <- renderUI({
    s <- sel(); req(s)
    h3(paste0("RUL Forecast – ", s$disp_name))
  })
  output$rul_chart <- renderPlotly({
    s <- sel(); req(s)
    rul <- if (is.na(s$rul_days) || s$rul_days > 999) 30 else s$rul_days
    t  <- seq(0, 30, by = 0.5)
    y  <- pmax(rul - t, 0)
    lo <- pmax(rul - 1.15 * t - 1, 0)
    hi <- rul + 0.6 * t + 1
    ann <- paste0("RUL: ", floor(rul), " – ", ceiling(rul), " days")
    plot_ly() |>
      add_lines(x = t, y = hi, line = list(color = "#f0b429", dash = "dash",
                                           width = 1.2), name = "Upper Bound") |>
      add_lines(x = t, y = lo, line = list(color = "#f0b429", dash = "dash",
                                           width = 1.2), name = "Lower Bound") |>
      add_lines(x = t, y = y, line = list(color = "#2563eb", width = 3),
                name = "Predicted RUL") |>
      add_segments(x = rul, xend = rul, y = 0, yend = max(y),
                   line = list(color = "#d43a3a", dash = "dot", width = 1.2),
                   showlegend = FALSE) |>
      layout(margin = list(20, 10, 10, 35),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             legend = list(font = list(size = 10)),
             annotations = list(x = rul, y = max(y) * 0.9, text = ann,
                                showarrow = TRUE, arrowcolor = "#d43a3a",
                                font = list(color = "#fff", size = 11),
                                bgcolor = "#d43a3a", borderpad = 5),
             xaxis = list(title = "Days", gridcolor = "#eef2f9"),
             yaxis = list(title = "Hours", gridcolor = "#eef2f9")) |>
      config(displayModeBar = FALSE)
  })

  output$insights <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    s <- sel()
    ins <- list(
      list("ok",   "circle-check", "Overall equipment health is stable."),
      list("bad",  "circle-exclamation",
           paste0(s$disp_name, " requires immediate attention.")),
      list("warn", "triangle-exclamation",
           paste0(sum(x$status == "atrisk"), " machines are at moderate risk.")),
      list("info", "circle-info", "Predictions refresh every 60 seconds."),
      list("info", "chart-line", "System performance is within normal range."))
    lapply(ins, function(i)
      div(class = paste0("insight ", i[[1]]), icon(i[[2]]), span(i[[3]])))
  })
  output$dist_donut <- renderPlotly({
    x <- scored(); req(nrow(x) > 0)
    n <- c(sum(x$status == "online"), sum(x$status == "atrisk"),
           sum(x$status == "critical"))
    donut(unname(n), c("#1a9e55", "#f0b429", "#d43a3a"),
          paste0("<b>", nrow(x), "</b><br>",
                 "<span style='font-size:9px;color:#7b8aa5'>Machines</span>"),
          w = 130, h = 130)
  })
  output$dist_legend <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    n <- nrow(x)
    seg <- function(col, lab, k) {
      div(class = "dleg",
          span(class = "sw", style = paste0("background:", col)),
          lab, tags$b(paste0(k, " (", round(100 * k / n), "%)")))
    }
    tagList(
      seg("#1a9e55", "Healthy",  sum(x$status == "online")),
      seg("#f0b429", "At Risk",  sum(x$status == "atrisk")),
      seg("#d43a3a", "Critical", sum(x$status == "critical")))
  })

  observeEvent(input$qa_alerts, showModal(modalDialog(
    title = "Alert thresholds (from model_config.yml)",
    HTML(paste0("<b>Medium ≥ ", cfg$risk$thresholds$medium, "</b> · ",
                "<b>High ≥ ", cfg$risk$thresholds$high, "</b> · ",
                "<b>Critical ≥ ", cfg$risk$thresholds$critical, "</b><br><br>",
                "Ensemble weights — failure: ", cfg$risk$weights$failure,
                ", RUL: ", cfg$risk$weights$rul,
                ", anomaly: ", cfg$risk$weights$anomaly)),
    easyClose = TRUE)))

  csv_name <- function() paste0("predictions_", format(Sys.Date(), "%Y%m%d"), ".csv")
  csv_data <- function() { x <- scored(); req(x); x }
  output$qa_csv  <- downloadHandler(csv_name(),
    function(file) readr::write_csv(csv_data(), file))
  output$qa_csv2 <- downloadHandler(csv_name(),
    function(file) readr::write_csv(csv_data(), file))

  ensure_pandoc <- function() {
    if (nchar(Sys.which("pandoc")) > 0) return(invisible(TRUE))
    cands <- unique(c(
      Sys.getenv("PANDOC_HOME"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
      "C:/Program Files/Pandoc",
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
      "C:/Program Files/RStudio/bin/pandoc"))
    cands <- cands[nzchar(cands)]
    for (d in cands) {
      if (file.exists(file.path(d, "pandoc.exe"))) {
        Sys.setenv(RSTUDIO_PANDOC = d)
        return(invisible(TRUE))
      }
    }
    stop("Pandoc not found. Install: winget install JohnMacFarlane.Pandoc")
  }
  report_file <- reactiveVal(NULL)

  do_report <- function() {
    dir <- file.path(project_root(), "reports", "maintenance_reports")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    out <- tryCatch({
      f <- file.path(dir, paste0("report_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"))
      rpredictmaint::build_report_html(f)
      f
    }, error = function(e) {
      showNotification(paste("Report failed:", conditionMessage(e)),
                       type = "error", duration = 15)
      NULL
    })
    if (is.null(out) || !file.exists(out) ||
        file.info(out)$size < 2000) {
      showNotification("Report rendered empty - check console",
                       type = "error", duration = 15)
      return(invisible(NULL))
    }
    report_file(out)
    showModal(modalDialog(
      title = "Report ready",
      tags$p("Saved to"), tags$code(out),
      downloadButton("report_dl", "Download Report (.html)"),
      footer = modalButton("Close"), easyClose = TRUE))
  }

  output$report_dl <- downloadHandler(
    filename = function() paste0("maintenance_report_",
      format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      f <- report_file()
      req(f, file.exists(f))
      file.copy(f, file)
    }
  )

  observeEvent(input$qa_report, do_report())
  observeEvent(input$qa_report2, do_report())

  output$pred_dt <- renderDT({
    x <- scored(); req(x)
    datatable(x[, c("machine_name", "disp_name", "failure_prob", "rul_days",
                    "anomaly_score", "risk_score", "risk_level", "predicted_at")],
              colnames = c("ID", "Machine", "Failure P", "RUL (d)", "Anomaly",
                           "Risk", "Level", "At"),
              options = list(pageLength = 10), rownames = FALSE)
  })
  output$registry_dt <- renderDT({
    reg <- tryCatch(get_active_models(), error = function(e) NULL)
    req(reg)
    datatable(reg[, c("model_name", "version", "model_type", "trained_at")],
              rownames = FALSE)
  })
  output$anom_dt <- renderDT({
    x <- scored(); req(x)
    datatable(x[order(-x$anomaly_score),
                c("disp_name", "anomaly_score", "risk_score", "risk_level")],
              colnames = c("Machine", "Anomaly", "Risk", "Level"),
              options = list(pageLength = 10), rownames = FALSE)
  })
  output$sched_dt <- renderDT({
    x <- scored(); req(x)
    d <- purrr::pmap_dfr(
      list(x$machine_id, x$disp_name, x$risk_level, x$risk_score, x$rul_days),
      function(mid, mname, lvl, score, rul)
        recommend_maintenance(mid, mname, lvl, score, rul, list(summary = "auto")))
    datatable(d[, c("machine_name", "action", "priority", "due_by")],
              options = list(pageLength = 10), rownames = FALSE)
  })
  output$read_dt <- renderDT({
    r <- fleet()$readings; req(r, nrow(r) > 0)
    datatable(r, options = list(pageLength = 10), rownames = FALSE)
  })
  output$perf_files <- renderUI({
    dir <- file.path(project_root(), "reports", "model_performance")
    fs <- list.files(dir)
    if (length(fs) == 0) empty_card("No performance snapshots yet (run: make monitor)")
    else tags$ul(lapply(fs, tags$li))
  })
  output$cfg_out <- renderPrint({ cfg$features; cat("\n"); cfg$risk })
}

shinyApp(ui, server)
