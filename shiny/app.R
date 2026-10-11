# File: shiny/app.R — ProMaint dashboard (data-driven, matches DB schema)
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION"))
  pkgload::load_all(quiet = TRUE) else library(rpredictmaint)

library(shiny)
library(plotly)
library(DT)

# ---------------- helpers ----------------
time_ago <- function(t) {
  if (length(t) == 0 || is.na(t)) return("—")
  s <- as.numeric(difftime(Sys.time(), t, units = "secs"))
  if (s < 60) "just now" else if (s < 3600) paste0(round(s/60), " min ago") else
    if (s < 86400) paste0(round(s/3600), " hours ago") else paste0(round(s/86400), " days ago")
}
pretty_name  <- function(x) tools::toTitleCase(tolower(x))
pretty_type  <- function(x) tools::toTitleCase(gsub("_", " ", x))
hpct_class   <- function(h) if (h >= 85) "g" else if (h >= 70) "y" else if (h >= 45) "o" else "r"
status_of <- function(rl) dplyr::case_when(
  rl %in% c("low") ~ "online", rl %in% c("medium", "high") ~ "atrisk", TRUE ~ "critical")
status_lab <- function(s) c(online = "Online", atrisk = "At Risk", critical = "Critical")[s]
alert_meta <- list(
  critical = list(ic = "circle-exclamation", col = "#d43a3a", pill = "critical"),
  high     = list(ic = "circle-exclamation", col = "#f08c1a", pill = "high"),
  medium   = list(ic = "circle-exclamation", col = "#e0a800", pill = "medium"),
  drift    = list(ic = "chart-line",         col = "#7c5cff", pill = "drift"),
  info     = list(ic = "circle-info",        col = "#2563eb", pill = "info"))
empty_card <- function(msg) div(class = "empty", icon("inbox"), " ", msg)
donut <- function(v, colr, center, sub) {
  plot_ly(values = c(v, 100 - v), type = "pie", hole = 0.72, sort = FALSE,
          marker = list(colors = c(colr, "#e8edf5")),
          textinfo = "none", hoverinfo = "none") |>
    layout(showlegend = FALSE, margin = list(0,0,0,0),
           paper_bgcolor = "transparent",
           annotations = list(text = paste0("<b>", center, "</b><br><span style='font-size:11px;color:#7b8aa5'>",
                                            sub, "</span>"),
                              showarrow = FALSE, font = list(size = 15))) |>
    config(displayModeBar = FALSE)
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/dashboard.css"),
    tags$style(htmltools::HTML(".container-fluid{padding:0;max-width:none}")),
  ),
  div(class = "app",
    # ================= SIDEBAR =================
    aside(class = "sidebar",
      div(class = "brand",
        div(class = "logo", icon("gears")),
        div(tags$b("ProMaint"), small("Predictive Maintenance"))),
      div(class = "nav",
        lapply(list(
            c("dashboard", "gauge-high",   "Dashboard"),
            c("monitor",   "wave-square",  "Live Monitoring"),
            c("predict",   "chart-line",   "Predictions"),
            c("model",     "chart-simple", "Model Performance"),
            c("anomaly",   "triangle-exclamation", "Anomaly Detection"),
            c("schedule",  "calendar-days", "Maintenance Schedule"),
            c("explorer",  "database",     "Data Explorer"),
            c("reports",   "file-lines",   "Reports"),
            c("settings",  "gear",         "Settings")),
          ~ actionLink(inputId = paste0("nav_", .x[1]), label = .x[3],
                       icon = icon(.x[2]), class = "nav-item"))),
      div(class = "sidebar-foot",
        div(class = "rlogo", "R"),
        div(div("R Shiny Dashboard"), span(style="color:#8fa2cf;font-size:11px", "v1.0.0")))),

    # ================= MAIN =================
    div(class = "main",
      # Header
      div(class = "header",
        div(class = "appicon", icon("chart-line")),
        div(class = "titles", h1("Predictive Maintenance"),
            small("Smarter insights", tags$b(" • "), "Higher uptime", tags$b(" • "), "Lower costs")),
        div(class = "hspacer"),
        span(class = "sys-online", span(class = "dot"), "System Online"),
        span(class = "clock", uiOutput("clock", inline = TRUE)),
        div(class = "bell", icon("bell"), span(class = "badge", uiOutput("bell_n", inline = TRUE))),
        div(class = "admin", div(class = "avatar", icon("user")),
            span(style="font-weight:700", "Admin"), icon("chevron-down"))),

      # Content
      div(class = "content",
        tabsetPanel(id = "main_tabs", type = "hidden",

          # ---------- DASHBOARD ----------
          tabPanelBody("dashboard",
            # KPI row
            uiOutput("kpi_row"),
            tags$br(),
            # Row 2
            div(class = "grid12",
              div(class = "card span5",
                div(class = "card-head", h3("Machine Status"), span(class = "live-tag", span(class="dot")," Live")),
                uiOutput("machine_table")),
              div(class = "card span4",
                div(class = "card-head", h3("Selected Machine"), uiOutput("sel_badge")),
                selectInput("sel_machine", NULL, choices = NULL, width = "100%"),
                div(class = "photo", icon("industry")),
                div(class = "sel-flex",
                  div(style = "width:150px;position:relative",
                    div(class = "donut-wrap", plotlyOutput("health_donut", height = "150px"),
                      div(class = "center-label", uiOutput("donut_center", inline = TRUE)))),
                  div(style = "flex:1",
                    uiOutput("sel_stats"))),
                uiOutput("sel_action"),
                uiOutput("sel_chips")),
              div(class = "card span3",
                div(class = "card-head", h3("Recent Alerts"),
                    actionLink("view_alerts", "View All →", class = "link-sm")),
                uiOutput("alert_list"))),
            tags$br(),
            # Row 3
            div(class = "grid12",
              div(class = "card span6",
                div(class = "card-head",
                  h3(uiOutput("sensor_title", inline = TRUE)),
                  span(class = "live-tag", span(class="dot")," Live")),
                div(class = "nav-pills-inline", uiOutput("sensor_tabs")),
                fluidRow(column(8, plotlyOutput("sensor_chart", height = "215px")),
                         column(4, div(class = "legend-side", uiOutput("sensor_legend"))))),
              div(class = "card span3",
                div(class = "card-head", h3("Model Performance"),
                    actionLink("compare_models", "⇄ Compare Models", class = "link-sm")),
                uiOutput("perf_table")),
              div(class = "card span3",
                div(class = "card-head", h3("Feature Importance")),
                uiOutput("imp_bars"))),
            tags$br(),
            # Row 4
            div(class = "grid12",
              div(class = "card span5",
                div(class = "card-head", h3(uiOutput("rul_title", inline = TRUE))),
                plotlyOutput("rul_chart", height = "230px")),
              div(class = "card span3",
                div(class = "card-head", h3("Quick Insights")),
                uiOutput("insights")),
              div(class = "card span3",
                div(class = "card-head", h3("Machine Health Distribution")),
                div(class = "donut-wrap", plotlyOutput("dist_donut", height = "170px"),
                  div(class = "center-label", uiOutput("dist_center", inline = TRUE)))),
              div(class = "card span4",
                div(class = "card-head", h3("Quick Actions")),
                actionButton("qa_view",  label = "View All Predictions", icon = icon("chart-line"),   class = "qa-btn blue"),
                actionButton("qa_report",label = "Download Report",      icon = icon("download"),     class = "qa-btn green"),
                downloadButton("qa_csv", label = "Export to CSV",        icon = icon("file-csv"),     class = "qa-btn purple",
                               style = "color:#fff;border:0;display:flex"),
                actionButton("qa_alerts",label = "Configure Alerts",     icon = icon("bell"),         class = "qa-btn teal"))
          ),

          # ---------- Other tabs (functional, same theme) ----------
          tabPanelBody("monitor",
            div(class = "card span12", div(class = "card-head", h3("Live Sensor Monitoring"),
                span(class = "live-tag", span(class="dot")," Live")),
              div(class = "nav-pills-inline", uiOutput("sensor_tabs2")),
              plotlyOutput("sensor_chart2", height = "320px"))),
          tabPanelBody("predict",
            div(class = "card span12", div(class = "card-head", h3("All Predictions")),
              DTOutput("pred_dt"))),
          tabPanelBody("model",
            div(class = "grid12",
              div(class = "card span7", div(class = "card-head", h3("Model Registry")), DTOutput("registry_dt")),
              div(class = "card span5", div(class = "card-head", h3("Feature Importance")), uiOutput("imp_bars2")))),
          tabPanelBody("anomaly",
            div(class = "card span12", div(class = "card-head", h3("Anomaly Leaderboard")), DTOutput("anom_dt"))),
          tabPanelBody("schedule",
            div(class = "card span12", div(class = "card-head", h3("Maintenance Schedule")), DTOutput("sched_dt"))),
          tabPanelBody("explorer",
            div(class = "card span12", div(class = "card-head", h3("Sensor Readings (last 48h)")), DTOutput("read_dt"))),
          tabPanelBody("reports",
            div(class = "card span12", div(class = "card-head", h3("Reports & Exports")),
              downloadButton("qa_csv2", "Export predictions CSV", class = "qa-btn purple", style="display:inline-flex;width:auto"),
              actionButton("qa_report2", "Render HTML maintenance report", icon = icon("file-lines"), class = "qa-btn green", style="display:inline-flex;width:auto"),
              uiOutput("perf_files"))),
          tabPanelBody("settings",
            div(class = "card span12", div(class = "card-head", h3("Risk Configuration (read-only)")),
              verbatimTextOutput("cfg_out")))
        )),

      # Footer
      div(class = "footer",
        span(tags$b(style="color:#1c2b46","R"), "  Predictive Maintenance System   |   Powered by R, Shiny, PostgreSQL, and Machine Learning"),
        span(class = "sys-online", span(class = "dot"), " All systems operational"))
    )))

# ---------------- SERVER ----------------
server <- function(input, output, session) {

  cfg <- get_model_config()
  sensor_list <- c(Temperature = "temperature", Vibration = "vibration",
                   Pressure = "pressure", RPM = "rpm")

  ## ---------- data (60s refresh) ----------
  fleet <- reactivePoll(60000, session,
    checkFunc = function() as.integer(Sys.time()) %/% 60,
    valueFunc = function() tryCatch(list(
      machines  = get_machines(),
      preds     = get_latest_predictions(),
      alerts    = get_open_alerts(),
      maint     = get_maintenance_from_db(),
      readings  = get_readings_from_db(since = now_utc() - lubridate::hours(48))),
      error = function(e) list(machines = NULL, preds = NULL, alerts = NULL,
                               maint = NULL, readings = NULL)))

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

  ## ---------- nav ----------
  observe({ for (n in c("dashboard","monitor","predict","model","anomaly",
                        "schedule","explorer","reports","settings")) {
    observeEvent(input[[paste0("nav_", n)]], ignoreNULL = TRUE, {
      updateTabsetPanel(session, "main_tabs", selected = n)
      lapply(setdiff(c("dashboard","monitor","predict","model","anomaly",
                       "schedule","explorer","reports","settings"), n),
             function(o) session$sendInputMessage(paste0("nav_", o), list(value = 0)))
    })
  }})
  observeEvent(input$view_alerts, updateTabsetPanel(session, "main_tabs", selected = "predict"))

  ## ---------- header ----------
  output$clock <- renderUI({
    invalidateLater(30000); format(Sys.time(), "%b %d, %Y &nbsp; %H:%M") |> HTML()
  })
  output$bell_n <- renderUI({
    a <- fleet()$alerts
    if (is.null(a) || nrow(a) == 0) return(NULL)
    if (nrow(a) > 9) "9+" else as.character(nrow(a))
  })

  ## ---------- KPIs ----------
  output$kpi_row <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    d <- fleet()
    cost <- if (!is.null(d$maint) && nrow(d$maint) > 0) {
      m <- d$maint[d$maint$performed_at >= (now_utc() - lubridate::days(30)), ]
      if (nrow(m) > 0) round(sum(m$downtime_hours, na.rm = TRUE) * 6500) else 0
    } else 0
    n_online <- sum(x$status == "online"); n_risk <- sum(x$status == "atrisk")
    n_crit   <- sum(x$status == "critical"); n <- nrow(x)
    pct <- function(k) paste0(round(100 * k / n, 1), "% of total")
    kpi_card <- function(ic, col, bg, title, big, sub, trend)
      div(class = "card span12 kpi", style = "background:linear-gradient(180deg,#fff," || bg || ")",
        div(class = "ic", style = paste0("background:", col), icon(ic)),
        div(h4(title), div(class = "big", big), div(class = "sub", sub)),
        span(class = "trend", icon(trend)))
    div(class = "grid12",
      div(class = "span3", kpi_card("robot", "var(--blue)", "#f2f6ff", "Total Machines", n,
          HTML(paste0("<span class='on'>● ", n_online, " Online</span> &nbsp; <span class='off'>● ",
                      n_crit + sum(x$status == 'atrisk' & FALSE), " Offline</span>")), "chart-line")),
      div(class = "span3", kpi_card("heart-pulse", "var(--green)", "#f0fbf4", "Healthy Machines",
          n_online, pct(n_online), "arrow-trend-up")),
      div(class = "span2", kpi_card("triangle-exclamation", "var(--amber)", "#fffaef", "At Risk",
          n_risk, pct(n_risk), "arrow-trend-up")),
      div(class = "span2", kpi_card("triangle-exclamation", "var(--red)", "#fef1f1", "Critical",
          n_crit, pct(n_crit), "chart-line")),
      div(class = "span2", kpi_card("screwdriver-wrench", "var(--purple)", "#f6f2ff",
          "Maintenance Cost", paste0("KSh ", format(cost, big.mark = ",")),
          "last 30 days (downtime-based)", "arrow-trend-down"))
    )
  })

  ## ---------- machine table ----------
  output$machine_table <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    ls <- latest_sensor()
    val <- function(mid, st) {
      if (is.null(ls)) return("—")
      v <- ls$value[ls$machine_id == mid & ls$sensor_type == st]
      if (length(v)) round(v, 1) else "—"
    }
    rows <- lapply(seq_len(nrow(x)), function(i) {
      r <- x[i, ]
      tags$tr(class = if (r$status == "critical") "critical-row",
        tags$td(icon("industry", style = "color:#2563eb")),
        tags$td(class = "mini", r$machine_name),
        tags$td(class = "mname", r$disp_name),
        tags$td(pretty_type(r$machine_type)),
        tags$td(class = paste0("hpct ", hpct_class(r$health)), paste0(round(r$health), "%")),
        tags$td(val(r$machine_id, "temperature")),
        tags$td(val(r$machine_id, "vibration")),
        tags$td(span(class = paste0("pill ", r$status), status_lab(r$status))),
        tags$td(class = "mini", time_ago(r$predicted_at)))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("", "Machine ID", "Machine Name", "Type", "Health",
                                 "Temp (°C)", "Vibration (mm/s)", "Status", "Last Update"), tags$th))),
      tags$tbody(rows))
  })

  ## ---------- selected machine ----------
  machine_choices <- reactive({
    x <- scored(); req(nrow(x) > 0)
    setNames(x$machine_id, paste0(x$disp_name, "  (", x$type_lab, ")"))
  })
  observe({
    ch <- machine_choices()
    sel0 <- isolate(input$sel_machine)
    updateSelectInput(session, "sel_machine", choices = ch,
                      selected = if (!is.null(sel0) && sel0 %in% unname(ch)) sel0 else ch[[1]])
  })
  sel <- reactive({ x <- scored(); req(nrow(x) > 0); x[x$machine_id == input$sel_machine, ][1, ] })

  output$sel_badge <- renderUI({
    s <- sel(); req(s)
    if (s$status == "critical") span(class = "badge-critical", span(class="dot", style="background:#fff"), " Critical") else
      span(class = paste0("pill ", s$status), status_lab(s$status))
  })
  output$health_donut <- renderPlotly({ s <- sel(); req(s); donut(s$health, "#d43a3a", "", "") })
  output$donut_center <- renderUI({
    s <- sel(); req(s)
    HTML(paste0("<b>", round(s$health), "%</b><span>Health Score</span>"))
  })
  output$sel_stats <- renderUI({
    s <- sel(); req(s)
    rul_txt <- if (is.na(s$rul_days) || s$rul_days > 999) "30+ days"
               else paste0(floor(s$rul_days), " – ", ceiling(s$rul_days), " days")
    tagList(
      div(class = "stat-row", span(class = "lab", "Failure Probability"),
          span(class = "val red", paste0(round(100 * s$failure_prob, 1), "%"))),
      div(class = "stat-row", span(class = "lab", icon("calendar-days"), " Estimated RUL"),
          span(class = "val", rul_txt)))
  })
  output$sel_action <- renderUI({
    s <- sel(); req(s)
    rec <- tryCatch(recommend_maintenance(s$machine_id, s$disp_name, s$risk_level,
                   s$risk_score, s$rul_days, list(summary = ""))$action,
                   error = function(e) "Routine monitoring")
    div(class = "stat-row", span(class = "lab", icon("screwdriver-wrench"), " Recommended Action"),
        span(style = "font-weight:700;font-size:12px", rec))
  })
  output$sel_chips <- renderUI({
    s <- sel(); req(s)
    tags$div(class = "chips",
      div(class = "chip", "Type", tags$b(s$type_lab)),
      div(class = "chip", "Location", tags$b(s$location %||% "—")),
      div(class = "chip", "Model", tags$b(paste0("PM-", sprintf("%03d", s$machine_id)))))
  })

  ## ---------- alerts ----------
  output$alert_list <- renderUI({
    a <- fleet()$alerts
    if (is.null(a) || nrow(a) == 0) return(empty_card("No open alerts — all clear."))
    m <- scored(); nm <- setNames(m$disp_name, m$machine_id)
    lapply(seq_len(min(5, nrow(a))), function(i) {
      r <- a[i, ]; am <- alert_meta[[r$alert_level]] %||% alert_meta$info
      div(class = "alert-row",
        div(class = "alert-ic", style = paste0("background:", am$col), icon(am$ic)),
        div(style = "flex:1", div(class = "t", paste0(r$machine_name, "  ",
             pretty_name(nm[as.character(r$machine_id)] %||% ""))),
            div(class = "m", r$message)),
        div(class = "when", time_ago(r$created_at),
            tags$br(), span(class = paste0("pill ", am$pill), toupper(r$alert_level))))
    })
  })

  ## ---------- sensor tabs + chart ----------
  sensor_tab_ui <- function(id) {
    renderUI({
      sel_t <- isolate(input[[id]]) %||% "temperature"
      tags$div(lapply(names(sensor_list), function(nm) {
        v <- sensor_list[[nm]]
        actionButton(paste0(id, "_", v), nm,
          class = if (v == input[[id]]) "active" else "")
      }))
    })
  }
  set_sensor <- function(id, v) {
    lapply(sensor_list, function(sv)
      observeEvent(input[[paste0(id, "_", sv)]], {
        session$sendInputMessage(id, list(value = sv))
        output[[id]] <<- sensor_tab_ui(id)  # re-render active state
      }))
  }
  output$sensor_tabs  <- sensor_tab_ui("sensor_sel")
  output$sensor_tabs2 <- sensor_tab_ui("sensor_sel2")
  set_sensor("sensor_sel"); set_sensor("sensor_sel2")

  sensor_plot <- function(sens, height) {
    renderPlotly({
      r <- fleet()$readings; req(r, nrow(r) > 0)
      d <- r[r$machine_id == input$sel_machine & r$sensor_type == sens, ]
      req(nrow(d) > 0)
      colors <- c(temperature = "#ef4444", vibration = "#f59e0b",
                  pressure = "#2563eb", rpm = "#10b981")
      plot_ly(d, x = ~reading_time, y = ~value, type = "scatter", mode = "lines",
              line = list(color = unname(colors[[sens]]), width = 2.4), name = sens) |>
        layout(margin = list(20, 10, 10, 30), paper_bgcolor = "transparent",
               plot_bgcolor = "transparent",
               xaxis = list(title = "", showgrid = FALSE),
               yaxis = list(title = "", gridcolor = "#eef2f9")) |>
        config(displayModeBar = FALSE)
    })
  }
  get_sensor_sel <- function(id) {
    v <- input[[id]]
    if (is.null(v) || !v %in% sensor_list) "temperature" else v
  }
  output$sensor_chart  <- sensor_plot(reactive(get_sensor_sel("sensor_sel"))())
  output$sensor_chart2 <- sensor_plot(reactive(get_sensor_sel("sensor_sel2"))())
  output$sensor_title <- renderUI({
    s <- sel(); req(s); paste0("Sensor Readings – ", s$disp_name)
  })
  output$sensor_legend <- renderUI({
    ls <- latest_sensor(); s <- sel(); req(s)
    cols <- c(temperature = "#ef4444", vibration = "#f59e0b", pressure = "#2563eb", rpm = "#10b981")
    lapply(names(sensor_list), function(nm) {
      st <- sensor_list[[nm]]
      v <- if (is.null(ls)) NA else ls$value[ls$machine_id == s$machine_id & ls$sensor_type == st]
      div(class = "leg", span(class = "sw", style = paste0("background:", cols[[nm]])),
          nm, tags$b(if (length(v) && !is.na(v)) round(v, 1) else "—"))
    })
  })

  ## ---------- model performance + importance ----------
  output$perf_table <- renderUI({
    reg <- tryCatch(get_active_models(), error = function(e) NULL)
    if (is.null(reg) || nrow(reg) == 0) return(empty_card("Train models first (make train)."))
    names_map <- c(failure_classifier = "XGBoost", rul_model = "Random Forest",
                   anomaly_detector = "Isolation Forest", survival_model = "Weibull AFT",
                   ensemble = "Ensemble")
    rows <- lapply(seq_len(nrow(reg)), function(i) {
      r <- reg[i, ]
      mt <- tryCatch(as.data.frame(jsonlite::fromJSON(r$metrics)), error = function(e) NULL)
      g <- function(k) if (!is.null(mt) && k %in% names(mt)) round(as.numeric(mt[[k]]), 3) else "—"
      tags$tr(class = if (r$model_name == "failure_classifier") "best",
        tags$td(names_map[[r$model_name]] %||% r$model_name),
        tags$td(g("accuracy")), tags$td(g("precision")), tags$td(g("recall")),
        tags$td(g("f1")), tags$td(g("roc_auc")))
    })
    tags$table(class = "models",
      tags$thead(tags$tr(lapply(c("Model", "Accuracy", "Precision", "Recall", "F1", "ROC-AUC"), tags$th))),
      tags$tbody(rows))
  })

  imp_df <- reactive({
    m <- tryCatch(load_active_models()$failure_classifier, error = function(e) NULL)
    req(m)
    imp <- tryCatch(get_feature_importance(m), error = function(e) NULL)
    req(!is.null(imp) && nrow(imp) > 0)
    imp$share <- 100 * imp$gain / sum(imp$gain)
    utils::head(imp[order(-imp$share), c("feature", "share")], 5)
  })
  imp_bars_ui <- function() renderUI({
    d <- imp_df()
    cols <- c("#d43a3a", "#f08c1a", "#e0a800", "#1a9e55", "#2563eb")
    lapply(seq_len(nrow(d)), function(i) div(class = "imp-row",
      span(class = "lab", gsub("^(value_|roll_mean_|roll_sd_|delta\\d+_)", "", d$feature[i])),
      span(class = "pct", paste0(round(d$share[i]), "%")),
      div(class = "imp-bar", tags$i(style = paste0("width:", max(d$share[i] * 2.2, 8),
                                                   "%;background:", cols[i %||% 1 %||% cols[i]], "")))))
  })
  output$imp_bars  <- imp_bars_ui()
  output$imp_bars2 <- imp_bars_ui()

  ## ---------- RUL forecast ----------
  output$rul_title <- renderUI({ s <- sel(); req(s); paste0("RUL Forecast – ", s$disp_name) })
  output$rul_chart <- renderPlotly({
    s <- sel(); req(s)
    rul <- if (is.na(s$rul_days) || s$rul_days > 999) 30 else s$rul_days
    t <- seq(0, 30, by = 0.5)
    y  <- pmax(rul - t, 0)
    lo <- pmax(rul - 1.15 * t - 1, 0); hi <- rul + 0.6 * t + 1
    ann <- paste0("RUL: ", floor(rul), " – ", ceiling(rul), " days")
    plot_ly() |>
      add_lines(x = t, y = hi, line = list(color = "#f0b429", dash = "dash", width = 1.2),
                name = "Upper Bound") |>
      add_lines(x = t, y = lo, line = list(color = "#f0b429", dash = "dash", width = 1.2),
                name = "Lower Bound") |>
      add_lines(x = t, y = y, line = list(color = "#2563eb", width = 3), name = "Predicted RUL") |>
      add_segments(x = rul, xend = rul, y = 0, yend = max(y),
                   line = list(color = "#d43a3a", dash = "dot", width = 1.2), showlegend = FALSE) |>
      layout(margin = list(20, 10, 10, 35), paper_bgcolor = "transparent",
             plot_bgcolor = "transparent", showlegend = TRUE, legend = list(font = list(size = 10)),
             annotations = list(x = rul, y = max(y) * 0.9, text = ann, showarrow = TRUE,
                                arrowcolor = "#d43a3a", font = list(color = "#fff", size = 11),
                                bgcolor = "#d43a3a", borderpad = 5),
             xaxis = list(title = "Days", gridcolor = "#eef2f9"),
             yaxis = list(title = "Hours", gridcolor = "#eef2f9")) |>
      config(displayModeBar = FALSE)
  })

  ## ---------- insights + distribution ----------
  output$insights <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    s <- sel()
    ins <- list(
      list("ok",   "circle-check", "Overall equipment health is stable."),
      list("bad",  "circle-check", paste0(s$disp_name, " requires immediate attention.")),
      list("warn", "triangle-exclamation", paste0(sum(x$status == "atrisk"),
            " machines are at moderate risk.")),
      list("info", "circle-info", "Predictions refresh every 60 seconds."),
      list("info", "chart-line", "System performance is within normal range."))
    lapply(ins, function(i) div(class = paste0("insight ", i[1]), icon(i[2]), i[3]))
  })
  output$dist_donut <- renderPlotly({
    x <- scored(); req(nrow(x) > 0)
    n <- c(online = sum(x$status == "online"), atrisk = sum(x$status == "atrisk"),
           critical = sum(x$status == "critical"))
    plot_ly(labels = c("Healthy", "At Risk", "Critical"), values = unname(n),
            type = "pie", hole = 0.62, sort = FALSE,
            marker = list(colors = c("#1a9e55", "#f0b429", "#d43a3a"))) |>
      layout(showlegend = FALSE, margin = list(0,0,0,0), paper_bgcolor = "transparent") |>
      config(displayModeBar = FALSE)
  })
  output$dist_center <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    HTML(paste0("<b>", nrow(x), "</b><span>Total Machines</span>"))
  })

  ## ---------- quick actions / other tabs ----------
  observeEvent(input$qa_view,  updateTabsetPanel(session, "main_tabs", selected = "predict"))
  observeEvent(input$qa_alerts, showModal(modalDialog(
    title = "Alert thresholds (from model_config.yml)",
    HTML(paste0("<b>Medium ≥ ", cfg$risk$thresholds$medium, "</b> · ",
                "<b>High ≥ ", cfg$risk$thresholds$high, "</b> · ",
                "<b>Critical ≥ ", cfg$risk$thresholds$critical, "</b><br><br>",
                "Ensemble weights — failure: ", cfg$risk$weights$failure,
                ", RUL: ", cfg$risk$weights$rul, ", anomaly: ", cfg$risk$weights$anomaly)),
    easyClose = TRUE)))
  export_name <- function() paste0("predictions_", format(Sys.Date(), "%Y%m%d"), ".csv")
  output$qa_csv  <- downloadHandler(export_name, function(file) {
    x <- scored(); req(x); readr::write_csv(x, file) })
  output$qa_csv2 <- output$qa_csv
  do_report <- function() {
    showModal(modalDialog("Rendering HTML report… this can take ~30s.", footer = NULL, easyClose = TRUE))
    out <- tryCatch({
      f <- tempfile(fileext = ".html")
      rmarkdown::render(file.path(project_root(), "reports", "final_report.Rmd"),
                        output_file = f, quiet = TRUE); f
    }, error = function(e) e$message)
    removeModal()
    if (file.exists(out)) utils::browseURL(out) else showNotification(out, type = "error")
  }
  observeEvent(input$qa_report,  do_report())
  observeEvent(input$qa_report2, do_report())

  output$pred_dt <- renderDT({
    x <- scored(); req(x)
    datatable(x[, c("disp_name", "type_lab", "failure_prob", "rul_days",
                    "anomaly_score", "risk_score", "risk_level", "predicted_at")],
              colnames = c("Machine", "Type", "Failure P", "RUL (d)", "Anomaly",
                           "Risk", "Level", "At"),
              options = list(pageLength = 10), rownames = FALSE)
  })
  output$registry_dt <- renderDT({
    reg <- tryCatch(get_active_models(), error = function(e) NULL)
    req(reg)
    datatable(reg[, c("model_name", "version", "model_type", "trained_at")], rownames = FALSE)
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
    d <- purrr::pmap_dfr(list(x$machine_id, x$disp_name, x$risk_level, x$risk_score, x$rul_days),
      ~ recommend_maintenance(..1, ..2, ..3, ..4, ..5, list(summary = "auto")))
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
    if (length(fs) == 0) empty_card("No performance snapshots yet (run: make monitor).")
    else tags$ul(lapply(fs, tags$li))
  })
  output$cfg_out <- renderPrint({
    cfg$features; cat("\n"); cfg$risk; cat("\n"); cfg$labels
  })
}

shinyApp(ui, server)
