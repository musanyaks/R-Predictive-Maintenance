## File: shiny/mon_server.R — Live Monitoring server logic
mon_server_init <- function(input, output, session, ctx) {
  fleet         <- ctx$fleet
  scored        <- ctx$scored
  latest_sensor <- ctx$latest_sensor
  alerts_df     <- ctx$alerts_df
  go_tab        <- ctx$current_tab

  # Recency is anchored to the newest reading in the DB, not wall clock,
  # so replayed/historical telemetry windows stay meaningful.
  data_now <- reactive({
    r <- fleet()$readings
    if (is.null(r) || nrow(r) == 0) now_utc() else max(r$reading_time)
  })

  sensor_meta <- list(
    temperature = list(lab = "Temperature", ic = "temperature-half",
                       unit = " C",    col = "#f59e0b"),
    vibration   = list(lab = "Vibration",   ic = "wave-square",
                       unit = " mm/s", col = "#2563eb"),
    pressure    = list(lab = "Pressure",    ic = "gauge",
                       unit = " kPa",  col = "#1a9e55"),
    rpm         = list(lab = "RPM",         ic = "fan",
                       unit = "",      col = "#7c5cff"))

  mon_sel <- reactive({
    x <- scored()
    if (is.null(x) || nrow(x) == 0) return(NULL)
    id <- input$mon_machine
    if (!is.null(id) && id %in% x$machine_id) return(x[x$machine_id == id, ][1, ])
    crit <- x[x$status == "critical", ]
    if (nrow(crit) > 0) crit[1, ] else x[1, ]
  })

  observe({
    x <- scored()
    if (is.null(x) || nrow(x) == 0) return()
    ch <- setNames(x$machine_id, paste0(x$disp_name, " (", x$type_lab, ")"))
    cur <- isolate(input$mon_machine)
    updateSelectInput(session, "mon_machine", choices = ch,
      selected = if (!is.null(cur) && cur %in% unname(ch)) cur else unname(ch)[1])
  })

  observeEvent(input$mon_view_all,    go_tab("predict"))
  observeEvent(input$mon_view_alerts, go_tab("predict"))

  output$mon_updated <- renderUI({
    invalidateLater(30000)
    paste("Updated", format(Sys.time(), "%H:%M:%S"))
  })

  output$mon_tel_title <- renderUI({
    m <- mon_sel(); req(m)
    h3(paste("Live Sensor Telemetry -", m$disp_name))
  })

  output$mon_legend <- renderUI({
    tags$div(style = paste0("display:flex;gap:13px;align-items:center;",
                            "font-size:11px;color:#54627d"),
      lapply(list(c("Temp (C)", "#ef4444"), c("Pressure (kPa)", "#1a9e55"),
                  c("Vibration (mm/s)", "#2563eb"), c("RPM /100", "#7c5cff")),
             function(x) span(
               span(class = "sw", style = paste0("background:", x[2],
                    ";display:inline-block;width:8px;height:8px;",
                    "border-radius:50%;margin-right:5px")),
               x[1])))
  })

  output$mon_kpis <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    r <- fleet()$readings
    online <- nrow(x)
    if (!is.null(r) && nrow(r) > 0) {
      active <- unique(r$machine_id[r$reading_time >=
                                      data_now() - lubridate::hours(3)])
      online <- sum(x$machine_id %in% active)
    }
    n_off <- nrow(x) - online
    n_hey <- sum(x$status == "online")
    n_wrn <- sum(x$status == "atrisk")
    n_crt <- sum(x$status == "critical")
    avg   <- round(mean(x$health))
    sub_off <- if (n_off > 0)
      paste0("<span class='off'>", n_off, " offline</span>")
    else "<span class='on'>all reporting</span>"
    div(class = "grid12",
      kpi_card("span3", "robot", "var(--blue)", "#f2f6ff", "Machines Online",
        HTML(paste0(online, " <span style='font-size:14px;color:#7b8aa5'>/ ",
                    nrow(x), "</span>")),
        HTML(sub_off), "chart-line"),
      kpi_card("span2", "heart-pulse", "var(--green)", "#f0fbf4", "Healthy",
        n_hey, paste0(round(100 * n_hey / nrow(x)), "% of total"),
        "arrow-trend-up"),
      kpi_card("span2", "triangle-exclamation", "var(--amber)", "#fffaef",
        "Warnings", n_wrn,
        paste0(round(100 * n_wrn / nrow(x)), "% of total"),
        "triangle-exclamation"),
      kpi_card("span2", "triangle-exclamation", "var(--red)", "#fef1f1",
        "Critical", n_crt,
        paste0(round(100 * n_crt / nrow(x)), "% of total"), "chart-line"),
      kpi_card("span2", "chart-simple", "var(--purple)", "#f6f2ff",
        "Avg. Fleet Health", paste0(avg, "%"), "mean of health scores",
        "arrow-trend-up"))
  })

  output$mon_chart <- renderPlotly({
    m <- mon_sel(); req(m)
    r <- fleet()$readings; req(r, nrow(r) > 0)
    d <- r[r$machine_id == m$machine_id &
           r$reading_time >= data_now() - lubridate::hours(12), ]
    req(nrow(d) > 5)
    w <- d |>
      dplyr::group_by(reading_time, sensor_type) |>
      dplyr::summarise(value = mean(value), .groups = "drop") |>
      tidyr::pivot_wider(names_from = sensor_type, values_from = value)
    p <- plot_ly()
    series <- list(
      list(n = "temperature", col = "#ef4444", lab = "Temp (C)",         f = 1),
      list(n = "pressure",    col = "#1a9e55", lab = "Pressure (kPa)",   f = 1),
      list(n = "vibration",   col = "#2563eb", lab = "Vibration (mm/s)", f = 1),
      list(n = "rpm",         col = "#7c5cff", lab = "RPM /100",        f = 100))
    for (s in series) {
      if (s$n %in% names(w)) {
        p <- plotly::add_lines(p, x = w$reading_time, y = w[[s$n]] / s$f,
                               line = list(color = s$col, width = 2),
                               name = s$lab)
      }
    }
    p |> layout(margin = list(20, 10, 10, 40),
                paper_bgcolor = "transparent", plot_bgcolor = "transparent",
                legend = list(orientation = "h", y = -0.22,
                              font = list(size = 10)),
                xaxis = list(title = "", showgrid = FALSE),
                yaxis = list(title = "", gridcolor = "#eef2f9")) |>
      config(displayModeBar = FALSE)
  })

  output$mon_side <- renderUI({
    m <- mon_sel(); req(m)
    ls <- latest_sensor(); req(!is.null(ls))
    labs <- c(temperature = "Temp", pressure = "Pressure",
              vibration = "Vibration", rpm = "RPM")
    lapply(c("temperature", "pressure", "vibration", "rpm"), function(st) {
      v <- ls$value[ls$machine_id == m$machine_id & ls$sensor_type == st]
      div(class = "leg",
          span(class = "sw",
               style = paste0("background:", sensor_colors[[st]])),
          labs[[st]],
          tags$b(if (length(v)) round(v, 1) else "---"))
    })
  })

  output$mon_badge <- renderUI({
    m <- mon_sel(); req(m)
    if (m$status == "critical") span(class = "badge-critical", "CRITICAL")
    else if (m$status == "atrisk") span(class = "badge-warn", "WARNING")
    else span(class = "badge-run", "RUNNING")
  })

  output$mon_details <- renderUI({
    m <- mon_sel(); req(m)
    src <- machine_img(m$machine_name)
    img <- if (file.exists(file.path(project_root(), "shiny", "www", src)))
      tags$img(src = src, alt = m$machine_name, style = "width:120px;height:92px;object-fit:cover;border-radius:10px;flex-shrink:0")
    else div(class = "photo-tile", icon("industry"))
    rul_h <- if (is.na(m$rul_days) || m$rul_days > 999) "720+ h"
             else paste0(round(m$rul_days * 24), " h")
    tagList(
      div(class = "detail-top", img,
          div(h3(m$disp_name), div(class = "mini", m$machine_name))),
      div(class = "detail-metrics",
          div(style = "width:92px;flex-shrink:0",
              plotlyOutput("mon_donut", height = "92px", width = "92px")),
          div(class = "dm",
              div(class = "lab", "Failure Risk"),
              div(class = "val red",
                  paste0(round(100 * m$failure_prob), "%"))),
          div(class = "dm",
              div(class = "lab", icon("clock"), "Est. RUL"),
              div(class = "val", rul_h))),
      div(class = "chips",
          div(class = "chip", "Type", tags$b(m$type_lab)),
          div(class = "chip", "Location",
              tags$b(m$location %||% "---")),
          div(class = "chip", "Model",
              tags$b(paste0("PM-", sprintf("%03d", m$machine_id))))))
  })

  output$mon_donut <- renderPlotly({
    m <- mon_sel(); req(m)
    donut(c(m$health, max(100 - m$health, 0)),
          c(health_col(m$health), "#e8edf5"),
          paste0("<b>", round(m$health), "</b><br>Health"),
          w = 92, h = 92)
  })

  output$mon_minis <- renderUI({
    m <- mon_sel(); req(m)
    r <- fleet()$readings; req(r, nrow(r) > 0)
    cards <- lapply(names(sensor_meta), function(nm) {
      d <- r[r$machine_id == m$machine_id & r$sensor_type == nm, ]
      d <- d[order(d$reading_time), ]
      if (nrow(d) < 6) return(NULL)
      now_v <- mean(tail(d$value, 3))
      tmax  <- max(d$reading_time)
      past  <- d$value[d$reading_time >= tmax - lubridate::hours(2) &
                       d$reading_time <  tmax - lubridate::hours(1)]
      delta <- if (length(past) >= 2 && mean(past) != 0)
        100 * (now_v - mean(past)) / mean(past) else 0
      dcls  <- if (abs(delta) < 2) "flat"
               else if (delta > 0) "up" else "down"
      arrow <- if (dcls == "up") "&uarr;"
               else if (dcls == "down") "&darr;" else "&harr;"
      sm    <- sensor_meta[[nm]]
      div(class = "mini-card", style = "grid-column:span 2",
          div(class = "lab",
              icon(sm$ic, style = paste0("color:", sm$col)), sm$lab),
          div(class = "val", paste0(round(now_v, 1), sm$unit)),
          div(class = paste0("delta ", dcls),
              HTML(paste0(arrow, " ", round(abs(delta), 1),
                          "% vs last hour"))),
          plotlyOutput(paste0("spark_", nm), height = "38px"))
    })
    cards <- Filter(Negate(is.null), cards)
    req(length(cards) > 0)
    do.call(tagList, cards)
  })

  for (sn in names(sensor_meta)) {
    local({
      sn_ <- sn
      output[[paste0("spark_", sn_)]] <- renderPlotly({
        m <- mon_sel(); req(m)
        r <- fleet()$readings; req(r, nrow(r) > 0)
        d <- r[r$machine_id == m$machine_id & r$sensor_type == sn_, ]
        d <- d[order(d$reading_time), ]
        req(nrow(d) >= 5)
        d <- tail(d, 24)
        plot_ly(d, x = ~reading_time, y = ~value,
                type = "scatter", mode = "lines",
                line = list(color = sensor_meta[[sn_]]$col, width = 1.6),
                hoverinfo = "none") |>
          layout(margin = list(0, 0, 0, 0),
                 paper_bgcolor = "transparent",
                 plot_bgcolor = "transparent",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)) |>
          config(displayModeBar = FALSE, staticPlot = TRUE)
      })
    })
  }

  output$mon_status_table <- renderUI({
    x <- scored(); req(nrow(x) > 0)
    lab <- c(online = "Healthy", atrisk = "Warning", critical = "Critical")
    rows <- lapply(seq_len(nrow(x)), function(i) {
      rr <- x[i, ]
      tags$tr(
        tags$td(class = "mini", rr$machine_name),
        tags$td(class = "mname", rr$disp_name),
        tags$td(pretty_type(rr$machine_type)),
        tags$td(span(class = paste0("pill ", rr$status),
                     lab[[rr$status]])))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("Machine ID", "Machine Name", "Type",
                                 "Status"), tags$th))),
      tags$tbody(rows))
  })

  output$mon_alert_count <- renderUI({
    n <- nrow(alerts_df())
    if (n > 0) span(class = "alert-count", n) else NULL
  })

  output$mon_alerts_table <- renderUI({
    a <- alerts_df()
    if (nrow(a) == 0) {
      return(div(class = "empty", icon("circle-check"),
                 "Sensor stream stable - no active alerts"))
    }
    pl <- c(critical = "critical", high = "high",
            medium = "medium", drift = "drift")
    rows <- lapply(seq_len(min(8, nrow(a))), function(i) {
      rr <- a[i, ]
      tags$tr(
        tags$td(span(class = paste0("pill ",
                    pl[[rr$alert_level]] %||% "info"),
                    toupper(rr$alert_level))),
        tags$td(rr$message),
        tags$td(class = "mini", rr$machine_id),
        tags$td(class = "mname", rr$machine_name),
        tags$td(class = "mini", format(rr$created_at, "%H:%M:%S")))
    })
    tags$table(class = "status",
      tags$thead(tags$tr(lapply(c("Priority", "Message", "Machine ID",
                                 "Machine Name", "Time"), tags$th))),
      tags$tbody(rows))
  })

  output$mon_banner <- renderUI({
    a <- alerts_df()
    n_crt <- sum(a$alert_level == "critical", na.rm = TRUE)
    if (n_crt > 0) {
      div(class = "banner warn",
          div(class = "bicon", icon("triangle-exclamation")),
          div(tags$b("Attention required"),
              small(paste0(n_crt, " critical alert(s) open"))),
          span(class = "art", icon("industry")))
    } else {
      div(class = "banner ok",
          div(class = "bicon", icon("check")),
          div(tags$b("All systems operational"),
              small("No critical issues detected")),
          span(class = "art", icon("industry")))
    }
  })
}
