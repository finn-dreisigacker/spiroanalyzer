# ============================================================
#  mod_vo2max_verify.R  --  VO2max-Verifikations-Tab
#
#  Visualisiert VO2 vs Zeit mit der aktuell ausgewählten
#  Mittelungs-Fensterposition. Der User kann das Fenster (Breite
#  15/20/30/45/60 Sek.) entlang der Belastungsphase verschieben
#  und über "Übernehmen" den so berechneten VO2peak in die
#  globalen Ergebnisse (Übersicht, Header, Exporte) propagieren.
#
#  Return-Reactive (für mod_single):
#    list(active = TRUE/FALSE,
#         VO2peak_abs, VO2peak_rel,
#         t_start_min, t_end_min, window_sec, source = "user"|"auto")
# ============================================================

mod_vo2max_verify_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .vv-card { background:#fff; border:1px solid #d9e4f5;
        border-radius:14px; padding:18px;
        box-shadow:0 3px 12px rgba(31,61,107,0.06);
        margin-bottom:14px; }
      .vv-card h5 { color:#1f3d6b; font-weight:800; font-size:0.95rem;
        margin:0 0 12px; padding-bottom:8px;
        border-bottom:2px solid #e5eefa; }
      .vv-row { display:flex; gap:18px; flex-wrap:wrap; align-items:center; }
      .vv-tile { background:linear-gradient(135deg,#f0f6ff,#e8f0fe);
        border:1px solid #d6e4fa; border-radius:10px;
        padding:12px 16px; min-width:160px; }
      .vv-tile-lbl { font-size:0.72rem; color:#5b7fa6; font-weight:600;
        text-transform:uppercase; letter-spacing:0.04em; }
      .vv-tile-val { font-size:1.25rem; font-weight:800; color:#1f3d6b;
        line-height:1.2; }
      .vv-tile-sub { font-size:0.72rem; color:#94a3b8; margin-top:2px; }
      .vv-auto-tile { background:linear-gradient(135deg,#fefce8,#fef9c3);
        border-color:#fde68a; }
      .vv-active-tile { background:linear-gradient(135deg,#dcfce7,#bbf7d0);
        border-color:#86efac; }
      .vv-note { font-size:0.82rem; color:#64748b; margin-top:8px; }
    ")),

    shiny::div(class = "vv-card",
      shiny::tags$h5(shiny::icon("crosshairs"),
        " VO2max-Verifikation – Mittelungsfenster auswählen"),

      shiny::tags$p(style = "color:#64748b; font-size:0.85rem; margin-bottom:14px;",
        "Das gleitende Mittel über die letzten Sekunden definiert den ",
        "VO2peak. Per Default wird das höchste 30-Sekunden-Mittel ",
        "innerhalb der Belastungsphase verwendet. Du kannst die ",
        "Fensterbreite und den Mittelpunkt anpassen und das Ergebnis ",
        "anschließend übernehmen."),

      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("win_sec"), "Fensterbreite (Sek.)",
            choices = c("15" = "15", "20" = "20", "30" = "30",
                        "45" = "45", "60" = "60"),
            selected = "30", width = "100%")),
        shiny::column(8,
          shiny::sliderInput(ns("win_center"),
            "Fenster-Mittelpunkt (min)",
            min = 0, max = 60, value = 5, step = 0.05,
            width = "100%", animate = FALSE))
      ),

      shiny::div(class = "vv-row", style = "margin-top:10px;",
        shiny::actionButton(ns("set_auto"),
          shiny::tagList(shiny::icon("magic-wand-sparkles"),
            " Auf Auto-Peak setzen"),
          class = "btn-outline-secondary btn-sm"),
        shiny::actionButton(ns("apply"),
          shiny::tagList(shiny::icon("check-circle"),
            " Übernehmen"),
          class = "btn-success btn-sm"),
        shiny::actionButton(ns("reset"),
          shiny::tagList(shiny::icon("undo"),
            " Override zurücksetzen"),
          class = "btn-outline-danger btn-sm"),
        shiny::uiOutput(ns("apply_status"), inline = TRUE)
      )
    ),

    # Kennzahlen-Tiles
    shiny::div(class = "vv-card",
      shiny::tags$h5(shiny::icon("chart-simple"), " Aktuelle Werte"),
      shiny::uiOutput(ns("kpi_row"))
    ),

    shiny::div(class = "vv-card",
      shiny::tags$h5(shiny::icon("chart-line"),
        " VO2-Verlauf mit Fenster"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("plot"), height = "460px"), type = 6),
      shiny::div(class = "vv-note",
        shiny::icon("info-circle"),
        " Belastungsphase blau hinterlegt. Das gewählte Fenster ist ",
        "grün; die rote Linie zeigt das gleitende Mittel über die ",
        "Fensterbreite (auf Belastungs-Samples).")
    )
  )
}

# ------------------------------------------------------------
mod_vo2max_verify_server <- function(id, params_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Persistenter Override-State
    override <- shiny::reactiveValues(
      active     = FALSE,
      VO2peak_abs = NA_real_,
      VO2peak_rel = NA_real_,
      t_start_min = NA_real_,
      t_end_min   = NA_real_,
      window_sec  = 30,
      source      = "auto"
    )

    # Slider-Range an Belastungsphase anpassen, sobald Daten kommen.
    shiny::observeEvent(params_reactive(), {
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      ts <- p$ts
      bel <- ts[ts$Phase %in% c("Belastung", "Exercise"), , drop = FALSE]
      if (nrow(bel) < 3) return()
      t_min <- min(bel$time_min, na.rm = TRUE)
      t_max <- max(bel$time_min, na.rm = TRUE)

      # Slider darf nur in [t_min + w/2, t_max - w/2] liegen
      w_sec <- as.numeric(input$win_sec %||% 30)
      half  <- w_sec / 60 / 2
      sl_min <- t_min + half
      sl_max <- max(sl_min + 0.05, t_max - half)

      # Default: Auto-Peak (wenn vorhanden)
      auto_t <- p$VO2peak_auto$abs$t_center_min %||% NA_real_
      default_v <- if (is.finite(auto_t))
        min(max(auto_t, sl_min), sl_max) else (sl_min + sl_max) / 2

      shiny::updateSliderInput(session, "win_center",
        min = round(sl_min, 2), max = round(sl_max, 2),
        value = round(default_v, 2), step = 0.05)

      # Override zurücksetzen, wenn neue Datei geladen wird
      override$active     <- FALSE
      override$VO2peak_abs <- NA_real_
      override$VO2peak_rel <- NA_real_
      override$source     <- "auto"
    }, ignoreInit = TRUE)

    # Slider-Range bei Breiten-Wechsel neu klemmen
    shiny::observeEvent(input$win_sec, {
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      ts <- p$ts
      bel <- ts[ts$Phase %in% c("Belastung", "Exercise"), , drop = FALSE]
      if (nrow(bel) < 3) return()
      t_min <- min(bel$time_min, na.rm = TRUE)
      t_max <- max(bel$time_min, na.rm = TRUE)
      w_sec <- as.numeric(input$win_sec)
      half  <- w_sec / 60 / 2
      sl_min <- t_min + half
      sl_max <- max(sl_min + 0.05, t_max - half)
      cur <- input$win_center %||% ((sl_min + sl_max) / 2)
      shiny::updateSliderInput(session, "win_center",
        min = round(sl_min, 2), max = round(sl_max, 2),
        value = round(min(max(cur, sl_min), sl_max), 2))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$set_auto, {
      p <- params_reactive(); shiny::req(p)
      auto_t <- p$VO2peak_auto$abs$t_center_min %||% NA_real_
      if (is.finite(auto_t)) {
        shiny::updateSliderInput(session, "win_center",
          value = round(auto_t, 2))
        shiny::updateSelectInput(session, "win_sec",
          selected = as.character(p$VO2peak_auto$window_sec %||% 30))
      }
    })

    # Live-Berechnung des aktuellen Fenster-Mittels
    current_window <- shiny::reactive({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      w_sec  <- as.numeric(input$win_sec  %||% 30)
      center <- input$win_center
      if (is.null(center) || !is.finite(center))
        return(NULL)
      half <- w_sec / 60 / 2
      t_start <- center - half
      t_end   <- center + half

      abs_res <- vo2_peak_window(p$ts, "VO2abs", window_sec = w_sec,
                                  t_min = t_start, t_max = t_end)
      rel_res <- vo2_peak_window(p$ts, "VO2kg",  window_sec = w_sec,
                                  t_min = t_start, t_max = t_end)

      # vo2_peak_window berechnet auf Belastungs-Samples; da die
      # Suchregion bereits sehr klein ist, ist der "Peak" innerhalb
      # = das eigentliche Fenstermittel.
      list(
        VO2abs   = abs_res$value,
        VO2kg    = rel_res$value,
        t_start  = abs_res$t_start_min %||% t_start,
        t_end    = abs_res$t_end_min   %||% t_end,
        n        = abs_res$n_samples,
        ok       = abs_res$ok,
        window_sec = w_sec
      )
    })

    output$kpi_row <- shiny::renderUI({
      p <- params_reactive(); shiny::req(p)
      cur <- current_window()
      auto_abs <- p$VO2peak_auto$abs %||% list(value = NA, t_center_min = NA)
      auto_rel <- p$VO2peak_auto$rel %||% list(value = NA, t_center_min = NA)

      tile <- function(label, value, sub, class = "vv-tile") {
        shiny::div(class = class,
          shiny::div(class = "vv-tile-lbl", label),
          shiny::div(class = "vv-tile-val", value),
          shiny::div(class = "vv-tile-sub", sub))
      }

      shiny::div(class = "vv-row",
        tile("Aktuelles Fenster — VO2 abs",
             if (!is.null(cur) && cur$ok) paste0(fmt(cur$VO2abs, 2), " L/min") else "–",
             sprintf("@ %s min  (%d Samples)",
                     if (!is.null(cur)) fmt((cur$t_start + cur$t_end)/2, 2) else "–",
                     if (!is.null(cur)) cur$n else 0L)),
        tile("Aktuelles Fenster — VO2 rel",
             if (!is.null(cur) && cur$ok) paste0(fmt(cur$VO2kg, 1), " ml/min/kg") else "–",
             sprintf("Breite: %s s",
                     if (!is.null(cur)) cur$window_sec else "–")),
        tile("Auto-Peak (Default)",
             if (is.finite(auto_abs$value)) paste0(fmt(auto_abs$value, 2), " L/min") else "–",
             if (is.finite(auto_abs$t_center_min)) paste0("@ ", fmt(auto_abs$t_center_min, 2), " min  /  ", fmt(auto_rel$value, 1), " ml/min/kg") else "–",
             class = "vv-tile vv-auto-tile"),
        if (override$active)
          tile("Übernommen (Override)",
               paste0(fmt(override$VO2peak_abs, 2), " L/min"),
               sprintf("@ %s min  /  %s ml/min/kg",
                       fmt((override$t_start_min + override$t_end_min)/2, 2),
                       fmt(override$VO2peak_rel, 1)),
               class = "vv-tile vv-active-tile")
        else NULL
      )
    })

    output$plot <- plotly::renderPlotly({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      ts <- p$ts
      cur <- current_window()

      w_sec <- as.numeric(input$win_sec %||% 30)
      # Rolling mean nur auf Belastung — damit Cooldown nicht verfälscht.
      bel <- ts[ts$Phase %in% c("Belastung", "Exercise"), , drop = FALSE]
      bel <- bel[order(bel$time_min), , drop = FALSE]

      # Median-Sampleabstand → Anzahl Punkte für Glättung
      dt <- suppressWarnings(stats::median(diff(bel$time_s), na.rm = TRUE))
      if (!is.finite(dt) || dt <= 0) dt <- 1
      n_pts <- max(3, round(w_sec / dt))
      bel$VO2_s <- safe_roll(bel$VO2abs, n_pts)

      auto_t <- p$VO2peak_auto$abs$t_center_min %||% NA_real_

      # Belastungs-Shading
      shapes <- list(
        list(type = "rect",
             xref = "x", yref = "paper",
             x0 = min(bel$time_min, na.rm = TRUE),
             x1 = max(bel$time_min, na.rm = TRUE),
             y0 = 0, y1 = 1,
             fillcolor = "rgba(37,99,235,0.06)",
             line = list(width = 0), layer = "below")
      )

      # Aktuelles Fenster (grün)
      if (!is.null(cur) && is.finite(cur$t_start) && is.finite(cur$t_end)) {
        shapes[[length(shapes) + 1]] <- list(type = "rect",
          xref = "x", yref = "paper",
          x0 = cur$t_start, x1 = cur$t_end,
          y0 = 0, y1 = 1,
          fillcolor = "rgba(34,197,94,0.18)",
          line = list(color = "#16a34a", width = 1.5, dash = "dash"),
          layer = "below")
      }

      # Auto-Peak vertikale Linie (gelb)
      if (is.finite(auto_t)) {
        shapes[[length(shapes) + 1]] <- list(type = "line",
          xref = "x", yref = "paper",
          x0 = auto_t, x1 = auto_t, y0 = 0, y1 = 1,
          line = list(color = "#ca8a04", width = 1.5, dash = "dot"))
      }

      annot <- list()
      if (!is.null(cur) && cur$ok) {
        annot[[length(annot) + 1]] <- list(
          x = (cur$t_start + cur$t_end) / 2,
          y = cur$VO2abs,
          text = sprintf("%.2f L/min", cur$VO2abs),
          showarrow = TRUE, arrowhead = 2, ax = 0, ay = -30,
          font = list(color = "#15803d", size = 11),
          bgcolor = "rgba(220,252,231,0.85)", borderpad = 3)
      }

      plotly::plot_ly(source = "vv_plot") |>
        plotly::add_markers(data = ts, x = ~time_min, y = ~VO2abs,
          name = "Rohdaten",
          marker = list(size = 3, color = "#94a3b8", opacity = 0.45),
          hovertemplate = "t=%{x:.2f} min<br>VO2=%{y:.2f}<extra></extra>") |>
        plotly::add_lines(data = bel, x = ~time_min, y = ~VO2_s,
          name = "VO2 geglättet (Belastung)",
          line = list(color = "#dc2626", width = 2.4)) |>
        plotly::layout(
          shapes = shapes, annotations = annot,
          xaxis = list(title = "Zeit (min)", gridcolor = "#e2e8f0"),
          yaxis = list(title = "VO2 abs [L/min]", gridcolor = "#e2e8f0",
                       rangemode = "tozero"),
          legend = list(orientation = "h", x = 0, y = 1.08),
          margin = list(l = 60, r = 30, t = 30, b = 50)) |>
        plotly::config(displaylogo = FALSE)
    })

    shiny::observeEvent(input$apply, {
      cur <- current_window()
      if (is.null(cur) || !cur$ok) {
        shiny::showNotification("Kein gültiges Fenster — bitte zuerst Position prüfen.",
                                type = "warning")
        return()
      }
      override$active      <- TRUE
      override$VO2peak_abs <- round(cur$VO2abs, 2)
      override$VO2peak_rel <- round(cur$VO2kg, 2)
      override$t_start_min <- cur$t_start
      override$t_end_min   <- cur$t_end
      override$window_sec  <- cur$window_sec
      override$source      <- "user"
      shiny::showNotification(
        sprintf("VO2peak übernommen: %.2f L/min  (%.1f ml/min/kg)",
                cur$VO2abs, cur$VO2kg),
        type = "message", duration = 3)
    })

    shiny::observeEvent(input$reset, {
      override$active      <- FALSE
      override$VO2peak_abs <- NA_real_
      override$VO2peak_rel <- NA_real_
      override$source      <- "auto"
      shiny::showNotification("Override zurückgesetzt — Auto-Peak gilt wieder.",
                              type = "message", duration = 3)
    })

    output$apply_status <- shiny::renderUI({
      if (override$active)
        shiny::tags$span(style = "color:#15803d; font-weight:600; margin-left:8px;",
          shiny::icon("check"),
          sprintf(" Aktiv: %.2f L/min", override$VO2peak_abs))
      else
        shiny::tags$span(style = "color:#94a3b8; margin-left:8px;",
          "Default: Auto-Peak")
    })

    # Reactive für mod_single zum Anwenden des Overrides
    shiny::reactive({
      list(
        active      = isTRUE(override$active),
        VO2peak_abs = override$VO2peak_abs,
        VO2peak_rel = override$VO2peak_rel,
        t_start_min = override$t_start_min,
        t_end_min   = override$t_end_min,
        window_sec  = override$window_sec,
        source      = override$source
      )
    })
  })
}
