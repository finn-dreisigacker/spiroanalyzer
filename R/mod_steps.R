# ============================================================
#  mod_steps.R  –  Stufenzusammenfassung (UI + Server)
#
#  Kopf-Spalten sind klickbar → MathJax-Modal mit Formel + Quelle.
#  MFO-Berechnung als opt-in Switch unter der Tabelle.
#  MFO-Plot im Stil von Maunder et al. (2018, Front Physiol).
# ============================================================

mod_steps_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::withMathJax(),
    shiny::tags$style(shiny::HTML("
      .steps-summary { padding: 8px 0; }

      /* Kontroll-Leiste mit dezentem Farbverlauf */
      .steps-controls {
        background: linear-gradient(135deg, #f0f9ff 0%, #f8fafc 100%);
        border: 1px solid #bae6fd; border-radius: 12px;
        padding: 14px 18px; margin-bottom: 14px;
        display: flex; flex-wrap: wrap; gap: 22px; align-items: center;
      }
      .steps-controls label { font-weight: 600; color: #0c4a6e;
        font-size: 0.85rem; margin-bottom: 2px; }
      .steps-info { font-size: 0.78rem; color: #0369a1; margin-left: auto; }

      /* Tabelle */
      .steps-table { width: 100%; border-collapse: collapse;
        font-size: 0.86rem; }
      .steps-table th {
        background: linear-gradient(180deg, #f1f5f9 0%, #e2e8f0 100%);
        color: #0f172a; font-weight: 700;
        padding: 8px 10px; border-bottom: 2px solid #94a3b8;
        text-align: right; white-space: nowrap;
      }
      .steps-table th.col-clickable {
        cursor: pointer; transition: background .15s, color .15s;
        position: relative; user-select: none;
      }
      .steps-table th.col-clickable:hover {
        background: linear-gradient(180deg, #dbeafe 0%, #bfdbfe 100%);
        color: #1d4ed8;
      }
      .steps-table th.col-clickable::after {
        content: ' \\f05a'; font-family: 'FontAwesome';
        font-size: 0.65rem; color: #94a3b8; opacity: 0.7;
      }
      .steps-table th:nth-child(1),
      .steps-table th:nth-child(2),
      .steps-table th:nth-child(3) { text-align: left; }
      .steps-table td {
        padding: 6px 10px; border-bottom: 1px solid #f1f5f9;
        text-align: right; font-variant-numeric: tabular-nums;
      }
      .steps-table td:nth-child(1) { font-weight: 700; color: #1f3d6b; }
      .steps-table td:nth-child(2),
      .steps-table td:nth-child(3) { text-align: left; }

      /* Phase-Pills (farbiger Tag) */
      .phase-pill {
        display: inline-block; padding: 2px 10px; border-radius: 12px;
        font-size: 0.74rem; font-weight: 700; letter-spacing: 0.02em;
        text-transform: uppercase;
      }
      .phase-pill.warmup { background: #fef3c7; color: #92400e;
        border: 1px solid #fcd34d; }
      .phase-pill.load   { background: #dbeafe; color: #1e3a8a;
        border: 1px solid #93c5fd; }
      .phase-pill.cool   { background: #ede9fe; color: #5b21b6;
        border: 1px solid #c4b5fd; }

      /* Zeilen-Akzent */
      .steps-table tr.row-warmup td { background: rgba(254,243,199,0.20); }
      .steps-table tr.row-load   td { background: rgba(219,234,254,0.18); }
      .steps-table tr.row-cool   td { background: rgba(237,233,254,0.20); }
      .steps-table tr:hover td { background: #f0f9ff !important; }

      /* MFO-Sektion unter der Tabelle */
      .mfo-section {
        background: #fff; border: 1px solid #e2e8f0; border-radius: 14px;
        padding: 18px 20px; margin-top: 20px;
      }
      .mfo-toggle-row {
        display: flex; align-items: center; gap: 14px;
        flex-wrap: wrap;
      }
      .mfo-toggle-row .mfo-question {
        font-size: 0.95rem; font-weight: 700; color: #1f3d6b;
        display: flex; align-items: center; gap: 8px;
      }
      .mfo-toggle-row .mfo-question .fa { color: #f59e0b; }
      .mfo-disabled-hint {
        font-size: 0.82rem; color: #94a3b8; margin-left: 6px;
        font-style: italic;
      }

      .mfo-cards {
        display: flex; gap: 14px; flex-wrap: wrap; margin-top: 16px;
      }
      .mfo-card {
        flex: 1; min-width: 200px;
        border-radius: 14px; padding: 16px 18px;
        display: flex; align-items: center; gap: 14px;
        box-shadow: 0 4px 14px rgba(31,61,107,.05);
      }
      .mfo-card.mfo-orange {
        background: linear-gradient(135deg, #fff7ed 0%, #ffedd5 100%);
        border: 1px solid #fb923c;
      }
      .mfo-card.mfo-blue {
        background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
        border: 1px solid #60a5fa;
      }
      .mfo-icon { font-size: 1.7rem; }
      .mfo-card.mfo-orange .mfo-icon { color: #c2410c; }
      .mfo-card.mfo-blue   .mfo-icon { color: #1d4ed8; }
      .mfo-num { font-size: 1.45rem; font-weight: 800; line-height: 1.1; }
      .mfo-card.mfo-orange .mfo-num { color: #7c2d12; }
      .mfo-card.mfo-blue   .mfo-num { color: #1e3a8a; }
      .mfo-lbl { font-size: 0.74rem; font-weight: 700;
        text-transform: uppercase; letter-spacing: 0.04em; }
      .mfo-card.mfo-orange .mfo-lbl { color: #9a3412; }
      .mfo-card.mfo-blue   .mfo-lbl { color: #1e40af; }
      .mfo-card .mfo-sub { font-size: 0.78rem; color: #64748b;
        margin-top: 3px; }

      .mfo-warning {
        background: #fff7ed; border: 1px solid #fdba74;
        border-radius: 10px; padding: 10px 14px; margin-top: 14px;
        font-size: 0.86rem; color: #9a3412;
      }
      .mfo-ref {
        font-size: 0.78rem; color: #64748b; margin-top: 14px;
        background: #f8fafc; border-left: 3px solid #94a3b8;
        padding: 8px 12px; border-radius: 4px;
      }
      .mfo-ref a { color: #2563eb; text-decoration: none; }
      .mfo-ref a:hover { text-decoration: underline; }

      .steps-empty { color: #94a3b8; padding: 24px; text-align: center;
        font-style: italic; }
      .formula-modal-body { padding: 8px 4px; line-height: 1.7; }
      .formula-modal-body .ref {
        margin-top: 14px; font-size: 0.82rem; color: #64748b;
        background: #f8fafc; border-left: 3px solid #94a3b8;
        padding: 8px 12px; border-radius: 4px;
      }
      .formula-display {
        background: linear-gradient(135deg, #f8fafc 0%, #eff6ff 100%);
        border: 1px solid #bfdbfe; border-radius: 8px;
        padding: 14px; font-size: 1.05rem; text-align: center;
        margin: 10px 0;
      }

      /* Custom-Switch (bslib) etwas auffälliger */
      .form-switch .form-check-input:checked {
        background-color: #f59e0b; border-color: #f59e0b;
      }
    ")),

    # JS: Klick auf Spaltenkopf → Shiny Event
    shiny::tags$script(shiny::HTML(sprintf("
      $(document).on('click', '#%s .col-clickable', function() {
        var col = $(this).data('col');
        Shiny.setInputValue('%s',
          {col: col, nonce: Math.random()},
          {priority: 'event'});
      });
    ", ns("steps_table_wrap"), ns("show_formula")))),

    shiny::div(class = "steps-summary",
      # Kontroll-Leiste: nur Hinweis sichtbar, Parameter im Advanced-Panel
      shiny::div(class = "steps-controls",
        shiny::div(class = "steps-info", style = "margin-left: 0;",
          shiny::icon("hand-pointer"),
          " Spaltenkopf anklicken → Formel & Quelle anzeigen.")
      ),

      # Erweiterte Einstellungen (eingeklappt)
      shiny::tags$details(style = "margin-bottom:14px;",
        shiny::tags$summary(
          style = "cursor:pointer; font-size:0.85rem; font-weight:600; color:#0c4a6e; padding:6px 10px; border:1px solid #bae6fd; border-radius:8px; background:#f0f9ff; display:inline-block;",
          shiny::icon("sliders"), " Erweiterte Einstellungen"
        ),
        shiny::div(style = "margin-top:10px; padding:14px 18px; background:#f8fafc; border:1px solid #e2e8f0; border-radius:10px; display:flex; flex-wrap:wrap; gap:22px; align-items:flex-start;",
          shiny::div(
            shiny::tags$label(shiny::icon("clock"),
              " Mittelungs-Fenster (Sek.)",
              style = "font-weight:600; color:#0c4a6e; font-size:0.85rem;"),
            shiny::tags$div(style = "font-size:0.75rem; color:#64748b; margin-bottom:4px;",
              "Letzte X Sek. jeder Stufe werden gemittelt."),
            shiny::numericInput(ns("window_sec"),
              label = NULL, value = 30,
              min = 5, max = 120, step = 5, width = "120px")),
          shiny::div(
            shiny::tags$label(shiny::icon("ruler-vertical"),
              " Min. Stufendauer (Sek.)",
              style = "font-weight:600; color:#0c4a6e; font-size:0.85rem;"),
            shiny::tags$div(style = "font-size:0.75rem; color:#64748b; margin-bottom:4px;",
              "Kürzere Stufen werden ausgeschlossen."),
            shiny::numericInput(ns("min_step"),
              label = NULL, value = 15,
              min = 5, max = 120, step = 5, width = "120px")),
          shiny::div(
            shiny::tags$label(shiny::icon("weight"),
              " Körpergewicht (kg)",
              style = "font-weight:600; color:#0c4a6e; font-size:0.85rem;"),
            shiny::tags$div(style = "font-size:0.75rem; color:#64748b; margin-bottom:4px;",
              "Override für VO₂/kg-Berechnung (leer = aus Datei)."),
            shiny::numericInput(ns("weight"),
              label = NULL, value = NA,
              min = 30, max = 200, step = 0.5, width = "120px"))
        )
      ),

      # Tabelle
      shiny::div(class = "sa-card",
        shiny::tags$h4(shiny::icon("layer-group"),
          " Stufen-Übersicht"),
        shiny::div(id = ns("steps_table_wrap"),
          shiny::uiOutput(ns("steps_table_ui")))),

      # ─────────── MFO-Sektion (opt-in) ───────────
      shiny::div(class = "mfo-section",
        shiny::div(class = "mfo-toggle-row",
          shiny::div(class = "mfo-question",
            shiny::icon("fire"),
            " MFO & Fatmax aus den Belastungsstufen berechnen?"),
          shiny::div(
            bslib::input_switch(
              id    = ns("mfo_on"),
              label = "MFO berechnen",
              value = FALSE)
          ),
          shiny::uiOutput(ns("mfo_status_hint"), inline = TRUE)
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("mfo_on")),
          shiny::uiOutput(ns("mfo_cards")),
          shiny::uiOutput(ns("mfo_warning_box")),
          plotly::plotlyOutput(ns("mfo_plot"), height = "420px"),
          shiny::div(class = "mfo-ref",
            shiny::icon("book"),
            shiny::HTML(paste0(
              " Methode nach <strong>Achten & Jeukendrup (2003)</strong>, ",
              "Int J Sports Med 24:603–608. ",
              "Visualisierung in Anlehnung an ",
              "<strong>Maunder et al. (2018)</strong>, ",
              "<a href='https://www.frontiersin.org/journals/physiology/articles/10.3389/fphys.2018.00599/full' ",
              "target='_blank'>Front Physiol 9:599</a>: ",
              "Polynom 2. Grades durch FO–Power, MFO = Maximum, ",
              "Fatmax = Power am Maximum.")))
        )
      )
    )
  )
}


mod_steps_server <- function(id, params_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Init: Gewicht aus Metadaten übernehmen
    shiny::observeEvent(params_reactive(), {
      p <- params_reactive(); shiny::req(p)
      w <- p$Weight_kg %||% NA_real_
      if (is.finite(w))
        shiny::updateNumericInput(session, "weight", value = round(w, 1))
    })

    summary_r <- shiny::reactive({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      w <- input$weight
      if (!is.finite(w)) w <- p$Weight_kg %||% NA_real_
      ws <- input$window_sec %||% 30
      ms <- input$min_step %||% 15
      build_step_summary(p$ts,
        weight_kg    = w,
        window_sec   = if (is.finite(ws) && ws > 0) ws else 30,
        min_step_sec = if (is.finite(ms) && ms > 0) ms else 15)
    })

    n_belastung_steps <- shiny::reactive({
      df <- tryCatch(summary_r(), error = function(e) NULL)
      if (is.null(df)) return(0L)
      sum(df$type == "Belastung", na.rm = TRUE)
    })

    mfo_r <- shiny::reactive({
      df <- tryCatch(summary_r(), error = function(e) NULL)
      calc_MFO(df)
    })

    # Status-Hinweis am Toggle (z.B. "nur 2 Stufen vorhanden")
    output$mfo_status_hint <- shiny::renderUI({
      n <- n_belastung_steps()
      if (n < 3) {
        shiny::span(class = "mfo-disabled-hint",
          shiny::icon("circle-info"),
          sprintf(" %d Stufe(n) – ab 3 Stufen sinnvoll.", n))
      } else {
        shiny::span(class = "mfo-disabled-hint",
          shiny::icon("circle-check"),
          sprintf(" %d Belastungs-Stufen erkannt.", n))
      }
    })

    # ── MFO-Karten ────────────────────────────────────────
    output$mfo_cards <- shiny::renderUI({
      mf <- mfo_r()
      mfo_txt <- if (isTRUE(mf$ok))
        sprintf("%.2f g·min⁻¹", mf$MFO) else "–"
      fmax_txt <- if (isTRUE(mf$ok))
        sprintf("%.0f W", mf$Fatmax_W) else "–"
      sub_txt <- if (isTRUE(mf$ok))
        "am Scheitel der Polynom​funktion" else "Berechnung nicht möglich"
      shiny::div(class = "mfo-cards",
        shiny::div(class = "mfo-card mfo-orange",
          shiny::tags$i(class = "fa fa-fire mfo-icon"),
          shiny::div(
            shiny::div(class = "mfo-lbl", "MFO"),
            shiny::div(class = "mfo-num", mfo_txt),
            shiny::div(class = "mfo-sub", sub_txt))),
        shiny::div(class = "mfo-card mfo-blue",
          shiny::tags$i(class = "fa fa-bolt mfo-icon"),
          shiny::div(
            shiny::div(class = "mfo-lbl", "Fatmax (Power)"),
            shiny::div(class = "mfo-num", fmax_txt),
            shiny::div(class = "mfo-sub",
              "Belastung mit höchster Fettoxidation"))))
    })

    # Warn-Box bei Problemen
    output$mfo_warning_box <- shiny::renderUI({
      mf <- mfo_r()
      if (isTRUE(mf$ok) || is.null(mf) || is.null(mf$reason)) return(NULL)
      shiny::div(class = "mfo-warning",
        shiny::icon("triangle-exclamation"),
        " ", mf$reason, ".")
    })

    # ── MFO-Plot (Frontiers-Stil) ─────────────────────────
    output$mfo_plot <- plotly::renderPlotly({
      mf <- mfo_r()
      shiny::req(mf, !is.null(mf$x), length(mf$x) >= 1)

      x  <- mf$x; y <- mf$y
      fx <- mf$fit_x; fy <- mf$fit_y

      # Achsen-Bereiche
      x_max <- max(c(x, fx), na.rm = TRUE) * 1.05
      x_min <- 0
      y_max <- max(c(y, fy, mf$MFO), na.rm = TRUE)
      if (!is.finite(y_max) || y_max <= 0) y_max <- max(y, na.rm = TRUE)
      y_max <- y_max * 1.15
      y_min <- 0

      shapes <- list()
      annots <- list()

      if (isTRUE(mf$ok)) {
        # Horizontale MFO-Linie
        shapes[[length(shapes) + 1]] <- list(
          type = "line", x0 = x_min, x1 = mf$Fatmax_W,
          y0 = mf$MFO, y1 = mf$MFO,
          line = list(color = "#ea580c", width = 1.5, dash = "dash"))
        # Vertikale Fatmax-Linie
        shapes[[length(shapes) + 1]] <- list(
          type = "line",
          x0 = mf$Fatmax_W, x1 = mf$Fatmax_W,
          y0 = y_min, y1 = mf$MFO,
          line = list(color = "#1d4ed8", width = 1.5, dash = "dash"))
        # MFO-Label
        annots[[length(annots) + 1]] <- list(
          x = x_min, y = mf$MFO, xanchor = "left", yanchor = "bottom",
          text = sprintf("<b>MFO = %.2f g·min⁻¹</b>", mf$MFO),
          showarrow = FALSE,
          font = list(color = "#9a3412", size = 12),
          bgcolor = "rgba(255,237,213,0.9)",
          bordercolor = "#fb923c", borderpad = 4)
        # Fatmax-Label
        annots[[length(annots) + 1]] <- list(
          x = mf$Fatmax_W, y = y_min, xanchor = "left", yanchor = "bottom",
          text = sprintf("<b>Fatmax = %.0f W</b>", mf$Fatmax_W),
          showarrow = FALSE,
          font = list(color = "#1e3a8a", size = 12),
          bgcolor = "rgba(219,234,254,0.9)",
          bordercolor = "#60a5fa", borderpad = 4,
          ax = 8, ay = 0)
      }

      # Build plot
      p <- plotly::plot_ly()

      # Polynomfit-Kurve (gestrichelt)
      if (length(fx) > 1) {
        p <- p |> plotly::add_lines(
          x = fx, y = fy,
          name = "Polynom 2. Grades",
          line = list(color = "#9ca3af", width = 1.6, dash = "dash"),
          hovertemplate = "P: %{x:.0f} W<br>FO (fit): %{y:.2f} g/min<extra></extra>")
      }

      # Datenpunkte (Quadrate, schwarz wie im Frontiers-Plot)
      p <- p |> plotly::add_markers(
        x = x, y = y,
        name = "Stufenwerte",
        marker = list(size = 9, color = "#0f172a",
          symbol = "square", line = list(color = "#fff", width = 1)),
        hovertemplate = "P: %{x:.0f} W<br>FO: %{y:.2f} g/min<extra></extra>")

      # MFO-Punkt highlighten
      if (isTRUE(mf$ok)) {
        p <- p |> plotly::add_markers(
          x = mf$Fatmax_W, y = mf$MFO,
          name = "MFO",
          marker = list(size = 13, color = "#f59e0b",
            symbol = "diamond", line = list(color = "#7c2d12", width = 2)),
          hovertemplate = sprintf(
            "<b>MFO = %.2f g/min</b><br>Fatmax = %.0f W<extra></extra>",
            mf$MFO, mf$Fatmax_W))
      }

      p |> plotly::layout(
        title = list(text = ""),
        xaxis = list(title = "Power [W]",
          range = c(x_min, x_max),
          zeroline = FALSE, gridcolor = "#e2e8f0", griddash = "dash"),
        yaxis = list(title = "Fettoxidation [g·min⁻¹]",
          range = c(y_min, y_max),
          zeroline = FALSE, gridcolor = "#e2e8f0", griddash = "dash"),
        shapes = shapes, annotations = annots,
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = 1.08,
          font = list(size = 10)),
        margin = list(t = 40, b = 50, l = 60, r = 20),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#fafbfc"
      ) |>
      plotly::config(displaylogo = FALSE)
    })

    # ── Tabelle ────────────────────────────────────────────
    output$steps_table_ui <- shiny::renderUI({
      df <- tryCatch(summary_r(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        return(shiny::div(class = "steps-empty",
          shiny::icon("triangle-exclamation"),
          " Keine Stufen erkannt. ",
          "(Daten enthalten möglicherweise eine Rampe statt Stufen,",
          " oder die Mindestdauer ist zu hoch.)"))
      }

      fmt_min <- function(t) {
        if (!is.finite(t)) return("–")
        m <- floor(t); s <- round((t - m) * 60)
        sprintf("%02d:%02d", as.integer(m), as.integer(s))
      }
      fmt_n <- function(v, dp = 2) {
        if (!is.finite(v)) "–" else format(round(v, dp), nsmall = dp)
      }
      phase_pill <- function(type) {
        cls <- switch(type,
          "WarmUp" = "warmup",
          "Belastung" = "load",
          "Cooldown" = "cool",
          "")
        sprintf('<span class="phase-pill %s">%s</span>', cls, type)
      }
      row_class <- function(type) {
        switch(type,
          "WarmUp" = "row-warmup",
          "Belastung" = "row-load",
          "Cooldown" = "row-cool",
          "")
      }

      cols <- list(
        list("Nr",         "no",          0,    NULL),
        list("Bezeichnung","label",       NULL, NULL),
        list("Phase",      "type",        "ph", NULL),
        list("Start",      "t_start_min", "tt", NULL),
        list("Ende",       "t_end_min",   "tt", NULL),
        list("Dauer [s]",  "duration_sec",0,    NULL),
        list("P [W]",      "P",           0,    NULL),
        list("V'O₂ [L/min]", "VO2",  3,    NULL),
        list("V'O₂/kg [ml/min/kg]","VO2_kg", 1, "vo2kg"),
        list("V'CO₂ [L/min]","VCO2", 3,    NULL),
        list("V'E [L/min]","VE",          1,    NULL),
        list("HR [bpm]",   "HR",          0,    NULL),
        list("RER",        "RER",         2,    "rer"),
        list("FO [g/min]", "FO_g",        2,    "fo"),
        list("CHO [g/min]","CHO_g",       2,    "cho"),
        list("%FO",        "FO_pct",      1,    "fo_pct"),
        list("%CHO",       "CHO_pct",     1,    "cho_pct"),
        list("ET [kcal/min]","ET_kcal_min",2,   "et"),
        list("ET [kJ/min]","ET_kJ_min",   1,    "et")
      )

      header_html <- '<thead><tr>'
      for (c in cols) {
        cls <- if (!is.null(c[[4]])) ' class="col-clickable"' else ""
        att <- if (!is.null(c[[4]]))
          sprintf(' data-col="%s"', c[[4]]) else ""
        header_html <- paste0(header_html,
          "<th", cls, att, ">", c[[1]], "</th>")
      }
      header_html <- paste0(header_html, "</tr></thead>")

      body <- ""
      for (i in seq_len(nrow(df))) {
        r <- df[i, ]
        body <- paste0(body, '<tr class="', row_class(r$type), '">')
        for (c in cols) {
          key <- c[[2]]; dp <- c[[3]]
          v <- r[[key]]
          if (identical(dp, "ph")) {
            txt <- phase_pill(as.character(v))
          } else if (identical(dp, "tt")) {
            txt <- fmt_min(v)
          } else if (is.null(dp)) {
            txt <- if (is.na(v)) "–" else as.character(v)
          } else if (is.na(v)) {
            txt <- "–"
          } else if (dp == 0) {
            txt <- as.character(round(v))
          } else {
            txt <- fmt_n(v, dp)
          }
          if (key == "no" && is.na(v)) txt <- ""
          body <- paste0(body, "<td>", txt, "</td>")
        }
        body <- paste0(body, "</tr>")
      }
      shiny::HTML(paste0(
        '<div style="overflow-x:auto;">',
        '<table class="steps-table">',
        header_html,
        '<tbody>', body, '</tbody>',
        '</table></div>'))
    })

    # ── Formel-Modal bei Klick auf Spaltenkopf ──────────────
    shiny::observeEvent(input$show_formula, {
      col <- input$show_formula$col
      ftab <- formula_definitions()
      f <- ftab[[col]]
      if (is.null(f)) return()
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("function"), " ", f$title),
        shiny::withMathJax(
          shiny::div(class = "formula-modal-body",
            shiny::tags$p(f$desc),
            shiny::div(class = "formula-display",
              shiny::HTML(f$formula)),
            shiny::div(class = "ref",
              shiny::icon("book"), " ", f$ref))),
        easyClose = TRUE,
        size = "m",
        footer = shiny::modalButton("Schließen")))
    })

    # Erweiterte Einstellungen reactive für Export
    advanced_settings_r <- shiny::reactive({
      list(
        window_sec = input$window_sec,
        min_step   = input$min_step,
        weight     = input$weight
      )
    })

    return(list(
      summary           = summary_r,
      mfo               = mfo_r,
      advanced_settings = advanced_settings_r
    ))
  })
}


# ── Formel-Texte (MathJax) ───────────────────────────────────
formula_definitions <- function() {
  list(
    rer = list(
      title  = "RER (Respiratorischer Quotient)",
      desc   = "Verhältnis von CO₂-Abgabe zu O₂-Aufnahme.",
      formula = "$$ \\mathrm{RER} = \\frac{\\dot{V}\\mathrm{CO}_2}{\\dot{V}\\mathrm{O}_2} $$",
      ref    = "Standardgleichung; vgl. Wasserman et al., Principles of Exercise Testing."
    ),
    vo2kg = list(
      title  = "V'O₂ / kg (Sauerstoffaufnahme pro Körpergewicht)",
      desc   = "Sauerstoffaufnahme normiert auf das Körpergewicht.",
      formula = "$$ \\dot{V}\\mathrm{O}_{2}\\,/\\,\\mathrm{kg} \\; [\\mathrm{ml \\cdot min^{-1} \\cdot kg^{-1}}] = \\frac{\\dot{V}\\mathrm{O}_{2}\\,[\\mathrm{L \\cdot min^{-1}}] \\cdot 1000}{m\\,[\\mathrm{kg}]} $$",
      ref    = "Konvention: Umrechnung L → mL durch ×1000."
    ),
    fo = list(
      title  = "FO – Fettoxidation [g·min⁻¹]",
      desc   = "Fettoxidationsrate pro Minute aus Atemgasen.",
      formula = "$$ \\mathrm{FO}\\;[\\mathrm{g \\cdot min^{-1}}] = 1{,}695 \\cdot \\dot{V}\\mathrm{O}_2 - 1{,}701 \\cdot \\dot{V}\\mathrm{CO}_2 $$",
      ref    = "Achten J. & Jeukendrup A.E. (2003). Maximal fat oxidation during exercise in trained men. Int J Sports Med 24(8):603–608."
    ),
    cho = list(
      title  = "CHO – Kohlenhydratoxidation [g·min⁻¹]",
      desc   = "Kohlenhydratoxidationsrate pro Minute aus Atemgasen.",
      formula = "$$ \\mathrm{CHO}\\;[\\mathrm{g \\cdot min^{-1}}] = 4{,}585 \\cdot \\dot{V}\\mathrm{CO}_2 - 3{,}226 \\cdot \\dot{V}\\mathrm{O}_2 $$",
      ref    = "Achten J. & Jeukendrup A.E. (2003). Int J Sports Med 24(8):603–608."
    ),
    fo_pct = list(
      title  = "%FO – Anteil der Fettoxidation am Energieumsatz",
      desc   = "Prozentualer Energieanteil aus Fettoxidation, RER-basiert linear interpoliert (RER 0{,}707 = 100 % Fett, RER 1{,}000 = 0 % Fett).",
      formula = "$$ \\%\\mathrm{FO} = 1 - \\frac{\\mathrm{RER} - 0{,}707}{1{,}000 - 0{,}707} \\quad \\text{für } 0{,}707 \\leq \\mathrm{RER} \\leq 1 $$",
      ref    = "Péronnet F. & Massicotte D. (1991). Table of nonprotein respiratory quotient: an update. Can J Sport Sci 16(1):23–29."
    ),
    cho_pct = list(
      title  = "%CHO – Anteil der Kohlenhydratoxidation am Energieumsatz",
      desc   = "Prozentualer Energieanteil aus Kohlenhydraten, RER-basiert linear interpoliert.",
      formula = "$$ \\%\\mathrm{CHO} = \\frac{\\mathrm{RER} - 0{,}707}{1{,}000 - 0{,}707} \\quad \\text{für } 0{,}707 \\leq \\mathrm{RER} \\leq 1 $$",
      ref    = "Péronnet F. & Massicotte D. (1991). Can J Sport Sci 16(1):23–29."
    ),
    et = list(
      title  = "ET – Energy Turnover",
      desc   = "Energieumsatz pro Minute aus V'O₂ und Substrat-Anteilen.",
      formula = "$$ \\mathrm{ET}\\;[\\mathrm{kcal \\cdot min^{-1}}] = \\frac{\\dot{V}\\mathrm{O}_2 \\cdot \\left( \\%\\mathrm{FO} \\cdot 19{,}6 + \\%\\mathrm{CHO} \\cdot 21{,}1 \\right)}{4{,}18} $$",
      ref    = paste0(
        "Kroidl R., Schwarz S., Lehnigk B., Fritsch J. (2015). ",
        "Kursbuch Spiroergometrie, 3. Aufl., Thieme. ",
        "Kalorische Äquivalente: 19{,}6 kJ/L (Fett) bzw. 21{,}1 kJ/L (CHO).")
    )
  )
}
