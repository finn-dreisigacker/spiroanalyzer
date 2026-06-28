# ============================================================
#  mod_vt_analysis.R -- VT1/VT2 Analyse (v5)
#  Sidebar + VT-Seiten + Zwei-Segment-Regression + Hilfe
# ============================================================

mod_vt_ui <- function(id) {
  ns <- shiny::NS(id)

  # Helper: ? button neben jedem Plot
  help_btn <- function(help_id) {
    shiny::tags$button(
      class = "vt-help-btn",
      onclick = paste0(
        "Shiny.setInputValue('", ns(paste0("help_", help_id)),
        "', Math.random(), {priority:'event'})"),
      shiny::icon("circle-question"))
  }

  # Helper: Plot-Header (Tag + ?)
  plot_hdr <- function(label, tag_class, help_id) {
    shiny::div(
      style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
      shiny::span(class = paste("vt-plot-tag", tag_class), label),
      help_btn(help_id))
  }

  shiny::tagList(
    # ── CSS ────────────────────────────────────────────────────
    shiny::tags$style(shiny::HTML("
      .vt-sidebar {
        background: #f8fafc; border: 1px solid #e2e8f0;
        border-radius: 12px; padding: 16px; font-size: 0.84rem;
        max-height: calc(100vh - 160px); overflow-y: auto;
      }
      .vt-sidebar h6 {
        font-weight: 700; color: #1f3d6b; margin: 0 0 8px;
        font-size: 0.88rem;
      }
      .vt-sidebar hr { margin: 8px 0; border-color: #e2e8f0; }
      .vt-sidebar .form-group { margin-bottom: 6px; }
      .vt-sidebar .checkbox { margin-top: 2px; margin-bottom: 2px; }
      .vt-auto   { color: #2563eb; font-weight: 600; font-size: 0.82rem; }
      .vt-manual { color: #D97706; font-weight: 600; font-size: 0.82rem; }
      .vt-final  { color: #059669; font-weight: 600; font-size: 0.82rem; }
      .vt-plot-tag {
        font-size: 0.70rem; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.04em; padding: 2px 8px; border-radius: 4px;
        display: inline-block;
      }
      .vt-tag-vt1  { background: #dcfce7; color: #166534; }
      .vt-tag-vt2  { background: #fef9c3; color: #854d0e; }
      .vt-tag-both { background: #dbeafe; color: #1e40af; }
      .vt-tag-info { background: #f1f5f9; color: #475569; }
      .vt-plot-wrap { margin-bottom: 10px; }
      .vt-section-divider {
        font-size: 0.72rem; font-weight: 700; color: #94a3b8;
        text-transform: uppercase; letter-spacing: 0.06em;
        margin: 4px 0 6px; padding-bottom: 4px;
        border-bottom: 1px solid #e2e8f0;
      }
      .vt-help-btn {
        background: none; border: 1px solid #d0dbe8; border-radius: 50%;
        width: 22px; height: 22px; padding: 0; cursor: pointer;
        color: #64748b; font-size: 0.72rem; display: inline-flex;
        align-items: center; justify-content: center;
        transition: all 0.15s;
      }
      .vt-help-btn:hover {
        background: #f0f6ff; color: #2563eb; border-color: #2563eb;
      }
      .vt-confirm-bar {
        text-align: center; padding: 18px 0 6px;
        border-top: 1px solid #e2e8f0; margin-top: 10px;
      }
      /* VT parameter table */
      .vt-param-tbl {
        width: 100%; border-collapse: collapse; font-size: 0.80rem;
        margin-top: 6px;
      }
      .vt-param-tbl th {
        padding: 4px 6px; font-weight: 700; text-align: center;
        border-bottom: 2px solid #d6e4fa; background: #f0f6ff;
      }
      .vt-param-tbl th.vt1-h { color: #006400; }
      .vt-param-tbl th.vt2-h { color: #6B8E23; }
      .vt-param-tbl td {
        padding: 3px 6px; border-bottom: 1px solid #f0f4fb;
        text-align: center;
      }
      .vt-param-tbl td:first-child {
        text-align: left; font-weight: 600; color: #5b7fa6;
      }
      .vt-param-tbl tr:last-child td { border-bottom: none; }
      /* Hilfe-Tab */
      .help-section {
        background: #fff; border: 1px solid #e2e8f0; border-radius: 12px;
        padding: 20px; margin-bottom: 16px;
      }
      .help-section h5 {
        font-weight: 800; color: #1f3d6b; margin: 0 0 12px;
        padding-bottom: 8px; border-bottom: 2px solid #e5eefa;
        font-size: 0.95rem;
      }
      .help-section p { font-size: 0.86rem; color: #334155; line-height: 1.55; margin-bottom: 8px; }
      .help-formula {
        background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;
        padding: 8px 14px; font-family: monospace; font-size: 0.84rem;
        color: #334155; margin: 6px 0;
      }
      .help-key { font-weight: 700; color: #1f3d6b; }
    ")),


    # ── Layout ─────────────────────────────────────────────────
    shiny::fluidRow(

      # ========================================================
      #  LINKE SIDEBAR
      # ========================================================
      shiny::column(3,
        shiny::div(class = "vt-sidebar",
          shiny::tags$h6(shiny::icon("sliders"), " Einstellungen"),
          shiny::numericInput(ns("smooth"), "Glättung (Punkte)",
            value = 15, min = 3, max = 60, step = 1, width = "100%"),
          shiny::div(class = "vt-section-divider", "Angezeigte Phasen"),
          shiny::checkboxInput(ns("ph_warmup"), "Erwärmung",
            value = TRUE),
          shiny::checkboxInput(ns("ph_recovery"), "Erholung",
            value = FALSE),
          shiny::hr(),

          # VT1
          shiny::tags$h6(style = "color:#006400;",
            shiny::HTML("&#9679;"),
            " VT1 (1. ventilatorische Schwelle)"),
          shiny::uiOutput(ns("vt1_status")),
          shiny::numericInput(ns("vt1_time"), "VT1 Zeit (min)",
            value = NA, min = 0, max = 60, step = 0.1, width = "100%"),
          shiny::actionButton(ns("vt1_reset"), "Reset",
            class = "btn-sm btn-outline-secondary w-100",
            style = "margin-bottom:8px;"),
          shiny::hr(),

          # VT2
          shiny::tags$h6(style = "color:#6B8E23;",
            shiny::HTML("&#9679;"),
            " VT2"),
          shiny::uiOutput(ns("vt2_status")),
          shiny::numericInput(ns("vt2_time"), "VT2 Zeit (min)",
            value = NA, min = 0, max = 60, step = 0.1, width = "100%"),
          shiny::actionButton(ns("vt2_reset"), "Reset",
            class = "btn-sm btn-outline-secondary w-100",
            style = "margin-bottom:8px;"),
          shiny::hr(),

          shiny::actionButton(ns("apply_9p"),
            "In 9-Felder übernehmen",
            icon = shiny::icon("share"),
            class = "btn-outline-primary w-100"),
          shiny::hr(),

          shiny::tags$h6(shiny::icon("table"), " VT-Parameter"),
          shiny::uiOutput(ns("vt_params")),

          shiny::div(style = "margin-top:10px; font-size:0.75rem; color:#94a3b8;",
            shiny::icon("info-circle"),
            " Verschiebe die grünen/gelben Linien, ",
            "um VT1/VT2 manuell anzupassen.")
        )
      ),

      # ========================================================
      #  RECHTS: TABS
      # ========================================================
      shiny::column(9,
        bslib::navset_card_tab(

          # ── SEITE VT1 ──────────────────────────────────────
          bslib::nav_panel(
            shiny::tagList(shiny::span(
              style = "color:#006400; font-weight:700;",
              shiny::HTML("&#9679;"), " VT1")),

            shiny::div(class = "vt-plot-wrap",
              plot_hdr("V-Slope (VO\u2082 vs VCO\u2082) \u2013 S=1-Linie verschiebbar",
                "vt-tag-vt1", "vslope"),
              plotly::plotlyOutput(ns("p_vslope"), height = "480px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Excess CO\u2082",
                "vt-tag-vt1", "exco2"),
              plotly::plotlyOutput(ns("p_exco2"), height = "320px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Ventilatorische Äquivalente (VE/VO\u2082, VE/VCO\u2082)",
                "vt-tag-vt1", "eq_vt1"),
              plotly::plotlyOutput(ns("p_eq_vt1"), height = "320px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Endtidale Drücke (PetO\u2082, PetCO\u2082)",
                "vt-tag-vt1", "pet_vt1"),
              plotly::plotlyOutput(ns("p_pet_vt1"), height = "320px")),
            # Bestätigen-Button
            shiny::div(class = "vt-confirm-bar",
              shiny::actionButton(ns("vt1_confirm"),
                shiny::tagList(shiny::icon("check-circle"),
                  " VT1 bestätigen"),
                class = "btn-success btn-lg")),
            # Kommentar zu VT1
            shiny::div(class = "sa-card", style = "margin-top:14px;",
              shiny::tags$h6(
                shiny::icon("comment"), " Kommentar zu VT1",
                style = "font-weight:700; color:#006400; margin-bottom:6px;"),
              shiny::textAreaInput(ns("vt1_comment"),
                label = NULL, value = "", width = "100%", rows = 4,
                placeholder = "Methodische Einordnung, Auffälligkeiten ..."))
          ),

          # ── SEITE VT2 ──────────────────────────────────────
          bslib::nav_panel(
            shiny::tagList(shiny::span(
              style = "color:#6B8E23; font-weight:700;",
              shiny::HTML("&#9679;"), " VT2")),

            shiny::div(class = "vt-plot-wrap",
              plot_hdr("VE vs VCO\u2082",
                "vt-tag-vt2", "ve_vco2"),
              plotly::plotlyOutput(ns("p_ve_vco2"), height = "480px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Excess VE",
                "vt-tag-vt2", "exve"),
              plotly::plotlyOutput(ns("p_exve"), height = "320px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Ventilatorische Äquivalente (VE/VO\u2082, VE/VCO\u2082)",
                "vt-tag-vt2", "eq_vt2"),
              plotly::plotlyOutput(ns("p_eq_vt2"), height = "320px")),
            shiny::div(class = "vt-plot-wrap",
              plot_hdr("Endtidale Drücke (PetO\u2082, PetCO\u2082)",
                "vt-tag-vt2", "pet_vt2"),
              plotly::plotlyOutput(ns("p_pet_vt2"), height = "320px")),
            shiny::div(class = "vt-confirm-bar",
              shiny::actionButton(ns("vt2_confirm"),
                shiny::tagList(shiny::icon("check-circle"),
                  " VT2 bestätigen"),
                class = "btn-success btn-lg")),
            # Kommentar zu VT2
            shiny::div(class = "sa-card", style = "margin-top:14px;",
              shiny::tags$h6(
                shiny::icon("comment"), " Kommentar zu VT2",
                style = "font-weight:700; color:#6B8E23; margin-bottom:6px;"),
              shiny::textAreaInput(ns("vt2_comment"),
                label = NULL, value = "", width = "100%", rows = 4,
                placeholder = "Methodische Einordnung, Auffälligkeiten ..."))
          ),

          # ── ÜBERSICHT ──────────────────────────────────────
          bslib::nav_panel(
            shiny::tagList(shiny::icon("chart-line"), " Übersicht"),
            shiny::fluidRow(
              shiny::column(6, shiny::div(class = "vt-plot-wrap",
                plot_hdr("VO\u2082 & VCO\u2082 Zeitverlauf",
                  "vt-tag-info", "overview"),
                plotly::plotlyOutput(ns("p_overview"), height = "400px"))),
              shiny::column(6, shiny::div(class = "vt-plot-wrap",
                plot_hdr("RER (unterstützend)",
                  "vt-tag-info", "rer"),
                plotly::plotlyOutput(ns("p_rer"), height = "400px")))
            ),
            # Allgemeiner Spiro-Kommentar
            shiny::div(class = "sa-card", style = "margin-top:14px;",
              shiny::tags$h6(
                shiny::icon("comment"), " Allgemeiner Spiro-Kommentar",
                style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
              shiny::textAreaInput(ns("general_comment"),
                label = NULL, value = "", width = "100%", rows = 5,
                placeholder = "Allgemeine Hinweise zur Spiroergometrie ..."))
          ),

          # ── HILFE (rechts) ─────────────────────────────────
          bslib::nav_spacer(),
          bslib::nav_panel(
            shiny::tagList(shiny::icon("graduation-cap"),
              " Hilfe"),
            shiny::uiOutput(ns("help_tab"))
          )
        )
      )
    )
  )
}


# ============================================================
#  SERVER
# ============================================================
mod_vt_server <- function(id, params_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── State ───────────────────────────────────────────────────
    vt <- shiny::reactiveValues(
      vt1_time = NA_real_, vt2_time = NA_real_,
      vt1_final = NA_real_, vt2_final = NA_real_,
      vt1_method = "auto", vt2_method = "auto",
      vt1_confirmed = FALSE, vt2_confirmed = FALSE,
      slope1_offset = 0,
      # Kommentare (kontextuell zu VT1/VT2 + allgemeiner Spiro-Kommentar)
      vt1_comment = "", vt2_comment = "", general_comment = ""
    )

    # ── Gefilterte Zeitreihe ────────────────────────────────────
    ts_r <- shiny::reactive({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      ts <- p$ts
      keep <- c("Belastung", "Exercise")
      if (isTRUE(input$ph_warmup))
        keep <- c(keep, "Warmup", "Warm Up", "Warm-Up",
                  "Erwärmung", "Erwaermung")
      if (isTRUE(input$ph_recovery))
        keep <- c(keep, "Cooldown", "Cool Down", "Erholung", "Recovery")
      ts |>
        dplyr::filter(Phase %in% keep | is.na(Phase)) |>
        dplyr::arrange(time_min)
    })

    sn <- shiny::reactive(input$smooth %||% 15)

    # ── Init ────────────────────────────────────────────────────
    shiny::observeEvent(params_reactive(), {
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      t1 <- if (is.finite(p$vt1_time %||% NA)) p$vt1_time else {
        idx <- tryCatch(auto_vt1(p$ts), error = function(e) NA_integer_)
        vt_idx_to_time(p$ts, idx)
      }
      t2 <- if (is.finite(p$vt2_time %||% NA)) p$vt2_time else {
        idx <- tryCatch(auto_vt2(p$ts), error = function(e) NA_integer_)
        vt_idx_to_time(p$ts, idx)
      }
      vt$vt1_time <- t1; vt$vt2_time <- t2
      vt$vt1_final <- NA_real_; vt$vt2_final <- NA_real_
      vt$vt1_method <- if (is.finite(p$vt1_time %||% NA)) "Excel" else "auto"
      vt$vt2_method <- if (is.finite(p$vt2_time %||% NA)) "Excel" else "auto"
      vt$vt1_confirmed <- FALSE; vt$vt2_confirmed <- FALSE
      # Kommentare bei Datei-Wechsel zuruecksetzen (messungsspezifisch)
      vt$vt1_comment <- ""; vt$vt2_comment <- ""; vt$general_comment <- ""
      shiny::updateTextAreaInput(session, "vt1_comment", value = "")
      shiny::updateTextAreaInput(session, "vt2_comment", value = "")
      shiny::updateTextAreaInput(session, "general_comment", value = "")
      shiny::updateNumericInput(session, "vt1_time",
        value = if (is.finite(t1)) round(t1, 2) else NA)
      shiny::updateNumericInput(session, "vt2_time",
        value = if (is.finite(t2)) round(t2, 2) else NA)
    })

    # Kommentar-Felder mit reactiveValues synchronisieren
    shiny::observeEvent(input$vt1_comment, {
      vt$vt1_comment <- input$vt1_comment %||% ""
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$vt2_comment, {
      vt$vt2_comment <- input$vt2_comment %||% ""
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$general_comment, {
      vt$general_comment <- input$general_comment %||% ""
    }, ignoreNULL = FALSE)

    # ── JS Drag → VT Update ────────────────────────────────────
    shiny::observeEvent(input$drag_vt1, {
      t <- input$drag_vt1$time
      if (is.finite(t)) {
        vt$vt1_time <- t
        vt$vt1_method <- "manuell"; vt$vt1_confirmed <- FALSE
        shiny::updateNumericInput(session, "vt1_time", value = round(t, 2))
      }
    })
    shiny::observeEvent(input$drag_vt2, {
      t <- input$drag_vt2$time
      if (is.finite(t)) {
        vt$vt2_time <- t
        vt$vt2_method <- "manuell"; vt$vt2_confirmed <- FALSE
        shiny::updateNumericInput(session, "vt2_time", value = round(t, 2))
      }
    })
    # V'E~V'CO2-Plot: x-Achse ist V'CO2, nicht Zeit. Gezogene VT2-Linie
    # liefert einen V'CO2-Wert → auf nächstgelegene Belastungs-Zeit mappen.
    shiny::observeEvent(input$drag_vt2_vco2, {
      v <- input$drag_vt2_vco2$vco2
      if (is.null(v) || !is.finite(v)) return()
      d <- tryCatch(ts_r(), error = function(e) NULL)
      if (is.null(d) || nrow(d) < 5) return()
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      bel <- d[d$Phase %in% c("Belastung", "Exercise") &
                 is.finite(d$VCO2_s), , drop = FALSE]
      if (nrow(bel) == 0) bel <- d[is.finite(d$VCO2_s), , drop = FALSE]
      if (nrow(bel) == 0) return()
      t <- bel$time_min[which.min(abs(bel$VCO2_s - v))]
      if (is.finite(t)) {
        vt$vt2_time <- t
        vt$vt2_method <- "manuell"; vt$vt2_confirmed <- FALSE
        shiny::updateNumericInput(session, "vt2_time", value = round(t, 2))
      }
    })

    # ── NumericInput → VT ───────────────────────────────────────
    shiny::observeEvent(input$vt1_time, {
      if (!is.finite(input$vt1_time)) return()
      vt$vt1_time <- input$vt1_time
      vt$vt1_method <- "manuell"; vt$vt1_confirmed <- FALSE
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$vt2_time, {
      if (!is.finite(input$vt2_time)) return()
      vt$vt2_time <- input$vt2_time
      vt$vt2_method <- "manuell"; vt$vt2_confirmed <- FALSE
    }, ignoreInit = TRUE)

    # ── Reset ───────────────────────────────────────────────────
    shiny::observeEvent(input$vt1_reset, {
      p <- tryCatch(params_reactive(), error = function(e) NULL)
      if (is.null(p)) return()
      t1 <- if (is.finite(p$vt1_time %||% NA)) p$vt1_time
            else vt_idx_to_time(p$ts,
                   tryCatch(auto_vt1(p$ts), error = function(e) NA_integer_))
      vt$vt1_time <- t1; vt$vt1_method <- "auto"; vt$vt1_confirmed <- FALSE
      shiny::updateNumericInput(session, "vt1_time",
        value = if (is.finite(t1)) round(t1, 2) else NA)
    })
    shiny::observeEvent(input$vt2_reset, {
      p <- tryCatch(params_reactive(), error = function(e) NULL)
      if (is.null(p)) return()
      t2 <- if (is.finite(p$vt2_time %||% NA)) p$vt2_time
            else vt_idx_to_time(p$ts,
                   tryCatch(auto_vt2(p$ts), error = function(e) NA_integer_))
      vt$vt2_time <- t2; vt$vt2_method <- "auto"; vt$vt2_confirmed <- FALSE
      shiny::updateNumericInput(session, "vt2_time",
        value = if (is.finite(t2)) round(t2, 2) else NA)
    })

    # ── S=1-Linie verschoben (Slope bleibt 1, nur Offset ändert sich) ──
    shiny::observeEvent(input$drag_slope1, {
      off <- input$drag_slope1$offset
      if (is.finite(off)) vt$slope1_offset <- off
    })

    # ── Bestätigen (unter den Plots) ────────────────────────────
    shiny::observeEvent(input$vt1_confirm, {
      vt$vt1_final <- vt$vt1_time; vt$vt1_confirmed <- TRUE
      shiny::showModal(shiny::modalDialog(
        shiny::div(style = "text-align:center; padding:20px;",
          shiny::tags$h4(style = "color:#059669;",
            shiny::icon("check-circle"), " VT1 bestätigt!"),
          shiny::tags$p(style = "font-size:1.1rem; margin-top:10px;",
            if (is.finite(vt$vt1_time))
              paste0("VT1 bei ", round(vt$vt1_time, 2), " min")
            else "VT1 gesetzt")),
        title = NULL,
        footer = shiny::modalButton("OK"),
        easyClose = TRUE, size = "s"))
    })
    shiny::observeEvent(input$vt2_confirm, {
      vt$vt2_final <- vt$vt2_time; vt$vt2_confirmed <- TRUE
      shiny::showModal(shiny::modalDialog(
        shiny::div(style = "text-align:center; padding:20px;",
          shiny::tags$h4(style = "color:#059669;",
            shiny::icon("check-circle"), " VT2 bestätigt!"),
          shiny::tags$p(style = "font-size:1.1rem; margin-top:10px;",
            if (is.finite(vt$vt2_time))
              paste0("VT2 bei ", round(vt$vt2_time, 2), " min")
            else "VT2 gesetzt")),
        title = NULL,
        footer = shiny::modalButton("OK"),
        easyClose = TRUE, size = "s"))
    })

    # ── Status-Anzeige ──────────────────────────────────────────
    output$vt1_status <- shiny::renderUI({
      cls <- if (vt$vt1_confirmed) "vt-final"
             else if (vt$vt1_method == "manuell") "vt-manual" else "vt-auto"
      lbl <- if (vt$vt1_confirmed) "Final"
             else if (vt$vt1_method == "manuell") "Manuell" else vt$vt1_method
      shiny::div(class = cls, paste0("Status: ", lbl))
    })
    output$vt2_status <- shiny::renderUI({
      cls <- if (vt$vt2_confirmed) "vt-final"
             else if (vt$vt2_method == "manuell") "vt-manual" else "vt-auto"
      lbl <- if (vt$vt2_confirmed) "Final"
             else if (vt$vt2_method == "manuell") "Manuell" else vt$vt2_method
      shiny::div(class = cls, paste0("Status: ", lbl))
    })

    # ══════════════════════════════════════════════════════════════
    #  VT-PARAMETER (Wide HTML-Tabelle)
    # ══════════════════════════════════════════════════════════════
    output$vt_params <- shiny::renderUI({
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      if (is.null(ts) || nrow(ts) < 5)
        return(shiny::div(style = "color:#94a3b8;", "Keine Daten"))
      t1 <- vt$vt1_time; t2 <- vt$vt2_time
      val_at <- function(col, tv) {
        if (!is.finite(tv) || !col %in% names(ts)) return("-")
        idx <- which.min(abs(ts$time_min - tv))
        if (length(idx) != 1) return("-")
        v <- ts[[col]][idx]
        if (!is.finite(v)) return("-")
        if (col %in% c("VO2abs","VCO2")) format(round(v,3), nsmall=3)
        else if (col %in% c("VE","HR","P")) as.character(round(v,0))
        else if (col == "RER") format(round(v,2), nsmall=2)
        else format(round(v,1), nsmall=1)
      }
      fmt_t <- function(tv) if (is.finite(tv)) format(round(tv,2), nsmall=2) else "-"
      rows <- list(
        c("Zeit (min)",        fmt_t(t1),             fmt_t(t2)),
        c("Leistung (W)",      val_at("P",t1),        val_at("P",t2)),
        c("VO\u2082 (L/min)",  val_at("VO2abs",t1),   val_at("VO2abs",t2)),
        c("VCO\u2082 (L/min)", val_at("VCO2",t1),     val_at("VCO2",t2)),
        c("VE (L/min)",        val_at("VE",t1),       val_at("VE",t2)),
        c("HR (bpm)",          val_at("HR",t1),       val_at("HR",t2)),
        c("RER",               val_at("RER",t1),      val_at("RER",t2)),
        c("VE/VO\u2082",       val_at("VE_VO2",t1),   val_at("VE_VO2",t2)),
        c("VE/VCO\u2082",      val_at("VE_VCO2",t1),  val_at("VE_VCO2",t2)),
        c("Methode",           vt$vt1_method,          vt$vt2_method),
        c("Status",
          if (vt$vt1_confirmed) "Final" else "\u2013",
          if (vt$vt2_confirmed) "Final" else "\u2013"))
      html <- paste0(
        '<table class="vt-param-tbl"><thead><tr>',
        '<th></th><th class="vt1-h">VT1</th><th class="vt2-h">VT2</th>',
        '</tr></thead><tbody>')
      for (r in rows)
        html <- paste0(html,'<tr><td>',r[1],'</td><td>',r[2],'</td><td>',r[3],'</td></tr>')
      shiny::HTML(paste0(html, '</tbody></table>'))
    })

    # ══════════════════════════════════════════════════════════════
    #  PLOT HELPERS
    # ══════════════════════════════════════════════════════════════

    # ── Phasen-Hintergründe (NICHT verschiebbar, gelb/blau/lila) ──
    phase_shapes <- function(d) {
      shapes <- list()
      if (is.null(d) || nrow(d) == 0 || !"Phase" %in% names(d)) return(shapes)
      cols <- list(
        "Erwärmung" = "rgba(252,211,77,0.22)",   # warmes Gelb
        "Erwaermung"= "rgba(252,211,77,0.22)",
        "Belastung" = "rgba(147,197,253,0.22)",  # Sky-Blue
        "Exercise"  = "rgba(147,197,253,0.22)",
        "Erholung"  = "rgba(216,180,254,0.28)",  # Lavendel
        "Recovery"  = "rgba(216,180,254,0.28)"
      )
      ph <- as.character(d$Phase); ph[is.na(ph)] <- ""
      r  <- rle(ph)
      ends   <- cumsum(r$lengths)
      starts <- c(1, head(ends, -1) + 1)
      for (i in seq_along(r$values)) {
        p <- r$values[i]
        if (p %in% names(cols)) {
          x0 <- d$time_min[starts[i]]
          x1 <- d$time_min[ends[i]]
          if (is.finite(x0) && is.finite(x1) && x1 > x0) {
            shapes[[length(shapes)+1]] <- list(
              type="rect", x0=x0, x1=x1, y0=0, y1=1, yref="paper",
              fillcolor=cols[[p]], line=list(width=0),
              layer="below", editable=FALSE)
          }
        }
      }
      shapes
    }

    # ── VT-Marker-Linie (verschiebbar) ──
    vt_marker_shape <- function(t, color) {
      if (!is.finite(t)) return(list())
      list(list(
        type="line", x0=t, x1=t, y0=0, y1=1, yref="paper",
        line=list(color=color, width=2.5),
        layer="above", editable=TRUE))
    }

    # ── Schöne MM:SS-Zeitachse ──
    pretty_time_axis <- function(d) {
      tmax <- if (is.null(d) || nrow(d) == 0) 20 else max(d$time_min, na.rm = TRUE)
      if (!is.finite(tmax) || tmax <= 0) tmax <- 20
      step <- if (tmax <= 8) 1 else if (tmax <= 20) 2 else if (tmax <= 40) 4 else 5
      vals <- seq(0, ceiling(tmax/step) * step, by = step)
      txt  <- sprintf("%02d:%02d",
        as.integer(vals),
        as.integer(round((vals - as.integer(vals)) * 60)))
      list(
        title = list(text = "Zeit", font = list(size = 11)),
        tickmode = "array", tickvals = vals, ticktext = txt,
        showgrid = TRUE, gridcolor = "#e2e8f0", griddash = "dash",
        zeroline = FALSE, ticks = "outside", tickfont = list(size = 10))
    }

    # ── Pro-Tab Shape-Listen ──
    # WICHTIG: VT-Linie zuerst (Index 0), Phasen danach – damit der
    # Drag-Handler immer shape[0] = VT-Linie sieht.
    vt1_shapes <- shiny::reactive({
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      c(vt_marker_shape(vt$vt1_time, "#006400"),
        phase_shapes(ts))
    })
    vt2_shapes <- shiny::reactive({
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      c(vt_marker_shape(vt$vt2_time, "#6B8E23"),
        phase_shapes(ts))
    })
    # Übersicht: VT1 = shape[0], VT2 = shape[1], Phasen danach
    vt_shapes <- shiny::reactive({
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      c(vt_marker_shape(vt$vt1_time, "#006400"),
        vt_marker_shape(vt$vt2_time, "#6B8E23"),
        phase_shapes(ts))
    })

    # JS für VT1-Tabs: nur shape[0] → drag_vt1
    drag_js_vt1 <- sprintf("
      function(el, x) {
        el.on('plotly_relayout', function(ed) {
          if (!ed) return;
          var v = ed['shapes[0].x0'];
          if (v !== undefined && isFinite(v))
            Shiny.setInputValue('%s', {time:v, nonce:Math.random()},
              {priority:'event'});
        });
      }
    ", ns("drag_vt1"))

    # JS für VT2-Tabs: nur shape[0] → drag_vt2
    drag_js_vt2 <- sprintf("
      function(el, x) {
        el.on('plotly_relayout', function(ed) {
          if (!ed) return;
          var v = ed['shapes[0].x0'];
          if (v !== undefined && isFinite(v))
            Shiny.setInputValue('%s', {time:v, nonce:Math.random()},
              {priority:'event'});
        });
      }
    ", ns("drag_vt2"))

    # JS für Übersicht: shape[0] → vt1, shape[1] → vt2
    drag_js_both <- sprintf("
      function(el, x) {
        el.on('plotly_relayout', function(ed) {
          if (!ed) return;
          var keys = Object.keys(ed);
          for (var i = 0; i < keys.length; i++) {
            if (keys[i].indexOf('shapes[0].x0') >= 0 && isFinite(ed[keys[i]]))
              Shiny.setInputValue('%s', {time:ed[keys[i]], nonce:Math.random()},
                {priority:'event'});
            if (keys[i].indexOf('shapes[1].x0') >= 0 && isFinite(ed[keys[i]]))
              Shiny.setInputValue('%s', {time:ed[keys[i]], nonce:Math.random()},
                {priority:'event'});
          }
        });
      }
    ", ns("drag_vt1"), ns("drag_vt2"))

    # JS für V'E~V'CO2: shape[0] (VT2-Linie) → V'CO2-Wert an drag_vt2_vco2
    drag_js_vt2_vco2 <- sprintf("
      function(el, x) {
        el.on('plotly_relayout', function(ed) {
          if (!ed) return;
          var v = ed['shapes[0].x0'];
          if (v !== undefined && isFinite(v))
            Shiny.setInputValue('%s', {vco2:v, nonce:Math.random()},
              {priority:'event'});
        });
      }
    ", ns("drag_vt2_vco2"))

    # Edit-Optionen: NUR Shape-Positionen ändern, keine Titel/Achsen/Legende
    drag_edits <- list(
      shapePosition = TRUE,
      titleText = FALSE,
      axisTitleText = FALSE,
      legendPosition = FALSE,
      annotationPosition = FALSE,
      annotationText = FALSE,
      annotationTail = FALSE)

    add_drag_vt1 <- function(p) {
      p |>
        plotly::config(editable = TRUE, edits = drag_edits,
          displaylogo = FALSE) |>
        htmlwidgets::onRender(drag_js_vt1)
    }
    add_drag_vt2 <- function(p) {
      p |>
        plotly::config(editable = TRUE, edits = drag_edits,
          displaylogo = FALSE) |>
        htmlwidgets::onRender(drag_js_vt2)
    }
    add_drag_vt2_vco2 <- function(p) {
      p |>
        plotly::config(editable = TRUE, edits = drag_edits,
          displaylogo = FALSE) |>
        htmlwidgets::onRender(drag_js_vt2_vco2)
    }
    add_drag <- function(p) {
      p |>
        plotly::config(editable = TRUE, edits = drag_edits,
          displaylogo = FALSE) |>
        htmlwidgets::onRender(drag_js_both)
    }

    m <- list(t=30, b=40, l=55, r=20)
    m_dual <- list(t=30, b=40, l=55, r=55)

    # Helper: 2-Segment-Regression als Shapes (grau, dünn, gestrichelt,
    # layer="below" → hinter den Punkten, ganzer Chart)
    two_seg_shapes <- function(x_vec, y_vec, x_min, x_max, min_seg = 10) {
      out <- list(shapes = list(), s1 = NA_real_, s2 = NA_real_)
      ok <- is.finite(x_vec) & is.finite(y_vec)
      if (sum(ok) < 2 * min_seg) return(out)
      bp <- tryCatch(
        find_breakpoint(x_vec, y_vec, min_seg = min_seg),
        error = function(e) NA_integer_)
      if (is.na(bp)) return(out)
      idx_ok <- which(ok); bp_pos <- match(bp, idx_ok)
      if (is.na(bp_pos) || bp_pos < min_seg) return(out)
      n <- length(idx_ok)
      s1_idx <- idx_ok[1:bp_pos]
      s2_idx <- idx_ok[(bp_pos+1):n]
      fit1 <- tryCatch(lm(y_vec[s1_idx] ~ x_vec[s1_idx]),
        error = function(e) NULL)
      fit2 <- tryCatch(lm(y_vec[s2_idx] ~ x_vec[s2_idx]),
        error = function(e) NULL)
      if (is.null(fit1) || is.null(fit2)) return(out)
      a1 <- as.numeric(coef(fit1)[1]); b1 <- as.numeric(coef(fit1)[2])
      a2 <- as.numeric(coef(fit2)[1]); b2 <- as.numeric(coef(fit2)[2])
      grey <- "#9ca3af"
      out$shapes <- list(
        list(type = "line",
          x0 = x_min, y0 = a1 + b1 * x_min,
          x1 = x_max, y1 = a1 + b1 * x_max,
          line = list(color = grey, width = 1.2, dash = "dash"),
          layer = "below", editable = FALSE),
        list(type = "line",
          x0 = x_min, y0 = a2 + b2 * x_min,
          x1 = x_max, y1 = a2 + b2 * x_max,
          line = list(color = grey, width = 1.2, dash = "dot"),
          layer = "below", editable = FALSE))
      out$s1 <- b1; out$s2 <- b2
      out
    }

    # ══════════════════════════════════════════════════════════════
    #  VT1-PLOTS
    # ══════════════════════════════════════════════════════════════

    # ── V-Slope mit verschiebbarer S=1-Linie + 2-Segment-Regression ────
    output$p_vslope <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$VO2_s  <- safe_roll(d$VO2abs, sn())
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      ax <- max(c(d$VO2_s, d$VCO2_s), na.rm = TRUE) * 1.1
      if (!is.finite(ax)) ax <- 4

      off <- vt$slope1_offset %||% 0
      if (!is.finite(off)) off <- 0

      # SHAPE 0: verschiebbare S=1-Linie (Steigung bleibt = 1, nur Offset)
      shapes <- list(
        list(type = "line",
          x0 = 0,  y0 = off,
          x1 = ax, y1 = ax + off,
          line = list(color = "#374151", width = 1.6, dash = "dash"),
          editable = TRUE,
          layer = "above"))

      # 2-Segment-Regression: grau, dünn, gestrichelt, hinter den Punkten
      seg <- two_seg_shapes(d$VO2_s, d$VCO2_s, 0, ax,
        min_seg = max(10, sum(is.finite(d$VO2_s)) %/% 8))
      shapes <- c(shapes, seg$shapes)

      annotations <- list(
        list(x = ax * 0.92, y = ax * 0.92 + off,
          text = sprintf("S=1 (Offset %.2f)", off),
          showarrow = FALSE,
          font = list(size = 10, color = "#475569"),
          bgcolor = "rgba(255,255,255,0.7)"))
      if (is.finite(seg$s1) && is.finite(seg$s2)) {
        annotations[[length(annotations) + 1]] <- list(
          x = 0.99, y = 0.02, xref = "paper", yref = "paper",
          text = sprintf("S1 = %.2f   S2 = %.2f", seg$s1, seg$s2),
          showarrow = FALSE, xanchor = "right", yanchor = "bottom",
          font = list(size = 10, color = "#6b7280"),
          bgcolor = "rgba(255,255,255,0.6)")
      }

      # VT1-Markierung (nicht editable)
      if (is.finite(vt$vt1_time)) {
        idx <- which.min(abs(d$time_min - vt$vt1_time))
        if (length(idx) == 1 && is.finite(d$VO2_s[idx])) {
          vx <- d$VO2_s[idx]; vy <- d$VCO2_s[idx]
          shapes[[length(shapes) + 1]] <- list(type = "line",
            x0 = vx, x1 = vx, y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "#006400", width = 2, dash = "dash"),
            editable = FALSE)
          annotations[[length(annotations) + 1]] <- list(
            x = vx, y = vy, text = "VT1",
            showarrow = TRUE, arrowhead = 2, ax = 30, ay = -25,
            font = list(color = "#006400", size = 12))
        }
      }

      p <- plotly::plot_ly(d, x = ~VO2_s, y = ~VCO2_s, type = "scatter",
        mode = "markers", name = "Daten",
        marker = list(size = 3, color = "#DC2626", opacity = 0.45),
        hovertemplate = "VO\u2082: %{x:.3f}<br>VCO\u2082: %{y:.3f}<extra></extra>") |>
        plotly::layout(
          title = list(text = ""),
          xaxis = list(title = "V'O\u2082 [L/min]", range = c(0, ax),
            zeroline = FALSE, gridcolor = "#e2e8f0", griddash = "dash"),
          yaxis = list(title = "V'CO\u2082 [L/min]", range = c(0, ax),
            zeroline = FALSE, gridcolor = "#e2e8f0", griddash = "dash"),
          showlegend = FALSE,
          shapes = shapes, annotations = annotations, margin = m)

      # Drag-Handler: S=1-Linie kann verschoben werden, behält aber Steigung 1
      slope1_js <- sprintf("
        function(el, x) {
          var ax = %f;
          var inProgress = false;
          el.on('plotly_relayout', function(ed) {
            if (inProgress || !ed) return;
            var sx0 = ed['shapes[0].x0'];
            var sy0 = ed['shapes[0].y0'];
            var sx1 = ed['shapes[0].x1'];
            var sy1 = ed['shapes[0].y1'];
            if (sx0 === undefined && sy0 === undefined &&
                sx1 === undefined && sy1 === undefined) return;
            var off = null;
            if (sy0 !== undefined && sx0 !== undefined) {
              off = sy0 - sx0;
            } else if (sy1 !== undefined && sx1 !== undefined) {
              off = sy1 - sx1;
            } else if (sy0 !== undefined) {
              off = sy0;
            } else if (sy1 !== undefined) {
              off = sy1 - ax;
            } else if (sx0 !== undefined) {
              off = -sx0;
            } else if (sx1 !== undefined) {
              off = ax - sx1;
            }
            if (off === null || !isFinite(off)) return;
            inProgress = true;
            Plotly.relayout(el, {
              'shapes[0].x0': 0,
              'shapes[0].y0': off,
              'shapes[0].x1': ax,
              'shapes[0].y1': ax + off
            }).then(function(){ inProgress = false; },
                    function(){ inProgress = false; });
            Shiny.setInputValue('%s',
              {offset: off, nonce: Math.random()},
              {priority: 'event'});
          });
        }", ax, ns("drag_slope1"))

      p |>
        plotly::config(editable = TRUE,
          edits = list(shapePosition = TRUE,
            titleText = FALSE, axisTitleText = FALSE,
            legendPosition = FALSE, annotationPosition = FALSE),
          displaylogo = FALSE) |>
        htmlwidgets::onRender(slope1_js)
    })

    # ── Excess CO₂ ─────────────────────────────────────────────
    output$p_exco2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$ExCO2   <- calc_exco2(d$VO2abs, d$VCO2)
      d$ExCO2_s <- safe_roll(d$ExCO2, sn())
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_markers(y=~ExCO2, name="ExCO\u2082",
          marker=list(size=2, color="#DC2626", opacity=0.18)) |>
        plotly::add_lines(y=~ExCO2_s, name="Geglättet",
          line=list(color="#DC2626", width=2.5)) |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="Excess CO\u2082"),
          showlegend=FALSE, shapes=vt1_shapes(), margin=m) |>
        add_drag_vt1()
    })

    # ── Äquivalente (VT1) – duale y-Achse: VE/VO₂ links, VE/VCO₂ rechts ──
    output$p_eq_vt1 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$EQ_O2  <- safe_roll(d$VE_VO2, sn())
      d$EQ_CO2 <- safe_roll(d$VE_VCO2, sn())
      ann <- list()
      if (is.finite(vt$vt1_time))
        ann[[1]] <- list(x=vt$vt1_time, y=1.05, yref="paper",
          text="VE/VO\u2082 \u2191  VE/VCO\u2082 \u2192",
          showarrow=FALSE, font=list(size=10, color="#006400"),
          bgcolor="#dcfce7", borderpad=3)
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~EQ_O2, name="VE/VO\u2082",
          line=list(color="#DC2626", width=2)) |>
        plotly::add_lines(y=~EQ_CO2, name="VE/VCO\u2082",
          line=list(color="#2563EB", width=2), yaxis="y2") |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title=list(text="VE/VO\u2082",
            font=list(color="#DC2626")),
            tickfont=list(color="#DC2626"),
            side="left", zeroline=FALSE),
          yaxis2=list(title=list(text="VE/VCO\u2082",
            font=list(color="#2563EB")),
            tickfont=list(color="#2563EB"),
            overlaying="y", side="right", showgrid=FALSE,
            zeroline=FALSE),
          showlegend=TRUE,
          legend=list(orientation="h", x=0, y=1.15, font=list(size=10)),
          shapes=vt1_shapes(), annotations=ann, margin=m_dual) |>
        add_drag_vt1()
    })

    # ── Endtidale Drücke (VT1, duale y-Achse) ──────────────────
    output$p_pet_vt1 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$PO2  <- safe_roll(d$PetO2, sn())
      d$PCO2 <- safe_roll(d$PetCO2, sn())
      ann <- list()
      if (is.finite(vt$vt1_time))
        ann[[1]] <- list(x=vt$vt1_time, y=1.05, yref="paper",
          text="PetO\u2082 \u2191  PetCO\u2082 \u2192",
          showarrow=FALSE, font=list(size=10, color="#006400"),
          bgcolor="#dcfce7", borderpad=3)
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~PO2, name="PetO\u2082",
          line=list(color="#1D4ED8", width=2)) |>
        plotly::add_lines(y=~PCO2, name="PetCO\u2082",
          line=list(color="#9333EA", width=2), yaxis="y2") |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="PetO\u2082 [mmHg]", side="left"),
          yaxis2=list(title="PetCO\u2082 [mmHg]",
            overlaying="y", side="right", showgrid=FALSE),
          showlegend=TRUE,
          legend=list(orientation="h", x=0, y=1.15, font=list(size=10)),
          shapes=vt1_shapes(), annotations=ann, margin=m_dual) |>
        add_drag_vt1()
    })

    # ══════════════════════════════════════════════════════════════
    #  VT2-PLOTS
    # ══════════════════════════════════════════════════════════════

    # ── VE vs VCO₂ mit 2-Segment-Regression ────────────────────
    output$p_ve_vco2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      d$VE_s   <- safe_roll(d$VE, sn())

      x_max <- max(d$VCO2_s, na.rm = TRUE)
      x_min <- min(d$VCO2_s, na.rm = TRUE)
      if (!is.finite(x_max)) x_max <- 4
      if (!is.finite(x_min)) x_min <- 0

      shapes <- list(); annotations <- list()

      # VT2-Linie ZUERST (shapes[0]) und verschiebbar (editable=TRUE) –
      # so liest der Drag-Handler shapes[0].x0. x-Achse ist V'CO2.
      if (is.finite(vt$vt2_time)) {
        idx <- which.min(abs(d$time_min - vt$vt2_time))
        if (length(idx)==1 && is.finite(d$VCO2_s[idx])) {
          vx <- d$VCO2_s[idx]; vy <- d$VE_s[idx]
          shapes[[1]] <- list(type="line",
            x0=vx, x1=vx, y0=0, y1=1, yref="paper",
            line=list(color="#6B8E23", width=2.5, dash="dash"),
            layer="above", editable=TRUE)
          annotations[[length(annotations)+1]] <- list(
            x=vx, y=vy, text="VT2",
            showarrow=TRUE, arrowhead=2, ax=30, ay=-25,
            font=list(color="#6B8E23", size=12))
        }
      }

      # 2-Segment-Regression danach: grau, dünn, gestrichelt, NICHT editierbar
      seg <- two_seg_shapes(d$VCO2_s, d$VE_s, x_min, x_max,
        min_seg = max(10, sum(is.finite(d$VCO2_s)) %/% 8))
      shapes <- c(shapes, seg$shapes)
      if (is.finite(seg$s1) && is.finite(seg$s2)) {
        annotations[[length(annotations)+1]] <- list(
          x=0.99, y=0.02, xref="paper", yref="paper",
          text=sprintf("S1 = %.2f   S2 = %.2f", seg$s1, seg$s2),
          showarrow=FALSE, xanchor="right", yanchor="bottom",
          font=list(size=10, color="#6b7280"),
          bgcolor="rgba(255,255,255,0.6)")
      }

      plotly::plot_ly(d, x=~VCO2_s, y=~VE_s, type="scatter",
        mode="markers", name="Daten",
        marker=list(size=3, color="#2563EB", opacity=0.45),
        hovertemplate="VCO\u2082: %{x:.3f}<br>VE: %{y:.1f}<extra></extra>") |>
        plotly::layout(
          title = list(text = ""),
          xaxis=list(title="V'CO\u2082 [L/min]",
            zeroline=FALSE, gridcolor="#e2e8f0", griddash="dash"),
          yaxis=list(title="V'E [L/min]",
            zeroline=FALSE, gridcolor="#e2e8f0", griddash="dash"),
          showlegend=FALSE,
          shapes=shapes, annotations=annotations, margin=m) |>
        add_drag_vt2_vco2()
    })

    # ── Excess VE ──────────────────────────────────────────────
    output$p_exve <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$ExVE   <- calc_exve(d$VE, d$VCO2)
      d$ExVE_s <- safe_roll(d$ExVE, sn())
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_markers(y=~ExVE, name="ExVE",
          marker=list(size=2, color="#B45309", opacity=0.18)) |>
        plotly::add_lines(y=~ExVE_s, name="Geglättet",
          line=list(color="#D97706", width=2.5)) |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="Excess VE"),
          showlegend=FALSE, shapes=vt2_shapes(), margin=m) |>
        add_drag_vt2()
    })

    # ── Äquivalente (VT2) – duale y-Achse: VE/VO₂ links, VE/VCO₂ rechts ──
    output$p_eq_vt2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$EQ_O2  <- safe_roll(d$VE_VO2, sn())
      d$EQ_CO2 <- safe_roll(d$VE_VCO2, sn())
      ann <- list()
      if (is.finite(vt$vt2_time))
        ann[[1]] <- list(x=vt$vt2_time, y=1.05, yref="paper",
          text="VE/VCO\u2082 steigt jetzt auch \u2191",
          showarrow=FALSE, font=list(size=10, color="#6B8E23"),
          bgcolor="#fef9c3", borderpad=3)
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~EQ_O2, name="VE/VO\u2082",
          line=list(color="#DC2626", width=2)) |>
        plotly::add_lines(y=~EQ_CO2, name="VE/VCO\u2082",
          line=list(color="#2563EB", width=2), yaxis="y2") |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title=list(text="VE/VO\u2082",
            font=list(color="#DC2626")),
            tickfont=list(color="#DC2626"),
            side="left", zeroline=FALSE),
          yaxis2=list(title=list(text="VE/VCO\u2082",
            font=list(color="#2563EB")),
            tickfont=list(color="#2563EB"),
            overlaying="y", side="right", showgrid=FALSE,
            zeroline=FALSE),
          showlegend=TRUE,
          legend=list(orientation="h", x=0, y=1.15, font=list(size=10)),
          shapes=vt2_shapes(), annotations=ann, margin=m_dual) |>
        add_drag_vt2()
    })

    # ── Endtidale Drücke (VT2, duale y-Achse) ──────────────────
    output$p_pet_vt2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$PO2  <- safe_roll(d$PetO2, sn())
      d$PCO2 <- safe_roll(d$PetCO2, sn())
      ann <- list()
      if (is.finite(vt$vt2_time))
        ann[[1]] <- list(x=vt$vt2_time, y=1.05, yref="paper",
          text="PetCO\u2082 \u2193  PetO\u2082 \u2191",
          showarrow=FALSE, font=list(size=10, color="#6B8E23"),
          bgcolor="#fef9c3", borderpad=3)
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~PO2, name="PetO\u2082",
          line=list(color="#1D4ED8", width=2)) |>
        plotly::add_lines(y=~PCO2, name="PetCO\u2082",
          line=list(color="#9333EA", width=2), yaxis="y2") |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="PetO\u2082 [mmHg]", side="left"),
          yaxis2=list(title="PetCO\u2082 [mmHg]",
            overlaying="y", side="right", showgrid=FALSE),
          showlegend=TRUE,
          legend=list(orientation="h", x=0, y=1.15, font=list(size=10)),
          shapes=vt2_shapes(), annotations=ann, margin=m_dual) |>
        add_drag_vt2()
    })

    # ══════════════════════════════════════════════════════════════
    #  ÜBERSICHT-PLOTS
    # ══════════════════════════════════════════════════════════════

    output$p_overview <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$VO2_s  <- safe_roll(d$VO2abs, sn())
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~VO2_s, name="V'O\u2082",
          line=list(color="#DC2626", width=2)) |>
        plotly::add_lines(y=~VCO2_s, name="V'CO\u2082",
          line=list(color="#2563EB", width=2)) |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="L/min"), showlegend=TRUE,
          legend=list(orientation="h", x=0, y=1.12, font=list(size=10)),
          shapes=vt_shapes(), margin=m) |>
        add_drag()
    })

    output$p_rer <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 5)
      d$RER_s <- safe_roll(d$RER, sn())
      rs <- vt_shapes()
      rs[[length(rs)+1]] <- list(type="line", x0=0, x1=1, xref="paper",
        y0=1.0, y1=1.0, line=list(color="#94A3B8", dash="dot", width=1))
      rs[[length(rs)+1]] <- list(type="line", x0=0, x1=1, xref="paper",
        y0=1.1, y1=1.1, line=list(color="#DC2626", dash="dash", width=1))
      plotly::plot_ly(d, x=~time_min) |>
        plotly::add_lines(y=~RER_s, name="RER",
          line=list(color="#1E293B", width=2)) |>
        plotly::layout(title=list(text=""),
          xaxis=pretty_time_axis(d),
          yaxis=list(title="RER"), showlegend=FALSE, shapes=rs,
          annotations=list(
            list(x=1.01, y=1.0, xref="paper", text="1.0",
              showarrow=FALSE, font=list(size=9, color="#94A3B8"), xanchor="left"),
            list(x=1.01, y=1.1, xref="paper", text="1.1",
              showarrow=FALSE, font=list(size=9, color="#DC2626"), xanchor="left")),
          margin=m) |>
        add_drag()
    })

    # ══════════════════════════════════════════════════════════════
    #  HILFE-TAB (Physiologie)
    # ══════════════════════════════════════════════════════════════
    output$help_tab <- shiny::renderUI({
      shiny::tagList(
        # ── VT1 ────────────────────────────────────
        shiny::div(class = "help-section",
          shiny::tags$h5(shiny::HTML("&#9679;"),
            style = "color:#006400;",
            " VT1 \u2013 Erste ventilatorische Schwelle"),
          shiny::tags$p(
            "VT1 markiert den Übergang von rein aerober zu gemischt ",
            "aerob-anaerober Energiebereitstellung. Ab hier beginnt ",
            "die Laktatproduktion die Elimination zu übersteigen. ",
            "Die CO\u2082-Produktion steigt durch die Pufferung von ",
            "Laktat (HCO\u2083\u207b + H\u207a \u2192 CO\u2082 + H\u2082O) ",
            "überproportional zur O\u2082-Aufnahme."),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>V-Slope:</span> ",
            "x = VO\u2082, y = VCO\u2082. Beide steigen zunächst linear ",
            "(Steigung \u22481). Ab VT1 steigt VCO\u2082 stärker \u2192 ",
            "Knickpunkt. Zwei Regressionsgeraden verdeutlichen den Knick. ",
            "Die schwarze S=1-Linie dient als Referenz."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Excess CO\u2082:</span> ",
            "ExCO\u2082 = (VCO\u2082\u00b2 / VO\u2082) \u2013 VCO\u2082. ",
            "Das Minimum der geglätteten Kurve entspricht VT1 ",
            "(Wasserman-Methode)."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Ventilatorische Äquivalente:</span> ",
            "VE/VO\u2082 steigt (mehr Ventilation pro O\u2082-Aufnahme), ",
            "während VE/VCO\u2082 noch stabil bleibt oder sogar fällt. ",
            "Dieser disproportionale Anstieg kennzeichnet VT1."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Endtidale Drücke:</span> ",
            "PetO\u2082 steigt (Hyperventilation relativ zu O\u2082), ",
            "PetCO\u2082 bleibt stabil (CO\u2082-Produktion und ",
            "-Abatmung im Gleichgewicht).")))
        ),

        # ── VT2 ────────────────────────────────────
        shiny::div(class = "help-section",
          shiny::tags$h5(shiny::HTML("&#9679;"),
            style = "color:#6B8E23;",
            " VT2"),
          shiny::tags$p(
            "VT2 (auch RCP) markiert den Beginn der respiratorischen ",
            "Kompensation der metabolischen Azidose. Der pH-Abfall ",
            "durch Laktatakkumulation löst eine zusätzliche ",
            "Hyperventilation aus, die über das zur CO\u2082-Elimination ",
            "notwendige Maß hinausgeht."),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>VE vs VCO\u2082:</span> ",
            "x = VCO\u2082, y = VE. Bis VT2 steigt VE proportional zu ",
            "VCO\u2082. Ab VT2 steigt VE überproportional \u2192 ",
            "Knickpunkt. Zwei Regressionsgeraden zeigen den Übergang."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Excess VE:</span> ",
            "ExVE = (VE\u00b2 / VCO\u2082) \u2013 VE. ",
            "Das Minimum der geglätteten Kurve = VT2."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Ventilatorische Äquivalente:</span> ",
            "Jetzt steigt auch VE/VCO\u2082 an (zuvor war nur ",
            "VE/VO\u2082 gestiegen). Beide Äquivalente steigen ",
            "gleichzeitig."))),

          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>Endtidale Drücke:</span> ",
            "PetCO\u2082 beginnt zu fallen (CO\u2082 wird durch ",
            "Hyperventilation verstärkt abgeatmet), PetO\u2082 steigt weiter.")))
        ),

        # ── Empfehlung ─────────────────────────────
        shiny::div(class = "help-section",
          shiny::tags$h5(shiny::icon("lightbulb"),
            " Empfehlung zur Bestimmung"),
          shiny::tags$p(
            "Die Bestimmung sollte nicht aus einem einzelnen Plot ",
            "erfolgen, sondern durch Übereinstimmung mehrerer Methoden:"),
          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>VT1:</span> V-Slope-Knick ",
            "\u2713 + VE/VO\u2082-Anstieg bei stabilem VE/VCO\u2082 ",
            "\u2713 + PetO\u2082-Anstieg bei stabilem PetCO\u2082 \u2713"))),
          shiny::tags$p(shiny::HTML(paste0(
            "<span class='help-key'>VT2:</span> VE-VCO\u2082-Knick ",
            "\u2713 + VE/VCO\u2082-Anstieg \u2713 + PetCO\u2082-Abfall \u2713"))),
          shiny::tags$p(style = "font-size:0.82rem; color:#64748b; margin-top:12px;",
            "Referenzen: Beaver, Wasserman & Whipp (1986); ",
            "ATS/ACCP Statement (2003); ",
            "Guazzi et al. (2012, AHA/EACPR).")
        )
      )
    })

    # ══════════════════════════════════════════════════════════════
    #  HILFE-MODALS (? Buttons pro Plot)
    # ══════════════════════════════════════════════════════════════
    help_modal <- function(title, ...) {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("circle-question"), " ", title),
        ...,
        easyClose = TRUE,
        footer = shiny::modalButton("Schließen"),
        size = "m"))
    }

    shiny::observeEvent(input$help_vslope, {
      help_modal("V-Slope (VO\u2082 vs VCO\u2082)",
        shiny::tags$p("x-Achse: VO\u2082 [L/min], y-Achse: VCO\u2082 [L/min]"),
        shiny::tags$p("Beide steigen zunächst linear (Steigung \u22481). ",
          "Ab VT1 steigt VCO\u2082 überproportional \u2192 Knickpunkt."),
        shiny::tags$p("Die ", shiny::strong("schwarze S=1-Linie"),
          " ist ", shiny::strong("verschiebbar"),
          " (mit der Maus ziehen). Die Steigung bleibt dabei = 1, ",
          "nur der Offset ändert sich. So kann der lineare aerobe ",
          "Bereich tangiert werden \u2013 ab dem Punkt, wo die Daten ",
          "über der Linie nach oben abweichen, liegt VT1."),
        shiny::tags$p("Die ", shiny::strong("zwei Regressionsgeraden"),
          " (S1, S2) verdeutlichen den Steigungswechsel am Breakpoint."))
    })
    shiny::observeEvent(input$help_exco2, {
      help_modal("Excess CO\u2082",
        shiny::div(class="help-formula",
          "ExCO\u2082 = (VCO\u2082\u00b2 / VO\u2082) \u2013 VCO\u2082"),
        shiny::tags$p("Das ", shiny::strong("Minimum"),
          " der geglätteten Kurve markiert VT1."),
        shiny::tags$p("Methode nach Wasserman. Die ExCO\u2082-Kurve zeigt ",
          "den Punkt, ab dem die CO\u2082-Produktion nicht mehr proportional ",
          "zur O\u2082-Aufnahme ist."))
    })
    shiny::observeEvent(input$help_eq_vt1, {
      help_modal("Ventilatorische Äquivalente (VT1)",
        shiny::tags$p(shiny::strong("VE/VO\u2082"), " (rot): Ventilation pro ",
          "Liter O\u2082-Aufnahme."),
        shiny::tags$p(shiny::strong("VE/VCO\u2082"), " (blau): Ventilation pro ",
          "Liter CO\u2082-Abgabe."),
        shiny::tags$p("Bei VT1: VE/VO\u2082 steigt systematisch an (\u2191), ",
          "während VE/VCO\u2082 noch stabil bleibt (\u2192)."))
    })
    shiny::observeEvent(input$help_pet_vt1, {
      help_modal("Endtidale Drücke (VT1)",
        shiny::tags$p(shiny::strong("PetO\u2082"), " (blau, linke Achse): ",
          "Endtidaler O\u2082-Partialdruck."),
        shiny::tags$p(shiny::strong("PetCO\u2082"), " (lila, rechte Achse): ",
          "Endtidaler CO\u2082-Partialdruck."),
        shiny::tags$p("Bei VT1: PetO\u2082 steigt (\u2191), ",
          "PetCO\u2082 bleibt stabil (\u2192)."))
    })
    shiny::observeEvent(input$help_ve_vco2, {
      help_modal("VE vs VCO\u2082",
        shiny::tags$p("x-Achse: VCO\u2082 [L/min], y-Achse: VE [L/min]"),
        shiny::tags$p("Bis VT2 steigt die Ventilation proportional zur ",
          "CO\u2082-Produktion. Ab VT2 beginnt die respiratorische ",
          "Kompensation: VE steigt überproportional."),
        shiny::tags$p("Zwei Regressionsgeraden zeigen den Knick."))
    })
    shiny::observeEvent(input$help_exve, {
      help_modal("Excess VE",
        shiny::div(class="help-formula",
          "ExVE = (VE\u00b2 / VCO\u2082) \u2013 VE"),
        shiny::tags$p("Das ", shiny::strong("Minimum"),
          " der geglätteten Kurve markiert VT2."),
        shiny::tags$p("Analog zur ExCO\u2082-Methode für VT1."))
    })
    shiny::observeEvent(input$help_eq_vt2, {
      help_modal("Ventilatorische Äquivalente (VT2)",
        shiny::tags$p("Bei VT2: Jetzt steigt auch ",
          shiny::strong("VE/VCO\u2082"), " an (\u2191)."),
        shiny::tags$p("Zuvor (zwischen VT1 und VT2) war nur VE/VO\u2082 ",
          "gestiegen, VE/VCO\u2082 war stabil. Ab VT2 steigen beide."))
    })
    shiny::observeEvent(input$help_pet_vt2, {
      help_modal("Endtidale Drücke (VT2)",
        shiny::tags$p("Bei VT2: ", shiny::strong("PetCO\u2082 fällt"),
          " (\u2193) \u2013 CO\u2082 wird durch die kompensatorische ",
          "Hyperventilation verstärkt abgeatmet."),
        shiny::tags$p("PetO\u2082 steigt weiterhin (\u2191)."))
    })
    shiny::observeEvent(input$help_overview, {
      help_modal("Übersicht: VO\u2082 & VCO\u2082",
        shiny::tags$p("Zeitverlauf von VO\u2082 (rot) und VCO\u2082 (blau). ",
          "Zeigt den Gesamtverlauf der Gasaustauschparameter während der ",
          "Belastung. VT-Linien als Orientierung."))
    })
    shiny::observeEvent(input$help_rer, {
      help_modal("RER (Respiratorischer Quotient)",
        shiny::div(class="help-formula", "RER = VCO\u2082 / VO\u2082"),
        shiny::tags$p("RER < 1.0: vorwiegend aerober Stoffwechsel."),
        shiny::tags$p("RER > 1.0: CO\u2082-Produktion übersteigt ",
          "O\u2082-Aufnahme (anaerobe Schwelle überschritten)."),
        shiny::tags$p("RER > 1.1: häufiges Ausbelastungskriterium."),
        shiny::tags$p(style="color:#64748b; font-size:0.82rem;",
          "Unterstützend, nicht allein zur VT-Bestimmung verwenden."))
    })

    # ══════════════════════════════════════════════════════════════
    #  VT-TABLE FÜR EXPORT
    # ══════════════════════════════════════════════════════════════
    vt_table_r <- shiny::reactive({
      p  <- tryCatch(params_reactive(), error = function(e) NULL)
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      if (is.null(ts) || is.null(p) || nrow(ts) < 5) return(NULL)
      idx1 <- if (is.finite(vt$vt1_time))
        which.min(abs(ts$time_min - vt$vt1_time)) else NA_integer_
      idx2 <- if (is.finite(vt$vt2_time))
        which.min(abs(ts$time_min - vt$vt2_time)) else NA_integer_
      build_vt_table(ts, idx1, idx2, params = p,
        vt1_method = vt$vt1_method, vt2_method = vt$vt2_method,
        vt1_confirmed = vt$vt1_confirmed, vt2_confirmed = vt$vt2_confirmed)
    })

    # ══════════════════════════════════════════════════════════════
    #  RETURN
    # ══════════════════════════════════════════════════════════════
    return(list(
      vt_state      = vt,
      vt_table      = vt_table_r,
      apply_trigger = shiny::reactive(input$apply_9p)
    ))
  })
}
