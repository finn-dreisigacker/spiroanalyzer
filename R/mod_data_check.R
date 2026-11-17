# ============================================================
#  mod_data_check.R
#  Sub-Tab "Datenüberprüfung"
#    - Ausreißer (mit Klick-Modal: Behalten/Ausschließen/auf Median)
#    - Stufenübersicht (Custom-HTML, mod_steps-Stil)
#    - Gesamtverlauf (abs + normalisiert eingeklappt)
#  Kommentare leben im VT-Modul; hier nur Datenqualität.
# ============================================================

# ---- Farbschema ---------------------------------------------
.dc_colors <- list(
  raw            = "rgba(148, 163, 184, 0.55)",
  raw_excluded   = "rgba(148, 163, 184, 0.18)",
  median_line    = "#1d1d1f",
  band_fill      = "rgba(31, 61, 107, 0.15)",
  candidate      = "#F59E0B",
  strong         = "#EF4444",
  artifact       = "#991B1B",
  manual         = "#475569",
  vt1            = "#006400",
  vt2            = "#6B8E23",
  hr             = "#DC2626",
  vo2            = "#2563EB",
  vco2           = "#EA580C",
  power          = "#0F766E",
  ve             = "#9333EA",
  af             = "#EC4899"
)

.dc_severity_color <- function(severity) {
  vapply(severity, function(s) switch(s,
    candidate        = .dc_colors$candidate,
    strong_candidate = .dc_colors$strong,
    likely_artifact  = .dc_colors$artifact,
    manual           = .dc_colors$manual,
    .dc_colors$candidate
  ), character(1))
}

.dc_severity_label <- function(severity) {
  vapply(severity, function(s) switch(s,
    candidate        = "Kandidat",
    strong_candidate = "Stark",
    likely_artifact  = "Likely Artefakt",
    manual           = "Manuell",
    s
  ), character(1))
}

# Sekundär-Variablen, die optional im Outlier-Plot eingeblendet werden
.dc_secondary_vars <- list(
  HR    = list(label = "HF",       color = "hr"),
  VCO2  = list(label = "V'CO₂",   color = "vco2"),
  VE    = list(label = "V'E",      color = "ve"),
  P     = list(label = "Leistung", color = "power")
)

# ---- UI -----------------------------------------------------
mod_data_check_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0("
      .dc-hint { font-size:0.78rem; color:#888; margin-top:2px; }
      .dc-badge {
        display:inline-block; padding:3px 10px; border-radius:14px;
        background:#e5eefa; color:#1d1d1f; font-weight:700;
        font-size:0.78rem; margin-right:6px;
      }
      .dc-badge-warn { background:#FEF3C7; color:#92400E; }
      .dc-badge-err  { background:#FEE2E2; color:#991B1B; }
      .dc-section-title {
        font-weight:700; color:#1d1d1f; margin:6px 0 8px;
        font-size:0.95rem;
      }
      .dc-action-bar { display:flex; gap:6px; margin-bottom:8px;
                       flex-wrap:wrap; }

      /* Vollbild-Mechanismus für Outlier-Plot (analog 9-Felder) */
      .dc-fs-btn {
        position:absolute; top:8px; right:8px; z-index:20;
        background:#1d1d1f; color:#fff; border:none;
        border-radius:6px; padding:4px 10px; font-size:0.78rem;
        cursor:pointer; opacity:0.85;
      }
      .dc-fs-btn:hover { opacity:1; }
      #", ns("plot_wrap"), ".dc-fullscreen {
        position:fixed; top:0; left:0; width:100vw; height:100vh;
        z-index:9999; background:#fff; overflow-y:auto; padding:14px 20px;
      }

      /* Stufentabelle (custom HTML, mod_steps-Stil) */
      .dc-stage-table { width:100%; border-collapse:collapse;
                        font-size:0.86rem; }
      .dc-stage-table thead th {
        background:linear-gradient(180deg, #f1f5f9 0%, #e2e8f0 100%);
        color:#0f172a; font-weight:700; padding:8px 10px;
        border-bottom:2px solid #94a3b8;
        text-align:right; white-space:nowrap;
        position:sticky; top:0; z-index:5;
      }
      .dc-stage-table thead th:first-child,
      .dc-stage-table thead th:last-child { text-align:left; }
      .dc-stage-table th[title] { cursor:help;
        text-decoration:underline dotted #94a3b8; }
      .dc-stage-table td {
        padding:6px 10px; border-bottom:1px solid #f1f5f9;
        text-align:right; font-variant-numeric:tabular-nums;
      }
      .dc-stage-table td:first-child,
      .dc-stage-table td:last-child { text-align:left; }
      .dc-stage-table tbody tr:hover td { background:#f0f9ff; }
      .dc-stage-pill {
        display:inline-block; padding:3px 12px; border-radius:14px;
        font-weight:800; color:#fff; font-size:0.78rem;
        background:linear-gradient(135deg, #2563eb, #1d4ed8);
        letter-spacing:0.04em;
      }
      .dc-power-bar {
        display:inline-block; height:8px; border-radius:4px;
        background:linear-gradient(90deg, #93c5fd, #1d4ed8);
        vertical-align:middle; margin-left:6px;
      }
      .dc-empty {
        color:#94a3b8; padding:24px; text-align:center; font-style:italic;
      }
    "))),

    # JS für Vollbild
    shiny::tags$script(shiny::HTML(paste0("
      Shiny.addCustomMessageHandler('dcFS_", id, "', function(msg) {
        var el = document.getElementById('", ns("plot_wrap"), "');
        el.classList.toggle('dc-fullscreen');
        var btn = el.querySelector('.dc-fs-btn');
        if (el.classList.contains('dc-fullscreen')) {
          btn.innerHTML = '<i class=\"fa fa-compress\"></i> Vollbild beenden';
        } else {
          btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild';
        }
      });
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          var el = document.getElementById('", ns("plot_wrap"), "');
          if (el && el.classList.contains('dc-fullscreen')) {
            el.classList.remove('dc-fullscreen');
            var btn = el.querySelector('.dc-fs-btn');
            if (btn) btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild';
          }
        }
      });
    "))),

    shiny::div(style = "display:none;",
      shiny::div(
        # ----- Ausreisser-Sidebar (ausgeblendet: Tab ist Coming-Soon;
        #        Inputs bleiben im DOM, damit Server-Referenzen gültig sind) -
        shiny::conditionalPanel(
          condition = "false",
          shiny::div(class = "sa-card",
            shiny::tags$h6("Ausreißer-Erkennung",
              style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
            shiny::numericInput(ns("window_sec"),
              "Fenstergröße (Sekunden)",
              value = 60, min = 20, max = 180, step = 5, width = "100%"),
            shiny::tags$label("Median-Ausrichtung:",
              style = "font-size:0.83rem; font-weight:600; color:#444;"),
            shiny::radioButtons(ns("align"), label = NULL, inline = TRUE,
              choices = c("zentriert" = "center", "trailing" = "right"),
              selected = "center"),
            shiny::tags$p("Rolling Median/MAD; |z|≥3 markiert.",
              class = "dc-hint"),
            shiny::hr(style = "margin:8px 0;"),
            shiny::tags$h6("Default-Aktion",
              style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
            shiny::checkboxInput(ns("def_strong"),
              "Starke Kandidaten (|z|≥4,5) ausschließen",
              value = FALSE),
            shiny::checkboxInput(ns("def_artifact"),
              "Sehr wahrscheinliche Artefakte (|z|≥6) ausschließen",
              value = TRUE)
          ),
          shiny::div(class = "sa-card",
            shiny::tags$h6("Plot-Anzeige",
              style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
            shiny::selectInput(ns("plot_var"),
              "Plot-Variable",
              choices = c("VO₂" = "VO2abs"), selected = "VO2abs",
              width = "100%"),
            shiny::tags$label("Sekundär anzeigen (% vom Maximum):",
              style = "font-size:0.83rem; font-weight:600; color:#444;"),
            shiny::checkboxGroupInput(ns("plot_sec"),
              label = NULL, inline = TRUE,
              choices = c("HF" = "HR", "V'CO₂" = "VCO2",
                          "V'E" = "VE", "Leistung" = "P"),
              selected = character(0)),
            shiny::tags$label("Punkte:",
              style = "font-size:0.83rem; font-weight:600; color:#444;
                       margin-top:6px;"),
            shiny::radioButtons(ns("points_mode"),
              label = NULL, inline = TRUE,
              choices = c("raw" = "raw",
                          "ohne Ausreißer" = "clean"),
              selected = "raw"),
            shiny::tags$label("Plot-Höhe:",
              style = "font-size:0.83rem; font-weight:600; color:#444;
                       margin-top:6px;"),
            shiny::radioButtons(ns("plot_height"),
              label = NULL, inline = TRUE,
              choices = c("normal" = "normal", "groß" = "large"),
              selected = "large")
          ),
          shiny::div(class = "sa-card",
            shiny::tags$h6("Tabelle",
              style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
            shiny::checkboxInput(ns("opt_show_table"),
              "Tabelle anzeigen", value = TRUE),
            shiny::checkboxInput(ns("opt_filter_var"),
              "Nur aktuelle Plot-Variable filtern", value = TRUE),
            shiny::checkboxInput(ns("opt_manual"),
              "Manuelle Bedienung aktivieren", value = TRUE)
          )
        ),
        # ----- Stufenuebersicht-Sidebar (ausgeblendet: Coming-Soon) -----
        shiny::conditionalPanel(
          condition = "false",
          shiny::div(class = "sa-card",
            shiny::tags$h6("Stufen-Mittelwerte",
              style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
            shiny::radioButtons(ns("mean_method"),
              label = NULL,
              choices = c(
                "Ganze Stufe"        = "whole",
                "Letzte 30 Sekunden" = "last30",
                "Letzte 60 Sekunden" = "last60",
                "Benutzerdefiniert"  = "custom"),
              selected = "last30"),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'",
                                   ns("mean_method")),
              shiny::numericInput(ns("custom_seconds"),
                "Sekunden am Stufenende",
                value = 60, min = 10, max = 180, step = 5, width = "100%")),
            shiny::hr(style = "margin:8px 0;"),
            shiny::checkboxInput(ns("opt_apply"),
              "Ausreißer aus Mittelwertberechnung ausschließen",
              value = TRUE)
          )
        ),
      ),
      # Kein sichtbarer Bereich: nur Platzhalter für Server-Referenzen
      shiny::div(style = "display:none;",
        bslib::navset_hidden(id = ns("nav"),
          bslib::nav_panel_hidden("Gesamtverlauf", NULL)))
    ),

    shiny::div(class = "sa-card dc-card",
      shiny::div(class = "dc-toolbar",
        shiny::div(class = "dc-ctl",
          shiny::tags$label("Ansicht"),
          shiny::radioButtons(ns("ov_view"), NULL, inline = TRUE,
            choices = c("Absolut" = "abs", "Relativ zum Maximum" = "norm"))),
        shiny::div(class = "dc-ctl",
          shiny::tags$label("Hintergrund"),
          shiny::radioButtons(ns("ov_bg"), NULL, inline = TRUE,
            choices = c("Testphasen" = "phase", "Belastungsstufen" = "stage", "Keiner" = "none")))),
      shiny::conditionalPanel(sprintf("input['%s'] == 'abs'", ns("ov_view")),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("overview_abs"), height = "620px"), type = 6)),
      shiny::conditionalPanel(sprintf("input['%s'] == 'norm'", ns("ov_view")),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("overview_norm"), height = "520px"), type = 6)),
      shiny::div(class = "dc-legend", shiny::uiOutput(ns("bg_legend"), inline = TRUE)))
  )
}

# Hintergrund-Rechtecke (Plotly-Shapes) nach Testphase oder Stufe
dc_bg_shapes <- function(ts, mode) {
  rect <- function(x0, x1, fill) list(type = "rect", xref = "x", yref = "paper",
    x0 = x0, x1 = x1, y0 = 0, y1 = 1, fillcolor = fill, line = list(width = 0), layer = "below")
  if (identical(mode, "phase")) {
    ph <- as.character(ts$Phase); ph[is.na(ph)] <- ""
    r <- rle(ph); e <- cumsum(r$lengths); st <- e - r$lengths + 1
    lapply(seq_along(r$values), function(i) {
      f <- DC_PHASE_FILL[r$values[i]]
      if (is.na(f)) return(NULL)
      rect(ts$time_min[st[i]], ts$time_min[min(e[i] + 1, nrow(ts))], unname(f))
    }) |> Filter(f = Negate(is.null))
  } else if (identical(mode, "stage")) {
    sr <- stage_ranges(ts, stage_col = "stage", time_col = "time_s")
    lapply(seq_len(nrow(sr)), function(i) rect(sr$x_start[i] / 60, sr$x_end[i] / 60,
      if (i %% 2 == 0) "rgba(0,0,0,0.025)" else "rgba(0,0,0,0.06)"))
  } else list()
}
DC_PHASE_FILL <- c("Erwärmung" = "rgba(252,211,77,0.22)",
                   "Belastung" = "rgba(147,197,253,0.22)",
                   "Erholung"  = "rgba(216,180,254,0.25)")

dc_vt_shapes <- function(p) {
  out <- list(shapes = list(), annot = list())
  for (k in c("vt1", "vt2")) {
    t <- p[[paste0(k, "_time")]] %||% NA
    if (!is.finite(t)) next
    col <- .dc_colors[[k]]
    out$shapes[[length(out$shapes) + 1]] <- list(type = "line", xref = "x", yref = "paper",
      x0 = t, x1 = t, y0 = 0, y1 = 1, line = list(color = col, width = 1.5, dash = "dash"))
    out$annot[[length(out$annot) + 1]] <- list(x = t, y = 1, xref = "x", yref = "paper",
      text = toupper(k), showarrow = FALSE, yanchor = "bottom",
      font = list(color = col, size = 11))
  }
  out
}

DC_AXIS <- list(gridcolor = "#ececf0", zeroline = FALSE, linecolor = "#d2d2d7",
                tickfont = list(size = 11, color = "#6e6e73"),
                titlefont = list(size = 12, color = "#1d1d1f"))

# ============================================================
#  SERVER
# ============================================================
mod_data_check_server <- function(id, params_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State -------------------------------------------
    # decisions: Liste mit Schluessel "<row_id>_<variable>";
    # Wert = list(action, replacement_value, comment)
    state <- shiny::reactiveValues(
      decisions = list(),
      last_params_sig = NULL
    )

    # Reset bei Datei-Wechsel (selektiv: nur Decisions)
    shiny::observeEvent(params_reactive(), {
      p <- params_reactive()
      sig <- if (is.null(p)) NULL else
        paste0(deparse(list(p$ID, p$Timepoint, p$Date,
          if (!is.null(p$ts)) nrow(p$ts) else 0)), collapse = "|")
      if (!identical(sig, state$last_params_sig)) {
        state$decisions <- list()
        state$last_params_sig <- sig
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Vollbild-Toggle
    shiny::observeEvent(input$plot_fullscreen, {
      session$sendCustomMessage(paste0("dcFS_", id), list())
    })

    # ---- Daten -------------------------------------------
    ts_r <- shiny::reactive({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts), nrow(p$ts) > 0)
      p$ts
    })

    stage_assign_r <- shiny::reactive({
      resolve_stage_assignment(ts_r())
    })

    ts_with_stage_r <- shiny::reactive({
      ts <- ts_r(); sa <- stage_assign_r()
      ts$stage <- sa$stage
      ts
    })

    avail_vars_r <- shiny::reactive({
      available_outlier_variables(ts_r())
    })

    # Variablen-Dropdown dynamisch
    shiny::observeEvent(avail_vars_r(), {
      vars <- avail_vars_r()
      if (length(vars) == 0L) return()
      labels <- vapply(vars, outlier_variable_label, character(1))
      choices <- stats::setNames(vars, labels)
      sel <- if (!is.null(input$plot_var) && input$plot_var %in% vars)
        input$plot_var else
        if ("VO2abs" %in% vars) "VO2abs" else vars[1]
      shiny::updateSelectInput(session, "plot_var",
        choices = choices, selected = sel)
    })

    # Debounced Inputs
    window_sec_d <- shiny::reactive({
      v <- input$window_sec
      if (is.null(v) || !is.finite(v)) 60
      else max(20, min(180, as.numeric(v)))
    }) |> shiny::debounce(400)

    custom_sec_d <- shiny::reactive({
      v <- input$custom_seconds
      if (is.null(v) || !is.finite(v)) 60
      else max(10, min(180, as.numeric(v)))
    }) |> shiny::debounce(400)

    align_r <- shiny::reactive(input$align %||% "center")

    # Outlier-Detection (cached) — Detection auf Roh-Median
    candidates_raw_r <- shiny::reactive({
      detect_spiro_outliers(ts_with_stage_r(),
        time_col   = "time_s",
        variables  = avail_vars_r(),
        window_sec = window_sec_d(),
        stage_col  = "stage",
        align      = align_r())
    })

    # Effektive Aktionen + Manuelle Einträge auf Nicht-Kandidaten
    candidates_r <- shiny::reactive({
      cand <- candidates_raw_r()

      def_strong   <- isTRUE(input$def_strong)
      def_artifact <- isTRUE(input$def_artifact)

      if (nrow(cand) > 0L) {
        auto_action <- ifelse(
          cand$severity == "likely_artifact"  & def_artifact, "exclude",
          ifelse(cand$severity == "strong_candidate" & def_strong,  "exclude",
                 "keep"))
        keys <- paste0(cand$row_id, "_", cand$variable)
        man_action  <- vapply(keys, function(k) {
          d <- state$decisions[[k]]
          if (is.null(d)) NA_character_ else d$action
        }, character(1))
        man_repval <- vapply(keys, function(k) {
          d <- state$decisions[[k]]
          if (is.null(d) || is.null(d$replacement_value))
            NA_real_ else as.numeric(d$replacement_value)
        }, numeric(1))
        man_comm <- vapply(keys, function(k) {
          d <- state$decisions[[k]]
          if (is.null(d)) "" else (d$comment %||% "")
        }, character(1))
        cand$action  <- ifelse(!is.na(man_action), man_action, auto_action)
        cand$replacement_value <- man_repval
        cand$comment <- man_comm
      } else {
        cand$replacement_value <- numeric(0)
      }

      # Manuelle Einträge auf Nicht-Kandidaten -> als severity="manual" anhängen
      ts <- ts_with_stage_r()
      keys_cand <- if (nrow(cand) > 0L)
        paste0(cand$row_id, "_", cand$variable) else character(0)
      keys_dec  <- names(state$decisions)
      keys_extra <- setdiff(keys_dec, keys_cand)
      if (length(keys_extra) > 0L) {
        parts <- strsplit(keys_extra, "_", fixed = TRUE)
        rid <- suppressWarnings(as.integer(vapply(parts, `[`, character(1), 1)))
        var <- vapply(parts, function(p) paste(p[-1], collapse = "_"),
                      character(1))
        ok <- !is.na(rid) & rid >= 1L & rid <= nrow(ts) & var %in% names(ts)
        rid <- rid[ok]; var <- var[ok]; keys_extra <- keys_extra[ok]
        if (length(keys_extra) > 0L) {
          act_x <- vapply(keys_extra, function(k) state$decisions[[k]]$action,
                          character(1))
          repv_x <- vapply(keys_extra, function(k) {
            v <- state$decisions[[k]]$replacement_value
            if (is.null(v)) NA_real_ else as.numeric(v)
          }, numeric(1))
          comm_x <- vapply(keys_extra, function(k) {
            state$decisions[[k]]$comment %||% ""
          }, character(1))
          val_x <- vapply(seq_along(rid), function(i)
            as.numeric(ts[[var[i]]][rid[i]]), numeric(1))
          extra <- tibble::tibble(
            row_id            = rid,
            time_sec          = as.numeric(ts$time_s)[rid],
            stage             = as.character(ts$stage)[rid],
            variable          = var,
            value             = val_x,
            rolling_median    = NA_real_,
            rolling_MAD       = NA_real_,
            robust_z          = NA_real_,
            reason            = "manuell",
            severity          = "manual",
            action            = act_x,
            comment           = comm_x,
            replacement_value = repv_x
          )
          cand <- dplyr::bind_rows(cand, extra)
        }
      }

      if (nrow(cand) > 0L)
        cand <- dplyr::arrange(cand, time_sec, variable)
      cand
    })

    # exclude_row_ids (Zeilen-Level fuer Aggregation)
    excluded_row_ids_r <- shiny::reactive({
      cand <- candidates_r()
      if (nrow(cand) == 0L) return(integer(0))
      sort(unique(cand$row_id[cand$action == "exclude"]))
    })

    # replacements_map (Zellen-Level fuer Aggregation und fuer Plot-Median)
    replacements_r <- shiny::reactive({
      cand <- candidates_r()
      if (nrow(cand) == 0L) return(list())
      sub <- cand[cand$action == "replace_median" &
                  is.finite(cand$replacement_value), , drop = FALSE]
      if (nrow(sub) == 0L) return(list())
      out <- list()
      for (v in unique(sub$variable)) {
        s2 <- sub[sub$variable == v, , drop = FALSE]
        out[[v]] <- stats::setNames(as.numeric(s2$replacement_value),
                                     as.character(s2$row_id))
      }
      out
    })

    # ---- Stufen-Mittelwerte ------------------------------
    stage_summary_r <- shiny::reactive({
      sa <- stage_assign_r(); shiny::req(sa$available)
      p  <- params_reactive()
      wkg <- if (!is.null(p) && is.finite(p$Weight_kg %||% NA))
        p$Weight_kg else NA_real_
      excl <- if (isTRUE(input$opt_apply)) excluded_row_ids_r() else integer(0)
      repl <- if (isTRUE(input$opt_apply)) replacements_r() else list()
      calculate_stage_summary(ts_with_stage_r(),
        stage_col = "stage", time_col = "time_s",
        method = input$mean_method %||% "last30",
        custom_seconds  = custom_sec_d(),
        exclude_row_ids = excl,
        replacements    = repl,
        weight_kg       = wkg)
    })

    # ---- Outlier-Badges ----------------------------------
    output$outlier_badges <- shiny::renderUI({
      cand <- candidates_r()
      n_total <- nrow(cand)
      n_excl  <- if (n_total > 0) sum(cand$action == "exclude") else 0
      n_repl  <- if (n_total > 0) sum(cand$action == "replace_median") else 0
      n_strong <- if (n_total > 0)
        sum(cand$severity == "strong_candidate", na.rm = TRUE) else 0
      n_art   <- if (n_total > 0)
        sum(cand$severity == "likely_artifact", na.rm = TRUE) else 0

      shiny::tagList(
        shiny::span(class = "dc-badge",
          paste0("Kandidaten: ", n_total)),
        shiny::span(class = if (n_strong > 0) "dc-badge dc-badge-warn"
                            else "dc-badge",
          paste0("|z|≥4,5: ", n_strong)),
        shiny::span(class = if (n_art > 0) "dc-badge dc-badge-err"
                            else "dc-badge",
          paste0("|z|≥6: ", n_art)),
        shiny::span(class = "dc-badge",
          paste0("Ausgeschlossen: ", n_excl)),
        shiny::span(class = "dc-badge",
          paste0("Ersetzt: ", n_repl))
      )
    })

    # ---- Stufen-Badges -----------------------------------
    output$stage_badges <- shiny::renderUI({
      sa <- stage_assign_r()
      cls <- if (sa$available) "dc-badge"
             else if (isTRUE(sa$ramp)) "dc-badge dc-badge-warn"
             else "dc-badge dc-badge-err"
      n_st <- if (sa$available)
        length(unique(stats::na.omit(sa$stage))) else 0L
      shiny::tagList(
        shiny::span(class = cls, paste0("Stufenquelle: ", sa$source_label)),
        if (sa$available)
          shiny::span(class = "dc-badge",
            paste0("Anzahl Stufen: ", n_st))
      )
    })

    # ---- Outlier-Plot ------------------------------------
    output$outlier_plot_ui <- shiny::renderUI({
      h <- if (isTRUE(input$plot_height == "large")) "720px" else "480px"
      plotly::plotlyOutput(ns("outlier_plot"), height = h)
    })

    output$outlier_plot <- plotly::renderPlotly({
      ts <- ts_with_stage_r()
      var <- input$plot_var %||% "VO2abs"
      shiny::validate(
        shiny::need(var %in% names(ts),
          "Ausgewählte Variable nicht im Datensatz vorhanden."))
      vals <- as.numeric(ts[[var]])
      shiny::validate(shiny::need(any(is.finite(vals)),
        "Keine numerischen Werte für die ausgewählte Variable."))

      # Median + Band aus bereinigtem Datensatz
      excl <- excluded_row_ids_r()
      rep_for_var <- replacements_r()[[var]]
      if (is.null(rep_for_var)) rep_for_var <- numeric(0)
      rm <- rolling_median_mad_time_cleaned(
        as.numeric(ts$time_s), vals,
        exclude_row_ids = excl,
        replacement_map = rep_for_var,
        window_sec = window_sec_d(),
        align = align_r())
      band_hi <- rm$median + 3 * 1.4826 * rm$mad
      band_lo <- rm$median - 3 * 1.4826 * rm$mad

      label <- outlier_variable_label(var)

      # Effektive Punktwerte fuer Anzeige (replacement angewendet, je nach Modus)
      vals_eff <- vals
      if (length(rep_for_var) > 0L) {
        rid <- suppressWarnings(as.integer(names(rep_for_var)))
        ok  <- !is.na(rid) & rid >= 1L & rid <= length(vals_eff)
        if (any(ok)) vals_eff[rid[ok]] <- as.numeric(rep_for_var[ok])
      }
      points_clean <- isTRUE(input$points_mode == "clean")

      df <- data.frame(
        time_s = as.numeric(ts$time_s),
        value_raw = vals,
        value_eff = vals_eff,
        med = rm$median, bhi = band_hi, blo = band_lo,
        row_id = seq_len(nrow(ts)),
        stage = ts$stage,
        is_excluded = seq_len(nrow(ts)) %in% excl,
        stringsAsFactors = FALSE
      )
      df$value <- if (points_clean) df$value_eff else df$value_raw

      cand_all <- candidates_r()
      cand_var <- cand_all[cand_all$variable == var, , drop = FALSE]

      pl <- plotly::plot_ly(source = ns("outlier_plot_src"))

      # Stufenhintergrund
      sr <- stage_ranges(ts, stage_col = "stage", time_col = "time_s")
      shapes <- list(); annotations <- list()
      if (nrow(sr) > 0) {
        for (i in seq_len(nrow(sr))) {
          col_alt <- if (i %% 2 == 0) "rgba(31, 61, 107, 0.04)"
                     else              "rgba(31, 61, 107, 0.08)"
          shapes[[length(shapes) + 1]] <- list(
            type = "rect", xref = "x", yref = "paper",
            x0 = sr$x_start[i], x1 = sr$x_end[i],
            y0 = 0, y1 = 1, fillcolor = col_alt,
            line = list(width = 0), layer = "below")
          annotations[[length(annotations) + 1]] <- list(
            x = (sr$x_start[i] + sr$x_end[i]) / 2,
            y = 1, xref = "x", yref = "paper",
            text = paste0("S", sr$stage[i]), showarrow = FALSE,
            font = list(size = 10, color = "#5b7fa6"),
            yanchor = "bottom")
        }
      }

      # Band + Median-Linie
      pl <- pl |>
        plotly::add_ribbons(data = df, x = ~time_s,
          ymin = ~blo, ymax = ~bhi,
          fillcolor = .dc_colors$band_fill, line = list(width = 0),
          name = "MAD-Band (±3·MAD·1,4826)", hoverinfo = "skip",
          showlegend = TRUE) |>
        plotly::add_lines(data = df, x = ~time_s, y = ~med,
          line = list(color = .dc_colors$median_line, width = 2),
          name = "Rolling Median (bereinigt)", hoverinfo = "skip")

      # Rohpunkte
      df_show <- df
      if (points_clean) {
        df_show <- df_show[!df_show$is_excluded, , drop = FALSE]
      }
      pl <- pl |>
        plotly::add_markers(data = df_show,
          x = ~time_s, y = ~value,
          marker = list(color = .dc_colors$raw, size = 5,
                        line = list(width = 0)),
          name = "Rohdaten",
          customdata = ~row_id,
          hovertemplate = paste0(
            "<b>Zeit:</b> %{x:.1f} s<br>",
            "<b>", label, ":</b> %{y:.2f}<br>",
            "<b>row_id:</b> %{customdata}<extra></extra>"))

      # Kandidaten/Manuelle Marker
      if (nrow(cand_var) > 0) {
        for (sev in unique(cand_var$severity)) {
          dsub <- cand_var[cand_var$severity == sev, , drop = FALSE]
          if (nrow(dsub) == 0) next
          col_sev <- .dc_severity_color(sev)[1]
          sev_label <- switch(sev,
            candidate         = "Kandidat (|z|≥3)",
            strong_candidate  = "Starker Kandidat (|z|≥4,5)",
            likely_artifact   = "Sehr wahrscheinl. Artefakt (|z|≥6)",
            manual            = "Manuell",
            sev)
          pl <- pl |>
            plotly::add_markers(data = dsub,
              x = ~time_sec, y = ~value,
              marker = list(color = col_sev, size = 9,
                            line = list(width = 1, color = "#fff")),
              name = sev_label,
              customdata = ~row_id,
              hovertemplate = paste0(
                "<b>Zeit:</b> %{x:.1f} s<br>",
                "<b>", label, ":</b> %{y:.2f}<br>",
                "<b>z:</b> ",
                ifelse(is.finite(dsub$robust_z),
                       sprintf("%.2f", dsub$robust_z), "—"),
                "<br><b>Grund:</b> ", dsub$reason,
                "<br><b>row_id:</b> %{customdata}<extra></extra>"))
        }
        # Schwarze Outline für manuelle Einträge
        man_pts <- cand_var[cand_var$severity == "manual", , drop = FALSE]
        if (nrow(man_pts) > 0) {
          pl <- pl |>
            plotly::add_markers(data = man_pts,
              x = ~time_sec, y = ~value,
              marker = list(symbol = "circle-open",
                            color = "#000", size = 14,
                            line = list(width = 2, color = "#000")),
              name = "Manuell (Outline)",
              customdata = ~row_id,
              hovertemplate = "manuell<extra></extra>",
              showlegend = FALSE)
        }
        # X-Marker für ausgeschlossene Punkte
        excl_pts <- cand_var[cand_var$action == "exclude", , drop = FALSE]
        if (nrow(excl_pts) > 0) {
          pl <- pl |>
            plotly::add_markers(data = excl_pts,
              x = ~time_sec, y = ~value,
              marker = list(symbol = "x-thin-open",
                            color = "#000", size = 14,
                            line = list(width = 2)),
              name = "Aus Analyse ausgeschlossen",
              customdata = ~row_id,
              hovertemplate = "ausgeschlossen<extra></extra>")
        }
        # Replacement-Marker (Diamant am Median-Wert)
        rep_pts <- cand_var[cand_var$action == "replace_median" &
                            is.finite(cand_var$replacement_value),
                            , drop = FALSE]
        if (nrow(rep_pts) > 0) {
          pl <- pl |>
            plotly::add_markers(data = rep_pts,
              x = ~time_sec, y = ~replacement_value,
              marker = list(symbol = "diamond",
                            color = "#16a34a", size = 11,
                            line = list(width = 1, color = "#fff")),
              name = "Auf Median ersetzt",
              customdata = ~row_id,
              hovertemplate = paste0(
                "<b>Zeit:</b> %{x:.1f} s<br>",
                "<b>Ersatzwert:</b> %{y:.2f}<extra></extra>"))
        }
      }

      # Sekundär-Variablen (auf gemeinsamer rechter %-Achse)
      sec_vars <- input$plot_sec %||% character(0)
      sec_vars <- intersect(sec_vars, names(ts))
      use_y2 <- length(sec_vars) > 0L
      if (use_y2) {
        for (sv in sec_vars) {
          x_v <- as.numeric(ts[[sv]])
          mx <- suppressWarnings(max(x_v, na.rm = TRUE))
          if (!is.finite(mx) || mx <= 0) next
          y_pct <- x_v / mx * 100
          col <- .dc_colors[[ .dc_secondary_vars[[sv]]$color ]]
          lbl <- paste0(.dc_secondary_vars[[sv]]$label, " (% Max)")
          pl <- pl |>
            plotly::add_lines(x = as.numeric(ts$time_s), y = y_pct,
              yaxis = "y2",
              line = list(color = col, width = 1.4, dash = "dot"),
              name = lbl,
              hovertemplate = paste0(lbl,
                ": %{y:.1f}<extra></extra>"))
        }
      }

      lyt <- list(
        xaxis = list(title = "Zeit [s]"),
        yaxis = list(title = label),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0, y = -0.18))
      if (length(shapes) > 0) lyt$shapes <- shapes
      if (length(annotations) > 0) lyt$annotations <- annotations
      if (use_y2) {
        lyt$yaxis2 <- list(title = "Sekundär [% Max]",
                           overlaying = "y", side = "right",
                           range = c(0, 110), showgrid = FALSE)
      }

      do.call(plotly::layout, c(list(p = pl), lyt)) |>
        plotly::event_register("plotly_click") |>
        plotly::config(displaylogo = FALSE)
    })

    # ---- Klick im Plot -> Modal --------------------------
    shiny::observeEvent(plotly::event_data(
      "plotly_click", source = ns("outlier_plot_src")), {
      ed <- plotly::event_data("plotly_click",
                                source = ns("outlier_plot_src"))
      if (is.null(ed) || is.null(ed$customdata)) return()
      if (!isTRUE(input$opt_manual)) {
        shiny::showNotification(
          "Manuelle Bedienung ist deaktiviert.",
          type = "warning", duration = 3); return()
      }
      rid <- as.integer(ed$customdata)
      var <- input$plot_var %||% "VO2abs"
      ts <- ts_with_stage_r()
      if (rid < 1L || rid > nrow(ts)) return()
      raw_val <- as.numeric(ts[[var]][rid])
      time_s_v <- as.numeric(ts$time_s[rid])

      # Aktuellen Median + |z| zum Snapshotten
      rm_pt <- rolling_median_mad_time(
        as.numeric(ts$time_s), as.numeric(ts[[var]]),
        window_sec = window_sec_d(), align = align_r())
      med_v <- rm_pt$median[rid]
      mad_v <- rm_pt$mad[rid]
      z_v <- if (is.finite(med_v) && is.finite(mad_v) && mad_v > 0)
        (raw_val - med_v) / (1.4826 * mad_v) else NA_real_

      key <- paste0(rid, "_", var)
      prev <- state$decisions[[key]]
      prev_action <- if (!is.null(prev)) prev$action else "keep"
      prev_comment <- if (!is.null(prev)) prev$comment %||% "" else ""

      label <- outlier_variable_label(var)
      med_txt  <- if (is.finite(med_v)) sprintf("%.2f", med_v) else "—"
      z_txt    <- if (is.finite(z_v)) sprintf("%.2f", abs(z_v)) else "—"
      val_txt  <- if (is.finite(raw_val)) sprintf("%.2f", raw_val) else "—"
      time_txt <- if (is.finite(time_s_v)) sprintf("%.1f", time_s_v) else "—"

      shiny::showModal(shiny::modalDialog(
        title = paste0("Punkt-Aktion bei Zeit ", time_txt,
                       " s — ", label, " = ", val_txt),
        shiny::div(
          shiny::tags$p(shiny::HTML(paste0(
            "<strong>Aktueller Rolling-Median:</strong> ", med_txt,
            " &nbsp;&nbsp; <strong>|z|:</strong> ", z_txt))),
          shiny::radioButtons(ns("modal_action"),
            label = "Aktion",
            choices = c(
              "Behalten"                      = "keep",
              "Aus Analyse ausschließen"      = "exclude",
              "Auf Rolling-Median setzen"     = "replace_median"),
            selected = prev_action),
          shiny::tags$p(class = "dc-hint",
            paste0("Bei \"Auf Median setzen\" wird der Wert ", med_txt,
                   " als Ersatzwert gespeichert (Snapshot). ",
                   "Rohdaten bleiben unverändert.")),
          shiny::textAreaInput(ns("modal_comment"),
            label = "Kommentar (optional)", value = prev_comment,
            rows = 3, width = "100%")),
        footer = shiny::tagList(
          shiny::modalButton("Abbrechen"),
          shiny::actionButton(ns("modal_save"), "Speichern",
            class = "btn-primary")),
        easyClose = TRUE,
        size = "m"
      ))
      session$userData[[paste0("modal_ctx_", id)]] <- list(
        key = key, rid = rid, var = var,
        median_snapshot = med_v)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$modal_save, {
      ctx <- session$userData[[paste0("modal_ctx_", id)]]
      if (is.null(ctx)) { shiny::removeModal(); return() }
      action <- input$modal_action %||% "keep"
      comment_val <- input$modal_comment %||% ""
      d <- state$decisions
      d[[ctx$key]] <- list(
        action = action,
        replacement_value = if (action == "replace_median")
          ctx$median_snapshot else NA_real_,
        comment = comment_val
      )
      if (action == "keep" && !nzchar(trimws(comment_val))) {
        d[[ctx$key]] <- NULL
      }
      state$decisions <- d
      shiny::removeModal()
    })

    # ---- Kandidaten-Tabelle (DT) -------------------------
    candidates_filtered_r <- shiny::reactive({
      cand <- candidates_r()
      if (isTRUE(input$opt_filter_var) && nrow(cand) > 0L) {
        cur_var <- input$plot_var %||% "VO2abs"
        cand <- cand[cand$variable == cur_var, , drop = FALSE]
      }
      cand
    })

    output$candidates_table <- DT::renderDT({
      cand <- candidates_filtered_r()
      shiny::req(isTRUE(input$opt_show_table))

      if (nrow(cand) == 0L) {
        return(DT::datatable(
          data.frame(Hinweis = "Keine Kandidaten — passt!"),
          rownames = FALSE, options = list(dom = "t"),
          class = "table-sm table-hover"))
      }

      action_fmt <- vapply(seq_len(nrow(cand)), function(i) {
        switch(cand$action[i],
          keep            = "behalten",
          exclude         = "ausschließen",
          replace_median  = if (is.finite(cand$replacement_value[i]))
            sprintf("→ Median %.2f", cand$replacement_value[i])
            else "→ Median",
          cand$action[i])
      }, character(1))

      df_show <- tibble::tibble(
        Zeit_s   = round(cand$time_sec, 1),
        Stufe    = cand$stage,
        Variable = vapply(cand$variable, outlier_variable_label,
                          character(1)),
        Wert     = round(cand$value, 3),
        z        = ifelse(is.finite(cand$robust_z),
                          round(cand$robust_z, 2), NA_real_),
        Grund    = cand$reason,
        Schweregrad = .dc_severity_label(cand$severity),
        Aktion    = action_fmt,
        Kommentar = cand$comment
      )

      DT::datatable(
        df_show, rownames = FALSE,
        selection = "multiple",
        options = list(pageLength = 10, order = list(list(0, "asc")),
          columnDefs = list(list(className = "dt-center",
            targets = c(0, 1, 3, 4, 6, 7)))),
        class = "table-sm table-hover"
      ) |>
        DT::formatStyle("Schweregrad", target = "row",
          backgroundColor = DT::styleEqual(
            c("Kandidat", "Stark", "Likely Artefakt", "Manuell"),
            c("#FEF3C7", "#FECACA", "#FCA5A5", "#E5E7EB")))
    })

    output$selection_info <- shiny::renderText({
      sel <- input$candidates_table_rows_selected
      if (is.null(sel) || length(sel) == 0L) return("")
      paste(length(sel), "Zeile(n) ausgewählt")
    })

    selected_keys <- function() {
      sel <- input$candidates_table_rows_selected
      if (is.null(sel) || length(sel) == 0L) return(character(0))
      cand <- candidates_filtered_r()
      sel <- sel[sel <= nrow(cand)]
      if (length(sel) == 0L) return(character(0))
      paste0(cand$row_id[sel], "_", cand$variable[sel])
    }

    apply_action_to_selected <- function(new_action) {
      keys <- selected_keys()
      if (length(keys) == 0L) {
        shiny::showNotification("Keine Zeilen ausgewählt.",
          type = "warning", duration = 3); return()
      }
      if (!isTRUE(input$opt_manual)) {
        shiny::showNotification("Manuelle Bedienung ist deaktiviert.",
          type = "warning", duration = 3); return()
      }
      d <- state$decisions
      for (k in keys) {
        prev <- d[[k]]
        d[[k]] <- list(
          action = new_action,
          replacement_value = if (!is.null(prev))
            prev$replacement_value %||% NA_real_ else NA_real_,
          comment = if (!is.null(prev)) prev$comment %||% "" else ""
        )
        if (new_action == "keep" &&
            (is.null(d[[k]]$comment) || !nzchar(trimws(d[[k]]$comment))))
          d[[k]] <- NULL
      }
      state$decisions <- d
    }

    shiny::observeEvent(input$act_keep,    apply_action_to_selected("keep"))
    shiny::observeEvent(input$act_exclude, apply_action_to_selected("exclude"))

    shiny::observeEvent(input$act_comment, {
      keys <- selected_keys()
      if (length(keys) == 0L) {
        shiny::showNotification("Keine Zeilen ausgewählt.",
          type = "warning", duration = 3); return()
      }
      if (!isTRUE(input$opt_manual)) {
        shiny::showNotification("Manuelle Bedienung ist deaktiviert.",
          type = "warning", duration = 3); return()
      }
      first_key <- keys[1]
      prev_comment <- if (!is.null(state$decisions[[first_key]]))
        state$decisions[[first_key]]$comment %||% "" else ""
      shiny::showModal(shiny::modalDialog(
        title = paste0("Kommentar (", length(keys), " Eintrag/Eintraege)"),
        shiny::textAreaInput(ns("bulk_comment_text"),
          label = NULL, value = prev_comment,
          rows = 5, width = "100%",
          placeholder = "Kommentar eingeben ..."),
        footer = shiny::tagList(
          shiny::modalButton("Abbrechen"),
          shiny::actionButton(ns("bulk_comment_save"),
            "Speichern", class = "btn-primary")),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$bulk_comment_save, {
      keys <- selected_keys()
      if (length(keys) == 0L) { shiny::removeModal(); return() }
      txt <- input$bulk_comment_text %||% ""
      d <- state$decisions
      for (k in keys) {
        prev <- d[[k]]
        d[[k]] <- list(
          action = if (!is.null(prev)) prev$action %||% "keep" else "keep",
          replacement_value = if (!is.null(prev))
            prev$replacement_value %||% NA_real_ else NA_real_,
          comment = txt)
      }
      state$decisions <- d
      shiny::removeModal()
      shiny::showNotification("Kommentar gespeichert.",
        type = "message", duration = 2)
    })

    # ---- Stufen-Tabelle (Custom HTML) --------------------
    output$stage_table_html <- shiny::renderUI({
      sa <- stage_assign_r()
      if (!sa$available) {
        msg <- if (isTRUE(sa$ramp))
          "Rampenprotokoll erkannt — Stufenmittelwerte nicht verfügbar."
        else
          "Keine Stufenzuordnung möglich (weder Stage-Spalte noch ableitbare Stufen aus Leistung/Geschwindigkeit)."
        return(shiny::div(class = "dc-empty",
          shiny::icon("triangle-exclamation"), " ", msg))
      }
      ss <- stage_summary_r()
      if (nrow(ss) == 0L) {
        return(shiny::div(class = "dc-empty",
          shiny::icon("circle-info"),
          " Keine Stufen-Mittelwerte berechnet."))
      }


      fmt_n <- function(v, dp) {
        if (!is.finite(v)) "–"
        else format(round(v, dp), nsmall = dp,
                    decimal.mark = ",", big.mark = ".")
      }
      fmt_t <- function(t) {
        if (!is.finite(t)) "–"
        else {
          m <- floor(t / 60); s <- round(t - m * 60)
          sprintf("%02d:%02d", as.integer(m), as.integer(s))
        }
      }

      cols <- list(
        list("Stufe",          "Stufe",         "stufe",  NULL),
        list("Start",          "Startzeit_s",   "tt",     NULL),
        list("Ende",           "Endzeit_s",     "tt",     NULL),
        list("Dauer [s]",      "Dauer_s",       "0",      NULL),
        list("Speed [km/h]",   "Speed_mean",    "1",
             "Mittlere Geschwindigkeit"),
        list("HF [bpm]",       "HR_mean",       "0",      NULL),
        list("V'O₂ [L/min]", "VO2_mean",   "1",
             NULL),
        list("V'O₂/kg",   "VO2/kg_mean",   "1",      NULL),
        list("V'CO₂ [L/min]","VCO2_mean",  "2",      NULL),
        list("V'E [L/min]",    "VE_mean",       "1",      NULL),
        list("RER",            "RER_mean",      "2",      NULL),
        list("AF",             "AF_mean",       "1",      NULL),
        list("V_T",            "VT_mean",       "2",      NULL),
        list("n_raw",          "n_raw",         "0",
             "Anzahl Rohpunkte im Stufenfenster"),
        list("n_excl",         "n_excluded",    "0",
             "Davon ausgeschlossene"),
        list("n_repl",         "n_replaced",    "0",
             "Auf Median ersetzte Zellen"),
        list("n_valid",        "n_valid",       "0",      NULL),
        list("Methode",        "mean_method",   "raw",    NULL)
      )

      header <- "<thead><tr>"
      for (c in cols) {
        ttl <- if (!is.null(c[[4]])) sprintf(' title="%s"', c[[4]]) else ""
        header <- paste0(header, "<th", ttl, ">", c[[1]], "</th>")
      }
      header <- paste0(header, "</tr></thead>")

      body <- ""
      for (i in seq_len(nrow(ss))) {
        body <- paste0(body, "<tr>")
        r <- ss[i, ]
        for (c in cols) {
          key <- c[[2]]; fmt <- c[[3]]
          v <- r[[key]]
          txt <- switch(fmt,
            stufe   = sprintf("<span class='dc-stage-pill'>S%s</span>", v),
            tt      = fmt_t(v),
            "0"     = fmt_n(v, 0),
            "1"     = fmt_n(v, 1),
            "2"     = fmt_n(v, 2),
            raw     = if (is.na(v)) "–" else as.character(v),
            fmt_n(v, 2))
          body <- paste0(body, "<td>", txt, "</td>")
        }
        body <- paste0(body, "</tr>")
      }
      shiny::HTML(paste0(
        '<div style="overflow-x:auto;">',
        '<table class="dc-stage-table">',
        header, '<tbody>', body, '</tbody></table></div>'))
    })

    output$bg_legend <- shiny::renderUI({
      if (!identical(input$ov_bg, "phase")) return(NULL)
      sw <- function(col, lbl) shiny::span(class = "dc-sw",
        shiny::span(class = "dc-sw-c", style = paste0("background:", col)), lbl)
      shiny::tagList(sw(DC_PHASE_FILL[["Erwärmung"]], "Erwärmung"),
                     sw(DC_PHASE_FILL[["Belastung"]], "Belastung"),
                     sw(DC_PHASE_FILL[["Erholung"]], "Erholung"))
    })

    # ---- Gesamtverlauf: absolute Werte, gestapelt --------
    output$overview_abs <- plotly::renderPlotly({
      ts <- ts_with_stage_r()
      has <- function(v) v %in% names(ts) && any(is.finite(ts[[v]]))
      line <- function(col, ...) list(color = col, width = 1.4, ...)
      subs <- list()
      add <- function(pl, ttl) subs[[length(subs) + 1]] <<-
        plotly::layout(pl, yaxis = c(DC_AXIS, list(title = ttl)))

      if (has("HR")) add(plotly::plot_ly(ts, x = ~time_min, y = ~HR, type = "scatter",
        mode = "lines", line = line(.dc_colors$hr), name = "HF"), "HF [1/min]")
      if (has("VO2abs")) {
        pl <- plotly::plot_ly(ts, x = ~time_min, y = ~VO2abs, type = "scatter",
          mode = "lines", line = line(.dc_colors$vo2), name = "V'O₂")
        if (has("VCO2")) pl <- plotly::add_lines(pl, y = ~VCO2,
          line = line(.dc_colors$vco2), name = "V'CO₂")
        add(pl, "L/min")
      }
      if (has("P")) add(plotly::plot_ly(ts, x = ~time_min, y = ~P, type = "scatter",
        mode = "lines", line = line(.dc_colors$power, shape = "hv"), name = "Leistung"),
        "Leistung [W]")
      else if (has("Speed")) add(plotly::plot_ly(ts, x = ~time_min, y = ~Speed,
        type = "scatter", mode = "lines", line = line(.dc_colors$power, shape = "hv"),
        name = "Geschwindigkeit"), "km/h")
      shiny::validate(shiny::need(length(subs) > 0, "Keine HF-, V'O₂- oder Leistungsdaten vorhanden."))

      vt <- dc_vt_shapes(params_reactive())
      out <- plotly::subplot(subs, nrows = length(subs), shareX = TRUE, titleY = TRUE,
                      margin = 0.03) |>
        plotly::layout(
          xaxis = c(DC_AXIS, list(title = "Zeit [min]")),
          hovermode = "x unified", plot_bgcolor = "#fff", paper_bgcolor = "#fff",
          font = list(family = "-apple-system, BlinkMacSystemFont, Segoe UI, sans-serif"),
          legend = list(orientation = "h", x = 0, y = 1.06, font = list(size = 12)),
          margin = list(t = 40, r = 16)) |>
        plotly::config(displaylogo = FALSE) |>
        plotly::plotly_build()
      # layout(shapes=) nach subplot() wird von plotly verworfen -> direkt setzen
      out$x$layout$shapes <- c(dc_bg_shapes(ts, input$ov_bg), vt$shapes)
      out$x$layout$annotations <- c(out$x$layout$annotations, vt$annot)
      out
    })

    # ---- Gesamtverlauf: relativ zum beobachteten Maximum --
    output$overview_norm <- plotly::renderPlotly({
      ts <- ts_with_stage_r()
      norm <- prepare_normalized_overview(ts)
      shiny::validate(shiny::need(
        any(is.finite(norm$HR_pct) | is.finite(norm$VO2_pct) |
            is.finite(norm$Power_pct) | is.finite(norm$Speed_pct)),
        "Keine normalisierten Werte verfügbar."))
      t <- norm$time_s / 60
      vt <- dc_vt_shapes(params_reactive())
      pl <- plotly::plot_ly() |>
        plotly::layout(
          xaxis = c(DC_AXIS, list(title = "Zeit [min]")),
          yaxis = c(DC_AXIS, list(title = "% vom Maximum", range = c(0, 110))),
          hovermode = "x unified", plot_bgcolor = "#fff", paper_bgcolor = "#fff",
          legend = list(orientation = "h", x = 0, y = 1.08),
          margin = list(t = 40, r = 16),
          shapes = c(dc_bg_shapes(ts, input$ov_bg), vt$shapes), annotations = vt$annot) |>
        plotly::config(displaylogo = FALSE)
      lw <- function(col, ...) list(color = col, width = 1.6, ...)
      if (any(is.finite(norm$HR_pct))) pl <- plotly::add_lines(pl, x = t, y = norm$HR_pct,
        name = "HF (% HFmax)", line = lw(.dc_colors$hr))
      if (any(is.finite(norm$VO2_pct))) pl <- plotly::add_lines(pl, x = t, y = norm$VO2_pct,
        name = "V'O₂ (% V'O₂peak)", line = lw(.dc_colors$vo2))
      if (any(is.finite(norm$Power_pct))) pl <- plotly::add_lines(pl, x = t, y = norm$Power_pct,
        name = "Leistung (% Pmax)", line = lw(.dc_colors$power, shape = "hv"))
      else if (any(is.finite(norm$Speed_pct))) pl <- plotly::add_lines(pl, x = t,
        y = norm$Speed_pct, name = "Geschwindigkeit (% Vmax)", line = lw(.dc_colors$power, shape = "hv"))
      pl
    })

    # ---- Rueckgabe ---------------------------------------
    return(list(
      decisions     = shiny::reactive(state$decisions),
      candidates    = candidates_r,
      stage_summary = stage_summary_r
    ))
  })
}
