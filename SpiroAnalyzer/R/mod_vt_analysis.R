# ============================================================
#  mod_vt_analysis.R -- VT1/VT2 Analyse
#  Draggable VT-Linien in Plotly, Excel-VTs als Default
# ============================================================

mod_vt_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .vt-auto   { color:#2563eb; font-weight:600; }
      .vt-manual { color:#D97706; font-weight:600; }
      .vt-final  { color:#059669; font-weight:600; }
      .vt-hint   { font-size:0.78rem; color:#888; margin-top:2px; }
    ")),
    shiny::fluidRow(
      # == Steuerung links ====================================
      shiny::column(3,
        shiny::div(class = "sa-card",
          shiny::tags$h6("VT-Steuerung",
            style = "font-weight:700; color:#1f3d6b; margin-bottom:8px;"),
          shiny::p("Verschiebe die VT-Linien direkt in den Plots.",
            class = "vt-hint"),
          shiny::hr(style = "margin:6px 0;"),

          # Glaettung
          shiny::numericInput(ns("smooth"), "Glaettung (Punkte)",
            value = 15, min = 3, max = 60, step = 1, width = "100%"),

          # Phasen
          shiny::tags$h6("Phasen", style = "font-weight:600; margin-top:8px;"),
          shiny::checkboxInput(ns("ph_warmup"), "Erwaermung", value = TRUE),
          shiny::checkboxInput(ns("ph_recovery"), "Erholung", value = FALSE),
          shiny::hr(style = "margin:6px 0;"),

          # VT1
          shiny::tags$h6("VT1",
            style = "font-weight:700; color:#006400;"),
          shiny::uiOutput(ns("vt1_status")),
          shiny::numericInput(ns("vt1_time"), "Zeit (min)",
            value = NA, min = 0, max = 60, step = 0.1, width = "100%"),
          shiny::div(style = "display:flex; gap:4px; margin-bottom:6px;",
            shiny::actionButton(ns("vt1_reset"), "Reset",
              class = "btn-sm btn-outline-secondary", style = "flex:1;"),
            shiny::actionButton(ns("vt1_confirm"), "Bestaetigen",
              class = "btn-sm btn-success", style = "flex:1;")),
          shiny::hr(style = "margin:4px 0;"),

          # VT2
          shiny::tags$h6("VT2",
            style = "font-weight:700; color:#6B8E23;"),
          shiny::uiOutput(ns("vt2_status")),
          shiny::numericInput(ns("vt2_time"), "Zeit (min)",
            value = NA, min = 0, max = 60, step = 0.1, width = "100%"),
          shiny::div(style = "display:flex; gap:4px; margin-bottom:6px;",
            shiny::actionButton(ns("vt2_reset"), "Reset",
              class = "btn-sm btn-outline-secondary", style = "flex:1;"),
            shiny::actionButton(ns("vt2_confirm"), "Bestaetigen",
              class = "btn-sm btn-success", style = "flex:1;")),
          shiny::hr(style = "margin:6px 0;"),

          shiny::actionButton(ns("apply_9p"),
            "In 9-Felder uebernehmen",
            icon = shiny::icon("share"),
            class = "btn-outline-primary w-100")
        ),
        shiny::div(class = "sa-card",
          shiny::tags$h6("VT-Parameter"),
          DT::DTOutput(ns("vt_tbl"), height = "auto")
        )
      ),

      # == Plots rechts =======================================
      shiny::column(9,
        bslib::navset_card_tab(
          bslib::nav_panel("VT1",
            shiny::fluidRow(
              shiny::column(6, plotly::plotlyOutput(ns("p_exco2"), height="270px")),
              shiny::column(6, plotly::plotlyOutput(ns("p_vslope1"), height="270px"))
            ),
            shiny::fluidRow(
              shiny::column(6, plotly::plotlyOutput(ns("p_eq1"), height="270px")),
              shiny::column(6, plotly::plotlyOutput(ns("p_pet1"), height="270px"))
            )
          ),
          bslib::nav_panel("VT2",
            shiny::fluidRow(
              shiny::column(6, plotly::plotlyOutput(ns("p_exve"), height="270px")),
              shiny::column(6, plotly::plotlyOutput(ns("p_vslope2"), height="270px"))
            ),
            shiny::fluidRow(
              shiny::column(6, plotly::plotlyOutput(ns("p_eq2"), height="270px")),
              shiny::column(6, plotly::plotlyOutput(ns("p_pet2"), height="270px"))
            )
          ),
          bslib::nav_panel("Uebersicht",
            shiny::fluidRow(
              shiny::column(6, plotly::plotlyOutput(ns("p_ov_vo2"), height="290px")),
              shiny::column(6, plotly::plotlyOutput(ns("p_ov_rer"), height="290px"))
            )
          )
        )
      )
    )
  )
}


# ============================================================
mod_vt_server <- function(id, params_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # == State =================================================
    vt <- shiny::reactiveValues(
      vt1_time = NA_real_, vt2_time = NA_real_,
      vt1_final = NA_real_, vt2_final = NA_real_,
      vt1_method = "auto", vt2_method = "auto",
      vt1_confirmed = FALSE, vt2_confirmed = FALSE
    )

    # == Gefilterte Daten ======================================
    ts_r <- shiny::reactive({
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))
      ts <- p$ts
      keep <- c("Belastung", "Exercise")
      if (isTRUE(input$ph_warmup))
        keep <- c(keep, "Warmup","Warm Up","Warm-Up","Erwaermung","Erwärmung")
      if (isTRUE(input$ph_recovery))
        keep <- c(keep, "Cooldown","Cool Down","Erholung","Recovery")
      ts |> dplyr::filter(Phase %in% keep | is.na(Phase)) |>
        dplyr::arrange(time_min)
    })

    # == Init: VTs aus Excel ===================================
    shiny::observeEvent(params_reactive(), {
      p <- params_reactive(); shiny::req(p, !is.null(p$ts))

      # Excel-VTs als Default
      t1 <- if (is.finite(p$vt1_time %||% NA)) p$vt1_time else NA_real_
      t2 <- if (is.finite(p$vt2_time %||% NA)) p$vt2_time else NA_real_

      # Falls keine Excel-VTs: Auto-Berechnung
      if (is.na(t1)) {
        idx <- tryCatch(auto_vt1(p$ts), error = function(e) NA_integer_)
        t1 <- vt_idx_to_time(p$ts, idx)
      }
      if (is.na(t2)) {
        idx <- tryCatch(auto_vt2(p$ts), error = function(e) NA_integer_)
        t2 <- vt_idx_to_time(p$ts, idx)
      }

      vt$vt1_time <- t1; vt$vt2_time <- t2
      vt$vt1_final <- NA_real_; vt$vt2_final <- NA_real_
      vt$vt1_method <- if (is.finite(p$vt1_time %||% NA)) "Excel" else "auto"
      vt$vt2_method <- if (is.finite(p$vt2_time %||% NA)) "Excel" else "auto"
      vt$vt1_confirmed <- FALSE; vt$vt2_confirmed <- FALSE

      shiny::updateNumericInput(session, "vt1_time",
        value = if (is.finite(t1)) round(t1, 2) else NA)
      shiny::updateNumericInput(session, "vt2_time",
        value = if (is.finite(t2)) round(t2, 2) else NA)
    })

    # == NumericInput -> update VT =============================
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

    # == Plotly relayout -> Linie verschoben ===================
    # Eigene Wrapper-Funktion: event_data ohne Warning wenn Plot
    # noch nicht gerendert (withCallingHandlers fängt die Warnung
    # ab bevor sie in der Shiny-Konsole erscheint)
    safe_event_data <- function(event, source) {
      tryCatch(
        withCallingHandlers(
          plotly::event_data(event, source = source),
          warning = function(w) {
            if (grepl("is not registered", conditionMessage(w)))
              invokeRestart("muffleWarning")
          }
        ),
        error = function(e) NULL
      )
    }

    # Einzelne Observer pro Plot-Gruppe (VT1-Plots, VT2-Plots, Uebersicht)
    # So werden nur die Quellen abgefragt die auch existieren könnten
    shiny::observe({
      srcs <- c("exco2", "eq1", "pet1", "exve", "eq2", "pet2",
                "ov_vo2", "ov_rer")
      for (src in srcs) {
        ev <- safe_event_data("plotly_relayout", ns(src))
        if (!is.null(ev)) {
          nms <- names(ev)
          for (nm in nms) {
            if (grepl("shapes\\[0\\]\\.x0", nm) && is.finite(ev[[nm]])) {
              vt$vt1_time <- ev[[nm]]
              vt$vt1_method <- "manuell"; vt$vt1_confirmed <- FALSE
              shiny::updateNumericInput(session, "vt1_time",
                value = round(ev[[nm]], 2))
            }
            if (grepl("shapes\\[1\\]\\.x0", nm) && is.finite(ev[[nm]])) {
              vt$vt2_time <- ev[[nm]]
              vt$vt2_method <- "manuell"; vt$vt2_confirmed <- FALSE
              shiny::updateNumericInput(session, "vt2_time",
                value = round(ev[[nm]], 2))
            }
          }
        }
      }
    })

    # == Reset / Confirm =======================================
    shiny::observeEvent(input$vt1_reset, {
      p <- tryCatch(params_reactive(), error = function(e) NULL)
      if (is.null(p)) return()
      t1 <- if (is.finite(p$vt1_time %||% NA)) p$vt1_time
            else vt_idx_to_time(p$ts, tryCatch(auto_vt1(p$ts),
                   error = function(e) NA_integer_))
      vt$vt1_time <- t1; vt$vt1_method <- "auto"; vt$vt1_confirmed <- FALSE
      shiny::updateNumericInput(session, "vt1_time",
        value = if (is.finite(t1)) round(t1, 2) else NA)
    })
    shiny::observeEvent(input$vt2_reset, {
      p <- tryCatch(params_reactive(), error = function(e) NULL)
      if (is.null(p)) return()
      t2 <- if (is.finite(p$vt2_time %||% NA)) p$vt2_time
            else vt_idx_to_time(p$ts, tryCatch(auto_vt2(p$ts),
                   error = function(e) NA_integer_))
      vt$vt2_time <- t2; vt$vt2_method <- "auto"; vt$vt2_confirmed <- FALSE
      shiny::updateNumericInput(session, "vt2_time",
        value = if (is.finite(t2)) round(t2, 2) else NA)
    })
    shiny::observeEvent(input$vt1_confirm, {
      vt$vt1_final <- vt$vt1_time; vt$vt1_confirmed <- TRUE
      shiny::showNotification("VT1 bestaetigt!", type = "message", duration = 3)
    })
    shiny::observeEvent(input$vt2_confirm, {
      vt$vt2_final <- vt$vt2_time; vt$vt2_confirmed <- TRUE
      shiny::showNotification("VT2 bestaetigt!", type = "message", duration = 3)
    })

    # == Status UI =============================================
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

    # == Plot Helpers ==========================================
    sn <- shiny::reactive(input$smooth %||% 15)

    # Build draggable VT shapes for time-based plots
    vt_shapes <- shiny::reactive({
      shapes <- list()
      if (is.finite(vt$vt1_time)) {
        shapes[[1]] <- list(
          type = "line", x0 = vt$vt1_time, x1 = vt$vt1_time,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#006400", width = 2.5))
      } else {
        shapes[[1]] <- list(
          type = "line", x0 = 0, x1 = 0,
          y0 = 0, y1 = 0, yref = "paper", visible = FALSE,
          line = list(color = "#006400", width = 0))
      }
      if (is.finite(vt$vt2_time)) {
        shapes[[2]] <- list(
          type = "line", x0 = vt$vt2_time, x1 = vt$vt2_time,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#6B8E23", width = 2.5))
      } else {
        shapes[[2]] <- list(
          type = "line", x0 = 0, x1 = 0,
          y0 = 0, y1 = 0, yref = "paper", visible = FALSE,
          line = list(color = "#6B8E23", width = 0))
      }
      shapes
    })

    # Config: shapes draggable
    cfg <- function(p, src) {
      p |>
        plotly::event_register("plotly_relayout") |>
        plotly::config(editable = TRUE,
                       edits = list(shapePosition = TRUE))
    }

    # == PLOTS =================================================

    # -- ExCO2 (VT1) ------------------------------------------
    output$p_exco2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$ExCO2 <- calc_exco2(d$VO2abs, d$VCO2)
      d$ExCO2_s <- safe_roll(d$ExCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("exco2")) |>
        plotly::add_markers(y = ~ExCO2, name = "ExCO2",
          marker = list(size = 2, color = "#8B0000", opacity = 0.3)) |>
        plotly::add_lines(y = ~ExCO2_s, name = "Gegl.",
          line = list(color = "#CD0000", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "ExCO2"), showlegend = FALSE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "exco2")
    })

    # -- V-Slope VT1: VO2 vs VCO2 + 45-Linie ------------------
    output$p_vslope1 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$VO2_s <- safe_roll(d$VO2abs, sn())
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      ax <- max(c(d$VO2_s, d$VCO2_s), na.rm = TRUE) * 1.1
      if (!is.finite(ax)) ax <- 3

      # VT1 als vertikale Linie bei VO2-Wert
      shapes <- list(
        list(type = "line", x0 = 0, x1 = ax, y0 = 0, y1 = ax,
             line = list(color = "black", width = 1, dash = "dot")))
      if (is.finite(vt$vt1_time)) {
        idx <- which.min(abs(d$time_min - vt$vt1_time))
        vt1_vo2 <- if (length(idx)==1) d$VO2_s[idx] else NA_real_
        if (is.finite(vt1_vo2))
          shapes[[2]] <- list(type="line", x0=vt1_vo2, x1=vt1_vo2,
            y0=0, y1=1, yref="paper",
            line=list(color="#006400", width=2.5))
      }

      p <- plotly::plot_ly(d, x = ~VO2_s, y = ~VCO2_s, type = "scatter",
        mode = "markers",
        marker = list(size = 3, color = "#CD0000", opacity = 0.5)) |>
        plotly::layout(
          xaxis = list(title = "V'O2 [L/min]", range = c(0, ax)),
          yaxis = list(title = "V'CO2 [L/min]", range = c(0, ax)),
          showlegend = FALSE, shapes = shapes,
          margin = list(t = 30))
      p |> plotly::config(editable = FALSE)
    })

    # -- Aeq VT1 -----------------------------------------------
    output$p_eq1 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$EQ_O2 <- safe_roll(d$VE_VO2, sn())
      d$EQ_CO2 <- safe_roll(d$VE_VCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("eq1")) |>
        plotly::add_lines(y = ~EQ_O2, name = "VE/VO2",
          line = list(color = "#CD0000", width = 2)) |>
        plotly::add_lines(y = ~EQ_CO2, name = "VE/VCO2",
          line = list(color = "#0000CD", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = ""), showlegend = TRUE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "eq1")
    })

    # -- Pet VT1 -----------------------------------------------
    output$p_pet1 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$PO2 <- safe_roll(d$PetO2, sn())
      d$PCO2 <- safe_roll(d$PetCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("pet1")) |>
        plotly::add_lines(y = ~PO2, name = "PetO2",
          line = list(color = "#00008B", width = 2)) |>
        plotly::add_lines(y = ~PCO2, name = "PetCO2",
          line = list(color = "#CD00CD", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "mmHg"), showlegend = TRUE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "pet1")
    })

    # -- ExVE (VT2) --------------------------------------------
    output$p_exve <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$ExVE <- calc_exve(d$VE, d$VCO2)
      d$ExVE_s <- safe_roll(d$ExVE, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("exve")) |>
        plotly::add_markers(y = ~ExVE, name = "ExVE",
          marker = list(size = 2, color = "#8B4513", opacity = 0.3)) |>
        plotly::add_lines(y = ~ExVE_s, name = "Gegl.",
          line = list(color = "#D2691E", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "ExVE"), showlegend = FALSE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "exve")
    })

    # -- V-Slope VT2: VCO2 vs VE --------------------------------
    output$p_vslope2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      d$VE_s <- safe_roll(d$VE, sn())

      shapes <- list()
      if (is.finite(vt$vt2_time)) {
        idx <- which.min(abs(d$time_min - vt$vt2_time))
        vt2_vco2 <- if (length(idx)==1) d$VCO2_s[idx] else NA_real_
        if (is.finite(vt2_vco2))
          shapes[[1]] <- list(type="line", x0=vt2_vco2, x1=vt2_vco2,
            y0=0, y1=1, yref="paper",
            line=list(color="#6B8E23", width=2.5))
      }

      p <- plotly::plot_ly(d, x = ~VCO2_s, y = ~VE_s, type = "scatter",
        mode = "markers",
        marker = list(size = 3, color = "#8B0000", opacity = 0.5)) |>
        plotly::layout(
          xaxis = list(title = "V'CO2 [L/min]"),
          yaxis = list(title = "V'E [L/min]"),
          showlegend = FALSE, shapes = shapes,
          margin = list(t = 30))
      p |> plotly::config(editable = FALSE)
    })

    # -- Aeq VT2 -----------------------------------------------
    output$p_eq2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$EQ_O2 <- safe_roll(d$VE_VO2, sn())
      d$EQ_CO2 <- safe_roll(d$VE_VCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("eq2")) |>
        plotly::add_lines(y = ~EQ_O2, name = "VE/VO2",
          line = list(color = "#CD0000", width = 2)) |>
        plotly::add_lines(y = ~EQ_CO2, name = "VE/VCO2",
          line = list(color = "#0000CD", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = ""), showlegend = TRUE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "eq2")
    })

    # -- Pet VT2 -----------------------------------------------
    output$p_pet2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$PO2 <- safe_roll(d$PetO2, sn())
      d$PCO2 <- safe_roll(d$PetCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("pet2")) |>
        plotly::add_lines(y = ~PO2, name = "PetO2",
          line = list(color = "#00008B", width = 2)) |>
        plotly::add_lines(y = ~PCO2, name = "PetCO2",
          line = list(color = "#CD00CD", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "mmHg"), showlegend = TRUE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "pet2")
    })

    # -- Uebersicht VO2/VCO2 -----------------------------------
    output$p_ov_vo2 <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$VO2_s <- safe_roll(d$VO2abs, sn())
      d$VCO2_s <- safe_roll(d$VCO2, sn())
      p <- plotly::plot_ly(d, x = ~time_min, source = ns("ov_vo2")) |>
        plotly::add_lines(y = ~VO2_s, name = "V'O2",
          line = list(color = "#CD0000", width = 2)) |>
        plotly::add_lines(y = ~VCO2_s, name = "V'CO2",
          line = list(color = "#0000CD", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "L/min"), showlegend = TRUE,
          shapes = vt_shapes(), margin = list(t = 30))
      cfg(p, "ov_vo2")
    })

    output$p_ov_rer <- plotly::renderPlotly({
      d <- ts_r(); shiny::req(d, nrow(d) > 0)
      d$RER_s <- safe_roll(d$RER, sn())
      rer_shapes <- vt_shapes()
      rer_shapes[[length(rer_shapes)+1]] <- list(
        type="line", x0=0, x1=1, xref="paper",
        y0=1.0, y1=1.0, line=list(color="grey", dash="dot", width=1))
      rer_shapes[[length(rer_shapes)+1]] <- list(
        type="line", x0=0, x1=1, xref="paper",
        y0=1.1, y1=1.1, line=list(color="#8B0000", dash="dash", width=1))

      p <- plotly::plot_ly(d, x = ~time_min, source = ns("ov_rer")) |>
        plotly::add_lines(y = ~RER_s, name = "RER",
          line = list(color = "black", width = 2)) |>
        plotly::layout(xaxis = list(title = "Zeit [min]"),
          yaxis = list(title = "RER"),
          showlegend = FALSE, shapes = rer_shapes,
          margin = list(t = 30))
      cfg(p, "ov_rer")
    })

    # == VT-Tabelle ============================================
    vt_table_r <- shiny::reactive({
      p <- tryCatch(params_reactive(), error = function(e) NULL)
      ts <- tryCatch(ts_r(), error = function(e) NULL)
      if (is.null(ts) || is.null(p)) return(NULL)
      # Index aus time finden
      idx1 <- if (is.finite(vt$vt1_time))
        which.min(abs(ts$time_min - vt$vt1_time)) else NA_integer_
      idx2 <- if (is.finite(vt$vt2_time))
        which.min(abs(ts$time_min - vt$vt2_time)) else NA_integer_
      build_vt_table(ts, idx1, idx2, params = p,
        vt1_method = vt$vt1_method, vt2_method = vt$vt2_method,
        vt1_confirmed = vt$vt1_confirmed, vt2_confirmed = vt$vt2_confirmed)
    })

    output$vt_tbl <- DT::renderDT({
      df <- vt_table_r(); shiny::req(df)
      DT::datatable(df, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE), class = "table-sm")
    })

    # == Return ================================================
    return(list(
      vt_state = vt,
      vt_table = vt_table_r,
      apply_trigger = shiny::reactive(input$apply_9p)
    ))
  })
}