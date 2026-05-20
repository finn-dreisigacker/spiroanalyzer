# ============================================================
#  mod_single.R -- Einzelansicht (v5-fix)
# ============================================================

mod_single_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # -- Vollbild CSS/JS ---------------------------------------
    shiny::tags$style(shiny::HTML(paste0("
      .np-fs-btn {
        position:absolute; top:8px; right:8px; z-index:20;
        background:#1f3d6b; color:#fff; border:none;
        border-radius:6px; padding:4px 10px; font-size:0.78rem;
        cursor:pointer; opacity:0.85;
      }
      .np-fs-btn:hover { opacity:1; }
      #", ns("np_wrapper"), ".np-fullscreen {
        position:fixed; top:0; left:0; width:100vw; height:100vh;
        z-index:9999; background:#fff; overflow-y:auto;
        padding:12px 20px;
      }
      body.np-fs-active #", ns("sidebar_col"), " {
        display:none !important;
      }
      body.np-fs-active #", ns("main_col"), " {
        width:100% !important; max-width:100% !important;
        flex: 0 0 100% !important;
      }
    "))),
    shiny::tags$script(shiny::HTML(paste0("
      Shiny.addCustomMessageHandler('npFS_", id, "', function(msg) {
        var el = document.getElementById('", ns("np_wrapper"), "');
        el.classList.toggle('np-fullscreen');
        document.body.classList.toggle('np-fs-active');
        var btn = el.querySelector('.np-fs-btn');
        if (el.classList.contains('np-fullscreen')) {
          btn.innerHTML = '<i class=\"fa fa-compress\"></i> Vollbild beenden';
        } else {
          btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild';
        }
      });
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          var el = document.getElementById('", ns("np_wrapper"), "');
          if (el && el.classList.contains('np-fullscreen')) {
            el.classList.remove('np-fullscreen');
            document.body.classList.remove('np-fs-active');
            var btn = el.querySelector('.np-fs-btn');
            if (btn) btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild';
          }
        }
      });
    "))),

    shiny::fluidRow(
      # ========== Linke Spalte ================================
      shiny::column(4, id = ns("sidebar_col"),
        bslib::card(
          bslib::card_header(
            shiny::tags$span(shiny::icon("upload"), " Datei hochladen")),
          shiny::fileInput(ns("file"), label = NULL,
            accept = c(".xlsx", ".xml"),
            buttonLabel = "Datei waehlen ...",
            placeholder = "XML oder Excel"),
          shiny::div(
            style = "display:flex; align-items:center; gap:10px; margin-bottom:6px;",
            shiny::div(style = "flex:0 0 auto; font-size:0.83rem;
              font-weight:600; color:#444; white-space:nowrap;",
              "Glaettung (Punkte):"),
            shiny::div(style = "flex:1;",
              shiny::numericInput(ns("smooth"), label = NULL,
                value = 20, min = 5, max = 60, step = 1, width = "100%")),
            shiny::div(style = "flex:0 0 auto; font-size:0.75rem; color:#888;",
              "zentriert")
          ),
          shiny::hr(style = "margin:6px 0 10px;"),
          shiny::uiOutput(ns("meta_box"))
        )
      ),

      # ========== Rechte Spalte ================================
      shiny::column(8, id = ns("main_col"),
        bslib::navset_card_tab(

          # --- Zeitreihe ----------------------------------------
          bslib::nav_panel("Zeitreihe",
            shinycssloaders::withSpinner(
              shiny::plotOutput(ns("ts_plot"), height = "420px"), type = 6)
          ),

          # --- 9-Felder (mit Vollbild) --------------------------
          bslib::nav_panel("9-Felder",
            shiny::div(id = ns("np_wrapper"), style = "position:relative;",
              shiny::tags$button(class = "np-fs-btn",
                onclick = paste0("Shiny.setInputValue('",
                  ns("np_fullscreen"), "', Math.random())"),
                shiny::icon("expand"), " Vollbild"),
              shiny::fluidRow(
                shiny::column(3,
                  shiny::div(class = "sa-card", style = "font-size:0.85rem;",
                    shiny::tags$h6("Anordnung",
                      style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                    shiny::radioButtons(ns("np_arr"), label = NULL, inline = TRUE,
                      choices = c("Alt" = "alt", "Neu" = "neu"), selected = "alt"),
                    shiny::hr(style = "margin:6px 0;"),
                    shiny::tags$h6("Phasen",
                      style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                    shiny::checkboxInput(ns("np_warmup"), "Erwaermung", value = TRUE),
                    shiny::checkboxInput(ns("np_recovery"), "Erholung", value = TRUE),
                    shiny::hr(style = "margin:6px 0;"),
                    shiny::tags$h6("Felder",
                      style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                    shiny::checkboxGroupInput(ns("np_panels"), label = NULL,
                      choiceNames = lapply(paste0(1:9, c(
                        " V'E/AF", " HF/O2P", " VO2/VCO2", " V'E-V'CO2",
                        " V-Slope", " Aeq.", " VT-V'E", " RER", " Pet")),
                        function(x) shiny::HTML(paste0("<small>", x, "</small>"))),
                      choiceValues = as.character(1:9),
                      selected = as.character(1:9)),
                    shiny::div(style = "display:flex; gap:4px; margin-bottom:6px;",
                      shiny::actionButton(ns("np_all"), "Alle",
                        class = "btn-sm btn-outline-secondary", style = "flex:1;"),
                      shiny::actionButton(ns("np_none"), "Keine",
                        class = "btn-sm btn-outline-secondary", style = "flex:1;")),
                    shiny::hr(style = "margin:6px 0;"),
                    shiny::tags$h6("Spalten",
                      style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                    shiny::radioButtons(ns("np_ncol"), label = NULL, inline = TRUE,
                      choices = c("1"="1","2"="2","3"="3"), selected = "3"),
                    shiny::hr(style = "margin:6px 0;"),
                    shiny::tags$h6("Export",
                      style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                    shiny::div(style = "display:flex; gap:6px; margin-bottom:4px;",
                      shiny::div(style = "flex:1;",
                        shiny::numericInput(ns("np_w"), "B(cm)", value = 30,
                          min = 10, max = 60, step = 1, width = "100%")),
                      shiny::div(style = "flex:1;",
                        shiny::numericInput(ns("np_h"), "H(cm)", value = 25,
                          min = 8, max = 50, step = 1, width = "100%"))),
                    shiny::numericInput(ns("np_dpi"), "DPI", value = 600,
                      min = 150, max = 1200, step = 50, width = "100%"),
                    shiny::downloadButton(ns("dl_np"), "PNG",
                      class = "btn-outline-primary w-100 mt-1")
                  )
                ),
                shiny::column(9,
                  shinycssloaders::withSpinner(
                    shiny::uiOutput(ns("np_plot_ui")), type = 6))
              )
            )
          ),

          # --- VT-Analyse (eingebettet) -------------------------
          bslib::nav_panel("VT-Analyse",
            mod_vt_ui(ns("vt"))
          ),

          # --- Parameter ----------------------------------------
          bslib::nav_panel("Parameter",
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("param_table")), type = 6)
          ),

          # --- Ausbelastung -------------------------------------
          bslib::nav_panel("Ausbelastung",
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("exhaust_table")), type = 6)
          ),

          # --- Export -------------------------------------------
          bslib::nav_panel("Export",
            shiny::br(),
            shiny::downloadButton(ns("dl_plot"), "Zeitreihe PNG",
              class = "btn-outline-primary me-2"),
            shiny::downloadButton(ns("dl_csv"), "Parameter CSV",
              class = "btn-outline-secondary me-2"),
            shiny::downloadButton(ns("dl_xlsx"), "Alles als Excel",
              class = "btn-primary me-2")
          )
        )
      )
    )
  )
}


# ============================================================
#  SERVER
# ============================================================
mod_single_server <- function(id, vt_override = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Vollbild -----------------------------------------------
    shiny::observeEvent(input$np_fullscreen, {
      session$sendCustomMessage(paste0("npFS_", id), list())
    })

    # -- Daten laden --------------------------------------------
    params_r <- shiny::reactive({
      shiny::req(input$file)
      fp    <- input$file$datapath
      fname <- input$file$name
      shiny::withProgress(message = "Lade Datei ...", value = 0.3, {
        spiro <- tryCatch(load_spiro_file(fp, fname),
          error = function(e) {
            shiny::showNotification(paste("Fehler:", e$message),
              type = "error"); NULL })
        shiny::setProgress(0.7)
        if (is.null(spiro)) return(NULL)
        p <- tryCatch(extract_params(spiro, fname),
          error = function(e) {
            shiny::showNotification(paste("Fehler:", e$message),
              type = "error"); NULL })
        shiny::setProgress(1); p
      })
    })

    label_r <- shiny::reactive({
      p <- params_r(); shiny::req(p)
      paste0(p$ID %||% "Messung", "_", p$Timepoint %||% "")
    })

    output$meta_box <- shiny::renderUI({
      p <- params_r(); shiny::req(p); make_meta_card(p)
    })

    # -- Zeitreihe-Plot -----------------------------------------
    plot_r <- shiny::reactive({
      p <- params_r(); shiny::req(p, !is.null(p$ts), nrow(p$ts) > 0)
      single_ts_plot(ts = p$ts, smooth_n = input$smooth %||% 20,
                     label = label_r())
    })
    output$ts_plot <- shiny::renderPlot({ plot_r() }, res = 120)

    # -- VT-Modul (eingebettet) ---------------------------------
    vt_res <- mod_vt_server(ns("vt"), params_reactive = params_r)

    # reactiveVal: Zaehler um 9-Felder-Plot manuell zu invalidieren
    vt_apply_counter <- shiny::reactiveVal(0L)

    # "In 9-Felder uebernehmen"-Button
    shiny::observeEvent(
      if (!is.null(vt_res)) vt_res$apply_trigger() else NULL,
    {
      vt_apply_counter(shiny::isolate(vt_apply_counter()) + 1L)
      shiny::showNotification("VT-Werte in 9-Felder uebernommen!",
        type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # -- 9-Felder -----------------------------------------------
    shiny::observeEvent(input$np_all, {
      shiny::updateCheckboxGroupInput(session, "np_panels",
        selected = as.character(1:9))
    })
    shiny::observeEvent(input$np_none, {
      shiny::updateCheckboxGroupInput(session, "np_panels",
        selected = character(0))
    })

    np_plot_r <- shiny::reactive({
      p <- params_r(); shiny::req(p, !is.null(p$ts), nrow(p$ts) > 0)
      sel <- input$np_panels; shiny::req(length(sel) > 0)

      # Abhaengigkeit auf apply-Button (erzwingt Re-Render)
      vt_apply_counter()

      # VT: Basis = Excel-Werte, dann VT-Modul-State ueberschreiben
      vt1_t <- p$vt1_time; vt2_t <- p$vt2_time; vt_d <- p$vt
      if (!is.null(vt_res) && !is.null(vt_res$vt_state)) {
        st <- vt_res$vt_state
        # Immer den aktuellen VT-Zeit-Wert aus dem Modul verwenden
        # (egal ob manuell, auto, oder bestaetigt)
        if (is.finite(st$vt1_time)) vt1_t <- st$vt1_time
        if (is.finite(st$vt2_time)) vt2_t <- st$vt2_time
        # Bei Bestaetigung: final-Wert verwenden (Abhaengigkeit erzeugen)
        if (isTRUE(st$vt1_confirmed) && is.finite(st$vt1_final))
          vt1_t <- st$vt1_final
        if (isTRUE(st$vt2_confirmed) && is.finite(st$vt2_final))
          vt2_t <- st$vt2_final
      }

      nine_panel_assemble(
        ts = p$ts, smooth_n = input$smooth %||% 20, label = label_r(),
        vt = vt_d, vt1_time = vt1_t, vt2_time = vt2_t,
        panels = sel, ncol = as.integer(input$np_ncol %||% 3),
        arrangement = input$np_arr %||% "alt",
        show_warmup = isTRUE(input$np_warmup),
        show_recovery = isTRUE(input$np_recovery)
      )
    })

    output$np_plot_ui <- shiny::renderUI({
      sel <- input$np_panels %||% as.character(1:9)
      nc  <- as.integer(input$np_ncol %||% 3)
      n_rows <- ceiling(max(length(sel), 1) / nc)
      shiny::plotOutput(ns("np_plot"), height = paste0(n_rows * 260 + 30, "px"))
    })

    output$np_plot <- shiny::renderPlot({
      p <- np_plot_r()
      if (inherits(p, "gtable") || inherits(p, "grob")) grid::grid.draw(p)
      else p
    }, res = 110)

    output$dl_np <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_9Felder.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = np_plot_r(),
          width = input$np_w %||% 30, height = input$np_h %||% 25,
          units = "cm", dpi = input$np_dpi %||% 600)
    )

    # -- Parametertabelle ---------------------------------------
    output$param_table <- DT::renderDT({
      p <- params_r(); shiny::req(p)
      df <- tibble::tibble(
        Parameter = c("ID","Zeitpunkt","Datum","Gewicht (kg)",
          "Groesse (cm)","Alter (Jahre)","Temperatur","Luftdruck",
          "Luftfeuchtigkeit","Dauer","PPO (W)",
          "PPO interpoliert","VO2peak abs (L/min)",
          "VO2peak rel (ml/min/kg)","RERmax","EQO2max","HRmax"),
        Wert = c(p$ID %||% "-", p$Timepoint %||% "-",
          if (!is.na(p$Date)) format(p$Date,"%d.%m.%Y %H:%M") else "-",
          fmt(p$Weight_kg),fmt(p$Height_cm),fmt(p$Age_years),
          fmt(p$Temperature),fmt(p$AirPressure),fmt(p$Humidity),
          p$Duration %||% "-", fmt(p$PPO),
          p$PPO_interpolated %||% "-",
          fmt(p$VO2peak_abs),fmt(p$VO2peak_rel),
          fmt(p$RERmax),fmt(p$EQO2max),fmt(p$HRmax)))
      DT::datatable(df, rownames = FALSE,
        options = list(pageLength = 20, dom = "t"),
        class = "table-sm table-hover")
    })

    # -- Ausbelastung -------------------------------------------
    output$exhaust_table <- DT::renderDT({
      p <- params_r(); shiny::req(p)
      df <- calc_exhaustion(p)
      df$Wert <- sapply(df$Wert, fmt)
      erf_col <- grep("rf.llt$", names(df), value = TRUE)[1]
      if (!is.na(erf_col))
        df[[erf_col]] <- ifelse(df[[erf_col]], "Ja", "Nein")
      DT::datatable(df, rownames = FALSE, options = list(dom = "t"),
        class = "table-sm table-hover")
    })

    # -- Downloads ----------------------------------------------
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Plot.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = plot_r(), width = 9, height = 5.5, dpi = 600)
    )
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Parameter.csv"),
      content = function(file) {
        p <- params_r()
        df <- tibble::tibble(
          Parameter = c("ID","Zeitpunkt","Gewicht_kg","Groesse_cm",
            "Alter_Jahre","PPO_W","PPO_interpoliert",
            "VO2peak_abs","VO2peak_rel","RERmax","EQO2max","HRmax"),
          Wert = c(p$ID,p$Timepoint,p$Weight_kg,p$Height_cm,
            p$Age_years,p$PPO,p$PPO_interpolated,
            p$VO2peak_abs,p$VO2peak_rel,p$RERmax,p$EQO2max,p$HRmax))
        utils::write.csv2(df, file, row.names = FALSE)
      }
    )
    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Ergebnisse.xlsx"),
      content = function(file) {
        p <- params_r(); shiny::req(p)
        vt_tab <- tryCatch(vt_res$vt_table(), error = function(e) NULL)

        # Bestaetigte VT-Zeiten aus Modul uebernehmen
        vt1_t <- NA_real_; vt2_t <- NA_real_
        if (!is.null(vt_res) && !is.null(vt_res$vt_state)) {
          st <- vt_res$vt_state
          if (isTRUE(st$vt1_confirmed) && is.finite(st$vt1_final))
            vt1_t <- st$vt1_final
          else if (is.finite(st$vt1_time))
            vt1_t <- st$vt1_time
          if (isTRUE(st$vt2_confirmed) && is.finite(st$vt2_final))
            vt2_t <- st$vt2_final
          else if (is.finite(st$vt2_time))
            vt2_t <- st$vt2_time
        }

        export_xlsx(params = p, vt_table = vt_tab,
                    file = file,
                    vt1_time = vt1_t, vt2_time = vt2_t)
      }
    )

    # -- Rueckgabe ----------------------------------------------
    return(params_r)
  })
}

# -- Helpers ---------------------------------------------------
fmt <- function(x, digits = 2) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("-")
  if (is.numeric(x)) format(round(x, digits), decimal.mark = ",", big.mark = ".")
  else as.character(x)
}

make_meta_card <- function(p) {
  badge <- function(val, lbl)
    shiny::span(lbl, ": ", shiny::strong(fmt(val)),
                style = "display:block; margin-bottom:4px;")
  shiny::tagList(
    shiny::h6(shiny::icon("person"), " Proband"),
    badge(p$ID, "ID"), badge(p$Timepoint, "Zeitpunkt"),
    badge(p$Weight_kg, "Gewicht (kg)"),
    badge(p$Height_cm, "Groesse (cm)"),
    badge(p$Age_years, "Alter (Jahre)"),
    shiny::hr(),
    shiny::h6(shiny::icon("trophy"), " Peak-Werte"),
    badge(p$PPO, "PPO (W)"),
    badge(p$VO2peak_rel, "VO2peak rel"),
    badge(p$RERmax, "RERmax"),
    badge(p$EQO2max, "EQO2max")
  )
}