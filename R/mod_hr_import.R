# ============================================================
#  mod_hr_import.R  --  HR-Daten Import & Trim (Plotly)
#  Platzhalter-Modul: vorbereitet fuer spaetere Skript-Integration
# ============================================================

mod_hr_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        bslib::card(
          bslib::card_header(shiny::icon("heart-pulse"), " HR-Import"),
          shiny::fileInput(ns("hr_file"), label = NULL,
                           accept = c(".csv", ".xlsx", ".txt", ".fit"),
                           buttonLabel = "HR-Datei w\u00e4hlen ...",
                           placeholder = "CSV, Excel, FIT"),
          shiny::hr(style = "margin:6px 0;"),
          shiny::tags$h6(shiny::icon("scissors"), " Trimmen",
            style = "font-weight:700; color:#1f3d6b;"),
          shiny::p("Start- und Endzeit im Plot ausw\u00e4hlen.",
                   style = "font-size:0.82rem; color:#888;"),
          shiny::div(style = "display:flex; gap:6px; margin-bottom:6px;",
            shiny::numericInput(ns("trim_start"), "Start (s)",
              value = 0, min = 0, step = 1, width = "100%"),
            shiny::numericInput(ns("trim_end"), "Ende (s)",
              value = NA, min = 0, step = 1, width = "100%")
          ),
          shiny::actionButton(ns("apply_trim"), "Trimmen anwenden",
            class = "btn-outline-primary w-100 mb-2"),
          shiny::actionButton(ns("reset_trim"), "Zur\u00fccksetzen",
            class = "btn-outline-secondary w-100"),
          shiny::hr(style = "margin:8px 0;"),
          shiny::uiOutput(ns("hr_info"))
        )
      ),
      shiny::column(8,
        bslib::card(
          bslib::card_header(shiny::icon("chart-line"), " HR-Vorschau"),
          plotly::plotlyOutput(ns("hr_plot"), height = "400px"),
          shiny::hr(style = "margin:8px 0;"),
          shiny::tags$h6("Getrimmte HR-Daten"),
          plotly::plotlyOutput(ns("hr_trimmed_plot"), height = "250px")
        )
      )
    )
  )
}

mod_hr_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hr_state <- shiny::reactiveValues(
      raw      = NULL,   # data.frame mit time_s, HR
      trimmed  = NULL,   # getrimmter Bereich
      trim_start = 0,
      trim_end   = NA_real_
    )

    # ── HR-Datei laden ───────────────────────────────────────
    #    HOOK: Hier spaeter eigenes Import-Skript einfuegen
    shiny::observeEvent(input$hr_file, {
      fp    <- input$hr_file$datapath
      fname <- input$hr_file$name
      ext   <- tolower(tools::file_ext(fname))

      hr_df <- tryCatch({
        # Standard-CSV mit time_s / HR Spalten
        if (ext %in% c("csv", "txt")) {
          raw <- utils::read.csv(fp, stringsAsFactors = FALSE)
          # Robuste Spaltenfindung
          t_col <- grep("^(time|zeit|timestamp|seconds?|time_s)$",
                        names(raw), ignore.case = TRUE, value = TRUE)[1]
          h_col <- grep("^(hr|hf|heart|heartrate|heart_rate|bpm)$",
                        names(raw), ignore.case = TRUE, value = TRUE)[1]
          if (is.na(t_col) || is.na(h_col)) stop("Spalten time/HR nicht gefunden")
          data.frame(time_s = as.numeric(raw[[t_col]]),
                     HR = as.numeric(raw[[h_col]]))
        } else if (ext %in% c("xlsx", "xls")) {
          raw <- readxl::read_excel(fp, col_types = "numeric")
          t_col <- grep("^(time|zeit|seconds?)$",
                        names(raw), ignore.case = TRUE, value = TRUE)[1]
          h_col <- grep("^(hr|hf|heart|bpm)$",
                        names(raw), ignore.case = TRUE, value = TRUE)[1]
          if (is.na(t_col) || is.na(h_col)) stop("Spalten time/HR nicht gefunden")
          data.frame(time_s = as.numeric(raw[[t_col]]),
                     HR = as.numeric(raw[[h_col]]))
        } else {
          # FIT-Dateien: Platzhalter fuer spaetere Integration
          # → Hier eigenes FIT-Parse-Skript einfuegen
          stop("FIT-Import noch nicht implementiert. Bitte CSV/Excel nutzen.")
        }
      }, error = function(e) {
        shiny::showNotification(paste("HR-Import Fehler:", e$message),
                                type = "error")
        NULL
      })

      if (!is.null(hr_df)) {
        hr_df <- hr_df[is.finite(hr_df$time_s) & is.finite(hr_df$HR), ]
        hr_df$time_s <- hr_df$time_s - min(hr_df$time_s, na.rm = TRUE)
        hr_state$raw <- hr_df
        hr_state$trim_start <- 0
        hr_state$trim_end <- max(hr_df$time_s, na.rm = TRUE)
        hr_state$trimmed <- hr_df
        shiny::updateNumericInput(session, "trim_start", value = 0)
        shiny::updateNumericInput(session, "trim_end",
          value = round(max(hr_df$time_s)))
      }
    })

    # ── Trim anwenden ────────────────────────────────────────
    shiny::observeEvent(input$apply_trim, {
      shiny::req(hr_state$raw)
      s <- input$trim_start %||% 0
      e <- input$trim_end %||% max(hr_state$raw$time_s)
      hr_state$trim_start <- s
      hr_state$trim_end   <- e
      hr_state$trimmed <- hr_state$raw |>
        dplyr::filter(time_s >= s, time_s <= e)
    })

    shiny::observeEvent(input$reset_trim, {
      shiny::req(hr_state$raw)
      hr_state$trim_start <- 0
      hr_state$trim_end <- max(hr_state$raw$time_s, na.rm = TRUE)
      hr_state$trimmed <- hr_state$raw
      shiny::updateNumericInput(session, "trim_start", value = 0)
      shiny::updateNumericInput(session, "trim_end",
        value = round(max(hr_state$raw$time_s)))
    })

    # ── Info ─────────────────────────────────────────────────
    output$hr_info <- shiny::renderUI({
      if (is.null(hr_state$raw))
        return(shiny::p("Keine HR-Datei geladen.", class = "text-muted"))
      n_raw <- nrow(hr_state$raw)
      n_trim <- if (!is.null(hr_state$trimmed)) nrow(hr_state$trimmed) else 0
      shiny::tagList(
        shiny::tags$b(paste0(n_raw, " Datenpunkte geladen")),
        shiny::br(),
        shiny::tags$span(paste0(n_trim, " nach Trim"),
                         style = "color:#059669;")
      )
    })

    # ── Plots ────────────────────────────────────────────────
    output$hr_plot <- plotly::renderPlotly({
      shiny::req(hr_state$raw)
      d <- hr_state$raw
      p <- plotly::plot_ly(d, x = ~time_s, y = ~HR, type = "scatter",
        mode = "lines", line = list(color = "#CD0000", width = 1)) |>
        plotly::layout(
          xaxis = list(title = "Zeit [s]"),
          yaxis = list(title = "HR [bpm]"),
          title = list(text = "HR Rohdaten", font = list(size = 13)),
          margin = list(t = 40))

      # Trim-Bereich markieren
      s <- hr_state$trim_start
      e <- hr_state$trim_end
      if (is.finite(s) && is.finite(e)) {
        p <- p |> plotly::layout(shapes = list(
          list(type = "rect", x0 = s, x1 = e,
               y0 = 0, y1 = 1, yref = "paper",
               fillcolor = "rgba(37,99,235,0.1)",
               line = list(color = "rgba(37,99,235,0.5)", width = 1))
        ))
      }
      p
    })

    output$hr_trimmed_plot <- plotly::renderPlotly({
      shiny::req(hr_state$trimmed)
      d <- hr_state$trimmed
      plotly::plot_ly(d, x = ~time_s, y = ~HR, type = "scatter",
        mode = "lines", line = list(color = "#059669", width = 1.5)) |>
        plotly::layout(
          xaxis = list(title = "Zeit [s]"),
          yaxis = list(title = "HR [bpm]"),
          title = list(text = "Getrimmte HR", font = list(size = 12)),
          margin = list(t = 35))
    })

    # ── Rueckgabe ────────────────────────────────────────────
    return(shiny::reactive(hr_state$trimmed))
  })
}
