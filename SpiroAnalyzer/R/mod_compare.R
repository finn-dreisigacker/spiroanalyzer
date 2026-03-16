# ============================================================
#  mod_compare.R  –  Vergleich zweier Spiro-Messungen
# ============================================================

mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      # Messung 1
      shiny::column(3,
        bslib::card(
          bslib::card_header(
            shiny::span(style = "color:#0072B2; font-weight:600;",
                        shiny::icon("circle-1"), " Messung 1 (z.B. T0)")),
          shiny::fileInput(ns("file1"), label = NULL,
                           accept = c(".xlsx", ".xml"),
                           buttonLabel = "Datei w\u00e4hlen ...",
                           placeholder = "XML oder Excel"),
          shiny::uiOutput(ns("meta1"))
        )
      ),
      # Messung 2
      shiny::column(3,
        bslib::card(
          bslib::card_header(
            shiny::span(style = "color:#D55E00; font-weight:600;",
                        shiny::icon("circle-2"), " Messung 2 (z.B. T10)")),
          shiny::fileInput(ns("file2"), label = NULL,
                           accept = c(".xlsx", ".xml"),
                           buttonLabel = "Datei w\u00e4hlen ...",
                           placeholder = "XML oder Excel"),
          shiny::uiOutput(ns("meta2"))
        )
      ),
      # Einstellungen
      shiny::column(3,
        bslib::card(
          bslib::card_header(shiny::icon("sliders"), " Einstellungen"),
          shiny::div(
            style = "display:flex; align-items:center; gap:10px; margin-bottom:6px;",
            shiny::div(
              style = "flex:0 0 auto; font-size:0.83rem; font-weight:600; color:#444; white-space:nowrap;",
              "Gl\u00e4ttung (Punkte):"
            ),
            shiny::div(
              style = "flex:1;",
              shiny::numericInput(ns("smooth"), label = NULL,
                                  value = 20, min = 5, max = 60, step = 1,
                                  width = "100%")
            ),
            shiny::div(
              style = "flex:0 0 auto; font-size:0.75rem; color:#888;",
              "zentriert"
            )
          ),
          shiny::hr(style = "margin:6px 0 10px;"),
          shiny::downloadButton(ns("dl_plot"), "Plot als PNG",
                                class = "btn-outline-primary w-100 mb-2"),
          shiny::downloadButton(ns("dl_csv"), "Vergleich als CSV",
                                class = "btn-outline-secondary w-100")
        )
      )
    ),
    # Plot & Tabelle
    shiny::fluidRow(
      shiny::column(12,
        bslib::navset_card_tab(
          bslib::nav_panel(
            shiny::icon("chart-area"), " Vergleichsplot",
            shinycssloaders::withSpinner(
              shiny::plotOutput(ns("comp_plot"), height = "460px"), type = 6)
          ),
          bslib::nav_panel(
            shiny::icon("table-columns"), " \u0394-Tabelle",
            shiny::br(),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("delta_table")), type = 6)
          )
        )
      )
    )
  )
}

mod_compare_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    make_params <- function(file_input) {
      shiny::reactive({
        shiny::req(file_input())
        fp    <- file_input()$datapath
        fname <- file_input()$name
        shiny::withProgress(message = paste("Lade", fname, "..."), value = 0.3, {
          spiro <- tryCatch(load_spiro_file(fp, fname), error = function(e) {
            shiny::showNotification(paste("Fehler:", e$message), type = "error"); NULL
          })
          shiny::setProgress(0.7)
          if (is.null(spiro)) return(NULL)
          p <- tryCatch(extract_params(spiro, fname), error = function(e) {
            shiny::showNotification(paste("Fehler beim Parsen:", e$message),
                                    type = "error"); NULL
          })
          shiny::setProgress(1)
          p
        })
      })
    }

    p1_r <- make_params(shiny::reactive(input$file1))
    p2_r <- make_params(shiny::reactive(input$file2))

    label1_r <- shiny::reactive({
      p <- p1_r(); if (is.null(p)) return("Messung 1")
      paste0(p$ID %||% "M1", "_", p$Timepoint %||% "T1")
    })
    label2_r <- shiny::reactive({
      p <- p2_r(); if (is.null(p)) return("Messung 2")
      paste0(p$ID %||% "M2", "_", p$Timepoint %||% "T2")
    })

    output$meta1 <- shiny::renderUI({
      p <- p1_r()
      if (is.null(p)) return(shiny::p("Noch keine Datei geladen.", class = "text-muted"))
      make_meta_mini(p, colour = "#0072B2")
    })
    output$meta2 <- shiny::renderUI({
      p <- p2_r()
      if (is.null(p)) return(shiny::p("Noch keine Datei geladen.", class = "text-muted"))
      make_meta_mini(p, colour = "#D55E00")
    })

    comp_plot_r <- shiny::reactive({
      p1 <- p1_r(); p2 <- p2_r()
      shiny::req(!is.null(p1), !is.null(p2), !is.null(p1$ts), !is.null(p2$ts))
      compare_plot(p1$ts, p2$ts, label1_r(), label2_r(),
                   smooth_n = input$smooth %||% 20)
    })

    output$comp_plot <- shiny::renderPlot({ comp_plot_r() }, res = 120)

    output$delta_table <- DT::renderDT({
      p1 <- p1_r(); p2 <- p2_r()
      shiny::req(!is.null(p1), !is.null(p2))
      df <- compare_table(p1, p2)
      df_disp <- df
      num_cols <- c(2, 3, 4, 5)
      df_disp[, num_cols] <- lapply(df_disp[, num_cols], function(col)
        ifelse(is.na(col), "\u2013", format(round(col, 2), decimal.mark = ",")))
      names(df_disp)[5] <- "\u0394%"
      DT::datatable(df_disp, rownames = FALSE,
                    options = list(pageLength = 15, dom = "t"),
                    class = "table-sm table-hover") |>
        DT::formatStyle("\u0394",
          color = DT::styleInterval(c(-0.001, 0.001),
                                    c("#D55E00", "black", "#009E73"))) |>
        DT::formatStyle("\u0394%", fontWeight = "bold")
    })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0(label1_r(), "_vs_", label2_r(), "_Plot.png"),
      content  = function(file)
        ggplot2::ggsave(file, plot = comp_plot_r(), width = 10, height = 6, dpi = 600)
    )

    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0(label1_r(), "_vs_", label2_r(), "_Vergleich.csv"),
      content  = function(file) {
        p1 <- p1_r(); p2 <- p2_r()
        shiny::req(!is.null(p1), !is.null(p2))
        utils::write.csv2(compare_table(p1, p2), file, row.names = FALSE)
      }
    )
  })
}

# ── Meta-Schnellansicht  (inkl. PPO W/kg) ───────────────────
make_meta_mini <- function(p, colour = "#333") {
  b <- function(lbl, val) shiny::tags$p(
    shiny::tags$b(lbl), ": ",
    if (is.null(val) || (length(val) == 1 && is.na(val))) "\u2013"
    else if (is.numeric(val)) format(round(val, 2), decimal.mark = ",")
    else as.character(val),
    style = "margin-bottom:3px; font-size:0.88em;"
  )
  shiny::tagList(
    shiny::tags$div(
      style = paste0("border-left: 3px solid ", colour, "; padding-left:8px;"),
      b("ID", p$ID), b("Zeitpunkt", p$Timepoint),
      b("Gewicht (kg)", p$Weight_kg), b("Alter (J.)", p$Age_years),
      shiny::tags$hr(style = "margin:6px 0;"),
      b("PPO (W)", p$PPO),
      b("PPO (W/kg)", p$PPO_wkg),          # NEU
      b("VO\u2082peak rel", p$VO2peak_rel),
      b("RERmax", p$RERmax), b("HRmax", p$HRmax)
    )
  )
}