# ============================================================
#  run_app.R  –  App starten (golem-Konvention)
# ============================================================
#
#  Aufruf:  source("run_app.R")
#  oder:    SpiroAnalyzer::run_app()
# ============================================================

#' App starten
#' @param ... weitere Argumente an shiny::shinyApp()
#' @export
run_app <- function(...) {
  # Alle Funktionen aus dem Paket laden (wenn direkt gesourced)
  if (!exists("app_ui", mode = "function")) {
    src_files <- list.files(
      file.path(dirname(sys.frame(1)$ofile), "R"),
      pattern = "\\.R$", full.names = TRUE
    )
    lapply(src_files, source)
  }

  shiny::shinyApp(
    ui     = app_ui,
    server = app_server,
    ...
  )
}

# Direkt starten, wenn dieses Skript selbst ausgeführt wird
if (sys.nframe() == 0) run_app()
