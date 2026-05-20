#!/usr/bin/env Rscript
# ============================================================
#  start.R  –  Einfacher Starter (ohne Paket-Installation)
#
#  Verwendung:
#    cd SpiroAnalyzer
#    Rscript start.R
#  oder in RStudio:
#    source("start.R")
# ============================================================

cat("=== SpiroAnalyzer wird gestartet ===\n")

# Benötigte Pakete prüfen / installieren
pkgs <- c("shiny","bslib","openxlsx","readxl","xml2","dplyr","tidyr",
          "stringr","lubridate","zoo","ggplot2","hms","DT",
          "shinycssloaders","purrr","tibble","golem","htmltools")

missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installiere fehlende Pakete:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cran.rstudio.com/")
}

# Alle Pakete laden
invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE))

# Alle R-Quellcode-Dateien laden
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))

cat("Öffne App im Browser ...\n")
shiny::shinyApp(ui = app_ui, server = app_server)
