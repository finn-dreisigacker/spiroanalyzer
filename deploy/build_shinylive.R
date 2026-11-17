# Baut die statische Browser-Version (Shinylive/webR) nach site/.
# Aufruf aus dem Projektordner:  Rscript deploy/build_shinylive.R
app_dir <- file.path(tempdir(), "spiroanalyzer-app")
unlink(app_dir, recursive = TRUE)
dir.create(file.path(app_dir, "inst", "extdata"), recursive = TRUE)
file.copy("R", app_dir, recursive = TRUE)
file.copy("inst/extdata/data_demo.xlsx", file.path(app_dir, "inst", "extdata"))

writeLines(c(
  "library(shiny)",
  "for (f in list.files('R', pattern = '\\\\.R$', full.names = TRUE)) source(f)",
  "shinyApp(ui = app_ui, server = app_server)"
), file.path(app_dir, "app.R"))

unlink("site", recursive = TRUE)
shinylive::export(app_dir, "site")
