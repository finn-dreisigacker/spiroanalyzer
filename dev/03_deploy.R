# ============================================================
#  03_deploy.R  --  Deployment auf shinyapps.io
# ============================================================
#
#  Voraussetzungen (einmalig):
#
#  1) Account bei https://www.shinyapps.io anlegen / einloggen.
#
#  2) Token auf https://www.shinyapps.io/admin/#/tokens erzeugen.
#     Beim Klick auf "Show" steht dort eine setAccountInfo()-Zeile,
#     die NAME / TOKEN / SECRET enthält.
#
#  3) Diese Werte in ~/.Renviron eintragen (NICHT in dieses Skript!):
#
#        SHINYAPPS_NAME=dreisigacker-finn
#        SHINYAPPS_TOKEN=...
#        SHINYAPPS_SECRET=...
#
#     Anschließend R neu starten, damit die Variablen gelesen werden.
#
#  4) Pakete sicherstellen:
#        install.packages(c("rsconnect", "devtools"))
#
#  Danach reicht es, dieses Skript Schritt für Schritt auszuführen.
# ============================================================

# ── 0) Optional: lokalen Smoke-Test ──────────────────────────
# Lokal starten und alle Tabs einmal anklicken bevor deployt wird.
# source("start.R")

# ── 1) Anmeldedaten setzen ───────────────────────────────────
shiny_name   <- Sys.getenv("SHINYAPPS_NAME",   unset = "")
shiny_token  <- Sys.getenv("SHINYAPPS_TOKEN",  unset = "")
shiny_secret <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

stopifnot(
  "SHINYAPPS_NAME/TOKEN/SECRET fehlen in ~/.Renviron"
    = nzchar(shiny_name) && nzchar(shiny_token) && nzchar(shiny_secret)
)

rsconnect::setAccountInfo(
  name   = shiny_name,
  token  = shiny_token,
  secret = shiny_secret
)

# Sanity-Check: ist der Account hinterlegt?
print(rsconnect::accounts())

# ── 2) Paket installieren, damit run_app() im Cloud-Prozess
#       auflösbar ist (shinyapps.io kennt sonst SpiroAnalyzer nicht).
#       Das schreibt eine app.R am Repo-Root, die shinyapps.io
#       als Entry-Point benutzt.
golem::add_shinyappsio_file()   # erzeugt app.R (idempotent)

# ── 3) Welche Dateien sollen mit hochgeladen werden? ─────────
#       Wir nehmen explizit die wirklich nötigen Pfade:
#       - DESCRIPTION/NAMESPACE für die Paket-Installation auf shinyapps.io
#       - R/ mit allen Modulen und Helpers
#       - inst/ mit Demo-Daten, QMD-Vorlage, www-Assets
#       - app.R als Entry-Point (von add_shinyappsio_file erzeugt)
deploy_files <- c(
  "DESCRIPTION", "NAMESPACE", "app.R",
  list.files("R",   recursive = TRUE, full.names = TRUE),
  list.files("inst", recursive = TRUE, full.names = TRUE)
)

# Sicherheitsfilter: keine Backups, keine .DS_Store, keine Caches
deploy_files <- deploy_files[
  !grepl("\\.bak$|\\.DS_Store$|/_cache/|/_files/|\\.Rproj\\.user", deploy_files)
]

cat("Werden hochgeladen (", length(deploy_files), " Dateien):\n", sep = "")
cat("  ", head(deploy_files, 20), sep = "\n  ")
if (length(deploy_files) > 20) cat("  ... (+ ", length(deploy_files) - 20, " weitere)\n", sep = "")

# ── 4) Tatsächliches Deployment ──────────────────────────────
#       appName  → URL-Slug: https://<account>.shinyapps.io/<appName>/
#       appTitle → menschenlesbarer Titel im Dashboard
rsconnect::deployApp(
  appDir       = ".",
  appName      = "spiroanalyzer",
  appTitle     = "SpiroAnalyzer",
  appFiles     = deploy_files,
  account      = shiny_name,
  server       = "shinyapps.io",
  forceUpdate  = TRUE,
  launch.browser = TRUE
)

# Wenn alles glatt durchläuft, öffnet sich die App im Browser unter
#   https://<SHINYAPPS_NAME>.shinyapps.io/spiroanalyzer/

# ============================================================
#  Hinweise zur Fehlersuche
# ============================================================
#
#  - "Application failed to start": meist Paket-Auflösung.
#    rsconnect liest die DESCRIPTION; alle Imports/Suggests müssen
#    auf CRAN sein. `spiro` (ZAN-Branch) ist auf GitHub – falls die
#    Cloud sie braucht, in DESCRIPTION als Remotes deklarieren:
#        Remotes: ropensci/spiro@zan2
#    Andernfalls Import-Aufrufe weiter über requireNamespace() lazy.
#
#  - Quarto-DOCX-Export funktioniert auf shinyapps.io NICHT direkt
#    (kein Quarto-Binary). Wenn DOCX-Export in der Cloud nötig ist,
#    musst du auf Posit Connect / ShinyProxy umsteigen oder den
#    Export deaktivieren, wenn `quarto_bin()` fehlschlägt.
#
#  - Logs ansehen:  rsconnect::showLogs(appName = "spiroanalyzer")
#  - App stoppen:   rsconnect::terminateApp("spiroanalyzer")
#  - App löschen:   rsconnect::deleteApp("spiroanalyzer")
