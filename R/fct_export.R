# ============================================================
#  fct_export.R  --  DOCX-Bericht-Export via Quarto
# ============================================================

# ============================================================
#  DOCX-Export via Quarto (.qmd → .docx)
#
#  Render-Strategie:
#    1) QMD-Vorlage aus inst/extdata/Bericht_Vorlage.qmd in tempdir kopieren
#    2) Daten als RDS in den gleichen tempdir schreiben
#    3) `quarto render` als System-Aufruf mit `-P payload_rds:<pfad>`
#    4) Output-DOCX in Zielpfad verschieben
# ============================================================

#' Pfad zur mitgelieferten QMD-Vorlage
qmd_template_path <- function() {
  fp <- system.file("extdata", "Bericht_Vorlage.qmd",
                    package = "SpiroAnalyzer")
  if (!nzchar(fp)) fp <- file.path("inst", "extdata", "Bericht_Vorlage.qmd")
  if (!file.exists(fp)) stop("QMD-Vorlage nicht gefunden: ", fp)
  fp
}

#' Erkennt den Pfad zur quarto-Binary (sucht u.a. in /usr/local/bin)
quarto_bin <- function() {
  # bevorzugt: Paket-Helper, falls verfügbar
  if (requireNamespace("quarto", quietly = TRUE)) {
    qp <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
    if (!is.null(qp) && file.exists(qp)) return(qp)
  }
  # Fallback: PATH-Suche
  qp <- Sys.which("quarto")
  if (nzchar(qp)) return(unname(qp))
  # macOS/Homebrew Standardpfad
  for (p in c("/usr/local/bin/quarto", "/opt/homebrew/bin/quarto",
              "/usr/bin/quarto")) {
    if (file.exists(p)) return(p)
  }
  stop(
    "Quarto-Binary nicht gefunden. Bitte installiere Quarto:\n",
    "  https://quarto.org/docs/get-started/\n",
    "(Pfad sollte in PATH verfügbar sein)"
  )
}

#' Erstellt den DOCX-Bericht via Quarto-Rendering der QMD-Vorlage
#'
#' @param params       extract_params()-Ergebnis (mit ts, vt etc.)
#' @param vt1_time,vt2_time  Schwellen-Zeitpunkte in Minuten (NA = nicht gesetzt)
#' @param vt_table     optionale VT-Zusammenfassung (tibble)
#' @param steps_table  optionale Stufentabelle
#' @param mfo_result   optionale MFO-Ergebnisliste
#' @param testleiter,geraet,kommentar  Freitexte aus dem Export-Formular
#' @param vt_comments  list(vt1=..., vt2=..., general=...)
#' @param advanced_settings  list(window_sec, min_step, weight, smooth_n)
#' @param file         Zielpfad (.docx)
export_docx <- function(params,
                        vt1_time            = NA_real_,
                        vt2_time            = NA_real_,
                        vt_table            = NULL,
                        steps_table         = NULL,
                        mfo_result          = NULL,
                        testleiter          = "",
                        geraet              = "",
                        kommentar           = "",
                        vt_comments         = list(),
                        advanced_settings   = list(),
                        file) {

  qbin <- quarto_bin()
  qmd  <- qmd_template_path()

  # Arbeitsverzeichnis: temp-Verzeichnis, damit Quarto-Outputs isoliert bleiben
  work <- tempfile("spirorep_")
  dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  qmd_copy <- file.path(work, "Bericht_Vorlage.qmd")
  file.copy(qmd, qmd_copy, overwrite = TRUE)

  # Paket-R-Dateien sammeln, damit der frische Quarto-R-Prozess
  # alle Hilfsfunktionen kennt (auch ohne installiertes Paket).
  pkg_r_dir <- file.path(getwd(), "R")
  if (!dir.exists(pkg_r_dir)) {
    sf <- system.file("R", package = "SpiroAnalyzer")
    if (nzchar(sf)) pkg_r_dir <- sf
  }
  source_files <- if (dir.exists(pkg_r_dir))
    list.files(pkg_r_dir, pattern = "\\.R$", full.names = TRUE) else character(0)
  source_files <- source_files[!grepl("\\.bak$", source_files)]
  source_files <- normalizePath(source_files, mustWork = FALSE)

  # Payload als RDS — komplexe Objekte (tibbles, Listen) sicher serialisieren
  payload <- list(
    params            = params,
    vt1_time          = vt1_time,
    vt2_time          = vt2_time,
    vt_table          = vt_table,
    steps_table       = steps_table,
    mfo_result        = mfo_result,
    testleiter        = testleiter,
    geraet            = geraet,
    kommentar         = kommentar,
    vt_comments       = vt_comments,
    advanced_settings = advanced_settings,
    source_files      = source_files
  )
  payload_rds <- file.path(work, "payload.rds")
  saveRDS(payload, payload_rds)

  # quarto-Aufruf:  quarto render <qmd> --to docx -P payload_rds:<path>
  args <- c(
    "render", qmd_copy,
    "--to",  "docx",
    "-P",   paste0("payload_rds:", payload_rds)
  )

  # Quarto-Aufruf: Warnungen unterdrücken; wir prüfen Erfolg über den
  # tatsächlichen Datei-Output, nicht über den Status-Code (Quarto setzt
  # Status 1 auch bei rein kosmetischen Knitr-Warnungen).
  res <- suppressWarnings(
    system2(qbin, args, stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status")

  # Output-Datei suchen
  rendered <- file.path(work, "Bericht_Vorlage.docx")
  if (!file.exists(rendered)) {
    msg <- paste(res, collapse = "\n")
    stop("Quarto-Rendering fehlgeschlagen (Status ",
         if (is.null(status)) "?" else status, "):\n", msg)
  }

  ok <- file.copy(rendered, file, overwrite = TRUE)
  if (!isTRUE(ok)) stop("Konnte DOCX nicht nach Zielpfad kopieren: ", file)

  invisible(file)
}
