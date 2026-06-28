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
#' @param include_mfo  soll die MFO-Sektion in den Bericht? (Default FALSE)
#' @param file         Zielpfad (.docx)
#' Erzeugt eine Referenz-DOCX (officer) mit Kopf-/Fußzeile für Quarto
#'
#' Kopfzeile: Proband-ID + Testdatum (rechtsbündig, dezent).
#' Fußzeile:  "SpiroAnalyzer — Auswertungsfall · Seite X von Y" (zentriert).
#' Wird per-Render erzeugt, damit die Kopfzeile dynamisch ist.
make_reference_docx <- function(path, header_text) {
  grey <- officer::fp_text(font.size = 9, color = "#5b6770",
                           font.family = "Arial")
  hb   <- officer::fp_border(color = "#cccccc", width = 0.75)
  header <- officer::block_list(
    officer::fpar(
      officer::ftext(header_text, grey),
      fp_p = officer::fp_par(text.align = "right", padding.bottom = 3,
                             border.bottom = hb)))
  footer <- officer::block_list(
    officer::fpar(
      officer::ftext("SpiroAnalyzer — Auswertungsfall    ·    Seite ", grey),
      officer::run_word_field("PAGE"),
      officer::ftext(" von ", grey),
      officer::run_word_field("NUMPAGES"),
      fp_p = officer::fp_par(text.align = "center", padding.top = 3,
                             border.top = hb)))
  sect <- officer::prop_section(header_default = header,
                                footer_default = footer)
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, sect)
  print(doc, target = path)
  invisible(path)
}

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
                        include_mfo         = FALSE,
                        file) {

  qbin <- quarto_bin()
  qmd  <- qmd_template_path()

  # Arbeitsverzeichnis: temp-Verzeichnis, damit Quarto-Outputs isoliert bleiben
  work <- tempfile("spirorep_")
  dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  qmd_copy <- file.path(work, "Bericht_Vorlage.qmd")
  file.copy(qmd, qmd_copy, overwrite = TRUE)

  # Referenz-DOCX (Kopf-/Fußzeile) per-Render erzeugen. Schlägt das fehl,
  # wird die reference-doc-Zeile aus der QMD-Kopie entfernt, damit der
  # Render trotzdem durchläuft.
  ref_path <- file.path(work, "reference.docx")
  header_text <- {
    pid <- params$ID %||% ""
    td  <- if (!is.null(params$Date) && !is.na(params$Date))
      format(params$Date, "%d.%m.%Y") else format(Sys.Date(), "%d.%m.%Y")
    paste0(if (nzchar(pid)) pid else "Auswertungsfall", "  ·  ", td)
  }
  ref_ok <- tryCatch({ make_reference_docx(ref_path, header_text); TRUE },
                     error = function(e) FALSE)
  if (!ref_ok) {
    ql <- readLines(qmd_copy, warn = FALSE)
    ql <- ql[!grepl("^\\s*reference-doc:\\s*reference\\.docx\\s*$", ql)]
    writeLines(ql, qmd_copy)
  }

  # Bibliografie (reference.bib) + APA-CSL (apa.csl) mitkopieren, falls
  # vorhanden. Fehlt eine Datei, wird die zugehörige YAML-Zeile entfernt,
  # damit der Render nicht abbricht (csl ist optional, vom Nutzer geladen).
  tmpl_dir <- dirname(qmd)
  for (aux in c("reference.bib", "apa.csl")) {
    src <- file.path(tmpl_dir, aux)
    if (file.exists(src)) file.copy(src, file.path(work, aux), overwrite = TRUE)
  }
  strip_yaml <- function(pattern) {
    ql <- readLines(qmd_copy, warn = FALSE)
    writeLines(ql[!grepl(pattern, ql)], qmd_copy)
  }
  if (!file.exists(file.path(work, "apa.csl")))
    strip_yaml("^\\s*csl:\\s*apa\\.csl\\s*$")
  if (!file.exists(file.path(work, "reference.bib")))
    strip_yaml("^\\s*bibliography:\\s*reference\\.bib\\s*$")

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
    include_mfo       = isTRUE(include_mfo),
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
