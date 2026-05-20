# ============================================================
#  fct_data.R  –  Daten einlesen & Parameter berechnen
#  v3 – PPO (W/kg) berechnet und in Vergleichstabelle ergänzt
# ============================================================

# ---- globale Hilfsfunktion ----
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && (is.na(x) || x == ""))) y else x
}

# ---- sicheres rollapply (NA-robust, kurze Vektoren) --------
safe_roll <- function(x, width = 20) {
  if (!is.numeric(x) || length(x) == 0) return(rep(NA_real_, length(x)))
  n_finite <- sum(is.finite(x))
  if (n_finite < 3) return(rep(NA_real_, length(x)))
  w <- min(width, floor(n_finite / 2))
  if (w < 2) return(x)
  zoo::rollapply(x, width = w, FUN = function(z) mean(z, na.rm = TRUE),
                 fill = NA, align = "center")
}

# ----------------------------------------------------------------
#  VO2-Peak als höchstes gleitendes Mittel über `window_sec`
#  Sekunden -- AUSSCHLIESSLICH innerhalb der Belastungsphase.
#
#  Hintergrund: ein zentriertes rollendes Mittel auf der vollen
#  Zeitreihe verzerrt VO2peak nach unten, sobald das Fenster nahe
#  am Ende der Belastung in den Cooldown ragt. Daher rechnet diese
#  Hilfsfunktion das Mittel ausschließlich auf den Belastungs-
#  Samples (Phase %in% c("Belastung","Exercise")).
#
#  Liefert: list(value = max-Wert, t_center_min = Mittelpunkt
#  des besten Fensters in Minuten, n_samples = Anzahl Samples im
#  Fenster, ok = TRUE/FALSE)
# ----------------------------------------------------------------
vo2_peak_window <- function(ts, column = "VO2abs", window_sec = 30,
                             t_min = NA_real_, t_max = NA_real_) {
  empty <- list(value = NA_real_, t_center_min = NA_real_,
                t_start_min = NA_real_, t_end_min = NA_real_,
                n_samples = 0L, ok = FALSE)
  if (is.null(ts) || nrow(ts) == 0 || !column %in% names(ts))
    return(empty)

  bel <- ts[ts$Phase %in% c("Belastung", "Exercise"), , drop = FALSE]
  bel <- bel[is.finite(bel$time_s) & is.finite(bel[[column]]), , drop = FALSE]
  if (nrow(bel) < 3) return(empty)

  bel <- bel[order(bel$time_s), , drop = FALSE]
  t_s <- bel$time_s
  v   <- bel[[column]]

  # Falls ein expliziter Such-Bereich gegeben ist (z.B. aus dem
  # Verifikations-Tab): auf diesen Bereich beschränken.
  if (is.finite(t_min) || is.finite(t_max)) {
    tmin_s <- if (is.finite(t_min)) t_min * 60 else -Inf
    tmax_s <- if (is.finite(t_max)) t_max * 60 else  Inf
    keep <- t_s >= tmin_s & t_s <= tmax_s
    if (sum(keep) < 3) return(empty)
    bel <- bel[keep, , drop = FALSE]
    t_s <- bel$time_s
    v   <- bel[[column]]
  }

  # Schiebe-Fenster mit einer Breite von `window_sec` Sekunden
  # (Belastungssamples -- nicht-äquidistant). Für jedes i das
  # Mittel aller Punkte in [t_i, t_i + window_sec].
  best_mean   <- -Inf
  best_i      <- NA_integer_
  best_j      <- NA_integer_
  best_start  <- NA_real_
  best_end    <- NA_real_

  n <- length(t_s)
  j <- 1L
  for (i in seq_len(n)) {
    win_end <- t_s[i] + window_sec
    while (j < n && t_s[j + 1L] <= win_end) j <- j + 1L
    if (t_s[j] - t_s[i] < window_sec * 0.5) next   # Mindestbreite
    m <- mean(v[i:j], na.rm = TRUE)
    if (is.finite(m) && m > best_mean) {
      best_mean  <- m
      best_i     <- i
      best_j     <- j
      best_start <- t_s[i]
      best_end   <- t_s[j]
    }
  }

  if (!is.finite(best_mean)) return(empty)

  list(value        = best_mean,
       t_center_min = (best_start + best_end) / 2 / 60,
       t_start_min  = best_start / 60,
       t_end_min    = best_end   / 60,
       n_samples    = as.integer(best_j - best_i + 1L),
       ok           = TRUE)
}
# ---- Rohen data.frame aus XLSX lesen (openxlsx -> readxl Fallback) ----------
#   Gibt list(raw = data.frame, engine = "openxlsx"|"readxl")
#   raw: alle Zellen als character, inkl. Leerzeilen
read_xlsx_raw <- function(path, sheet = 1) {

  # --- Versuch 1: openxlsx ---
  raw_oxl <- tryCatch({
    wb  <- openxlsx::loadWorkbook(path)
    shn <- names(wb)
    sh  <- if (is.numeric(sheet)) shn[sheet] else sheet
    df  <- openxlsx::readWorkbook(wb, sheet = sh,
                                   colNames = FALSE,
                                   skipEmptyRows  = FALSE,
                                   skipEmptyCols  = FALSE)
    df[] <- lapply(df, as.character)
    df
  }, error = function(e) NULL, warning = function(w) NULL)

  if (!is.null(raw_oxl) && nrow(raw_oxl) > 0)
    return(list(raw = raw_oxl, engine = "openxlsx"))

  # --- Versuch 2: readxl (unterstuetzt Strict OOXML, aeltere Formate etc.) ---
  raw_rxl <- tryCatch({
    df <- readxl::read_excel(path,
                              sheet        = sheet,
                              col_names    = FALSE,
                              col_types    = "text",
                              .name_repair = "unique")
    as.data.frame(df, stringsAsFactors = FALSE)
  }, error = function(e) NULL, warning = function(w) NULL)

  if (!is.null(raw_rxl) && nrow(raw_rxl) > 0)
    return(list(raw = raw_rxl, engine = "readxl"))

  # --- Beide gescheitert ---
  stop(paste0(
    "Die Excel-Datei konnte weder mit openxlsx noch mit readxl gelesen werden.\n",
    "Bitte speichere die Datei in Excel neu als 'Excel-Arbeitsmappe (*.xlsx)'."
  ))
}

# ============================================================
#   XML: direkte Spiro-Datei einlesen
# ============================================================
parse_xml_spiro <- function(path) {
  doc <- xml2::read_xml(path)
  ns  <- xml2::xml_ns(doc)
  rows <- xml2::xml_find_all(doc, ".//ss:Row", ns)

  row_cells <- function(row) {
    cells <- xml2::xml_find_all(row, ".//ss:Cell", ns)
    style <- xml2::xml_attr(cells, "ss:StyleID")
    text  <- vapply(cells, function(c) {
      d <- xml2::xml_find_first(c, ".//ss:Data", ns)
      if (inherits(d, "xml_missing") || is.na(d)) NA_character_ else xml2::xml_text(d)
    }, character(1))
    data.frame(style = style, text = text, stringsAsFactors = FALSE)
  }

  all_cells <- lapply(rows, row_cells)

  # --- 1) Metadaten ---
  meta <- list()
  for (i in seq_along(all_cells)) {
    rc <- all_cells[[i]]
    if (nrow(rc) == 0 || is.na(rc$style[1])) next
    if (grepl("HeadTableParameterName", rc$style[1]) &&
        !is.na(rc$text[1]) && nzchar(trimws(rc$text[1]))) {
      if (i < length(all_cells)) {
        rv <- all_cells[[i + 1]]
        if (nrow(rv) > 0 && !is.na(rv$style[1]) &&
            grepl("HeadTableParameterValue", rv$style[1])) {
          meta[[trimws(rc$text[1])]] <- trimws(rv$text[1])
        }
      }
    }
  }

  # --- 2) Header-Row finden ---
  header_idx <- NA_integer_
  for (i in seq_along(all_cells)) {
    rc <- all_cells[[i]]
    if (nrow(rc) == 0) next
    if (any(grepl("MeasurementDataTableHead", rc$style, na.rm = TRUE)) &&
        any(trimws(rc$text) == "t", na.rm = TRUE)) {
      header_idx <- i; break
    }
  }

  if (is.na(header_idx)) {
    warning("XML: Keine Messdaten-Kopfzeile (mit 't') gefunden.")
    return(list(meta = meta, ts = NULL, vt = NULL))
  }

  col_names <- trimws(all_cells[[header_idx]]$text)
  col_names[is.na(col_names) | col_names == ""] <-
    paste0("col_", which(is.na(col_names) | col_names == ""))

  data_start <- header_idx + 2

  # --- 3) Daten-Rows ---
  valid_pfx <- c("MeasurementDataTableTime", "MeasurementDataTablePhases",
                 "MeasurementDataTableMarkers", "MeasurementDataTableValues")
  data_rows <- list()
  for (i in seq(data_start, length(all_cells))) {
    rc <- all_cells[[i]]
    if (nrow(rc) == 0) next
    has_data <- any(sapply(rc$style, function(s)
      !is.na(s) && any(startsWith(s, valid_pfx))), na.rm = TRUE)
    if (!has_data) next
    vals <- rep(NA_character_, length(col_names))
    n    <- min(nrow(rc), length(col_names))
    vals[seq_len(n)] <- rc$text[seq_len(n)]
    data_rows[[length(data_rows) + 1]] <- vals
  }

  if (length(data_rows) == 0) {
    warning("XML: Keine Daten-Rows nach Header.")
    return(list(meta = meta, ts = NULL, vt = NULL))
  }

  df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  colnames(df) <- col_names

  for (cn in setdiff(col_names, c("t", "Phase", "Marker"))) {
    if (cn %in% names(df))
      df[[cn]] <- suppressWarnings(as.numeric(gsub(",", ".", df[[cn]])))
  }

  list(meta = meta, ts = build_timeseries(df), vt = NULL)
}

# ============================================================
#   XLSX: Pfad -> list(meta, ts)  [openxlsx + readxl Fallback]
# ============================================================
parse_xlsx_spiro <- function(path, filename, sheet = 1) {

  result <- read_xlsx_raw(path, sheet = sheet)   # wirft bei Fehler
  raw    <- result$raw

  trim_chr <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x %in% c("", "NA")] <- NA_character_
    x
  }

  norm_lbl <- function(x) {
    x <- trim_chr(x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(x)
    x <- gsub("\\s+", " ", x)
    x
  }

  # Labelsuche mit Aliasen (DE/EN)
  find_value_by_labels <- function(labels) {
    labels_n <- unique(stats::na.omit(norm_lbl(labels)))
    for (i in seq_len(nrow(raw))) {
      rr   <- trim_chr(unlist(raw[i, , drop = FALSE], use.names = FALSE))
      rr_n <- norm_lbl(rr)
      hit <- which(rr_n %in% labels_n)
      if (length(hit) > 0) {
        j <- hit[1]
        if (j < length(rr)) {
          cand <- rr[(j + 1):length(rr)]
          cand <- cand[!is.na(cand)]
          if (length(cand) > 0) return(cand[1])
        }
      }
    }
    NA_character_
  }

  meta <- list(
    "ID"                = find_value_by_labels(c("ID")),
    "Geburtsdatum"      = find_value_by_labels(c("Geburtsdatum", "Date of Birth")),
    "Größe"             = find_value_by_labels(c("Größe", "Groesse", "Height")),
    "Gewicht"           = find_value_by_labels(c("Gewicht", "Weight")),
    "Startzeit"         = find_value_by_labels(c("Startzeit", "Start Time")),
    "Dauer"             = find_value_by_labels(c("Dauer", "Duration")),
    "Temperatur"        = find_value_by_labels(c("Temperatur", "Temperature")),
    "Luftdruck"         = find_value_by_labels(c("Luftdruck", "Barometric Pressure")),
    "Luftfeuchtigkeit"  = find_value_by_labels(c("Luftfeuchtigkeit", "Humidity"))
  )

  # Header-Zeile dynamisch finden (t + Phase/Stage)
  header_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    rr <- trim_chr(unlist(raw[i, , drop = FALSE], use.names = FALSE))
    if (any(rr == "t", na.rm = TRUE) &&
        any(rr %in% c("Phase", "Stage"), na.rm = TRUE)) {
      header_row <- i
      break
    }
  }
  if (is.na(header_row)) return(list(meta = meta, ts = NULL, vt = NULL))

  header <- trim_chr(unlist(raw[header_row, , drop = FALSE], use.names = FALSE))
  if (all(is.na(header))) return(list(meta = meta, ts = NULL, vt = NULL))

  # Leere Headernamen auffüllen
  empty_idx <- which(is.na(header))
  if (length(empty_idx) > 0) header[empty_idx] <- paste0("col_", empty_idx)

  # Daten beginnen 2 Zeilen unter Header (1 Zeile = Einheiten)
  if ((header_row + 2) > nrow(raw)) return(list(meta = meta, ts = NULL, vt = NULL))
  df <- raw[(header_row + 2):nrow(raw), , drop = FALSE]
  names(df) <- header

  # Nur Zeilen behalten, die Inhalt haben
  keep <- apply(df, 1, function(r) any(!is.na(trim_chr(r))))
  df <- df[keep, , drop = FALSE]

  if (nrow(df) == 0) return(list(meta = meta, ts = NULL, vt = NULL))

  # Numerische Spalten
  # Numerische Spalten (inkl. HealthFit/MetasoftStudio: WR=Power, BF=Atemfreq.)
  num_cols <- c("P", "WR",
                "VO2.kg","V.O2.kg","VO2","V.O2","RER",
                "VE.VO2","V.E.V.O2","V.E.V.O2.","VE.VCO2","V.E.V.CO2",
                "VE","V.E","VT","AF","BF","HR","HF","VCO2","V.CO2",
                "PetCO2","ExCO2","PEO2",
                "V'O2", "V'O2/kg", "V'O2/HR", "V'E/V'O2", "V'E/V'CO2", "V'E",
                "v", "Speed", "Geschw.", "Geschwindigkeit", "km/h",
                "Stufe", "Step", "Lev", "Level")

  for (cn in intersect(num_cols, names(df))) {
    df[[cn]] <- suppressWarnings(as.numeric(gsub(",", ".", df[[cn]])))
  }

  # ── VT1/VT2 aus Übersichtstabelle extrahieren ───────────────
  vt_data <- parse_overview_vt(raw, trim_chr)

  list(meta = meta, ts = build_timeseries(df), vt = vt_data)
}

# ============================================================
#   VT1 / VT2 aus Übersichtstabelle extrahieren (XLSX)
# ============================================================
parse_overview_vt <- function(raw, trim_chr_fn) {
  # Suche "Kanal" Zeile der Übersichtstabelle
  header_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    rr <- trim_chr_fn(unlist(raw[i, , drop = FALSE], use.names = FALSE))
    if (any(rr == "Kanal", na.rm = TRUE) &&
        any(rr == "VT1",   na.rm = TRUE)) {
      header_row <- i
      break
    }
  }
  if (is.na(header_row)) return(NULL)

  hdr <- trim_chr_fn(unlist(raw[header_row, , drop = FALSE], use.names = FALSE))
  col_vt1 <- which(hdr == "VT1")[1]
  col_vt2 <- which(hdr == "VT2")[1]

  if (is.na(col_vt1) || is.na(col_vt2)) return(NULL)

  parse_de <- function(x) {
    if (is.null(x) || is.na(x) || x %in% c("", "-", "NA")) return(NA_real_)
    suppressWarnings(as.numeric(gsub(",", ".", trimws(x))))
  }

  vt <- list()
  search_rows <- seq(header_row + 1, min(header_row + 20, nrow(raw)))
  for (i in search_rows) {
    rr <- trim_chr_fn(unlist(raw[i, , drop = FALSE], use.names = FALSE))
    kanal <- rr[1]
    if (is.na(kanal) || kanal == "") next
    v1 <- if (col_vt1 <= length(rr)) parse_de(rr[col_vt1]) else NA_real_
    v2 <- if (col_vt2 <= length(rr)) parse_de(rr[col_vt2]) else NA_real_

    # Normalisiere Kanal-Name
    key <- gsub("\u2019", "'", kanal)  # fancy apostrophe
    vt[[paste0(key, "_VT1")]] <- v1
    vt[[paste0(key, "_VT2")]] <- v2
  }
  vt
}

# ============================================================
#   Gemeinsame Zeitreihe aus rohem df
# ============================================================
build_timeseries <- function(df, smooth_n = 20) {
  pick <- function(cands) {
    nm <- intersect(cands, names(df))
    if (length(nm) == 0) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[nm[1]]]))))
  }

  t_raw <- if ("t" %in% names(df)) df[["t"]] else rep(NA_character_, nrow(df))

  # Zeitparsing: HH:MM:SS, HH:MM:SS.ms, h:mm:ss.ms (HealthFit/MetasoftStudio)
  parse_time_to_sec <- function(x) {
    x <- as.character(x)
    # Versuch 1: hms::parse_hms (robust fuer HH:MM:SS[.ms])
    out <- suppressWarnings(tryCatch(
      as.numeric(hms::parse_hms(x)),
      error = function(e) rep(NA_real_, length(x))
    ))
    # Versuch 2: lubridate::hms (flexibler bei einstelliger Stunde)
    bad <- is.na(out) & !is.na(x) & nzchar(trimws(x)) & trimws(x) != "NA"
    if (any(bad)) {
      out[bad] <- suppressWarnings(tryCatch(
        as.numeric(lubridate::hms(x[bad], quiet = TRUE)),
        error = function(e) rep(NA_real_, sum(bad))
      ))
    }
    # Versuch 3: manuelles Regex-Parsen "h:mm:ss[.ms]"
    bad2 <- is.na(out) & !is.na(x) & nzchar(trimws(x)) & trimws(x) != "NA"
    if (any(bad2)) {
      out[bad2] <- vapply(x[bad2], function(s) {
        m <- regmatches(s, regexpr("^(\\d+):(\\d+):(\\d+(?:\\.\\d+)?)$", s, perl = TRUE))
        if (length(m) == 0 || !nzchar(m)) return(NA_real_)
        p <- strsplit(m, ":")[[1]]
        suppressWarnings(as.numeric(p[1])*3600 + as.numeric(p[2])*60 + as.numeric(p[3]))
      }, numeric(1))
    }
    out
  }

  sec <- parse_time_to_sec(t_raw)

  sec0 <- suppressWarnings(min(sec, na.rm = TRUE))
  sec_rel <- if (is.finite(sec0)) sec - sec0 else rep(NA_real_, length(sec))

  phase_raw <- if ("Phase" %in% names(df)) {
    df[["Phase"]]
  } else if ("Stage" %in% names(df)) {
    df[["Stage"]]
  } else {
    rep(NA_character_, nrow(df))
  }

  phase <- trimws(as.character(phase_raw))
  phase[phase %in% c("Warm Up", "Warm-Up", "Warmup")] <- "Erwärmung"
  phase[phase %in% c("Exercise")] <- "Belastung"
  phase[phase %in% c("Recovery", "Cool Down", "Cooldown")] <- "Erholung"

  VO2abs <- pick(c("V'O2",    "VO2",    "V.O2"))
  VO2kg  <- pick(c("V'O2/kg", "VO2/kg", "V.O2.kg", "VO2.kg"))
  P      <- pick(c("P", "WR"))  # WR = Work Rate (HealthFit/MetasoftStudio)
  RER    <- pick("RER")
  VE_VO2 <- pick(c("V'E/V'O2", "VE/VO2", "V.E.V.O2", "VE.VO2", "V.E.V.O2."))
  HR     <- pick(c("HR", "HF"))
  # ── Zusätzliche Kanäle für 9-Felder-Grafik ──
  VCO2     <- pick(c("V'CO2", "VCO2", "V.CO2"))
  VE       <- pick(c("V'E",   "VE",   "V.E"))
  VE_VCO2  <- pick(c("V'E/V'CO2", "VE/VCO2", "V.E.V.CO2", "VE.VCO2"))
  VT_vol   <- pick(c("VT"))
  AF       <- pick(c("AF", "BF"))
  PetCO2   <- pick(c("PetCO2", "ExCO2"))
  PetO2    <- pick(c("PetO2",  "PEO2"))
  VO2_HR   <- pick(c("V'O2/HR", "VO2/HR", "V.O2.HR"))
  # ── Zusätzlich für Datenüberprüfung ──
  Speed    <- pick(c("v", "Speed", "Geschw.", "Geschwindigkeit", "km/h"))
  # Stage_raw: explizite Stufenspalte aus Quelldatei (sonst NA)
  stage_candidates <- intersect(c("Stufe", "Step", "Lev", "Level"), names(df))
  Stage_raw <- if (length(stage_candidates) > 0) {
    as.character(df[[stage_candidates[1]]])
  } else {
    rep(NA_character_, nrow(df))
  }

  tibble::tibble(
    time_s    = sec_rel,
    time_min  = sec_rel / 60,
    Phase     = as.character(phase),
    VO2abs    = VO2abs,
    VO2kg     = VO2kg,
    P         = P,
    RER       = RER,
    VE_VO2    = VE_VO2,
    HR        = HR,
    VCO2      = VCO2,
    VE        = VE,
    VE_VCO2   = VE_VCO2,
    VT_vol    = VT_vol,
    AF        = AF,
    PetCO2    = PetCO2,
    PetO2     = PetO2,
    VO2_HR    = VO2_HR,
    Speed     = Speed,
    Stage_raw = Stage_raw
  ) |>
    dplyr::mutate(
      VO2abs_smooth = safe_roll(VO2abs, smooth_n),
      VO2kg_smooth  = safe_roll(VO2kg,  smooth_n)
    )
}

# ============================================================
#   Format-Detection
# ============================================================
#  Liefert einen Format-Key aus der PARSER_REGISTRY zurück, basierend
#  auf Datei-Endung und Inhalts-Sniffing der ersten Zeilen.
#  - "metalyzer_xml"    : .xml im MetaLyzer/SpiroSoft-Schema
#  - "metalyzer_xlsx"   : .xlsx/.xls aus MetaLyzer/MetaSoft Studio (HealthFit)
#  - "zan_xlsx"         : .xlsx aus ZAN-Spirometriegerät (via spiro-Paket)
detect_format <- function(path, fname) {
  ext <- tolower(tools::file_ext(fname))

  if (ext == "xml") return("metalyzer_xml")

  if (ext %in% c("xlsx", "xls")) {
    sniff <- tryCatch(read_xlsx_raw(path, sheet = 1)$raw,
                      error = function(e) NULL)
    if (is.null(sniff)) return("metalyzer_xlsx")  # Fallback

    # Erste ~30 Zeilen als ein Such-Text zusammenführen
    n_peek <- min(40, nrow(sniff))
    flat <- paste(unlist(sniff[seq_len(n_peek), , drop = FALSE],
                         use.names = FALSE), collapse = "¦")
    flat <- tolower(flat)

    # MetaLyzer-Marker: deutsche Labels + typische Spalten
    metalyzer_hits <- c("geburtsdatum", "startzeit", "v'o2/kg",
                        "vo2/kg", "vo2.kg", "phase")
    has_metalyzer <- any(vapply(metalyzer_hits,
                                function(m) grepl(m, flat, fixed = TRUE),
                                logical(1)))

    if (has_metalyzer) return("metalyzer_xlsx")

    # ZAN: typische Marker (Geräte- oder Tabellenbezeichnung)
    zan_hits <- c("zan", "ergospir", "spirometer")
    has_zan <- any(vapply(zan_hits,
                          function(m) grepl(m, flat, fixed = TRUE),
                          logical(1)))
    if (has_zan) return("zan_xlsx")

    # Default: versuchen wir MetaLyzer (bisheriges Verhalten)
    return("metalyzer_xlsx")
  }

  stop("Unbekanntes Dateiformat: .", ext)
}

# ============================================================
#   Parser-Registry: jedes Format hat genau einen Parser, der
#   list(meta = ..., ts = ..., vt = ...) zurückgibt.
# ============================================================
PARSER_REGISTRY <- list(
  metalyzer_xml  = function(path, fname) parse_xml_spiro(path),
  metalyzer_xlsx = function(path, fname) parse_xlsx_spiro(path, fname),
  zan_xlsx       = function(path, fname) parse_zan_xlsx(path)
)

# ============================================================
#   Haupt-Lader: erkennt Format und dispatcht
# ============================================================
load_spiro_file <- function(path, fname) {
  fmt <- detect_format(path, fname)
  parser <- PARSER_REGISTRY[[fmt]]
  if (is.null(parser)) stop("Kein Parser für Format: ", fmt)
  parser(path, fname)
}

# ============================================================
#   ZAN-Parser: nutzt das spiro-Paket (optional, Suggested)
# ============================================================
parse_zan_xlsx <- function(path) {
  if (!requireNamespace("spiro", quietly = TRUE)) {
    stop(
      "ZAN-Dateien benötigen das Paket 'spiro'. Bitte installiere es einmalig:\n",
      "  remotes::install_github('ropensci/spiro@zan2')\n",
      "(siehe README → Unterstützte Dateiformate)"
    )
  }

  raw <- tryCatch(
    spiro::spiro(file = path, device = "zan2"),
    error = function(e) stop(
      "Die Datei konnte vom spiro-Paket nicht als ZAN-Format gelesen werden: ",
      conditionMessage(e))
  )

  adapt_spiro_to_internal(raw)
}

# ============================================================
#   Adapter: spiro-Objekt → internes list(meta, ts, vt)
# ============================================================
adapt_spiro_to_internal <- function(spiro_obj) {
  # spiro liefert tibble-ähnliches df mit attributes(info = list(weight, ...))
  info <- attr(spiro_obj, "info")
  if (is.null(info)) info <- list()

  meta <- list(
    "ID"               = info$id %||% info$participant %||% NA_character_,
    "Geburtsdatum"     = NA_character_,
    "Größe"            = if (!is.null(info$height))      paste0(info$height, " cm") else NA_character_,
    "Gewicht"          = if (!is.null(info$bodymass))    paste0(info$bodymass, " kg") else
                          if (!is.null(info$weight))     paste0(info$weight, " kg") else NA_character_,
    "Startzeit"        = info$start_time %||% NA_character_,
    "Dauer"            = NA_character_,
    "Temperatur"       = NA_character_,
    "Luftdruck"        = NA_character_,
    "Luftfeuchtigkeit" = NA_character_
  )

  # spiro-Tibble in Roh-DF konvertieren mit unseren erwarteten Spaltennamen,
  # dann durch build_timeseries() reichen (volle Glättungs-/Phase-Logik).
  df <- as.data.frame(spiro_obj, stringsAsFactors = FALSE)

  has <- function(nm) nm %in% names(df)
  pick <- function(nms) {
    for (n in nms) if (has(n)) return(df[[n]])
    rep(NA_real_, nrow(df))
  }

  vo2_abs <- pick(c("VO2", "V'O2"))
  vco2    <- pick(c("VCO2", "V'CO2"))
  ve      <- pick(c("VE",  "V'E"))
  hr      <- pick(c("HR", "HF"))
  load_w  <- pick(c("load", "WR", "P"))
  step    <- pick(c("step_number", "step", "protocol"))

  # Body weight für VO2/kg (kg)
  bw <- info$bodymass %||% info$weight %||% NA_real_
  vo2_kg <- if (is.finite(bw) && bw > 0) vo2_abs / bw else rep(NA_real_, nrow(df))

  # Zeit: spiro liefert i.d.R. Sekunden (numerisch). Wir wandeln in HH:MM:SS.
  t_sec <- pick(c("time", "t_s"))
  t_str <- vapply(t_sec, function(s) {
    if (!is.finite(s)) return(NA_character_)
    h <- floor(s / 3600); m <- floor((s %% 3600) / 60); sec <- s %% 60
    sprintf("%02d:%02d:%05.2f", as.integer(h), as.integer(m), sec)
  }, character(1))

  # Phase-Mapping anhand step/protocol (spiro: "rest", "warmup", "load", "recovery")
  phase_raw <- pick(c("phase", "section"))
  if (all(is.na(phase_raw))) {
    # Fallback: aus step ableiten
    phase_raw <- ifelse(step <= 0, "Erwärmung", "Belastung")
  }
  phase_raw <- tolower(as.character(phase_raw))
  phase_de <- dplyr::case_when(
    phase_raw %in% c("warmup", "warm-up", "warm up", "rest", "pre") ~ "Erwärmung",
    phase_raw %in% c("load", "exercise", "test")                    ~ "Belastung",
    phase_raw %in% c("recovery", "cooldown", "cool down", "post")   ~ "Erholung",
    TRUE                                                             ~ "Belastung"
  )

  raw_df <- data.frame(
    t           = t_str,
    Phase       = phase_de,
    P           = load_w,
    `V'O2`      = vo2_abs,
    `V'O2/kg`   = vo2_kg,
    `V'CO2`     = vco2,
    `V'E`       = ve,
    HR          = hr,
    RER         = ifelse(is.finite(vo2_abs) & vo2_abs > 0, vco2 / vo2_abs, NA_real_),
    `V'E/V'O2`  = ifelse(is.finite(vo2_abs) & vo2_abs > 0, ve   / vo2_abs * 1000, NA_real_),
    `V'E/V'CO2` = ifelse(is.finite(vco2)    & vco2    > 0, ve   / vco2    * 1000, NA_real_),
    Stufe       = step,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  list(meta = meta, ts = build_timeseries(raw_df), vt = NULL)
}

# ============================================================
#   Parameter-Extraktion
# ============================================================
extract_params <- function(spiro_data, fname) {
  meta <- spiro_data$meta
  ts   <- spiro_data$ts
  vt   <- spiro_data$vt  # VT1/VT2 aus Übersichtstabelle (kann NULL sein)

  parse_num <- function(x, rm_pat = NULL) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(NA_real_)
    if (!is.null(rm_pat)) x <- gsub(rm_pat, "", x)
    suppressWarnings(as.numeric(gsub(",", ".", trimws(x))))
  }

  weight <- parse_num(meta[["Gewicht"]],           " kg")
  height <- parse_num(meta[["Größe"]],              " cm")
  temp   <- parse_num(meta[["Temperatur"]],         "\u00b0C")
  press  <- parse_num(meta[["Luftdruck"]],          "mBar")
  humid  <- parse_num(meta[["Luftfeuchtigkeit"]],   "%")

    parse_birth_date <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NA)
    x <- trimws(x)
    out <- suppressWarnings(lubridate::dmy(x))
    if (is.na(out)) out <- suppressWarnings(lubridate::mdy(x))
    if (is.na(out)) out <- suppressWarnings(lubridate::ymd(x))
    out
  }

  parse_start_datetime <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NA)
    x <- trimws(x)

    out <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c(
        # DE
        "d.m.Y H:M:S", "d.m.Y H:M", "dmY HMS", "dmY HM",
        # EN / US mit AM/PM
        "m/d/Y I:M:S p", "m/d/Y I:M p",
        # EN / US 24h
        "m/d/Y H:M:S", "m/d/Y H:M"
      ),
      tz = "Europe/Berlin"
    ))

    if (length(out) == 0) return(NA)
    out[1]
  }

  birth <- parse_birth_date(meta[["Geburtsdatum"]])
  mdate <- parse_start_datetime(meta[["Startzeit"]])

  age <- if (!anyNA(c(birth, mdate)))
    as.numeric(round(lubridate::interval(birth, mdate) / lubridate::years(1), 0))
  else NA_real_

  dur_str <- tryCatch({
    p <- hms::parse_hms(meta[["Dauer"]])
    sprintf("%02d:%02d:%02d",
            as.integer(hms::hms_hours(p)),
            as.integer(hms::hms_minutes(p)),
            as.integer(hms::hms_seconds(p)))
  }, error = function(e) meta[["Dauer"]] %||% NA_character_)

  ID_file <- stringr::str_extract(basename(fname), "HF_\\d+")
  ID_meta <- stringr::str_extract(meta[["ID"]] %||% "", "HF_\\d+")
  ID        <- if (!is.na(ID_file)) ID_file else if (!is.na(ID_meta)) ID_meta else
                 tools::file_path_sans_ext(basename(fname))
  timepoint <- stringr::str_extract(basename(fname), "T\\d+")
  if (is.na(timepoint))
    timepoint <- stringr::str_extract(meta[["ID"]] %||% "", "T\\d+")

  # --- Peak-Parameter ---
  if (!is.null(ts) && nrow(ts) > 0 &&
      any(ts$Phase %in% c("Belastung", "Exercise"), na.rm = TRUE)) {

    bel <- dplyr::filter(ts, Phase %in% c("Belastung", "Exercise"))

    # VO2peak als höchstes 30s-Mittel innerhalb der Belastungsphase
    # (verhindert Verzerrung durch ins Cooldown ragende Fenster).
    pk_abs <- vo2_peak_window(ts, "VO2abs", window_sec = 30)
    pk_rel <- vo2_peak_window(ts, "VO2kg",  window_sec = 30)

    VO2peak_abs <- if (pk_abs$ok) round(pk_abs$value, 2) else
      if (any(is.finite(bel$VO2abs))) round(max(bel$VO2abs, na.rm = TRUE), 2) else NA_real_

    VO2peak_rel <- if (pk_rel$ok) round(pk_rel$value, 2) else
      if (any(is.finite(bel$VO2kg))) round(max(bel$VO2kg, na.rm = TRUE), 2) else NA_real_

    RERmax  <- if (any(is.finite(bel$RER)))    round(max(bel$RER,    na.rm = TRUE), 2) else NA_real_
    EQO2max <- if (any(is.finite(bel$VE_VO2))) round(max(bel$VE_VO2, na.rm = TRUE), 0) else NA_real_
    HRmax   <- if (any(is.finite(bel$HR)))     round(max(bel$HR,     na.rm = TRUE), 0) else NA_real_

    ppo_res  <- calc_ppo(bel)
    PPO      <- ppo_res$PPO
    ppo_flag <- ppo_res$interpolated

    # ── NEU: PPO in W/kg ─────────────────────────────────────
    PPO_wkg  <- if (is.finite(PPO) && is.finite(weight) && weight > 0)
      round(PPO / weight, 2) else NA_real_

  } else {
    VO2peak_rel <- VO2peak_abs <- RERmax <- EQO2max <- HRmax <- PPO <- NA_real_
    PPO_wkg  <- NA_real_
    ppo_flag <- NA_character_
    pk_abs <- pk_rel <- list(value = NA_real_, t_center_min = NA_real_,
                              t_start_min = NA_real_, t_end_min = NA_real_,
                              n_samples = 0L, ok = FALSE)
  }

  # ── VT1/VT2 Zeitpunkte aus V'O2 Werten interpolieren ──────
  vt1_time <- NA_real_
  vt2_time <- NA_real_
  if (!is.null(vt) && !is.null(ts) && nrow(ts) > 0) {
    # Flexibler Key-Lookup: suche V'O2 (absolut, nicht /kg)
    find_vt_key <- function(vt_list, suffix) {
      for (k in names(vt_list)) {
        k_norm <- tolower(gsub("[\u2019\u2018'`]", "'", k))
        if (grepl(paste0("^v'?o2", suffix), k_norm))
          return(vt_list[[k]])
      }
      NA_real_
    }
    vt1_vo2 <- find_vt_key(vt, "_vt1$")
    vt2_vo2 <- find_vt_key(vt, "_vt2$")

    bel_ts <- ts[ts$Phase %in% c("Belastung", "Exercise") & is.finite(ts$VO2abs), ]
    if (nrow(bel_ts) > 0) {
      if (is.finite(vt1_vo2 %||% NA)) {
        idx <- which.min(abs(bel_ts$VO2abs - vt1_vo2))
        if (length(idx) > 0) vt1_time <- bel_ts$time_min[idx]
      }
      if (is.finite(vt2_vo2 %||% NA)) {
        idx <- which.min(abs(bel_ts$VO2abs - vt2_vo2))
        if (length(idx) > 0) vt2_time <- bel_ts$time_min[idx]
      }
    }
  }

  list(
    ID = ID, Timepoint = timepoint,
    Date = mdate,
    Weight_kg = weight, Height_cm = height, Age_years = age,
    Temperature = temp, AirPressure = press, Humidity = humid,
    Duration = dur_str,
    PPO = PPO, PPO_wkg = PPO_wkg, PPO_interpolated = ppo_flag,
    VO2peak_abs = VO2peak_abs, VO2peak_rel = VO2peak_rel,
    # Automatischer Default-Verifikations-Fensterstand (höchstes
    # 30s-Mittel innerhalb Belastung). Wird vom VO2max-Verifikations-
    # Tab als Initialwert verwendet.
    VO2peak_auto = list(abs = pk_abs, rel = pk_rel, window_sec = 30),
    RERmax = RERmax, EQO2max = EQO2max, HRmax = HRmax,
    ts = ts,
    vt = vt,
    vt1_time = vt1_time,
    vt2_time = vt2_time
  )
}

# ============================================================
#   PPO
# ============================================================
calc_ppo <- function(df_bel) {
  df <- dplyr::filter(df_bel, is.finite(P), is.finite(time_s))
  if (nrow(df) == 0) return(list(PPO = NA_real_, interpolated = NA_character_))

  Pmax   <- max(df$P, na.rm = TRUE)
  Pmax_r <- round(Pmax, 0)
  at_max <- dplyr::filter(df, round(P, 0) == Pmax_r)
  if (nrow(at_max) == 0) return(list(PPO = NA_real_, interpolated = NA_character_))

  dt      <- suppressWarnings(median(diff(unique(df$time_s)), na.rm = TRUE))
  if (!is.finite(dt) || dt <= 0) dt <- 1
  ppo_dur <- (max(at_max$time_s) - min(at_max$time_s)) + dt

  if (ppo_dur >= 30)
    list(PPO = Pmax_r, interpolated = "No")
  else
    list(PPO = round((Pmax_r - 10) + (ppo_dur / 30) * 10, 0),
         interpolated = "Yes")
}

# ============================================================
#   Ausbelastungskriterien
# ============================================================
calc_exhaustion <- function(params) {
  HFcalc   <- if (!is.na(params$Age_years)) 200 - params$Age_years else NA
  crit_RER <- !is.na(params$RERmax)  && params$RERmax  > 1.1
  crit_EQ  <- !is.na(params$EQO2max) && params$EQO2max > 35
  crit_HF  <- !is.na(params$HRmax) && !is.na(HFcalc) && params$HRmax > HFcalc
  crit_sum <- sum(c(crit_RER, crit_EQ, crit_HF), na.rm = TRUE)

  tibble::tibble(
    Kriterium = c("RER > 1,1", "EQO2 > 35",
                  paste0("HF > ", if (!is.na(HFcalc)) HFcalc else "?"),
                  "Ausbelastet (≥ 2 Kriterien)"),
    Wert      = c(
      if (!is.na(params$RERmax))  round(params$RERmax, 2)  else NA_real_,
      if (!is.na(params$EQO2max)) round(params$EQO2max, 0) else NA_real_,
      if (!is.na(params$HRmax))   round(params$HRmax, 0)   else NA_real_,
      NA_real_
    ),
    Erfüllt = c(crit_RER, crit_EQ, crit_HF, crit_sum >= 2)
  )
}

# ============================================================
#   Vergleichs-Tabelle  –  inkl. PPO (W/kg)
# ============================================================
compare_table <- function(p1, p2) {
  lbl1 <- paste0(p1$ID %||% "M1", "_", p1$Timepoint %||% "T1")
  lbl2 <- paste0(p2$ID %||% "M2", "_", p2$Timepoint %||% "T2")

  params_def <- list(
    "PPO (W)"                 = list(v1 = p1$PPO,         v2 = p2$PPO),
    "PPO (W/kg)"              = list(v1 = p1$PPO_wkg,     v2 = p2$PPO_wkg),   # NEU
    "VO2peak abs (L/min)"     = list(v1 = p1$VO2peak_abs, v2 = p2$VO2peak_abs),
    "VO2peak rel (ml/min/kg)" = list(v1 = p1$VO2peak_rel, v2 = p2$VO2peak_rel),
    "RERmax"                  = list(v1 = p1$RERmax,       v2 = p2$RERmax),
    "EQO2max"                 = list(v1 = p1$EQO2max,      v2 = p2$EQO2max),
    "HRmax (bpm)"             = list(v1 = p1$HRmax,        v2 = p2$HRmax),
    "Gewicht (kg)"            = list(v1 = p1$Weight_kg,    v2 = p2$Weight_kg),
    "Alter (Jahre)"           = list(v1 = p1$Age_years,    v2 = p2$Age_years)
  )

  purrr::imap_dfr(params_def, function(vals, nm) {
    v1 <- suppressWarnings(as.numeric(vals$v1))
    v2 <- suppressWarnings(as.numeric(vals$v2))
    d  <- if (is.finite(v1) && is.finite(v2)) round(v2 - v1, 2) else NA_real_
    dp <- if (is.finite(v1) && is.finite(v2) && v1 != 0)
      round((v2 - v1) / v1 * 100, 1) else NA_real_
    tibble::tibble(
      Parameter = nm,
      M1        = if (is.finite(v1)) round(v1, 2) else NA_real_,
      M2        = if (is.finite(v2)) round(v2, 2) else NA_real_,
      Delta     = d,
      Delta_pct = dp
    )
  }) |>
    dplyr::rename(!!lbl1 := M1, !!lbl2 := M2,
                  `Δ` = Delta, `Δ%` = Delta_pct)
}