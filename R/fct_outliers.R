# ============================================================
#  fct_outliers.R
#  Robuste Ausreissererkennung fuer Breath-by-Breath-Spirodaten
#  - zeitbasiertes Rolling-Fenster (Hampel/MAD)
#  - Plausibilitaetschecks (Hardlimits, 50%-Sprungregel)
# ============================================================

# Plausibilitaets-Hardlimits pro logischer Variable
.outlier_hardlimits <- list(
  VO2abs = list(min = 0,    max = 8,    strict_zero = TRUE,
                label = "VO₂"),
  VCO2   = list(min = 0,    max = 8,    strict_zero = TRUE,
                label = "VCO₂"),
  VE     = list(min = 0,    max = 250,  strict_zero = TRUE,
                label = "VE"),
  RER    = list(min = 0.5,  max = 1.5,  strict_zero = FALSE,
                label = "RER"),
  HR     = list(min = 30,   max = 230,  strict_zero = FALSE,
                label = "HF"),
  AF     = list(min = 5,    max = 80,   strict_zero = FALSE,
                label = "AF"),
  VT_vol = list(min = 0.2,  max = 4.5,  strict_zero = FALSE,
                label = "VT"),
  P      = list(min = 0,    max = 800,  strict_zero = FALSE,
                label = "Leistung"),
  Speed  = list(min = 0,    max = 30,   strict_zero = FALSE,
                label = "Geschwindigkeit")
)

# Variablen-Reihenfolge fuer UI / Detektion
.outlier_variables <- c("VO2abs", "VCO2", "VE", "RER",
                         "HR", "AF", "VT_vol", "P", "Speed")

#' Zeitbasiertes Rolling Median + MAD
#'
#' Fuer jeden Punkt i wird der Median und die MAD ueber alle Punkte j
#' berechnet, fuer die |time_sec[j] - time_sec[i]| <= window_sec/2.
#' Variable Atemfrequenz fuehrt also zu variabler Sample-Anzahl je Fenster,
#' aber konstanter Zeitbreite.
#'
#' @param time_sec numerischer Zeitvektor in Sekunden (sortiert)
#' @param x numerischer Wertevektor
#' @param window_sec Fensterbreite in Sekunden (Gesamtbreite)
#' @param align "center" (Default; ±window_sec/2 um t_i)
#'              oder "right"/"trailing" (Fenster [t_i - window_sec, t_i])
#' @return list(median = ..., mad = ..., n = ...) gleicher Laenge wie x
rolling_median_mad_time <- function(time_sec, x, window_sec = 60,
                                     align = c("center", "right")) {
  align <- match.arg(align)
  n <- length(x)
  if (n == 0L)
    return(list(median = numeric(0), mad = numeric(0), n = integer(0)))

  med <- rep(NA_real_, n)
  mad_v <- rep(NA_real_, n)
  cnt <- rep(0L, n)

  # findInterval auf sortierter Zeit: O(n log n)
  ord_ok <- !any(diff(stats::na.omit(time_sec)) < 0, na.rm = TRUE)
  if (!ord_ok) {
    # Falls ungeordnet: einmal sortieren, am Ende zuruecksortieren
    o <- order(time_sec, na.last = NA)
    res <- rolling_median_mad_time(time_sec[o], x[o], window_sec, align)
    out_med <- rep(NA_real_, n); out_mad <- rep(NA_real_, n); out_n <- rep(0L, n)
    out_med[o] <- res$median; out_mad[o] <- res$mad; out_n[o] <- res$n
    return(list(median = out_med, mad = out_mad, n = out_n))
  }

  for (i in seq_len(n)) {
    if (!is.finite(time_sec[i])) next
    if (align == "center") {
      lo <- time_sec[i] - window_sec / 2
      hi <- time_sec[i] + window_sec / 2
    } else {
      lo <- time_sec[i] - window_sec
      hi <- time_sec[i]
    }
    # binaere Suche
    j_lo <- findInterval(lo, time_sec, left.open = TRUE) + 1L
    j_hi <- findInterval(hi, time_sec)
    if (j_hi < j_lo) next
    win <- x[j_lo:j_hi]
    win <- win[is.finite(win)]
    if (length(win) < 3L) next
    m <- stats::median(win)
    md <- stats::median(abs(win - m))
    med[i] <- m
    mad_v[i] <- md
    cnt[i] <- length(win)
  }

  list(median = med, mad = mad_v, n = cnt)
}

# Severity-Klassifizierung anhand robust_z
.classify_severity <- function(robust_z) {
  az <- abs(robust_z)
  out <- rep(NA_character_, length(robust_z))
  out[is.finite(az) & az >= 3.0] <- "candidate"
  out[is.finite(az) & az >= 4.5] <- "strong_candidate"
  out[is.finite(az) & az >= 6.0] <- "likely_artifact"
  out
}

# Plausibilitaetspruefung pro Variable: Hardlimits + 50%-Sprung
.plausibility_flags <- function(values, var_name) {
  hl <- .outlier_hardlimits[[var_name]]
  if (is.null(hl)) return(rep(NA_character_, length(values)))

  reasons <- rep(NA_character_, length(values))

  # Hardlimits
  bad_lo <- is.finite(values) & values < hl$min
  bad_hi <- is.finite(values) & values > hl$max
  reasons[bad_lo] <- paste0(hl$label, " < ", hl$min)
  reasons[bad_hi] <- paste0(hl$label, " > ", hl$max)

  # Strict-Zero (negative oder 0 bei VO2/VCO2/VE)
  if (isTRUE(hl$strict_zero)) {
    bad_zero <- is.finite(values) & values <= 0
    reasons[bad_zero] <- paste0(hl$label, " ≤ 0")
  }

  # 50%-Sprungregel zwischen benachbarten Punkten
  if (length(values) >= 2L) {
    prev_v <- c(NA_real_, values[-length(values)])
    rel_jump <- abs(values - prev_v) / pmax(abs(prev_v), 1e-9)
    bad_jump <- is.finite(rel_jump) & is.finite(prev_v) & prev_v != 0 &
                rel_jump > 0.5 & is.na(reasons)
    reasons[bad_jump] <- paste0(hl$label, " Sprung > 50 %")
  }

  reasons
}

#' Robuste Ausreissererkennung fuer Spiro-Daten
#'
#' Wendet pro Variable ein zeitbasiertes Rolling Median/MAD an, klassifiziert
#' nach Schweregrad und ergaenzt Plausibilitaetschecks. Erzeugt eine Long-Format-
#' Kandidatentabelle. Fehlende Spalten werden uebersprungen.
#'
#' @param data tibble mit time_s und Variablenspalten
#' @param time_col Spaltenname der Zeit (Sekunden)
#' @param variables Vektor von Variablen, die geprueft werden sollen.
#'                  Default = .outlier_variables (was gefunden wird).
#' @param window_sec Rolling-Fenster (Sekunden)
#' @param stage_col optional Spaltenname mit Stufenzuordnung (fuer Output)
#' @return tibble mit row_id, time_sec, stage, variable, value,
#'         rolling_median, rolling_MAD, robust_z, reason, severity,
#'         action ("keep"), comment ("")
detect_spiro_outliers <- function(data,
                                  time_col   = "time_s",
                                  variables  = NULL,
                                  window_sec = 60,
                                  stage_col  = NULL,
                                  align      = c("center", "right")) {
  align <- match.arg(align)

  if (!is.data.frame(data) || nrow(data) == 0L)
    return(.empty_outlier_table())

  if (!time_col %in% names(data))
    return(.empty_outlier_table())

  if (is.null(variables))
    variables <- .outlier_variables
  variables <- intersect(variables, names(data))

  # Variablen, die komplett NA sind, weglassen
  variables <- variables[vapply(variables, function(v)
    any(is.finite(data[[v]])), logical(1))]

  if (length(variables) == 0L)
    return(.empty_outlier_table())

  time_sec <- as.numeric(data[[time_col]])
  row_ids  <- seq_len(nrow(data))
  stages   <- if (!is.null(stage_col) && stage_col %in% names(data))
    as.character(data[[stage_col]]) else rep(NA_character_, nrow(data))

  pieces <- lapply(variables, function(v) {
    vals <- as.numeric(data[[v]])

    rm <- rolling_median_mad_time(time_sec, vals,
                                   window_sec = window_sec, align = align)
    denom <- 1.4826 * rm$mad
    robust_z <- ifelse(is.finite(denom) & denom > 0,
                       (vals - rm$median) / denom, NA_real_)

    sev <- .classify_severity(robust_z)
    plaus <- .plausibility_flags(vals, v)

    # Vereinige Severity- und Plausibilitaets-Kandidaten
    is_cand <- !is.na(sev) | !is.na(plaus)

    # Reason zusammensetzen
    reason <- rep(NA_character_, length(vals))
    z_reason <- ifelse(is.finite(robust_z),
                       sprintf("|z| = %.2f", abs(robust_z)),
                       NA_character_)
    reason[!is.na(sev)] <- z_reason[!is.na(sev)]
    has_both <- !is.na(sev) & !is.na(plaus)
    reason[has_both] <- paste0(reason[has_both], "; ", plaus[has_both])
    only_plaus <- is.na(sev) & !is.na(plaus)
    reason[only_plaus] <- plaus[only_plaus]

    # Severity fuer reine Plausibilitaetstreffer (ohne MAD-Z) auf "candidate"
    sev[only_plaus] <- "candidate"

    if (!any(is_cand)) return(NULL)

    idx <- which(is_cand)
    tibble::tibble(
      row_id         = row_ids[idx],
      time_sec       = time_sec[idx],
      stage          = stages[idx],
      variable       = v,
      value          = vals[idx],
      rolling_median = rm$median[idx],
      rolling_MAD    = rm$mad[idx],
      robust_z       = robust_z[idx],
      reason         = reason[idx],
      severity       = sev[idx],
      action         = "keep",
      comment        = ""
    )
  })

  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) return(.empty_outlier_table())

  out <- dplyr::bind_rows(pieces)
  out <- dplyr::arrange(out, time_sec, variable)
  out
}

.empty_outlier_table <- function() {
  tibble::tibble(
    row_id         = integer(0),
    time_sec       = numeric(0),
    stage          = character(0),
    variable       = character(0),
    value          = numeric(0),
    rolling_median = numeric(0),
    rolling_MAD    = numeric(0),
    robust_z       = numeric(0),
    reason         = character(0),
    severity       = character(0),
    action         = character(0),
    comment        = character(0)
  )
}

#' Anzeigelabel fuer Variablenname (DE)
outlier_variable_label <- function(v) {
  hl <- .outlier_hardlimits[[v]]
  if (is.null(hl)) v else hl$label
}

#' Liefert die im Datensatz tatsaechlich vorhandenen Outlier-Variablen
#' in der Standardreihenfolge.
available_outlier_variables <- function(data) {
  vars <- intersect(.outlier_variables, names(data))
  vars[vapply(vars, function(v) any(is.finite(data[[v]])), logical(1))]
}

#' Rolling Median + MAD auf bereinigter Variable.
#'
#' Wendet exclude_row_ids (Werte werden auf NA gesetzt -> rausgefiltert) und
#' replacements (Werte werden ersetzt) an, *bevor* der Rolling-Median
#' berechnet wird. Wird fuer die Hintergrund-Linie und das MAD-Band im
#' Outlier-Plot verwendet, damit Artefakte den Median nicht verzerren.
#'
#' @param time_sec numerischer Zeitvektor in Sekunden
#' @param x numerischer Wertevektor (Rohwerte der Variable)
#' @param exclude_row_ids integer-Vektor mit Indices, die ausgeschlossen werden
#' @param replacement_map named numeric vector: row_id (als character) -> value
#' @param window_sec Fensterbreite in Sekunden
#' @param align "center" oder "right"
rolling_median_mad_time_cleaned <- function(time_sec, x,
                                            exclude_row_ids = integer(0),
                                            replacement_map = numeric(0),
                                            window_sec = 60,
                                            align = c("center", "right")) {
  align <- match.arg(align)
  vals <- x
  # Replacements anwenden
  if (length(replacement_map) > 0L) {
    rid <- suppressWarnings(as.integer(names(replacement_map)))
    ok  <- !is.na(rid) & rid >= 1L & rid <= length(vals)
    if (any(ok)) vals[rid[ok]] <- as.numeric(replacement_map[ok])
  }
  # Excludes -> NA setzen, damit sie aus is.finite(win) fallen
  if (length(exclude_row_ids) > 0L) {
    eid <- as.integer(exclude_row_ids)
    eid <- eid[eid >= 1L & eid <= length(vals)]
    if (length(eid) > 0L) vals[eid] <- NA_real_
  }
  rolling_median_mad_time(time_sec, vals, window_sec = window_sec, align = align)
}
