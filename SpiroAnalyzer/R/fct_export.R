# ============================================================
#  fct_export.R  --  Excel-Export (openxlsx)
# ============================================================

#' Exportiert alle Ergebnisse als mehrseitige Excel-Datei
#'
#' @param params  extract_params() Ergebnis
#' @param vt_table  VT-Zusammenfassungstabelle (tibble)
#' @param file  Zielpfad
export_xlsx <- function(params, vt_table = NULL, file,
                        vt1_time = NA_real_, vt2_time = NA_real_) {
  wb <- openxlsx::createWorkbook()

  # ── 1) CPET-Zusammenfassung ────────────────────────────────
  openxlsx::addWorksheet(wb, "Zusammenfassung")

  # VT-Werte am Index nachschlagen (fuer Zusammenfassung)
  vt1_vo2  <- NA_real_; vt1_hr  <- NA_real_; vt1_pwr  <- NA_real_
  vt2_vo2  <- NA_real_; vt2_hr  <- NA_real_; vt2_pwr  <- NA_real_
  vt1_vo2r <- NA_real_; vt2_vo2r <- NA_real_
  ts <- params$ts
  wt <- params$Weight_kg

  if (!is.null(ts) && nrow(ts) > 0 && is.finite(vt1_time)) {
    i1 <- which.min(abs(ts$time_min - vt1_time))
    if (length(i1) == 1) {
      vt1_vo2 <- ts$VO2abs[i1]
      vt1_hr  <- ts$HR[i1]
      vt1_pwr <- if ("P" %in% names(ts)) ts$P[i1] else NA_real_
      vt1_vo2r <- if (is.finite(vt1_vo2) && is.finite(wt) && wt > 0)
        round(vt1_vo2 * 1000 / wt, 1) else NA_real_
    }
  }
  if (!is.null(ts) && nrow(ts) > 0 && is.finite(vt2_time)) {
    i2 <- which.min(abs(ts$time_min - vt2_time))
    if (length(i2) == 1) {
      vt2_vo2 <- ts$VO2abs[i2]
      vt2_hr  <- ts$HR[i2]
      vt2_pwr <- if ("P" %in% names(ts)) ts$P[i2] else NA_real_
      vt2_vo2r <- if (is.finite(vt2_vo2) && is.finite(wt) && wt > 0)
        round(vt2_vo2 * 1000 / wt, 1) else NA_real_
    }
  }

  summary_df <- tibble::tibble(
    Parameter = c("ID", "Zeitpunkt", "Datum", "Gewicht (kg)",
      "Groesse (cm)", "Alter (Jahre)", "Dauer",
      "PPO (W)", "PPO (W/kg)", "PPO interpoliert",
      "VO2peak abs (L/min)", "VO2peak rel (ml/min/kg)",
      "RERmax", "EQO2max", "HRmax",
      "VT1 Zeit (min)", "VT1 VO2 abs (L/min)", "VT1 VO2 rel (ml/min/kg)",
      "VT1 HR (bpm)", "VT1 Power (W)",
      "VT2 Zeit (min)", "VT2 VO2 abs (L/min)", "VT2 VO2 rel (ml/min/kg)",
      "VT2 HR (bpm)", "VT2 Power (W)"),
    Wert = c(
      params$ID %||% NA, params$Timepoint %||% NA,
      if (!is.na(params$Date)) format(params$Date, "%d.%m.%Y %H:%M") else NA,
      params$Weight_kg, params$Height_cm, params$Age_years,
      params$Duration %||% NA,
      params$PPO, params$PPO_wkg, params$PPO_interpolated %||% NA,
      params$VO2peak_abs, params$VO2peak_rel,
      params$RERmax, params$EQO2max, params$HRmax,
      if (is.finite(vt1_time)) round(vt1_time, 2) else NA,
      if (is.finite(vt1_vo2)) round(vt1_vo2, 3) else NA,
      vt1_vo2r, vt1_hr, vt1_pwr,
      if (is.finite(vt2_time)) round(vt2_time, 2) else NA,
      if (is.finite(vt2_vo2)) round(vt2_vo2, 3) else NA,
      vt2_vo2r, vt2_hr, vt2_pwr)
  )
  openxlsx::writeDataTable(wb, "Zusammenfassung", summary_df)

  # ── 2) Zeitreihen-Daten ────────────────────────────────────
  if (!is.null(params$ts) && nrow(params$ts) > 0) {
    openxlsx::addWorksheet(wb, "CPET-Daten")
    openxlsx::writeDataTable(wb, "CPET-Daten", params$ts)
  }

  # ── 3) VT-Ergebnisse ──────────────────────────────────────
  if (!is.null(vt_table) && nrow(vt_table) > 0) {
    openxlsx::addWorksheet(wb, "VT-Ergebnisse")
    openxlsx::writeDataTable(wb, "VT-Ergebnisse", vt_table)
  }

  # ── 4) Ausbelastung ───────────────────────────────────────
  exhaust <- tryCatch(calc_exhaustion(params), error = function(e) NULL)
  if (!is.null(exhaust)) {
    openxlsx::addWorksheet(wb, "Ausbelastung")
    openxlsx::writeDataTable(wb, "Ausbelastung", exhaust)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}