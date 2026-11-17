# ============================================================
#  fct_vt.R  --  VT1/VT2 Berechnung (Excess CO2, Excess VE)
# ============================================================

# ── ExCO2: Excess CO2 fuer VT1-Bestimmung ────────────────────
#    ExCO2 = (VCO2^2 / VO2) - VCO2
calc_exco2 <- function(vo2, vco2) {
  ok <- is.finite(vo2) & is.finite(vco2) & vo2 > 0
  out <- rep(NA_real_, length(vo2))
  out[ok] <- (vco2[ok]^2 / vo2[ok]) - vco2[ok]
  out
}

# ── ExVE: Excess VE fuer VT2-Bestimmung ─────────────────────
#    ExVE = (VE^2 / VCO2) - VE
calc_exve <- function(ve, vco2) {
  ok <- is.finite(ve) & is.finite(vco2) & vco2 > 0
  out <- rep(NA_real_, length(ve))
  out[ok] <- (ve[ok]^2 / vco2[ok]) - ve[ok]
  out
}

# ── V-Slope Breakpoint: 2-Segment Regression ────────────────
#    Findet den Knickpunkt in y vs x via minimales RSS
#    Gibt den Index des Breakpoints zurueck
#    require_steeper = TRUE: nur Knicke mit steilerem 2. Segment
#    (Schwellen sind immer ein Anstieg der Steigung)
find_breakpoint <- function(x, y, min_seg = 10, require_steeper = FALSE) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2 * min_seg) return(NA_integer_)

  idx <- which(ok)
  x_ok <- x[idx]
  y_ok <- y[idx]
  n <- length(x_ok)

  best_rss <- Inf
  best_k   <- NA_integer_

  search_from <- min_seg
  search_to   <- n - min_seg
  if (search_from >= search_to) return(NA_integer_)

  for (k in search_from:search_to) {
    fit1 <- tryCatch(lm.fit(cbind(1, x_ok[1:k]), y_ok[1:k]),
                     error = function(e) NULL)
    fit2 <- tryCatch(lm.fit(cbind(1, x_ok[(k+1):n]), y_ok[(k+1):n]),
                     error = function(e) NULL)
    if (is.null(fit1) || is.null(fit2)) next
    if (require_steeper &&
        !isTRUE(fit2$coefficients[2] > fit1$coefficients[2])) next
    rss <- sum(fit1$residuals^2) + sum(fit2$residuals^2)
    if (rss < best_rss) {
      best_rss <- rss
      best_k   <- k
    }
  }

  if (is.na(best_k)) return(NA_integer_)
  idx[best_k]
}

# ── Excess-Breakpoint: Knick der geglaetteten Excess-Kurve ──
#    Schwelle = Beginn des systematischen Anstiegs ueber die Zeit,
#    nicht das Minimum (das liegt oft in Rauschen/Aufwaermphase)
find_excess_breakpoint <- function(t, excess, smooth_n = 15, min_seg = 10) {
  find_breakpoint(t, safe_roll(excess, smooth_n), min_seg = min_seg,
                  require_steeper = TRUE)
}

# Max. Zeitabstand (min), bis zu dem Excess- und V-Slope-Kandidat als
# uebereinstimmend gelten; darueber gewinnt V-Slope (Primaermethode,
# Beaver, Wasserman & Whipp 1986)
VT_AGREE_TOL_MIN <- 1.0

# ── Kandidaten kombinieren (lokale Indizes in rows) ─────────
#    Vergleich ueber Zeit, da Atemzugdaten nicht aequidistant sind
combine_vt_candidates <- function(ts, rows, idx_excess, idx_vslope) {
  if (is.na(idx_vslope) && is.na(idx_excess)) return(NA_integer_)
  if (is.na(idx_excess)) return(rows[idx_vslope])
  if (is.na(idx_vslope)) return(rows[idx_excess])
  t_ex <- ts$time_min[rows[idx_excess]]
  t_vs <- ts$time_min[rows[idx_vslope]]
  if (abs(t_ex - t_vs) > VT_AGREE_TOL_MIN) return(rows[idx_vslope])
  t_mean <- (t_ex + t_vs) / 2
  rows[which.min(abs(ts$time_min[rows] - t_mean))]
}

# ── Automatische VT1-Bestimmung ─────────────────────────────
#    Kombiniert ExCO2-Knick und V-Slope (VO2 vs VCO2)
#    Gibt den Index in der Zeitreihe zurueck
auto_vt1 <- function(ts, smooth_n = 15) {
  if (is.null(ts) || nrow(ts) == 0) return(NA_integer_)

  # Nur Belastung: in der Erwaermung liegen sonst Schein-Minima/-Knicke
  bel <- which(ts$Phase %in% c("Belastung", "Exercise"))
  if (length(bel) < 20) bel <- seq_len(nrow(ts))

  # V-Slope gilt nur unterhalb des RCP (Beaver et al. 1986): die
  # Hyperventilation darueber zieht den Knick sonst nach hinten.
  rcp <- find_breakpoint(ts$VCO2[bel], ts$VE[bel],
                         min_seg = max(10, length(bel) %/% 8),
                         require_steeper = TRUE)
  if (!is.na(rcp) && rcp >= 20) bel <- bel[seq_len(rcp)]

  vo2  <- ts$VO2abs[bel]
  vco2 <- ts$VCO2[bel]
  min_seg <- max(10, length(bel) %/% 8)

  idx_exco2 <- find_excess_breakpoint(ts$time_min[bel],
                                       calc_exco2(vo2, vco2),
                                       smooth_n, min_seg)
  idx_vslope <- find_breakpoint(vo2, vco2, min_seg = min_seg,
                                require_steeper = TRUE)

  combine_vt_candidates(ts, bel, idx_exco2, idx_vslope)
}

# ── Automatische VT2-Bestimmung ─────────────────────────────
#    Kombiniert ExVE-Knick und V-Slope (VCO2 vs VE),
#    gesucht nur nach VT1 (VT2 > VT1 physiologisch zwingend)
auto_vt2 <- function(ts, smooth_n = 15) {
  if (is.null(ts) || nrow(ts) == 0) return(NA_integer_)

  # Nur Belastung: in der Erwaermung liegen sonst Schein-Minima/-Knicke
  bel <- which(ts$Phase %in% c("Belastung", "Exercise"))
  if (length(bel) < 20) bel <- seq_len(nrow(ts))
  vt1 <- auto_vt1(ts, smooth_n)
  if (!is.na(vt1)) {
    bel <- bel[bel > vt1]
    if (length(bel) < 20) return(NA_integer_)
  }

  vco2 <- ts$VCO2[bel]
  ve   <- ts$VE[bel]
  min_seg <- max(10, length(bel) %/% 8)

  idx_exve   <- find_excess_breakpoint(ts$time_min[bel],
                                       calc_exve(ve, vco2),
                                       smooth_n, min_seg)
  idx_vslope <- find_breakpoint(vco2, ve, min_seg = min_seg,
                                require_steeper = TRUE)

  combine_vt_candidates(ts, bel, idx_exve, idx_vslope)
}

# ── Interpolierte Leistung an einem (Schwellen-)Zeitpunkt ───
#    Liegt der Zeitpunkt innerhalb einer Stufe, wird die Leistung
#    linear interpoliert (Treppe -> Rampe):
#      P = letzte_volle_Stufe + Inkrement * (Zeit_in_Stufe / Stufendauer)
#    mit letzte_volle_Stufe = P_aktuelle_Stufe - Inkrement.
#    Rückgabe als ganze Zahl (W); Fallback: gemessene Leistung.
interp_vt_power <- function(ts, t_min, min_step_sec = 15) {
  if (is.null(ts) || !is.finite(t_min) ||
      !all(c("time_min", "P") %in% names(ts))) return(NA_real_)

  meas_p <- {
    i <- which.min(abs(ts$time_min - t_min))
    if (length(i) == 1 && is.finite(ts$P[i])) ts$P[i] else NA_real_
  }

  steps <- tryCatch(detect_steps(ts, min_step_sec = min_step_sec),
                    error = function(e) NULL)
  if (is.null(steps) || nrow(steps) == 0) return(round(meas_p, 0))

  # Stufe, die t_min enthält (sonst letzte Stufe mit Start <= t_min)
  in_step <- which(steps$t_start_min <= t_min & steps$t_end_min >= t_min)
  k <- if (length(in_step) >= 1) in_step[1] else {
    before <- which(steps$t_start_min <= t_min)
    if (length(before) == 0) return(round(meas_p, 0))
    max(before)
  }

  p_cur <- steps$P_set[k]
  increment <- if (nrow(steps) >= 2)
    stats::median(diff(steps$P_set), na.rm = TRUE) else NA_real_
  if (!is.finite(increment) || increment <= 0) return(round(p_cur, 0))

  # nominale Stufendauer = Median der erkannten Stufen
  stage_dur_min <- stats::median(steps$duration_sec, na.rm = TRUE) / 60
  if (!is.finite(stage_dur_min) || stage_dur_min <= 0)
    stage_dur_min <- steps$t_end_min[k] - steps$t_start_min[k]
  if (!is.finite(stage_dur_min) || stage_dur_min <= 0) return(round(p_cur, 0))

  frac <- (t_min - steps$t_start_min[k]) / stage_dur_min
  frac <- max(0, min(1, frac))
  round((p_cur - increment) + increment * frac, 0)
}

# Halbe Fensterbreite (s) fuer Messwerte an einer Schwelle: ein
# einzelner Atemzug ist bei Breath-by-Breath-Daten Rauschen.
VT_READ_HALF_SEC <- 15

# ── Messwert an einem Schwellen-Zeitpunkt (Mittel ueber +-15 s) ─
vt_mean_at <- function(ts, col, t_min, half_sec = VT_READ_HALF_SEC) {
  if (is.null(ts) || !is.finite(t_min) ||
      !all(c(col, "time_min") %in% names(ts))) return(NA_real_)
  v <- ts[[col]][abs(ts$time_min - t_min) <= half_sec / 60]
  v <- v[is.finite(v)]
  if (length(v) == 0) NA_real_ else mean(v)
}

# ── VT-Zusammenfassungstabelle ──────────────────────────────
#    Erstellt eine Tabelle mit allen Parametern an VT1 und VT2
build_vt_table <- function(ts, vt1_idx, vt2_idx, params = NULL,
                            vt1_method = "auto", vt2_method = "auto",
                            vt1_confirmed = FALSE, vt2_confirmed = FALSE) {
  get_val <- function(col, idx) {
    if (is.na(idx) || idx < 1 || idx > nrow(ts)) return(NA_real_)
    if (!col %in% names(ts)) return(NA_real_)
    if (col %in% c("time_s", "time_min")) return(ts[[col]][idx])
    vt_mean_at(ts, col, ts$time_min[idx])
  }

  weight <- if (!is.null(params)) params$Weight_kg else NA_real_

  make_row <- function(idx, label, method, confirmed) {
    vo2abs <- get_val("VO2abs", idx)
    hr_val <- get_val("HR", idx)
    o2puls <- if (is.finite(vo2abs) && is.finite(hr_val) && hr_val > 0)
      round(vo2abs * 1000 / hr_val, 1) else NA_real_

    tibble::tibble(
      Schwelle       = label,
      Zeit_s         = get_val("time_s", idx),
      Zeit_min       = round(get_val("time_min", idx), 2),
      Power_W        = interp_vt_power(ts, get_val("time_min", idx)),
      VO2_abs_Lmin   = round(vo2abs %||% NA, 3),
      VO2_rel_mlkgmin = if (is.finite(vo2abs) && is.finite(weight) && weight > 0)
        round(vo2abs * 1000 / weight, 1) else round(get_val("VO2kg", idx), 1),
      VCO2_Lmin      = round(get_val("VCO2", idx), 3),
      VE_Lmin        = round(get_val("VE", idx), 1),
      HR_bpm         = round(get_val("HR", idx), 0),
      RER            = round(get_val("RER", idx), 2),
      O2_Puls_ml     = o2puls,
      VE_VO2         = round(get_val("VE_VO2", idx), 1),
      VE_VCO2        = round(get_val("VE_VCO2", idx), 1),
      PetO2_mmHg     = round(get_val("PetO2", idx), 0),
      PetCO2_mmHg    = round(get_val("PetCO2", idx), 0),
      Methode        = method,
      Bestaetigt     = confirmed
    )
  }

  vt1_row <- make_row(vt1_idx, "VT1", vt1_method, vt1_confirmed)
  vt2_row <- make_row(vt2_idx, "VT2", vt2_method, vt2_confirmed)

  # ExCO2 / ExVE Werte ergaenzen
  if (!is.na(vt1_idx) && vt1_idx >= 1 && vt1_idx <= nrow(ts)) {
    vo2_v  <- get_val("VO2abs", vt1_idx)
    vco2_v <- get_val("VCO2",   vt1_idx)
    vt1_row$ExCO2 <- if (is.finite(vo2_v) && is.finite(vco2_v) && vo2_v > 0)
      round(calc_exco2(vo2_v, vco2_v), 3) else NA_real_
  } else {
    vt1_row$ExCO2 <- NA_real_
  }
  vt1_row$ExVE <- NA_real_

  if (!is.na(vt2_idx) && vt2_idx >= 1 && vt2_idx <= nrow(ts)) {
    ve_v   <- get_val("VE",   vt2_idx)
    vco2_v <- get_val("VCO2", vt2_idx)
    vt2_row$ExVE <- if (is.finite(ve_v) && is.finite(vco2_v) && vco2_v > 0)
      round(calc_exve(ve_v, vco2_v), 3) else NA_real_
  } else {
    vt2_row$ExVE <- NA_real_
  }
  vt2_row$ExCO2 <- NA_real_

  dplyr::bind_rows(vt1_row, vt2_row)
}

# ── VT-Index zu time_min konvertieren ────────────────────────
vt_idx_to_time <- function(ts, idx) {
  if (is.null(ts) || is.na(idx) || idx < 1 || idx > nrow(ts)) return(NA_real_)
  ts$time_min[idx]
}
