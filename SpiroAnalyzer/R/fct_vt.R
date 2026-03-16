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
find_breakpoint <- function(x, y, min_seg = 10) {
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
    rss <- sum(fit1$residuals^2) + sum(fit2$residuals^2)
    if (rss < best_rss) {
      best_rss <- rss
      best_k   <- k
    }
  }

  if (is.na(best_k)) return(NA_integer_)
  idx[best_k]
}

# ── ExCO2-Breakpoint: Minimum der geglaetteten ExCO2-Kurve ──
find_exco2_breakpoint <- function(exco2, smooth_n = 15) {
  s <- safe_roll(exco2, smooth_n)
  ok <- is.finite(s)
  if (sum(ok) < 5) return(NA_integer_)
  idx <- which(ok)
  # Minimum der ExCO2-Kurve = VT1
  min_pos <- idx[which.min(s[idx])]
  min_pos
}

# ── ExVE-Breakpoint: Minimum der geglaetteten ExVE-Kurve ────
find_exve_breakpoint <- function(exve, smooth_n = 15) {
  s <- safe_roll(exve, smooth_n)
  ok <- is.finite(s)
  if (sum(ok) < 5) return(NA_integer_)
  idx <- which(ok)
  min_pos <- idx[which.min(s[idx])]
  min_pos
}

# ── Automatische VT1-Bestimmung ─────────────────────────────
#    Kombiniert ExCO2-Minimum und V-Slope (VO2 vs VCO2)
#    Gibt den Index in der Zeitreihe zurueck
auto_vt1 <- function(ts, smooth_n = 15) {
  if (is.null(ts) || nrow(ts) == 0) return(NA_integer_)

  bel <- which(ts$Phase %in% c("Belastung", "Exercise",
                                 "Erwärmung", "Warmup", "Warm Up"))
  if (length(bel) < 20) bel <- seq_len(nrow(ts))

  vo2  <- ts$VO2abs[bel]
  vco2 <- ts$VCO2[bel]

  # Methode 1: ExCO2 Minimum
  exco2 <- calc_exco2(vo2, vco2)
  idx_exco2 <- find_exco2_breakpoint(exco2, smooth_n)

  # Methode 2: V-Slope (VO2 vs VCO2)
  idx_vslope <- find_breakpoint(vo2, vco2, min_seg = max(10, length(bel) %/% 8))

  # Konsens: Durchschnitt der beiden Methoden, oder die eine die klappt
  candidates <- c(idx_exco2, idx_vslope)
  candidates <- candidates[is.finite(candidates)]
  if (length(candidates) == 0) return(NA_integer_)

  # Zurueck auf globalen Index
  local_idx <- round(mean(candidates))
  if (local_idx < 1 || local_idx > length(bel)) return(NA_integer_)
  bel[local_idx]
}

# ── Automatische VT2-Bestimmung ─────────────────────────────
#    Kombiniert ExVE-Minimum und V-Slope (VCO2 vs VE)
auto_vt2 <- function(ts, smooth_n = 15) {
  if (is.null(ts) || nrow(ts) == 0) return(NA_integer_)

  bel <- which(ts$Phase %in% c("Belastung", "Exercise",
                                 "Erwärmung", "Warmup", "Warm Up"))
  if (length(bel) < 20) bel <- seq_len(nrow(ts))

  vco2 <- ts$VCO2[bel]
  ve   <- ts$VE[bel]

  # Methode 1: ExVE Minimum
  exve <- calc_exve(ve, vco2)
  idx_exve <- find_exve_breakpoint(exve, smooth_n)

  # Methode 2: V-Slope (VCO2 vs VE)
  idx_vslope <- find_breakpoint(vco2, ve, min_seg = max(10, length(bel) %/% 8))

  candidates <- c(idx_exve, idx_vslope)
  candidates <- candidates[is.finite(candidates)]
  if (length(candidates) == 0) return(NA_integer_)

  local_idx <- round(mean(candidates))
  if (local_idx < 1 || local_idx > length(bel)) return(NA_integer_)
  bel[local_idx]
}

# ── VT-Zusammenfassungstabelle ──────────────────────────────
#    Erstellt eine Tabelle mit allen Parametern an VT1 und VT2
build_vt_table <- function(ts, vt1_idx, vt2_idx, params = NULL,
                            vt1_method = "auto", vt2_method = "auto",
                            vt1_confirmed = FALSE, vt2_confirmed = FALSE) {
  get_val <- function(col, idx) {
    if (is.na(idx) || idx < 1 || idx > nrow(ts)) return(NA_real_)
    if (!col %in% names(ts)) return(NA_real_)
    ts[[col]][idx]
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
      Power_W        = get_val("P", idx),
      VO2_abs_Lmin   = round(vo2abs %||% NA, 3),
      VO2_rel_mlkgmin = if (is.finite(vo2abs) && is.finite(weight) && weight > 0)
        round(vo2abs * 1000 / weight, 1) else round(get_val("VO2kg", idx), 1),
      VCO2_Lmin      = round(get_val("VCO2", idx), 3),
      VE_Lmin        = round(get_val("VE", idx), 1),
      HR_bpm         = get_val("HR", idx),
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
    vo2_v  <- ts$VO2abs[vt1_idx]
    vco2_v <- ts$VCO2[vt1_idx]
    vt1_row$ExCO2 <- if (is.finite(vo2_v) && is.finite(vco2_v) && vo2_v > 0)
      round(calc_exco2(vo2_v, vco2_v), 3) else NA_real_
  } else {
    vt1_row$ExCO2 <- NA_real_
  }
  vt1_row$ExVE <- NA_real_

  if (!is.na(vt2_idx) && vt2_idx >= 1 && vt2_idx <= nrow(ts)) {
    ve_v   <- ts$VE[vt2_idx]
    vco2_v <- ts$VCO2[vt2_idx]
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
