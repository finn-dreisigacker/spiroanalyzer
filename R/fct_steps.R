# ============================================================
#  fct_steps.R  –  Stufen-Erkennung & Zusammenfassung
#
#  Pro Phase eine Zeile:
#    - WarmUp        (eine Zeile)
#    - Belastung     (mehrere Zeilen, S1 .. Sn)
#    - Cooldown      (eine Zeile)
#
#  Substrat-Oxidation
#    Achten & Jeukendrup (2003), Int J Sports Med 24(8):603–608.
#      FO  [g/min] = 1.695 * V'O2 - 1.701 * V'CO2          (paper 2002 → 1.695)
#                  = 1.695..  jeweilige Quellen variieren leicht.
#      CHO [g/min] = 4.585 * V'CO2 - 3.226 * V'O2
#    -> Hinweis: Userpaper nutzt 1.695 als Frayn-Variante.
#       Wir nehmen die im Anwendungstext genannten Koeffizienten.
#
#  Energetische Aufteilung %FO / %CHO
#    Péronnet & Massicotte (1991), Can J Sport Sci 16(1):23–29.
#      Linear interpoliert über RER zwischen 0.707 (100% Fett)
#      und 1.000 (100% CHO). Aus den substrat-spezifischen
#      kalorischen Äquivalenten resultiert die %-Aufteilung
#      der Energieabgabe.
#
#  Energieumsatz (Energy Turnover, ET)
#    Kroidl R, Schwarz S, Lehnigk B, Fritsch J. (2015).
#    Kursbuch Spiroergometrie, Thieme.
#      ET [kcal/min] = (V'O2 [L/min] *
#                      (%FO * 19.6 + %CHO * 21.1)) / 4.18
#      mit %FO / %CHO als Anteile (0..1).
#
#  Maximale Fettoxidation (MFO)
#    Achten & Jeukendrup (2003): Polynom 2. Grades durch
#    FO-Werte über die Belastungsstufen, MFO = Maximum-Wert.
# ============================================================


# ── Substrat-Oxidation (Achten & Jeukendrup 2003) ─────────────
calc_FO_CHO <- function(VO2, VCO2) {
  if (!is.finite(VO2) || !is.finite(VCO2))
    return(list(FO = NA_real_, CHO = NA_real_))
  FO  <- 1.695 * VO2  - 1.701 * VCO2
  CHO <- 4.585 * VCO2 - 3.226 * VO2
  list(FO = max(0, FO), CHO = max(0, CHO))
}

# ── Anteile %FO / %CHO (Péronnet & Massicotte 1991, RER-basiert) ─
calc_substrate_fraction <- function(RER) {
  if (!is.finite(RER)) return(list(FO_frac = NA_real_, CHO_frac = NA_real_))
  if (RER <= 0.707) return(list(FO_frac = 1.0, CHO_frac = 0.0))
  if (RER >= 1.000) return(list(FO_frac = 0.0, CHO_frac = 1.0))
  cho <- (RER - 0.707) / (1.000 - 0.707)
  list(FO_frac = 1 - cho, CHO_frac = cho)
}

# ── Energy Turnover (Kroidl et al. 2015) ──────────────────────
calc_ET <- function(VO2, FO_frac, CHO_frac) {
  if (!is.finite(VO2) || !is.finite(FO_frac) || !is.finite(CHO_frac))
    return(NA_real_)
  # ET [kcal/min] = (VO2 [L/min] * (%FO*19.6 + %CHO*21.1)) / 4.18
  (VO2 * (FO_frac * 19.6 + CHO_frac * 21.1)) / 4.18
}


# ============================================================
#  Stufen-Erkennung innerhalb der Belastungsphase
# ============================================================
detect_steps <- function(ts, min_step_sec = 15, tol = 5) {
  if (is.null(ts) || nrow(ts) < 5 || !"P" %in% names(ts) ||
      !"time_min" %in% names(ts) || !"Phase" %in% names(ts))
    return(NULL)

  bel <- ts |>
    dplyr::filter(Phase %in% c("Belastung", "Exercise"),
                  is.finite(P), is.finite(time_min)) |>
    dplyr::arrange(time_min)
  if (nrow(bel) < 5) return(NULL)

  P  <- bel$P
  tm <- bel$time_min

  P_round <- round(P)
  r <- rle(P_round)
  ends   <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)

  plateaus <- data.frame(
    P_set        = r$values,
    n            = r$lengths,
    t_start_min  = tm[starts],
    t_end_min    = tm[ends]
  )
  plateaus$duration_sec <- (plateaus$t_end_min - plateaus$t_start_min) * 60

  # Konsekutive Plateaus mit nahezu gleicher Power zusammenfassen
  if (nrow(plateaus) > 1) {
    keep <- rep(TRUE, nrow(plateaus))
    i <- 1
    while (i < nrow(plateaus)) {
      j <- i + 1
      while (j <= nrow(plateaus) &&
             abs(plateaus$P_set[j] - plateaus$P_set[i]) < tol) {
        plateaus$t_end_min[i] <- plateaus$t_end_min[j]
        plateaus$n[i]         <- plateaus$n[i] + plateaus$n[j]
        keep[j] <- FALSE
        j <- j + 1
      }
      i <- j
    }
    plateaus <- plateaus[keep, , drop = FALSE]
    plateaus$duration_sec <- (plateaus$t_end_min - plateaus$t_start_min) * 60
  }

  plateaus <- plateaus[plateaus$duration_sec >= min_step_sec, , drop = FALSE]
  if (nrow(plateaus) == 0) return(NULL)
  plateaus$step_no <- seq_len(nrow(plateaus))
  plateaus[, c("step_no", "t_start_min", "t_end_min",
               "duration_sec", "P_set")]
}


# ============================================================
#  Zusammenfassung pro Zeile (WarmUp + Belastung-Stufen + Cooldown)
# ============================================================
build_step_summary <- function(ts, weight_kg = NA_real_,
                                window_sec = 30,
                                min_step_sec = 15) {
  if (is.null(ts) || nrow(ts) < 5) return(NULL)
  has <- function(col) col %in% names(ts)

  # Mittelwert über die letzten window_sec Sekunden des Bereichs
  mean_in_range <- function(col, t0, t1) {
    if (!has(col)) return(NA_real_)
    win_start <- max(t0, t1 - window_sec / 60)
    sel <- ts$time_min >= win_start & ts$time_min <= t1 &
           is.finite(ts[[col]])
    if (sum(sel) == 0) return(NA_real_)
    mean(ts[[col]][sel], na.rm = TRUE)
  }

  # Gemeinsamer Zeilen-Builder
  build_row <- function(label_de, type, no, t0, t1, P_set = NA_real_) {
    P_mean   <- if (is.finite(P_set)) P_set else mean_in_range("P", t0, t1)
    VO2      <- mean_in_range("VO2abs", t0, t1)
    VCO2     <- mean_in_range("VCO2",   t0, t1)
    VE       <- mean_in_range("VE",     t0, t1)
    HR       <- mean_in_range("HR",     t0, t1)
    RER      <- mean_in_range("RER",    t0, t1)

    fc       <- calc_FO_CHO(VO2, VCO2)
    frac     <- calc_substrate_fraction(RER)
    ET       <- calc_ET(VO2, frac$FO_frac, frac$CHO_frac)

    VO2_kg   <- if (is.finite(VO2) && is.finite(weight_kg) && weight_kg > 0)
      VO2 / weight_kg * 1000 else NA_real_

    data.frame(
      type         = type,
      no           = no,
      label        = label_de,
      t_start_min  = t0,
      t_end_min    = t1,
      duration_sec = round((t1 - t0) * 60),
      P            = P_mean,
      VO2          = VO2,
      VO2_kg       = VO2_kg,
      VCO2         = VCO2,
      VE           = VE,
      HR           = HR,
      RER          = RER,
      FO_g         = fc$FO,
      CHO_g        = fc$CHO,
      FO_pct       = frac$FO_frac  * 100,
      CHO_pct      = frac$CHO_frac * 100,
      ET_kcal_min  = ET,
      ET_kJ_min    = if (is.finite(ET)) ET * 4.184 else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  out <- list()
  ts <- ts[order(ts$time_min), ]

  # ── WarmUp ─────────────────────────────────────────────
  wu <- ts[ts$Phase %in% c("Erwärmung", "Warmup", "Warm Up", "Warm-Up"), ]
  if (nrow(wu) >= 5)
    out[[length(out)+1]] <- build_row("WarmUp", "WarmUp", NA_integer_,
      min(wu$time_min, na.rm = TRUE), max(wu$time_min, na.rm = TRUE))

  # ── Belastung-Stufen ───────────────────────────────────
  steps <- detect_steps(ts, min_step_sec = min_step_sec)
  if (!is.null(steps) && nrow(steps) > 0) {
    for (i in seq_len(nrow(steps))) {
      s <- steps[i, ]
      out[[length(out)+1]] <- build_row(
        sprintf("Stufe %d (%d W)", s$step_no, round(s$P_set)),
        "Belastung", s$step_no,
        s$t_start_min, s$t_end_min, P_set = s$P_set)
    }
  }

  # ── Cooldown ───────────────────────────────────────────
  cd <- ts[ts$Phase %in% c("Erholung", "Recovery", "Cool Down", "Cooldown"), ]
  if (nrow(cd) >= 5)
    out[[length(out)+1]] <- build_row("Cooldown", "Cooldown", NA_integer_,
      min(cd$time_min, na.rm = TRUE), max(cd$time_min, na.rm = TRUE))

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}


# ============================================================
#  MFO – Maximum Fat Oxidation, Fatmax-Power
#
#  Polynom 2. Grades durch FO-Werte über die Belastungsstufen.
#  x = Leistung P [W], y = FO [g/min]
#  MFO     = Funktionswert am Scheitel der Parabel
#  Fatmax  = Power [W] am Scheitel
#  Achten J. & Jeukendrup A.E. (2003) Int J Sports Med 24:603-608.
#  Maunder E. et al. (2018) Front Physiol 9:599.
# ============================================================
calc_MFO <- function(summary_df) {
  empty <- list(MFO = NA_real_, Fatmax_W = NA_real_,
                fit = NULL, x = NULL, y = NULL,
                fit_x = NULL, fit_y = NULL,
                reason = "Nicht genug Belastungs-Stufen", ok = FALSE)
  if (is.null(summary_df) || nrow(summary_df) < 3) return(empty)
  bel <- summary_df[summary_df$type == "Belastung" &
                    is.finite(summary_df$P) &
                    is.finite(summary_df$FO_g), ]
  if (nrow(bel) < 3) return(empty)

  fit <- tryCatch(
    lm(FO_g ~ P + I(P^2), data = bel),
    error = function(e) NULL)
  if (is.null(fit)) {
    empty$reason <- "Polynom-Anpassung fehlgeschlagen"
    return(empty)
  }
  cf <- coef(fit)
  a <- as.numeric(cf["I(P^2)"])
  b <- as.numeric(cf["P"])
  c0 <- as.numeric(cf["(Intercept)"])

  # Fit-Kurve über den ganzen Power-Bereich
  px <- seq(min(bel$P, na.rm = TRUE),
            max(bel$P, na.rm = TRUE), length.out = 80)
  py <- a * px^2 + b * px + c0

  if (!is.finite(a) || a >= 0)
    return(list(MFO = NA_real_, Fatmax_W = NA_real_,
                fit = fit, x = bel$P, y = bel$FO_g,
                fit_x = px, fit_y = py,
                reason = "Parabel ist nicht nach unten geöffnet",
                ok = FALSE))
  vx <- -b / (2 * a)
  in_range <- is.finite(vx) && vx >= min(bel$P) && vx <= max(bel$P)
  mfo_val <- a * vx^2 + b * vx + c0

  list(
    MFO       = mfo_val,
    Fatmax_W  = vx,
    fit       = fit,
    x         = bel$P,
    y         = bel$FO_g,
    fit_x     = px,
    fit_y     = py,
    reason    = if (in_range) NA_character_ else "Scheitelpunkt liegt außerhalb des Power-Bereichs",
    ok        = in_range
  )
}
