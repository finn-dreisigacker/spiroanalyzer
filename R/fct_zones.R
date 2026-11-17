# ============================================================
#   Trainingsbereiche nach Hansen et al. (2022), Eur J Prev Cardiol
#   29(1), 230-245. Grenzen in % V'O2peak und % HFmax.
# ============================================================
TZ_LEVELS <- data.frame(
  name   = c("Niedrig, leichte Belastung", "Moderat", "Hoch", "Sehr hoch"),
  vo2_lo = c(NA, 40, 70, 85), vo2_hi = c(40, 69, 85, NA),
  hr_lo  = c(NA, 55, 75, 90), hr_hi  = c(55, 74, 90, NA),
  rpe    = c("10–11", "12–13", "14–16", "17–19"),
  zone   = c("Aerob", "Aerob", "Aerob + Laktat", "Aerob + Laktat + anaerob"),
  stringsAsFactors = FALSE)

# Leistung [W] im Test, wenn das geglättete V'O2/kg in der Belastung
# erstmals den Wert erreicht (NA, wenn nie erreicht).
tz_power_at_vo2 <- function(ts, vo2kg) {
  if (!is.finite(vo2kg) || is.null(ts)) return(NA_real_)
  bel <- ts[ts$Phase %in% "Belastung" & is.finite(ts$VO2kg_smooth), , drop = FALSE]
  i <- which(bel$VO2kg_smooth >= vo2kg)[1]
  if (is.na(i)) return(NA_real_)
  interp_vt_power(ts, bel$time_min[i])
}

# Liefert je Stufe die absoluten Grenzen (V'O2 ml/min/kg, HF 1/min, Watt).
calc_training_zones <- function(p) {
  z <- TZ_LEVELS
  pk <- p$VO2peak_rel %||% NA_real_; hf <- p$HRmax %||% NA_real_
  z$vo2_abs_lo <- pk * z$vo2_lo / 100; z$vo2_abs_hi <- pk * z$vo2_hi / 100
  z$hr_abs_lo  <- hf * z$hr_lo  / 100; z$hr_abs_hi  <- hf * z$hr_hi  / 100
  z$w_lo <- vapply(z$vo2_abs_lo, function(v) tz_power_at_vo2(p$ts, v), numeric(1))
  z$w_hi <- vapply(z$vo2_abs_hi, function(v) tz_power_at_vo2(p$ts, v), numeric(1))
  z
}

# "<19,3" / "19,3–33,3" / ">41,0"
tz_range <- function(lo, hi, digits = 0) {
  f <- function(x) formatC(x, format = "f", digits = digits, decimal.mark = ",")
  if (!is.finite(lo) && !is.finite(hi)) return("–")
  if (!is.finite(lo)) return(paste0("<", f(hi)))
  if (!is.finite(hi)) return(paste0(">", f(lo)))
  paste0(f(lo), "–", f(hi))
}
