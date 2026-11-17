# Ausfuehren aus dem Repo-Root: Rscript tests/test_vt.R
source("R/fct_data.R")
source("R/fct_steps.R")
source("R/fct_vt.R")

in_phase <- function(ts, idx) ts$Phase[idx] %in% c("Belastung", "Exercise")

# ── 1) Synthetischer Stufentest mit bekannten Schwellen ─────
set.seed(1)
T1 <- 9; T2 <- 14
t  <- seq(0, 20 * 60 - 1) / 60
phase <- ifelse(t < 5, "Erwärmung", ifelse(t < 17, "Belastung", "Erholung"))
tb <- pmin(pmax(t - 5, 0), 12)
vo2 <- 1 + 0.25 * tb
vo2_t1 <- 1 + 0.25 * (T1 - 5)
vco2 <- ifelse(vo2 < vo2_t1, 0.9 * vo2, 0.9 * vo2_t1 + 1.4 * (vo2 - vo2_t1))
vco2_t2 <- vco2[which.min(abs(t - T2))]
ve <- ifelse(vco2 < vco2_t2, 25 * vco2, 25 * vco2_t2 + 45 * (vco2 - vco2_t2))
n <- length(t)
syn <- data.frame(
  time_s = t * 60, time_min = t, Phase = phase,
  VO2abs = vo2 + rnorm(n, sd = 0.03),
  VCO2   = vco2 + rnorm(n, sd = 0.03),
  VE     = ve + rnorm(n, sd = 0.8),
  stringsAsFactors = FALSE
)
v1 <- auto_vt1(syn); v2 <- auto_vt2(syn)
cat(sprintf("Synthetisch: VT1 = %.2f min (soll %g), VT2 = %.2f min (soll %g)\n",
            syn$time_min[v1], T1, syn$time_min[v2], T2))
stopifnot(!is.na(v1), !is.na(v2),
          in_phase(syn, v1), in_phase(syn, v2),
          abs(syn$time_min[v1] - T1) <= 0.75,
          abs(syn$time_min[v2] - T2) <= 0.75,
          syn$time_min[v2] > syn$time_min[v1])

# ── 2) Regression: Demo-Datei ───────────────────────────────
#    Geraeteschwellen V'O2 2.03 / 3.05 L/min -> 8.1 / 13.0 min
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
s <- load_spiro_file("inst/extdata/data_demo.xlsx", "data_demo.xlsx")
p <- extract_params(s, "data_demo.xlsx")
d1 <- auto_vt1(p$ts); d2 <- auto_vt2(p$ts)
cat(sprintf("Demo: VT1 = %.2f min, VT2 = %.2f min (Geraet: %.2f / %.2f)\n",
            p$ts$time_min[d1], p$ts$time_min[d2], p$vt1_time, p$vt2_time))
# Zeit aus Geraete-V'O2: Messwerte dort muessen zu den Geraetewerten passen
stopifnot(abs(p$vt1_time - 8.1) <= 0.3, abs(p$vt2_time - 13.0) <= 0.3,
          abs(vt_mean_at(p$ts, "VE", p$vt1_time) - 46) <= 3,
          abs(vt_mean_at(p$ts, "VE", p$vt2_time) - 87) <= 3)
stopifnot(!is.na(d1), !is.na(d2),
          in_phase(p$ts, d1), in_phase(p$ts, d2),
          p$ts$time_min[d2] > p$ts$time_min[d1],
          abs(p$ts$time_min[d1] - p$vt1_time) <= 1.5,
          abs(p$ts$time_min[d2] - p$vt2_time) <= 1.5)

# ── 3) find_breakpoint ohne neues Argument unveraendert ─────
#    Erwartungswert 60 vor der Aenderung berechnet (2. Segment flacher)
set.seed(7)
x <- 1:150
y <- ifelse(x < 60, 2 * x, 120 + 0.3 * (x - 60)) + rnorm(150)
stopifnot(identical(find_breakpoint(x, y, 15), 60L))

cat("Alle VT-Tests bestanden.\n")

# ── 4) Schwellen-Messwert = Mittel ueber +-15 s, nicht Einzelwert ─
tsm <- data.frame(time_min = (0:120) / 60, HR = 100)
tsm$HR[61] <- 200                                  # Ausreisser genau bei 1.0 min
stopifnot(abs(vt_mean_at(tsm, "HR", 1.0) - (100 * 30 + 200) / 31) < 1e-9,
          is.na(vt_mean_at(tsm, "HR", 99)),
          is.na(vt_mean_at(tsm, "fehlt", 1.0)))
cat("VT-Mittelwert-Test bestanden.\n")
