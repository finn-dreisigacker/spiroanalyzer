# Ausfuehren aus dem Repo-Root: Rscript tests/test_ppo.R
source("R/fct_data.R")
source("R/fct_steps.R")

# Stufentest: Start 50 W, +inc alle dur s, letzte Stufe nach last_s abgebrochen
mk <- function(inc, dur, n_full, last_s) {
  P <- c(rep(50 + inc * (seq_len(n_full) - 1), each = dur),
         rep(50 + inc * n_full, last_s))
  t <- seq_along(P) - 1
  data.frame(time_s = t, time_min = t / 60, Phase = "Belastung", P = P)
}

# 50 W / 3 min, letzte Stufe (250 W) nach 90 s abgebrochen -> 200 + 0.5 * 50
r <- calc_ppo(mk(50, 180, 4, 90))
stopifnot(r$PPO == 225, r$interpolated == "Yes")
# 10 W / 30 s (bisheriges Verhalten): 280 W fuer 18 s -> 270 + 18/30 * 10
r <- calc_ppo(mk(10, 30, 23, 18))
stopifnot(r$PPO == 276, r$interpolated == "Yes")
# letzte Stufe voll gefahren -> keine Interpolation
r <- calc_ppo(mk(20, 60, 5, 60))
stopifnot(r$PPO == 150, r$interpolated == "No")
# Rampe (1 W/s): keine Stufen erkennbar -> Pmax
ramp <- data.frame(time_s = 0:299, time_min = (0:299) / 60,
                   Phase = "Belastung", P = 50 + 0:299)
stopifnot(calc_ppo(ramp)$PPO == 349)

# Rampe mit zufaelligen kurzen Plateaus (wie MetaLyzer-Rampe 1 W / 3 s)
ramp2 <- data.frame(time_s = 0:599, time_min = (0:599) / 60,
                    Phase = "Belastung", P = 30 + (0:599) %/% 3)
ramp2$P[200:220] <- ramp2$P[200]                 # 21-s-Plateau mittendrin
stopifnot(is.null(detect_steps(ramp2)), calc_ppo(ramp2)$PPO == 229,
          calc_ppo(ramp2)$interpolated == "No")

cat("Alle PPO-Tests bestanden.\n")
