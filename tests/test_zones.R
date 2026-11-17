for (f in list.files("R", full.names = TRUE)) source(f)
`%||%` <- function(a, b) if (is.null(a)) b else a

s <- load_spiro_file("inst/extdata/data_demo.xlsx", "data_demo.xlsx")
p <- extract_params(s, "data_demo.xlsx")
z <- calc_training_zones(p)
print(z[, c("name", "vo2_abs_lo", "vo2_abs_hi", "hr_abs_lo", "hr_abs_hi", "w_lo", "w_hi")])

stopifnot(nrow(z) == 4,
          isTRUE(all.equal(z$vo2_abs_hi[1], p$VO2peak_rel * 0.40)),
          isTRUE(all.equal(z$hr_abs_lo[4], p$HRmax * 0.90)),
          # Watt steigen mit der Intensität
          all(diff(stats::na.omit(c(z$w_hi[1], z$w_hi[2], z$w_hi[3]))) >= 0),
          tz_range(NA, 19.28, 1) == "<19,3",
          tz_range(33.7, 41, 1) == "33,7–41,0",
          tz_range(41, NA, 1) == ">41,0")
cat("Trainingsbereiche-Tests bestanden.\n")
