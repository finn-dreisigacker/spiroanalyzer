# ============================================================
#  fct_plots.R  –  ggplot2 Visualisierungen  (v8)
#
#  Änderungen v8:
#    - PPO-Linien: xstart = Beginn der letzten Stufe (min statt max)
#    - PPO-Linien: xstart nutzt immer Pmax (nicht interpolierten PPO)
#      → Linie erscheint auch bei interpoliertem PPO korrekt
#    - is_smaller: which.min() statt == min() → kein Inf-Warning mehr
#    - Text: oberhalb/unterhalb der Linie an der sekundären Y-Achse
# ============================================================

# ============================================================
#  EINZELANSICHT  –  1 Messung
# ============================================================
single_ts_plot <- function(ts, smooth_n = 20, label = "") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)

  keep_phases <- c("Warmup", "Erwärmung", "Belastung", "Cooldown", "Erholung")

  ts <- ts |>
    dplyr::filter(Phase %in% keep_phases | is.na(Phase)) |>
    dplyr::arrange(time_min) |>
    dplyr::mutate(
      VO2kg_smooth = safe_roll(VO2kg, smooth_n)
    )

  # ── Achsenskalierung ────────────────────────────────────────
  power_min  <- 20
  p_max_data <- max(ts$P, na.rm = TRUE)
  if (!is.finite(p_max_data)) p_max_data <- 300
  power_max    <- ceiling((p_max_data + 100) / 20) * 20
  power_breaks <- seq(power_min, power_max, by = 20)

  vo2_vals <- ts$VO2kg_smooth[is.finite(ts$VO2kg_smooth)]
  if (length(vo2_vals) < 5) vo2_vals <- ts$VO2kg[is.finite(ts$VO2kg)]
  if (length(vo2_vals) < 5) return(NULL)

  y_breaks <- pretty(vo2_vals)
  y_lim    <- range(y_breaks, finite = TRUE)
  y_base   <- y_lim[1]
  f2       <- (y_lim[2] - y_base) / (power_max - power_min)

  ts_plot <- ts |>
    dplyr::mutate(
      P_scaled = dplyr::if_else(
        is.finite(P),
        (pmin(pmax(P, power_min), power_max) - power_min) * f2 + y_base,
        NA_real_
      )
    )

  x_max  <- ceiling(max(ts_plot$time_min, na.rm = TRUE))
  dy_txt <- (y_lim[2] - y_lim[1]) * 0.02

  # ── VO2peak aus Belastungsphase ─────────────────────────────
  bel <- dplyr::filter(ts_plot, Phase == "Belastung")

  vo2_peak <- bel |>
    dplyr::filter(is.finite(VO2kg_smooth)) |>
    dplyr::slice_max(VO2kg_smooth, n = 1, with_ties = FALSE)

  # ── PPO intern berechnen ────────────────────────────────────
  ppo_res  <- calc_ppo(bel)
  ppo_val  <- ppo_res$PPO

  # xstart = Beginn der höchsten Stufe (min der Zeitpunkte bei Pmax)
  # Nutze immer Pmax (round), nicht den ggf. interpolierten PPO-Wert
  ppo_xstart <- if (is.finite(ppo_val) && any(is.finite(bel$P))) {
    pmax_r <- round(max(bel$P, na.rm = TRUE), 0)
    v <- bel$time_min[is.finite(bel$P) & round(bel$P, 0) == pmax_r]
    if (length(v) > 0) suppressWarnings(min(v, na.rm = TRUE)) else NA_real_
  } else NA_real_

  y_ppo <- if (is.finite(ppo_val))
    (pmin(pmax(ppo_val, power_min), power_max) - power_min) * f2 + y_base
  else NA_real_

  # ── Plot ────────────────────────────────────────────────────
  ggplot2::ggplot(ts_plot, ggplot2::aes(x = time_min)) +

    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_base, ymax = P_scaled),
      fill = "#0072B2", colour = NA, alpha = 0.6, na.rm = TRUE) +

    ggplot2::geom_line(
      ggplot2::aes(y = VO2kg_smooth),
      colour = "#D55E00", linewidth = 0.8, na.rm = TRUE) +

    {if (nrow(vo2_peak) > 0) {
      # Label y clampen damit kein geom_text-Warning entsteht
      vp <- vo2_peak |>
        dplyr::mutate(
          seg_y  = pmax(y_lim[1], pmin(VO2kg_smooth, y_lim[2])),
          txt_y  = seg_y + dy_txt,
          txt_y  = dplyr::if_else(txt_y > y_lim[2], seg_y - dy_txt, txt_y),
          vjst   = dplyr::if_else(txt_y >= seg_y, 0L, 1L),
          lab    = paste0("VO\u2082peak: ",
                          format(round(VO2kg_smooth, 1), decimal.mark = ","))
        )
      list(
        ggplot2::geom_segment(
          data = vp,
          ggplot2::aes(x = 0, xend = time_min, y = seg_y, yend = seg_y),
          linetype = "dashed", colour = "#D55E00", linewidth = 0.6),
        ggplot2::geom_text(
          data = vp,
          ggplot2::aes(x = 0, y = txt_y, label = lab, vjust = vjst),
          hjust = 0, size = 3, colour = "#D55E00")
      )
    } else NULL} +

    # PPO-Linie: beginnt bei Beginn der Maximalleistungsstufe,
    # endet an der rechten (sekundären) Y-Achse; Text daneben
    {if (is.finite(ppo_val) && is.finite(ppo_xstart)) {
      # y_ppo innerhalb y_lim clampen; Text oberhalb der Linie (oder darunter wenn zu hoch)
      y_ppo_c  <- pmax(y_lim[1], pmin(y_ppo, y_lim[2]))
      txt_y    <- y_ppo_c + dy_txt
      txt_vjust <- 0
      if (txt_y > y_lim[2]) { txt_y <- y_ppo_c - dy_txt; txt_vjust <- 1 }
      list(
        ggplot2::annotate("segment",
          x = ppo_xstart, xend = x_max,
          y = y_ppo_c, yend = y_ppo_c,
          linetype = "dashed", colour = "#0072B2", linewidth = 0.6),
        ggplot2::annotate("text",
          x = x_max, y = txt_y,
          label = paste0("PPO: ", round(ppo_val, 0), " W"),
          hjust = 1, vjust = txt_vjust, size = 3, colour = "#0072B2")
      )
    } else list()} +

    ggplot2::scale_y_continuous(
      name = expression(VO[2]*" ["*ml%.%min^{-1}%.%kg^{-1}*"]"),
      breaks = y_breaks, limits = y_lim,
      sec.axis = ggplot2::sec_axis(
        ~ (. - y_base) / f2 + power_min,
        name = "Power (W)", breaks = power_breaks)) +
    ggplot2::scale_x_continuous(
      name = "Zeit [min]",
      breaks = seq(0, x_max, 1), limits = c(0, x_max)) +
    ggplot2::labs(title = label) +
    .spiro_theme()
}


# ============================================================
#  VERGLEICHSPLOT  –  T0 vs T10
# ============================================================
compare_plot <- function(ts1, ts2,
                         label1   = "Messung 1",
                         label2   = "Messung 2",
                         smooth_n = 20) {
  if (is.null(ts1) || is.null(ts2)) return(NULL)

  keep_phases <- c("Warmup", "Erwärmung", "Belastung", "Cooldown", "Erholung")

  prep <- function(ts, lbl, n) {
    ts |>
      dplyr::filter(Phase %in% keep_phases | is.na(Phase)) |>
      dplyr::arrange(time_min) |>
      dplyr::mutate(
        Test         = lbl,
        VO2kg_smooth = safe_roll(VO2kg, n)
      )
  }

  d1 <- prep(ts1, label1, smooth_n)
  d2 <- prep(ts2, label2, smooth_n)
  d  <- dplyr::bind_rows(d1, d2) |>
    dplyr::mutate(Test = factor(Test, levels = c(label1, label2)))

  # ── Achsenskalierung ────────────────────────────────────────
  power_min  <- 20
  p_max_data <- max(d$P, na.rm = TRUE)
  if (!is.finite(p_max_data)) p_max_data <- 300
  power_max    <- ceiling((p_max_data + 100) / 20) * 20
  power_breaks <- seq(power_min, power_max, by = 20)

  vo2_vals <- d$VO2kg_smooth[is.finite(d$VO2kg_smooth)]
  if (length(vo2_vals) < 5) vo2_vals <- d$VO2kg[is.finite(d$VO2kg)]

  y_breaks <- pretty(vo2_vals)
  y_lim    <- range(y_breaks, finite = TRUE)
  y_base   <- y_lim[1]
  f2       <- (y_lim[2] - y_base) / (power_max - power_min)

  d_plot <- d |>
    dplyr::mutate(
      P_scaled = dplyr::if_else(
        is.finite(P),
        (pmin(pmax(P, power_min), power_max) - power_min) * f2 + y_base,
        NA_real_
      )
    )

  x_max  <- ceiling(max(d_plot$time_min, na.rm = TRUE))
  dy_txt <- (y_lim[2] - y_lim[1]) * 0.02

  # ── VO2peak pro Test ────────────────────────────────────────
  vo2_peaks <- d_plot |>
    dplyr::filter(Phase == "Belastung", is.finite(VO2kg_smooth)) |>
    dplyr::group_by(Test) |>
    dplyr::slice_max(VO2kg_smooth, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      x_peak     = time_min,
      lab_vo2    = paste0("VO\u2082peak: ",
                          format(round(VO2kg_smooth, 1), decimal.mark = ",")),
      # which.min() statt == min() → kein Inf-Warning bei n=1
      is_smaller = dplyr::row_number() == which.min(VO2kg_smooth),
      nudge_y    = dplyr::if_else(is_smaller, -dy_txt, dy_txt),
      vjust_lab  = dplyr::if_else(is_smaller, 1, 0)
    )

  # ── PPO pro Test ────────────────────────────────────────────
  # xstart = Beginn der höchsten Stufe (min Zeitpunkt bei Pmax)
  # Pmax wird immer zum Suchen verwendet, auch wenn PPO interpoliert ist
  ppo_peaks <- d_plot |>
    dplyr::group_by(Test) |>
    dplyr::group_modify(~ {
      bel <- dplyr::filter(.x, Phase == "Belastung")
      res <- calc_ppo(bel)

      ppo_t <- if (is.finite(res$PPO) && any(is.finite(bel$P))) {
        pmax_r <- round(max(bel$P, na.rm = TRUE), 0)
        v <- bel$time_min[is.finite(bel$P) & round(bel$P, 0) == pmax_r]
        if (length(v) > 0) suppressWarnings(min(v, na.rm = TRUE)) else NA_real_
      } else NA_real_

      tibble::tibble(PPO = res$PPO, ppo_xstart = ppo_t)
    }) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(PPO), is.finite(ppo_xstart)) |>
    dplyr::mutate(
      y_ppo   = (pmin(pmax(PPO, power_min), power_max) - power_min) * f2 + y_base,
      lab_ppo = paste0("PPO: ", round(PPO, 0), " W"),
      # which.min() statt == min() → kein Inf-Warning bei n=1
      is_smaller = dplyr::row_number() == which.min(PPO),
      nudge_y    = dplyr::if_else(is_smaller, -dy_txt, dy_txt),
      vjust_lab  = dplyr::if_else(is_smaller, 1, 0)
    )

  cols <- c("#0072B2", "#D55E00")
  names(cols) <- c(label1, label2)

  ggplot2::ggplot(d_plot, ggplot2::aes(x = time_min)) +

    # Power-Ribbons
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_base, ymax = P_scaled,
                   group = Test, fill = Test),
      colour = NA, alpha = 0.6, na.rm = TRUE) +

    # VO2/kg Linien  →  nur diese erscheinen in der Legende
    ggplot2::geom_line(
      ggplot2::aes(y = VO2kg_smooth, colour = Test),
      linewidth = 0.8, na.rm = TRUE) +

    # VO2peak pro Test: x=0 → x_peak
    {if (nrow(vo2_peaks) > 0) list(
      ggplot2::geom_segment(
        data = vo2_peaks,
        ggplot2::aes(x = 0, xend = x_peak,
                     y = VO2kg_smooth, yend = VO2kg_smooth,
                     colour = Test),
        linetype = "dashed", linewidth = 0.6,
        inherit.aes = FALSE,
        show.legend = FALSE),
      ggplot2::geom_text(
        data = vo2_peaks |>
          dplyr::mutate(
            txt_y2 = pmax(y_lim[1] + dy_txt*0.1,
                          pmin(VO2kg_smooth + nudge_y, y_lim[2] - dy_txt*0.1))
          ),
        ggplot2::aes(x = 0, y = txt_y2,
                     label = lab_vo2, colour = Test, vjust = vjust_lab),
        hjust = 0, size = 3,
        inherit.aes = FALSE,
        show.legend = FALSE)
    )} +

    # PPO pro Test: ppo_xstart -> x_max (sekundaere Y-Achse)
    # y-Positionen innerhalb y_lim geclamped -> kein geom_text-Warning
    {if (nrow(ppo_peaks) > 0) {
      pp <- ppo_peaks |>
        dplyr::mutate(
          y_seg   = pmax(y_lim[1], pmin(y_ppo, y_lim[2])),
          txt_y   = y_seg + nudge_y,
          txt_y   = dplyr::if_else(txt_y > y_lim[2], y_seg - abs(nudge_y), txt_y),
          txt_y   = dplyr::if_else(txt_y < y_lim[1], y_seg + abs(nudge_y), txt_y),
          vjust_f = dplyr::if_else(txt_y >= y_seg, 0L, 1L)
        )
      list(
        ggplot2::geom_segment(
          data = pp,
          ggplot2::aes(x = ppo_xstart, xend = x_max,
                       y = y_seg, yend = y_seg, colour = Test),
          linetype = "dashed", linewidth = 0.6,
          inherit.aes = FALSE,
          show.legend = FALSE),
        ggplot2::geom_text(
          data = pp,
          ggplot2::aes(x = x_max, y = txt_y,
                       label = lab_ppo, colour = Test, vjust = vjust_f),
          hjust = 1, size = 3,
          inherit.aes = FALSE,
          show.legend = FALSE)
      )
    } else NULL} +

    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_fill_manual(  values = cols) +
    ggplot2::scale_y_continuous(
      name = expression(VO[2]*" ["*ml%.%min^{-1}%.%kg^{-1}*"]"),
      breaks = y_breaks, limits = y_lim,
      sec.axis = ggplot2::sec_axis(
        ~ (. - y_base) / f2 + power_min,
        name = "Power (W)", breaks = power_breaks)) +
    ggplot2::scale_x_continuous(
      name   = "Zeit [min]",
      breaks = seq(0, x_max, 1), limits = c(0, x_max)) +
    .spiro_theme() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      fill   = "none",
      colour = ggplot2::guide_legend(
        override.aes = list(linewidth = 1.5, linetype = "solid")
      )
    )
}


# ============================================================
#  Theme
# ============================================================
.spiro_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background     = ggplot2::element_rect(fill = "white", color = NA),
      panel.background    = ggplot2::element_rect(fill = "white", color = NA),
      axis.text           = ggplot2::element_text(color = "black"),
      axis.title          = ggplot2::element_text(color = "black"),
      axis.title.y        = ggplot2::element_text(size = 11),
      axis.title.y.right  = ggplot2::element_text(size = 11),
      axis.ticks          = ggplot2::element_line(color = "black"),
      axis.line           = ggplot2::element_line(color = "black"),
      axis.ticks.length   = ggplot2::unit(-0.15, "cm"),
      panel.grid.major    = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank(),
      legend.title        = ggplot2::element_blank()
    )
}

# ============================================================
#  9-FELDER-GRAFIK  (Wasserman-Style / Cortex)
# ============================================================

# ── MM:SS Zeitachsen-Formatter ───────────────────────────────
.time_fmt <- function(x) {
  sprintf("%02d:%02d", as.integer(floor(x)), as.integer(round((x %% 1) * 60)))
}
.time_scale <- function(x_max) {
  brk <- seq(0, ceiling(x_max), by = 3)
  ggplot2::scale_x_continuous(name = "Time", breaks = brk,
                               labels = .time_fmt, expand = c(0.01, 0))
}

# Time-axis helper for time-based panels
.np_time_x <- function(d) {
  x_max <- suppressWarnings(max(d$time_min, na.rm = TRUE))
  if (!is.finite(x_max)) x_max <- 15
  brk <- seq(0, ceiling(x_max / 3) * 3, by = 3)
  ggplot2::scale_x_continuous(name = "Time", breaks = brk,
                               labels = .time_fmt, expand = c(0.01, 0))
}
# ============================================================

# ── Theme: klinischer Stil mit gestricheltem Gitter ──────────
.np_theme <- function() {
  ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.border       = ggplot2::element_rect(fill = NA, colour = "grey60",
                                                  linewidth = 0.4),
      axis.text          = ggplot2::element_text(color = "black", size = 7),
      axis.title         = ggplot2::element_text(color = "black", size = 8,
                                                  face = "bold"),
      axis.title.y.right = ggplot2::element_text(color = "black", size = 8,
                                                  face = "bold"),
      axis.ticks         = ggplot2::element_line(color = "grey40", linewidth = 0.3),
      axis.ticks.length  = ggplot2::unit(0.1, "cm"),
      panel.grid.major   = ggplot2::element_line(colour = "grey80",
                                                  linetype = "dashed",
                                                  linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = 7),
      legend.key.size    = ggplot2::unit(0.35, "cm"),
      legend.position    = "none",
      plot.margin        = ggplot2::margin(4, 8, 4, 6)
    )
}

# ============================================================
#  ZENTRALE ACHSEN-/LABEL-HELFER  (Subscripts via plotmath)
#  Unicode-Subscripts (₂) werden im PNG-Device oft verschluckt;
#  daher werden alle Gas-Labels zentral als expression() gebaut.
#  Neue Labels NUR hier ergänzen, nicht pro Plot.
# ============================================================
.lab <- function(key) {
  switch(key,
    VE      = expression("V'E [L/min]"),
    AF      = expression("AF [/min]"),
    HF      = expression("HF [/min]"),
    O2puls  = expression("V'O"[2]*"/HF [ml]"),
    VO2     = expression("V'O"[2]*" [L/min]"),
    VCO2    = expression("V'CO"[2]*" [L/min]"),
    VO2VCO2 = expression("V'O"[2]*" / V'CO"[2]*" [L/min]"),
    Power   = expression("Power [W]"),
    EQO2    = expression("V'E/V'O"[2]),
    EQCO2   = expression("V'E/V'CO"[2]),
    VT      = expression("VT [L]"),
    RER     = expression("RER"),
    PetO2   = expression("PetO"[2]*" [mmHg]"),
    PetCO2  = expression("PetCO"[2]*" [mmHg]"),
    time    = expression("Time"),
    stop("unbekannter .lab-Key: ", key)
  )
}

# ── Zentrale Serien-Farben ──────────────────────────────────
#  VO2 = Blau, VCO2 = Rot (konsistent in allen Panels).
.col_vo2  <- "#1F4FD8"   # blau
.col_vco2 <- "#CD0000"   # rot
.col_grey <- "#BBBBBB"   # ausgegraut: NICHT im Fit/Slope

# ── Fit-Maske: nur Belastungs-Samples gehen in Fit/Slope ein ─
#  Liefert logischen Vektor; übrige Punkte werden grau gezeichnet
#  und fließen NICHT in lm()/Slope ein. Zentral für Konsistenz.
.in_fit_mask <- function(d) {
  if ("Phase" %in% names(d))
    d$Phase %in% c("Belastung", "Exercise")
  else rep(TRUE, nrow(d))
}

# ── VT-Vertikallinien (dunkelgrün VT1, olivgrün VT2) ────────
.vt_vlines <- function(vt1_time, vt2_time) {
  layers <- list()
  if (is.finite(vt1_time)) {
    layers <- c(layers, list(
      ggplot2::geom_vline(xintercept = vt1_time, colour = "#006400",
                          linewidth = 0.8)
    ))
  }
  if (is.finite(vt2_time)) {
    layers <- c(layers, list(
      ggplot2::geom_vline(xintercept = vt2_time, colour = "#6B8E23",
                          linewidth = 0.8)
    ))
  }
  layers
}

# ── Belastungsstart-Vertikallinie (orange) ──────────────────
# Markiert den Beginn der Phase "Belastung"/"Exercise" in allen
# zeitbasierten Panels. Farbe: Okabe-Ito-Orange (colorblind-safe).
.belastung_start_vline <- function(d, colour = "#E69F00") {
  if (!"Phase" %in% names(d) || !"time_min" %in% names(d)) return(list())
  bel <- d$time_min[d$Phase %in% c("Belastung", "Exercise") &
                      is.finite(d$time_min)]
  if (length(bel) == 0) return(list())
  t0 <- min(bel, na.rm = TRUE)
  if (!is.finite(t0)) return(list())
  list(ggplot2::geom_vline(xintercept = t0, colour = colour,
                           linewidth = 0.4))
}

# ── VT-Kreismarker (auf Wunsch komplett entfernt) ───────────
# Die früheren Kreis-/Punktmarker an den VT-Schnittpunkten wurden
# vollständig entfernt; nur die vertikalen VT-Linien (.vt_vlines /
# .vt_xlines) bleiben. Die Funktionen liefern bewusst keine Layer
# mehr, damit die zahlreichen Aufrufstellen unverändert bleiben.
.vt_circles_time <- function(d, y_col, vt1_time, vt2_time,
                              col_vt1 = "#006400", col_vt2 = "#6B8E23") {
  list()
}

# ── Flex VT key lookup ───────────────────────────────────────
.find_vt_val <- function(vt_list, pattern) {
  if (is.null(vt_list)) return(NA_real_)
  for (k in names(vt_list)) {
    k_norm <- tolower(gsub("[\u2019\u2018\u0027`]", "'", k))
    if (grepl(pattern, k_norm)) return(vt_list[[k]])
  }
  NA_real_
}

# ── VT-Vertikallinien für XY-Plots (x = V'O2 oder V'CO2) ───
.vt_xlines <- function(vt, pattern_vt1, pattern_vt2,
                        col_vt1 = "#006400", col_vt2 = "#6B8E23") {
  layers <- list()
  v1 <- .find_vt_val(vt, pattern_vt1)
  v2 <- .find_vt_val(vt, pattern_vt2)
  if (is.finite(v1 %||% NA))
    layers <- c(layers, list(
      ggplot2::geom_vline(xintercept = v1, colour = col_vt1, linewidth = 0.8)
    ))
  if (is.finite(v2 %||% NA))
    layers <- c(layers, list(
      ggplot2::geom_vline(xintercept = v2, colour = col_vt2, linewidth = 0.8)
    ))
  layers
}

# ── XY-Plot: VT-Kreismarker (auf Wunsch komplett entfernt) ──
# Siehe .vt_circles_time: Kreis-/Punktmarker an den VT-Schnitt-
# punkten wurden entfernt; nur die vertikalen VT-Linien bleiben.
.vt_circles_xy <- function(d, x_col, y_col, vt, pattern,
                            col = "#006400") {
  list()
}


# ============================================================
#  PANEL-FUNKTIONEN
# ============================================================

# Panel 1 ── V'E vs. Zeit ─────────────────────────────────────
np_panel_1 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    VE_s = safe_roll(VE, smooth_n),
    AF_s = safe_roll(AF, smooth_n)
  )
  has_af <- any(is.finite(d$AF_s))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min))
  if (has_af) {
    af_rng <- range(d$AF_s[is.finite(d$AF_s)])
    ve_rng <- range(d$VE_s[is.finite(d$VE_s)])
    if (diff(af_rng) == 0) af_rng <- af_rng + c(-5, 5)
    if (diff(ve_rng) == 0) ve_rng <- ve_rng + c(-5, 5)
    f <- diff(ve_rng) / max(diff(af_rng), 0.1)
    b <- ve_rng[1] - af_rng[1] * f
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = VE_s), colour = "#8B0000",
                         linewidth = 0.7, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = AF_s * f + b), colour = "#006400",
                         linewidth = 0.7, na.rm = TRUE) +
      ggplot2::scale_y_continuous(
        name = .lab("VE"),
        sec.axis = ggplot2::sec_axis(~ (. - b) / f, name = .lab("AF")))
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = VE_s), colour = "#8B0000",
                         linewidth = 0.7, na.rm = TRUE) +
      ggplot2::scale_y_continuous(name = .lab("VE"))
  }
  p + .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d) +
    .np_time_x(d) +
    .np_theme() +
    ggplot2::theme(axis.title.y       = ggplot2::element_text(colour = "#8B0000",
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(colour = "#006400",
                                                               size = 8, face = "bold"))
}

# Panel 2 ── HR & O₂-Puls vs. Zeit ────────────────────────────
np_panel_2 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    HR_s    = safe_roll(HR, smooth_n),
    # O2-Puls: VO2_HR wenn vorhanden, sonst VO2abs*1000/HR
    VO2HR_calc = dplyr::if_else(
      is.finite(VO2_HR), VO2_HR,
      dplyr::if_else(is.finite(VO2abs) & is.finite(HR) & HR > 0,
                     VO2abs * 1000 / HR, NA_real_)),
    VO2HR_s = safe_roll(VO2HR_calc, smooth_n)
  )
  has_hr   <- any(is.finite(d$HR_s))
  has_opul <- any(is.finite(d$VO2HR_s))
  x_max <- suppressWarnings(max(d$time_min, na.rm = TRUE))
  if (!is.finite(x_max)) x_max <- 15

  if (!has_hr && !has_opul) {
    return(
      ggplot2::ggplot(d, ggplot2::aes(x = time_min)) +
        .np_time_x(d) +
        ggplot2::scale_y_continuous(name = .lab("HF"), limits = c(60, 200)) +
        ggplot2::annotate("text", x = ceiling(x_max)/2, y = 130,
                          label = "Keine HR-Daten vorhanden",
                          size = 3.5, colour = "grey50", fontface = "italic") +
        .np_theme()
    )
  }
  hr_rng <- if (has_hr) range(d$HR_s[is.finite(d$HR_s)]) else c(60, 200)
  op_rng <- if (has_opul) range(d$VO2HR_s[is.finite(d$VO2HR_s)]) else c(0, 25)
  if (diff(hr_rng) == 0) hr_rng <- hr_rng + c(-10, 10)
  if (diff(op_rng) == 0) op_rng <- op_rng + c(-2, 2)
  f <- diff(hr_rng) / max(diff(op_rng), 0.1)
  b <- hr_rng[1] - op_rng[1] * f

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min))
  if (has_hr)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = HR_s), colour = "#00008B",
                                linewidth = 0.7, na.rm = TRUE)
  if (has_opul)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = VO2HR_s * f + b),
                                colour = "#00CED1", linewidth = 0.7, na.rm = TRUE)
  p + .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d) +
    ggplot2::scale_y_continuous(
      name = .lab("HF"), limits = hr_rng,
      sec.axis = ggplot2::sec_axis(~ (. - b) / f, name = .lab("O2puls"))) +
    .np_time_x(d) +
    .np_theme() +
    ggplot2::theme(axis.title.y       = ggplot2::element_text(colour = "#00008B",
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(colour = "#00CED1",
                                                               size = 8, face = "bold"))
}

# Panel 3 ── V'O₂ & V'CO₂ vs. Zeit (+ Power Ribbon) ──────────
np_panel_3 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    VO2_s  = safe_roll(VO2abs, smooth_n),
    VCO2_s = safe_roll(VCO2,  smooth_n)
  )
  vo2_vals <- c(d$VO2_s[is.finite(d$VO2_s)], d$VCO2_s[is.finite(d$VCO2_s)])
  if (length(vo2_vals) < 2) vo2_vals <- c(0, 3)
  y_lim <- c(0, max(vo2_vals) * 1.1)

  has_p <- any(is.finite(d$P))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min))

  # Power als graues Ribbon
  if (has_p) {
    p_max <- max(d$P[is.finite(d$P)], na.rm = TRUE)
    if (p_max <= 0) p_max <- 300
    fp <- diff(y_lim) / p_max
    bp <- y_lim[1]
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = bp,
                   ymax = dplyr::if_else(is.finite(P),
                            pmin(P, p_max) * fp + bp, bp)),
      fill = "grey60", alpha = 0.3, na.rm = TRUE)
  }

  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = VO2_s),  colour = .col_vo2,
                       linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = VCO2_s), colour = .col_vco2,
                       linewidth = 0.7, na.rm = TRUE) +
    .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d)

  # Linke Achse tr\u00e4gt VO2 UND VCO2 (gleiche Einheit/Skala L/min);
  # der Titel weist beide aus. Power bleibt \u2013 falls vorhanden \u2013 rechts.
  # Power-Achse: mind. 6 Werte (n.breaks).
  if (has_p) {
    p <- p + ggplot2::scale_y_continuous(
      name = .lab("VO2VCO2"), limits = y_lim,
      sec.axis = ggplot2::sec_axis(~ (. - bp) / fp, name = .lab("Power"),
                                   breaks = function(x) pretty(x, n = 6)))
  } else {
    p <- p + ggplot2::scale_y_continuous(
      name = .lab("VO2VCO2"),
      sec.axis = ggplot2::dup_axis(name = .lab("VCO2")))
  }

  p + .np_time_x(d) +
    .np_theme() +
    ggplot2::theme(axis.title.y       = ggplot2::element_text(colour = "black",
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(
                     colour = if (has_p) "grey40" else .col_vco2,
                     size = 8, face = "bold"))
}

# Panel 4 ── V'E vs. V'CO₂  (EqCO₂-Isopleten + Schattierung) ─
np_panel_4 <- function(ts, smooth_n = 20, vt = NULL,
                        vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    VCO2_s = safe_roll(VCO2, smooth_n),
    VE_s   = safe_roll(VE,   smooth_n),
    in_fit = .in_fit_mask(ts)
  )
  xm <- suppressWarnings(max(d$VCO2_s, na.rm = TRUE))
  ym <- suppressWarnings(max(d$VE_s,   na.rm = TRUE))
  if (!is.finite(xm)) xm <- 3;  if (!is.finite(ym)) ym <- 120
  ax <- xm * 1.15

  # Schattierung: Normalbereich Slope 25-34
  shade <- data.frame(
    x = c(0, ax, ax, 0),
    y = c(0, 25*ax, 34*ax, 0)
  )
  # Nur zwei Isopleten: Slope 25 und Slope 34
  iso_eq <- c(25, 34)
  iso_df <- do.call(rbind, lapply(iso_eq, function(eq) {
    data.frame(x = c(0, ax), y = c(0, eq * ax), grp = as.character(eq))
  }))

  # V'E/V'CO2 Slope NUR aus Belastungs-Samples (in_fit); ausgegraute
  # Warmup-/Erholungs-Punkte flie\u00dfen NICHT ein.
  d_bel <- d |> dplyr::filter(in_fit, is.finite(VCO2_s), is.finite(VE_s))
  slope_val <- tryCatch({
    fit <- lm(VE_s ~ VCO2_s, data = d_bel)
    round(coef(fit)[2], 1)
  }, error = function(e) NA_real_)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = VCO2_s, y = VE_s)) +
    ggplot2::geom_polygon(data = shade, ggplot2::aes(x = x, y = y),
                          fill = "#B0C4DE", alpha = 0.25, inherit.aes = FALSE) +
    ggplot2::geom_line(data = iso_df, ggplot2::aes(x = x, y = y, group = grp),
                       colour = "#4682B4", linewidth = 0.35, inherit.aes = FALSE) +
    ggplot2::geom_point(ggplot2::aes(colour = in_fit), size = 0.5,
                        alpha = 0.6, na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#8B0000", `FALSE` = .col_grey), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, ax), ylim = c(0, max(ym * 1.1, 30*ax))) +
    .vt_xlines(vt, "v'?co2.*_vt1$", "v'?co2.*_vt2$") +
    ggplot2::labs(x = .lab("VCO2"), y = .lab("VE")) +
    .np_theme()

  if (is.finite(slope_val)) {
    slv <- format(slope_val, decimal.mark = ",")
    p <- p + ggplot2::annotate("label", x = 0.05 * ax, y = ym * 0.95,
              label = paste0("\"Slope V'E(V'CO\"[2]*\": ", slv, "\""),
              parse = TRUE,
              hjust = 0, size = 2.5, fill = "white",
              label.padding = ggplot2::unit(0.15, "lines"))
  }
  p
}

# Panel 5 ── V'CO₂ vs V'O₂ + HF (O₂-Puls-Isopleten, keine RER) ─
np_panel_5 <- function(ts, smooth_n = 20, vt = NULL,
                        vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    VO2_s  = safe_roll(VO2abs, smooth_n),
    VCO2_s = safe_roll(VCO2,  smooth_n),
    HR_s   = safe_roll(HR,    smooth_n),
    in_fit = .in_fit_mask(ts)
  )
  vo2_max  <- suppressWarnings(max(d$VO2_s,  na.rm = TRUE))
  vco2_max <- suppressWarnings(max(d$VCO2_s, na.rm = TRUE))
  if (!is.finite(vo2_max))  vo2_max  <- 3
  if (!is.finite(vco2_max)) vco2_max <- 3
  ax <- max(vo2_max, vco2_max) * 1.15

  has_hr <- any(is.finite(d$HR_s))

  # Dual-Achse fuer HR
  if (has_hr) {
    hr_rng <- range(d$HR_s[is.finite(d$HR_s)])
    if (diff(hr_rng) == 0) hr_rng <- hr_rng + c(-10, 10)
    fhr <- ax / max(diff(hr_rng), 1)
    bhr <- 0 - hr_rng[1] * fhr
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(x = VO2_s))

  # O2-Puls-Isopleten (10, 20 ml) mit Schattierung
  if (has_hr) {
    # HR = VO2*1000/O2P → auf primaere y: y = HR * fhr + bhr
    n_pts <- 50
    x_seq <- seq(0.001, ax, length.out = n_pts)
    shade_o2p <- data.frame(
      x = c(x_seq, rev(x_seq)),
      y = c((x_seq * 1000 / 20) * fhr + bhr,
            rev((x_seq * 1000 / 10) * fhr + bhr))
    )
    p <- p + ggplot2::geom_polygon(data = shade_o2p,
              ggplot2::aes(x = x, y = y),
              fill = "#DDA0DD", alpha = 0.2, inherit.aes = FALSE)

    iso_o2p <- c(10, 20)
    iso_o2p_df <- do.call(rbind, lapply(iso_o2p, function(o2p) {
      data.frame(x = c(0, ax),
                 y = c(0 * fhr + bhr, (ax * 1000 / o2p) * fhr + bhr),
                 grp = paste0("O2P=", o2p))
    }))
    p <- p + ggplot2::geom_line(data = iso_o2p_df,
              ggplot2::aes(x = x, y = y, group = grp),
              colour = "#CD00CD", linewidth = 0.4, inherit.aes = FALSE)
  }

  # VCO2 vs VO2 (rote Punkte; Warmup/Erholung ausgegraut)
  p <- p + ggplot2::geom_point(ggplot2::aes(y = VCO2_s, colour = in_fit),
                                size = 0.5, alpha = 0.6, na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = .col_vco2, `FALSE` = .col_grey), guide = "none")

  # HR als Punkte auf sekundaerer Achse
  if (has_hr) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = HR_s * fhr + bhr),
                                  size = 0.4, alpha = 0.5, colour = "#CD00CD",
                                  na.rm = TRUE)

    # Horizontale VT-Marker auf HR-Achse
    # Nearest-Neighbour-Lookup strikt auf Belastungs-Samples – sonst
    # kann der gleiche V'O2-Wert auf dem Erholungs-Ast getroffen werden.
    d_bel <- if ("Phase" %in% names(d))
      d[d$Phase %in% c("Belastung", "Exercise"), , drop = FALSE] else d
    vt1_vo2 <- .find_vt_val(vt, "v'?o2_vt1$")
    vt2_vo2 <- .find_vt_val(vt, "v'?o2_vt2$")
    if (is.finite(vt1_vo2 %||% NA) && nrow(d_bel) > 0) {
      idx <- which.min(abs(d_bel$VO2_s - vt1_vo2))
      if (length(idx) == 1 && is.finite(d_bel$HR_s[idx]))
        p <- p + ggplot2::geom_hline(
          yintercept = d_bel$HR_s[idx] * fhr + bhr,
          colour = "#006400", linewidth = 0.5, linetype = "dashed")
    }
    if (is.finite(vt2_vo2 %||% NA) && nrow(d_bel) > 0) {
      idx <- which.min(abs(d_bel$VO2_s - vt2_vo2))
      if (length(idx) == 1 && is.finite(d_bel$HR_s[idx]))
        p <- p + ggplot2::geom_hline(
          yintercept = d_bel$HR_s[idx] * fhr + bhr,
          colour = "#6B8E23", linewidth = 0.5, linetype = "dashed")
    }

    p <- p + ggplot2::coord_cartesian(xlim = c(0, ax), ylim = c(0, ax)) +
      ggplot2::scale_y_continuous(
        name = .lab("VCO2"),
        sec.axis = ggplot2::sec_axis(~ (. - bhr) / fhr, name = .lab("HF")))
  } else {
    p <- p + ggplot2::coord_cartesian(xlim = c(0, ax), ylim = c(0, ax)) +
      ggplot2::scale_y_continuous(name = .lab("VCO2"))
  }

  p <- p +
    .vt_xlines(vt, "v'?o2_vt1$", "v'?o2_vt2$") +
    ggplot2::labs(x = .lab("VO2")) +
    .np_theme() +
    ggplot2::theme(axis.title.x        = ggplot2::element_text(colour = .col_vo2,
                                                               size = 8, face = "bold"),
                   axis.title.y       = ggplot2::element_text(colour = .col_vco2,
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(colour = "#CD00CD",
                                                               size = 8, face = "bold"))
  p
}

# Panel 6 ── V'E/V'O₂ & V'E/V'CO₂ vs. Zeit ──────────────────
np_panel_6 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    EQ_O2_s  = safe_roll(VE_VO2, smooth_n),
    EQ_CO2_s = safe_roll(VE_VCO2, smooth_n)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min)) +
    ggplot2::geom_line(ggplot2::aes(y = EQ_O2_s),  colour = .col_vo2,
                       linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = EQ_CO2_s), colour = .col_vco2,
                       linewidth = 0.7, na.rm = TRUE) +
    .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d)

  p + ggplot2::scale_y_continuous(
      name = .lab("EQO2"),
      sec.axis = ggplot2::dup_axis(name = .lab("EQCO2"))) +
    .np_time_x(d) +
    .np_theme() +
    ggplot2::theme(axis.title.y       = ggplot2::element_text(colour = .col_vo2,
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(colour = .col_vco2,
                                                               size = 8, face = "bold"))
}

# Panel 7 ── VT vs. V'E  (BF20 + BF50 Isopleten + Schattierung) ─
np_panel_7 <- function(ts, smooth_n = 20, vt = NULL,
                        vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    VE_s = safe_roll(VE, smooth_n),
    VT_s = safe_roll(VT_vol, smooth_n),
    in_fit = .in_fit_mask(ts)
  )
  ve_max <- suppressWarnings(max(d$VE_s, na.rm = TRUE))
  vt_max <- suppressWarnings(max(d$VT_s, na.rm = TRUE))
  if (!is.finite(ve_max)) ve_max <- 120
  if (!is.finite(vt_max)) vt_max <- 3
  xm <- ve_max * 1.15
  ym <- vt_max * 1.25

  # Schattierung zwischen BF20 und BF50  (VT = V'E / BF)
  shade <- data.frame(
    x = c(0, xm, xm, 0),
    y = c(0, xm/20, xm/50, 0)
  )
  # Nur BF20 und BF50 Isopleten
  iso_af <- c(20, 50)
  iso_df <- do.call(rbind, lapply(iso_af, function(af) {
    data.frame(x = c(0, xm), y = c(0, xm / af), grp = as.character(af))
  }))

  # VT1/VT2: V'E Werte verwenden
  vt1_ve <- .find_vt_val(vt, "v'?e_vt1$")
  vt2_ve <- .find_vt_val(vt, "v'?e_vt2$")

  p <- ggplot2::ggplot(d, ggplot2::aes(x = VE_s, y = VT_s)) +
    ggplot2::geom_polygon(data = shade, ggplot2::aes(x = x, y = y),
                          fill = "#B0C4DE", alpha = 0.25, inherit.aes = FALSE) +
    ggplot2::geom_line(data = iso_df, ggplot2::aes(x = x, y = y, group = grp),
                       colour = "#4682B4", linewidth = 0.35, inherit.aes = FALSE) +
    ggplot2::geom_point(ggplot2::aes(colour = in_fit), size = 0.5,
                        alpha = 0.6, na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#8B4513", `FALSE` = .col_grey), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, xm), ylim = c(0, ym))

  # VT Linien vertikal bei V'E-Wert
  if (is.finite(vt1_ve %||% NA))
    p <- p + ggplot2::geom_vline(xintercept = vt1_ve, colour = "#006400",
                                  linewidth = 0.8) +
      .vt_circles_xy(d, "VE_s", "VT_s", vt, "v'?e_vt1$", "#006400")
  if (is.finite(vt2_ve %||% NA))
    p <- p + ggplot2::geom_vline(xintercept = vt2_ve, colour = "#6B8E23",
                                  linewidth = 0.8) +
      .vt_circles_xy(d, "VE_s", "VT_s", vt, "v'?e_vt2$", "#6B8E23")

  p + ggplot2::labs(x = .lab("VE"), y = .lab("VT")) +
    .np_theme()
}

# Panel 8 ── RER vs. Zeit ────────────────────────────────────
np_panel_8 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(RER_s = safe_roll(RER, smooth_n))
  rer_vals <- d$RER_s[is.finite(d$RER_s)]
  if (length(rer_vals) < 2) rer_vals <- c(0.7, 1.2)
  y_lo <- floor(min(rer_vals) * 10) / 10
  y_hi <- ceiling(max(rer_vals) * 10) / 10
  y_brk <- seq(y_lo, y_hi, by = 0.1)

  ggplot2::ggplot(d, ggplot2::aes(x = time_min, y = RER_s)) +
    ggplot2::geom_line(colour = "black", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 1.0, linetype = "solid",
                        colour = "grey50", linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = 1.1, linetype = "dashed",
                        colour = "#8B0000", linewidth = 0.35) +
    .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d) +
    ggplot2::scale_y_continuous(name = "RER", breaks = y_brk,
                                limits = c(y_lo, y_hi),
                                labels = function(x) format(x, nsmall = 1)) +
    .np_time_x(d) +
    .np_theme()
}

# Panel 9 ── PetO₂ & PetCO₂ vs. Zeit ─────────────────────────
np_panel_9 <- function(ts, smooth_n = 20, vt1_time = NA, vt2_time = NA) {
  d <- ts |> dplyr::mutate(
    PetO2_s  = safe_roll(PetO2,  smooth_n),
    PetCO2_s = safe_roll(PetCO2, smooth_n)
  )
  has_o2  <- any(is.finite(d$PetO2_s))
  has_co2 <- any(is.finite(d$PetCO2_s))

  if (!has_o2 && !has_co2) {
    x_max <- suppressWarnings(max(d$time_min, na.rm = TRUE))
    if (!is.finite(x_max)) x_max <- 15
    return(ggplot2::ggplot(d, ggplot2::aes(x = time_min)) +
      .np_time_x(d) +
      ggplot2::scale_y_continuous(name = .lab("PetO2"), limits = c(60, 140)) +
      ggplot2::annotate("text", x = ceiling(x_max)/2, y = 100,
                        label = "Keine PetO\u2082/PetCO\u2082 Daten",
                        size = 3, colour = "grey50", fontface = "italic") +
      .np_theme())
  }

  o2_rng  <- if (has_o2)  range(d$PetO2_s[is.finite(d$PetO2_s)])   else c(80, 130)
  co2_rng <- if (has_co2) range(d$PetCO2_s[is.finite(d$PetCO2_s)]) else c(20, 50)

  if (diff(o2_rng) == 0) o2_rng <- o2_rng + c(-5, 5)
  if (diff(co2_rng) == 0) co2_rng <- co2_rng + c(-5, 5)

  f <- diff(o2_rng) / max(diff(co2_rng), 0.1)
  b <- o2_rng[1] - co2_rng[1] * f

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min))
  if (has_o2)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = PetO2_s), colour = "#00008B",
                                linewidth = 0.7, na.rm = TRUE)
  if (has_co2)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = PetCO2_s * f + b),
                                colour = "#CD00CD", linewidth = 0.7, na.rm = TRUE)

  p + .vt_vlines(vt1_time, vt2_time) +
    .belastung_start_vline(d) +
    ggplot2::scale_y_continuous(
      name = .lab("PetO2"), limits = o2_rng,
      sec.axis = ggplot2::sec_axis(~ (. - b) / f, name = .lab("PetCO2"))) +
    .np_time_x(d) +
    .np_theme() +
    ggplot2::theme(axis.title.y       = ggplot2::element_text(colour = "#00008B",
                                                               size = 8, face = "bold"),
                   axis.title.y.right = ggplot2::element_text(colour = "#CD00CD",
                                                               size = 8, face = "bold"))
}


# ============================================================
#  ZUSAMMENBAU: flexible Panel-Auswahl
# ============================================================

# Alle 9 Panels erstellen und zurückgeben als benannte Liste
.build_all_panels <- function(ts, smooth_n, vt, vt1_time, vt2_time) {
  list(
    "1" = np_panel_1(ts, smooth_n, vt1_time, vt2_time),
    "2" = np_panel_2(ts, smooth_n, vt1_time, vt2_time),
    "3" = np_panel_3(ts, smooth_n, vt1_time, vt2_time),
    "4" = np_panel_4(ts, smooth_n, vt, vt1_time, vt2_time),
    "5" = np_panel_5(ts, smooth_n, vt, vt1_time, vt2_time),
    "6" = np_panel_6(ts, smooth_n, vt1_time, vt2_time),
    "7" = np_panel_7(ts, smooth_n, vt, vt1_time, vt2_time),
    "8" = np_panel_8(ts, smooth_n, vt1_time, vt2_time),
    "9" = np_panel_9(ts, smooth_n, vt1_time, vt2_time)
  )
}

# Panels zusammenbauen nach Auswahl und Anordnung
#   arrangement = "alt"  → 1,2,3/4,5,6/7,8,9 (Wasserman klassisch)
#   arrangement = "neu"  → 3,2,5/6,1,4/9,8,7 (ÖGP 2024 / Cortex neu)
.arrangement_order <- function(arrangement = "alt") {
  if (arrangement == "neu") c("3","2","5","6","1","4","9","8","7")
  else as.character(1:9)
}

nine_panel_assemble <- function(ts, smooth_n = 20, label = "",
                                vt = NULL, vt1_time = NA, vt2_time = NA,
                                panels = as.character(1:9), ncol = 3,
                                arrangement = "alt",
                                show_warmup = TRUE, show_recovery = TRUE) {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)

  keep <- c("Belastung", "Exercise")
  if (show_warmup)   keep <- c(keep, "Warmup", "Warm Up", "Warm-Up", "Erwärmung")
  if (show_recovery) keep <- c(keep, "Cooldown", "Cool Down", "Erholung", "Recovery")
  ts <- ts |> dplyr::filter(Phase %in% keep | is.na(Phase)) |>
    dplyr::arrange(time_min)

  all_p <- .build_all_panels(ts, smooth_n, vt, vt1_time, vt2_time)

  # Anordnung anwenden
  grid_order <- .arrangement_order(arrangement)
  # Nur die Panels auswählen die sowohl in panels als auch in grid_order sind
  ordered <- grid_order[grid_order %in% panels]
  sel <- all_p[ordered]
  sel <- sel[!vapply(sel, is.null, logical(1))]
  if (length(sel) == 0) return(NULL)

  if (length(sel) == 1) return(sel[[1]])

  patchwork_available <- requireNamespace("patchwork", quietly = TRUE)
  if (patchwork_available) {
    pw <- patchwork::wrap_plots(sel, ncol = ncol) +
      patchwork::plot_annotation(
        title = if (nzchar(label)) paste0("9-Felder-Grafik: ", label) else NULL,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12),
          plot.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
      )
    pw
  } else {
    gridExtra::arrangeGrob(
      grobs = sel, ncol = ncol,
      top = grid::textGrob(
        if (nzchar(label)) paste0("9-Felder-Grafik: ", label) else "",
        gp = grid::gpar(fontsize = 12, fontface = "bold"))
    )
  }
}

# ── Legacy Wrapper ───────────────────────────────────────────
nine_panel_plot <- function(ts, smooth_n = 20, label = "",
                            vt = NULL, vt1_time = NA, vt2_time = NA) {
  nine_panel_assemble(ts, smooth_n, label, vt, vt1_time, vt2_time,
                      panels = as.character(1:9), ncol = 3)
}

nine_panel_single <- function(ts, panel_idx, smooth_n = 20,
                              vt = NULL, vt1_time = NA, vt2_time = NA) {
  nine_panel_assemble(ts, smooth_n, "", vt, vt1_time, vt2_time,
                      panels = as.character(panel_idx), ncol = 1)
}

# ============================================================
#  STATISCHE VT-PLOTS (ggplot2) FÜR DOCX-EXPORT
#
#  Pendant zu den interaktiven plotly-Plots aus mod_vt_analysis.R.
#  Style: minimal-wissenschaftlich, weißer Hintergrund, klare Achsen.
# ============================================================

# Gemeinsames Theme
# Formaler Stil identisch zur 9-Felder-Grafik (.np_theme):
# schwarze Achsenlinien/-ticks UND schwarze Schrift, gestricheltes
# graues Gitter, grauer Panel-Rahmen. So sind die VT-Plots im Export
# formal deckungsgleich mit den App-Panels.
.vt_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.border       = ggplot2::element_rect(fill = NA, colour = "grey60",
                                                  linewidth = 0.4),
      axis.text          = ggplot2::element_text(color = "black"),
      axis.title         = ggplot2::element_text(color = "black", face = "bold"),
      axis.title.y.right = ggplot2::element_text(color = "black", face = "bold"),
      axis.ticks         = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.line          = ggplot2::element_line(color = "black"),
      axis.ticks.length  = ggplot2::unit(0.1, "cm"),
      panel.grid.major   = ggplot2::element_line(colour = "grey80",
                                                  linetype = "dashed",
                                                  linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(color = "black", face = "bold",
                                                  size = 12),
      legend.position    = "top",
      legend.title       = ggplot2::element_blank()
    )
}

# ── 1) V-Slope (VO2 vs VCO2) mit VT1-Markierung ─────────────
vt_vslope_ggplot <- function(ts, smooth_n = 20, vt1_time = NA, title = "V-Slope") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)
  # Alle Phasen behalten: Warmup/Erholung werden ausgegraut gezeigt,
  # zählen aber NICHT zur Slope (in_fit = nur Belastung).
  d <- ts[is.finite(ts$VO2abs), ]
  if (nrow(d) < 5) return(NULL)
  d$VO2_s  <- safe_roll(d$VO2abs, smooth_n)
  d$VCO2_s <- safe_roll(d$VCO2,   smooth_n)
  d$in_fit <- .in_fit_mask(d)
  ax <- max(c(d$VO2_s, d$VCO2_s), na.rm = TRUE) * 1.05
  if (!is.finite(ax)) ax <- 4

  p <- ggplot2::ggplot(d, ggplot2::aes(x = VO2_s, y = VCO2_s)) +
    ggplot2::geom_point(ggplot2::aes(colour = in_fit), alpha = 0.5, size = 1) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = .col_vo2, `FALSE` = .col_grey), guide = "none") +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "#374151", linewidth = 0.4) +
    ggplot2::annotate("text", x = ax * 0.9, y = ax * 0.93, label = "S = 1",
                      color = "#475569", size = 3) +
    ggplot2::coord_cartesian(xlim = c(0, ax), ylim = c(0, ax)) +
    ggplot2::labs(title = title, x = .lab("VO2"), y = .lab("VCO2")) +
    .vt_theme()

  if (is.finite(vt1_time)) {
    idx <- which.min(abs(d$time_min - vt1_time))
    if (length(idx) == 1 && is.finite(d$VO2_s[idx]) && is.finite(d$VCO2_s[idx])) {
      vx <- d$VO2_s[idx]; vy <- d$VCO2_s[idx]
      p <- p +
        ggplot2::geom_vline(xintercept = vx, linetype = "dashed",
                            color = "#006400", linewidth = 0.6) +
        ggplot2::annotate("label", x = vx, y = vy, label = "VT1",
                          color = "#006400", fill = "white",
                          size = 3.2,
                          hjust = -0.2, vjust = 1.2)
    }
  }
  p
}

# ── 2) Excess CO2 vs Zeit mit VT1-Markierung ────────────────
vt_exco2_ggplot <- function(ts, smooth_n = 20, vt1_time = NA, title = "Excess CO2") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)
  d <- ts[ts$Phase %in% c("Belastung", "Exercise"), ]
  if (nrow(d) < 5) return(NULL)
  d$ExCO2   <- calc_exco2(d$VO2abs, d$VCO2)
  d$ExCO2_s <- safe_roll(d$ExCO2, smooth_n)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min)) +
    ggplot2::geom_point(ggplot2::aes(y = ExCO2),
                        color = "#DC2626", alpha = 0.18, size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = ExCO2_s),
                       color = "#DC2626", linewidth = 0.8) +
    ggplot2::labs(title = title, y = expression("Excess CO"[2])) +
    .np_time_x(d) +
    .vt_theme()

  if (is.finite(vt1_time)) {
    p <- p +
      ggplot2::geom_vline(xintercept = vt1_time, linetype = "dashed",
                          color = "#006400", linewidth = 0.6) +
      ggplot2::annotate("label", x = vt1_time,
                        y = max(d$ExCO2_s, na.rm = TRUE),
                        label = sprintf("VT1 %.2f min", vt1_time),
                        color = "#006400", fill = "white", size = 3.2,
                          hjust = -0.05, vjust = 1)
  }
  p
}

# ── 3) VE vs VCO2 mit VT2-Markierung ────────────────────────
vt_ve_vco2_ggplot <- function(ts, smooth_n = 20, vt2_time = NA, title = "VE vs VCO2") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)
  # Alle Phasen behalten; Warmup/Erholung ausgegraut und NICHT im lm-Fit.
  d <- ts[is.finite(ts$VCO2), ]
  if (nrow(d) < 5) return(NULL)
  d$VCO2_s <- safe_roll(d$VCO2, smooth_n)
  d$VE_s   <- safe_roll(d$VE,   smooth_n)
  d$in_fit <- .in_fit_mask(d)
  d_fit <- d[d$in_fit & is.finite(d$VCO2_s) & is.finite(d$VE_s), ]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = VCO2_s, y = VE_s)) +
    ggplot2::geom_point(ggplot2::aes(colour = in_fit), alpha = 0.45, size = 1) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#2563EB", `FALSE` = .col_grey), guide = "none") +
    ggplot2::geom_smooth(data = d_fit, method = "lm", se = FALSE,
                         color = "#94a3b8", linewidth = 0.4,
                         linetype = "dotted", formula = y ~ x) +
    ggplot2::labs(title = title, x = .lab("VCO2"), y = .lab("VE")) +
    .vt_theme()

  if (is.finite(vt2_time)) {
    idx <- which.min(abs(d$time_min - vt2_time))
    if (length(idx) == 1 && is.finite(d$VCO2_s[idx]) && is.finite(d$VE_s[idx])) {
      vx <- d$VCO2_s[idx]; vy <- d$VE_s[idx]
      p <- p +
        ggplot2::geom_vline(xintercept = vx, linetype = "dashed",
                            color = "#B8860B", linewidth = 0.6) +
        ggplot2::annotate("label", x = vx, y = vy, label = "VT2",
                          color = "#B8860B", fill = "white",
                          size = 3.2,
                          hjust = -0.2, vjust = 1.2)
    }
  }
  p
}

# ── 4) Excess VE vs Zeit mit VT2-Markierung ─────────────────
vt_exve_ggplot <- function(ts, smooth_n = 20, vt2_time = NA, title = "Excess VE") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)
  d <- ts[ts$Phase %in% c("Belastung", "Exercise"), ]
  if (nrow(d) < 5) return(NULL)
  d$ExVE   <- calc_exve(d$VE, d$VCO2)
  d$ExVE_s <- safe_roll(d$ExVE, smooth_n)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = time_min)) +
    ggplot2::geom_point(ggplot2::aes(y = ExVE),
                        color = "#B45309", alpha = 0.18, size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = ExVE_s),
                       color = "#D97706", linewidth = 0.8) +
    ggplot2::labs(title = title, y = "Excess VE") +
    .np_time_x(d) +
    .vt_theme()

  if (is.finite(vt2_time)) {
    p <- p +
      ggplot2::geom_vline(xintercept = vt2_time, linetype = "dashed",
                          color = "#B8860B", linewidth = 0.6) +
      ggplot2::annotate("label", x = vt2_time,
                        y = max(d$ExVE_s, na.rm = TRUE),
                        label = sprintf("VT2 %.2f min", vt2_time),
                        color = "#B8860B", fill = "white", size = 3.2,
                          hjust = -0.05, vjust = 1)
  }
  p
}

# ── 5) Übersicht: VO2 + VCO2 vs Zeit mit VT1 + VT2 ──────────
vt_overview_ggplot <- function(ts, smooth_n = 20,
                               vt1_time = NA, vt2_time = NA,
                               title = "Übersicht V'O2 / V'CO2") {
  if (is.null(ts) || nrow(ts) == 0) return(NULL)
  d <- ts[ts$Phase %in% c("Belastung", "Exercise"), ]
  if (nrow(d) < 5) return(NULL)
  d$VO2_s  <- safe_roll(d$VO2abs, smooth_n)
  d$VCO2_s <- safe_roll(d$VCO2,   smooth_n)

  long <- data.frame(
    time_min = rep(d$time_min, 2),
    value    = c(d$VO2_s, d$VCO2_s),
    Variable = c(rep("V'O2", nrow(d)), rep("V'CO2", nrow(d)))
  )

  p <- ggplot2::ggplot(long,
        ggplot2::aes(x = time_min, y = value, color = Variable)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("V'O2" = .col_vo2, "V'CO2" = .col_vco2)) +
    ggplot2::labs(title = title, y = "[L/min]") +
    .np_time_x(d) +
    .vt_theme()

  if (is.finite(vt1_time)) {
    p <- p + ggplot2::geom_vline(xintercept = vt1_time, linetype = "dashed",
                                 color = "#006400", linewidth = 0.6) +
      ggplot2::annotate("text", x = vt1_time,
                        y = max(long$value, na.rm = TRUE),
                        label = "VT1", color = "#006400",
                        size = 3.2, hjust = -0.2, vjust = 1)
  }
  if (is.finite(vt2_time)) {
    p <- p + ggplot2::geom_vline(xintercept = vt2_time, linetype = "dashed",
                                 color = "#B8860B", linewidth = 0.6) +
      ggplot2::annotate("text", x = vt2_time,
                        y = max(long$value, na.rm = TRUE),
                        label = "VT2", color = "#B8860B",
                        size = 3.2, hjust = -0.2, vjust = 1)
  }
  p
}
