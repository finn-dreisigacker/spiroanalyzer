# ============================================================
#  fct_stages.R
#  Hilfen fuer den Datenueberpruefungs-Tab:
#    - Stufenzuordnung pro Zeile (nutzt detect_steps() aus fct_steps.R
#      als Stufenquelle, mit Vorrang fuer Stage_raw aus Quelldatei)
#    - Stufenmittelwerte mit 4 Methoden + Zeilen-Level Outlier-Exclusion
#    - Daten fuer den normalisierten Gesamtverlauf
#  Setzt detect_steps() aus fct_steps.R voraus -- keine eigene
#  Stufenerkennung, um Konsistenz mit dem Stufen-Tab zu garantieren.
# ============================================================

# Speed-Pendant zu detect_steps() — gleiche Logik, nur ueber Speed.
# Wird nur als Fallback genutzt, wenn keine Power-Spalte vorhanden ist.
.detect_steps_speed <- function(ts, min_step_sec = 15, tol = 0.5) {
  if (is.null(ts) || nrow(ts) < 5 || !"Speed" %in% names(ts) ||
      !"time_min" %in% names(ts) || !"Phase" %in% names(ts))
    return(NULL)

  bel <- ts |>
    dplyr::filter(Phase %in% c("Belastung", "Exercise"),
                  is.finite(Speed), is.finite(time_min)) |>
    dplyr::arrange(time_min)
  if (nrow(bel) < 5) return(NULL)

  S  <- bel$Speed
  tm <- bel$time_min

  S_round <- round(S * 2) / 2  # 0,5 km/h Raster
  r <- rle(S_round)
  ends   <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)

  plateaus <- data.frame(
    Speed_set    = r$values,
    n            = r$lengths,
    t_start_min  = tm[starts],
    t_end_min    = tm[ends]
  )
  plateaus$duration_sec <- (plateaus$t_end_min - plateaus$t_start_min) * 60

  if (nrow(plateaus) > 1) {
    keep <- rep(TRUE, nrow(plateaus))
    i <- 1
    while (i < nrow(plateaus)) {
      j <- i + 1
      while (j <= nrow(plateaus) &&
             abs(plateaus$Speed_set[j] - plateaus$Speed_set[i]) < tol) {
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
               "duration_sec", "Speed_set")]
}

#' Stufen-Zuordnung pro Zeile mit Quellenpriorisierung.
#'
#' (1) Wenn Stage_raw aus Quelldatei -> direkt verwenden.
#' (2) Sonst detect_steps() (Power-Plateaus) aus fct_steps.R.
#' (3) Sonst .detect_steps_speed() (Speed-Plateaus) als Fallback.
#' (4) Sonst NA mit "Rampenprotokoll erkannt" oder "nicht verfuegbar".
#'
#' Liefert pro Zeile eine character-Stufenzuweisung, die Belastungs-Stufen
#' nummeriert (z.B. "1", "2", ...). WarmUp- und Cooldown-Zeilen bleiben
#' NA (analog zum bestehenden Stufen-Tab).
resolve_stage_assignment <- function(ts, min_step_sec = 15) {
  n <- if (is.null(ts)) 0L else nrow(ts)
  none_out <- list(stage = rep(NA_character_, n),
                   source_label = "nicht verfuegbar",
                   ramp = FALSE, available = FALSE)
  if (n == 0L) return(none_out)

  # (1) Stage_raw aus Datei
  if ("Stage_raw" %in% names(ts) &&
      any(!is.na(ts$Stage_raw) &
          nzchar(trimws(as.character(ts$Stage_raw))))) {
    sr <- as.character(ts$Stage_raw)
    sr[is.na(sr) | !nzchar(trimws(sr))] <- NA_character_
    return(list(stage = sr,
                source_label = "Quelle: Datei",
                ramp = FALSE, available = TRUE))
  }

  # (2) detect_steps() aus fct_steps.R (Power)
  if ("P" %in% names(ts) && any(is.finite(ts$P))) {
    steps <- detect_steps(ts, min_step_sec = min_step_sec)
    if (!is.null(steps) && nrow(steps) > 0) {
      stage_full <- rep(NA_character_, n)
      for (i in seq_len(nrow(steps))) {
        in_step <- !is.na(ts$time_min) &
                   ts$time_min >= steps$t_start_min[i] &
                   ts$time_min <= steps$t_end_min[i]
        stage_full[in_step] <- as.character(steps$step_no[i])
      }
      return(list(stage = stage_full,
                  source_label = "Quelle: aus Leistung abgeleitet",
                  ramp = FALSE, available = TRUE))
    }
    # P vorhanden, aber keine Stufen -> Rampen-Verdacht
    has_belastung <- "Phase" %in% names(ts) &&
      any(ts$Phase %in% c("Belastung", "Exercise"))
    if (has_belastung) {
      return(list(stage = rep(NA_character_, n),
                  source_label = "Rampenprotokoll erkannt",
                  ramp = TRUE, available = FALSE))
    }
  }

  # (3) Speed-Fallback
  if ("Speed" %in% names(ts) && any(is.finite(ts$Speed))) {
    sp_steps <- .detect_steps_speed(ts, min_step_sec = min_step_sec)
    if (!is.null(sp_steps) && nrow(sp_steps) > 0) {
      stage_full <- rep(NA_character_, n)
      for (i in seq_len(nrow(sp_steps))) {
        in_step <- !is.na(ts$time_min) &
                   ts$time_min >= sp_steps$t_start_min[i] &
                   ts$time_min <= sp_steps$t_end_min[i]
        stage_full[in_step] <- as.character(sp_steps$step_no[i])
      }
      return(list(stage = stage_full,
                  source_label = "Quelle: aus Geschwindigkeit abgeleitet",
                  ramp = FALSE, available = TRUE))
    }
    return(list(stage = rep(NA_character_, n),
                source_label = "Rampenprotokoll erkannt",
                ramp = TRUE, available = FALSE))
  }

  none_out
}

#' Mittelwert-Berechnung pro Stufe
#'
#' @param data tibble mit time_s und Variablenspalten
#' @param stage_col Spaltenname mit Stufenzuordnung in data
#' @param time_col  Spaltenname mit Zeit (Sekunden)
#' @param method    "whole" | "last30" | "last60" | "custom"
#' @param custom_seconds nur fuer "custom"
#' @param exclude_row_ids integer vector — Zeilen-IDs (Index 1..n), die
#'                        aus den Aggregationen rausgenommen werden
#'                        (Zeilen-Level Exclusion)
#' @param replacements list mit Eintraegen pro Variable; jeder Eintrag ist
#'                     ein named numeric vector, names = row_id (character),
#'                     values = ersetzter Wert. Wirkt **Zellen-Level**: nur
#'                     der Wert fuer (row_id, variable) wird ersetzt, andere
#'                     Variablen derselben Zeile bleiben mit Rohwert.
#' @param weight_kg optional: fuer VO2/kg, falls VO2kg fehlt
#' @return tibble mit einer Zeile pro Stufe (inkl. n_replaced-Spalte)
calculate_stage_summary <- function(data,
                                    stage_col = "stage",
                                    time_col  = "time_s",
                                    method    = c("whole", "last30",
                                                  "last60", "custom"),
                                    custom_seconds = 60,
                                    exclude_row_ids = integer(0),
                                    replacements    = list(),
                                    weight_kg = NA_real_) {
  method <- match.arg(method)

  empty <- tibble::tibble(
    Stufe          = character(0),
    Startzeit_s    = numeric(0),
    Endzeit_s      = numeric(0),
    Dauer_s        = numeric(0),
    P_mean         = numeric(0),
    Speed_mean     = numeric(0),
    HR_mean        = numeric(0),
    VO2_mean       = numeric(0),
    `VO2/kg_mean`  = numeric(0),
    VCO2_mean      = numeric(0),
    VE_mean        = numeric(0),
    RER_mean       = numeric(0),
    AF_mean        = numeric(0),
    VT_mean        = numeric(0),
    n_raw          = integer(0),
    n_excluded     = integer(0),
    n_replaced     = integer(0),
    n_valid        = integer(0),
    mean_method    = character(0),
    Kommentar      = character(0)
  )

  if (!is.data.frame(data) || nrow(data) == 0L) return(empty)
  if (!stage_col %in% names(data) || !time_col %in% names(data)) return(empty)

  stages_vec <- as.character(data[[stage_col]])
  if (all(is.na(stages_vec))) return(empty)

  stage_levels <- unique(stages_vec[!is.na(stages_vec)])
  if (length(stage_levels) == 0L) return(empty)

  excl_set <- as.integer(exclude_row_ids)

  mean_safe <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) NA_real_ else mean(x)
  }

  rows_idx <- seq_len(nrow(data))

  out_list <- lapply(stage_levels, function(st) {
    in_stage <- !is.na(stages_vec) & stages_vec == st
    if (!any(in_stage)) return(NULL)

    idx_stage <- rows_idx[in_stage]
    t_stage   <- as.numeric(data[[time_col]])[idx_stage]
    t_start <- suppressWarnings(min(t_stage, na.rm = TRUE))
    t_end   <- suppressWarnings(max(t_stage, na.rm = TRUE))
    dauer   <- if (is.finite(t_start) && is.finite(t_end)) t_end - t_start else NA_real_

    sel_idx <- switch(method,
      whole  = idx_stage,
      last30 = idx_stage[t_stage >= (t_end - 30)],
      last60 = idx_stage[t_stage >= (t_end - 60)],
      custom = idx_stage[t_stage >= (t_end - custom_seconds)]
    )

    n_raw_v <- length(sel_idx)

    excl_in_sel <- intersect(sel_idx, excl_set)
    n_excl <- length(excl_in_sel)
    valid_idx <- setdiff(sel_idx, excl_set)
    n_val <- length(valid_idx)

    # Cell-Level Replacement: pro Variable Wertvektor mit Ersetzungen
    # n_replaced zaehlt eindeutige Zellen (row_id, variable) im Stufenfenster
    repl_cells <- character(0)
    pick_mean <- function(v) {
      if (!v %in% names(data)) return(NA_real_)
      x <- as.numeric(data[[v]])
      rmap <- replacements[[v]]
      if (is.numeric(rmap) && length(rmap) > 0L) {
        rid <- suppressWarnings(as.integer(names(rmap)))
        ok  <- !is.na(rid) & rid >= 1L & rid <= length(x)
        rid_v <- rid[ok]
        in_v  <- rid_v %in% valid_idx
        if (any(in_v)) {
          x[rid_v[in_v]] <- as.numeric(rmap[ok][in_v])
          repl_cells <<- c(repl_cells,
                           paste0(rid_v[in_v], "__", v))
        }
      }
      mean_safe(x[valid_idx])
    }

    HR    <- pick_mean("HR")
    VO2   <- pick_mean("VO2abs")
    VO2kg <- if ("VO2kg" %in% names(data)) pick_mean("VO2kg") else NA_real_
    if (!is.finite(VO2kg) && is.finite(VO2) && is.finite(weight_kg) && weight_kg > 0)
      VO2kg <- VO2 / weight_kg * 1000
    VCO2  <- pick_mean("VCO2")
    VE_v  <- pick_mean("VE")
    RER   <- pick_mean("RER")
    AF    <- pick_mean("AF")
    VT_v  <- pick_mean("VT_vol")
    P_m   <- pick_mean("P")
    Sp_m  <- pick_mean("Speed")

    method_label <- switch(method,
      whole  = "ganze Stufe",
      last30 = "letzte 30 s",
      last60 = "letzte 60 s",
      custom = paste0("letzte ", custom_seconds, " s")
    )

    tibble::tibble(
      Stufe          = st,
      Startzeit_s    = t_start,
      Endzeit_s      = t_end,
      Dauer_s        = dauer,
      P_mean         = P_m,
      Speed_mean     = Sp_m,
      HR_mean        = HR,
      VO2_mean       = VO2,
      `VO2/kg_mean`  = VO2kg,
      VCO2_mean      = VCO2,
      VE_mean        = VE_v,
      RER_mean       = RER,
      AF_mean        = AF,
      VT_mean        = VT_v,
      n_raw          = as.integer(n_raw_v),
      n_excluded     = as.integer(n_excl),
      n_replaced     = as.integer(length(unique(repl_cells))),
      n_valid        = as.integer(n_val),
      mean_method    = method_label,
      Kommentar      = ""
    )
  })

  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0L) return(empty)

  result <- dplyr::bind_rows(out_list)
  # Stufen numerisch sortieren wenn moeglich
  num_order <- suppressWarnings(as.numeric(result$Stufe))
  if (!any(is.na(num_order)))
    result <- result[order(num_order), , drop = FALSE]
  result
}

#' Daten fuer normalisierten Verlauf vorbereiten
prepare_normalized_overview <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(tibble::tibble(
      time_s = numeric(0), HR_pct = numeric(0),
      VO2_pct = numeric(0), Power_pct = numeric(0),
      Speed_pct = numeric(0)
    ))
  }

  pct <- function(v) {
    if (!v %in% names(data)) return(rep(NA_real_, nrow(data)))
    x <- as.numeric(data[[v]])
    mx <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(mx) || mx <= 0) return(rep(NA_real_, nrow(data)))
    x / mx * 100
  }

  tibble::tibble(
    time_s     = if ("time_s" %in% names(data)) as.numeric(data$time_s)
                 else rep(NA_real_, nrow(data)),
    HR_pct     = pct("HR"),
    VO2_pct    = pct("VO2abs"),
    Power_pct  = pct("P"),
    Speed_pct  = pct("Speed")
  )
}

#' Stufen-Zeitbereiche fuer Plot-Hintergrund (Start/End in Sekunden)
stage_ranges <- function(data, stage_col = "stage", time_col = "time_s") {
  empty <- tibble::tibble(stage = character(0), x_start = numeric(0),
                         x_end = numeric(0))
  if (!stage_col %in% names(data) || !time_col %in% names(data)) return(empty)
  st <- as.character(data[[stage_col]])
  if (all(is.na(st))) return(empty)
  t  <- as.numeric(data[[time_col]])

  uniq <- unique(st[!is.na(st)])
  out <- lapply(uniq, function(s) {
    idx <- which(!is.na(st) & st == s)
    tibble::tibble(stage = s,
                   x_start = min(t[idx], na.rm = TRUE),
                   x_end   = max(t[idx], na.rm = TRUE))
  })
  dplyr::bind_rows(out)
}
