#' App UI
#' @import shiny
#' @noRd
app_ui <- function(request) {

  shiny::navbarPage(
    title       = shiny::tags$span(
      shiny::HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"
        style="width:28px;height:28px;vertical-align:middle;margin-right:6px;">
        <rect x="4" y="4" width="56" height="56" rx="12" fill="#0B1220"/>
        <path d="M12 38h9l4-12 7 22 6-18 5 8h9" fill="none" stroke="#E6F0FF"
              stroke-width="3.5" stroke-linecap="round" stroke-linejoin="round"/>
      </svg>SpiroAnalyzer'),
      style = "font-weight:800; font-size:1.05rem; color:#fff;"
    ),
    windowTitle = "SpiroAnalyzer",
    id          = "main_navbar",
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      base_font  = bslib::font_google("Manrope"),
      font_scale = 1
    ),

    header = shiny::tags$head(shiny::tags$style(shiny::HTML("

      /* ── Navbar ── */
      .navbar { padding: 4px 12px !important; min-height: 50px !important; }
      .navbar-brand { padding: 4px 0 !important; }

      /* ── Karten ── */
      .sa-card {
        background:#fff; border:1px solid #d9e4f5;
        border-radius:14px; padding:20px;
        box-shadow:0 4px 14px rgba(31,61,107,.07);
        margin-bottom:16px;
      }
      .sa-card h4 {
        color:#1f3d6b; font-weight:800; font-size:1rem;
        margin:0 0 12px; padding-bottom:8px;
        border-bottom:2px solid #e5eefa;
      }

      /* ── Hero (Start-Seite) ── */
      .hero-outer {
        background:linear-gradient(135deg,#0b1220 0%,#1a3a5c 55%,#0b2240 100%);
        border-radius:18px; padding:44px 40px 32px;
        margin-top:14px; position:relative; overflow:hidden;
      }
      .hero-outer::before {
        content:''; position:absolute; inset:0;
        background:radial-gradient(circle at 75% 30%,rgba(125,211,252,.12) 0%,transparent 60%);
        pointer-events:none;
      }
      .hero-eyebrow {
        font-size:.76rem; font-weight:700; letter-spacing:.12em;
        text-transform:uppercase; color:#7dd3fc; margin-bottom:8px;
      }
      .hero-title {
        font-size:clamp(1.8rem,3vw,3rem); color:#f0f8ff;
        font-weight:800; line-height:1.15; margin-bottom:12px;
      }
      .hero-sub { color:#93c5e0; font-size:1rem; margin-bottom:24px; max-width:520px; }
      .hero-chips { display:flex; gap:10px; flex-wrap:wrap; margin-top:28px; }
      .hero-chip {
        background:rgba(255,255,255,.1); border:1px solid rgba(255,255,255,.2);
        border-radius:20px; padding:5px 14px;
        font-size:.82rem; color:rgba(255,255,255,.85); font-weight:600;
      }
      .steps-grid {
        display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:14px;
      }
      .step-card {
        background:#fff; border:1px solid #d9e4f5; border-radius:12px;
        padding:16px; box-shadow:0 3px 10px rgba(31,61,107,.06);
        transition:transform .18s, box-shadow .18s;
      }
      .step-card:hover { transform:translateY(-3px); box-shadow:0 7px 20px rgba(31,61,107,.12); }
      .step-badge {
        width:30px; height:30px; border-radius:50%;
        background:linear-gradient(135deg,#2563eb,#1d4ed8);
        color:#fff; font-weight:800; font-size:.82rem;
        display:flex; align-items:center; justify-content:center; margin-bottom:8px;
      }
      .step-card h5 { font-size:.88rem; font-weight:800; color:#1f3d6b; margin-bottom:4px; }
      .step-card p  { font-size:.80rem; color:#5b6575; margin:0; }

      /* ── Info-Seite ── */
      .info-page { padding-top:12px; }
      .info-author-card {
        background:linear-gradient(135deg,#0b1220 0%,#1a3a5c 50%,#0b2240 100%);
        border-radius:20px; padding:32px; color:#fff;
        margin-bottom:20px; position:relative; overflow:hidden;
      }
      .info-author-card::before {
        content:''; position:absolute; inset:0;
        background:radial-gradient(circle at 80% 20%,rgba(125,211,252,.15) 0%,transparent 60%);
        pointer-events:none;
      }
      .info-author-avatar {
        width:76px; height:76px; border-radius:50%;
        background:linear-gradient(135deg,#2563eb,#7dd3fc);
        display:flex; align-items:center; justify-content:center;
        font-size:1.9rem; font-weight:800; color:#fff;
        margin-bottom:14px; border:3px solid rgba(255,255,255,.25);
      }
      .info-author-name  { font-size:1.55rem; font-weight:800; color:#f0f8ff; margin-bottom:4px; }
      .info-author-role  { font-size:.9rem; color:#7dd3fc; margin-bottom:14px;
                           font-weight:600; letter-spacing:.04em; }
      .info-contact-row  { display:flex; flex-wrap:wrap; gap:10px; margin-top:4px; }
      .info-contact-item {
        display:flex; align-items:center; gap:7px;
        background:rgba(255,255,255,.12); border:1px solid rgba(255,255,255,.2);
        border-radius:24px; padding:5px 14px;
        font-size:.83rem; color:rgba(255,255,255,.9);
        text-decoration:none; transition:background .15s;
      }
      .info-contact-item:hover { background:rgba(255,255,255,.22); color:#fff; text-decoration:none; }
      .info-contact-item .fa   { color:#7dd3fc; }
      .info-grid {
        display:grid; grid-template-columns:repeat(auto-fit,minmax(280px,1fr)); gap:16px;
      }
      .info-card {
        background:#fff; border:1px solid #d9e4f5; border-radius:14px;
        padding:20px; box-shadow:0 4px 14px rgba(31,61,107,.06);
      }
      .info-card h4 {
        color:#1f3d6b; font-weight:800; font-size:1rem;
        margin:0 0 14px; padding-bottom:8px; border-bottom:2px solid #e5eefa;
      }
      .info-row {
        display:flex; justify-content:space-between; align-items:flex-start;
        padding:6px 0; border-bottom:1px solid #f0f4fb; font-size:.87rem;
      }
      .info-row:last-child  { border-bottom:none; }
      .info-row-key { color:#5b7fa6; font-weight:600; min-width:120px; }
      .info-row-val { color:#1f3d6b; text-align:right; font-weight:500; flex:1; }
      .info-wf-item {
        display:flex; align-items:flex-start; gap:12px;
        padding:7px 0; border-bottom:1px solid #f0f4fb; font-size:.87rem;
      }
      .info-wf-item:last-child { border-bottom:none; }
      .info-wf-num {
        width:24px; height:24px; border-radius:50%;
        background:linear-gradient(135deg,#2563eb,#1d4ed8);
        color:#fff; font-weight:800; font-size:.72rem;
        display:flex; align-items:center; justify-content:center; flex-shrink:0;
      }
      .info-cite-box {
        background:#f4f8ff; border:1px solid #d6e4fa; border-radius:10px;
        padding:12px 14px; font-size:.82rem; color:#334155;
        font-style:italic; margin-top:6px;
      }
    "))),

# ════════════════════════════════════════════════════════
    # 1) START (Upload + Analyse, ehemals "Einzelanalyse")
    # ════════════════════════════════════════════════════════
    shiny::tabPanel(
      title = "Start",
      value = "start",
      mod_single_ui("single")
    ),

    # ════════════════════════════════════════════════════════
    # 2) PRE-POST-VERGLEICH (coming soon; mod_compare-Code bleibt aktiv)
    # ════════════════════════════════════════════════════════
    shiny::tabPanel(
      title = "Pre-Post-Vergleich",
      value = "compare",
      # Platzhalter statt mod_compare_ui("compare") – Modul bleibt im Code.
      shiny::div(
        style = paste0("max-width:560px; margin:80px auto; text-align:center;",
                       "padding:40px 32px; border:1px solid #e2e8f0;",
                       "border-radius:16px; background:#f8fafc;"),
        shiny::div(style = "font-size:2.4rem; margin-bottom:8px;", "🚧"),
        shiny::tags$h3(style = "color:#1f3d6b; font-weight:800; margin-bottom:8px;",
                       "Pre-Post-Vergleich"),
        shiny::tags$p(style = "color:#64748b; font-size:1rem; margin:0;",
                      "Coming soon – dieser Bereich befindet sich noch in",
                      "Entwicklung und wird in einer kommenden Version verfügbar sein.")
      )
    ),

    # ════════════════════════════════════════════════════════
    # 3) INFO
    # ════════════════════════════════════════════════════════
    shiny::tabPanel(
      title = "Info",
      value = "info",
      shiny::fluidPage(
        class = "info-page",

        # ── Author card ──────────────────────────────────────
        shiny::div(class = "info-author-card",
          shiny::div(class = "info-author-avatar", "FD"),
          shiny::div(class = "info-author-name", "Finn Dreisigacker"),
          shiny::div(class = "info-contact-row",
            shiny::tags$a(class = "info-contact-item",
              href = "mailto:dreisigacker.finn@web.de", target = "_blank",
              shiny::icon("envelope"), "E-Mail"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://github.com/finn-dreisigacker", target = "_blank",
              shiny::icon("github"), "GitHub"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://orcid.org/0009-0008-6419-0751", target = "_blank",
              shiny::icon("id-card"), "ORCID"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://www.dshs-koeln.de/visitenkarte/einrichtung/kreislaufforschung-und-sportmedizin/",
              target = "_blank",
              shiny::icon("building"), "DSHS K\u00f6ln")
          )
        ),

        # ── Info-Karten ──────────────────────────────────────
        shiny::div(class = "info-grid",

          # Über die App
          shiny::div(class = "info-card",
            shiny::tags$h4(shiny::icon("circle-info"), " \u00dcber die App"),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "App"),
              shiny::div(class = "info-row-val", "SpiroAnalyzer")),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Version"),
              shiny::div(class = "info-row-val", "1.0.0")),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Zweck"),
              shiny::div(class = "info-row-val",
                "Analyse & Vergleich von Spiroergometrie-Messungen (MetaLyzer)")),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Dateiformate"),
              shiny::div(class = "info-row-val", ".xml, .xlsx")),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Lizenz"),
              shiny::div(class = "info-row-val", "MIT License")),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Datenschutz"),
              shiny::div(class = "info-row-val", "Keine Online-\u00dcbertragung."))
          ),

          # Zitation
          shiny::div(class = "info-card",
            shiny::tags$h4(shiny::icon("quote-left"), " Zitation"),
            shiny::div(class = "info-cite-box",
              "Dreisigacker, F. (2025). SpiroAnalyzer \u2013 Spiroergometrie-Analyse ",
              "[R Shiny App]. ",
              shiny::tags$a(
                href = "https://github.com/finn-dreisigacker/spiroanalyzer",
                target = "_blank",
                "https://github.com/finn-dreisigacker/spiroanalyzer")
            ),
            shiny::tags$br(),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Kontakt"),
              shiny::div(class = "info-row-val",
                shiny::tags$a(href = "mailto:dreisigacker.finn@web.de",
                              "dreisigacker.finn@web.de"))),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "GitHub"),
              shiny::div(class = "info-row-val",
                shiny::tags$a(href = "https://github.com/finn-dreisigacker",
                              target = "_blank", "Repository")))
          ),

          # Literatur / Quellen (zugrunde liegende Methoden)
          shiny::div(class = "info-card", style = "grid-column:1 / -1;",
            shiny::tags$h4(shiny::icon("book"), " Literatur"),
            lit_section_label("Ventilatorische Schwellen & 9-Felder"),
            lit_item("Beaver, W. L., Wasserman, K., & Whipp, B. J. (1986).",
              "A new method for detecting anaerobic threshold by gas exchange.",
              "Journal of Applied Physiology, 60(6), 2020–2027.",
              "https://doi.org/10.1152/jappl.1986.60.6.2020"),
            lit_item("Wasserman, K., Hansen, J. E., Sue, D. Y., Stringer, W. W., & Whipp, B. J. (2012).",
              "Principles of Exercise Testing and Interpretation (5th ed.).",
              "Lippincott Williams & Wilkins."),
            lit_item("Kroidl, R. F., Schwarz, S., Lehnigk, B., & Fritsch, J. (Hrsg.). (2015).",
              "Kursbuch Spiroergometrie (3. Aufl.).", "Georg Thieme Verlag.",
              "https://doi.org/10.1055/b-003-104201"),

            lit_section_label("Fettoxidation (MFO / Fatmax)"),
            lit_item("Achten, J., Gleeson, M., & Jeukendrup, A. E. (2002).",
              "Determination of the exercise intensity that elicits maximal fat oxidation.",
              "Medicine & Science in Sports & Exercise, 34(1), 92–97.",
              "https://doi.org/10.1097/00005768-200201000-00015"),
            lit_item("Achten, J., & Jeukendrup, A. E. (2003).",
              "Maximal fat oxidation during exercise in trained men.",
              "International Journal of Sports Medicine, 24(8), 603–608.",
              "https://doi.org/10.1055/s-2003-43265"),
            lit_item("Frayn, K. N. (1983).",
              "Calculation of substrate oxidation rates in vivo from gaseous exchange.",
              "Journal of Applied Physiology, 55(2), 628–634.",
              "https://doi.org/10.1152/jappl.1983.55.2.628"),
            lit_item("Péronnet, F., & Massicotte, D. (1991).",
              "Table of nonprotein respiratory quotient: An update.",
              "Canadian Journal of Sport Sciences, 16(1), 23–29."),
            lit_item("Maunder, E., Plews, D. J., & Kilding, A. E. (2018).",
              "Contextualising maximal fat oxidation during exercise: Determinants and normative values.",
              "Frontiers in Physiology, 9, 599.",
              "https://doi.org/10.3389/fphys.2018.00599"),

            lit_section_label("VO₂max & Ausbelastung"),
            lit_item("Howley, E. T., Bassett, D. R., & Welch, H. G. (1995).",
              "Criteria for maximal oxygen uptake: Review and commentary.",
              "Medicine & Science in Sports & Exercise, 27(9), 1292–1301."),
            lit_item("Taylor, H. L., Buskirk, E., & Henschel, A. (1955).",
              "Maximal oxygen intake as an objective measure of cardio-respiratory performance.",
              "Journal of Applied Physiology, 8(1), 73–80.",
              "https://doi.org/10.1152/jappl.1955.8.1.73"),
            lit_item("Midgley, A. W., Carroll, S., Marchant, D., McNaughton, L. R., & Siegler, J. (2009).",
              "Evaluation of true maximal oxygen uptake based on a novel set of standardized criteria.",
              "Applied Physiology, Nutrition, and Metabolism, 34(2), 115–123.",
              "https://doi.org/10.1139/H08-146"),
            lit_item("Niemeyer, M., Knaier, R., & Beneke, R. (2021).",
              "The oxygen uptake plateau – A critical review of the frequently misunderstood phenomenon.",
              "Sports Medicine, 51(9), 1815–1834.",
              "https://doi.org/10.1007/s40279-021-01471-4"),
            lit_item("Bassett, D. R., & Howley, E. T. (2000).",
              "Limiting factors for maximum oxygen uptake and determinants of endurance performance.",
              "Medicine & Science in Sports & Exercise, 32(1), 70–84."),
            lit_item("Meyer, T., & Kindermann, W. (1999).",
              "Die maximale Sauerstoffaufnahme (VO₂max).",
              "Deutsche Zeitschrift für Sportmedizin, 50(9), 285–286."),
            lit_item("Sperlich, P. F., Holmberg, H.-C., Reed, J. L., Zinner, C., Mester, J., & Sperlich, B. (2015).",
              "Individual versus standardized running protocols in the determination of VO₂max.",
              "Journal of Sports Science & Medicine, 14(2), 386–393."),

            lit_section_label("Anthropometrie"),
            lit_item("World Health Organization. (2000).",
              "Obesity: Preventing and managing the global epidemic (WHO Technical Report Series 894).",
              "World Health Organization.")
          )
        )
      )
    )
  )
}

# Kleiner Abschnittstitel innerhalb der Literatur-Karte
lit_section_label <- function(txt) {
  shiny::div(style = paste0("margin:12px 0 4px; font-size:0.72rem;",
    "font-weight:800; letter-spacing:0.04em; text-transform:uppercase;",
    "color:#94a3b8;"), txt)
}
