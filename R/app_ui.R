#' App UI
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page(
    title = "SpiroAnalyzer",
    # Systemschrift statt Google-Font: kein Netzabruf ("Keine Online-
    # Übertragung") und auf macOS automatisch San Francisco.
    theme = bslib::bs_theme(
      version   = 5,
      bg        = "#ffffff",
      fg        = "#1d1d1f",
      primary   = "#0066cc",
      secondary = "#6e6e73",
      success   = "#248a3d",
      danger    = "#d70015",
      base_font = bslib::font_collection(
        "-apple-system", "BlinkMacSystemFont", "Segoe UI",
        "Helvetica Neue", "Arial", "sans-serif"),
      "border-radius" = "10px"
    ),

    shiny::tags$head(shiny::tags$style(shiny::HTML("
      /* ── Design-Tokens (einzige Quelle für Farben/Radien) ── */
      :root {
        --sa-bg:#f5f5f7; --sa-surface:#ffffff; --sa-fill:#f5f5f7;
        --sa-text:#1d1d1f; --sa-text-2:#6e6e73;      /* 16.8:1 / 5.1:1 auf Weiß */
        --sa-sep:#d2d2d7; --sa-sep-soft:#e8e8ed;
        --sa-accent:#0066cc;                          /* 5.6:1 auf Weiß */
        --sa-accent-soft:#e8f1fb; --sa-navy:#0B3D6E;
        --sa-vt1:#006400; --sa-vt2:#556b2f;
        --sa-radius:12px; --sa-radius-sm:8px;
        --sa-shadow:0 1px 2px rgba(0,0,0,.04);
      }
      body { background:var(--sa-bg); color:var(--sa-text);
             -webkit-font-smoothing:antialiased; }
      :focus-visible { outline:2px solid var(--sa-accent); outline-offset:2px; }
      @media (prefers-reduced-motion: reduce) {
        *, *::before, *::after { transition:none !important; animation:none !important; }
      }

      /* ── Karten ── */
      .card { border:1px solid var(--sa-sep-soft); border-radius:var(--sa-radius);
              box-shadow:var(--sa-shadow); }
      .sa-card, .info-card {
        background:var(--sa-surface); border:1px solid var(--sa-sep-soft);
        border-radius:var(--sa-radius); padding:20px;
        box-shadow:var(--sa-shadow); margin-bottom:16px;
      }
      .sa-card h4, .info-card h4 {
        color:var(--sa-text); font-weight:600; font-size:1.0625rem;
        margin:0 0 12px;
      }
      .btn { border-radius:var(--sa-radius-sm); font-weight:500; }


      .info-row { display:flex; justify-content:space-between; gap:12px;
        border-bottom:1px solid var(--sa-sep-soft); }
      .info-row:last-child { border-bottom:none; }
      .info-row-key { color:var(--sa-text-2); }
      .info-row-val { text-align:right; }
      .info-cite-box { background:var(--sa-fill); border-radius:var(--sa-radius-sm); padding:8px 10px; }

      /* ── Rahmen: Kopfzeile ── */
      .sa-top { position:fixed; inset:0 0 auto 0; height:52px; z-index:1030;
        display:flex; align-items:center; gap:12px; padding:0 16px;
        background:rgba(255,255,255,.85); border-bottom:1px solid var(--sa-sep-soft);
        -webkit-backdrop-filter:saturate(180%) blur(20px); backdrop-filter:saturate(180%) blur(20px); }
      @media (prefers-reduced-transparency: reduce) { .sa-top { background:#fff; backdrop-filter:none; } }
      .sa-brand { display:flex; align-items:center; gap:8px; font-weight:600; width:208px; }
      .sa-brand-mark { width:26px; height:26px; border-radius:7px; background:var(--sa-navy);
        display:inline-flex; align-items:center; justify-content:center; }
      .sa-brand-mark svg { width:18px; height:18px; fill:none; stroke:#fff; stroke-width:2.2;
        stroke-linecap:round; stroke-linejoin:round; }
      .sa-crumb { flex:1; min-width:0; color:var(--sa-text-2); font-size:.875rem;
        white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
      .sa-crumb b { color:var(--sa-text); font-weight:600; }
      .sa-sep { margin:0 8px; color:var(--sa-sep); }
      .sa-top-r { display:flex; gap:6px; }
      .sa-tb { display:inline-flex; align-items:center; gap:6px; min-height:32px; padding:0 10px;
        border:0; border-radius:var(--sa-radius-sm); background:transparent; color:var(--sa-text);
        font-size:.875rem; font-weight:500; }
      .sa-tb:hover { background:var(--sa-fill); }
      .sa-icon-btn { width:32px; justify-content:center; padding:0; color:var(--sa-text-2); }
      .sa-chev { font-size:.65rem; color:var(--sa-text-2); }
      .sa-lang-opt { display:flex; justify-content:space-between; gap:16px; padding:6px 4px; font-size:.875rem; }
      .sa-lang-opt.on { font-weight:600; }
      .sa-lang-opt.soon { color:var(--sa-text-2); }
      .sa-chip { display:inline-block; margin-left:6px; padding:1px 7px; border-radius:10px;
        background:var(--sa-fill); color:var(--sa-text-2); font-size:.6875rem; font-weight:500;
        vertical-align:middle; }

      /* ── Seitenleiste ── */
      .sa-side { position:fixed; top:52px; bottom:0; left:0; width:240px; overflow-y:auto;
        padding:14px 10px; background:var(--sa-fill); border-right:1px solid var(--sa-sep-soft); }
      body.sa-side-hidden .sa-side { display:none; }
      /* eingeklappt: nur Symbole */
      body.sa-rail .sa-side { width:60px; padding:14px 8px; }
      body.sa-rail .sa-main { margin-left:60px; }
      body.sa-rail .sa-file, body.sa-rail .sa-grp, body.sa-rail .sa-sub,
      body.sa-rail .sa-st, body.sa-rail .sa-dis { display:none !important; }
      body.sa-rail .sa-nav-a { font-size:0; justify-content:center; padding:4px 0; min-height:36px; }
      body.sa-rail .sa-nav-a > i, body.sa-rail .sa-nav-a > svg { font-size:1rem; width:18px; }
      .sa-side, .sa-main { transition:width .2s, margin-left .2s; }
      body.sa-side-hidden .sa-main { margin-left:0; }
      .sa-file { display:flex; align-items:center; gap:10px; padding:10px; margin-bottom:14px;
        background:var(--sa-surface); border:1px solid var(--sa-sep-soft); border-radius:var(--sa-radius-sm); }
      body.sa-locked .sa-file { display:none; }
      .sa-file-av { color:var(--sa-navy); }
      .sa-file-t { font-weight:600; font-size:.875rem; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .sa-file-s { font-size:.75rem; color:var(--sa-text-2); }
      .sa-file-sw { margin-left:auto; }
      .sa-grp { padding:0 10px 6px; font-size:.6875rem; font-weight:600; letter-spacing:.04em;
        text-transform:uppercase; color:var(--sa-text-2); }
      .sa-nav, .sa-sub { list-style:none; margin:0; padding:0; }
      .sa-nav-a { display:flex; align-items:center; gap:10px; min-height:32px; padding:4px 10px;
        border-radius:var(--sa-radius-sm); color:var(--sa-text); text-decoration:none; font-size:.875rem; }
      .sa-nav-a > i, .sa-nav-a > svg { width:16px; color:var(--sa-text-2); }
      .sa-nav-a:hover { background:rgba(0,0,0,.05); color:var(--sa-text); }
      .sa-nav-a.on { background:var(--sa-surface); font-weight:600; box-shadow:inset 3px 0 0 var(--sa-navy); }
      .sa-nav-a.soon, body.sa-locked .sa-nav-a[data-page]:not([data-page=\"upload\"]),
      body.sa-locked .sa-nav-grp > .sa-nav-a { color:var(--sa-text-2); pointer-events:none; opacity:.6; }
      .sa-st { margin-left:auto; font-size:.6875rem; color:var(--sa-text-2); }
      .sa-st.ok { color:var(--sa-vt1); font-weight:600; }
      .sa-sub { display:none; padding-left:26px; }
      .sa-nav-grp.open .sa-sub { display:block; }
      .sa-dis { margin-left:auto; font-size:.65rem; transition:transform .15s; }
      .sa-nav-grp.open .sa-dis { transform:rotate(90deg); }
      .sa-nav-grp.parent-on > .sa-nav-a { font-weight:600; }

      /* ── Inhalt ── */
      .sa-main { margin-left:240px; padding:76px 32px 40px; min-height:100vh; }
      .sa-page-h h1 { font-size:1.75rem; font-weight:700; letter-spacing:-.02em; margin:0; }
      .sa-page-sub { color:var(--sa-text-2); margin:2px 0 20px; }
      .sa-note { display:flex; gap:10px; align-items:center; max-width:640px; margin:0 auto 24px;
        padding:10px 14px; border-radius:var(--sa-radius-sm); background:#fff8e6; font-size:.875rem; }
      .upload-formats { margin:14px 0; text-align:center; }
      .sa-kpis { display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:12px; margin-bottom:16px; }
      .sa-kpi { background:var(--sa-surface); border:1px solid var(--sa-sep-soft);
        border-radius:var(--sa-radius); padding:14px 16px; }
      .sa-kpi-l { font-size:.8125rem; color:var(--sa-text-2); }
      .sa-kpi-v { font-size:1.625rem; font-weight:600; letter-spacing:-.01em; font-variant-numeric:tabular-nums; }
      .sa-kpi-u { font-size:.8125rem; font-weight:400; color:var(--sa-text-2); margin-left:4px; }
      .ov-wide { grid-column:1 / -1; }
      .sa-link { display:inline-flex; gap:6px; align-items:center; margin-top:12px;
        color:var(--sa-navy); font-weight:500; text-decoration:none; font-size:.875rem; }
      .popover { --bs-popover-max-width:340px; }
      .sa-info { width:300px; max-width:100%; font-size:.8125rem; }
      .sa-info .info-row-val { overflow-wrap:anywhere; }
      .sa-info .info-row { padding:5px 0; }
      /* Trainingsbereiche */
      .tz-ref { display:flex; gap:24px; margin-bottom:12px; font-size:.8125rem; color:var(--sa-text-2); }
      .tz-ref b { color:var(--sa-text); font-variant-numeric:tabular-nums; }
      .tz-scroll { overflow-x:auto; }
      table.tz { width:100%; min-width:720px; border-collapse:collapse; font-size:.875rem; font-variant-numeric:tabular-nums; }
      .tz th { text-align:left; font-weight:500; color:var(--sa-text-2); padding:8px 10px; border-bottom:1px solid var(--sa-sep); }
      .tz th small { display:block; font-weight:400; font-size:.75rem; }
      .tz td { padding:10px; border-bottom:1px solid var(--sa-sep-soft); vertical-align:top; }
      .tz tr:last-child td { border-bottom:0; }
      .tz-lv { font-weight:600; white-space:nowrap; }
      .tz-lv i { display:inline-block; width:8px; height:8px; border-radius:2px; margin-right:8px; }
      .tz-pct { display:block; color:var(--sa-text-2); font-size:.75rem; }
      .tz-src { margin-top:12px; font-size:.75rem; color:var(--sa-text-2); }
      /* Datenüberprüfung */
      .dc-toolbar { display:flex; flex-wrap:wrap; gap:8px 32px; margin-bottom:8px; }
      .dc-ctl { display:flex; align-items:center; gap:10px; }
      .dc-ctl > label { font-size:.8125rem; color:var(--sa-text-2); margin:0; }
      .dc-ctl .form-group { margin:0; }
      .dc-ctl .shiny-options-group { display:inline-flex; background:var(--sa-fill);
        border-radius:var(--sa-radius-sm); padding:2px; }
      .dc-ctl .radio-inline { margin:0; padding:0; }
      .dc-ctl .radio-inline input { position:absolute; opacity:0; }
      .dc-ctl .radio-inline span { display:inline-block; padding:4px 12px; border-radius:6px;
        font-size:.8125rem; cursor:pointer; }
      .dc-ctl .radio-inline input:checked + span { background:#fff; font-weight:600;
        box-shadow:0 1px 2px rgba(0,0,0,.12); }
      .dc-ctl .radio-inline input:focus-visible + span { outline:2px solid var(--sa-accent); }
      .dc-legend { display:flex; gap:16px; font-size:.8125rem; color:var(--sa-text-2); min-height:20px; }
      .dc-sw { display:inline-flex; align-items:center; gap:6px; margin-right:16px; }
      .dc-sw-c { width:14px; height:14px; border-radius:3px; border:1px solid var(--sa-sep-soft); }
      @media (max-width: 900px) {
        .sa-side { display:none; } .sa-main, body.sa-rail .sa-main { margin-left:0; padding:72px 16px 32px; }
        body.sa-side-hidden .sa-side { display:block; z-index:1020; }
        .sa-brand { width:auto; }
      }
    "))),
    mod_single_ui("single")
  )
}

# Inhalt des Info-Popovers (oben rechts)
info_popover_content <- function() {
  row <- function(k, v) shiny::div(class = "info-row",
    shiny::div(class = "info-row-key", k), shiny::div(class = "info-row-val", v))
  shiny::div(class = "sa-info",
    row("Version", "1.0.0"),
    row("Dateiformate", ".xml, .xlsx"),
    row("Lizenz", "GPL-3"),
    row("Datenschutz", "Keine Online-\u00dcbertragung."),
    row("Autor", "Finn Dreisigacker"),
    row("Kontakt", shiny::tags$a(href = "mailto:dreisigacker.finn@web.de", "E-Mail")),
    row("Quellcode", shiny::tags$a(href = "https://github.com/finn-dreisigacker/spiroanalyzer",
                                   target = "_blank", "GitHub")),
    row("ORCID", shiny::tags$a(href = "https://orcid.org/0009-0008-6419-0751",
                               target = "_blank", "0009-0008-6419-0751")),
    shiny::div(class = "info-cite-box", style = "margin-top:8px;",
      "Dreisigacker, F. (2025). SpiroAnalyzer \u2013 Spiroergometrie-Analyse [R Shiny App]."))
}

# Coming-Soon-Platzhalter (Pre-Post-Vergleich, Datenüberprüfung-Tabs …)
coming_soon_box <- function(title) {
  shiny::div(
    style = paste0("max-width:560px; margin:80px auto; text-align:center;",
                   "padding:40px 32px; border:1px solid #e8e8ed;",
                   "border-radius:12px; background:#fff;"),
    shiny::div(style = "font-size:2rem; margin-bottom:8px; color:#6e6e73;",
               shiny::icon("screwdriver-wrench")),
    shiny::tags$h3(style = "color:#1d1d1f; font-weight:600; margin-bottom:8px;",
                   title),
    shiny::tags$p(style = "color:#6e6e73; font-size:1rem; margin:0;",
                  "Dieser Bereich befindet sich noch in",
                  "Entwicklung und wird in einer kommenden Version verfügbar sein."))
}
