# ============================================================
#  mod_single.R -- Einzelansicht (v8 - Verification + Literatur)
# ============================================================

mod_single_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$script("document.addEventListener('DOMContentLoaded', function(){ document.body.classList.add('sa-locked'); });"),
    # -- CSS ---------------------------------------------------
    shiny::tags$style(shiny::HTML(paste0("

      /* Farben/Radien: Tokens aus app_ui.R (--sa-*) */

      /* ── Upload-Seite ────────────────────────────────── */
      .upload-page { max-width: 640px; margin: 48px auto; text-align: center; }
      .upload-page .upload-title { font-size: 1.75rem; font-weight: 600; letter-spacing: -0.015em; margin-bottom: 8px; }
      .upload-page .upload-sub { font-size: 1rem; color: var(--sa-text-2); margin-bottom: 28px; }
      .upload-dropzone {
        border: 1.5px dashed var(--sa-sep); border-radius: var(--sa-radius);
        padding: 40px 32px 32px; background: var(--sa-surface);
        transition: border-color 0.15s, background 0.15s; position: relative;
      }
      .upload-dropzone:hover, .upload-dropzone.dragover { border-color: var(--sa-accent); background: var(--sa-accent-soft); }
      .upload-dropzone .upload-icon { font-size: 2.25rem; color: var(--sa-text-2); margin-bottom: 12px; }
      .upload-dropzone .upload-label { font-size: 1.0625rem; font-weight: 600; margin-bottom: 4px; }
      .upload-dropzone .upload-hint  { font-size: 0.8125rem; color: var(--sa-text-2); }
      .upload-dropzone .btn-browse {
        display: inline-block; margin-top: 16px; min-height: 36px;
        background: var(--sa-accent); color: #fff; border: none; border-radius: var(--sa-radius-sm);
        padding: 8px 22px; font-weight: 500; font-size: 0.9375rem; cursor: pointer;
      }
      .upload-dropzone .btn-browse:hover { filter: brightness(1.08); }
      #", ns("file_wrapper"), " .form-group { margin-bottom: 0; }
      #", ns("file_wrapper"), " .shiny-input-container { width: 100% !important; }
      #", ns("file_wrapper"), " label.control-label { display: none !important; }
      #", ns("file_wrapper"), " .input-group { display: none !important; }
      .demo-link { margin-top: 20px; font-size: 0.9375rem; color: var(--sa-text-2); }
      .demo-link button { background: none; border: none; padding: 4px 6px; color: var(--sa-accent); font: inherit; cursor: pointer; border-radius: 6px; }
      .demo-link button:hover { text-decoration: underline; }

      /* ── Header-Bar ──────────────────────────────────── */
      .sa-header { padding: 14px 4px 12px; display: flex; align-items: center; gap: 12px; flex-wrap: wrap; }
      .sa-header-back { background: var(--sa-surface); border: 1px solid var(--sa-sep-soft); color: var(--sa-accent); border-radius: var(--sa-radius-sm); min-width: 36px; min-height: 36px; padding: 6px 10px; cursor: pointer; }
      .sa-header-back:hover { background: var(--sa-accent-soft); color: var(--sa-accent); }
      .sa-header-id { font-size: 1.375rem; font-weight: 600; letter-spacing: -0.01em; display: flex; align-items: center; gap: 10px; }
      .sa-header-tp { background: var(--sa-accent-soft); border-radius: 6px; padding: 2px 10px; font-size: 0.8125rem; font-weight: 600; color: var(--sa-accent); }

      /* ── Uebersicht ──────────────────────────────────── */
      .ov-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; }
      @media (max-width: 992px) { .ov-grid { grid-template-columns: 1fr 1fr; } }
      @media (max-width: 600px) { .ov-grid { grid-template-columns: 1fr; } }
      .ov-card, .lit-card { background: var(--sa-surface); border: 1px solid var(--sa-sep-soft); border-radius: var(--sa-radius); padding: 20px; box-shadow: var(--sa-shadow); }
      .ov-card h5, .lit-card h5 { font-size: 0.8125rem; font-weight: 600; color: var(--sa-text-2); margin: 0 0 12px; }
      .ov-card h5 .fa, .ov-card h5 svg { margin-right: 4px; }
      .ov-row { display: flex; justify-content: space-between; padding: 7px 0; border-bottom: 1px solid var(--sa-sep-soft); font-size: 0.875rem; }
      .ov-row:last-child { border-bottom: none; }
      .ov-key { color: var(--sa-text-2); }
      .ov-val { color: var(--sa-text); font-weight: 600; text-align: right; font-variant-numeric: tabular-nums; }
      .ov-peak-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 8px; }
      .ov-peak-tile { background: var(--sa-fill); border-radius: var(--sa-radius-sm); padding: 12px; }
      .ov-peak-tile .pk-val { font-size: 1.375rem; font-weight: 600; line-height: 1.2; letter-spacing: -0.01em; font-variant-numeric: tabular-nums; }
      .ov-peak-tile .pk-lbl { font-size: 0.75rem; color: var(--sa-text-2); margin-top: 2px; }

      /* ── VT mini table ───────────────────────────────── */
      .vt-mini-tbl { width:100%; border-collapse:collapse; font-size:0.875rem; font-variant-numeric: tabular-nums; }
      .vt-mini-tbl th { color:var(--sa-text-2); font-weight:600; padding:6px 8px; text-align:left; border-bottom:1px solid var(--sa-sep); }
      .vt-mini-tbl td { padding:6px 8px; border-bottom:1px solid var(--sa-sep-soft); }
      .vt-mini-tbl tr:last-child td { border-bottom:none; }
      .vt-mini-tbl .vt1-col { color:var(--sa-vt1); }
      .vt-mini-tbl .vt2-col { color:var(--sa-vt2); }

      /* ── Literatur ───────────────────────────────────── */
      .lit-grid { display:grid; grid-template-columns: 1fr 1fr; gap: 16px; margin-top:12px; }
      @media (max-width: 768px) { .lit-grid { grid-template-columns: 1fr; } }
      .lit-item { padding:8px 0; border-bottom:1px solid var(--sa-sep-soft); font-size:0.875rem; }
      .lit-item:last-child { border-bottom:none; }
      .lit-item a { color:var(--sa-accent); text-decoration:none; font-weight:500; }
      .lit-item a:hover { text-decoration:underline; }
      .lit-item .lit-authors { color:var(--sa-text-2); font-size:0.8125rem; }
      .lit-formula { background:var(--sa-fill); border-radius:var(--sa-radius-sm); padding:10px 14px; margin:6px 0; font-family:ui-monospace,SFMono-Regular,Menlo,monospace; font-size:0.8125rem; }

      /* ── 9-Felder ────────────────────────────────────── */
      .np-fs-btn { position:absolute; top:8px; right:8px; z-index:20; background:var(--sa-surface); color:var(--sa-accent); border:1px solid var(--sa-sep-soft); border-radius:var(--sa-radius-sm); min-height:32px; padding:4px 12px; font-size:0.8125rem; cursor:pointer; }
      .np-fs-btn:hover { background:var(--sa-accent-soft); }
      .np-gear { position:absolute; top:8px; right:108px; z-index:20; width:32px; height:32px;
        background:var(--sa-surface); color:var(--sa-text-2); border:1px solid var(--sa-sep-soft);
        border-radius:var(--sa-radius-sm); }
      .np-gear:hover { background:var(--sa-fill); color:var(--sa-text); }
      .np-settings { width:240px; font-size:.8125rem; }
      .np-settings h6 { font-size:.75rem; font-weight:600; color:var(--sa-text-2); margin:8px 0 4px; }
      .np-settings .form-group { margin-bottom:4px; }
      #", ns("np_wrapper"), ".np-fullscreen { position:fixed !important; top:0; left:0; width:100vw; height:100vh; z-index:9999; background:#fff; overflow-y:auto; padding:12px 20px; }
    "))),

    # -- JS ---------------------------------------------------
    shiny::tags$script(shiny::HTML(paste0("
      $(document).ready(function() {
        Shiny.addCustomMessageHandler('npFS_", id, "', function(msg) {
          var el = document.getElementById('", ns("np_wrapper"), "');
          el.classList.toggle('np-fullscreen');
          document.body.classList.toggle('np-fs-active');
          var btn = el.querySelector('.np-fs-btn');
          if (el.classList.contains('np-fullscreen')) { btn.innerHTML = '<i class=\"fa fa-compress\"></i> Beenden'; }
          else { btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild'; }
        });
        document.addEventListener('keydown', function(e) {
          if (e.key === 'Escape') {
            var el = document.getElementById('", ns("np_wrapper"), "');
            if (el && el.classList.contains('np-fullscreen')) {
              el.classList.remove('np-fullscreen'); document.body.classList.remove('np-fs-active');
              var btn = el.querySelector('.np-fs-btn');
              if (btn) btn.innerHTML = '<i class=\"fa fa-expand\"></i> Vollbild';
            }
          }
        });
        var dz = document.getElementById('", ns("dropzone"), "');
        if (dz) {
          dz.addEventListener('dragover', function(e) { e.preventDefault(); dz.classList.add('dragover'); });
          dz.addEventListener('dragleave', function(e) { e.preventDefault(); dz.classList.remove('dragover'); });
          dz.addEventListener('drop', function(e) {
            e.preventDefault(); dz.classList.remove('dragover');
            var fi = document.querySelector('#", ns("file_wrapper"), " input[type=file]');
            if (fi && e.dataTransfer.files.length > 0) { fi.files = e.dataTransfer.files; $(fi).trigger('change'); }
          });
        }
        var bb = document.getElementById('", ns("browse_btn"), "');
        if (bb) { bb.addEventListener('click', function(e) { e.preventDefault(); var fi = document.querySelector('#", ns("file_wrapper"), " input[type=file]'); if (fi) fi.click(); }); }
      });
    "))),

    # ════════════════════════════════════════════════════════════
    #  RAHMEN: Kopfzeile + Seitenleiste + Inhalt
    # ════════════════════════════════════════════════════════════
    shiny::tags$header(class = "sa-top",
      shiny::div(class = "sa-brand",
        shiny::span(class = "sa-brand-mark", shiny::HTML(
          '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M3 14h3l2-6 3 10 3-8 2 4h5"/></svg>')),
        "SpiroAnalyzer"),
      shiny::tags$button(type = "button", class = "sa-tb sa-icon-btn",
        title = "Seitenleiste ein-/ausblenden", `aria-label` = "Seitenleiste ein-/ausblenden",
        onclick = "saToggleSide();",
        shiny::icon("table-columns")),
      shiny::div(class = "sa-crumb", id = ns("crumb"), shiny::tags$b("Keine Messung geladen")),
      shiny::div(class = "sa-top-r",
        bslib::popover(
          shiny::tags$button(type = "button", class = "sa-tb",
            shiny::icon("globe"), "DE", shiny::icon("chevron-down", class = "sa-chev")),
          shiny::div(class = "sa-lang",
            shiny::div(class = "sa-lang-opt on", "Deutsch"),
            shiny::div(class = "sa-lang-opt soon", "English", shiny::span(class = "sa-chip", "bald"))),
          placement = "bottom"),
        bslib::popover(
          shiny::tags$button(type = "button", class = "sa-tb",
            shiny::icon("circle-info"), "Info"),
          info_popover_content(),
          title = "SpiroAnalyzer", placement = "bottom"))),

    shiny::tags$nav(class = "sa-side", `aria-label` = "Navigation",
      shiny::div(class = "sa-file", id = ns("filebox"),
        shiny::span(class = "sa-file-av", shiny::icon("file-lines")),
        shiny::div(style = "min-width:0;",
          shiny::div(class = "sa-file-t", id = ns("file_id"), "–"),
          shiny::div(class = "sa-file-s", id = ns("file_date"), "")),
        shiny::actionButton(ns("reset_data"), label = NULL,
          icon = shiny::icon("rotate"), class = "sa-tb sa-icon-btn sa-file-sw",
          title = "Andere Datei laden", `aria-label` = "Andere Datei laden")),

      shiny::div(class = "sa-grp", "Messung"),
      shiny::tags$ul(class = "sa-nav",
        sa_nav_item("upload",   "Datei laden",           "arrow-up-from-bracket", ns),
        sa_nav_item("overview", "Übersicht",             "table-cells-large", ns),
        sa_nav_item("dcheck",   "Datenüberprüfung",      "shield-halved", ns),
        sa_nav_item("nine",     "9-Felder",              "border-all", ns),
        shiny::tags$li(class = "sa-nav-grp", id = ns("grp_eval"),
          shiny::tags$a(class = "sa-nav-a", href = "#", role = "button",
            onclick = "saToggleEval(); return false;",
            shiny::icon("chart-line"), "Auswertung",
            shiny::icon("chevron-right", class = "sa-dis")),
          shiny::tags$ul(class = "sa-sub",
            sa_nav_item("vt1",    "VT1",    NULL, ns, shiny::uiOutput(ns("st_vt1"), inline = TRUE),
                        title = "VT1 – erste ventilatorische Schwelle"),
            sa_nav_item("vt2",    "VT2",    NULL, ns, shiny::uiOutput(ns("st_vt2"), inline = TRUE),
                        title = "VT2 – zweite ventilatorische Schwelle"),
            sa_nav_item("vo2max", "V'O₂max", NULL, ns, shiny::uiOutput(ns("st_vo2"), inline = TRUE),
                        title = "V'O₂max-Verifikation"),
            sa_nav_item("vtsum",  "Schwellen-Übersicht", NULL, ns),
            sa_nav_item("zones",  "Trainingsbereiche", NULL, ns))),
        sa_nav_item("steps",    "Stufenzusammenfassung", "stairs", ns),
        sa_nav_item("export",   "Bericht & Export",      "file-lines", ns, soon = TRUE)),

      shiny::div(class = "sa-grp", style = "margin-top:16px;", "Vergleich"),
      shiny::tags$ul(class = "sa-nav",
        sa_nav_item("compare", "Pre-Post-Vergleich", "code-compare", ns, soon = TRUE))),

    shiny::tags$main(class = "sa-main",
      shiny::div(class = "sa-page-h", id = ns("page_h"),
        shiny::tags$h1(id = ns("ptitle"), ""),
        shiny::tags$p(class = "sa-page-sub", id = ns("psub"), "")),

      bslib::navset_hidden(id = ns("main"),

        bslib::nav_panel_hidden("upload",
          shiny::div(class = "upload-page",
            shiny::tags$h1(class = "upload-title", "Spiroergometrie-Datei laden"),
            shiny::div(class = "upload-sub",
              "Das Format wird automatisch erkannt. Die Datei verlässt diesen Rechner nicht."),
        shiny::div(id = ns("dropzone"), class = "upload-dropzone",
          shiny::div(class = "upload-icon", shiny::icon("arrow-up-from-bracket")),
          shiny::div(class = "upload-label", "Datei hierher ziehen"),
          shiny::div(class = "upload-hint", "XML oder Excel (.xlsx / .xls)"),
          shiny::tags$button(id = ns("browse_btn"), type = "button", class = "btn-browse", "Datei auswählen"),
          shiny::div(id = ns("file_wrapper"), style = "position:absolute; opacity:0; width:0; height:0; overflow:hidden;",
            shiny::fileInput(ns("file"), label = NULL, accept = c(".xlsx",".xls",".xml"), buttonLabel = "Datei", placeholder = ""))
        ),
            shiny::div(class = "demo-link", "Keine Datei zur Hand?",
              shiny::tags$button(type = "button",
                onclick = paste0("Shiny.setInputValue('", ns("load_demo"), "', 'demo', {priority: 'event'})"),
                "Demo-Datensatz laden")))),

        bslib::nav_panel_hidden("overview", shiny::uiOutput(ns("overview_tab"))),
        bslib::nav_panel_hidden("dcheck", mod_data_check_ui(ns("dcheck"))),
        bslib::nav_panel_hidden("nine",
          shiny::div(id = ns("np_wrapper"), style = "position:relative;",
            shiny::tags$button(class = "np-fs-btn",
              onclick = paste0("Shiny.setInputValue('", ns("np_fullscreen"), "', Math.random())"),
              shiny::icon("expand"), " Vollbild"),
            bslib::popover(
              shiny::tags$button(type = "button", class = "np-gear",
                title = "Einstellungen", `aria-label` = "Einstellungen", shiny::icon("gear")),
              shiny::div(class = "np-settings",
                shiny::tags$h6("Anordnung", style = "font-weight:700; color:#1d1d1f; margin-bottom:6px;"),
                  shiny::radioButtons(ns("np_arr"), label = NULL, inline = TRUE,
                    choices = c("Alt"="alt","Neu"="neu"), selected = "alt"),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Phasen", style = "font-weight:700; color:#1d1d1f; margin-bottom:4px;"),
                  shiny::checkboxInput(ns("np_warmup"), "Erwärmung", value = TRUE),
                  shiny::checkboxInput(ns("np_recovery"), "Erholung", value = TRUE),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Felder", style = "font-weight:700; color:#1d1d1f; margin-bottom:4px;"),
                  shiny::checkboxGroupInput(ns("np_panels"), label = NULL,
                    choiceNames = lapply(paste0(1:9, c(" V'E/AF"," HF/O2P"," VO2/VCO2"," V'E-V'CO2"," V-Slope"," Aeq."," VT-V'E"," RER"," Pet")),
                      function(x) shiny::HTML(paste0("<small>", x, "</small>"))),
                    choiceValues = as.character(1:9), selected = as.character(1:9)),
                  shiny::div(style = "display:flex; gap:4px; margin-bottom:4px;",
                    shiny::actionButton(ns("np_all"), "Alle", class = "btn-sm btn-outline-secondary", style = "flex:1;"),
                    shiny::actionButton(ns("np_none"), "Keine", class = "btn-sm btn-outline-secondary", style = "flex:1;")),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Spalten", style = "font-weight:700; color:#1d1d1f; margin-bottom:4px;"),
                  shiny::radioButtons(ns("np_ncol"), label = NULL, inline = TRUE,
                    choices = c("1"="1","2"="2","3"="3"), selected = "3"),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Glättung", style = "font-weight:700; color:#1d1d1f; margin-bottom:4px;"),
                  shiny::numericInput(ns("np_smooth"), label = NULL, value = 20, min = 5, max = 60, step = 1, width = "100%")),
              title = "Einstellungen", placement = "bottom"),
            shinycssloaders::withSpinner(shiny::uiOutput(ns("np_plot_ui")), type = 6)
          )),
        bslib::nav_panel_hidden("eval",
          mod_vt_ui(ns("vt"), vo2max_ui = mod_vo2max_verify_ui(ns("vo2v")))),
        bslib::nav_panel_hidden("steps", mod_steps_ui(ns("steps"))),
        bslib::nav_panel_hidden("zones", shiny::uiOutput(ns("zones_tab")))
      )
    ),

    # -- Navigation (JS) ---------------------------------------
    shiny::tags$script(shiny::HTML(paste0("
      var SA_NS = '", ns(""), "';
      var SA_SUB = ['vt1','vt2','vo2max','vtsum'];
      var SA_STATE = { loaded:false, id:'', date:'' };
      function saEl(id){ return document.getElementById(SA_NS + id); }
      function saGo(page) {
        if (!SA_STATE.loaded && page !== 'upload') return;
        var sub = SA_SUB.indexOf(page) >= 0 ? page : null;
        var inGrp = !!sub || page === 'zones';
        document.querySelectorAll('.sa-nav-a[data-page]').forEach(function(a){
          a.classList.toggle('on', a.dataset.page === page);
          if (a.dataset.page === page) a.setAttribute('aria-current','page'); else a.removeAttribute('aria-current');
        });
        var grp = saEl('grp_eval');
        grp.classList.toggle('open', inGrp);
        grp.classList.toggle('parent-on', inGrp);
        var a = document.querySelector('.sa-nav-a[data-page=\"' + page + '\"]');
        var label = a ? a.dataset.label : '';
        var title = a ? (a.dataset.title || label) : '';
        saEl('ptitle').textContent = page === 'upload' ? '' : title;
        saEl('page_h').style.display = page === 'upload' ? 'none' : '';
        saEl('psub').textContent = SA_STATE.loaded ? (SA_STATE.id + (SA_STATE.date ? ', ' + SA_STATE.date : '')) : '';
        var c = saEl('crumb');
        if (!SA_STATE.loaded) { c.innerHTML = '<b>Keine Messung geladen</b>'; }
        else {
          var parts = ['<b>' + SA_STATE.id + '</b>'];
          if (inGrp) parts.push('Auswertung');
          parts.push(label);
          c.innerHTML = parts.join('<span class=\"sa-sep\">/</span>');
        }
        Shiny.setInputValue(SA_NS + 'nav', sub ? 'eval' : page, {priority:'event'});
        if (sub) Shiny.setInputValue(SA_NS + 'vt-sub', sub, {priority:'event'});
      }
      function saToggleSide() {
        var narrow = window.matchMedia('(max-width: 900px)').matches;
        var cls = narrow ? 'sa-side-hidden' : 'sa-rail';
        var on = document.body.classList.toggle(cls);
        if (!narrow) localStorage.setItem('saRail', on ? '1' : '0');
        setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
      }
      if (localStorage.getItem('saRail') === '1')
        document.addEventListener('DOMContentLoaded', function(){ document.body.classList.add('sa-rail'); });
      function saToggleEval() {
        var grp = saEl('grp_eval');
        if (document.body.classList.contains('sa-rail')) { saToggleSide(); grp.classList.add('open'); }
        if (grp.classList.contains('open')) { grp.classList.remove('open'); return; }
        grp.classList.add('open');
        if (!grp.classList.contains('parent-on')) saGo('vt1');
      }
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('saState', function(m) {
          SA_STATE = m;
          document.body.classList.toggle('sa-locked', !m.loaded);
          saEl('file_id').textContent = m.id || '–';
          saEl('file_date').textContent = m.date || '';
          saGo(m.page);
        });
        Shiny.addCustomMessageHandler('saGo', function(m) { saGo(m.page); });
      });
      // Plots in eben eingeblendeten Seiten neu vermessen
      $(document).on('shown.bs.tab', function() {
        setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 30);
      });
    ")))
  )
}

# Ein Eintrag der Seitenleiste
sa_nav_item <- function(page, label, icon, ns, extra = NULL, title = label, soon = FALSE) {
  shiny::tags$li(
    shiny::tags$a(class = paste("sa-nav-a", if (soon) "soon"), href = "#",
      role = "button",
      `data-page` = if (!soon) page, `data-label` = label, `data-title` = title,
      title = label,
      `aria-disabled` = if (soon) "true",
      onclick = if (soon) "return false;" else paste0("saGo('", page, "'); return false;"),
      if (!is.null(icon)) shiny::icon(icon),
      label,
      if (soon) shiny::span(class = "sa-st", "bald"),
      extra))
}


# ============================================================
#  SERVER
# ============================================================
mod_single_server <- function(id, vt_override = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$np_fullscreen, {
      session$sendCustomMessage(paste0("npFS_", id), list())
    })

    params_r <- shiny::reactiveVal(NULL)
    output$has_data <- shiny::reactive({ !is.null(params_r()) })
    shiny::outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # Freundliche Fehlermeldung bei Einlese-/Parsing-Problemen
    notify_parse_error <- function(e) {
      shiny::showNotification(
        ui = shiny::div(
          shiny::tags$b("Die Datei konnte leider nicht eingelesen werden."),
          shiny::tags$br(),
          "Bitte prüfe kurz das Format (MetaLyzer XML/Excel oder ZAN xlsx). ",
          "Klappt es weiterhin nicht, schreib mir gern eine kurze E-Mail an ",
          shiny::tags$a(href = "mailto:dreisigacker.finn@web.de",
                        "dreisigacker.finn@web.de"),
          " – am besten mit der Datei im Anhang, dann schaue ich es mir an.",
          shiny::tags$br(),
          shiny::tags$small(style = "color:#94a3b8;",
            paste0("Technische Details: ", conditionMessage(e)))),
        type = "error", duration = 15)
    }

    # -- Datei-Upload -----------------------------------------
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      fp <- input$file$datapath; fname <- input$file$name
      shiny::withProgress(message = "Lade Datei ...", value = 0.3, {
        spiro <- tryCatch(load_spiro_file(fp, fname),
          error = function(e) { notify_parse_error(e); NULL })
        shiny::setProgress(0.7)
        if (is.null(spiro)) return()
        p <- tryCatch(extract_params(spiro, fname),
          error = function(e) { notify_parse_error(e); NULL })
        shiny::setProgress(1)
        if (!is.null(p)) params_r(p)
      })
    })

    shiny::observeEvent(input$load_demo, {
      shiny::req(identical(input$load_demo, "demo"))
      fname <- "data_demo.xlsx"
      fp <- system.file("extdata", fname, package = "SpiroAnalyzer")
      if (!nzchar(fp)) fp <- file.path("inst", "extdata", fname)
      shiny::req(file.exists(fp))
      shiny::withProgress(message = "Lade Demo-Daten ...", value = 0.3, {
        spiro <- tryCatch(load_spiro_file(fp, fname),
          error = function(e) { notify_parse_error(e); NULL })
        shiny::setProgress(0.7)
        if (is.null(spiro)) return()
        p <- tryCatch(extract_params(spiro, fname),
          error = function(e) { notify_parse_error(e); NULL })
        shiny::setProgress(1)
        if (!is.null(p)) {
          # Datei-Basename als ID nicht aussagekräftig → den Datei-internen
          # ID-Wert (z.B. "Demo_1") bevorzugen, falls vorhanden.
          if (!is.null(spiro$meta$ID) && nzchar(spiro$meta$ID))
            p$ID <- spiro$meta$ID
          params_r(p)
        }
      })
    })

    shiny::observeEvent(input$reset_data, { params_r(NULL) })

    # VO2max-Verifikations-Modul: liefert ggf. einen Override für VO2peak.
    # Wird VOR den abgeleiteten Reactives erstellt, damit
    # params_effective() den Override anwenden kann.
    vo2v_res <- mod_vo2max_verify_server("vo2v", params_reactive = params_r)

    # Wirksame Parameter: params_r mit ggf. übernommenem VO2peak-Override.
    # Alle Anzeigen (Übersicht, Header, Ausbelastung) und Exporte (CSV, Excel,
    # DOCX) lesen diesen Wrapper, damit ein einziger Override-Punkt reicht.
    params_effective <- shiny::reactive({
      p <- params_r()
      if (is.null(p)) return(NULL)
      ov <- vo2v_res()
      if (isTRUE(ov$active)) {
        p$VO2peak_abs <- ov$VO2peak_abs
        p$VO2peak_rel <- ov$VO2peak_rel
        p$VO2peak_source <- "user"
        p$VO2peak_window <- list(
          t_start_min = ov$t_start_min,
          t_end_min   = ov$t_end_min,
          window_sec  = ov$window_sec)
      } else {
        p$VO2peak_source <- "auto"
      }
      p
    })

    label_r <- shiny::reactive({
      p <- params_effective(); shiny::req(p)
      paste0(p$ID %||% "Messung", "_", p$Timepoint %||% "")
    })

    # ── Navigation + Seitenleisten-Zustand ─────────────────
    shiny::observeEvent(input$nav, bslib::nav_select("main", input$nav))
    shiny::observeEvent(params_r(), ignoreNULL = FALSE, {
      p <- params_r()
      session$sendCustomMessage("saState", list(
        loaded = !is.null(p),
        id   = if (is.null(p)) "" else as.character(p$ID %||% "Messung"),
        date = if (is.null(p) || is.na(p$Date)) "" else format(p$Date, "%d.%m.%Y"),
        page = if (is.null(p)) "upload" else "overview"))
    })

    st_badge <- function(ok) shiny::span(class = paste("sa-st", if (ok) "ok"),
                                         if (ok) "bestätigt" else "offen")

    # ── Uebersicht ───────────────────────────────────────────
    output$overview_tab <- shiny::renderUI({
      p <- params_effective(); shiny::req(p)
      bmi <- calc_bmi(p$Weight_kg, p$Height_cm)
      kpi <- function(label, val, unit, tag = NULL)
        shiny::div(class = "sa-kpi",
          shiny::div(class = "sa-kpi-l", label, if (!is.null(tag)) shiny::span(class = "sa-chip", tag)),
          shiny::div(class = "sa-kpi-v", if (is.null(val) || is.na(val)) "–" else val,
                     shiny::span(class = "sa-kpi-u", unit)))

      shiny::tagList(
        shiny::div(class = "sa-kpis",
          kpi("V'O₂peak relativ", fmt(p$VO2peak_rel, 1), "ml/min/kg", "30-s-Mittel"),
          kpi("V'O₂peak absolut", fmt(p$VO2peak_abs, 2), "L/min"),
          kpi("PPO", fmt(p$PPO, 0), "W", if (identical(p$PPO_interpolated, "Yes")) "interpoliert"),
          kpi("PPO relativ", fmt(p$PPO_wkg, 2), "W/kg"),
          kpi("HFmax", fmt(p$HRmax, 0), "1/min")),
        shiny::div(class = "ov-grid",
          shiny::div(class = "ov-card",
            shiny::tags$h5("Proband"),
            ov_row("ID", p$ID), ov_row("Zeitpunkt", p$Timepoint),
            ov_row("Datum", if (!is.na(p$Date)) format(p$Date, "%d.%m.%Y") else "-"),
            ov_row("Alter", fmt(p$Age_years, 0), "Jahre"),
            ov_row("Gewicht", fmt(p$Weight_kg, 1), "kg"),
            ov_row("Größe", fmt(p$Height_cm, 0), "cm"),
            ov_row("BMI", fmt(bmi, 1), "kg/m²")),
          shiny::div(class = "ov-card",
            shiny::tags$h5("Ausbelastung"),
            ov_row("Dauer", p$Duration %||% "-"),
            build_exhaust_ui(p),
            shiny::tags$h5(style = "margin-top:16px;", "Umgebung"),
            ov_row("Temperatur", fmt(p$Temperature, 1), "°C"),
            ov_row("Luftdruck", fmt(p$AirPressure, 0), "mbar"),
            ov_row("Luftfeuchte", fmt(p$Humidity, 0), "%")),
          shiny::div(class = "ov-card ov-wide",
            shiny::tags$h5("Ventilatorische Schwellen"),
            shiny::HTML(build_vt_overview(p)),
            shiny::tags$a(href = "#", class = "sa-link",
              onclick = "saGo('vt1'); return false;",
              "Schwellen prüfen und bestätigen", shiny::icon("arrow-right")))))
    })

    # ── Trainingsbereiche (Hansen et al. 2022) ──────────────
    output$zones_tab <- shiny::renderUI({
      p <- params_effective(); shiny::req(p)
      z <- calc_training_zones(p)
      cols <- c("#d1d5db", "#93C5FD", "#60A5FA", "#1D4ED8")
      pct <- function(lo, hi, what) shiny::span(class = "tz-pct",
        paste0(tz_range(lo, hi), " % ", what))
      rows <- lapply(seq_len(nrow(z)), function(i) shiny::tags$tr(
        shiny::tags$td(class = "tz-lv", shiny::tags$i(style = paste0("background:", cols[i])), z$name[i]),
        shiny::tags$td(tz_range(z$vo2_abs_lo[i], z$vo2_abs_hi[i], 1),
                       pct(z$vo2_lo[i], z$vo2_hi[i], "V\u0027O\u2082peak")),
        shiny::tags$td(tz_range(z$hr_abs_lo[i], z$hr_abs_hi[i]),
                       pct(z$hr_lo[i], z$hr_hi[i], "HFmax")),
        shiny::tags$td(tz_range(z$w_lo[i], z$w_hi[i])),
        shiny::tags$td(z$rpe[i]),
        shiny::tags$td(z$zone[i])))
      th <- function(t, u) shiny::tags$th(t, shiny::tags$small(u))
      shiny::div(class = "sa-card",
        shiny::div(class = "tz-ref",
          shiny::span("V\u0027O\u2082peak ", shiny::tags$b(fmt(p$VO2peak_rel, 1), "ml/min/kg")),
          shiny::span("HFmax ", shiny::tags$b(fmt(p$HRmax, 0), "1/min"))),
        shiny::div(class = "tz-scroll",
          shiny::tags$table(class = "tz",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("Intensität"), th("V\u0027O\u2082", "ml/min/kg"), th("HF", "1/min"),
              th("Leistung", "W, im Test erreicht"), th("RPE", "Borg 6\u201320"),
              shiny::tags$th("Trainingsbereich"))),
            shiny::tags$tbody(rows))),
        shiny::div(class = "tz-src",
          "Grenzen nach Hansen et al. (2022), Eur J Prev Cardiol 29(1), 230\u2013245, ",
          shiny::tags$a(href = "https://doi.org/10.1093/eurjpc/zwab007", target = "_blank",
                        "doi:10.1093/eurjpc/zwab007"),
          ". Leistung: Watt im Test, als V\u0027O\u2082 (geglättet) die jeweilige Grenze erstmals erreichte."))
    })

    # -- Zeitreihe-Plot ----------------------------------------
    plot_r <- shiny::reactive({
      p <- params_effective(); shiny::req(p, !is.null(p$ts), nrow(p$ts) > 0)
      single_ts_plot(ts = p$ts, smooth_n = input$np_smooth %||% 20, label = label_r())
    })
    output$ts_plot <- shiny::renderPlot({ plot_r() }, res = 120)

    # -- VT-Modul ----------------------------------------------
    vt_res <- mod_vt_server("vt", params_reactive = params_r)
    steps_res <- mod_steps_server("steps", params_reactive = params_r)
    dcheck_res <- mod_data_check_server("dcheck", params_reactive = params_r)
    output$st_vt1 <- shiny::renderUI(st_badge(isTRUE(vt_res$vt_state$vt1_confirmed)))
    output$st_vt2 <- shiny::renderUI(st_badge(isTRUE(vt_res$vt_state$vt2_confirmed)))
    output$st_vo2 <- shiny::renderUI(st_badge(isTRUE(vo2v_res()$active)))
    for (o in c("st_vt1", "st_vt2", "st_vo2"))
      shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
    vt_apply_counter <- shiny::reactiveVal(0L)
    shiny::observeEvent(
      if (!is.null(vt_res)) vt_res$apply_trigger() else NULL, {
      vt_apply_counter(shiny::isolate(vt_apply_counter()) + 1L)
      shiny::showNotification("VT-Werte in 9-Felder übernommen!", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # -- 9-Felder ----------------------------------------------
    shiny::observeEvent(input$np_all, {
      shiny::updateCheckboxGroupInput(session, "np_panels", selected = as.character(1:9))
    })
    shiny::observeEvent(input$np_none, {
      shiny::updateCheckboxGroupInput(session, "np_panels", selected = character(0))
    })

    np_plot_r <- shiny::reactive({
      p <- params_effective(); shiny::req(p, !is.null(p$ts), nrow(p$ts) > 0)
      sel <- input$np_panels; shiny::req(length(sel) > 0)
      vt_apply_counter()
      vt1_t <- p$vt1_time; vt2_t <- p$vt2_time; vt_d <- p$vt
      if (!is.null(vt_res) && !is.null(vt_res$vt_state)) {
        st <- vt_res$vt_state
        if (is.finite(st$vt1_time)) vt1_t <- st$vt1_time
        if (is.finite(st$vt2_time)) vt2_t <- st$vt2_time
        if (isTRUE(st$vt1_confirmed) && is.finite(st$vt1_final)) vt1_t <- st$vt1_final
        if (isTRUE(st$vt2_confirmed) && is.finite(st$vt2_final)) vt2_t <- st$vt2_final
      }
      nine_panel_assemble(
        ts = p$ts, smooth_n = input$np_smooth %||% 20, label = label_r(),
        vt = vt_d, vt1_time = vt1_t, vt2_time = vt2_t,
        panels = sel, ncol = as.integer(input$np_ncol %||% 3),
        arrangement = input$np_arr %||% "alt",
        show_warmup = isTRUE(input$np_warmup),
        show_recovery = isTRUE(input$np_recovery))
    })

    output$np_plot_ui <- shiny::renderUI({
      sel <- input$np_panels %||% as.character(1:9)
      nc  <- as.integer(input$np_ncol %||% 3)
      n_rows <- ceiling(max(length(sel), 1) / nc)
      shiny::plotOutput(ns("np_plot"), height = paste0(n_rows * 260 + 30, "px"))
    })
    output$np_plot <- shiny::renderPlot({
      p <- np_plot_r()
      if (inherits(p, "gtable") || inherits(p, "grob")) grid::grid.draw(p) else p
    }, res = 110)

    # -- Downloads ---------------------------------------------
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Plot.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = plot_r(), width = 10, height = 6, dpi = 600))
    # 9-Felder-PNG: feste Größe + DPI 600 (Export-Block aus Seitenleiste entfernt)
    output$dl_np_export <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_9Felder.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = np_plot_r(),
          width = 30, height = 25, units = "cm", dpi = 600))
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Parameter.csv"),
      content = function(file) {
        p <- params_effective()
        df <- tibble::tibble(
          Parameter = c("ID","Zeitpunkt","Gewicht_kg","Groesse_cm","Alter_Jahre","BMI",
            "PPO_W","PPO_Wkg","PPO_interpoliert","VO2peak_abs","VO2peak_rel",
            "RERmax","EQO2max","HRmax"),
          Wert = c(p$ID, p$Timepoint, p$Weight_kg, p$Height_cm, p$Age_years,
            calc_bmi(p$Weight_kg, p$Height_cm), p$PPO, p$PPO_wkg, p$PPO_interpolated,
            p$VO2peak_abs, p$VO2peak_rel, p$RERmax, p$EQO2max, p$HRmax))
        utils::write.csv2(df, file, row.names = FALSE)
      })
    # -- DOCX-Bericht ------------------------------------------
    output$dl_docx <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Bericht.docx"),
      content = function(file) {
        p <- params_effective(); shiny::req(p)

        # VT-Zeiten + Kommentare aus dem VT-Modul lesen
        vt1_t <- NA_real_; vt2_t <- NA_real_
        vt_comments <- list(vt1 = "", vt2 = "", general = "")
        if (!is.null(vt_res) && !is.null(vt_res$vt_state)) {
          st <- vt_res$vt_state
          if (isTRUE(st$vt1_confirmed) && is.finite(st$vt1_final)) vt1_t <- st$vt1_final
          else if (is.finite(st$vt1_time)) vt1_t <- st$vt1_time
          if (isTRUE(st$vt2_confirmed) && is.finite(st$vt2_final)) vt2_t <- st$vt2_final
          else if (is.finite(st$vt2_time)) vt2_t <- st$vt2_time
          vt_comments$vt1     <- st$vt1_comment     %||% ""
          vt_comments$vt2     <- st$vt2_comment     %||% ""
          vt_comments$general <- st$general_comment %||% ""
        }

        vt_tab    <- tryCatch(vt_res$vt_table(),  error = function(e) NULL)
        steps_tab <- tryCatch(steps_res$summary(), error = function(e) NULL)
        mfo_val   <- tryCatch(steps_res$mfo(),     error = function(e) NULL)
        adv_set   <- tryCatch(steps_res$advanced_settings(), error = function(e) list())
        adv_set$smooth_n <- input$np_smooth %||% 20

        shiny::withProgress(message = "Erstelle DOCX-Bericht …", value = 0.3, {
          export_docx(
            params            = p,
            vt1_time          = vt1_t,
            vt2_time          = vt2_t,
            vt_table          = vt_tab,
            steps_table       = steps_tab,
            mfo_result        = mfo_val,
            testleiter        = input$doc_testleiter %||% "",
            geraet            = input$doc_geraet     %||% "",
            kommentar         = input$doc_kommentar  %||% "",
            vt_comments       = vt_comments,
            advanced_settings = adv_set,
            include_mfo       = isTRUE(input$doc_include_mfo),
            file              = file)
          shiny::setProgress(1)
        })
      })

    return(params_r)
  })
}


# ============================================================
#  Helper functions
# ============================================================
fmt <- function(x, digits = 2) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("-")
  if (is.numeric(x)) format(round(x, digits), decimal.mark = ",", big.mark = ".")
  else as.character(x)
}

sa_stat <- function(val, label) {
  shiny::div(class = "sa-stat",
    shiny::div(class = "sa-stat-val", fmt(val)),
    shiny::div(class = "sa-stat-lbl", label))
}

ov_row <- function(key, val, unit = "") {
  shiny::div(class = "ov-row",
    shiny::div(class = "ov-key", key),
    shiny::div(class = "ov-val",
      if (!is.null(val) && !is.na(val) && nzchar(as.character(val)))
        paste0(val, if (nzchar(unit)) paste0(" ", unit) else "")
      else "-"))
}

peak_tile <- function(val, label, unit) {
  shiny::div(class = "ov-peak-tile",
    shiny::div(class = "pk-val", fmt(val)),
    shiny::div(class = "pk-lbl", paste0(label, if (nzchar(unit)) paste0(" (", unit, ")"))))
}

# ── BMI ──────────────────────────────────────────────────────
calc_bmi <- function(weight_kg, height_cm) {
  if (is.null(weight_kg) || is.null(height_cm) ||
      is.na(weight_kg) || is.na(height_cm) || height_cm <= 0)
    return(NA_real_)
  weight_kg / (height_cm / 100)^2
}

bmi_category <- function(bmi) {
  if (is.na(bmi)) return("-")
  if (bmi < 18.5) "Untergewicht"
  else if (bmi < 25) "Normalgewicht"
  else if (bmi < 30) "Uebergewicht"
  else "Adipositas"
}

bmi_color <- function(bmi) {
  if (is.na(bmi)) return("#888")
  if (bmi < 18.5) "#3b82f6"
  else if (bmi < 25) "#059669"
  else if (bmi < 30) "#d97706"
  else "#dc2626"
}

bmi_gauge_svg <- function(bmi) {
  bmi_clamped <- if (is.na(bmi)) 25 else max(14, min(42, bmi))
  angle <- (bmi_clamped - 14) / (42 - 14) * 180
  rad <- angle * pi / 180
  nx <- 90 - 70 * cos(rad); ny <- 85 - 70 * sin(rad)
  col <- bmi_color(bmi)
  paste0(
    '<svg viewBox="0 0 180 105" width="220" height="130" xmlns="http://www.w3.org/2000/svg">',
    '<path d="M 20 85 A 70 70 0 0 1 37.4 39.5" fill="none" stroke="#93c5fd" stroke-width="10" stroke-linecap="round"/>',
    '<path d="M 39 38 A 70 70 0 0 1 90 15"      fill="none" stroke="#6ee7b7" stroke-width="10" stroke-linecap="round"/>',
    '<path d="M 92 15 A 70 70 0 0 1 141 38"      fill="none" stroke="#fcd34d" stroke-width="10" stroke-linecap="round"/>',
    '<path d="M 143 39.5 A 70 70 0 0 1 160 85"   fill="none" stroke="#fca5a5" stroke-width="10" stroke-linecap="round"/>',
    '<text x="15" y="100" font-size="7" fill="#94a3b8" font-weight="600">14</text>',
    '<text x="155" y="100" font-size="7" fill="#94a3b8" font-weight="600">42</text>',
    '<text x="26" y="34" font-size="6" fill="#93c5fd" font-weight="600">18.5</text>',
    '<text x="82" y="12" font-size="6" fill="#6ee7b7" font-weight="600">25</text>',
    '<text x="139" y="34" font-size="6" fill="#fcd34d" font-weight="600">30</text>',
    if (!is.na(bmi)) paste0(
      '<line x1="90" y1="85" x2="', round(nx,1), '" y2="', round(ny,1),
      '" stroke="', col, '" stroke-width="2.5" stroke-linecap="round"/>',
      '<circle cx="90" cy="85" r="4" fill="', col, '"/>') else '',
    '</svg>')
}

# ── WHO-BMI-Klassifikationstabelle (WHO 2000) ───────────────
# Zeigt die Gewichtsklassen; die zum aktuellen BMI passende Zeile wird
# hervorgehoben. Reine Referenztabelle, keine wertende Aussage.
bmi_who_table <- function(bmi) {
  cls <- list(
    c("Untergewicht",    "&lt; 18,5"),
    c("Normalgewicht",   "18,5 – 24,9"),
    c("Präadipositas",   "25,0 – 29,9"),
    c("Adipositas I",    "30,0 – 34,9"),
    c("Adipositas II",   "35,0 – 39,9"),
    c("Adipositas III",  "&ge; 40,0"))
  active <- if (is.na(bmi)) 0L else if (bmi < 18.5) 1L else if (bmi < 25) 2L else
    if (bmi < 30) 3L else if (bmi < 35) 4L else if (bmi < 40) 5L else 6L
  rows <- vapply(seq_along(cls), function(i) {
    hl <- if (i == active)
      " style=\"background:#e8f1fb; font-weight:600; color:#1d1d1f;\"" else ""
    paste0("<tr", hl, "><td style=\"padding:2px 8px 2px 0;\">", cls[[i]][1],
           "</td><td style=\"padding:2px 0; text-align:right; color:#475569;\">",
           cls[[i]][2], "</td></tr>")
  }, character(1))
  paste0("<table style=\"width:100%; font-size:0.78rem; border-collapse:collapse;\">",
         "<tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

# ── Ausbelastung – Kennwerte ohne wertende Aussage ──────────
# Zeigt RERmax, EQO2max, HRmax als Werte plus eine neutrale Referenz-
# Zeile der gängigen Kriterien (kein "ausbelastet ja/nein"-Verdikt).
build_exhaust_ui <- function(p) {
  shiny::tagList(
    ov_row("RERmax", fmt(p$RERmax)),
    ov_row("EQO2max", fmt(p$EQO2max, 0)),
    ov_row("HRmax", fmt(p$HRmax, 0), "bpm"))
}

ov_row_badge <- function(label, value, ok) {
  shiny::div(class = "ov-row",
    shiny::div(class = "ov-key", label),
    shiny::div(style = "display:flex; align-items:center; gap:8px;",
      shiny::div(class = "ov-val", value),
      if (ok) shiny::span(class = "exhaust-badge-yes", "Ja")
      else    shiny::span(class = "exhaust-badge-no", "Nein")))
}

# ── VT overview detail table ────────────────────────────────
build_vt_overview <- function(p) {
  ts <- p$ts
  if (is.null(ts) || nrow(ts) == 0) return("<p style='color:#888;'>Keine Daten</p>")

  # Get VT times (from Excel or auto-calc)
  t1 <- if (is.finite(p$vt1_time %||% NA)) p$vt1_time else {
    idx <- tryCatch(auto_vt1(ts), error = function(e) NA_integer_)
    vt_idx_to_time(ts, idx)
  }
  t2 <- if (is.finite(p$vt2_time %||% NA)) p$vt2_time else {
    idx <- tryCatch(auto_vt2(ts), error = function(e) NA_integer_)
    vt_idx_to_time(ts, idx)
  }

  vt_val <- function(time_val, col, digits = 2) {
    if (!is.finite(time_val)) return("-")
    v <- vt_mean_at(ts, col, time_val)
    if (is.finite(v)) fmt(v, digits) else "-"
  }
  # Leistung zwischen Stufen linear interpoliert (ganze Zahl)
  vt_pwr <- function(time_val) {
    v <- interp_vt_power(ts, time_val)
    if (is.finite(v)) as.character(round(v, 0)) else "-"
  }

  # Build HTML table
  rows <- list(
    c("Zeit (min)", if (is.finite(t1)) fmt(t1, 2) else "-",
                    if (is.finite(t2)) fmt(t2, 2) else "-"),
    c("Power (W)", vt_pwr(t1), vt_pwr(t2)),
    c("VO2 (L/min)", vt_val(t1, "VO2abs"), vt_val(t2, "VO2abs")),
    c("HR (bpm)", vt_val(t1, "HR", 0), vt_val(t2, "HR", 0)),
    c("RER", vt_val(t1, "RER"), vt_val(t2, "RER"))
  )

  html <- '<table class="vt-mini-tbl"><thead><tr><th></th><th class="vt1-col">VT1</th><th class="vt2-col">VT2</th></tr></thead><tbody>'
  for (r in rows) {
    html <- paste0(html, '<tr><td>', r[1], '</td><td>', r[2], '</td><td>', r[3], '</td></tr>')
  }
  paste0(html, '</tbody></table>')
}

# ── Literatur helpers ────────────────────────────────────────
lit_item <- function(authors, title, extra = "", url = "") {
  shiny::div(class = "lit-item",
    shiny::div(class = "lit-authors", authors),
    if (nzchar(url)) shiny::tags$a(href = url, target = "_blank", title)
    else shiny::strong(title),
    if (nzchar(extra)) shiny::div(style = "font-size:0.78rem; color:#64748b;", extra))
}

lit_item_link <- function(title, desc, url) {
  shiny::div(class = "lit-item",
    shiny::tags$a(href = url, target = "_blank", title),
    shiny::div(style = "font-size:0.78rem; color:#64748b;", desc))
}

# Legacy (falls anderswo referenziert)
make_meta_card <- function(p) {
  badge <- function(val, lbl) shiny::span(lbl, ": ", shiny::strong(fmt(val)), style = "display:block; margin-bottom:4px;")
  shiny::tagList(
    shiny::h6(shiny::icon("person"), " Proband"),
    badge(p$ID, "ID"), badge(p$Timepoint, "Zeitpunkt"),
    badge(p$Weight_kg, "Gewicht (kg)"), badge(p$Height_cm, "Größe (cm)"),
    badge(p$Age_years, "Alter (Jahre)"), shiny::hr(),
    shiny::h6(shiny::icon("trophy"), " Peak-Werte"),
    badge(p$PPO, "PPO (W)"), badge(p$VO2peak_rel, "VO2peak rel"),
    badge(p$RERmax, "RERmax"), badge(p$EQO2max, "EQO2max"))
}
