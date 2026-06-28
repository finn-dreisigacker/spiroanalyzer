# ============================================================
#  mod_single.R -- Einzelansicht (v8 - Verification + Literatur)
# ============================================================

mod_single_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # -- CSS ---------------------------------------------------
    shiny::tags$style(shiny::HTML(paste0("

      /* ── Upload-Seite ────────────────────────────────── */
      .upload-page { max-width: 700px; margin: 40px auto; text-align: center; }
      .upload-page .upload-title { font-size: 1.8rem; font-weight: 800; color: #1f3d6b; margin-bottom: 8px; }
      .upload-page .upload-sub { font-size: 0.95rem; color: #5b7fa6; margin-bottom: 32px; }
      .upload-dropzone {
        border: 2.5px dashed #b0c4de; border-radius: 18px;
        padding: 48px 32px 36px;
        background: linear-gradient(135deg, #f8fbff 0%, #edf4ff 100%);
        transition: border-color 0.2s, background 0.2s, box-shadow 0.2s;
        cursor: pointer; position: relative;
      }
      .upload-dropzone:hover, .upload-dropzone.dragover {
        border-color: #2563eb;
        background: linear-gradient(135deg, #edf4ff 0%, #dbeafe 100%);
        box-shadow: 0 0 24px rgba(37,99,235,0.10);
      }
      .upload-dropzone .upload-icon { font-size: 2.8rem; color: #2563eb; margin-bottom: 14px; }
      .upload-dropzone .upload-label { font-size: 1.05rem; font-weight: 600; color: #1f3d6b; margin-bottom: 6px; }
      .upload-dropzone .upload-hint  { font-size: 0.82rem; color: #8899aa; }
      .upload-dropzone .btn-browse {
        display: inline-block; margin-top: 16px;
        background: linear-gradient(135deg,#2563eb,#1d4ed8);
        color: #fff; border: none; border-radius: 8px;
        padding: 10px 28px; font-weight: 700; font-size: 0.92rem;
        cursor: pointer; transition: transform 0.15s, box-shadow 0.15s;
      }
      .upload-dropzone .btn-browse:hover { transform: translateY(-1px); box-shadow: 0 4px 14px rgba(37,99,235,0.25); }
      #", ns("file_wrapper"), " .form-group { margin-bottom: 0; }
      #", ns("file_wrapper"), " .shiny-input-container { width: 100% !important; }
      #", ns("file_wrapper"), " label.control-label { display: none !important; }
      #", ns("file_wrapper"), " .input-group { display: none !important; }
      .upload-divider { display: flex; align-items: center; gap: 16px; margin: 28px 0; color: #8899aa; font-size: 0.85rem; font-weight: 600; }
      .upload-divider::before, .upload-divider::after { content: ''; flex: 1; height: 1px; background: #d0dbe8; }
      .demo-cards { display: flex; gap: 16px; justify-content: center; flex-wrap: wrap; }
      .demo-card {
        background: #fff; border: 1.5px solid #d9e4f5; border-radius: 14px;
        padding: 22px 28px; min-width: 200px; flex: 1; max-width: 280px;
        text-align: center; cursor: pointer;
        transition: transform 0.18s, box-shadow 0.18s, border-color 0.18s;
        box-shadow: 0 3px 10px rgba(31,61,107,0.06);
      }
      .demo-card:hover { transform: translateY(-3px); box-shadow: 0 7px 22px rgba(31,61,107,0.13); border-color: #2563eb; }
      .demo-card .demo-icon { font-size: 1.6rem; margin-bottom: 8px; }
      .demo-card .demo-name { font-weight: 700; color: #1f3d6b; font-size: 0.95rem; margin-bottom: 4px; }
      .demo-card .demo-desc { font-size: 0.78rem; color: #8899aa; }
      .demo-card-pre  .demo-icon { color: #2563eb; }
      .demo-card-post .demo-icon { color: #059669; }

      /* ── Header-Bar ──────────────────────────────────── */
      .sa-header {
        background: linear-gradient(135deg, #0b1220 0%, #1a3a5c 55%, #0b2240 100%);
        border-radius: 14px; padding: 16px 24px; margin-bottom: 14px;
        display: flex; align-items: center; gap: 20px; flex-wrap: wrap;
        position: relative; overflow: hidden;
      }
      .sa-header::before { content:''; position:absolute; inset:0; background:radial-gradient(circle at 80% 30%,rgba(125,211,252,.10) 0%,transparent 60%); pointer-events:none; }
      .sa-header-back { background: rgba(255,255,255,0.12); border: 1px solid rgba(255,255,255,0.2); color: #fff; border-radius: 8px; padding: 6px 10px; cursor: pointer; font-size: 0.85rem; transition: background 0.15s; }
      .sa-header-back:hover { background: rgba(255,255,255,0.25); color: #fff; }
      .sa-header-id { font-size: 1.15rem; font-weight: 800; color: #f0f8ff; display: flex; align-items: center; gap: 8px; }
      .sa-header-tp { background: rgba(125,211,252,0.2); border: 1px solid rgba(125,211,252,0.3); border-radius: 6px; padding: 2px 10px; font-size: 0.78rem; font-weight: 700; color: #7dd3fc; }
      .sa-header-stats { display: flex; gap: 16px; flex-wrap: wrap; margin-left: auto; }
      .sa-stat { text-align: center; min-width: 64px; }
      .sa-stat-val { font-size: 1.05rem; font-weight: 800; color: #f0f8ff; line-height: 1.2; }
      .sa-stat-lbl { font-size: 0.68rem; color: #7da8c9; font-weight: 600; text-transform: uppercase; letter-spacing: 0.04em; }

      /* ── Uebersicht ──────────────────────────────────── */
      .ov-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; }
      @media (max-width: 992px) { .ov-grid { grid-template-columns: 1fr 1fr; } }
      .ov-card { background: #fff; border: 1px solid #d9e4f5; border-radius: 14px; padding: 20px; box-shadow: 0 3px 12px rgba(31,61,107,0.06); }
      .ov-card h5 { font-size: 0.88rem; font-weight: 800; color: #1f3d6b; margin: 0 0 14px; padding-bottom: 8px; border-bottom: 2px solid #e5eefa; }
      .ov-row { display: flex; justify-content: space-between; padding: 5px 0; border-bottom: 1px solid #f0f4fb; font-size: 0.85rem; }
      .ov-row:last-child { border-bottom: none; }
      .ov-key { color: #5b7fa6; font-weight: 600; }
      .ov-val { color: #1f3d6b; font-weight: 700; text-align: right; }
      .ov-peak-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px; }
      .ov-peak-tile { background: linear-gradient(135deg, #f0f6ff, #e8f0fe); border-radius: 10px; padding: 14px 12px; text-align: center; border: 1px solid #d6e4fa; }
      .ov-peak-tile .pk-val { font-size: 1.2rem; font-weight: 800; color: #1f3d6b; line-height: 1.2; }
      .ov-peak-tile .pk-lbl { font-size: 0.72rem; color: #5b7fa6; font-weight: 600; margin-top: 2px; }
      .bmi-gauge-wrap { display: flex; flex-direction: column; align-items: center; padding: 10px 0 4px; }
      .exhaust-badge-yes { display: inline-block; background: #dcfce7; color: #166534; border-radius: 20px; padding: 2px 12px; font-weight: 700; font-size: 0.82rem; }
      .exhaust-badge-no  { display: inline-block; background: #fee2e2; color: #991b1b; border-radius: 20px; padding: 2px 12px; font-weight: 700; font-size: 0.82rem; }

      /* ── VT mini table ───────────────────────────────── */
      .vt-mini-tbl { width:100%; border-collapse:collapse; font-size:0.82rem; margin-top:8px; }
      .vt-mini-tbl th { background:#f0f6ff; color:#1f3d6b; font-weight:700; padding:5px 8px; text-align:left; border-bottom:2px solid #d6e4fa; }
      .vt-mini-tbl td { padding:4px 8px; border-bottom:1px solid #f0f4fb; color:#334155; }
      .vt-mini-tbl tr:last-child td { border-bottom:none; }
      .vt-mini-tbl .vt1-col { color:#006400; font-weight:700; }
      .vt-mini-tbl .vt2-col { color:#6B8E23; font-weight:700; }

      /* ── Literatur ───────────────────────────────────── */
      .lit-grid { display:grid; grid-template-columns: 1fr 1fr; gap: 16px; margin-top:12px; }
      @media (max-width: 768px) { .lit-grid { grid-template-columns: 1fr; } }
      .lit-card { background:#fff; border:1px solid #d9e4f5; border-radius:14px; padding:20px; box-shadow:0 3px 12px rgba(31,61,107,0.06); }
      .lit-card h5 { font-size:0.92rem; font-weight:800; color:#1f3d6b; margin:0 0 12px; padding-bottom:8px; border-bottom:2px solid #e5eefa; }
      .lit-item { padding:8px 0; border-bottom:1px solid #f0f4fb; font-size:0.84rem; }
      .lit-item:last-child { border-bottom:none; }
      .lit-item a { color:#2563eb; text-decoration:none; font-weight:600; }
      .lit-item a:hover { text-decoration:underline; }
      .lit-item .lit-authors { color:#8899aa; font-size:0.78rem; }
      .lit-formula { background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px; padding:10px 14px; margin:6px 0; font-family:monospace; font-size:0.84rem; color:#334155; }

      /* ── 9-Felder ────────────────────────────────────── */
      .np-fs-btn { position:absolute; top:8px; right:8px; z-index:20; background:#1f3d6b; color:#fff; border:none; border-radius:6px; padding:4px 10px; font-size:0.78rem; cursor:pointer; opacity:0.85; }
      .np-fs-btn:hover { opacity:1; }
      #", ns("np_wrapper"), ".np-fullscreen { position:fixed; top:0; left:0; width:100vw; height:100vh; z-index:9999; background:#fff; overflow-y:auto; padding:12px 20px; }
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
    #  SEITE 1: Upload
    # ════════════════════════════════════════════════════════════
    shiny::conditionalPanel(
      condition = paste0("!output['", ns("has_data"), "']"),
      shiny::div(class = "upload-page",
        shiny::div(class = "upload-title", shiny::icon("upload"), " Spiroergometrie-Datei laden"),
        shiny::div(class = "upload-sub",
          "MetaLyzer (XML/Excel) oder ZAN (xlsx) — das Format wird automatisch erkannt. ",
          "Alternativ stehen Demo-Datensätze bereit."),
        shiny::div(id = ns("dropzone"), class = "upload-dropzone",
          shiny::div(class = "upload-icon", shiny::icon("cloud-arrow-up")),
          shiny::div(class = "upload-label", "Datei hierher ziehen"),
          shiny::div(class = "upload-hint", "XML oder Excel (.xlsx / .xls)"),
          shiny::tags$button(id = ns("browse_btn"), class = "btn-browse", "Datei auswählen"),
          shiny::div(id = ns("file_wrapper"), style = "position:absolute; opacity:0; width:0; height:0; overflow:hidden;",
            shiny::fileInput(ns("file"), label = NULL, accept = c(".xlsx",".xls",".xml"), buttonLabel = "Datei", placeholder = ""))
        ),
        shiny::div(class = "upload-divider", "oder Demo-Datensatz verwenden"),
        shiny::div(class = "demo-cards",
          shiny::tags$div(class = "demo-card demo-card-pre",
            onclick = paste0("Shiny.setInputValue('", ns("load_demo"), "', 'demo', {priority: 'event'})"),
            shiny::div(class = "demo-icon", shiny::icon("flask")),
            shiny::div(class = "demo-name", "Demo Data"),
            shiny::div(class = "demo-desc", ""))
        )
      )
    ),

    # ════════════════════════════════════════════════════════════
    #  SEITE 2: Analyse (Full-Width)
    # ════════════════════════════════════════════════════════════
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),

      # Einheitliche Glättung: der "Glättung"-Regler im 9-Felder-Tab
      # (ns("np_smooth")) ist die EINZIGE Quelle für Zeitreihe,
      # 9-Felder und DOCX-Bericht.

      shiny::uiOutput(ns("header_bar")),

      bslib::navset_card_tab(

        # --- Uebersicht ------------------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("id-card"), " Übersicht"),
          shiny::uiOutput(ns("overview_tab"))),

        # --- Zeitreihe (im UI ausgeblendet, Code aktiv) -----------
        # bslib::nav_panel(shiny::tagList(shiny::icon("chart-line"), " Zeitreihe"),
        #   shinycssloaders::withSpinner(shiny::plotOutput(ns("ts_plot"), height = "480px"), type = 6)),

        # --- Datenüberprüfung ------------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("magnifying-glass-chart"),
          " Datenüberprüfung"),
          mod_data_check_ui(ns("dcheck"))),

        # --- 9-Felder --------------------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("grip"), " 9-Felder"),
          shiny::div(id = ns("np_wrapper"), style = "position:relative;",
            shiny::tags$button(class = "np-fs-btn",
              onclick = paste0("Shiny.setInputValue('", ns("np_fullscreen"), "', Math.random())"),
              shiny::icon("expand"), " Vollbild"),
            shiny::fluidRow(
              shiny::column(2,
                shiny::div(class = "sa-card", style = "font-size:0.82rem;",
                  shiny::tags$h6("Anordnung", style = "font-weight:700; color:#1f3d6b; margin-bottom:6px;"),
                  shiny::radioButtons(ns("np_arr"), label = NULL, inline = TRUE,
                    choices = c("Alt"="alt","Neu"="neu"), selected = "alt"),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Phasen", style = "font-weight:700; color:#1f3d6b; margin-bottom:4px;"),
                  shiny::checkboxInput(ns("np_warmup"), "Erwärmung", value = TRUE),
                  shiny::checkboxInput(ns("np_recovery"), "Erholung", value = TRUE),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Felder", style = "font-weight:700; color:#1f3d6b; margin-bottom:4px;"),
                  shiny::checkboxGroupInput(ns("np_panels"), label = NULL,
                    choiceNames = lapply(paste0(1:9, c(" V'E/AF"," HF/O2P"," VO2/VCO2"," V'E-V'CO2"," V-Slope"," Aeq."," VT-V'E"," RER"," Pet")),
                      function(x) shiny::HTML(paste0("<small>", x, "</small>"))),
                    choiceValues = as.character(1:9), selected = as.character(1:9)),
                  shiny::div(style = "display:flex; gap:4px; margin-bottom:4px;",
                    shiny::actionButton(ns("np_all"), "Alle", class = "btn-sm btn-outline-secondary", style = "flex:1;"),
                    shiny::actionButton(ns("np_none"), "Keine", class = "btn-sm btn-outline-secondary", style = "flex:1;")),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Spalten", style = "font-weight:700; color:#1f3d6b; margin-bottom:4px;"),
                  shiny::radioButtons(ns("np_ncol"), label = NULL, inline = TRUE,
                    choices = c("1"="1","2"="2","3"="3"), selected = "3"),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Glättung", style = "font-weight:700; color:#1f3d6b; margin-bottom:4px;"),
                  shiny::numericInput(ns("np_smooth"), label = NULL, value = 20, min = 5, max = 60, step = 1, width = "100%"),
                  shiny::hr(style = "margin:4px 0;"),
                  shiny::tags$h6("Export", style = "font-weight:700; color:#1f3d6b; margin-bottom:4px;"),
                  shiny::div(style = "display:flex; gap:4px; margin-bottom:4px;",
                    shiny::div(style = "flex:1;", shiny::numericInput(ns("np_w"), "B(cm)", value = 30, min = 10, max = 60, step = 1, width = "100%")),
                    shiny::div(style = "flex:1;", shiny::numericInput(ns("np_h"), "H(cm)", value = 25, min = 8, max = 50, step = 1, width = "100%"))),
                  shiny::numericInput(ns("np_dpi"), "DPI", value = 600, min = 150, max = 1200, step = 50, width = "100%"),
                  shiny::downloadButton(ns("dl_np"), "PNG", class = "btn-outline-primary w-100 mt-1")
                )
              ),
              shiny::column(10, shinycssloaders::withSpinner(shiny::uiOutput(ns("np_plot_ui")), type = 6))
            )
          )
        ),

        # --- VT-Analyse ------------------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("scissors"), " VT-Analyse"),
          mod_vt_ui(ns("vt"))),

        # --- VO2max-Verifikation (aus UI ausgeblendet; Code aktiv) -
        # bslib::nav_panel(shiny::tagList(shiny::icon("crosshairs"),
        #   " VO2max-Verifikation"),
        #   mod_vo2max_verify_ui(ns("vo2v"))),

        # --- Stufenzusammenfassung -------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("layer-group"),
          " Stufenzusammenfassung"),
          mod_steps_ui(ns("steps"))),

        # --- Export ----------------------------------------------
        bslib::nav_panel(shiny::tagList(shiny::icon("download"), " Export"),
          shiny::div(style = "padding:20px 0;",
            # Metadaten-Formular für DOCX-Bericht
            shiny::div(class = "sa-card", style = "margin-bottom:18px;",
              shiny::tags$h4(shiny::icon("file-word"),
                " Bericht (DOCX) — Angaben"),
              shiny::tags$p(style = "color:#64748b; font-size:0.85rem; margin-bottom:12px;",
                "Diese Angaben überschreiben die Platzhalter in der Vorlage. Frei lassen, um die Defaults zu behalten."),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::textInput(ns("doc_testleiter"), "Testleiter:in",
                    value = "", placeholder = "Name eintragen…"),
                  shiny::textInput(ns("doc_geraet"), "Gerät",
                    value = "", placeholder = "z.B. Cortex MetaLyzer 3B")),
                shiny::column(6,
                  shiny::textAreaInput(ns("doc_kommentar"), "Allgemeiner Kommentar",
                    value = "",
                    placeholder = "Optionale Anmerkung für den Bericht…",
                    rows = 4, resize = "vertical"))
              )
            ),
            # Download-Buttons
            shiny::div(style = "display:flex; gap:12px; flex-wrap:wrap;",
              shiny::downloadButton(ns("dl_docx"), "Bericht DOCX",
                class = "btn-primary"),
              shiny::downloadButton(ns("dl_np_export"), "9-Felder PNG",
                class = "btn-outline-primary"),
              shiny::downloadButton(ns("dl_plot"), "Zeitreihe PNG",
                class = "btn-outline-secondary"),
              shiny::downloadButton(ns("dl_csv"), "Parameter CSV",
                class = "btn-outline-secondary"))))
      )
    )
  )
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

    # -- Datei-Upload -----------------------------------------
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      fp <- input$file$datapath; fname <- input$file$name
      shiny::withProgress(message = "Lade Datei ...", value = 0.3, {
        spiro <- tryCatch(load_spiro_file(fp, fname),
          error = function(e) { shiny::showNotification(paste("Fehler:", e$message), type = "error"); NULL })
        shiny::setProgress(0.7)
        if (is.null(spiro)) return()
        p <- tryCatch(extract_params(spiro, fname),
          error = function(e) { shiny::showNotification(paste("Fehler:", e$message), type = "error"); NULL })
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
          error = function(e) { shiny::showNotification(paste("Fehler:", e$message), type = "error"); NULL })
        shiny::setProgress(0.7)
        if (is.null(spiro)) return()
        p <- tryCatch(extract_params(spiro, fname),
          error = function(e) { shiny::showNotification(paste("Fehler:", e$message), type = "error"); NULL })
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

    # ── Header-Bar ───────────────────────────────────────────
    output$header_bar <- shiny::renderUI({
      p <- params_effective(); shiny::req(p)
      shiny::div(class = "sa-header",
        shiny::actionButton(ns("reset_data"), label = NULL,
          icon = shiny::icon("arrow-left"), class = "sa-header-back",
          title = "Zurück zum Upload"),
        shiny::div(class = "sa-header-id",
          shiny::icon("person"), p$ID %||% "Messung",
          if (!is.null(p$Timepoint) && nzchar(p$Timepoint %||% ""))
            shiny::span(class = "sa-header-tp", p$Timepoint)),
        shiny::div(class = "sa-header-stats",
          sa_stat(p$VO2peak_rel, "VO2peak"),
          sa_stat(p$PPO, "PPO (W)"),
          sa_stat(p$RERmax, "RERmax"),
          sa_stat(p$HRmax, "HRmax"),
          sa_stat(p$EQO2max, "EQO2max"))
      )
    })

    # ── Uebersicht-Tab ───────────────────────────────────────
    output$overview_tab <- shiny::renderUI({
      p <- params_effective(); shiny::req(p)
      bmi <- calc_bmi(p$Weight_kg, p$Height_cm)

      # VT detail lookup (from params ts, Belastung only)
      vt_detail <- build_vt_overview(p)

      shiny::div(class = "ov-grid", style = "margin-top:12px;",
        # Card 1: Proband
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("person"), " Proband"),
          ov_row("ID", p$ID), ov_row("Zeitpunkt", p$Timepoint),
          ov_row("Datum", if (!is.na(p$Date)) format(p$Date, "%d.%m.%Y") else "-"),
          ov_row("Alter", fmt(p$Age_years, 0), "Jahre"),
          ov_row("Gewicht", fmt(p$Weight_kg, 1), "kg"),
          ov_row("Größe", fmt(p$Height_cm, 0), "cm")),

        # Card 2: BMI
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("weight-scale"), " BMI"),
          shiny::div(class = "bmi-gauge-wrap",
            shiny::HTML(bmi_gauge_svg(bmi)),
            shiny::div(style = "font-size:1.5rem; font-weight:800; color:#1f3d6b; margin-top:4px;",
              if (!is.na(bmi)) paste0(format(round(bmi, 1), nsmall = 1), " kg/m\u00b2") else "-"),
            shiny::div(style = paste0("font-size:0.82rem; font-weight:600; margin-top:2px; color:", bmi_color(bmi), ";"),
              bmi_category(bmi)))),

        # Card 3: Peak-Werte
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("trophy"), " Peak-Werte"),
          shiny::div(class = "ov-peak-grid",
            peak_tile(p$VO2peak_rel, "VO2peak rel", "ml/min/kg"),
            peak_tile(p$VO2peak_abs, "VO2peak abs", "L/min"),
            peak_tile(p$PPO, "PPO", "W"),
            peak_tile(p$PPO_wkg, "PPO", "W/kg"),
            peak_tile(p$RERmax, "RERmax", ""),
            peak_tile(p$HRmax, "HRmax", "bpm"))),

        # Card 4: Ausbelastung
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("fire"), " Ausbelastung"),
          build_exhaust_ui(p)),

        # Card 5: Umgebung
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("thermometer-half"), " Umgebung"),
          ov_row("Temperatur", fmt(p$Temperature, 1), "\u00b0C"),
          ov_row("Luftdruck", fmt(p$AirPressure, 0), "mmHg"),
          ov_row("Luftfeuchte", fmt(p$Humidity, 0), "%"),
          ov_row("Dauer", p$Duration %||% "-")),

        # Card 6: VT / Schwellen mit Detail-Tabelle
        shiny::div(class = "ov-card",
          shiny::tags$h5(shiny::icon("scissors"), " Ventilatorische Schwellen"),
          shiny::HTML(vt_detail),
          shiny::div(style = "margin-top:10px; font-size:0.78rem; color:#8899aa;",
            shiny::icon("arrow-right"), " Interaktive Bestimmung im Tab \"VT-Analyse\""))
      )
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

    output$dl_np <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_9Felder.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = np_plot_r(),
          width = input$np_w %||% 30, height = input$np_h %||% 25,
          units = "cm", dpi = input$np_dpi %||% 600))


    # -- Downloads ---------------------------------------------
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_Plot.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = plot_r(), width = 10, height = 6, dpi = 600))
    output$dl_np_export <- shiny::downloadHandler(
      filename = function() paste0(label_r(), "_9Felder.png"),
      content = function(file)
        ggplot2::ggsave(file, plot = np_plot_r(),
          width = input$np_w %||% 30, height = input$np_h %||% 25,
          units = "cm", dpi = input$np_dpi %||% 600))
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

# ── Ausbelastung ─────────────────────────────────────────────
build_exhaust_ui <- function(p) {
  HFcalc <- if (!is.na(p$Age_years)) 200 - p$Age_years else NA
  crit_RER <- !is.na(p$RERmax)  && p$RERmax  > 1.1
  crit_EQ  <- !is.na(p$EQO2max) && p$EQO2max > 35
  crit_HF  <- !is.na(p$HRmax) && !is.na(HFcalc) && p$HRmax > HFcalc
  crit_sum <- sum(c(crit_RER, crit_EQ, crit_HF), na.rm = TRUE)
  exhausted <- crit_sum >= 2
  shiny::tagList(
    ov_row_badge("RER > 1,1", fmt(p$RERmax), crit_RER),
    ov_row_badge("EQO2 > 35", fmt(p$EQO2max, 0), crit_EQ),
    ov_row_badge(paste0("HF > ", if (!is.na(HFcalc)) HFcalc else "?"), fmt(p$HRmax, 0), crit_HF),
    shiny::div(style = "margin-top:10px; text-align:center;",
      if (exhausted) shiny::div(style = "font-size:0.95rem; font-weight:800; color:#166534;",
          shiny::icon("circle-check"), " Ausbelastet")
      else shiny::div(style = "font-size:0.95rem; font-weight:800; color:#991b1b;",
          shiny::icon("circle-xmark"), " Nicht ausbelastet"),
      shiny::div(style = "font-size:0.75rem; color:#8899aa; margin-top:2px;",
        paste0(crit_sum, " von 3 Kriterien erfuellt"))))
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

  vt_val <- function(time_val, col) {
    if (!is.finite(time_val)) return("-")
    idx <- which.min(abs(ts$time_min - time_val))
    if (length(idx) != 1 || !col %in% names(ts)) return("-")
    v <- ts[[col]][idx]
    if (is.finite(v)) fmt(v) else "-"
  }

  # Build HTML table
  rows <- list(
    c("Zeit (min)", if (is.finite(t1)) fmt(t1, 2) else "-",
                    if (is.finite(t2)) fmt(t2, 2) else "-"),
    c("Power (W)", vt_val(t1, "P"), vt_val(t2, "P")),
    c("VO2 (L/min)", vt_val(t1, "VO2abs"), vt_val(t2, "VO2abs")),
    c("HR (bpm)", vt_val(t1, "HR"), vt_val(t2, "HR")),
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
