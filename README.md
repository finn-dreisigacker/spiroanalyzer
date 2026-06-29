# SpiroAnalyzer

**Shiny-App (golem) zur Analyse von Spiroergometrie-/CPET-Messungen mit
wissenschaftlichem Word-Bericht (Quarto)**

---

## Schnellstart

```r
# Einfach starten (installiert fehlende Pakete automatisch):
source("start.R")
```

Alternativ als R-Paket:

```r
# Im SpiroAnalyzer-Verzeichnis:
devtools::load_all()
run_app()
```

Für den DOCX-Export wird zusätzlich [Quarto](https://quarto.org) benötigt
(muss im PATH verfügbar sein).

---

## Funktionen

| Tab | Beschreibung |
|-----|-------------|
| **Start** | Datei hochladen → Übersicht, Datenüberprüfung, 9-Felder-Grafik, VT-Analyse, Stufenzusammenfassung und Export |
| **Pre-Post-Vergleich** | T0/T10-Vergleich – derzeit *coming soon* (Code vorhanden, im UI deaktiviert) |
| **Info** | Autor, App-Infos, Zitation |

### Start-Tab im Detail

- **Übersicht** – Proband, BMI (Wert + WHO-Klassifikation), Peak-Werte,
  Ausbelastungs-Kennwerte, Umgebung, ventilatorische Schwellen.
- **Datenüberprüfung** – Gesamtverlauf der Atemgase (Ausreißer-Erkennung
  und Stufenübersicht: *in Entwicklung*).
- **9-Felder-Grafik** – Wasserman-Panel (klassische/ÖGP-2024-Anordnung),
  einstellbare Glättung, VT- und Belastungsstart-Linien, PNG-Export.
- **Auswertung** – interaktive Bestimmung von VT1 (V-Slope, Excess CO₂,
  Atemäquivalente, PetO₂/PetCO₂) und VT2 (V'E/V'CO₂, Excess V'E …) sowie
  **VO₂max-Bestätigung** über ein verschiebbares Mittelungsfenster
  (nur Belastungsphase; höchstes Fenster wird vorgeschlagen, per
  „Übernehmen" als offizieller VO₂peak gesetzt).
- **Stufenzusammenfassung** – Kennwerte je Stufe inkl. MFO/Fatmax
  (Polynom 2. Grades mit Funktionsgleichung und R²).
- **Export** – wissenschaftlicher DOCX-Bericht (Quarto), 9-Felder-PNG,
  Parameter-CSV.

VT- und VO₂peak-Leistungen, die zwischen zwei Stufen liegen, werden linear
interpoliert (Treppe → Rampe) und als ganze Zahl ausgewiesen.

---

## Word-Bericht (DOCX)

Der Export erzeugt über Quarto einen formatierten Bericht mit:

- APA-Tabellen (flextable, Arial) mit „Tab. N"-Überschriften
- 9-Felder-Grafik und allen VT-Bestimmungsplots
- Stufenübersicht im Querformat
- Kopf-/Fußzeile (officer-Referenzdokument): Proband-ID + Datum bzw.
  Seitenzahl
- Quellenangaben (BibTeX `reference.bib` + APA-CSL)

> **APA-CSL:** Lege deine `apa.csl` unter `inst/extdata/apa.csl` ab.
> Fehlt sie, rendert der Bericht mit dem Standard-Zitationsstil.

---

## Unterstützte Dateiformate

| Format | Beschreibung |
|--------|-------------|
| `.xlsx` | Excel-Export (Cortex MetaLyzer / MetaSoft, HealthFit, ZAN) |
| `.xml`  | MetaLyzer-XML-Export |

---

## Berechnete Parameter

- **PPO** (Peak Power Output) – mit Interpolation bei letzter Stufe < 30 s
- **VO₂peak** absolut (L/min) und relativ (ml/min/kg)
- **RERmax**, **EQO₂max**, **HRmax**
- **VT1/VT2** – ventilatorische Schwellen
- **MFO/Fatmax** – maximale Fettoxidation
- **Ausbelastungs-Kennwerte** – RER, EQO₂, HF (Referenz: RER > 1,1;
  EQO₂ > 35; HF > 200 − Alter)
- **BMI** – inkl. WHO-Klassifikation

---

## Projektstruktur (golem)

```
SpiroAnalyzer/
├── R/
│   ├── app_ui.R / app_server.R   # Haupt-UI/-Server (bslib navbar)
│   ├── fct_data.R                # Einlesen & Parameterberechnung
│   ├── fct_plots.R               # ggplot2: 9-Felder + VT-Plots
│   ├── fct_vt.R / fct_steps.R    # VT-Erkennung, Stufen, MFO
│   ├── fct_export.R              # DOCX-Export (Quarto) + Referenz-DOCX
│   ├── mod_single.R              # Modul: Start/Einzelanalyse
│   ├── mod_compare.R             # Modul: Pre-Post (coming soon)
│   ├── mod_vt_analysis.R         # Modul: interaktive VT-Analyse
│   └── mod_data_check.R          # Modul: Datenüberprüfung
├── inst/extdata/
│   ├── Bericht_Vorlage.qmd       # Quarto-Berichtsvorlage
│   ├── reference.bib             # Literatur (BibTeX)
│   └── data_demo.xlsx            # Demo-Datensatz
├── DESCRIPTION
├── start.R                       # Schnellstart
└── run_app.R                     # golem-Starter
```

---

## Benötigte Pakete

```r
install.packages(c(
  "shiny", "bslib", "golem", "openxlsx", "readxl",
  "xml2", "dplyr", "tidyr", "stringr", "lubridate",
  "zoo", "ggplot2", "patchwork", "hms", "DT", "plotly",
  "shinycssloaders", "purrr", "tibble",
  "flextable", "officer", "quarto"
))
```

Außerdem: **Quarto** (System-Installation) für den DOCX-Export.

---

## Zitation

> Dreisigacker, F. (2025). *SpiroAnalyzer – Spiroergometrie-Analyse*
> [R Shiny App]. https://github.com/finn-dreisigacker/spiroanalyzer
