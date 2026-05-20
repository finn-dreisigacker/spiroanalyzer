# SpiroAnalyzer

**Shiny-App (golem) zur Analyse und zum Vergleich von Spiroergometrie-Messungen**

---

## Schnellstart

```r
# Einfach starten (installiert fehlende Pakete automatisch):
source("start.R")
```

Alternativ als R-Paket nutzen:
```r
# Im SpiroAnalyzer-Verzeichnis:
devtools::load_all()
run_app()
```

---

## Funktionen

| Tab | Beschreibung |
|-----|-------------|
| **Einzelanalyse** | XML oder Excel hochladen → Zeitreihenplot + Parametertabelle + Ausbelastungskriterien |
| **T0 / T10 Vergleich** | Zwei Dateien hochladen → Dual-Achsen-Plot (VO2/kg + Power) + Δ-Tabelle |
| **Export** | Plot als PNG (300 dpi), Parameter als CSV |

---

## Unterstützte Dateiformate

| Format | Beschreibung |
|--------|-------------|
| `.xlsx` | Excel-Export aus der Spiro-Software (HealthFit-Format) |
| `.xml`  | XML-Export (SpiroSoft o. ä., `ss:Row`/`ss:Cell`-Namespace) |

---

## Berechnete Parameter

- **PPO** (Peak Power Output) – mit Interpolation bei letzter Stufe < 30 s
- **VO2peak** absolut (L/min) und relativ (ml/min/kg)
- **RERmax** – Respiratorischer Quotient
- **EQO2max** – Ventilatorische Äquivalent
- **HRmax** – maximale Herzfrequenz
- **Ausbelastungskriterien**: RER > 1.1, EQO2 > 35, HF > 200 − Alter

---

## Projektstruktur (golem)

```
SpiroAnalyzer/
├── R/
│   ├── app_ui.R          # Haupt-UI (bslib navbar)
│   ├── app_server.R      # Haupt-Server
│   ├── fct_data.R        # Daten einlesen & Parameter berechnen
│   ├── fct_plots.R       # ggplot2 Plots
│   ├── mod_single.R      # Modul: Einzelanalyse
│   └── mod_compare.R     # Modul: Vergleich
├── inst/app/www/         # Statische Assets (CSS, Icons)
├── DESCRIPTION
├── start.R               # Schnellstart (ohne Paket-Installation)
└── run_app.R             # golem-konformer Starter
```

---

## Benötigte Pakete

```r
install.packages(c(
  "shiny", "bslib", "golem", "openxlsx", "readxl",
  "xml2", "dplyr", "tidyr", "stringr", "lubridate",
  "zoo", "ggplot2", "hms", "DT", "shinycssloaders",
  "purrr", "tibble"
))
```

---

## Plot-Beschreibung

Der Vergleichsplot (T0 vs. T10) zeigt:

- **Ribbon** (blau/orange): Power-Verlauf (skaliert auf linke VO2-Achse)
- **Linie** (blau/orange): VO2/kg geglättet (gleitendes Mittel, Standard: 20 Punkte)
- **Gestrichelte Linie links**: VO2peak mit Beschriftung
- **Gestrichelte Linie rechts**: PPO mit Beschriftung (W)
- **Zweite Y-Achse** (rechts): Power in Watt
