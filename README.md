# WorldView

WorldView is a browser-based teaching application for exploring and analysing
[World Values Survey (WVS)](https://www.worldvaluessurvey.org/) Wave 7 data.
The student-facing application is a static website: students do not need R,
RStudio, a server-side application, or programming experience. R is used only
by the educator or maintainer to prepare the data files consumed by the website.

This repository is intentionally organised around two separate layers:

- **`worldview_deployment/`** — the static application that students use. It
  contains the current HTML, CSS, JavaScript, data assets, maps, phylogeny
  files, and browser validation page.
- **R build/configuration layer** — prepares a reproducible teaching subset
  from WVS Wave 7 and updates the data assets used by the static application.

The current student interface is treated as the reference implementation.
Rebuilding the data does not require rebuilding or rewriting the JavaScript
application.

## Repository structure

| Path | Purpose |
| --- | --- |
| `worldview_deployment/` | Complete static student-facing application. This is the directory to host. |
| `build_worldview.R` | Single entry point for rebuilding WorldView data assets. |
| `R/worldview_build.R` | Reusable data-processing, sampling, codebook, export, and validation functions. |
| `config/WVS7_codebook_index.xlsx` | Educator-facing variable configuration and WVS recoding metadata. |
| `config/worldview_config.csv` | Build settings, including sample size and random seed. |
| `WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds` | Source WVS Wave 7 R dataset used by the build. |
| `WVS_Dataset/F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf` | WVS questionnaire documentation retained with the project. |

Historical Shiny code, one-off development-step scripts, duplicate release
folders, diagnostics, and intermediate derived datasets are intentionally
excluded from the cleaned repository.

## Configure the teaching dataset

### 1. Choose the variables students can see

Open:

`config/WVS7_codebook_index.xlsx`

On the **Codebook index** sheet, edit the `Variable_Display_Logical` column:

- `TRUE` = include the variable in the student-facing WorldView build.
- `FALSE` = exclude it from the student-facing WorldView build.

The workbook also retains the display type, response metadata, and `R_recode`
expressions used to process WVS variables. The default workbook contains the
current 39-variable teaching set. The corrected mappings for the recently
added items are Q8-Q16 (the nine displayed child-quality items) and Q235
(strong leader).

A small number of very high-cardinality WVS variables (`Q223`, `Q266`,
`Q267`, `Q268`, `Q272`, and `Q290`) remain excluded by the build because they
are unsuitable for the current browser interface.

### 2. Configure participant sampling

Edit:

`config/worldview_config.csv`

The default settings are:

- `sampling_seed = 382`
- `maximum_participants_per_country = 1000`
- `data_version = WV7-WORLDVIEW-1.2.0`

WorldView samples independently within each country and retains all
respondents when a country contains fewer than the configured maximum. Using a
fixed seed makes the teaching sample reproducible.

## Rebuild WorldView

### Requirements

Install R and the following packages once:

```r
install.packages(c("readxl", "jsonlite", "dplyr"))
```

The source WVS RDS file must be present at:

`WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds`

From the repository root, run:

```bash
Rscript build_worldview.R
```

Or from RStudio:

```r
source(file.path("R", "worldview_build.R"))
build_worldview()
```

The build updates the static application's runtime data files in
`worldview_deployment/data/`, including:

- `worldview-browser-data-v1.0.0.json`
- `worldview-browser-data-v1.0.0.json.gz`
- `worldview-codebook-v1.0.0.json`
- `manifest-v1.0.0.json`

It also updates `worldview_deployment/deployment-checks.csv` and stops with an
error if core consistency checks fail.

## Preview locally

Because the app loads JSON files with `fetch()`, do not open `index.html`
directly from the filesystem.

From RStudio:

```r
install.packages("servr") # once only
servr::httd("worldview_deployment")
```

Open the local address printed by `servr`. The browser validation page is
available at `/validation.html`.

## Host for students

WorldView is a static site. To deploy it, publish the **contents of
`worldview_deployment/`** using a static hosting service such as GitHub Pages,
Netlify, an institutional web server, or another static host.

No server-side R process is required after the data have been built.

## Updating the app

For data/configuration changes, prefer changing the Excel/codebook
configuration and rebuilding rather than editing generated JSON manually.

For interface or analysis-code changes, edit files directly under
`worldview_deployment/`. There is intentionally only one maintained copy of
the static app in the cleaned repository.

Before publishing a front-end change, preview the site and open
`worldview_deployment/validation.html` to run the browser-side validation
checks.

## Licence and reuse

WorldView uses separate licences for software and original content:

- **Software code:** [MIT Licence](LICENSE).
- **Original WorldView website content, documentation, and original visual
  assets:** [Creative Commons Attribution 4.0 International
  (CC BY 4.0)](LICENSE-CONTENT.md).

See [ATTRIBUTION.md](ATTRIBUTION.md) for attribution and third-party material.

These licences do **not** apply to World Values Survey data or other WVS
materials. The repository does not relicense WVS content.

## World Values Survey data and attribution

The application uses
[World Values Survey Wave 7](https://www.worldvaluessurvey.org/) data.
Anyone redistributing, teaching with, or publishing analyses from WVS data
should ensure that their use and hosting arrangements comply with the
applicable WVS data-use terms and citation requirements.

For WVS information, access, documentation, and current conditions of use,
visit the [World Values Survey website](https://www.worldvaluessurvey.org/).

The repository currently retains the data required for this project build. If
the project later adopts a private-data workflow, the source and/or generated
data paths can be moved behind repository ignore rules without changing the
static application architecture.
