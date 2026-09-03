# WorldView

WorldView is a browser-based teaching application for exploring and analysing World Values Survey (WVS) Wave 7 data. The student-facing application is a static website: students do not need R, RStudio, a server-side application, or programming experience. R is used only by the educator or maintainer to prepare the data files consumed by the website.

This repository is intentionally organised around two separate layers:

- **`worldview_deployment/`** — the static application that students use. It contains the current HTML, CSS, JavaScript, data assets, maps, phylogeny files, and browser validation page.
- **R build/configuration layer** — prepares a reproducible teaching subset from WVS Wave 7 and updates the data assets used by the static application.

The current student interface is treated as the reference implementation. Rebuilding the data does not require rebuilding or rewriting the JavaScript application.

## Repository structure

| Path | Purpose |
| --- | --- |
| `worldview_deployment/` | Complete static student-facing application. This is the directory to host. |
| `build_worldview.R` | Single entry point for rebuilding WorldView data assets. |
| `R/worldview_build.R` | Reusable data-processing, sampling, codebook, export, and validation functions. |
| `config/WVS7_codebook_index.xlsx` | Educator-facing variable configuration and WVS recoding metadata. |
| `config/worldview_config.csv` | Small set of build settings, including sample size and random seed. |
| `WVS_Dataset/WVS_Cross-National_Wave_7_rds_v6_0.rds` | Source WVS Wave 7 R dataset used by the build. |
| `WVS_Dataset/F00011012-WVS_WAVE_7_MASTER_QUESTIONNAIRE_2017-2021_ENGLISH.pdf` | WVS questionnaire documentation retained with the project. |

Historical Shiny code, one-off development-step scripts, duplicate release folders, diagnostics, and intermediate derived datasets are not required by the current static application and are intentionally excluded from the cleaned repository.

## Configure the teaching dataset

### 1. Choose the variables students can see

Open:

`config/WVS7_codebook_index.xlsx`

On the **Codebook index** sheet, edit the `Variable_Display_Logical` column:

- `TRUE` = include the variable in the student-facing WorldView build.
- `FALSE` = exclude it from the student-facing WorldView build.

The workbook also retains the display type, response metadata, and `R_recode` expressions used to process WVS variables. The default workbook preserves the current 39-variable teaching set conceptually. During cleanup, ten recently added variables were found to have been attached to incorrect WVS question IDs in the development branch; the cleaned configuration corrects those mappings to Q8-Q16 (the nine displayed child-quality items) and Q235 (strong leader).

A small number of very high-cardinality WVS variables (`Q223`, `Q266`, `Q267`, `Q268`, `Q272`, and `Q290`) remain excluded by the build because they are unsuitable for the current browser interface.

### 2. Configure participant sampling

Edit:

`config/worldview_config.csv`

The default settings are:

- `sampling_seed = 382`
- `maximum_participants_per_country = 1000`
- `data_version = WV7-WORLDVIEW-1.2.0`

WorldView samples independently within each country and retains all respondents when a country contains fewer than the configured maximum. Using a fixed seed makes the teaching sample reproducible.

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

The build updates the static application's runtime data files in `worldview_deployment/data/`, including:

- `worldview-browser-data-v1.0.0.json`
- `worldview-browser-data-v1.0.0.json.gz`
- `worldview-codebook-v1.0.0.json`
- `manifest-v1.0.0.json`

It also updates `worldview_deployment/deployment-checks.csv` and stops with an error if core consistency checks fail.

The browser representation deliberately follows the conventions used by the current static application. Ordered variables are represented numerically, binary recodes are represented numerically, and multi-category nominal factors remain readable labels in browser data.


### Important first rebuild after this cleanup

The `WorldView_Online` development branch contained a variable-ID mismatch in ten recently added fields: the student interface displayed nine child-quality concepts and the strong-leader item, but the runtime columns were named `Q18-Q26` and `Q238`. In the WVS Wave 7 questionnaire those concepts are `Q8-Q16` and `Q235`. The cleaned workbook uses the correct IDs. Run the build once after applying this cleanup so the generated browser data, codebook, and manifest are synchronized with the corrected configuration.

## Preview locally

Because the app loads JSON files with `fetch()`, do not open `index.html` directly from the filesystem. Serve the deployment directory with a local web server.

For example, with Python:

```bash
python -m http.server 8000 --directory worldview_deployment
```

Then open `http://localhost:8000/` in a browser.

Alternatively, R users can use a static-file server such as `servr::httd("worldview_deployment")`.

## Host for students

WorldView is a static site. To deploy it, publish the **contents of `worldview_deployment/`** using a static hosting service such as GitHub Pages, Netlify, an institutional web server, or another static host.

No server-side R process is required after the data have been built.

A straightforward GitHub Pages workflow is to configure Pages to publish a branch/folder containing the contents of `worldview_deployment/`. If a host expects `index.html` at the site root, deploy the contents of the directory rather than the repository root.

## Updating the app

For data/configuration changes, prefer changing the Excel/codebook configuration and rebuilding rather than editing generated JSON manually.

For interface or analysis-code changes, edit files directly under `worldview_deployment/`. There is intentionally only one maintained copy of the static app in the cleaned repository; do not recreate parallel `static_app`, `release`, or step-numbered copies.

Before publishing a front-end change, preview the site and open `worldview_deployment/validation.html` to run the browser-side validation checks.

## Data and attribution

The application uses World Values Survey Wave 7 data. Anyone redistributing, teaching with, or publishing analyses from WVS data should ensure that their use and hosting arrangements comply with the applicable WVS data-use terms and citation requirements.

The repository currently retains the data required for this project build. If the project later adopts a private-data workflow, the source and/or generated data paths can be moved behind repository ignore rules without changing the static application architecture.
