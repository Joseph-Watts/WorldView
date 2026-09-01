# WorldView Online - Step 6
# Build the first static application shell with working Home and Codebook pages.
# Run this script from the WorldView project working directory in RStudio.

source_data_dir <- file.path(
  "worldview_step3_output",
  "public_data"
)

app_dir <- file.path(getwd(), "worldview_static_app")
app_data_dir <- file.path(app_dir, "data")
app_assets_dir <- file.path(app_dir, "assets")

dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_assets_dir, recursive = TRUE, showWarnings = FALSE)

required_public_files <- c(
  "worldview-browser-data-v1.0.0.json",
  "worldview-browser-data-v1.0.0.json.gz",
  "worldview-wave7-teaching-data-v1.0.0.csv",
  "worldview-codebook-v1.0.0.json",
  "worldview-codebook-v1.0.0.csv",
  "worldview-reference-fixtures-v1.0.0.json",
  "manifest-v1.0.0.json"
)

missing_files <- required_public_files[
  !file.exists(file.path(source_data_dir, required_public_files))
]

if (length(missing_files) > 0L) {
  stop(
    "Required public files are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

file.copy(
  from = file.path(source_data_dir, required_public_files),
  to = file.path(app_data_dir, required_public_files),
  overwrite = TRUE
)

index_html <- '<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="WorldView teaching interface for selected World Values Survey Wave 7 data.">
  <title>WorldView</title>
  <link rel="stylesheet" href="assets/styles.css">
  <script src="assets/app.js" defer></script>
</head>
<body>
  <a class="skip-link" href="#main-content">Skip to main content</a>

  <header class="site-header">
    <div class="header-inner">
      <a class="brand" href="#home" aria-label="WorldView home">
        <span class="brand-mark" aria-hidden="true">W</span>
        <span>
          <strong>WorldView</strong>
          <small>World Values Survey teaching resource</small>
        </span>
      </a>

      <button id="menu-button" class="menu-button" type="button"
              aria-expanded="false" aria-controls="primary-navigation">
        Menu
      </button>

      <nav id="primary-navigation" class="primary-navigation" aria-label="Primary navigation">
        <a href="#home" data-route="home">Home</a>
        <a href="#codebook" data-route="codebook">Codebook</a>
        <a href="#summary" data-route="summary">Summary statistics</a>
        <a href="#visualisations" data-route="visualisations">Visualisations</a>
        <a href="#models" data-route="models">Models</a>
        <a href="#download" data-route="download">Download</a>
      </nav>
    </div>
  </header>

  <main id="main-content" tabindex="-1">
    <section id="page-home" class="page page-home" data-page="home">
      <div class="hero">
        <div class="hero-content">
          <p class="eyebrow">Explore selected Wave 7 data</p>
          <h1>Analyse values across countries without writing code</h1>
          <p class="lead">
            WorldView provides a point-and-click teaching interface for exploring a processed subset
            of the World Values Survey Wave 7 dataset.
          </p>
          <div class="button-row">
            <a class="button button-primary" href="#summary">Explore the data</a>
            <a class="button button-secondary" href="#codebook">View the codebook</a>
          </div>
        </div>
        <aside class="hero-panel" aria-label="Dataset summary">
          <p class="panel-label">Teaching dataset</p>
          <dl class="key-figures">
            <div><dt id="figure-participants">Loading</dt><dd>participants</dd></div>
            <div><dt id="figure-countries">Loading</dt><dd>countries</dd></div>
            <div><dt id="figure-variables">Loading</dt><dd>variables</dd></div>
          </dl>
          <p class="small-text">No more than 1,000 participants are included from any country.</p>
        </aside>
      </div>

      <div class="content-grid three-column">
        <article class="feature-card">
          <span class="feature-number">01</span>
          <h2>Understand the variables</h2>
          <p>Review question labels, processed response categories, missing-data treatment, and analysis coding.</p>
          <a href="#codebook">Open the codebook</a>
        </article>
        <article class="feature-card">
          <span class="feature-number">02</span>
          <h2>Describe and compare</h2>
          <p>Calculate summaries and compare response distributions for selected countries.</p>
          <a href="#summary">Open summary statistics</a>
        </article>
        <article class="feature-card">
          <span class="feature-number">03</span>
          <h2>Examine associations</h2>
          <p>Calculate flexible Pearson correlations between eligible processed variables.</p>
          <a href="#models">Open models</a>
        </article>
      </div>

      <section class="information-panel">
        <h2>About the data</h2>
        <p>
          WorldView uses a selected teaching subset of Wave 7. Negative source codes for missing,
          non-response, and not-asked values are treated as missing rather than substantive responses.
          Results describe the sampled WorldView dataset and do not establish causation.
        </p>
      </section>
    </section>

    <section id="page-codebook" class="page" data-page="codebook" hidden>
      <div class="page-heading">
        <p class="eyebrow">Variable documentation</p>
        <h1>Codebook</h1>
        <p>Search the 29 student-facing variables and review how WorldView processes them.</p>
      </div>

      <div class="toolbar" role="search">
        <label class="field search-field">
          <span>Search variables</span>
          <input id="codebook-search" type="search" placeholder="Search by Q number or description">
        </label>
        <label class="field">
          <span>Topic</span>
          <select id="codebook-group">
            <option value="">All topics</option>
          </select>
        </label>
        <label class="field">
          <span>Variable type</span>
          <select id="codebook-type">
            <option value="">All types</option>
          </select>
        </label>
      </div>

      <p id="codebook-status" class="status" aria-live="polite">Loading codebook...</p>
      <div id="codebook-list" class="codebook-list"></div>
    </section>

    <section id="page-summary" class="page" data-page="summary" hidden>
      <div class="page-heading"><p class="eyebrow">Explore one variable</p><h1>Summary statistics</h1></div>
      <div class="coming-soon"><h2>Application shell ready</h2><p>This page will be implemented in the next stage.</p></div>
    </section>

    <section id="page-visualisations" class="page" data-page="visualisations" hidden>
      <div class="page-heading"><p class="eyebrow">Compare distributions</p><h1>Visualisations</h1></div>
      <div class="coming-soon"><h2>Application shell ready</h2><p>Bar graphs will be implemented after summary statistics.</p></div>
    </section>

    <section id="page-models" class="page" data-page="models" hidden>
      <div class="page-heading"><p class="eyebrow">Examine associations</p><h1>Models</h1></div>
      <div class="coming-soon"><h2>Application shell ready</h2><p>Pearson correlations will be implemented and checked against the R fixtures.</p></div>
    </section>

    <section id="page-download" class="page" data-page="download" hidden>
      <div class="page-heading"><p class="eyebrow">Teaching data</p><h1>Download</h1></div>
      <div class="download-card">
        <h2>WorldView Wave 7 teaching dataset</h2>
        <p>The CSV contains country fields, an anonymous WorldView row ID, and the 29 approved variables.</p>
        <a class="button button-primary" href="data/worldview-wave7-teaching-data-v1.0.0.csv" download>Download CSV</a>
        <a class="button button-secondary" href="data/worldview-codebook-v1.0.0.csv" download>Download codebook</a>
      </div>
    </section>
  </main>

  <footer class="site-footer">
    <p>WorldView uses selected data from the World Values Survey Wave 7.</p>
    <p id="footer-version">Loading data version...</p>
  </footer>
</body>
</html>
'

styles_css <- ':root {
  --ink: #17252f;
  --muted: #596b75;
  --paper: #f7f5ef;
  --surface: #ffffff;
  --teal: #006b66;
  --teal-dark: #004b48;
  --gold: #d7a928;
  --line: #d8dfdc;
  --focus: #7c3aed;
  --shadow: 0 14px 34px rgba(23, 37, 47, 0.09);
  --radius: 18px;
  font-family: Inter, Aptos, "Segoe UI", Arial, sans-serif;
  color: var(--ink);
  background: var(--paper);
}

* { box-sizing: border-box; }
html { scroll-behavior: smooth; }
body { margin: 0; line-height: 1.55; background: var(--paper); }
a { color: var(--teal-dark); text-underline-offset: 0.2em; }
a:hover { color: var(--teal); }
button, input, select { font: inherit; }

.skip-link { position: absolute; left: -10000px; top: 0; z-index: 100; padding: 0.75rem 1rem; background: white; }
.skip-link:focus { left: 0.5rem; top: 0.5rem; }
:focus-visible { outline: 3px solid var(--focus); outline-offset: 3px; }

.site-header { position: sticky; top: 0; z-index: 20; background: rgba(247,245,239,0.96); border-bottom: 1px solid var(--line); backdrop-filter: blur(12px); }
.header-inner { max-width: 1200px; min-height: 78px; margin: auto; padding: 0 24px; display: flex; align-items: center; gap: 28px; }
.brand { display: flex; align-items: center; gap: 12px; margin-right: auto; color: var(--ink); text-decoration: none; }
.brand-mark { display: grid; place-items: center; width: 42px; height: 42px; border-radius: 12px; background: var(--teal); color: white; font: 700 1.2rem Georgia, serif; }
.brand strong, .brand small { display: block; }
.brand small { color: var(--muted); font-size: 0.75rem; }
.primary-navigation { display: flex; gap: 6px; flex-wrap: wrap; }
.primary-navigation a { padding: 9px 11px; border-radius: 999px; color: var(--ink); text-decoration: none; font-size: 0.92rem; }
.primary-navigation a:hover, .primary-navigation a[aria-current="page"] { background: white; color: var(--teal-dark); box-shadow: 0 2px 8px rgba(23,37,47,.08); }
.menu-button { display: none; border: 1px solid var(--line); border-radius: 10px; background: white; padding: 8px 12px; }

main { max-width: 1200px; margin: auto; padding: 48px 24px 72px; }
.page { min-height: 66vh; }
.hero { display: grid; grid-template-columns: minmax(0, 1.55fr) minmax(280px, .75fr); gap: 30px; align-items: stretch; }
.hero-content { padding: 44px 0 34px; }
.eyebrow { margin: 0 0 10px; color: var(--teal); font-weight: 800; letter-spacing: .09em; text-transform: uppercase; font-size: .76rem; }
h1, h2 { line-height: 1.12; }
h1 { max-width: 850px; margin: 0 0 22px; font: 700 clamp(2.4rem, 6vw, 4.8rem)/1.02 Georgia, Cambria, serif; letter-spacing: -.04em; }
h2 { font-size: 1.3rem; }
.lead { max-width: 720px; color: var(--muted); font-size: 1.18rem; }
.button-row { display: flex; gap: 12px; flex-wrap: wrap; margin-top: 28px; }
.button { display: inline-block; border: 1px solid var(--teal); border-radius: 999px; padding: 11px 18px; text-decoration: none; font-weight: 700; }
.button-primary { background: var(--teal); color: white; }
.button-primary:hover { background: var(--teal-dark); color: white; }
.button-secondary { background: transparent; color: var(--teal-dark); }

.hero-panel { padding: 30px; border-radius: var(--radius); background: var(--ink); color: white; box-shadow: var(--shadow); }
.panel-label { color: #b8d8d4; text-transform: uppercase; letter-spacing: .08em; font-size: .76rem; font-weight: 800; }
.key-figures { display: grid; gap: 18px; margin: 28px 0; }
.key-figures div { padding-bottom: 14px; border-bottom: 1px solid rgba(255,255,255,.16); }
.key-figures dt { font: 700 2.2rem Georgia, serif; }
.key-figures dd { margin: 2px 0 0; color: #d8e6e3; }
.small-text { color: #c8d6d4; font-size: .9rem; }

.content-grid { display: grid; gap: 18px; margin-top: 32px; }
.three-column { grid-template-columns: repeat(3, 1fr); }
.feature-card, .information-panel, .coming-soon, .download-card { background: var(--surface); border: 1px solid var(--line); border-radius: var(--radius); padding: 26px; }
.feature-card { min-height: 230px; box-shadow: 0 5px 20px rgba(23,37,47,.04); }
.feature-number { color: var(--gold); font-weight: 900; }
.information-panel { margin-top: 24px; border-left: 5px solid var(--gold); }

.page-heading { margin-bottom: 28px; }
.page-heading h1 { margin-bottom: 12px; font-size: clamp(2.4rem, 5vw, 4rem); }
.page-heading p:last-child { max-width: 750px; color: var(--muted); }
.toolbar { display: grid; grid-template-columns: 2fr 1fr 1fr; gap: 14px; margin-bottom: 18px; padding: 18px; background: white; border: 1px solid var(--line); border-radius: var(--radius); }
.field { display: grid; gap: 6px; font-size: .9rem; font-weight: 700; }
.field input, .field select { width: 100%; min-height: 44px; padding: 9px 11px; border: 1px solid #9babaf; border-radius: 9px; background: white; color: var(--ink); }
.status { color: var(--muted); }
.codebook-list { display: grid; gap: 14px; }
.variable-card { border: 1px solid var(--line); border-radius: 14px; background: white; overflow: hidden; }
.variable-card summary { cursor: pointer; display: grid; grid-template-columns: 90px 1fr auto; gap: 14px; align-items: center; padding: 18px 20px; }
.variable-card summary:hover { background: #fbfcfa; }
.variable-id { color: var(--teal-dark); font-weight: 900; }
.variable-title { font-weight: 750; }
.variable-badge { padding: 5px 9px; border-radius: 999px; background: #e5f2f0; color: var(--teal-dark); font-size: .76rem; font-weight: 800; }
.variable-details { padding: 0 20px 22px; border-top: 1px solid var(--line); }
.detail-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 12px 24px; }
.detail-grid dt { font-weight: 800; }
.detail-grid dd { margin: 3px 0 0; color: var(--muted); }
.level-list { margin: 8px 0 0; columns: 2; }
.download-card .button { margin: 6px 8px 0 0; }
.site-footer { max-width: 1200px; margin: auto; padding: 24px; border-top: 1px solid var(--line); color: var(--muted); font-size: .88rem; display: flex; justify-content: space-between; gap: 20px; }

@media (max-width: 850px) {
  .hero, .three-column, .toolbar, .detail-grid { grid-template-columns: 1fr; }
  .hero-content { padding-top: 18px; }
  .menu-button { display: block; }
  .primary-navigation { display: none; position: absolute; left: 16px; right: 16px; top: 70px; padding: 12px; border: 1px solid var(--line); border-radius: 14px; background: white; box-shadow: var(--shadow); }
  .primary-navigation.open { display: grid; }
  .variable-card summary { grid-template-columns: 70px 1fr; }
  .variable-badge { grid-column: 2; width: fit-content; }
  .site-footer { display: block; }
}

@media (max-width: 520px) {
  main { padding: 30px 16px 56px; }
  .header-inner { padding: 0 16px; }
  .brand small { display: none; }
  .level-list { columns: 1; }
}
'

app_js <- '\"use strict\";

const paths = {
  manifest: "data/manifest-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const state = { manifest: null, codebook: null };

function currentRoute() {
  const route = window.location.hash.replace(/^#/, "").split("?")[0];
  const valid = ["home", "codebook", "summary", "visualisations", "models", "download"];
  return valid.includes(route) ? route : "home";
}

function showRoute() {
  const route = currentRoute();

  document.querySelectorAll("[data-page]").forEach(page => {
    page.hidden = page.dataset.page !== route;
  });

  document.querySelectorAll("[data-route]").forEach(link => {
    if (link.dataset.route === route) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });

  document.getElementById("primary-navigation").classList.remove("open");
  document.getElementById("menu-button").setAttribute("aria-expanded", "false");
  document.getElementById("main-content").focus({ preventScroll: true });
  window.scrollTo({ top: 0, behavior: "instant" });
}

function formatNumber(value) {
  return new Intl.NumberFormat("en-NZ").format(value);
}

function populateManifest() {
  if (!state.manifest) return;
  document.getElementById("figure-participants").textContent = formatNumber(state.manifest.participantCount);
  document.getElementById("figure-countries").textContent = formatNumber(state.manifest.countryCount);
  document.getElementById("figure-variables").textContent = formatNumber(state.manifest.studentVariableCount);
  document.getElementById("footer-version").textContent = `Data version: ${state.manifest.dataVersion}`;
}

function escapeHtml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\\\"", "&quot;")
    .replaceAll("\'", "&#039;");
}

function variableSearchText(variable) {
  return [
    variable.id,
    variable.displayName,
    variable.group,
    variable.analysisType,
    variable.sourceQuestionLabel
  ].join(" ").toLowerCase();
}

function renderLevels(variable) {
  if (!Array.isArray(variable.levels) || variable.levels.length === 0) {
    if (variable.validRange) {
      return `<p>${escapeHtml(variable.validRange.minimum)} to ${escapeHtml(variable.validRange.maximum)}</p>`;
    }
    return "<p>Not specified</p>";
  }

  return `<ol class="level-list">${variable.levels.map(level =>
    `<li value="${escapeHtml(level.displayOrder)}">${escapeHtml(level.label)}</li>`
  ).join("")}</ol>`;
}

function variableCard(variable) {
  const eligible = variable.correlationEligible ? "Correlation eligible" : "Not correlation eligible";
  return `
    <details class="variable-card" id="${escapeHtml(variable.id)}">
      <summary>
        <span class="variable-id">${escapeHtml(variable.id)}</span>
        <span class="variable-title">${escapeHtml(variable.displayName)}</span>
        <span class="variable-badge">${escapeHtml(variable.analysisType)}</span>
      </summary>
      <div class="variable-details">
        <dl class="detail-grid">
          <div><dt>Topic</dt><dd>${escapeHtml(variable.group)}</dd></div>
          <div><dt>Display type</dt><dd>${escapeHtml(variable.displayType)}</dd></div>
          <div><dt>Correlation use</dt><dd>${eligible}</dd></div>
          <div><dt>Correlation coding</dt><dd>${escapeHtml(variable.correlationRepresentation)}</dd></div>
          <div><dt>Valid in full processed data</dt><dd>${formatNumber(variable.validCountInFullProcessedData)}</dd></div>
          <div><dt>Missing in full processed data</dt><dd>${formatNumber(variable.missingCountInFullProcessedData)}</dd></div>
        </dl>
        <h3>Processed values</h3>
        ${renderLevels(variable)}
        <h3>Missing-data treatment</h3>
        <p>${escapeHtml(variable.missingRule)}</p>
        <h3>Interpretation</h3>
        <p>${escapeHtml(variable.interpretationNote)}</p>
      </div>
    </details>`;
}

function populateFilters() {
  const variables = state.codebook.variables;
  const groups = [...new Set(variables.map(v => v.group))].sort();
  const types = [...new Set(variables.map(v => v.analysisType))].sort();
  const groupSelect = document.getElementById("codebook-group");
  const typeSelect = document.getElementById("codebook-type");

  groups.forEach(group => groupSelect.add(new Option(group, group)));
  types.forEach(type => typeSelect.add(new Option(type, type)));
}

function renderCodebook() {
  if (!state.codebook) return;

  const query = document.getElementById("codebook-search").value.trim().toLowerCase();
  const group = document.getElementById("codebook-group").value;
  const type = document.getElementById("codebook-type").value;

  const matches = state.codebook.variables.filter(variable => {
    return (!query || variableSearchText(variable).includes(query)) &&
      (!group || variable.group === group) &&
      (!type || variable.analysisType === type);
  });

  document.getElementById("codebook-list").innerHTML = matches.map(variableCard).join("");
  document.getElementById("codebook-status").textContent =
    `${matches.length} of ${state.codebook.variables.length} variables shown`;

  const anchor = window.location.hash.split("/")[1];
  if (anchor) document.getElementById(anchor)?.setAttribute("open", "");
}

async function loadJson(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Could not load ${path}: ${response.status}`);
  return response.json();
}

async function initialise() {
  showRoute();

  document.getElementById("menu-button").addEventListener("click", event => {
    const navigation = document.getElementById("primary-navigation");
    const open = navigation.classList.toggle("open");
    event.currentTarget.setAttribute("aria-expanded", String(open));
  });

  try {
    [state.manifest, state.codebook] = await Promise.all([
      loadJson(paths.manifest),
      loadJson(paths.codebook)
    ]);

    populateManifest();
    populateFilters();
    renderCodebook();
  } catch (error) {
    console.error(error);
    document.getElementById("codebook-status").textContent =
      "The codebook could not be loaded. Open this application through a local web server rather than opening index.html directly.";
  }

  ["codebook-search", "codebook-group", "codebook-type"].forEach(id => {
    document.getElementById(id).addEventListener("input", renderCodebook);
    document.getElementById(id).addEventListener("change", renderCodebook);
  });
}

window.addEventListener("hashchange", showRoute);
document.addEventListener("DOMContentLoaded", initialise);
'

readme_txt <- 'WorldView static application: Step 6

Contents
- index.html
- assets/styles.css
- assets/app.js
- data/* public WorldView assets

Run locally from RStudio

Option 1: using the servr package

  install.packages("servr")  # only if needed
  servr::httd("worldview_static_app", browser = TRUE)

Option 2: using the httpuv package

  install.packages("httpuv") # only if needed
  httpuv::runStaticServer("worldview_static_app")

Do not open index.html directly from the file system. Browser security rules can
block fetch() from loading the JSON files when the page uses a file URL.

Checks
1. Home page shows participant, country, and variable counts.
2. All navigation links change pages.
3. Codebook reports 29 of 29 variables shown.
4. Searching for Q167 shows the hell-belief variable.
5. Filtering Topic to Demographics shows six variables.
6. Download buttons start CSV downloads.
'

writeLines(index_html, file.path(app_dir, "index.html"), useBytes = TRUE)
writeLines(styles_css, file.path(app_assets_dir, "styles.css"), useBytes = TRUE)
writeLines(app_js, file.path(app_assets_dir, "app.js"), useBytes = TRUE)
writeLines(readme_txt, file.path(app_dir, "README.txt"), useBytes = TRUE)

expected_files <- c(
  file.path(app_dir, "index.html"),
  file.path(app_assets_dir, "styles.css"),
  file.path(app_assets_dir, "app.js"),
  file.path(app_dir, "README.txt"),
  file.path(app_data_dir, required_public_files)
)

validation <- data.frame(
  check = c(
    "app_directory_created",
    "index_created",
    "stylesheet_created",
    "javascript_created",
    "all_public_data_copied",
    "codebook_json_present",
    "manifest_present",
    "reference_fixtures_present"
  ),
  passed = c(
    dir.exists(app_dir),
    file.exists(file.path(app_dir, "index.html")),
    file.exists(file.path(app_assets_dir, "styles.css")),
    file.exists(file.path(app_assets_dir, "app.js")),
    all(file.exists(file.path(app_data_dir, required_public_files))),
    file.exists(file.path(app_data_dir, "worldview-codebook-v1.0.0.json")),
    file.exists(file.path(app_data_dir, "manifest-v1.0.0.json")),
    file.exists(file.path(app_data_dir, "worldview-reference-fixtures-v1.0.0.json"))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step6_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 6 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 6 completed successfully.\n")
cat("Static app created in: worldview_static_app\n\n")
cat("To run it locally in RStudio:\n")
cat("  install.packages(\"servr\")  # only if servr is not installed\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n\n")
cat("Then check Home, Codebook, navigation, and downloads.\n")
