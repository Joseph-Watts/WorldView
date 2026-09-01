# WorldView Online - Step 8
# Add interactive bar graphs to the static WorldView application.

app_dir <- file.path(getwd(), "worldview_static_app")
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
visual_js_path <- file.path(app_dir, "assets", "visualisations.js")

required <- c(
  index_path,
  css_path,
  file.path(app_dir, "assets", "app.js"),
  file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
  file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
)

missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Required app files are missing: ", paste(missing, collapse = ", "))
}

index_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

old_visualisations <- '<section id="page-visualisations" class="page" data-page="visualisations" hidden>
      <div class="page-heading"><p class="eyebrow">Compare distributions</p><h1>Visualisations</h1></div>
      <div class="coming-soon"><h2>Application shell ready</h2><p>Bar graphs will be implemented after summary statistics.</p></div>
    </section>'

new_visualisations <- '<section id="page-visualisations" class="page" data-page="visualisations" hidden>
      <div class="page-heading">
        <p class="eyebrow">Compare distributions</p>
        <h1>Visualisations</h1>
        <p>Create accessible bar graphs for one variable across one or more countries.</p>
      </div>

      <div id="visual-loading" class="notice" aria-live="polite">Loading data...</div>

      <form id="visual-form" class="analysis-layout" hidden>
        <aside class="control-panel">
          <h2>Choose data</h2>

          <label class="field">
            <span>Variable</span>
            <select id="visual-variable" required></select>
          </label>

          <label class="field">
            <span>Countries</span>
            <select id="visual-countries" multiple size="10" aria-describedby="visual-country-help"></select>
          </label>
          <p id="visual-country-help" class="field-help">Select one or more countries. Use Ctrl or Command to select several. If none are selected, all countries are combined.</p>

          <fieldset class="radio-group">
            <legend>Bar values</legend>
            <label><input type="radio" name="visual-measure" value="percentage" checked> Percentage of valid responses</label>
            <label><input type="radio" name="visual-measure" value="count"> Count</label>
          </fieldset>

          <div id="age-band-controls" hidden>
            <label class="field">
              <span>Age grouping</span>
              <select id="visual-age-bands">
                <option value="teaching">Teaching bands: 16-24, 25-34, ...</option>
                <option value="decades">Ten-year bands</option>
                <option value="individual">Individual ages</option>
              </select>
            </label>
          </div>

          <div class="button-row compact">
            <button class="button button-primary" type="submit">Create graph</button>
            <button id="visual-reset" class="button button-secondary" type="button">Reset</button>
          </div>
        </aside>

        <section class="results-panel" aria-labelledby="visual-title">
          <div class="results-heading">
            <div>
              <p class="eyebrow">Bar graph</p>
              <h2 id="visual-title">Select data to begin</h2>
            </div>
            <button id="visual-download" class="button button-secondary" type="button" hidden>Download graph data</button>
          </div>

          <div id="visual-message" class="notice subtle">Choose a variable and country selection, then create the graph.</div>
          <div id="visual-chart" class="bar-chart" role="img" aria-label="Bar graph results"></div>

          <details id="visual-table-disclosure" class="full-table-disclosure" hidden>
            <summary>Show accessible data table</summary>
            <div id="visual-table" class="disclosure-content"></div>
          </details>
        </section>
      </form>
    </section>'

if (!grepl(old_visualisations, index_html, fixed = TRUE)) {
  stop(
    "Could not find the Step 6 visualisations placeholder. ",
    "Run this script only once after Step 7b."
  )
}

index_html <- sub(old_visualisations, new_visualisations, index_html, fixed = TRUE)

if (!grepl("assets/visualisations.js", index_html, fixed = TRUE)) {
  index_html <- sub(
    "</head>",
    '  <script src="assets/visualisations.js" defer></script>\n</head>',
    index_html,
    fixed = TRUE
  )
}

writeLines(index_html, index_path, useBytes = TRUE)

visual_css <- '

/* Step 8: bar graphs */
.bar-chart {
  display: grid;
  gap: 26px;
  margin-top: 22px;
}
.chart-group {
  min-width: 0;
}
.chart-group h3 {
  margin: 0 0 12px;
  font-size: 1rem;
}
.chart-bars {
  display: grid;
  gap: 10px;
}
.chart-row {
  display: grid;
  grid-template-columns: minmax(135px, 1.25fr) minmax(180px, 4fr) 86px;
  gap: 10px;
  align-items: center;
}
.chart-label {
  overflow-wrap: anywhere;
  font-size: .85rem;
  text-align: right;
}
.chart-track {
  position: relative;
  height: 27px;
  border-radius: 5px;
  overflow: hidden;
  background: #edf1ef;
}
.chart-bar {
  height: 100%;
  min-width: 1px;
  border-radius: 5px;
  background: var(--teal);
  transition: width .25s ease;
}
.chart-bar.secondary-1 { background: #397c8e; }
.chart-bar.secondary-2 { background: #b07b25; }
.chart-bar.secondary-3 { background: #785b91; }
.chart-bar.secondary-4 { background: #64743e; }
.chart-value {
  color: var(--ink);
  font-variant-numeric: tabular-nums;
  font-size: .84rem;
  white-space: nowrap;
}
.chart-axis-note {
  margin: 8px 0 0 calc(22% + 10px);
  color: var(--muted);
  font-size: .78rem;
}
.chart-empty {
  padding: 18px;
  border: 1px dashed var(--line);
  border-radius: 12px;
  color: var(--muted);
}
@media (max-width: 700px) {
  .chart-row {
    grid-template-columns: 1fr 66px;
  }
  .chart-label {
    grid-column: 1 / -1;
    text-align: left;
  }
  .chart-axis-note { margin-left: 0; }
}
'

css_text <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 8: bar graphs", css_text, fixed = TRUE)) {
  cat(visual_css, file = css_path, append = TRUE)
}

visual_js <- '"use strict";

const visualPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const visualState = {
  data: null,
  codebook: null,
  variablesById: new Map(),
  countries: [],
  lastRows: []
};

function visualEscape(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\\\"", "&quot;")
    .replaceAll("\'", "&#039;");
}

function visualNumber(value, digits = 1) {
  return new Intl.NumberFormat("en-NZ", { maximumFractionDigits: digits }).format(value);
}

function parseVisualColumns(object) {
  const names = Object.keys(object);
  if (!names.length) throw new Error("The browser data has no columns.");
  const length = object[names[0]].length;
  if (!names.every(name => object[name].length === length)) {
    throw new Error("Browser data columns have different lengths.");
  }
  return { columns: object, length };
}

function visualSelectedCountries() {
  return [...document.getElementById("visual-countries").selectedOptions].map(option => option.value);
}

function visualMeasure() {
  return document.querySelector("input[name=visual-measure]:checked").value;
}

function visualDisplayLabel(variable, value) {
  if (variable.id === "Q289") return String(value);
  if (["Q165", "Q166", "Q167", "Q168"].includes(variable.id)) return Number(value) === 1 ? "Yes" : "No";
  if (variable.id === "Q260") return Number(value) === 1 ? "Male" : "Female";
  if (variable.id === "Q263") return Number(value) === 1
    ? "I am born in this country"
    : "I am an immigrant to this country (born outside this country)";
  if (Array.isArray(variable.levels)) {
    const match = variable.levels.find(level => Number(level.displayOrder) === Number(value));
    if (match) return match.label;
  }
  return String(value);
}

function ageBand(age, scheme) {
  const value = Number(age);
  if (!Number.isFinite(value)) return null;

  if (scheme === "individual") return String(value);

  if (scheme === "decades") {
    const lower = Math.floor(value / 10) * 10;
    return `${lower}-${lower + 9}`;
  }

  if (value <= 24) return "16-24";
  if (value <= 34) return "25-34";
  if (value <= 44) return "35-44";
  if (value <= 54) return "45-54";
  if (value <= 64) return "55-64";
  if (value <= 74) return "65-74";
  return "75+";
}

function categoryOrder(variable, values) {
  if (variable.id === "Q262") {
    const scheme = document.getElementById("visual-age-bands").value;
    const unique = [...new Set(values.map(value => ageBand(value, scheme)).filter(Boolean))];
    return unique.sort((a, b) => {
      const firstA = Number.parseInt(a, 10);
      const firstB = Number.parseInt(b, 10);
      return firstA - firstB;
    });
  }

  if (variable.id === "Q289") {
    return [...new Set(values.map(String))].sort((a, b) => a.localeCompare(b));
  }

  if (Array.isArray(variable.levels) && variable.levels.length) {
    return variable.levels.map(level => String(level.displayOrder));
  }

  return [...new Set(values.map(String))].sort((a, b) => Number(a) - Number(b));
}

function groupGraphData(variable, countryCodes) {
  const selected = new Set(countryCodes);
  const countryColumn = visualState.data.columns.B_COUNTRY_ALPHA;
  const valueColumn = visualState.data.columns[variable.id];
  const valid = [];
  let total = 0;

  for (let i = 0; i < visualState.data.length; i += 1) {
    if (selected.size && !selected.has(countryColumn[i])) continue;
    total += 1;
    const value = valueColumn[i];
    if (value !== null && value !== undefined && value !== "") valid.push(value);
  }

  const categories = categoryOrder(variable, valid);
  const counts = new Map(categories.map(category => [String(category), 0]));
  const ageScheme = document.getElementById("visual-age-bands").value;

  valid.forEach(value => {
    const category = variable.id === "Q262"
      ? ageBand(value, ageScheme)
      : String(value);
    counts.set(String(category), (counts.get(String(category)) || 0) + 1);
  });

  return {
    total,
    validN: valid.length,
    missingN: total - valid.length,
    rows: categories.map(category => {
      const count = counts.get(String(category)) || 0;
      return {
        category: String(category),
        label: variable.id === "Q262"
          ? String(category)
          : visualDisplayLabel(variable, category),
        count,
        percentage: valid.length ? 100 * count / valid.length : 0
      };
    })
  };
}

function graphGroupHtml(group, measure, colourIndex) {
  if (group.validN < 10) {
    return `<section class="chart-group"><h3>${visualEscape(group.title)}</h3>
      <p class="suppression-note">A graph is not displayed because fewer than 10 valid observations are available.</p></section>`;
  }

  const maximum = measure === "percentage"
    ? Math.max(100, ...group.rows.map(row => row.percentage))
    : Math.max(1, ...group.rows.map(row => row.count));

  return `<section class="chart-group">
    <h3>${visualEscape(group.title)}</h3>
    <div class="chart-bars">${group.rows.map(row => {
      const value = measure === "percentage" ? row.percentage : row.count;
      const width = 100 * value / maximum;
      const shown = measure === "percentage"
        ? `${visualNumber(value, 1)}%`
        : visualNumber(value, 0);
      const tooltip = `${row.label}: ${row.count} responses, ${visualNumber(row.percentage, 1)}% of valid responses`;
      return `<div class="chart-row">
        <div class="chart-label">${visualEscape(row.label)}</div>
        <div class="chart-track" title="${visualEscape(tooltip)}">
          <div class="chart-bar secondary-${colourIndex % 5}" style="width:${width}%"></div>
        </div>
        <div class="chart-value">${shown}</div>
      </div>`;
    }).join("")}</div>
    <p class="chart-axis-note">Valid n = ${visualNumber(group.validN, 0)}; missing n = ${visualNumber(group.missingN, 0)}</p>
  </section>`;
}

function graphTableHtml(groups) {
  const rows = groups.flatMap(group => group.rows.map(row => ({ ...row, group: group.title })));
  return `<div class="table-wrap"><table class="results-table">
    <thead><tr><th>Country selection</th><th>Processed response</th><th>Count</th><th>Percentage of valid</th></tr></thead>
    <tbody>${rows.map(row => `<tr>
      <td>${visualEscape(row.group)}</td>
      <td>${visualEscape(row.label)}</td>
      <td>${visualNumber(row.count, 0)}</td>
      <td>${visualNumber(row.percentage, 1)}%</td>
    </tr>`).join("")}</tbody></table></div>`;
}

function createGraph(event) {
  event.preventDefault();
  const variableId = document.getElementById("visual-variable").value;
  const variable = visualState.variablesById.get(variableId);
  const countries = visualSelectedCountries();
  const measure = visualMeasure();
  const countryMap = new Map(visualState.countries.map(country => [country.code, country.name]));

  const specifications = countries.length > 1
    ? countries.map(code => ({ title: countryMap.get(code) || code, codes: [code] }))
    : [{
        title: countries.length ? (countryMap.get(countries[0]) || countries[0]) : "All countries combined",
        codes: countries
      }];

  const groups = specifications.map(specification => ({
    ...groupGraphData(variable, specification.codes),
    title: specification.title
  }));

  document.getElementById("visual-title").textContent = `${variable.id}: ${variable.displayName}`;
  document.getElementById("visual-message").textContent = measure === "percentage"
    ? "Bar heights show percentages of valid responses within each country selection."
    : "Bar heights show retained participant counts. Country sample sizes can differ.";
  document.getElementById("visual-chart").innerHTML = groups.map((group, index) => graphGroupHtml(group, measure, index)).join("");
  document.getElementById("visual-chart").setAttribute(
    "aria-label",
    `Bar graph of ${variable.displayName} for ${specifications.map(item => item.title).join(", ")}`
  );
  document.getElementById("visual-table").innerHTML = graphTableHtml(groups);
  document.getElementById("visual-table-disclosure").hidden = false;
  document.getElementById("visual-download").hidden = false;

  visualState.lastRows = groups.flatMap(group => group.rows.map(row => ({
    country_selection: group.title,
    variable: variable.id,
    response: row.label,
    count: row.count,
    percentage_of_valid: row.percentage,
    valid_n: group.validN,
    missing_n: group.missingN
  })));
}

function visualCsvCell(value) {
  const text = value === null || value === undefined ? "" : String(value);
  return `"${text.replaceAll("\\\"", "\\\"\\\"")}"`;
}

function downloadGraphData() {
  const columns = ["country_selection", "variable", "response", "count", "percentage_of_valid", "valid_n", "missing_n"];
  const lines = [columns.join(",")].concat(
    visualState.lastRows.map(row => columns.map(column => visualCsvCell(row[column])).join(","))
  );
  const blob = new Blob([lines.join("\\n")], { type: "text/csv;charset=utf-8" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `worldview-bar-graph-${document.getElementById("visual-variable").value}.csv`;
  document.body.appendChild(link);
  link.click();
  URL.revokeObjectURL(link.href);
  link.remove();
}

function updateAgeControls() {
  document.getElementById("age-band-controls").hidden = document.getElementById("visual-variable").value !== "Q262";
}

function resetVisual() {
  document.getElementById("visual-form").reset();
  [...document.getElementById("visual-countries").options].forEach(option => { option.selected = option.value === "NZL"; });
  document.getElementById("visual-chart").innerHTML = "";
  document.getElementById("visual-title").textContent = "Select data to begin";
  document.getElementById("visual-message").textContent = "Choose a variable and country selection, then create the graph.";
  document.getElementById("visual-table-disclosure").hidden = true;
  document.getElementById("visual-download").hidden = true;
  visualState.lastRows = [];
  updateAgeControls();
}

async function initialiseVisualisations() {
  const loading = document.getElementById("visual-loading");
  try {
    const [dataObject, codebook] = await Promise.all([
      fetch(visualPaths.data).then(response => {
        if (!response.ok) throw new Error(`Data request failed: ${response.status}`);
        return response.json();
      }),
      fetch(visualPaths.codebook).then(response => {
        if (!response.ok) throw new Error(`Codebook request failed: ${response.status}`);
        return response.json();
      })
    ]);

    visualState.data = parseVisualColumns(dataObject);
    visualState.codebook = codebook;
    codebook.variables.forEach(variable => visualState.variablesById.set(variable.id, variable));

    const countryPairs = new Map();
    const names = visualState.data.columns.B_COUNTRY;
    const codes = visualState.data.columns.B_COUNTRY_ALPHA;
    for (let i = 0; i < visualState.data.length; i += 1) countryPairs.set(codes[i], names[i]);
    visualState.countries = [...countryPairs.entries()]
      .map(([code, name]) => ({ code, name }))
      .sort((a, b) => a.name.localeCompare(b.name));

    const variableSelect = document.getElementById("visual-variable");
    codebook.variables.forEach(variable => {
      variableSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
    });

    const countrySelect = document.getElementById("visual-countries");
    visualState.countries.forEach(country => {
      const option = new Option(`${country.name} (${country.code})`, country.code);
      if (country.code === "NZL") option.selected = true;
      countrySelect.add(option);
    });

    variableSelect.addEventListener("change", updateAgeControls);
    updateAgeControls();
    loading.hidden = true;
    document.getElementById("visual-form").hidden = false;
  } catch (error) {
    console.error(error);
    loading.textContent = `Visualisation data could not be loaded: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("visual-form").addEventListener("submit", createGraph);
  document.getElementById("visual-reset").addEventListener("click", resetVisual);
  document.getElementById("visual-download").addEventListener("click", downloadGraphData);
  initialiseVisualisations();
});
'

writeLines(visual_js, visual_js_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(visual_js_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c(
    "visualisation_javascript_created",
    "visualisation_script_linked",
    "visualisation_form_added",
    "variable_selector_added",
    "country_selector_added",
    "percentage_and_count_controls_added",
    "age_banding_added",
    "accessible_table_added",
    "graph_data_download_added",
    "bar_graph_styles_added"
  ),
  passed = c(
    file.exists(visual_js_path),
    grepl("assets/visualisations.js", updated_html, fixed = TRUE),
    grepl('id="visual-form"', updated_html, fixed = TRUE),
    grepl('id="visual-variable"', updated_html, fixed = TRUE),
    grepl('id="visual-countries"', updated_html, fixed = TRUE),
    grepl('name="visual-measure"', updated_html, fixed = TRUE),
    grepl("function ageBand", updated_js, fixed = TRUE),
    grepl('id="visual-table"', updated_html, fixed = TRUE),
    grepl('id="visual-download"', updated_html, fixed = TRUE),
    grepl("Step 8: bar graphs", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step8_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop("Step 8 validation failed: ", paste(validation$check[!validation$passed], collapse = ", "))
}

cat("\nStep 8 completed successfully.\n\n")
cat("Restart the local server, then open Visualisations.\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
