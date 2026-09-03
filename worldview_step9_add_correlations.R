# WorldView Online - Step 9
# Add flexible Pearson correlations to the static WorldView application.

app_dir <- file.path(getwd(), "worldview_static_app")
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
models_js_path <- file.path(app_dir, "assets", "models.js")

required <- c(
  index_path,
  css_path,
  file.path(app_dir, "assets", "app.js"),
  file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
  file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) stop("Required app files are missing: ", paste(missing, collapse = ", "))

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

old_models <- '<section id="page-models" class="page" data-page="models" hidden>
      <div class="page-heading"><p class="eyebrow">Examine associations</p><h1>Models</h1></div>
      <div class="coming-soon"><h2>Application shell ready</h2><p>Pearson correlations will be implemented and checked against the R fixtures.</p></div>
    </section>'

new_models <- '<section id="page-models" class="page" data-page="models" hidden>
      <div class="page-heading">
        <p class="eyebrow">Examine associations</p>
        <h1>Models</h1>
        <p>Calculate Pearson correlations between eligible processed variables.</p>
      </div>

      <div id="model-loading" class="notice" aria-live="polite">Loading data...</div>

      <form id="model-form" class="analysis-layout" hidden>
        <aside class="control-panel">
          <h2>Choose variables</h2>
          <label class="field"><span>Variable 1</span><select id="model-variable-1" required></select></label>
          <label class="field"><span>Variable 2</span><select id="model-variable-2" required></select></label>
          <label class="field"><span>Countries</span>
            <select id="model-countries" multiple size="10" aria-describedby="model-country-help"></select>
          </label>
          <p id="model-country-help" class="field-help">Select one or more countries. If none are selected, all countries are combined.</p>

          <fieldset class="radio-group">
            <legend>Multiple-country output</legend>
            <label><input type="radio" name="model-country-mode" value="combined" checked> Combine selected countries</label>
            <label><input type="radio" name="model-country-mode" value="separate"> Show each country separately</label>
          </fieldset>

          <div class="button-row compact">
            <button class="button button-primary" type="submit">Calculate correlation</button>
            <button id="model-reset" class="button button-secondary" type="button">Reset</button>
          </div>
        </aside>

        <section class="results-panel" aria-labelledby="model-title">
          <div class="results-heading">
            <div><p class="eyebrow">Correlation results</p><h2 id="model-title">Select data to begin</h2></div>
            <button id="model-download" class="button button-secondary" type="button" hidden>Download results</button>
          </div>
          <div id="model-message" class="notice subtle">Choose two different variables and a country selection.</div>
          <div id="model-results" aria-live="polite"></div>
        </section>
      </form>
    </section>'

if (!grepl(old_models, html, fixed = TRUE)) {
  stop("Could not find the models placeholder. Run this script once after Step 8b.")
}
html <- sub(old_models, new_models, html, fixed = TRUE)

if (!grepl("assets/models.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/models.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

models_css <- '

/* Step 9: correlations */
.correlation-card { margin-top: 20px; padding: 20px; border: 1px solid var(--line); border-radius: 14px; background: #fff; }
.correlation-card h3 { margin-top: 0; }
.correlation-figure { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 12px; margin: 16px 0; }
.correlation-figure div { padding: 16px; border-radius: 12px; background: #f1f5f3; }
.correlation-figure strong { display: block; color: var(--teal-dark); font: 700 1.55rem Georgia, serif; }
.correlation-figure span { color: var(--muted); font-size: .82rem; }
.model-note { margin-top: 14px; color: var(--muted); font-size: .88rem; }
'
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 9: correlations", css, fixed = TRUE)) cat(models_css, file = css_path, append = TRUE)

models_js <- '"use strict";

const modelPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const modelState = { data: null, codebook: null, variables: [], countries: [], lastRows: [] };

function modelEscape(value) {
  return String(value ?? "").replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\\\"", "&quot;").replaceAll("\'", "&#039;");
}

function modelNumber(value, digits = 3) {
  if (value === null || !Number.isFinite(value)) return "Not available";
  return new Intl.NumberFormat("en-NZ", { maximumFractionDigits: digits, minimumFractionDigits: digits }).format(value);
}

function parseModelColumns(object) {
  const names = Object.keys(object);
  const length = object[names[0]].length;
  if (!names.length || !names.every(name => object[name].length === length)) throw new Error("Invalid browser data columns.");
  return { columns: object, length };
}

function chosenCountries() {
  return [...document.getElementById("model-countries").selectedOptions].map(option => option.value);
}

function countryMode() {
  return document.querySelector("input[name=model-country-mode]:checked").value;
}

function normalCdf(x) {
  const sign = x < 0 ? -1 : 1;
  const z = Math.abs(x) / Math.sqrt(2);
  const t = 1 / (1 + 0.3275911 * z);
  const erf = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-z * z);
  return 0.5 * (1 + sign * erf);
}

function pearson(x, y) {
  const n = x.length;
  const meanX = x.reduce((a, b) => a + b, 0) / n;
  const meanY = y.reduce((a, b) => a + b, 0) / n;
  let numerator = 0, sumX = 0, sumY = 0;
  for (let i = 0; i < n; i += 1) {
    const dx = x[i] - meanX;
    const dy = y[i] - meanY;
    numerator += dx * dy;
    sumX += dx * dx;
    sumY += dy * dy;
  }
  const denominator = Math.sqrt(sumX * sumY);
  return denominator === 0 ? null : numerator / denominator;
}

function correlationResult(variable1, variable2, codes, title) {
  const selected = new Set(codes);
  const countries = modelState.data.columns.B_COUNTRY_ALPHA;
  const col1 = modelState.data.columns[variable1.id];
  const col2 = modelState.data.columns[variable2.id];
  const x = [], y = [];
  let filteredRows = 0;

  for (let i = 0; i < modelState.data.length; i += 1) {
    if (selected.size && !selected.has(countries[i])) continue;
    filteredRows += 1;
    const a = col1[i], b = col2[i];
    if (a === null || b === null || a === "" || b === "") continue;
    const ax = Number(a), by = Number(b);
    if (Number.isFinite(ax) && Number.isFinite(by)) { x.push(ax); y.push(by); }
  }

  const n = x.length;
  const base = { title, filteredRows, completePairs: n, excluded: filteredRows - n };
  if (n < 10) return { ...base, status: "insufficient" };
  if (new Set(x).size < 2 || new Set(y).size < 2) return { ...base, status: "no_variation" };

  const r = pearson(x, y);
  if (r === null) return { ...base, status: "no_variation" };

  const boundedR = Math.max(-0.999999999, Math.min(0.999999999, r));
  const z = 0.5 * Math.log((1 + boundedR) / (1 - boundedR));
  const se = 1 / Math.sqrt(n - 3);
  const lowerZ = z - 1.959963984540054 * se;
  const upperZ = z + 1.959963984540054 * se;
  const lower = Math.tanh(lowerZ);
  const upper = Math.tanh(upperZ);
  const t = Math.abs(r) * Math.sqrt((n - 2) / (1 - r * r));
  const pApprox = 2 * (1 - normalCdf(t));

  return { ...base, status: "ok", r, lower, upper, pApprox };
}

function directionText(result, v1, v2) {
  if (result.r > 0) return `Higher values of ${v1.displayName} tend to occur with higher values of ${v2.displayName} in this sample.`;
  if (result.r < 0) return `Higher values of ${v1.displayName} tend to occur with lower values of ${v2.displayName} in this sample.`;
  return "The numerical representations show little linear association in this sample.";
}

function resultHtml(result, v1, v2) {
  if (result.status === "insufficient") return `<section class="correlation-card"><h3>${modelEscape(result.title)}</h3><p class="suppression-note">Fewer than 10 complete pairs are available.</p></section>`;
  if (result.status === "no_variation") return `<section class="correlation-card"><h3>${modelEscape(result.title)}</h3><p class="suppression-note">A correlation cannot be calculated because at least one variable has no variation.</p></section>`;

  const pShown = result.pApprox < 0.001 ? "< 0.001" : modelNumber(result.pApprox, 3);
  return `<section class="correlation-card">
    <h3>${modelEscape(result.title)}</h3>
    <div class="correlation-figure">
      <div><strong>${modelNumber(result.r, 3)}</strong><span>Pearson r</span></div>
      <div><strong>${modelNumber(result.completePairs, 0)}</strong><span>Complete pairs</span></div>
      <div><strong>${modelNumber(result.lower, 3)} to ${modelNumber(result.upper, 3)}</strong><span>Approximate 95% CI</span></div>
      <div><strong>${pShown}</strong><span>Approximate p-value</span></div>
    </div>
    <p>${modelEscape(directionText(result, v1, v2))}</p>
    <p class="model-note">${modelEscape(v1.id)}: ${modelEscape(v1.correlationRepresentation)}. ${modelEscape(v2.id)}: ${modelEscape(v2.correlationRepresentation)}.</p>
    <p class="model-note">Rows missing either variable were excluded pairwise (${modelNumber(result.excluded, 0)} rows). Correlation does not establish causation.</p>
  </section>`;
}

function calculateModel(event) {
  event.preventDefault();
  const id1 = document.getElementById("model-variable-1").value;
  const id2 = document.getElementById("model-variable-2").value;
  const v1 = modelState.variables.find(v => v.id === id1);
  const v2 = modelState.variables.find(v => v.id === id2);
  if (id1 === id2) {
    document.getElementById("model-message").textContent = "Choose two different variables.";
    document.getElementById("model-results").innerHTML = "";
    return;
  }

  const selected = chosenCountries();
  const map = new Map(modelState.countries.map(c => [c.code, c.name]));
  let specs;
  if (countryMode() === "separate" && selected.length > 1) {
    specs = selected.map(code => ({ title: map.get(code) || code, codes: [code] }));
  } else {
    specs = [{ title: selected.length ? selected.map(code => map.get(code) || code).join(", ") : "All countries combined", codes: selected }];
  }

  const results = specs.map(spec => correlationResult(v1, v2, spec.codes, spec.title));
  document.getElementById("model-title").textContent = `${v1.id} and ${v2.id}`;
  document.getElementById("model-message").textContent = "Pearson correlation using pairwise complete observations and WorldView numerical representations.";
  document.getElementById("model-results").innerHTML = results.map(result => resultHtml(result, v1, v2)).join("");
  document.getElementById("model-download").hidden = false;
  modelState.lastRows = results.map(result => ({ country_selection: result.title, variable_1: id1, variable_2: id2, status: result.status, filtered_rows: result.filteredRows, complete_pairs: result.completePairs, excluded_missing_either: result.excluded, pearson_r: result.r ?? "", ci_lower: result.lower ?? "", ci_upper: result.upper ?? "", approximate_p_value: result.pApprox ?? "" }));
}

function csvCell(value) {
  const text = value === null || value === undefined ? "" : String(value);
  return `"${text.replaceAll("\\\"", "\\\"\\\"")}"`;
}

function downloadModel() {
  const columns = ["country_selection", "variable_1", "variable_2", "status", "filtered_rows", "complete_pairs", "excluded_missing_either", "pearson_r", "ci_lower", "ci_upper", "approximate_p_value"];
  const lines = [columns.join(",")].concat(modelState.lastRows.map(row => columns.map(column => csvCell(row[column])).join(",")));
  const blob = new Blob([lines.join("\\n")], { type: "text/csv;charset=utf-8" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `worldview-correlation-${document.getElementById("model-variable-1").value}-${document.getElementById("model-variable-2").value}.csv`;
  document.body.appendChild(link); link.click(); URL.revokeObjectURL(link.href); link.remove();
}

function resetModel() {
  document.getElementById("model-form").reset();
  [...document.getElementById("model-countries").options].forEach(option => { option.selected = option.value === "NZL"; });
  document.getElementById("model-title").textContent = "Select data to begin";
  document.getElementById("model-message").textContent = "Choose two different variables and a country selection.";
  document.getElementById("model-results").innerHTML = "";
  document.getElementById("model-download").hidden = true;
}

async function initialiseModels() {
  const loading = document.getElementById("model-loading");
  try {
    const [dataObject, codebook] = await Promise.all([
      fetch(modelPaths.data).then(r => { if (!r.ok) throw new Error(`Data request failed: ${r.status}`); return r.json(); }),
      fetch(modelPaths.codebook).then(r => { if (!r.ok) throw new Error(`Codebook request failed: ${r.status}`); return r.json(); })
    ]);
    modelState.data = parseModelColumns(dataObject);
    modelState.codebook = codebook;
    modelState.variables = codebook.variables.filter(variable => variable.correlationEligible);

    const pairs = new Map();
    for (let i = 0; i < modelState.data.length; i += 1) pairs.set(modelState.data.columns.B_COUNTRY_ALPHA[i], modelState.data.columns.B_COUNTRY[i]);
    modelState.countries = [...pairs.entries()].map(([code, name]) => ({ code, name })).sort((a, b) => a.name.localeCompare(b.name));

    const select1 = document.getElementById("model-variable-1");
    const select2 = document.getElementById("model-variable-2");
    modelState.variables.forEach(variable => {
      select1.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
      select2.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
    });
    select1.value = "Q165";
    select2.value = "Q167";

    const countrySelect = document.getElementById("model-countries");
    modelState.countries.forEach(country => {
      const option = new Option(`${country.name} (${country.code})`, country.code);
      if (country.code === "NZL") option.selected = true;
      countrySelect.add(option);
    });

    loading.hidden = true;
    document.getElementById("model-form").hidden = false;
  } catch (error) {
    console.error(error);
    loading.textContent = `Model data could not be loaded: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("model-form").addEventListener("submit", calculateModel);
  document.getElementById("model-reset").addEventListener("click", resetModel);
  document.getElementById("model-download").addEventListener("click", downloadModel);
  initialiseModels();
});
'

writeLines(models_js, models_js_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(models_js_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c("models_script_created", "models_script_linked", "models_form_added", "two_variable_selectors_added", "country_mode_added", "pearson_function_added", "pairwise_missing_logic_added", "confidence_interval_added", "download_added", "styles_added"),
  passed = c(file.exists(models_js_path), grepl("assets/models.js", updated_html, fixed = TRUE), grepl('id="model-form"', updated_html, fixed = TRUE), grepl('id="model-variable-1"', updated_html, fixed = TRUE) && grepl('id="model-variable-2"', updated_html, fixed = TRUE), grepl('name="model-country-mode"', updated_html, fixed = TRUE), grepl("function pearson", updated_js, fixed = TRUE), grepl("completePairs", updated_js, fixed = TRUE), grepl("Math.tanh", updated_js, fixed = TRUE), grepl('id="model-download"', updated_html, fixed = TRUE), grepl("Step 9: correlations", updated_css, fixed = TRUE)),
  stringsAsFactors = FALSE
)
write.csv(validation, file.path(app_dir, "step9_validation_checks.csv"), row.names = FALSE, na = "")
if (!all(validation$passed)) stop("Step 9 validation failed: ", paste(validation$check[!validation$passed], collapse = ", "))

cat("\nStep 9 completed successfully.\n\n")
cat("Restart the local server, then open Models:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
