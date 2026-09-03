# WorldView Online - Step 12
# Add simple linear regression to the Models page.
# Model: outcome = intercept + slope * predictor

app_dir <- file.path(getwd(), "worldview_static_app")
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
regression_js_path <- file.path(app_dir, "assets", "regression.js")

required <- c(
  index_path,
  css_path,
  file.path(app_dir, "assets", "models.js"),
  file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
  file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Required app files are missing: ", paste(missing, collapse = ", "))
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Insert the regression interface directly after the correlation form.
model_form_end <- '      </form>
    </section>

    <section id="page-download"'

regression_section <- '      </form>

      <hr class="model-divider">

      <div class="page-heading model-subheading">
        <p class="eyebrow">Estimate a linear relationship</p>
        <h2>Linear regression</h2>
        <p>Fit a simple ordinary least-squares model with one outcome and one predictor.</p>
      </div>

      <div id="regression-loading" class="notice" aria-live="polite">Loading regression data...</div>

      <form id="regression-form" class="analysis-layout" hidden>
        <aside class="control-panel">
          <h2>Specify model</h2>

          <label class="field">
            <span>Outcome variable</span>
            <select id="regression-outcome" required></select>
          </label>

          <label class="field">
            <span>Predictor variable</span>
            <select id="regression-predictor" required></select>
          </label>

          <label class="field">
            <span>Countries</span>
            <select id="regression-countries" multiple size="10" aria-describedby="regression-country-help"></select>
          </label>
          <p id="regression-country-help" class="field-help">Select one or more countries. If none are selected, all countries are combined.</p>

          <fieldset class="radio-group">
            <legend>Multiple-country output</legend>
            <label><input type="radio" name="regression-country-mode" value="combined" checked> Combine selected countries</label>
            <label><input type="radio" name="regression-country-mode" value="separate"> Show each country separately</label>
          </fieldset>

          <div class="button-row compact">
            <button class="button button-primary" type="submit">Fit linear regression</button>
            <button id="regression-reset" class="button button-secondary" type="button">Reset</button>
          </div>
        </aside>

        <section class="results-panel" aria-labelledby="regression-title">
          <div class="results-heading">
            <div>
              <p class="eyebrow">Regression results</p>
              <h2 id="regression-title">Specify a model to begin</h2>
            </div>
            <button id="regression-download" class="button button-secondary" type="button" hidden>Download results</button>
          </div>

          <div id="regression-message" class="notice subtle">Choose an outcome, predictor, and country selection.</div>
          <div id="regression-results" aria-live="polite"></div>
        </section>
      </form>
    </section>

    <section id="page-download"'

if (!grepl('id="regression-form"', html, fixed = TRUE)) {
  if (!grepl(model_form_end, html, fixed = TRUE)) {
    stop("Could not locate the end of the Models page in index.html.")
  }
  html <- sub(model_form_end, regression_section, html, fixed = TRUE)
}

if (!grepl("assets/regression.js", html, fixed = TRUE)) {
  html <- sub(
    "</head>",
    '  <script src="assets/regression.js" defer></script>\n</head>',
    html,
    fixed = TRUE
  )
}
writeLines(html, index_path, useBytes = TRUE)

regression_css <- '

/* Step 12: linear regression */
.model-divider {
  margin: 54px 0 42px;
  border: 0;
  border-top: 1px solid var(--line);
}
.model-subheading h2 {
  margin: 0 0 12px;
  font: 700 clamp(2rem, 4vw, 3rem)/1.08 Georgia, Cambria, serif;
}
.regression-card {
  margin-top: 20px;
  padding: 20px;
  border: 1px solid var(--line);
  border-radius: 14px;
  background: white;
}
.regression-card h3 { margin-top: 0; }
.model-fit-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(130px, 1fr));
  gap: 12px;
  margin: 16px 0 22px;
}
.model-fit-grid div {
  padding: 15px;
  border-radius: 12px;
  background: #f1f5f3;
}
.model-fit-grid strong {
  display: block;
  color: var(--teal-dark);
  font: 700 1.4rem Georgia, serif;
}
.model-fit-grid span {
  color: var(--muted);
  font-size: .82rem;
}
.equation-box {
  margin: 16px 0;
  padding: 14px 16px;
  border-left: 4px solid var(--gold);
  background: #fff9e8;
  font-family: ui-monospace, SFMono-Regular, Consolas, monospace;
  overflow-wrap: anywhere;
}
'
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 12: linear regression", css, fixed = TRUE)) {
  cat(regression_css, file = css_path, append = TRUE)
}

regression_js <- '"use strict";

const regressionPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const regressionState = {
  data: null,
  variables: [],
  countries: [],
  lastRows: []
};

function regressionEscape(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\\\"", "&quot;")
    .replaceAll("\'", "&#039;");
}

function regressionNumber(value, digits = 3) {
  if (value === null || value === undefined || !Number.isFinite(value)) return "Not available";
  return new Intl.NumberFormat("en-NZ", {
    maximumFractionDigits: digits,
    minimumFractionDigits: digits
  }).format(value);
}

function parseRegressionColumns(object) {
  const names = Object.keys(object);
  if (!names.length) throw new Error("The browser dataset has no columns.");
  const length = object[names[0]].length;
  if (!names.every(name => object[name].length === length)) {
    throw new Error("Browser data columns have different lengths.");
  }
  return { columns: object, length };
}

function selectedRegressionCountries() {
  return [...document.getElementById("regression-countries").selectedOptions]
    .map(option => option.value);
}

function regressionCountryMode() {
  return document.querySelector("input[name=regression-country-mode]:checked").value;
}

function normalCdf(x) {
  const sign = x < 0 ? -1 : 1;
  const z = Math.abs(x) / Math.sqrt(2);
  const t = 1 / (1 + 0.3275911 * z);
  const erf = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-z * z);
  return 0.5 * (1 + sign * erf);
}

function fitSimpleRegression(outcomeId, predictorId, countryCodes, title) {
  const selected = new Set(countryCodes);
  const countryColumn = regressionState.data.columns.B_COUNTRY_ALPHA;
  const outcomeColumn = regressionState.data.columns[outcomeId];
  const predictorColumn = regressionState.data.columns[predictorId];
  const x = [];
  const y = [];
  let filteredRows = 0;

  for (let i = 0; i < regressionState.data.length; i += 1) {
    if (selected.size && !selected.has(countryColumn[i])) continue;
    filteredRows += 1;

    const rawY = outcomeColumn[i];
    const rawX = predictorColumn[i];
    if (rawY === null || rawX === null || rawY === "" || rawX === "") continue;

    const numericY = Number(rawY);
    const numericX = Number(rawX);
    if (Number.isFinite(numericY) && Number.isFinite(numericX)) {
      y.push(numericY);
      x.push(numericX);
    }
  }

  const n = x.length;
  const base = {
    title,
    filteredRows,
    completeCases: n,
    excludedMissingEither: filteredRows - n
  };

  if (n < 10) return { ...base, status: "insufficient" };
  if (new Set(x).size < 2) return { ...base, status: "no_predictor_variation" };
  if (new Set(y).size < 2) return { ...base, status: "no_outcome_variation" };

  const meanX = x.reduce((sum, value) => sum + value, 0) / n;
  const meanY = y.reduce((sum, value) => sum + value, 0) / n;

  let sxx = 0;
  let sxy = 0;
  let syy = 0;
  for (let i = 0; i < n; i += 1) {
    const dx = x[i] - meanX;
    const dy = y[i] - meanY;
    sxx += dx * dx;
    sxy += dx * dy;
    syy += dy * dy;
  }

  const slope = sxy / sxx;
  const intercept = meanY - slope * meanX;

  let sse = 0;
  for (let i = 0; i < n; i += 1) {
    const residual = y[i] - (intercept + slope * x[i]);
    sse += residual * residual;
  }

  const degreesFreedom = n - 2;
  const residualVariance = sse / degreesFreedom;
  const residualStandardError = Math.sqrt(residualVariance);
  const slopeSE = Math.sqrt(residualVariance / sxx);
  const interceptSE = Math.sqrt(residualVariance * (1 / n + (meanX * meanX) / sxx));
  const slopeT = slope / slopeSE;
  const interceptT = intercept / interceptSE;
  const rSquared = syy === 0 ? null : 1 - sse / syy;
  const adjustedRSquared = rSquared === null ? null : 1 - (1 - rSquared) * (n - 1) / (n - 2);
  const fStatistic = rSquared === null || rSquared >= 1
    ? null
    : (rSquared / (1 - rSquared)) * degreesFreedom;

  // Normal-approximation p-values are adequate for this teaching application.
  // Confidence intervals use 1.96 because results need not replicate R exactly.
  const slopeP = 2 * (1 - normalCdf(Math.abs(slopeT)));
  const interceptP = 2 * (1 - normalCdf(Math.abs(interceptT)));
  const critical = 1.959963984540054;

  return {
    ...base,
    status: "ok",
    n,
    degreesFreedom,
    intercept,
    interceptSE,
    interceptT,
    interceptP,
    interceptLower: intercept - critical * interceptSE,
    interceptUpper: intercept + critical * interceptSE,
    slope,
    slopeSE,
    slopeT,
    slopeP,
    slopeLower: slope - critical * slopeSE,
    slopeUpper: slope + critical * slopeSE,
    rSquared,
    adjustedRSquared,
    residualStandardError,
    fStatistic
  };
}

function regressionInterpretation(result, outcome, predictor) {
  const direction = result.slope > 0 ? "increase" : result.slope < 0 ? "decrease" : "change";
  return `For each one-unit increase in ${predictor.displayName}, the predicted ${outcome.displayName} is estimated to ${direction} by ${regressionNumber(Math.abs(result.slope), 3)} units in this sample.`;
}

function coefficientTable(result, outcome, predictor) {
  const rows = [
    {
      term: "Intercept",
      estimate: result.intercept,
      standardError: result.interceptSE,
      statistic: result.interceptT,
      pValue: result.interceptP,
      lower: result.interceptLower,
      upper: result.interceptUpper
    },
    {
      term: predictor.displayName,
      estimate: result.slope,
      standardError: result.slopeSE,
      statistic: result.slopeT,
      pValue: result.slopeP,
      lower: result.slopeLower,
      upper: result.slopeUpper
    }
  ];

  return `<div class="table-wrap"><table class="results-table">
    <thead><tr>
      <th>Term</th><th>Estimate</th><th>SE</th><th>t</th><th>Approx. p</th><th>Approx. 95% CI</th>
    </tr></thead>
    <tbody>${rows.map(row => `<tr>
      <td>${regressionEscape(row.term)}</td>
      <td>${regressionNumber(row.estimate, 3)}</td>
      <td>${regressionNumber(row.standardError, 3)}</td>
      <td>${regressionNumber(row.statistic, 3)}</td>
      <td>${row.pValue < 0.001 ? "&lt; 0.001" : regressionNumber(row.pValue, 3)}</td>
      <td>${regressionNumber(row.lower, 3)} to ${regressionNumber(row.upper, 3)}</td>
    </tr>`).join("")}</tbody>
  </table></div>`;
}

function regressionResultHtml(result, outcome, predictor) {
  if (result.status === "insufficient") {
    return `<section class="regression-card"><h3>${regressionEscape(result.title)}</h3><p class="suppression-note">Fewer than 10 complete observations are available.</p></section>`;
  }
  if (result.status === "no_predictor_variation") {
    return `<section class="regression-card"><h3>${regressionEscape(result.title)}</h3><p class="suppression-note">The predictor has no variation, so the model cannot be fitted.</p></section>`;
  }
  if (result.status === "no_outcome_variation") {
    return `<section class="regression-card"><h3>${regressionEscape(result.title)}</h3><p class="suppression-note">The outcome has no variation, so the model cannot be fitted.</p></section>`;
  }

  return `<section class="regression-card">
    <h3>${regressionEscape(result.title)}</h3>
    <div class="equation-box">Predicted ${regressionEscape(outcome.id)} = ${regressionNumber(result.intercept, 3)} + ${regressionNumber(result.slope, 3)} × ${regressionEscape(predictor.id)}</div>
    <div class="model-fit-grid">
      <div><strong>${regressionNumber(result.n, 0)}</strong><span>Complete observations</span></div>
      <div><strong>${regressionNumber(result.rSquared, 3)}</strong><span>R squared</span></div>
      <div><strong>${regressionNumber(result.adjustedRSquared, 3)}</strong><span>Adjusted R squared</span></div>
      <div><strong>${regressionNumber(result.residualStandardError, 3)}</strong><span>Residual standard error</span></div>
      <div><strong>${regressionNumber(result.fStatistic, 3)}</strong><span>F statistic</span></div>
    </div>
    <h4>Coefficients</h4>
    ${coefficientTable(result, outcome, predictor)}
    <p>${regressionEscape(regressionInterpretation(result, outcome, predictor))}</p>
    <p class="model-note">Outcome coding: ${regressionEscape(outcome.correlationRepresentation)}. Predictor coding: ${regressionEscape(predictor.correlationRepresentation)}.</p>
    <p class="model-note">Rows missing either variable were excluded (${regressionNumber(result.excludedMissingEither, 0)} rows). This is an unweighted ordinary least-squares model. Association does not establish causation.</p>
  </section>`;
}

function calculateRegression(event) {
  event.preventDefault();

  const outcomeId = document.getElementById("regression-outcome").value;
  const predictorId = document.getElementById("regression-predictor").value;
  const outcome = regressionState.variables.find(variable => variable.id === outcomeId);
  const predictor = regressionState.variables.find(variable => variable.id === predictorId);

  if (outcomeId === predictorId) {
    document.getElementById("regression-message").textContent = "Choose different outcome and predictor variables.";
    document.getElementById("regression-results").innerHTML = "";
    document.getElementById("regression-download").hidden = true;
    return;
  }

  const selectedCountries = selectedRegressionCountries();
  const countryMap = new Map(regressionState.countries.map(country => [country.code, country.name]));
  let specifications;

  if (regressionCountryMode() === "separate" && selectedCountries.length > 1) {
    specifications = selectedCountries.map(code => ({
      title: countryMap.get(code) || code,
      codes: [code]
    }));
  } else {
    specifications = [{
      title: selectedCountries.length
        ? selectedCountries.map(code => countryMap.get(code) || code).join(", ")
        : "All countries combined",
      codes: selectedCountries
    }];
  }

  const results = specifications.map(specification =>
    fitSimpleRegression(outcomeId, predictorId, specification.codes, specification.title)
  );

  document.getElementById("regression-title").textContent = `${outcomeId} predicted by ${predictorId}`;
  document.getElementById("regression-message").textContent = "Simple ordinary least-squares regression using complete observations for the selected outcome and predictor.";
  document.getElementById("regression-results").innerHTML = results
    .map(result => regressionResultHtml(result, outcome, predictor))
    .join("");
  document.getElementById("regression-download").hidden = false;

  regressionState.lastRows = results.map(result => ({
    country_selection: result.title,
    outcome: outcomeId,
    predictor: predictorId,
    status: result.status,
    filtered_rows: result.filteredRows,
    complete_observations: result.completeCases,
    excluded_missing_either: result.excludedMissingEither,
    intercept: result.intercept ?? "",
    intercept_se: result.interceptSE ?? "",
    intercept_t: result.interceptT ?? "",
    intercept_approximate_p: result.interceptP ?? "",
    slope: result.slope ?? "",
    slope_se: result.slopeSE ?? "",
    slope_t: result.slopeT ?? "",
    slope_approximate_p: result.slopeP ?? "",
    slope_ci_lower: result.slopeLower ?? "",
    slope_ci_upper: result.slopeUpper ?? "",
    r_squared: result.rSquared ?? "",
    adjusted_r_squared: result.adjustedRSquared ?? "",
    residual_standard_error: result.residualStandardError ?? "",
    f_statistic: result.fStatistic ?? ""
  }));
}

function regressionCsvCell(value) {
  const text = value === null || value === undefined ? "" : String(value);
  return `"${text.replaceAll("\\\"", "\\\"\\\"")}"`;
}

function downloadRegression() {
  if (!regressionState.lastRows.length) return;
  const columns = Object.keys(regressionState.lastRows[0]);
  const lines = [columns.join(",")].concat(
    regressionState.lastRows.map(row =>
      columns.map(column => regressionCsvCell(row[column])).join(",")
    )
  );
  const blob = new Blob([lines.join("\\n")], { type: "text/csv;charset=utf-8" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `worldview-linear-regression-${document.getElementById("regression-outcome").value}-on-${document.getElementById("regression-predictor").value}.csv`;
  document.body.appendChild(link);
  link.click();
  URL.revokeObjectURL(link.href);
  link.remove();
}

function resetRegression() {
  document.getElementById("regression-form").reset();
  document.getElementById("regression-outcome").value = "Q167";
  document.getElementById("regression-predictor").value = "Q165";
  [...document.getElementById("regression-countries").options]
    .forEach(option => { option.selected = option.value === "NZL"; });
  document.getElementById("regression-title").textContent = "Specify a model to begin";
  document.getElementById("regression-message").textContent = "Choose an outcome, predictor, and country selection.";
  document.getElementById("regression-results").innerHTML = "";
  document.getElementById("regression-download").hidden = true;
  regressionState.lastRows = [];
}

async function initialiseRegression() {
  const loading = document.getElementById("regression-loading");
  try {
    const [rawData, codebook] = await Promise.all([
      fetch(regressionPaths.data).then(response => {
        if (!response.ok) throw new Error(`Data request failed: ${response.status}`);
        return response.json();
      }),
      fetch(regressionPaths.codebook).then(response => {
        if (!response.ok) throw new Error(`Codebook request failed: ${response.status}`);
        return response.json();
      })
    ]);

    regressionState.data = parseRegressionColumns(rawData);
    regressionState.variables = codebook.variables.filter(variable => variable.correlationEligible);

    const countryPairs = new Map();
    for (let i = 0; i < regressionState.data.length; i += 1) {
      countryPairs.set(
        regressionState.data.columns.B_COUNTRY_ALPHA[i],
        regressionState.data.columns.B_COUNTRY[i]
      );
    }
    regressionState.countries = [...countryPairs.entries()]
      .map(([code, name]) => ({ code, name }))
      .sort((a, b) => a.name.localeCompare(b.name));

    const outcomeSelect = document.getElementById("regression-outcome");
    const predictorSelect = document.getElementById("regression-predictor");
    regressionState.variables.forEach(variable => {
      outcomeSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
      predictorSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
    });
    outcomeSelect.value = "Q167";
    predictorSelect.value = "Q165";

    const countrySelect = document.getElementById("regression-countries");
    regressionState.countries.forEach(country => {
      const option = new Option(`${country.name} (${country.code})`, country.code);
      if (country.code === "NZL") option.selected = true;
      countrySelect.add(option);
    });

    loading.hidden = true;
    document.getElementById("regression-form").hidden = false;
  } catch (error) {
    console.error(error);
    loading.textContent = `Regression data could not be loaded: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("regression-form").addEventListener("submit", calculateRegression);
  document.getElementById("regression-reset").addEventListener("click", resetRegression);
  document.getElementById("regression-download").addEventListener("click", downloadRegression);
  initialiseRegression();
});
'

writeLines(regression_js, regression_js_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(regression_js_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c(
    "regression_form_added",
    "regression_script_created",
    "regression_script_linked",
    "outcome_and_predictor_selectors_added",
    "combined_and_separate_output_added",
    "ols_calculation_added",
    "coefficient_standard_errors_added",
    "r_squared_added",
    "confidence_intervals_added",
    "download_added",
    "regression_styles_added"
  ),
  passed = c(
    grepl('id="regression-form"', updated_html, fixed = TRUE),
    file.exists(regression_js_path),
    grepl("assets/regression.js", updated_html, fixed = TRUE),
    grepl('id="regression-outcome"', updated_html, fixed = TRUE) && grepl('id="regression-predictor"', updated_html, fixed = TRUE),
    grepl('name="regression-country-mode"', updated_html, fixed = TRUE),
    grepl("function fitSimpleRegression", updated_js, fixed = TRUE),
    grepl("slopeSE", updated_js, fixed = TRUE),
    grepl("rSquared", updated_js, fixed = TRUE),
    grepl("slopeLower", updated_js, fixed = TRUE),
    grepl('id="regression-download"', updated_html, fixed = TRUE),
    grepl("Step 12: linear regression", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step12_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 12 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 12 completed successfully.\n\n")
cat("Restart the local app, then open Models and scroll to Linear regression:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
