"use strict";

const modelPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const modelState = { data: null, codebook: null, variables: [], countries: [], lastRows: [] };

function modelEscape(value) {
  return String(value ?? "").replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;").replaceAll("'", "&#039;");
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
  return `"${text.replaceAll("\"", "\"\"")}"`;
}

function downloadModel() {
  const columns = ["country_selection", "variable_1", "variable_2", "status", "filtered_rows", "complete_pairs", "excluded_missing_either", "pearson_r", "ci_lower", "ci_upper", "approximate_p_value"];
  const lines = [columns.join(",")].concat(modelState.lastRows.map(row => columns.map(column => csvCell(row[column])).join(",")));
  const blob = new Blob([lines.join("\n")], { type: "text/csv;charset=utf-8" });
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

