"use strict";

const summaryPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const summaryState = {
  data: null,
  codebook: null,
  variablesById: new Map(),
  countries: [],
  lastRows: []
};

function summaryEscape(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#039;");
}

function summaryNumber(value, digits = 2) {
  if (value === null || value === undefined || !Number.isFinite(value)) return "Not available";
  return new Intl.NumberFormat("en-NZ", { maximumFractionDigits: digits }).format(value);
}

function parseColumnJson(object) {
  const names = Object.keys(object);
  if (!names.length) throw new Error("The browser data has no columns.");
  const length = object[names[0]].length;
  if (!names.every(name => object[name].length === length)) {
    throw new Error("Browser data columns have different lengths.");
  }
  return { columns: object, length };
}

function selectedCountries() {
  return [...document.getElementById("summary-countries").selectedOptions].map(option => option.value);
}

function selectedMode() {
  return document.querySelector("input[name=summary-mode]:checked").value;
}

function rowsForCountries(countries) {
  const selected = new Set(countries);
  const countryCodes = summaryState.data.columns.B_COUNTRY_ALPHA;
  const indices = [];
  for (let i = 0; i < summaryState.data.length; i += 1) {
    if (!selected.size || selected.has(countryCodes[i])) indices.push(i);
  }
  return indices;
}

function validValues(variableId, indices) {
  const column = summaryState.data.columns[variableId];
  return indices.map(i => column[i]).filter(value => value !== null && value !== undefined && value !== "");
}

function quantileType7(sorted, probability) {
  if (!sorted.length) return null;
  const position = (sorted.length - 1) * probability;
  const lower = Math.floor(position);
  const upper = Math.ceil(position);
  if (lower === upper) return sorted[lower];
  const weight = position - lower;
  return sorted[lower] + weight * (sorted[upper] - sorted[lower]);
}

function numericStatistics(values, totalN) {
  const numeric = values.map(Number).filter(Number.isFinite).sort((a, b) => a - b);
  const n = numeric.length;
  if (!n) return { totalN, validN: 0, missingN: totalN };
  const mean = numeric.reduce((sum, value) => sum + value, 0) / n;
  const variance = n > 1
    ? numeric.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (n - 1)
    : null;
  return {
    totalN,
    validN: n,
    missingN: totalN - n,
    mean,
    standardDeviation: variance === null ? null : Math.sqrt(variance),
    median: quantileType7(numeric, 0.5),
    firstQuartile: quantileType7(numeric, 0.25),
    thirdQuartile: quantileType7(numeric, 0.75),
    minimum: numeric[0],
    maximum: numeric[n - 1]
  };
}

function displayLabel(variable, value) {
  if (variable.id === "Q289") return String(value);
  if (["Q165", "Q166", "Q167", "Q168"].includes(variable.id)) return Number(value) === 1 ? "Yes" : "No";
  if (variable.id === "Q260") return Number(value) === 1 ? "Male" : "Female";
  if (variable.id === "Q263") return Number(value) === 1
    ? "I am born in this country"
    : "I am an immigrant to this country (born outside this country)";
  if (Array.isArray(variable.levels) && variable.levels.length) {
    const match = variable.levels.find(level => Number(level.displayOrder) === Number(value));
    return match ? match.label : String(value);
  }
  return String(value);
}

function frequencyStatistics(variable, values, totalN) {
  const counts = new Map();
  values.forEach(value => {
    const key = String(value);
    counts.set(key, (counts.get(key) || 0) + 1);
  });
  const validN = values.length;
  let keys = [...counts.keys()];
  if (variable.id !== "Q289") keys.sort((a, b) => Number(a) - Number(b));
  else keys.sort((a, b) => displayLabel(variable, a).localeCompare(displayLabel(variable, b)));
  return {
    totalN,
    validN,
    missingN: totalN - validN,
    categories: keys.map(key => ({
      value: key,
      label: displayLabel(variable, key),
      count: counts.get(key),
      percentage: validN ? 100 * counts.get(key) / validN : null
    }))
  };
}

function statCards(stats) {
  return `<div class="stat-cards">
    <div class="stat-card"><strong>${summaryNumber(stats.totalN, 0)}</strong><span>Total sampled</span></div>
    <div class="stat-card"><strong>${summaryNumber(stats.validN, 0)}</strong><span>Valid</span></div>
    <div class="stat-card"><strong>${summaryNumber(stats.missingN, 0)}</strong><span>Missing</span></div>
  </div>`;
}

function numericTable(stats) {
  if (stats.validN < 10) return `<p class="suppression-note">A substantive summary is not displayed because fewer than 10 valid observations are available.</p>`;
  const rows = [
    ["Mean", stats.mean], ["Standard deviation", stats.standardDeviation],
    ["Minimum", stats.minimum], ["First quartile", stats.firstQuartile],
    ["Median", stats.median], ["Third quartile", stats.thirdQuartile],
    ["Maximum", stats.maximum]
  ];
  return `<div class="table-wrap"><table class="results-table">
    <thead><tr><th>Statistic</th><th>Value</th></tr></thead>
    <tbody>${rows.map(row => `<tr><td>${row[0]}</td><td>${summaryNumber(row[1], 2)}</td></tr>`).join("")}</tbody>
  </table></div>`;
}

function frequencyTable(stats) {
  if (stats.validN < 10) return `<p class="suppression-note">A response distribution is not displayed because fewer than 10 valid observations are available.</p>`;
  return `<div class="table-wrap"><table class="results-table">
    <thead><tr><th>Processed response</th><th>Count</th><th>Percentage of valid</th></tr></thead>
    <tbody>${stats.categories.map(category => `<tr>
      <td>${summaryEscape(category.label)}</td>
      <td>${summaryNumber(category.count, 0)}</td>
      <td>${summaryNumber(category.percentage, 1)}%</td>
    </tr>`).join("")}</tbody>
  </table></div>`;
}

function analyseGroup(variable, indices, title) {
  const values = validValues(variable.id, indices);
  const frequency = frequencyStatistics(variable, values, indices.length);
  const includeNumeric = variable.analysisType === "integer" || variable.analysisType === "ordinal";
  const numeric = includeNumeric ? numericStatistics(values, indices.length) : null;
  const csvRows = [];

  frequency.categories.forEach(category => csvRows.push({
    group: title,
    variable: variable.id,
    result_type: "frequency",
    item: category.label,
    value: category.count,
    percentage_of_valid: category.percentage
  }));

  if (numeric && numeric.validN >= 10) {
    [
      ["mean", numeric.mean], ["standard_deviation", numeric.standardDeviation],
      ["minimum", numeric.minimum], ["first_quartile", numeric.firstQuartile],
      ["median", numeric.median], ["third_quartile", numeric.thirdQuartile],
      ["maximum", numeric.maximum]
    ].forEach(item => csvRows.push({
      group: title, variable: variable.id, result_type: "numeric_summary",
      item: item[0], value: item[1], percentage_of_valid: ""
    }));
  }

  const isScalar = variable.analysisType === "integer" || variable.analysisType === "ordinal";

  const resultContent = isScalar
    ? `${numeric ? `<h4>Numerical summary</h4>${numericTable(numeric)}` : ""}
       <details class="full-table-disclosure">
         <summary>Show full response table</summary>
         <div class="disclosure-content">
           <p class="field-help">Counts and percentages for every observed processed value.</p>
           ${frequencyTable(frequency)}
         </div>
       </details>`
    : `<h4>Response distribution</h4>${frequencyTable(frequency)}`;

  return {
    html: `<section class="result-block">
      <h3>${summaryEscape(title)}</h3>
      ${statCards(frequency)}
      ${resultContent}
    </section>`,
    csvRows
  };
}

function calculateSummary(event) {
  event.preventDefault();
  const variableId = document.getElementById("summary-variable").value;
  const variable = summaryState.variablesById.get(variableId);
  const countries = selectedCountries();
  const mode = selectedMode();
  const countryMap = new Map(summaryState.countries.map(country => [country.code, country.name]));
  let groups = [];

  if (mode === "separate" && countries.length > 1) {
    groups = countries.map(code => ({
      title: countryMap.get(code) || code,
      indices: rowsForCountries([code])
    }));
  } else {
    const title = countries.length
      ? countries.map(code => countryMap.get(code) || code).join(", ")
      : "All countries combined";
    groups = [{ title, indices: rowsForCountries(countries) }];
  }

  const analyses = groups.map(group => analyseGroup(variable, group.indices, group.title));
  document.getElementById("summary-results-title").textContent = `${variable.id}: ${variable.displayName}`;
  document.getElementById("summary-message").innerHTML =
    `Percentages use valid responses. Missing values are excluded from each statistic. ${summaryEscape(variable.interpretationNote)}`;
  document.getElementById("summary-results").innerHTML = analyses.map(result => result.html).join("");
  summaryState.lastRows = analyses.flatMap(result => result.csvRows);
  document.getElementById("summary-download").hidden = false;
}

function csvCell(value) {
  const text = value === null || value === undefined ? "" : String(value);
  return `"${text.replaceAll("\"", "\"\"")}"`;
}

function downloadSummary() {
  if (!summaryState.lastRows.length) return;
  const columns = ["group", "variable", "result_type", "item", "value", "percentage_of_valid"];
  const lines = [columns.join(",")].concat(
    summaryState.lastRows.map(row => columns.map(column => csvCell(row[column])).join(","))
  );
  const blob = new Blob([lines.join("\n")], { type: "text/csv;charset=utf-8" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `worldview-summary-${document.getElementById("summary-variable").value}.csv`;
  document.body.appendChild(link);
  link.click();
  URL.revokeObjectURL(link.href);
  link.remove();
}

function resetSummary() {
  document.getElementById("summary-form").reset();
  [...document.getElementById("summary-countries").options].forEach(option => { option.selected = option.value === "NZL"; });
  document.getElementById("summary-results-title").textContent = "Select data to begin";
  document.getElementById("summary-message").textContent = "Choose a variable and country selection, then calculate the summary.";
  document.getElementById("summary-results").innerHTML = "";
  document.getElementById("summary-download").hidden = true;
  summaryState.lastRows = [];
}

async function initialiseSummary() {
  const loading = document.getElementById("summary-loading");
  try {
    const [dataObject, codebook] = await Promise.all([
      fetch(summaryPaths.data).then(response => {
        if (!response.ok) throw new Error(`Data request failed: ${response.status}`);
        return response.json();
      }),
      fetch(summaryPaths.codebook).then(response => {
        if (!response.ok) throw new Error(`Codebook request failed: ${response.status}`);
        return response.json();
      })
    ]);

    summaryState.data = parseColumnJson(dataObject);
    summaryState.codebook = codebook;
    codebook.variables.forEach(variable => summaryState.variablesById.set(variable.id, variable));

    const countryPairs = new Map();
    const names = summaryState.data.columns.B_COUNTRY;
    const codes = summaryState.data.columns.B_COUNTRY_ALPHA;
    for (let i = 0; i < summaryState.data.length; i += 1) countryPairs.set(codes[i], names[i]);
    summaryState.countries = [...countryPairs.entries()]
      .map(([code, name]) => ({ code, name }))
      .sort((a, b) => a.name.localeCompare(b.name));

    const variableSelect = document.getElementById("summary-variable");
    codebook.variables.forEach(variable => {
      variableSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
    });

    const countrySelect = document.getElementById("summary-countries");
    summaryState.countries.forEach(country => {
      const option = new Option(`${country.name} (${country.code})`, country.code);
      if (country.code === "NZL") option.selected = true;
      countrySelect.add(option);
    });

    loading.hidden = true;
    document.getElementById("summary-form").hidden = false;
  } catch (error) {
    console.error(error);
    loading.textContent = `Summary data could not be loaded: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("summary-form").addEventListener("submit", calculateSummary);
  document.getElementById("summary-reset").addEventListener("click", resetSummary);
  document.getElementById("summary-download").addEventListener("click", downloadSummary);
  initialiseSummary();
});

