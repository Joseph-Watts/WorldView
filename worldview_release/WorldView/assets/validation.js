"use strict";

const paths = {
  manifest: "data/manifest-v1.0.0.json",
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json",
  fixtures: "data/worldview-reference-fixtures-v1.0.0.json"
};

const checks = [];

function addCheck(name, expected, actual, passed) {
  checks.push({ name, expected, actual, passed });
}

function closeEnough(actual, expected, tolerance) {
  if (actual === null || expected === null) return actual === expected;
  return Number.isFinite(actual) && Number.isFinite(expected) && Math.abs(actual - expected) <= tolerance;
}

function columnsFrom(object) {
  const names = Object.keys(object);
  const length = object[names[0]].length;
  if (!names.length || !names.every(name => object[name].length === length)) throw new Error("Invalid column-oriented data.");
  return { columns: object, length };
}

function indicesFor(data, country) {
  const output = [];
  for (let i = 0; i < data.length; i += 1) {
    if (country === "ALL" || data.columns.B_COUNTRY_ALPHA[i] === country) output.push(i);
  }
  return output;
}

function numericValues(data, variable, indices) {
  return indices.map(i => data.columns[variable][i]).filter(value => value !== null && value !== "").map(Number).filter(Number.isFinite);
}

function quantileType7(sorted, probability) {
  const position = (sorted.length - 1) * probability;
  const lower = Math.floor(position), upper = Math.ceil(position);
  if (lower === upper) return sorted[lower];
  return sorted[lower] + (position - lower) * (sorted[upper] - sorted[lower]);
}

function numericSummary(values, totalN) {
  const sorted = [...values].sort((a, b) => a - b);
  const n = sorted.length;
  const mean = sorted.reduce((a, b) => a + b, 0) / n;
  const variance = sorted.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (n - 1);
  return {
    totalN, validN: n, missingN: totalN - n, mean,
    standardDeviation: Math.sqrt(variance), median: quantileType7(sorted, 0.5),
    minimum: sorted[0], firstQuartile: quantileType7(sorted, 0.25),
    thirdQuartile: quantileType7(sorted, 0.75), maximum: sorted[n - 1]
  };
}

function pearson(x, y) {
  const n = x.length;
  const meanX = x.reduce((a, b) => a + b, 0) / n;
  const meanY = y.reduce((a, b) => a + b, 0) / n;
  let numerator = 0, sumX = 0, sumY = 0;
  for (let i = 0; i < n; i += 1) {
    const dx = x[i] - meanX, dy = y[i] - meanY;
    numerator += dx * dy; sumX += dx * dx; sumY += dy * dy;
  }
  return numerator / Math.sqrt(sumX * sumY);
}

function correlation(data, fixture) {
  const selected = fixture.countries === "ALL" ? [] : (Array.isArray(fixture.countries) ? fixture.countries : [fixture.countries]);
  const allowed = new Set(selected);
  const x = [], y = [];
  let filteredRows = 0;
  for (let i = 0; i < data.length; i += 1) {
    if (allowed.size && !allowed.has(data.columns.B_COUNTRY_ALPHA[i])) continue;
    filteredRows += 1;
    const a = data.columns[fixture.variable1][i], b = data.columns[fixture.variable2][i];
    if (a === null || b === null || a === "" || b === "") continue;
    x.push(Number(a)); y.push(Number(b));
  }
  const r = pearson(x, y);
  const z = 0.5 * Math.log((1 + r) / (1 - r));
  const se = 1 / Math.sqrt(x.length - 3);
  return {
    filteredRows, completePairs: x.length, excludedMissingEither: filteredRows - x.length,
    correlation: r,
    confidenceLower: Math.tanh(z - 1.959963984540054 * se),
    confidenceUpper: Math.tanh(z + 1.959963984540054 * se)
  };
}

function testSummaryFixtures(data, fixtures) {
  fixtures.summaryCases.forEach(test => {
    if (!test.id.includes("numeric")) return;
    const indices = indicesFor(data, test.country);
    const actual = numericSummary(numericValues(data, test.variable, indices), indices.length);
    ["totalN", "validN", "missingN"].forEach(field => addCheck(`${test.id}: ${field}`, test.result[field], actual[field], test.result[field] === actual[field]));
    ["mean", "standardDeviation", "median", "minimum", "firstQuartile", "thirdQuartile", "maximum"].forEach(field => addCheck(`${test.id}: ${field}`, test.result[field], actual[field], closeEnough(actual[field], test.result[field], 1e-10)));
  });
}

function testCorrelationFixtures(data, fixtures) {
  fixtures.correlationCases.forEach((test, index) => {
    const actual = correlation(data, test);
    const label = `Correlation ${index + 1}: ${test.variable1}/${test.variable2}`;
    addCheck(`${label}: filtered rows`, test.filteredRows, actual.filteredRows, test.filteredRows === actual.filteredRows);
    addCheck(`${label}: complete pairs`, test.completePairs, actual.completePairs, test.completePairs === actual.completePairs);
    addCheck(`${label}: excluded rows`, test.excludedMissingEither, actual.excludedMissingEither, test.excludedMissingEither === actual.excludedMissingEither);
    addCheck(`${label}: r`, test.correlation, actual.correlation, closeEnough(actual.correlation, test.correlation, 1e-10));
    addCheck(`${label}: CI lower`, test.confidenceLower, actual.confidenceLower, closeEnough(actual.confidenceLower, test.confidenceLower, 1e-8));
    addCheck(`${label}: CI upper`, test.confidenceUpper, actual.confidenceUpper, closeEnough(actual.confidenceUpper, test.confidenceUpper, 1e-8));
  });
}

function displayValue(value) {
  if (typeof value === "number") return Number.isInteger(value) ? String(value) : value.toPrecision(10);
  return String(value);
}

function render() {
  const body = document.getElementById("validation-results");
  body.innerHTML = checks.map(check => `<tr>
    <td>${check.name}</td><td>${displayValue(check.expected)}</td><td>${displayValue(check.actual)}</td>
    <td><span class="validation-badge ${check.passed ? "pass" : "fail"}">${check.passed ? "Pass" : "Fail"}</span></td>
  </tr>`).join("");
  const passed = checks.filter(check => check.passed).length;
  const failed = checks.length - passed;
  document.getElementById("validation-total").textContent = checks.length;
  document.getElementById("validation-passed").textContent = passed;
  document.getElementById("validation-failed").textContent = failed;
  const status = document.getElementById("validation-status");
  status.textContent = failed ? `${failed} validation checks failed.` : `All ${passed} validation checks passed.`;
  status.classList.toggle("validation-success", failed === 0);
  status.classList.toggle("validation-failure", failed > 0);
}

async function runValidation() {
  try {
    const [manifest, rawData, codebook, fixtures] = await Promise.all(
      Object.values(paths).map(path => fetch(path).then(response => {
        if (!response.ok) throw new Error(`${path} returned ${response.status}`);
        return response.json();
      }))
    );
    const data = columnsFrom(rawData);
    addCheck("Manifest participant count", manifest.participantCount, data.length, manifest.participantCount === data.length);
    addCheck("Manifest variable count", 29, manifest.studentVariableCount, manifest.studentVariableCount === 29);
    addCheck("Codebook variable count", 29, codebook.variables.length, codebook.variables.length === 29);
    addCheck("Only Q289 is correlation-ineligible", "Q289", codebook.variables.filter(v => !v.correlationEligible).map(v => v.id).join(","), codebook.variables.filter(v => !v.correlationEligible).map(v => v.id).join(",") === "Q289");
    testSummaryFixtures(data, fixtures);
    testCorrelationFixtures(data, fixtures);
    render();
  } catch (error) {
    console.error(error);
    document.getElementById("validation-status").textContent = `Validation could not run: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", runValidation);

