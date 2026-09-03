"use strict";

(() => {
  const paths = {
    data: "data/worldview-browser-data-v1.0.0.json",
    codebook: "data/worldview-codebook-v1.0.0.json"
  };

  const state = { data: null, variables: [], countries: [], lastRows: [] };

  const escapeHtml = value =>
    String(value ?? "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;");

  const formatNumber = (value, digits = 3) =>
    value === null || !Number.isFinite(value)
      ? "Not available"
      : new Intl.NumberFormat("en-NZ", {
          maximumFractionDigits: digits,
          minimumFractionDigits: digits
        }).format(value);

  function parseColumns(object) {
    const names = Object.keys(object);
    if (!names.length) throw new Error("Browser data contains no columns.");
    const first = object[names[0]];
    if (!Array.isArray(first)) throw new Error("Browser data is not in the expected column format.");
    const length = first.length;
    if (!names.every(name => Array.isArray(object[name]) && object[name].length === length)) {
      throw new Error("Browser data columns have inconsistent lengths.");
    }
    return { columns: object, length };
  }

  function selectedValues(id) {
    const element = document.getElementById(id);
    return element ? [...element.selectedOptions].map(option => option.value) : [];
  }

  function transpose(matrix) {
    return matrix[0].map((_, i) => matrix.map(row => row[i]));
  }

  function multiply(a, b) {
    const bt = transpose(b);
    return a.map(row => bt.map(column =>
      row.reduce((sum, value, i) => sum + value * column[i], 0)
    ));
  }

  function invert(matrix) {
    const n = matrix.length;
    const augmented = matrix.map((row, i) => [
      ...row,
      ...Array.from({ length: n }, (_, j) => i === j ? 1 : 0)
    ]);

    for (let i = 0; i < n; i += 1) {
      let pivot = i;
      for (let r = i + 1; r < n; r += 1) {
        if (Math.abs(augmented[r][i]) > Math.abs(augmented[pivot][i])) pivot = r;
      }
      if (Math.abs(augmented[pivot][i]) < 1e-12) return null;
      [augmented[i], augmented[pivot]] = [augmented[pivot], augmented[i]];

      const divisor = augmented[i][i];
      augmented[i] = augmented[i].map(value => value / divisor);

      for (let r = 0; r < n; r += 1) {
        if (r === i) continue;
        const factor = augmented[r][i];
        augmented[r] = augmented[r].map(
          (value, j) => value - factor * augmented[i][j]
        );
      }
    }
    return augmented.map(row => row.slice(n));
  }

  function normalCdf(x) {
    const sign = x < 0 ? -1 : 1;
    const z = Math.abs(x) / Math.sqrt(2);
    const t = 1 / (1 + 0.3275911 * z);
    const erf =
      1 -
      (((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) *
        t *
        Math.exp(-z * z));
    return 0.5 * (1 + sign * erf);
  }

  function fit(outcome, predictors, codes, title) {
    const allowed = new Set(codes);
    const countryCodes = state.data.columns.B_COUNTRY_ALPHA;
    const rows = [];
    let filtered = 0;

    for (let i = 0; i < state.data.length; i += 1) {
      if (allowed.size && !allowed.has(countryCodes[i])) continue;
      filtered += 1;

      const values = [
        state.data.columns[outcome]?.[i],
        ...predictors.map(predictor => state.data.columns[predictor]?.[i])
      ];

      if (values.some(value => value === null || value === "" || value === undefined)) continue;
      const numeric = values.map(Number);
      if (numeric.every(Number.isFinite)) rows.push(numeric);
    }

    const n = rows.length;
    const k = predictors.length + 1;
    const base = { title, filtered, n, excluded: filtered - n };

    if (n < Math.max(10, k + 2)) return { ...base, status: "insufficient" };

    const y = rows.map(row => [row[0]]);
    const X = rows.map(row => [1, ...row.slice(1)]);
    const xt = transpose(X);
    const inverse = invert(multiply(xt, X));
    if (!inverse) return { ...base, status: "singular" };

    const beta = multiply(multiply(inverse, xt), y).map(row => row[0]);
    const fitted = X.map(row => row.reduce((sum, value, j) => sum + value * beta[j], 0));
    const mean = y.reduce((sum, row) => sum + row[0], 0) / n;

    let sse = 0;
    let sst = 0;
    for (let i = 0; i < n; i += 1) {
      sse += (y[i][0] - fitted[i]) ** 2;
      sst += (y[i][0] - mean) ** 2;
    }

    const df = n - k;
    const s2 = sse / df;
    const se = inverse.map((row, i) => Math.sqrt(s2 * row[i]));
    const terms = ["Intercept", ...predictors];

    const coefficients = beta.map((estimate, i) => {
      const t = estimate / se[i];
      const p = 2 * (1 - normalCdf(Math.abs(t)));
      const critical = 1.959963984540054;
      return {
        term: terms[i],
        estimate,
        se: se[i],
        t,
        p,
        lower: estimate - critical * se[i],
        upper: estimate + critical * se[i]
      };
    });

    const r2 = sst === 0 ? null : 1 - sse / sst;
    const adjustedR2 = r2 === null ? null : 1 - (1 - r2) * (n - 1) / df;
    const f = r2 === null || predictors.length === 0 || r2 >= 1
      ? null
      : (r2 / predictors.length) / ((1 - r2) / df);

    return {
      ...base,
      status: "ok",
      df,
      coefficients,
      r2,
      adjustedR2,
      rse: Math.sqrt(s2),
      f
    };
  }

  function outcomeAdvice(variable) {
    if (variable.analysisType === "binary") {
      return "This binary outcome is being analysed as a linear probability model. Logistic regression is generally preferable; fitted values may fall outside 0 to 1.";
    }
    if (variable.id === "Q275") {
      return "Education is ordered. OLS treats adjacent education levels as equally spaced; ordinal logistic regression may be preferable.";
    }
    if (variable.analysisType === "ordinal") {
      return "This ordered outcome is treated as a numerical scale with equally spaced categories.";
    }
    return "This numerical outcome is suitable for OLS subject to linearity and residual assumptions.";
  }

  function resultCard(result, outcome, predictors) {
    if (result.status !== "ok") {
      return `<section class="regression-card">
        <h3>${escapeHtml(result.title)}</h3>
        <p class="suppression-note">${
          result.status === "singular"
            ? "The model matrix is singular. Remove duplicate or non-varying predictors."
            : "Too few complete observations are available."
        }</p>
      </section>`;
    }

    const rows = result.coefficients.map(coef => `
      <tr>
        <td>${escapeHtml(coef.term)}</td>
        <td>${formatNumber(coef.estimate)}</td>
        <td>${formatNumber(coef.se)}</td>
        <td>${formatNumber(coef.t)}</td>
        <td>${coef.p < 0.001 ? "&lt; 0.001" : formatNumber(coef.p)}</td>
        <td>${formatNumber(coef.lower)} to ${formatNumber(coef.upper)}</td>
      </tr>`).join("");

    return `<section class="regression-card">
      <h3>${escapeHtml(result.title)}</h3>
      <div class="model-fit-grid">
        <div><strong>${formatNumber(result.n, 0)}</strong><span>Complete observations</span></div>
        <div><strong>${formatNumber(result.r2)}</strong><span>R squared</span></div>
        <div><strong>${formatNumber(result.adjustedR2)}</strong><span>Adjusted R squared</span></div>
        <div><strong>${formatNumber(result.rse)}</strong><span>Residual standard error</span></div>
        <div><strong>${formatNumber(result.f)}</strong><span>F statistic</span></div>
      </div>
      <div class="table-wrap">
        <table class="results-table">
          <thead><tr><th>Term</th><th>Estimate</th><th>SE</th><th>t</th><th>Approx. p</th><th>Approx. 95% CI</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
      <p class="model-note">${escapeHtml(outcomeAdvice(outcome))}</p>
      <p class="model-note">Predictors: ${
        predictors.map(p => `${escapeHtml(p.id)}: ${escapeHtml(p.correlationRepresentation)}`).join("; ")
      }. Rows missing any model variable were excluded (${result.excluded}).</p>
    </section>`;
  }

  function enforcePredictorLimit() {
    const select = document.getElementById("regression-predictor");
    if (!select) return;
    const chosen = [...select.selectedOptions];
    if (chosen.length <= 5) return;
    chosen.slice(5).forEach(option => { option.selected = false; });
    document.getElementById("regression-message").textContent =
      "Select no more than five predictor variables.";
  }

  function calculate(event) {
    event.preventDefault();

    const outcomeId = document.getElementById("regression-outcome").value;
    const predictorIds = selectedValues("regression-predictor");

    if (predictorIds.length > 5) {
      document.getElementById("regression-message").textContent =
        "Select no more than five predictor variables.";
      return;
    }
    if (!predictorIds.length) {
      document.getElementById("regression-message").textContent =
        "Select at least one predictor.";
      return;
    }
    if (predictorIds.includes(outcomeId)) {
      document.getElementById("regression-message").textContent =
        "The outcome cannot also be a predictor.";
      return;
    }

    const outcome = state.variables.find(v => v.id === outcomeId);
    const predictors = predictorIds.map(id => state.variables.find(v => v.id === id));
    const countries = selectedValues("regression-countries");
    const countryMap = new Map(state.countries.map(c => [c.code, c.name]));
    const separate =
      document.querySelector("input[name=regression-country-mode]:checked")?.value === "separate";

    const specs =
      separate && countries.length > 1
        ? countries.map(code => ({ title: countryMap.get(code) || code, codes: [code] }))
        : [{
            title: countries.length
              ? countries.map(code => countryMap.get(code) || code).join(", ")
              : "All countries combined",
            codes: countries
          }];

    const results = specs.map(spec => fit(outcomeId, predictorIds, spec.codes, spec.title));

    document.getElementById("regression-title").textContent =
      `${outcomeId} predicted by ${predictorIds.join(", ")}`;
    document.getElementById("regression-message").textContent = outcomeAdvice(outcome);
    document.getElementById("regression-results").innerHTML =
      results.map(result => resultCard(result, outcome, predictors)).join("");
    document.getElementById("regression-download").hidden = false;

    state.lastRows = results.flatMap(result =>
      result.status === "ok"
        ? result.coefficients.map(coef => ({
            country: result.title,
            outcome: outcomeId,
            predictors: predictorIds.join(";"),
            term: coef.term,
            estimate: coef.estimate,
            se: coef.se,
            t: coef.t,
            p: coef.p,
            ci_lower: coef.lower,
            ci_upper: coef.upper,
            n: result.n,
            r_squared: result.r2,
            adjusted_r_squared: result.adjustedR2
          }))
        : []
    );
  }

  function downloadResults() {
    if (!state.lastRows.length) return;
    const columns = Object.keys(state.lastRows[0]);
    const quote = value => `"${String(value ?? "").replaceAll("\"", "\"\"")}"`;
    const text = [
      columns.join(","),
      ...state.lastRows.map(row => columns.map(column => quote(row[column])).join(","))
    ].join("\n");

    const link = document.createElement("a");
    link.href = URL.createObjectURL(new Blob([text], { type: "text/csv" }));
    link.download = "worldview-multiple-linear-regression.csv";
    link.click();
    URL.revokeObjectURL(link.href);
  }

  function resetForm() {
    const form = document.getElementById("regression-form");
    if (!form) return;
    form.reset();
    document.getElementById("regression-outcome").value = "Q177";
    [...document.getElementById("regression-predictor").options].forEach(option => {
      option.selected = ["Q165", "Q260", "Q262"].includes(option.value);
    });
    [...document.getElementById("regression-countries").options].forEach(option => {
      option.selected = option.value === "NZL";
    });
    document.getElementById("regression-results").innerHTML = "";
    document.getElementById("regression-download").hidden = true;
    document.getElementById("regression-title").textContent = "Specify a model to begin";
    document.getElementById("regression-message").textContent =
      "Choose an outcome, predictor, and country selection.";
  }

  async function initialise() {
    const loading = document.getElementById("regression-loading");
    const form = document.getElementById("regression-form");
    if (!loading || !form) return;

    try {
      const [dataObject, codebook] = await Promise.all([
        fetch(paths.data).then(response => {
          if (!response.ok) throw new Error(`Data request failed: ${response.status}`);
          return response.json();
        }),
        fetch(paths.codebook).then(response => {
          if (!response.ok) throw new Error(`Codebook request failed: ${response.status}`);
          return response.json();
        })
      ]);

      state.data = parseColumns(dataObject);
      state.variables = codebook.variables.filter(variable => variable.correlationEligible);

      if (!state.variables.length) throw new Error("No regression-eligible variables were found.");

      const pairs = new Map();
      for (let i = 0; i < state.data.length; i += 1) {
        pairs.set(
          state.data.columns.B_COUNTRY_ALPHA[i],
          state.data.columns.B_COUNTRY[i]
        );
      }

      state.countries = [...pairs]
        .map(([code, name]) => ({ code, name }))
        .filter(country => country.code && country.name)
        .sort((a, b) => String(a.name).localeCompare(String(b.name)));

      const outcomeSelect = document.getElementById("regression-outcome");
      const predictorSelect = document.getElementById("regression-predictor");

      state.variables.forEach(variable => {
        outcomeSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
        predictorSelect.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
      });

      outcomeSelect.value = state.variables.some(v => v.id === "Q177")
        ? "Q177"
        : state.variables[0].id;

      [...predictorSelect.options].forEach(option => {
        option.selected = ["Q165", "Q260", "Q262"].includes(option.value);
      });
      if (![...predictorSelect.selectedOptions].length && predictorSelect.options.length) {
        predictorSelect.options[0].selected = true;
      }

      const countrySelect = document.getElementById("regression-countries");
      state.countries.forEach(country => {
        const option = new Option(`${country.name} (${country.code})`, country.code);
        if (country.code === "NZL") option.selected = true;
        countrySelect.add(option);
      });

      loading.hidden = true;
      form.hidden = false;
    } catch (error) {
      console.error("Linear regression initialisation failed", error);
      loading.hidden = false;
      loading.textContent = `Regression data could not be loaded: ${error.message}`;
    }
  }

  document.addEventListener("DOMContentLoaded", () => {
    const form = document.getElementById("regression-form");
    const predictors = document.getElementById("regression-predictor");
    const reset = document.getElementById("regression-reset");
    const download = document.getElementById("regression-download");

    if (!form || !predictors || !reset || !download) return;

    form.addEventListener("submit", calculate);
    predictors.addEventListener("change", enforcePredictorLimit);
    reset.addEventListener("click", resetForm);
    download.addEventListener("click", downloadResults);
    initialise();
  });
})();
