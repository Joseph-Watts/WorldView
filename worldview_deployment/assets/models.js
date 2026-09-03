"use strict";

(() => {
  const paths = {
    data: "data/worldview-browser-data-v1.0.0.json",
    codebook: "data/worldview-codebook-v1.0.0.json"
  };

  const state = {
    data: null,
    codebook: null,
    variables: [],
    countries: [],
    lastRows: []
  };

  const METHODS = {
    pearson: {
      label: "Pearson",
      statistic: "r",
      description: "linear association"
    },
    spearman: {
      label: "Spearman",
      statistic: "ρ",
      description: "rank association"
    },
    kendall: {
      label: "Kendall",
      statistic: "τ-b",
      description: "rank concordance"
    }
  };

  function escapeHtml(value) {
    return String(value ?? "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("'", "&#039;");
  }

  function formatNumber(value, digits = 3) {
    if (value === null || !Number.isFinite(value)) return "Not available";
    return new Intl.NumberFormat("en-NZ", {
      maximumFractionDigits: digits,
      minimumFractionDigits: digits
    }).format(value);
  }

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

  function chosenCountries() {
    return [...document.getElementById("model-countries").selectedOptions]
      .map(option => option.value);
  }

  function countryMode() {
    return document.querySelector("input[name=model-country-mode]:checked")?.value || "combined";
  }

  function chosenMethod() {
    const value = document.getElementById("correlation-method")?.value || "pearson";
    return METHODS[value] ? value : "pearson";
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

  function pearson(x, y) {
    const n = x.length;
    const meanX = x.reduce((a, b) => a + b, 0) / n;
    const meanY = y.reduce((a, b) => a + b, 0) / n;

    let numerator = 0;
    let sumX = 0;
    let sumY = 0;

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

  function ranks(values) {
    const sorted = values
      .map((value, index) => ({ value, index }))
      .sort((a, b) => a.value - b.value);

    const result = new Array(values.length);
    let i = 0;

    while (i < sorted.length) {
      let j = i;
      while (j + 1 < sorted.length && sorted[j + 1].value === sorted[i].value) j += 1;
      const averageRank = (i + j + 2) / 2;
      for (let k = i; k <= j; k += 1) result[sorted[k].index] = averageRank;
      i = j + 1;
    }
    return result;
  }

  function spearman(x, y) {
    return pearson(ranks(x), ranks(y));
  }

  function kendallTauB(x, y) {
    let concordant = 0;
    let discordant = 0;
    let tiesX = 0;
    let tiesY = 0;

    for (let i = 0; i < x.length - 1; i += 1) {
      for (let j = i + 1; j < x.length; j += 1) {
        const sx = Math.sign(x[j] - x[i]);
        const sy = Math.sign(y[j] - y[i]);

        if (sx === 0 && sy === 0) continue;
        if (sx === 0) {
          tiesX += 1;
        } else if (sy === 0) {
          tiesY += 1;
        } else if (sx === sy) {
          concordant += 1;
        } else {
          discordant += 1;
        }
      }
    }

    const denominator =
      Math.sqrt((concordant + discordant + tiesX) * (concordant + discordant + tiesY));
    return denominator === 0 ? null : (concordant - discordant) / denominator;
  }

  function calculateStatistic(method, x, y) {
    if (method === "spearman") return spearman(x, y);
    if (method === "kendall") return kendallTauB(x, y);
    return pearson(x, y);
  }

  function inferentialSummary(method, statistic, n) {
    if (statistic === null || !Number.isFinite(statistic)) {
      return { lower: null, upper: null, pApprox: null };
    }

    if (method === "pearson") {
      const bounded = Math.max(-0.999999999, Math.min(0.999999999, statistic));
      const z = 0.5 * Math.log((1 + bounded) / (1 - bounded));
      const se = 1 / Math.sqrt(n - 3);
      const lower = Math.tanh(z - 1.959963984540054 * se);
      const upper = Math.tanh(z + 1.959963984540054 * se);
      const t = Math.abs(statistic) * Math.sqrt((n - 2) / (1 - statistic * statistic));
      return {
        lower,
        upper,
        pApprox: 2 * (1 - normalCdf(t))
      };
    }

    if (method === "spearman") {
      // Large-sample t approximation; CI intentionally omitted.
      const bounded = Math.max(-0.999999999, Math.min(0.999999999, statistic));
      const t = Math.abs(bounded) * Math.sqrt((n - 2) / (1 - bounded * bounded));
      return {
        lower: null,
        upper: null,
        pApprox: 2 * (1 - normalCdf(t))
      };
    }

    // Simple large-sample normal approximation for Kendall tau-b.
    // Ties affect the exact variance, so this is deliberately labelled approximate.
    const variance = (2 * (2 * n + 5)) / (9 * n * (n - 1));
    const z = Math.abs(statistic) / Math.sqrt(variance);
    return {
      lower: null,
      upper: null,
      pApprox: 2 * (1 - normalCdf(z))
    };
  }

  function correlationResult(variable1, variable2, codes, title, method) {
    const selected = new Set(codes);
    const countries = state.data.columns.B_COUNTRY_ALPHA;
    const col1 = state.data.columns[variable1.id];
    const col2 = state.data.columns[variable2.id];

    if (!col1 || !col2) {
      return {
        title,
        method,
        filteredRows: 0,
        completePairs: 0,
        excluded: 0,
        status: "missing_column"
      };
    }

    const x = [];
    const y = [];
    let filteredRows = 0;

    for (let i = 0; i < state.data.length; i += 1) {
      if (selected.size && !selected.has(countries[i])) continue;
      filteredRows += 1;

      const a = col1[i];
      const b = col2[i];
      if (a === null || b === null || a === "" || b === "") continue;

      const ax = Number(a);
      const by = Number(b);
      if (Number.isFinite(ax) && Number.isFinite(by)) {
        x.push(ax);
        y.push(by);
      }
    }

    const n = x.length;
    const base = {
      title,
      method,
      filteredRows,
      completePairs: n,
      excluded: filteredRows - n
    };

    if (n < 10) return { ...base, status: "insufficient" };
    if (new Set(x).size < 2 || new Set(y).size < 2) {
      return { ...base, status: "no_variation" };
    }

    const statistic = calculateStatistic(method, x, y);
    if (statistic === null) return { ...base, status: "no_variation" };

    const inference = inferentialSummary(method, statistic, n);
    return { ...base, status: "ok", statistic, ...inference };
  }

  function directionText(result, variable1, variable2) {
    const method = result.method;
    const meta = METHODS[method];

    if (result.statistic > 0) {
      return method === "pearson"
        ? `Higher values of ${variable1.displayName} tend to occur with higher values of ${variable2.displayName} in this sample.`
        : `Higher-ranked values of ${variable1.displayName} tend to occur with higher-ranked values of ${variable2.displayName} in this sample.`;
    }

    if (result.statistic < 0) {
      return method === "pearson"
        ? `Higher values of ${variable1.displayName} tend to occur with lower values of ${variable2.displayName} in this sample.`
        : `Higher-ranked values of ${variable1.displayName} tend to occur with lower-ranked values of ${variable2.displayName} in this sample.`;
    }

    return `The selected numerical representations show little ${meta.description} in this sample.`;
  }

  function resultHtml(result, variable1, variable2) {
    const meta = METHODS[result.method];

    if (result.status === "missing_column") {
      return `<section class="correlation-card"><h3>${escapeHtml(result.title)}</h3>
        <p class="suppression-note">One or both selected variables are unavailable in the browser data.</p></section>`;
    }

    if (result.status === "insufficient") {
      return `<section class="correlation-card"><h3>${escapeHtml(result.title)}</h3>
        <p class="suppression-note">Fewer than 10 complete pairs are available.</p></section>`;
    }

    if (result.status === "no_variation") {
      return `<section class="correlation-card"><h3>${escapeHtml(result.title)}</h3>
        <p class="suppression-note">A correlation cannot be calculated because at least one variable has no variation.</p></section>`;
    }

    const pShown =
      result.pApprox === null
        ? "Not available"
        : result.pApprox < 0.001
          ? "< 0.001"
          : formatNumber(result.pApprox, 3);

    const ciBlock =
      result.method === "pearson"
        ? `<div><strong>${formatNumber(result.lower, 3)} to ${formatNumber(result.upper, 3)}</strong><span>Approximate 95% CI</span></div>`
        : "";

    return `<section class="correlation-card">
      <h3>${escapeHtml(result.title)}</h3>
      <div class="correlation-figure">
        <div><strong>${formatNumber(result.statistic, 3)}</strong><span>${meta.label} ${meta.statistic}</span></div>
        <div><strong>${formatNumber(result.completePairs, 0)}</strong><span>Complete pairs</span></div>
        ${ciBlock}
        <div><strong>${escapeHtml(pShown)}</strong><span>Approximate p-value</span></div>
      </div>
      <p>${escapeHtml(directionText(result, variable1, variable2))}</p>
      <p class="model-note">${escapeHtml(variable1.id)}: ${escapeHtml(variable1.correlationRepresentation)}. ${escapeHtml(variable2.id)}: ${escapeHtml(variable2.correlationRepresentation)}.</p>
      <p class="model-note">Rows missing either variable were excluded pairwise (${formatNumber(result.excluded, 0)} rows). Correlation does not establish causation.${
        result.method === "spearman"
          ? " Spearman significance uses a large-sample approximation."
          : result.method === "kendall"
            ? " Kendall significance uses an approximate large-sample normal test."
            : ""
      }</p>
    </section>`;
  }

  function calculateModel(event) {
    event.preventDefault();

    const id1 = document.getElementById("model-variable-1").value;
    const id2 = document.getElementById("model-variable-2").value;
    const method = chosenMethod();

    const variable1 = state.variables.find(v => v.id === id1);
    const variable2 = state.variables.find(v => v.id === id2);

    if (!variable1 || !variable2) {
      document.getElementById("model-message").textContent =
        "The selected variables could not be found in the codebook.";
      return;
    }

    if (id1 === id2) {
      document.getElementById("model-message").textContent =
        "Choose two different variables.";
      document.getElementById("model-results").innerHTML = "";
      return;
    }

    const selected = chosenCountries();
    const map = new Map(state.countries.map(c => [c.code, c.name]));

    const specs =
      countryMode() === "separate" && selected.length > 1
        ? selected.map(code => ({ title: map.get(code) || code, codes: [code] }))
        : [{
            title: selected.length
              ? selected.map(code => map.get(code) || code).join(", ")
              : "All countries combined",
            codes: selected
          }];

    const results = specs.map(spec =>
      correlationResult(variable1, variable2, spec.codes, spec.title, method)
    );

    const meta = METHODS[method];
    document.getElementById("model-title").textContent =
      `${meta.label} correlation: ${variable1.id} and ${variable2.id}`;
    document.getElementById("model-message").textContent =
      `${meta.label} correlation using pairwise complete observations and WorldView numerical representations.`;

    document.getElementById("model-results").innerHTML =
      results.map(result => resultHtml(result, variable1, variable2)).join("");
    document.getElementById("model-download").hidden = false;

    state.lastRows = results.map(result => ({
      country_selection: result.title,
      method,
      variable_1: id1,
      variable_2: id2,
      status: result.status,
      filtered_rows: result.filteredRows,
      complete_pairs: result.completePairs,
      excluded_missing_either: result.excluded,
      correlation: result.statistic ?? "",
      ci_lower: result.lower ?? "",
      ci_upper: result.upper ?? "",
      approximate_p_value: result.pApprox ?? ""
    }));
  }

  function csvCell(value) {
    const text = value === null || value === undefined ? "" : String(value);
    return `"${text.replaceAll("\"", "\"\"")}"`;
  }

  function downloadModel() {
    if (!state.lastRows.length) return;
    const columns = [
      "country_selection",
      "method",
      "variable_1",
      "variable_2",
      "status",
      "filtered_rows",
      "complete_pairs",
      "excluded_missing_either",
      "correlation",
      "ci_lower",
      "ci_upper",
      "approximate_p_value"
    ];

    const lines = [
      columns.join(","),
      ...state.lastRows.map(row => columns.map(column => csvCell(row[column])).join(","))
    ];

    const blob = new Blob([lines.join("\n")], { type: "text/csv;charset=utf-8" });
    const link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download =
      `worldview-${chosenMethod()}-correlation-${document.getElementById("model-variable-1").value}-${document.getElementById("model-variable-2").value}.csv`;
    document.body.appendChild(link);
    link.click();
    URL.revokeObjectURL(link.href);
    link.remove();
  }

  function resetModel() {
    document.getElementById("model-form").reset();
    document.getElementById("correlation-method").value = "pearson";

    [...document.getElementById("model-countries").options].forEach(option => {
      option.selected = option.value === "NZL";
    });

    document.getElementById("model-title").textContent = "Select data to begin";
    document.getElementById("model-message").textContent =
      "Choose two different variables and a country selection.";
    document.getElementById("model-results").innerHTML = "";
    document.getElementById("model-download").hidden = true;
    state.lastRows = [];
  }

  async function initialiseModels() {
    const loading = document.getElementById("model-loading");

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
      state.codebook = codebook;
      state.variables = codebook.variables.filter(variable => variable.correlationEligible);

      const pairs = new Map();
      for (let i = 0; i < state.data.length; i += 1) {
        pairs.set(
          state.data.columns.B_COUNTRY_ALPHA[i],
          state.data.columns.B_COUNTRY[i]
        );
      }

      state.countries = [...pairs.entries()]
        .map(([code, name]) => ({ code, name }))
        .filter(country => country.code && country.name)
        .sort((a, b) => String(a.name).localeCompare(String(b.name)));

      const select1 = document.getElementById("model-variable-1");
      const select2 = document.getElementById("model-variable-2");

      state.variables.forEach(variable => {
        select1.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
        select2.add(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
      });

      select1.value = state.variables.some(v => v.id === "Q165") ? "Q165" : state.variables[0]?.id;
      select2.value = state.variables.some(v => v.id === "Q167") ? "Q167" : state.variables[1]?.id;

      const countrySelect = document.getElementById("model-countries");
      state.countries.forEach(country => {
        const option = new Option(`${country.name} (${country.code})`, country.code);
        if (country.code === "NZL") option.selected = true;
        countrySelect.add(option);
      });

      loading.hidden = true;
      document.getElementById("model-form").hidden = false;
    } catch (error) {
      console.error("Correlation initialisation failed", error);
      loading.hidden = false;
      loading.textContent = `Model data could not be loaded: ${error.message}`;
    }
  }

  document.addEventListener("DOMContentLoaded", () => {
    const form = document.getElementById("model-form");
    const reset = document.getElementById("model-reset");
    const download = document.getElementById("model-download");

    if (!form || !reset || !download) return;

    form.addEventListener("submit", calculateModel);
    reset.addEventListener("click", resetModel);
    download.addEventListener("click", downloadModel);
    initialiseModels();
  });
})();
