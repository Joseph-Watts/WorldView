"use strict";

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
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#039;");
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

function visualCountryMode() {
  return document.querySelector("input[name=visual-country-mode]:checked").value;
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
  if (["Q165", "Q166", "Q167", "Q168", "Q260", "Q263"].includes(variable.id)) {
    return ["0", "1"];
  }

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
  const countryMode = visualCountryMode();
  const countryMap = new Map(visualState.countries.map(country => [country.code, country.name]));

  let specifications;

  if (countryMode === "separate" && countries.length > 1) {
    specifications = countries.map(code => ({
      title: countryMap.get(code) || code,
      codes: [code]
    }));
  } else {
    specifications = [{
      title: countries.length
        ? countries.map(code => countryMap.get(code) || code).join(", ")
        : "All countries combined",
      codes: countries
    }];
  }

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
  return `"${text.replaceAll("\"", "\"\"")}"`;
}

function downloadGraphData() {
  if (!visualState.lastRows.length) return;

  const rows = visualState.lastRows;
  const groupNames = [...new Set(rows.map(row => row.country_selection))];
  const responseNames = [...new Set(rows.map(row => row.response))];
  const measureElement = document.getElementById("visual-measure");
  const measure = measureElement ? measureElement.value : "percentage";
  const valueFor = row => measure === "count" ? Number(row.count) : Number(row.percentage_of_valid);
  const maximum = Math.max(1, ...rows.map(valueFor));

  const width = Math.max(900, groupNames.length * Math.max(180, responseNames.length * 54));
  const height = 620;
  const margin = { top: 80, right: 40, bottom: 170, left: 85 };
  const plotWidth = width - margin.left - margin.right;
  const plotHeight = height - margin.top - margin.bottom;
  const groupWidth = plotWidth / groupNames.length;
  const barGap = 5;
  const barWidth = Math.max(8, Math.min(42, (groupWidth - 24) / Math.max(1, responseNames.length) - barGap));
  const colours = ["#005b53", "#2f7f77", "#65a39c", "#9bc7c2", "#d2e6e3", "#6c5b7b", "#aa6f73", "#d09a74"];
  const escapeXml = value => String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll(""", "&quot;");
  const y = value => margin.top + plotHeight - (value / maximum) * plotHeight;

  const ticks = 5;
  let body = `<rect width="${width}" height="${height}" fill="white"/>`;
  body += `<text x="${width / 2}" y="34" text-anchor="middle" font-family="Arial" font-size="22" font-weight="700">${escapeXml(document.getElementById("visual-title").textContent)}</text>`;

  for (let i = 0; i <= ticks; i += 1) {
    const value = maximum * i / ticks;
    const yy = y(value);
    body += `<line x1="${margin.left}" y1="${yy}" x2="${width - margin.right}" y2="${yy}" stroke="#d7dfdd"/>`;
    body += `<text x="${margin.left - 12}" y="${yy + 5}" text-anchor="end" font-family="Arial" font-size="13">${measure === "count" ? Math.round(value) : value.toFixed(0) + "%"}</text>`;
  }

  groupNames.forEach((groupName, groupIndex) => {
    const groupRows = rows.filter(row => row.country_selection === groupName);
    const usedWidth = responseNames.length * (barWidth + barGap);
    const groupStart = margin.left + groupIndex * groupWidth + (groupWidth - usedWidth) / 2;
    responseNames.forEach((response, responseIndex) => {
      const row = groupRows.find(item => item.response === response);
      if (!row) return;
      const value = valueFor(row);
      const x = groupStart + responseIndex * (barWidth + barGap);
      const yy = y(value);
      const barHeight = margin.top + plotHeight - yy;
      body += `<rect x="${x}" y="${yy}" width="${barWidth}" height="${barHeight}" rx="2" fill="${colours[responseIndex % colours.length]}"/>`;
    });
    const labelX = margin.left + groupIndex * groupWidth + groupWidth / 2;
    body += `<text x="${labelX}" y="${margin.top + plotHeight + 28}" text-anchor="end" transform="rotate(-35 ${labelX} ${margin.top + plotHeight + 28})" font-family="Arial" font-size="13">${escapeXml(groupName)}</text>`;
  });

  const legendY = height - 42;
  responseNames.forEach((response, index) => {
    const itemWidth = Math.min(220, plotWidth / Math.max(1, responseNames.length));
    const x = margin.left + index * itemWidth;
    body += `<rect x="${x}" y="${legendY - 13}" width="14" height="14" fill="${colours[index % colours.length]}"/>`;
    body += `<text x="${x + 20}" y="${legendY}" font-family="Arial" font-size="12">${escapeXml(response)}</text>`;
  });

  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">${body}</svg>`;
  const blob = new Blob([svg], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const image = new Image();
  image.onload = () => {
    const scale = 2;
    const canvas = document.createElement("canvas");
    canvas.width = width * scale;
    canvas.height = height * scale;
    const context = canvas.getContext("2d");
    context.scale(scale, scale);
    context.drawImage(image, 0, 0, width, height);
    URL.revokeObjectURL(url);
    canvas.toBlob(pngBlob => {
      const link = document.createElement("a");
      link.href = URL.createObjectURL(pngBlob);
      link.download = `worldview-bar-chart-${document.getElementById("visual-variable").value}.png`;
      document.body.appendChild(link);
      link.click();
      setTimeout(() => { URL.revokeObjectURL(link.href); link.remove(); }, 0);
    }, "image/png");
  };
  image.onerror = () => {
    URL.revokeObjectURL(url);
    document.getElementById("visual-message").textContent = "The PNG image could not be created in this browser.";
  };
  image.src = url;
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

