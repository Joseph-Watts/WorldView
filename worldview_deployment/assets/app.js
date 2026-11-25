"use strict";

const paths = {
  manifest: "data/manifest-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json"
};

const state = { manifest: null, codebook: null };

function currentRoute() {
  const route = window.location.hash.replace(/^#/, "").split("?")[0];
  const valid = ["home", "codebook", "summary", "visualisations", "models", "about"];
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
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#039;");
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

function responseLabel(variable, value) {
  if (value == null || value === "") return "Missing";
  if (variable.analysisType === "binary") {
    const matches = variable.levels.filter(level =>
      level.value != null && String(level.value) === String(value));
    return matches.map(level => level.label).join(" / ") || String(value);
  }
  if (variable.analysisType === "ordinal") {
    const match = variable.levels?.find(level => String(Object.hasOwn(level, "value") ? level.value : level.displayOrder) === String(value));
    if (match) return match.label;
  }
  return String(value);
}

function renderWorldViewValues(variable) {
  if (variable.analysisType === "integer") {
    const age = variable.id === "Q262";
    const description = age ? "WorldView uses the respondent’s age in years." : "WorldView stores a whole number for each valid response.";
    const range = variable.validRange;
    if (!range) return `<p>${description} There are no valid responses before sampling.</p>`;
    const subject = age ? "Ages" : "Values";
    return `<p>${description} ${subject} in the full dataset range from ${escapeHtml(range.minimum)} to ${escapeHtml(range.maximum)}.</p>`;
  }

  const nominal = variable.analysisType === "nominal";
  const headings = nominal
    ? ["Category stored in WorldView", "Original WVS code"]
    : ["Response", "Value used in WorldView", "Original WVS code"];
  const rows = variable.levels.map(level => `
    <tr><th scope="row">${escapeHtml(nominal ? level.value : level.label)}</th>
      ${nominal ? "" : `<td>${escapeHtml(level.value ?? "Missing")}</td>`}
      <td>${escapeHtml(level.wvsCode)}</td></tr>`);
  return `<div class="table-wrap"><table class="results-table" aria-label="Values used in WorldView for ${escapeHtml(variable.id)}">
    <thead><tr>${headings.map(heading => `<th scope="col">${heading}</th>`).join("")}</tr></thead>
    <tbody>${rows.join("")}</tbody>
  </table></div>`;
}

function analysisNote(variable) {
  const eligibility = variable.correlationEligible
    ? "Available for correlation." : "Excluded from correlation.";
  if (variable.analysisType === "binary") return `${eligibility} ${variable.interpretationNote}`;
  if (variable.analysisType === "ordinal") {
    return `${eligibility} The WorldView scores give the order of the responses. Calculating a mean treats adjacent scores as equally spaced.`;
  }
  if (variable.analysisType === "nominal") {
    return `${eligibility} Use counts or percentages to compare these unordered categories.`;
  }
  return eligibility;
}

function variableCard(variable) {
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
          <div><dt>Valid responses before sampling</dt><dd>${formatNumber(variable.validCountInFullProcessedData)}</dd></div>
          <div><dt>Missing responses before sampling</dt><dd>${formatNumber(variable.missingCountInFullProcessedData)}</dd></div>
        </dl>
        <h3>Values used in WorldView</h3>
        ${renderWorldViewValues(variable)}
        <h3>Missing-data treatment</h3>
        <p>${escapeHtml(variable.missingRule)}</p>
        <h3>Use in analysis</h3>
        <p>${escapeHtml(analysisNote(variable))}</p>
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

