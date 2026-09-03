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

function renderLevels(variable) {
  if (!Array.isArray(variable.levels) || variable.levels.length === 0) {
    if (variable.validRange) {
      return `<p>${escapeHtml(variable.validRange.minimum)} to ${escapeHtml(variable.validRange.maximum)}</p>`;
    }
    return "<p>Not specified</p>";
  }

  return `<ol class="level-list">${variable.levels.map(level =>
    `<li value="${escapeHtml(level.displayOrder)}">${escapeHtml(level.label)}</li>`
  ).join("")}</ol>`;
}

function variableCard(variable) {
  const eligible = variable.correlationEligible ? "Correlation eligible" : "Not correlation eligible";
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
          <div><dt>Correlation use</dt><dd>${eligible}</dd></div>
          <div><dt>Correlation coding</dt><dd>${escapeHtml(variable.correlationRepresentation)}</dd></div>
          <div><dt>Valid in full processed data</dt><dd>${formatNumber(variable.validCountInFullProcessedData)}</dd></div>
          <div><dt>Missing in full processed data</dt><dd>${formatNumber(variable.missingCountInFullProcessedData)}</dd></div>
        </dl>
        <h3>Processed values</h3>
        ${renderLevels(variable)}
        <h3>Missing-data treatment</h3>
        <p>${escapeHtml(variable.missingRule)}</p>
        <h3>Interpretation</h3>
        <p>${escapeHtml(variable.interpretationNote)}</p>
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

