"use strict";

const WORLDVIEW_MODEL_TABS = [
  "correlation",
  "linear-regression",
  "binary-logistic",
  "anova",
  "chi-square"
];

let activeModelTab = "correlation";

function activateModelTab(tabName) {
  const name = WORLDVIEW_MODEL_TABS.includes(tabName) ? tabName : "correlation";
  activeModelTab = name;

  const tabs = document.querySelectorAll("[data-model-tab]");
  const panels = {
    correlation: document.getElementById("panel-correlation"),
    "linear-regression": document.getElementById("panel-linear-regression"),
    "binary-logistic": document.getElementById("panel-binary-logistic"),
    anova: document.getElementById("panel-anova"),
    "chi-square": document.getElementById("panel-chi-square")
  };

  tabs.forEach(tab => {
    const active = tab.dataset.modelTab === name;
    tab.classList.toggle("active", active);
    tab.setAttribute("aria-selected", String(active));
    tab.setAttribute("tabindex", active ? "0" : "-1");
  });

  Object.entries(panels).forEach(([panelName, panel]) => {
    if (panel) panel.hidden = panelName !== name;
  });

  /*
   * Deliberately do not store model-tab state in the URL.
   * Main navigation therefore remains clean:
   *   #home, #summary, #visualisations, #models, etc.
   */
}

function removeLegacyModelParameter() {
  const url = new URL(window.location.href);
  if (!url.searchParams.has("model")) return;

  url.searchParams.delete("model");

  const query = url.searchParams.toString();
  const clean =
    url.pathname +
    (query ? `?${query}` : "") +
    (url.hash || "#models");

  window.history.replaceState({}, "", clean);
}

function initialiseModelTabs() {
  const tabs = [...document.querySelectorAll("[data-model-tab]")];
  if (!tabs.length) return;

  /*
   * Older WorldView builds wrote ?model=<tab> to the address bar.
   * Remove it automatically when an old/bookmarked URL is opened.
   */
  const legacyRequested = new URL(window.location.href).searchParams.get("model");
  removeLegacyModelParameter();

  tabs.forEach((tab, index) => {
    tab.addEventListener("click", () => activateModelTab(tab.dataset.modelTab));

    tab.addEventListener("keydown", event => {
      if (!["ArrowLeft", "ArrowRight", "Home", "End"].includes(event.key)) return;
      event.preventDefault();

      let nextIndex = index;
      if (event.key === "ArrowLeft") nextIndex = (index - 1 + tabs.length) % tabs.length;
      if (event.key === "ArrowRight") nextIndex = (index + 1) % tabs.length;
      if (event.key === "Home") nextIndex = 0;
      if (event.key === "End") nextIndex = tabs.length - 1;

      tabs[nextIndex].focus();
      activateModelTab(tabs[nextIndex].dataset.modelTab);
    });
  });

  /*
   * Preserve the selected sub-tab only for the current page load if someone
   * arrives via an old ?model= URL, then immediately clean the URL.
   * Otherwise default to Correlations.
   */
  activateModelTab(WORLDVIEW_MODEL_TABS.includes(legacyRequested) ? legacyRequested : activeModelTab);
}

document.addEventListener("DOMContentLoaded", initialiseModelTabs);
