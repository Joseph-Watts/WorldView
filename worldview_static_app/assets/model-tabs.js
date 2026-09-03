"use strict";

function activateModelTab(tabName) {
  const tabs = document.querySelectorAll("[data-model-tab]");
  const panels = {
    correlation: document.getElementById("panel-correlation"),
    "linear-regression": document.getElementById("panel-linear-regression"),
    "binary-logistic": document.getElementById("panel-binary-logistic"),
    anova: document.getElementById("panel-anova"),
    "chi-square": document.getElementById("panel-chi-square")
  };

  tabs.forEach(tab => {
    const active = tab.dataset.modelTab === tabName;
    tab.classList.toggle("active", active);
    tab.setAttribute("aria-selected", String(active));
    tab.setAttribute("tabindex", active ? "0" : "-1");
  });

  Object.entries(panels).forEach(([name, panel]) => {
    if (panel) panel.hidden = name !== tabName;
  });

  const url = new URL(window.location.href);
  url.searchParams.set("model", tabName);
  window.history.replaceState({}, "", url);
}

function initialiseModelTabs() {
  const tabs = [...document.querySelectorAll("[data-model-tab]")];
  if (!tabs.length) return;

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

  const requested = new URL(window.location.href).searchParams.get("model");
  activateModelTab(["linear-regression", "binary-logistic", "anova", "chi-square"].includes(requested) ? requested : "correlation");
}

document.addEventListener("DOMContentLoaded", initialiseModelTabs);

