# WorldView Online - Step 12c
# Put each model on its own sub-tab and limit linear regression to 5 predictors.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
regression_js_path <- file.path(app_dir, "assets", "regression.js")
css_path <- file.path(app_dir, "assets", "styles.css")
models_tabs_js_path <- file.path(app_dir, "assets", "model-tabs.js")

required <- c(index_path, regression_js_path, css_path)
if (!all(file.exists(required))) {
  stop("Required files were not found. Run Steps 9, 12, and 12b first.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
regression_js <- paste(readLines(regression_js_path, warn = FALSE), collapse = "\n")

# -----------------------------------------------------------------------------
# 1. Add model sub-tabs and wrap each model in its own panel
# -----------------------------------------------------------------------------

if (!grepl('id="model-subtabs"', html, fixed = TRUE)) {
  correlation_start <- '      <div id="model-loading" class="notice" aria-live="polite">Loading data...</div>'

  correlation_replacement <- paste0(
    '      <div id="model-subtabs" class="model-subtabs" role="tablist" aria-label="Statistical models">\n',
    '        <button id="tab-correlation" class="model-tab active" type="button" role="tab" aria-selected="true" aria-controls="panel-correlation" data-model-tab="correlation">Correlation</button>\n',
    '        <button id="tab-linear-regression" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-linear-regression" data-model-tab="linear-regression">Linear regression</button>\n',
    '      </div>\n\n',
    '      <div id="panel-correlation" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-correlation">\n',
    '        <div id="model-loading" class="notice" aria-live="polite">Loading data...</div>'
  )

  if (!grepl(correlation_start, html, fixed = TRUE)) {
    stop("Could not find the correlation model start in index.html.")
  }
  html <- sub(correlation_start, correlation_replacement, html, fixed = TRUE)

  between_models <- '      </form>\n\n      <hr class="model-divider">\n\n      <div class="page-heading model-subheading">'
  between_replacement <- '      </form>\n      </div>\n\n      <div id="panel-linear-regression" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-linear-regression" hidden>\n      <div class="page-heading model-subheading">'

  if (!grepl(between_models, html, fixed = TRUE)) {
    stop("Could not find the boundary between correlation and linear regression.")
  }
  html <- sub(between_models, between_replacement, html, fixed = TRUE)

  regression_end <- '      </form>\n    </section>\n\n    <section id="page-download"'
  regression_end_replacement <- '      </form>\n      </div>\n    </section>\n\n    <section id="page-download"'

  if (!grepl(regression_end, html, fixed = TRUE)) {
    stop("Could not find the end of the linear regression panel.")
  }
  html <- sub(regression_end, regression_end_replacement, html, fixed = TRUE)
}

# Replace the linear regression description with outcome-focused guidance.
html <- sub(
  "Fit an ordinary least-squares model with one outcome and one or more predictors.",
  "Model a numeric or scale-like outcome using one to five predictor variables.",
  html,
  fixed = TRUE
)

# Update predictor help text and size.
html <- sub(
  "Select one or more predictors. Use Ctrl or Command to select several.",
  "Select between one and five predictors. Use Ctrl or Command to select several.",
  html,
  fixed = TRUE
)

# Link the tab script.
if (!grepl("assets/model-tabs.js", html, fixed = TRUE)) {
  html <- sub(
    "</head>",
    '  <script src="assets/model-tabs.js" defer></script>\n</head>',
    html,
    fixed = TRUE
  )
}

writeLines(html, index_path, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 2. Add tab behaviour
# -----------------------------------------------------------------------------

tabs_js <- '"use strict";

function activateModelTab(tabName) {
  const tabs = document.querySelectorAll("[data-model-tab]");
  const panels = {
    correlation: document.getElementById("panel-correlation"),
    "linear-regression": document.getElementById("panel-linear-regression")
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
  activateModelTab(requested === "linear-regression" ? "linear-regression" : "correlation");
}

document.addEventListener("DOMContentLoaded", initialiseModelTabs);
'

writeLines(tabs_js, models_tabs_js_path, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 3. Enforce a maximum of five predictors in the regression script
# -----------------------------------------------------------------------------

# Add a reusable selection-limit function before calculate().
if (!grepl("function enforcePredictorLimit", regression_js, fixed = TRUE)) {
  calculate_marker <- "function calculate(e){"
  limit_function <- paste0(
    'function enforcePredictorLimit(event){',
    'const select=document.getElementById("regression-predictor"),chosen=[...select.selectedOptions];',
    'if(chosen.length<=5)return true;',
    'if(event&&event.target&&event.target.options){',
    'const recentlySelected=[...event.target.options].filter(o=>o.selected);',
    'recentlySelected.slice(5).forEach(o=>o.selected=false);',
    '}',
    'document.getElementById("regression-message").textContent="Select no more than five predictor variables.";',
    'return false;',
    '}\n'
  )

  if (!grepl(calculate_marker, regression_js, fixed = TRUE)) {
    stop("Could not find calculate() in regression.js.")
  }
  regression_js <- sub(
    calculate_marker,
    paste0(limit_function, calculate_marker),
    regression_js,
    fixed = TRUE
  )
}

# Add the limit check inside calculate().
old_calculate_start <- 'function calculate(e){e.preventDefault();const oid=document.getElementById("regression-outcome").value,pids=selected("regression-predictor");if(!pids.length||pids.includes(oid))'
new_calculate_start <- 'function calculate(e){e.preventDefault();const oid=document.getElementById("regression-outcome").value,pids=selected("regression-predictor");if(pids.length>5){document.getElementById("regression-message").textContent="Select no more than five predictor variables.";document.getElementById("regression-results").innerHTML="";document.getElementById("regression-download").hidden=true;return;}if(!pids.length||pids.includes(oid))'

if (grepl(old_calculate_start, regression_js, fixed = TRUE)) {
  regression_js <- sub(old_calculate_start, new_calculate_start, regression_js, fixed = TRUE)
} else if (!grepl("if(pids.length>5)", regression_js, fixed = TRUE)) {
  stop("Could not add the five-predictor check to calculate().")
}

# Register the change handler in the DOMContentLoaded setup.
old_listener <- 'document.getElementById("regression-form").addEventListener("submit",calculate);'
new_listener <- paste0(
  old_listener,
  'document.getElementById("regression-predictor").addEventListener("change",enforcePredictorLimit);'
)

if (!grepl('addEventListener("change",enforcePredictorLimit)', regression_js, fixed = TRUE)) {
  if (!grepl(old_listener, regression_js, fixed = TRUE)) {
    stop("Could not find regression event listeners.")
  }
  regression_js <- sub(old_listener, new_listener, regression_js, fixed = TRUE)
}

writeLines(regression_js, regression_js_path, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 4. Styles
# -----------------------------------------------------------------------------

styles <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
model_tab_css <- '\n\n/* Step 12c: model sub-tabs */\n.model-subtabs { display: flex; gap: 8px; margin: 0 0 28px; padding: 6px; width: fit-content; max-width: 100%; overflow-x: auto; border: 1px solid var(--line); border-radius: 999px; background: white; }\n.model-tab { appearance: none; border: 0; border-radius: 999px; padding: 10px 17px; background: transparent; color: var(--muted); cursor: pointer; font-weight: 800; white-space: nowrap; }\n.model-tab:hover { color: var(--teal-dark); background: #f1f5f3; }\n.model-tab.active { color: white; background: var(--teal); }\n.model-tab-panel[hidden] { display: none !important; }\n#regression-predictor { min-height: 210px; }\n'

if (!grepl("Step 12c: model sub-tabs", styles, fixed = TRUE)) {
  cat(model_tab_css, file = css_path, append = TRUE)
}

# -----------------------------------------------------------------------------
# 5. Structural validation
# -----------------------------------------------------------------------------

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_regression_js <- paste(readLines(regression_js_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c(
    "model_subtabs_added",
    "correlation_panel_added",
    "linear_regression_panel_added",
    "only_one_panel_visible_initially",
    "tab_script_linked",
    "five_predictor_help_text_added",
    "five_predictor_calculation_guard_added",
    "five_predictor_change_guard_added",
    "old_generic_description_removed",
    "tab_styles_added"
  ),
  passed = c(
    grepl('id="model-subtabs"', updated_html, fixed = TRUE),
    grepl('id="panel-correlation"', updated_html, fixed = TRUE),
    grepl('id="panel-linear-regression"', updated_html, fixed = TRUE),
    grepl('id="panel-linear-regression" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-linear-regression" hidden', updated_html, fixed = TRUE),
    grepl("assets/model-tabs.js", updated_html, fixed = TRUE),
    grepl("Select between one and five predictors", updated_html, fixed = TRUE),
    grepl("if(pids.length>5)", updated_regression_js, fixed = TRUE),
    grepl('addEventListener("change",enforcePredictorLimit)', updated_regression_js, fixed = TRUE),
    !grepl("Fit an ordinary least-squares model with one outcome and one or more predictors.", updated_html, fixed = TRUE),
    grepl("Step 12c: model sub-tabs", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step12c_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 12c validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 12c completed successfully.\n\n")
cat("Changes:\n")
cat("  - Correlation and linear regression now use separate model sub-tabs.\n")
cat("  - Only one model is displayed at a time.\n")
cat("  - Linear regression accepts between one and five predictors.\n")
cat("  - The old generic OLS description has been replaced.\n\n")
cat("Restart the development app:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
