# WorldView Online - Step 7b
# Refine summary-statistics presentation:
# - Integer and ordinal variables show numerical summaries first.
# - Their full response-frequency tables are collapsed by default.
# - Binary and nominal variables continue to show frequency tables directly.

app_dir <- file.path(getwd(), "worldview_static_app")
summary_js_path <- file.path(app_dir, "assets", "summary.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!file.exists(summary_js_path)) {
  stop("Summary script not found: ", summary_js_path)
}

if (!file.exists(css_path)) {
  stop("Stylesheet not found: ", css_path)
}

summary_js <- paste(readLines(summary_js_path, warn = FALSE), collapse = "\n")

old_block <- '  const numericHtml = numeric
    ? `<h4>Numerical summary</h4>${numericTable(numeric)}`
    : "";

  return {
    html: `<section class="result-block">
      <h3>${summaryEscape(title)}</h3>
      ${statCards(frequency)}
      <h4>Response distribution</h4>
      ${frequencyTable(frequency)}
      ${numericHtml}
    </section>`,
    csvRows
  };'

new_block <- '  const isScalar = variable.analysisType === "integer" || variable.analysisType === "ordinal";

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
  };'

if (!grepl(old_block, summary_js, fixed = TRUE)) {
  stop(
    "Could not find the expected Step 7 result-layout block. ",
    "Make sure worldview_step7_add_summary_statistics.R has been run once."
  )
}

summary_js <- sub(old_block, new_block, summary_js, fixed = TRUE)
writeLines(summary_js, summary_js_path, useBytes = TRUE)

css_text <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

refinement_css <- '

/* Step 7b: scalar summary presentation */
.full-table-disclosure {
  margin-top: 22px;
  border: 1px solid var(--line);
  border-radius: 12px;
  background: #fbfcfa;
  overflow: hidden;
}
.full-table-disclosure > summary {
  cursor: pointer;
  padding: 14px 16px;
  color: var(--teal-dark);
  font-weight: 800;
  list-style-position: inside;
}
.full-table-disclosure > summary:hover {
  background: #f1f5f3;
}
.full-table-disclosure[open] > summary {
  border-bottom: 1px solid var(--line);
}
.disclosure-content {
  padding: 4px 16px 16px;
}
.disclosure-content .field-help {
  margin: 12px 0 4px;
}
'

if (!grepl("Step 7b: scalar summary presentation", css_text, fixed = TRUE)) {
  cat(refinement_css, file = css_path, append = TRUE)
}

updated_js <- paste(readLines(summary_js_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c(
    "scalar_detection_added",
    "numeric_summary_presented_first",
    "full_table_disclosure_added",
    "frequency_variables_remain_direct",
    "refinement_styles_added"
  ),
  passed = c(
    grepl('const isScalar = variable.analysisType === "integer" || variable.analysisType === "ordinal";', updated_js, fixed = TRUE),
    grepl('<h4>Numerical summary</h4>${numericTable(numeric)}', updated_js, fixed = TRUE),
    grepl('Show full response table', updated_js, fixed = TRUE),
    grepl(': `<h4>Response distribution</h4>${frequencyTable(frequency)}`;', updated_js, fixed = TRUE),
    grepl("Step 7b: scalar summary presentation", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step7b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 7b validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 7b completed successfully.\n\n")
cat("Integer and ordinal variables now show summary statistics first.\n")
cat("Their full response tables are available in a collapsed disclosure.\n")
cat("Binary and nominal variables continue to show frequency tables directly.\n\n")
cat("Restart the local server and refresh the browser:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
