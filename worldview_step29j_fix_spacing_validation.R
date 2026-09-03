# WorldView Online - Step 29j
# Complete the Step 29i update after its overly strict CSS validation check failed.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, script_path, css_path)))) {
  stop("Required Codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Ensure the requested introductory text is present.
html <- gsub(
  "Search the 29 student-facing variables and review how WorldView processes them.",
  "Search variables and review how WorldView processes them.",
  html,
  fixed = TRUE
)
html <- gsub(
  "Search the 39 student-facing variables and review how WorldView processes them.",
  "Search variables and review how WorldView processes them.",
  html,
  fixed = TRUE
)
writeLines(html, index_path, useBytes = TRUE)

# Ensure the results-area spinner spacing exists. Step 29i may already have
# applied this CSS before its final validation stopped.
if (!grepl("Step 29i: spinner in the Codebook results area", css, fixed = TRUE) &&
    !grepl("Step 29i: spinner results area", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29i: spinner in the Codebook results area */\n#page-codebook .codebook-loading-icon-only { display: flex !important; width: 100%; min-height: 72px; align-items: center; justify-content: center; margin: 14px 0 18px; padding: 12px 0; border: 0; background: transparent; clear: both; position: static; }\n',
    file = css_path,
    append = TRUE
  )
}

# Strengthen cleanup so any legacy duplicate loader is removed after rendering.
if (!grepl('document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove())', js, fixed = TRUE)) {
  js <- gsub(
    'const loading=document.getElementById("codebook-topic-loading");\\n  if\\(loading\\)loading.remove\\(\\);',
    'document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove());',
    js,
    perl = TRUE
  )
}
writeLines(js, script_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "introductory_text_updated",
    "results_anchor_preferred",
    "search_section_fallback_present",
    "spinner_spacing_present",
    "spinner_cleanup_present"
  ),
  passed = c(
    grepl("Search variables and review how WorldView processes them.", updated_html, fixed = TRUE),
    grepl('resultsAnchor.insertAdjacentElement("beforebegin",loader)', updated_js, fixed = TRUE),
    grepl('searchSection.insertAdjacentElement("afterend",loader)', updated_js, fixed = TRUE),
    grepl("#page-codebook .codebook-loading-icon-only", updated_css, fixed = TRUE),
    grepl('querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove())', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(checks, file.path(app_dir, "step29j_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 29j validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 29j completed successfully.\n")
cat("The Step 29i changes were retained; only the incorrect CSS validation test was repaired.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
