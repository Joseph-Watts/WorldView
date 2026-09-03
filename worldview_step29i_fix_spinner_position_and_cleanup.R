# WorldView Online - Step 29i
# Place the Codebook spinner below the entire search section, remove it reliably
# after topic rendering, and simplify the Codebook introductory text.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, script_path, css_path)))) {
  stop("Run Step 29h first. Required Codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Update introductory wording.
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

# Replace insertion and cleanup functions with deterministic versions.
start <- regexpr("function cbInsertLoader\\(page\\)\\{", js, perl = TRUE)
finish <- regexpr("function cbFinish\\(page\\)\\{", js, perl = TRUE)
render <- regexpr("function cbRenderTopics\\(page,codebook\\)\\{", js, perl = TRUE)
if (start[1] < 0 || finish[1] < 0 || render[1] < 0) {
  stop("Could not locate the Step 29h loader functions.")
}

prefix <- substring(js, 1, start[1] - 1)
suffix <- substring(js, render[1])
replacement <- paste0(
  'function cbInsertLoader(page){\n',
  '  document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove());\n',
  '  const searchInput=page.querySelector("#codebook-search, input[type=search], input[placeholder*=Search], input[aria-label*=Search]");\n',
  '  let searchSection=null;\n',
  '  if(searchInput){\n',
  '    searchSection=searchInput.closest("form, .search-panel, .search-section, .filter-panel, .control-panel");\n',
  '    if(!searchSection)searchSection=searchInput.parentElement;\n',
  '  }\n',
  '  const resultsAnchor=page.querySelector("#codebook-results, .codebook-results, .codebook-list, [data-codebook-list]");\n',
  '  const loader=document.createElement("div");\n',
  '  loader.id="codebook-topic-loading";\n',
  '  loader.className="codebook-loading codebook-loading-icon-only";\n',
  '  loader.setAttribute("role","status");\n',
  '  loader.setAttribute("aria-label","Loading codebook variables");\n',
  '  loader.innerHTML=`<span class="codebook-loading-spinner" aria-hidden="true"></span><span class="visually-hidden">Loading codebook variables</span>`;\n',
  '  if(resultsAnchor)resultsAnchor.insertAdjacentElement("beforebegin",loader);\n',
  '  else if(searchSection)searchSection.insertAdjacentElement("afterend",loader);\n',
  '  else page.appendChild(loader);\n',
  '}\n',
  'function cbFinish(page){\n',
  '  page.classList.remove("codebook-topic-pending");\n',
  '  page.removeAttribute("data-codebook-pending");\n',
  '  document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove());\n',
  '}\n'
)
js <- paste0(prefix, replacement, suffix)

# Ensure cleanup runs only after topic cards have been moved into the host.
old_success <- paste0(
  '  page.dataset.topicNested="true";\n',
  '  cbFinish(page);\n',
  '  return true;'
)
new_success <- paste0(
  '  page.dataset.topicNested="true";\n',
  '  requestAnimationFrame(()=>cbFinish(page));\n',
  '  return true;'
)
js <- gsub(old_success, new_success, js, fixed = TRUE)
writeLines(js, script_path, useBytes = TRUE)

if (!grepl("Step 29i: spinner results area", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29i: spinner in the Codebook results area */\n#page-codebook .codebook-loading-icon-only { display: flex !important; width: 100%; min-height: 72px; align-items: center; justify-content: center; margin: 14px 0 18px; padding: 12px 0; border: 0; background: transparent; clear: both; }\n#page-codebook .search-panel + .codebook-loading-icon-only,\n#page-codebook .search-section + .codebook-loading-icon-only,\n#page-codebook form + .codebook-loading-icon-only { position: static; }\n',
    file = css_path,
    append = TRUE
  )
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "introductory_text_updated",
    "loader_not_inserted_inside_field",
    "results_area_preferred",
    "search_section_fallback_used",
    "all_loader_instances_removed_after_render",
    "cleanup_scheduled_after_topic_render",
    "results_area_spacing_added"
  ),
  passed = c(
    grepl("Search variables and review how WorldView processes them.", updated_html, fixed = TRUE),
    !grepl('search.closest(".field', updated_js, fixed = TRUE),
    grepl('resultsAnchor.insertAdjacentElement("beforebegin",loader)', updated_js, fixed = TRUE),
    grepl('searchSection.insertAdjacentElement("afterend",loader)', updated_js, fixed = TRUE),
    grepl('querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove())', updated_js, fixed = TRUE),
    grepl("requestAnimationFrame(()=>cbFinish(page))", updated_js, fixed = TRUE),
    grepl("Step 29i: spinner results area", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29i_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)
if (!all(checks$passed)) {
  stop("Step 29i validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 29i completed successfully.\n\n")
cat("The spinner now appears below the complete search section in the variable-results area.\n")
cat("It is removed after the topic sections have rendered.\n")
cat("The Codebook introductory sentence has been updated.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
