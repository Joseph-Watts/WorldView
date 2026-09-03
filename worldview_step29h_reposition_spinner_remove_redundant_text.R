# WorldView Online - Step 29h
# Move the Codebook spinner beneath the Search variable bar and remove the
# redundant loading text. The spinner is removed when topic sections are ready.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, script_path, css_path)))) {
  stop("Run Step 29g first. Required Codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Remove the static loader from its current location. JavaScript will place the
# spinner immediately after the actual search control, which is more reliable
# across small HTML layout changes.
html <- gsub(
  '<div id="codebook-topic-loading"[\\s\\S]*?</div>',
  "",
  html,
  perl = TRUE
)
writeLines(html, index_path, useBytes = TRUE)

# Add a helper that inserts a spinner-only loader after the search field.
insert_marker <- 'function cbFinish(page){'
insert_helper <- paste0(
  'function cbInsertLoader(page){\n',
  '  const existing=document.getElementById("codebook-topic-loading");\n',
  '  if(existing)existing.remove();\n',
  '  const search=page.querySelector("#codebook-search, input[type=search], input[placeholder*=Search], input[aria-label*=Search]");\n',
  '  const anchor=search?(search.closest(".field, .search-field, .codebook-search")||search):page.querySelector(".page-heading");\n',
  '  const loader=document.createElement("div");\n',
  '  loader.id="codebook-topic-loading";\n',
  '  loader.className="codebook-loading codebook-loading-icon-only";\n',
  '  loader.setAttribute("role","status");\n',
  '  loader.setAttribute("aria-label","Preparing codebook topics");\n',
  '  loader.innerHTML=`<span class="codebook-loading-spinner" aria-hidden="true"></span><span class="visually-hidden">Preparing codebook topics</span>`;\n',
  '  if(anchor)anchor.insertAdjacentElement("afterend",loader);\n',
  '  else page.prepend(loader);\n',
  '}\n'
)
if (!grepl("function cbInsertLoader", js, fixed = TRUE)) {
  if (!grepl(insert_marker, js, fixed = TRUE)) stop("Could not locate cbFinish().")
  js <- sub(insert_marker, paste0(insert_helper, insert_marker), js, fixed = TRUE)
}

# Insert the loader immediately when the Codebook script initializes.
old_start <- paste0(
  '  const page=document.getElementById("page-codebook");\n',
  '  if(!page)return;\n'
)
new_start <- paste0(old_start, '  cbInsertLoader(page);\n')
if (!grepl("cbInsertLoader(page);", js, fixed = TRUE)) {
  if (!grepl(old_start, js, fixed = TRUE)) stop("Could not locate Codebook initialization.")
  js <- sub(old_start, new_start, js, fixed = TRUE)
}

# Remove any legacy visible loading-message creation that may remain.
js <- gsub(
  'loading.textContent="Preparing codebook topics...";',
  "",
  js,
  fixed = TRUE
)
writeLines(js, script_path, useBytes = TRUE)

if (!grepl("Step 29h: spinner beneath search", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29h: spinner beneath search */\n.codebook-loading-icon-only { justify-content: center; min-height: 52px; margin: 8px 0 16px; padding: 8px; border: 0; background: transparent; }\n.codebook-loading-icon-only .codebook-loading-spinner { width: 26px; height: 26px; }\n.visually-hidden { position: absolute !important; width: 1px !important; height: 1px !important; padding: 0 !important; margin: -1px !important; overflow: hidden !important; clip: rect(0,0,0,0) !important; white-space: nowrap !important; border: 0 !important; }\n',
    file = css_path,
    append = TRUE
  )
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "static_loader_removed",
    "dynamic_loader_helper_added",
    "search_control_used_as_anchor",
    "spinner_only_markup_added",
    "visible_loading_text_removed",
    "screen_reader_label_retained",
    "loader_still_removed_after_render",
    "spinner_centered_below_search"
  ),
  passed = c(
    !grepl('id="codebook-topic-loading"', updated_html, fixed = TRUE),
    grepl("function cbInsertLoader", updated_js, fixed = TRUE),
    grepl("#codebook-search, input[type=search]", updated_js, fixed = TRUE),
    grepl("codebook-loading-spinner", updated_js, fixed = TRUE),
    !grepl("Preparing codebook topics...", updated_js, fixed = TRUE),
    grepl('aria-label","Preparing codebook topics"', updated_js, fixed = TRUE),
    grepl("if(loading)loading.remove()", updated_js, fixed = TRUE),
    grepl(".codebook-loading-icon-only", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29h_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29h validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 29h completed successfully.\n\n")
cat("The spinner is now positioned directly beneath the Codebook search bar.\n")
cat("The visible Preparing codebook topics text has been removed.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
