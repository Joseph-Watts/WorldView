# WorldView Online - Step 29g
# Ensure the Codebook loading spinner is inserted in a reliable visible location.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")

if (!all(file.exists(c(index_path, css_path, script_path)))) {
  stop("Run Step 29f first. Required Codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

# Remove any existing loader instance, wherever the prior regex placed it.
html <- gsub(
  '<div id="codebook-topic-loading"[\\s\\S]*?</div>',
  "",
  html,
  perl = TRUE
)

loader <- paste0(
  '<div id="codebook-topic-loading" class="codebook-loading" ',
  'role="status" aria-live="polite" aria-label="Preparing codebook topics">',
  '<span class="codebook-loading-spinner" aria-hidden="true"></span>',
  '<span class="codebook-loading-text">Preparing codebook topics...</span>',
  '</div>'
)

# Insert immediately after the Codebook section opening tag. This avoids relying
# on the nested page-heading structure and guarantees that it is inside the page.
pattern <- '(<section[^>]*id="page-codebook"[^>]*>)'
if (!grepl(pattern, html, perl = TRUE)) {
  stop("The page-codebook section could not be found in index.html.")
}
html <- sub(pattern, paste0('\\1\n      ', loader), html, perl = TRUE)
writeLines(html, index_path, useBytes = TRUE)

# Ensure the loader cannot be hidden by broad Codebook pending-state rules.
if (!grepl("Step 29g: force visible Codebook loader", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29g: force visible Codebook loader */\n#page-codebook #codebook-topic-loading.codebook-loading { display: flex !important; visibility: visible !important; opacity: 1 !important; position: relative; z-index: 20; }\n#page-codebook #codebook-topic-loading[hidden] { display: none !important; }\n#page-codebook .codebook-loading-spinner { display: inline-block !important; visibility: visible !important; opacity: 1 !important; box-sizing: border-box; width: 26px; height: 26px; border: 4px solid #b7d7d2; border-right-color: #005b53; border-bottom-color: #005b53; border-radius: 50%; animation: codebookVisibleSpinner .7s linear infinite; }\n#page-codebook .codebook-loading-text { display: inline-block; visibility: visible; }\n@keyframes codebookVisibleSpinner { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }\n',
    file = css_path,
    append = TRUE
  )
}

# Keep the loader visible until the topic host has actually been populated.
old_finish <- paste0(
  'function cbFinish(page){\n',
  '  page.classList.remove("codebook-topic-pending");\n',
  '  page.removeAttribute("data-codebook-pending");\n',
  '  const loading=document.getElementById("codebook-topic-loading");\n',
  '  if(loading)loading.remove();\n',
  '}'
)
new_finish <- paste0(
  'function cbFinish(page){\n',
  '  const host=document.getElementById("codebook-topic-groups");\n',
  '  if(host&&!host.querySelector(".codebook-topic-list > *"))return;\n',
  '  page.classList.remove("codebook-topic-pending");\n',
  '  page.removeAttribute("data-codebook-pending");\n',
  '  const loading=document.getElementById("codebook-topic-loading");\n',
  '  if(loading)loading.remove();\n',
  '}'
)
if (grepl(old_finish, js, fixed = TRUE)) {
  js <- sub(old_finish, new_finish, js, fixed = TRUE)
}
writeLines(js, script_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

section_start <- regexpr('<section[^>]*id="page-codebook"[^>]*>', updated_html, perl = TRUE)
loader_start <- regexpr('id="codebook-topic-loading"', updated_html, fixed = TRUE)

checks <- data.frame(
  check = c(
    "single_loader_present",
    "loader_inside_codebook_section",
    "spinner_element_present",
    "force_visible_css_added",
    "teal_spinner_border_added",
    "spinner_animation_added",
    "loader_removed_only_after_topic_content"
  ),
  passed = c(
    lengths(regmatches(updated_html, gregexpr('id="codebook-topic-loading"', updated_html, fixed = TRUE))) == 1L,
    section_start[1] > 0 && loader_start[1] > section_start[1],
    grepl('class="codebook-loading-spinner"', updated_html, fixed = TRUE),
    grepl("display: flex !important", updated_css, fixed = TRUE),
    grepl("border-right-color: #005b53", updated_css, fixed = TRUE),
    grepl("@keyframes codebookVisibleSpinner", updated_css, fixed = TRUE),
    grepl('host&&!host.querySelector(".codebook-topic-list > *")', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29g_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29g validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 29g completed successfully.\n\n")
cat("The spinner is now inserted directly inside the Codebook page and forced visible while topics are prepared.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
