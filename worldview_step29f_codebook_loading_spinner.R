# WorldView Online - Step 29f
# Add an immediate animated loading indicator while codebook topics are prepared.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, script_path, css_path)))) {
  stop("Run Step 29e first. Required codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Put the loader directly in the HTML so it is visible immediately, before the
# deferred codebook scripts execute.
loader <- paste0(
  '<div id="codebook-topic-loading" class="codebook-loading" ',
  'role="status" aria-live="polite">',
  '<span class="codebook-loading-spinner" aria-hidden="true"></span>',
  '<span>Preparing codebook topics...</span>',
  '</div>'
)

if (!grepl('id="codebook-topic-loading"', html, fixed = TRUE)) {
  page_pattern <- '(<section id="page-codebook"[^>]*>[\\s\\S]*?<div class="page-heading"[\\s\\S]*?</div>)'
  if (grepl(page_pattern, html, perl = TRUE)) {
    html <- sub(page_pattern, paste0('\\1\n      ', loader), html, perl = TRUE)
  } else {
    html <- sub(
      '(<section id="page-codebook"[^>]*>)',
      paste0('\\1\n      ', loader),
      html,
      perl = TRUE
    )
  }
}
writeLines(html, index_path, useBytes = TRUE)

# Remove the JavaScript-created notice because the loader is now present from
# first paint. Keep cbFinish(), which removes the loader after grouping.
old_loader_block <- paste0(
  '  const loading=document.createElement("div");\n',
  '  loading.id="codebook-topic-loading";\n',
  '  loading.className="notice subtle codebook-topic-loading";\n',
  '  loading.textContent="Preparing codebook topics...";\n',
  '  const heading=page.querySelector(".page-heading");\n',
  '  if(heading)heading.insertAdjacentElement("afterend",loading);\n'
)
js <- gsub(old_loader_block, "", js, fixed = TRUE)
writeLines(js, script_path, useBytes = TRUE)

if (!grepl("Step 29f: codebook loading spinner", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29f: codebook loading spinner */\n.codebook-loading { display: flex; align-items: center; gap: 12px; min-height: 64px; margin: 18px 0; padding: 14px 16px; border: 1px solid var(--line); border-radius: 12px; background: #f5f8f7; color: var(--teal-dark); font-weight: 700; }\n.codebook-loading-spinner { width: 24px; height: 24px; flex: 0 0 24px; border: 3px solid rgba(0,91,83,.18); border-top-color: #005b53; border-radius: 50%; animation: codebookSpinner .75s linear infinite; }\n@keyframes codebookSpinner { to { transform: rotate(360deg); } }\n@media (prefers-reduced-motion: reduce) { .codebook-loading-spinner { animation-duration: 1.8s; } }\n',
    file = css_path,
    append = TRUE
  )
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "loader_present_in_source_html",
    "spinner_present_in_source_html",
    "accessible_status_present",
    "spinner_animation_added",
    "javascript_duplicate_loader_removed",
    "loader_removed_after_topics_render",
    "reduced_motion_supported"
  ),
  passed = c(
    grepl('id="codebook-topic-loading"', updated_html, fixed = TRUE),
    grepl('class="codebook-loading-spinner"', updated_html, fixed = TRUE),
    grepl('role="status" aria-live="polite"', updated_html, fixed = TRUE),
    grepl("@keyframes codebookSpinner", updated_css, fixed = TRUE),
    !grepl('document.createElement("div");\\n  loading.id="codebook-topic-loading"', updated_js),
    grepl("if(loading)loading.remove()", updated_js, fixed = TRUE),
    grepl("prefers-reduced-motion", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29f_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29f validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 29f completed successfully.\n\n")
cat("An animated loading indicator now appears immediately while the codebook is prepared.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
