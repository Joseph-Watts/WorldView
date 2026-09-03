# WorldView Online - Step 29d
# Make codebook topic sections independently collapsible and closed by default.

app_dir <- "worldview_static_app"
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(script_path, css_path)))) {
  stop("Run Step 29c first. Required codebook files were not found.")
}

js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

old_section <- paste0(
  'const section=document.createElement("section");\n',
  '    section.className="codebook-topic-section";\n',
  '    section.dataset.topic=topic;\n',
  '    const heading=document.createElement("h2");\n',
  '    heading.className="codebook-topic-heading";\n',
  '    heading.textContent=topic;\n',
  '    const list=document.createElement("div");\n',
  '    list.className="codebook-topic-list";'
)

new_section <- paste0(
  'const section=document.createElement("details");\n',
  '    section.className="codebook-topic-section";\n',
  '    section.dataset.topic=topic;\n',
  '    section.open=false;\n',
  '    const heading=document.createElement("summary");\n',
  '    heading.className="codebook-topic-heading";\n',
  '    heading.textContent=`${topic} (${groups.get(topic).length})`;\n',
  '    heading.setAttribute("aria-label",`${topic}: ${groups.get(topic).length} variables`);\n',
  '    const list=document.createElement("div");\n',
  '    list.className="codebook-topic-list";'
)

if (grepl(old_section, js, fixed = TRUE)) {
  js <- sub(old_section, new_section, js, fixed = TRUE)
} else if (!grepl('section=document.createElement("details")', js, fixed = TRUE)) {
  stop("Could not locate the topic-section creation block in codebook-topics.js.")
}

# Each details element is independent, so opening one does not close another.
# This is the standard multi-open behaviour requested.
writeLines(js, script_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 29d: collapsible codebook topics", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29d: collapsible codebook topics */\n.codebook-topic-section { border: 0; }\n.codebook-topic-heading { cursor: pointer; list-style-position: outside; user-select: none; }\n.codebook-topic-heading:hover { background: #dcebe7; }\n.codebook-topic-heading:focus-visible { outline: 3px solid rgba(0,91,83,.28); outline-offset: 3px; }\n.codebook-topic-section[open] > .codebook-topic-heading { margin-bottom: 14px; }\n.codebook-topic-section:not([open]) > .codebook-topic-list { display: none; }\n.codebook-topic-list { padding-left: 12px; }\n',
    file = css_path,
    append = TRUE
  )
}

updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "topics_use_details_elements",
    "topics_closed_by_default",
    "topic_titles_use_summary_elements",
    "topic_counts_displayed",
    "multiple_topics_can_remain_open",
    "closed_topic_lists_hidden",
    "keyboard_focus_style_added"
  ),
  passed = c(
    grepl('section=document.createElement("details")', updated_js, fixed = TRUE),
    grepl("section.open=false", updated_js, fixed = TRUE),
    grepl('heading=document.createElement("summary")', updated_js, fixed = TRUE),
    grepl('${topic} (${groups.get(topic).length})', updated_js, fixed = TRUE),
    !grepl('name="codebook-topic"', updated_js, fixed = TRUE),
    grepl('.codebook-topic-section:not([open]) > .codebook-topic-list', updated_css, fixed = TRUE),
    grepl(".codebook-topic-heading:focus-visible", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29d_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29d validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 29d completed successfully.\n\n")
cat("Codebook topics now:\n")
cat("  - start closed;\n")
cat("  - open when their title is selected;\n")
cat("  - remain open independently, allowing multiple topics at once.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
