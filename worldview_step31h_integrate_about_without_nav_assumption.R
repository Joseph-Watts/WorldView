# WorldView Online - Step 31h
# Integrate About beside the existing page tabs without assuming a <nav> wrapper.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
app_js_path <- file.path(app_dir, "assets", "app.js")
workaround_path <- file.path(app_dir, "assets", "about-navigation.js")

if (!all(file.exists(c(index_path, app_js_path)))) {
  stop("Required index.html or assets/app.js was not found.")
}

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_js <- paste(readLines(app_js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Remove every temporary About control and workaround script reference.
html <- gsub(
  '<button[^>]*data-page="about"[^>]*>[\\s\\S]*?</button>',
  "", html, perl = TRUE, ignore.case = TRUE
)
html <- gsub(
  '<a[^>]*data-page="about"[^>]*>[\\s\\S]*?</a>',
  "", html, perl = TRUE, ignore.case = TRUE
)
html <- gsub(
  '<script[^>]*src="assets/about-navigation.js"[^>]*></script>',
  "", html, perl = TRUE, ignore.case = TRUE
)

# Find existing page controls directly. This works whether they are in nav, div,
# header, or another tab container.
control_pattern <- '<(?:button|a)[^>]*data-page="[^"]+"[^>]*>[\\s\\S]*?</(?:button|a)>'
locations <- gregexpr(control_pattern, html, perl = TRUE)[[1]]
controls <- regmatches(html, gregexpr(control_pattern, html, perl = TRUE))[[1]]
if (!length(controls) || locations[[1]] < 0 || length(controls) < 2L) {
  stop("Could not identify at least two existing page-tab controls.")
}

# Prefer the old Download control's position if present. Otherwise use the last
# existing tab control, which keeps About inside the same visual tab sequence.
page_names <- sub('[\\s\\S]*data-page="([^"]+)"[\\s\\S]*', '\\1', controls, perl = TRUE)
insert_index <- if ("download" %in% page_names) match("download", page_names) else length(controls)
peer <- controls[[insert_index]]
insert_start <- locations[[insert_index]]
insert_length <- attr(locations, "match.length")[[insert_index]]

# Build About by cloning the peer tab markup and changing only route, label,
# href, active state, and current-page status.
about_control <- peer
about_control <- sub('data-page="[^"]+"', 'data-page="about"', about_control, perl = TRUE)
about_control <- sub('href="#[^"]+"', 'href="#about"', about_control, perl = TRUE)
about_control <- gsub('\\s+active(?=["\\s])', '', about_control, perl = TRUE)
about_control <- gsub('aria-current="page"', 'aria-current="false"', about_control, fixed = TRUE)
about_control <- sub('>([\\s\\S]*)<(/(?:button|a))>', '>About<\\2>', about_control, perl = TRUE)

# Replace Download in place when available. Otherwise insert About immediately
# after the final existing page control.
if ("download" %in% page_names) {
  html <- paste0(
    substring(html, 1, insert_start - 1L),
    about_control,
    substring(html, insert_start + insert_length)
  )
} else {
  html <- paste0(
    substring(html, 1, insert_start + insert_length - 1L),
    "\n      ", about_control,
    substring(html, insert_start + insert_length)
  )
}

if (!grepl('id="page-about"', html, fixed = TRUE)) {
  stop("The About page section is missing from index.html.")
}

# Update the original page allow-list/router from Download to About.
app_js <- gsub('"download"', '"about"', app_js, fixed = TRUE)
app_js <- gsub("'download'", "'about'", app_js, fixed = TRUE)
app_js <- gsub('"about"\\s*,\\s*"about"', '"about"', app_js, perl = TRUE)
app_js <- gsub("'about'\\s*,\\s*'about'", "'about'", app_js, perl = TRUE)

writeLines(html, index_path, useBytes = TRUE)
writeLines(app_js, app_js_path, useBytes = TRUE)
if (file.exists(workaround_path)) file.remove(workaround_path)

# Update the current deployment copy too.
if (dir.exists("worldview_deployment")) {
  file.copy(index_path, file.path("worldview_deployment", "index.html"), overwrite = TRUE)
  file.copy(app_js_path, file.path("worldview_deployment", "assets", "app.js"), overwrite = TRUE)
  deployed_workaround <- file.path("worldview_deployment", "assets", "about-navigation.js")
  if (file.exists(deployed_workaround)) file.remove(deployed_workaround)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(app_js_path, warn = FALSE), collapse = "\n")
updated_controls <- regmatches(
  updated_html,
  gregexpr(control_pattern, updated_html, perl = TRUE)
)[[1]]
updated_names <- sub('[\\s\\S]*data-page="([^"]+)"[\\s\\S]*', '\\1', updated_controls, perl = TRUE)
about_index <- match("about", updated_names)

checks <- data.frame(
  check = c(
    "exactly_one_about_tab",
    "download_tab_replaced",
    "about_is_part_of_existing_control_sequence",
    "about_clones_peer_element_type",
    "about_clones_peer_class",
    "about_page_present",
    "temporary_router_removed",
    "original_router_updated"
  ),
  passed = c(
    sum(updated_names == "about") == 1L,
    !any(updated_names == "download"),
    !is.na(about_index) && length(updated_controls) >= 2L,
    substr(updated_controls[[about_index]], 1, 2) == substr(peer, 1, 2),
    {
      peer_class <- regmatches(peer, regexpr('class="[^"]*"', peer, perl = TRUE))
      !length(peer_class) || !nzchar(peer_class) || grepl(peer_class, updated_controls[[about_index]], fixed = TRUE)
    },
    grepl('id="page-about"', updated_html, fixed = TRUE),
    !grepl("assets/about-navigation.js", updated_html, fixed = TRUE) && !file.exists(workaround_path),
    !grepl('"download"', updated_js, fixed = TRUE) && !grepl("'download'", updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step31h_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31h validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31h completed successfully.\n\n")
cat("About now occupies the old Download tab position, or the final position in the existing tab sequence.\n")
cat("It uses the same HTML element and CSS class as the other tabs.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
