# WorldView Online - Step 31g
# Integrate About into the original navigation block and original page router.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
app_js_path <- file.path(app_dir, "assets", "app.js")
about_js_path <- file.path(app_dir, "assets", "about-navigation.js")

if (!all(file.exists(c(index_path, app_js_path)))) {
  stop("Required index.html or assets/app.js was not found.")
}

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_js <- paste(readLines(app_js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Remove the temporary standalone About controls and workaround script link.
html <- gsub(
  '<button[^>]*data-page="about"[^>]*>[\\s\\S]*?</button>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)
html <- gsub(
  '<a[^>]*data-page="about"[^>]*>[\\s\\S]*?</a>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)
html <- gsub(
  '<script[^>]*src="assets/about-navigation.js"[^>]*></script>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)

# Find the navigation block containing the existing data-page controls.
nav_matches <- gregexpr('<nav[^>]*>[\\s\\S]*?</nav>', html, perl = TRUE)
nav_blocks <- regmatches(html, nav_matches)[[1]]
if (!length(nav_blocks)) stop("No navigation block was found in index.html.")
page_counts <- vapply(
  nav_blocks,
  function(block) lengths(regmatches(block, gregexpr('data-page=', block, fixed = TRUE))),
  integer(1)
)
main_nav <- nav_blocks[[which.max(page_counts)]]
if (max(page_counts) < 2L) stop("Could not identify the main page-navigation block.")

# Match the existing control element and classes so About is visually identical.
peer <- regmatches(
  main_nav,
  regexpr('<(?:button|a)[^>]*data-page="[^"]+"[^>]*>[\\s\\S]*?</(?:button|a)>', main_nav, perl = TRUE)
)
if (!length(peer) || !nzchar(peer)) stop("No existing navigation control could be identified.")

tag <- if (grepl("^<a", peer, ignore.case = TRUE)) "a" else "button"
class_match <- regmatches(peer, regexpr('class="[^"]*"', peer, perl = TRUE))
class_attr <- if (length(class_match) && nzchar(class_match)) paste0(" ", class_match) else ""

if (tag == "a") {
  about_control <- paste0('<a', class_attr, ' href="#about" data-page="about">About</a>')
} else {
  about_control <- paste0('<button', class_attr, ' type="button" data-page="about">About</button>')
}

new_nav <- sub('</nav>', paste0('  ', about_control, '\n</nav>'), main_nav, fixed = TRUE)
html <- sub(main_nav, new_nav, html, fixed = TRUE)

# Ensure the About page exists and is hidden initially like the other pages.
if (!grepl('id="page-about"', html, fixed = TRUE)) {
  stop("The About page section is missing from index.html.")
}

# Integrate About into explicit router allow-lists by replacing the obsolete
# quoted page name only. This leaves prose and filenames untouched.
app_js <- gsub('"download"', '"about"', app_js, fixed = TRUE)
app_js <- gsub("'download'", "'about'", app_js, fixed = TRUE)

# Remove duplicate about entries from common arrays after replacement.
app_js <- gsub('"about"\\s*,\\s*"about"', '"about"', app_js, perl = TRUE)
app_js <- gsub("'about'\\s*,\\s*'about'", "'about'", app_js, perl = TRUE)

writeLines(html, index_path, useBytes = TRUE)
writeLines(app_js, app_js_path, useBytes = TRUE)
if (file.exists(about_js_path)) file.remove(about_js_path)

# Rebuild the current deployment copy from the corrected source if it exists.
if (dir.exists("worldview_deployment")) {
  file.copy(index_path, file.path("worldview_deployment", "index.html"), overwrite = TRUE)
  file.copy(app_js_path, file.path("worldview_deployment", "assets", "app.js"), overwrite = TRUE)
  deployed_workaround <- file.path("worldview_deployment", "assets", "about-navigation.js")
  if (file.exists(deployed_workaround)) file.remove(deployed_workaround)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(app_js_path, warn = FALSE), collapse = "\n")
updated_nav <- regmatches(updated_html, gregexpr('<nav[^>]*>[\\s\\S]*?</nav>', updated_html, perl = TRUE))[[1]]
updated_main_nav <- updated_nav[[which.max(vapply(updated_nav, function(x) lengths(regmatches(x, gregexpr('data-page=', x, fixed = TRUE))), integer(1)))]]

checks <- data.frame(
  check = c(
    "exactly_one_about_control",
    "about_inside_main_navigation",
    "about_uses_peer_navigation_class",
    "about_page_present",
    "temporary_router_link_removed",
    "temporary_router_file_removed",
    "obsolete_download_router_name_removed"
  ),
  passed = c(
    lengths(regmatches(updated_html, gregexpr('data-page="about"', updated_html, fixed = TRUE))) == 1L,
    grepl('data-page="about"', updated_main_nav, fixed = TRUE),
    !nzchar(class_attr) || grepl(class_match, updated_main_nav, fixed = TRUE),
    grepl('id="page-about"', updated_html, fixed = TRUE),
    !grepl("assets/about-navigation.js", updated_html, fixed = TRUE),
    !file.exists(about_js_path),
    !grepl('"download"', updated_js, fixed = TRUE) && !grepl("'download'", updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step31g_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31g validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31g completed successfully.\n\n")
cat("About is now inside the same navigation block and uses the same class as the other tabs.\n")
cat("The original app router now recognizes About in place of Download.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
