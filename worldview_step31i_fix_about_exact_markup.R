# WorldView Online - Step 31i
# Exact fix based on the inspected markup:
# navigation uses <a data-route>, while pages use <section data-page>.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
app_js_path <- file.path(app_dir, "assets", "app.js")
workaround_path <- file.path(app_dir, "assets", "about-navigation.js")

if (!all(file.exists(c(index_path, app_js_path)))) {
  stop("Required index.html or assets/app.js was not found.")
}

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_js <- paste(readLines(app_js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Replace the temporary button with the same markup used by all other primary tabs.
html <- gsub(
  '<button[^>]*data-page="about"[^>]*>[\\s\\S]*?</button>',
  '<a href="#about" data-route="about">About</a>',
  html,
  perl = TRUE,
  ignore.case = TRUE
)

# Remove any duplicate About navigation links, then ensure exactly one appears
# immediately after Models inside primary-navigation.
html <- gsub(
  '<a[^>]*data-route="about"[^>]*>[\\s\\S]*?</a>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)
models_link <- '<a href="#models" data-route="models">Models</a>'
if (!grepl(models_link, html, fixed = TRUE)) stop("The Models navigation link was not found.")
html <- sub(
  models_link,
  paste0(models_link, '\n        <a href="#about" data-route="about">About</a>'),
  html,
  fixed = TRUE
)

# The router hides/shows [data-page] sections. Add that attribute to About.
html <- sub(
  '<section id="page-about" class="page" hidden>',
  '<section id="page-about" class="page" data-page="about" hidden>',
  html,
  fixed = TRUE
)
if (!grepl('id="page-about" class="page" data-page="about"', html, fixed = TRUE)) {
  html <- sub(
    '<section id="page-about"',
    '<section id="page-about" data-page="about"',
    html,
    fixed = TRUE
  )
}

# Remove the obsolete workaround script declaration.
html <- gsub(
  '<script[^>]*src="assets/about-navigation.js"[^>]*></script>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)

# Add About to the exact route allow-list and remove Download.
old_routes <- '["home", "codebook", "summary", "visualisations", "models", "download"]'
new_routes <- '["home", "codebook", "summary", "visualisations", "models", "about"]'
if (grepl(old_routes, app_js, fixed = TRUE)) {
  app_js <- sub(old_routes, new_routes, app_js, fixed = TRUE)
} else {
  app_js <- gsub('"download"', '"about"', app_js, fixed = TRUE)
}

# Correct the encoding damage visible in the inspected About page.
html <- gsub("AndrĂ© De Vito", "André De Vito", html, fixed = TRUE)

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

updated_html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
updated_js <- paste(readLines(app_js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

primary_nav <- regmatches(
  updated_html,
  regexpr('<nav id="primary-navigation"[\\s\\S]*?</nav>', updated_html, perl = TRUE)
)

checks <- data.frame(
  check = c(
    "about_link_uses_data_route",
    "about_link_inside_primary_navigation",
    "about_link_is_not_button",
    "about_page_has_data_page",
    "about_route_is_allowed",
    "download_route_removed",
    "workaround_script_removed",
    "developer_name_encoding_fixed"
  ),
  passed = c(
    lengths(regmatches(updated_html, gregexpr('data-route="about"', updated_html, fixed = TRUE))) == 1L,
    nzchar(primary_nav) && grepl('href="#about" data-route="about"', primary_nav, fixed = TRUE),
    !grepl('<button[^>]*about', primary_nav, ignore.case = TRUE),
    grepl('data-page="about"', updated_html, fixed = TRUE),
    grepl('"models", "about"', updated_js, fixed = TRUE),
    !grepl('"download"', updated_js, fixed = TRUE),
    !grepl("assets/about-navigation.js", updated_html, fixed = TRUE) && !file.exists(workaround_path),
    grepl("André De Vito", updated_html, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step31i_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31i validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31i completed successfully.\n\n")
cat("About now uses the same <a data-route> markup as the other primary tabs.\n")
cat("The About section now has data-page=about, so the original router controls it.\n")
cat("The temporary workaround has been removed.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
