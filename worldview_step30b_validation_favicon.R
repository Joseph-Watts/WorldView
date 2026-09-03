# WorldView Online - Step 30b
# Replace the R favicon on validation.html with the WorldView favicon.

app_dir <- "worldview_static_app"
validation_path <- file.path(app_dir, "validation.html")
favicon_path <- file.path(app_dir, "assets", "worldview-favicon.svg")

if (!file.exists(validation_path)) stop("validation.html was not found.")
if (!file.exists(favicon_path)) stop("WorldView favicon was not found. Run Step 28 first.")

html <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")

# Remove existing icon declarations, including the inherited R/Shiny favicon.
html <- gsub(
  '<link[^>]+rel=["\\'][^"\\']*(?:icon|shortcut icon)[^"\\']*["\\'][^>]*>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)
html <- gsub(
  '<link[^>]+href=["\\'][^"\\']*(?:favicon|Rlogo|rstudio)[^"\\']*["\\'][^>]*>',
  "",
  html,
  perl = TRUE,
  ignore.case = TRUE
)

favicon_link <- '  <link rel="icon" type="image/svg+xml" href="assets/worldview-favicon.svg">'
html <- sub("</head>", paste0(favicon_link, "\n</head>"), html, fixed = TRUE)
writeLines(html, validation_path, useBytes = TRUE)

updated <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "worldview_favicon_file_present",
    "validation_favicon_link_added",
    "only_one_worldview_favicon_link",
    "r_logo_reference_removed"
  ),
  passed = c(
    file.exists(favicon_path),
    grepl('href="assets/worldview-favicon.svg"', updated, fixed = TRUE),
    lengths(regmatches(updated, gregexpr('href="assets/worldview-favicon.svg"', updated, fixed = TRUE))) == 1L,
    !grepl("Rlogo", updated, ignore.case = TRUE) &&
      !grepl("rstudio", updated, ignore.case = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step30b_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 30b validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 30b completed successfully.\n")
cat("validation.html now uses the WorldView favicon.\n\n")
cat("Restart the server, close the validation tab, reopen it, and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
