# WorldView Online - Step 30c
# Replace the validation-page favicon using simple line-based processing.

app_dir <- "worldview_static_app"
validation_path <- file.path(app_dir, "validation.html")
favicon_path <- file.path(app_dir, "assets", "worldview-favicon.svg")

if (!file.exists(validation_path)) stop("validation.html was not found.")
if (!file.exists(favicon_path)) stop("WorldView favicon was not found.")

lines <- readLines(validation_path, warn = FALSE, encoding = "UTF-8")

# Remove existing favicon/icon link lines without complex quote escaping.
is_icon_line <- grepl("<link", lines, ignore.case = TRUE) &
  (grepl("icon", lines, ignore.case = TRUE) |
   grepl("favicon", lines, ignore.case = TRUE) |
   grepl("Rlogo", lines, ignore.case = TRUE) |
   grepl("rstudio", lines, ignore.case = TRUE))
lines <- lines[!is_icon_line]

favicon_line <- '  <link rel="icon" type="image/svg+xml" href="assets/worldview-favicon.svg">'
head_close <- which(grepl("</head>", lines, fixed = TRUE))[1]
if (is.na(head_close)) stop("The closing head tag was not found in validation.html.")
lines <- append(lines, favicon_line, after = head_close - 1L)
writeLines(lines, validation_path, useBytes = TRUE)

updated <- readLines(validation_path, warn = FALSE, encoding = "UTF-8")
checks <- data.frame(
  check = c(
    "favicon_file_present",
    "worldview_favicon_link_added",
    "single_icon_link_present",
    "r_logo_reference_removed"
  ),
  passed = c(
    file.exists(favicon_path),
    any(grepl('href="assets/worldview-favicon.svg"', updated, fixed = TRUE)),
    sum(grepl("<link", updated, ignore.case = TRUE) & grepl("icon", updated, ignore.case = TRUE)) == 1L,
    !any(grepl("Rlogo|rstudio", updated, ignore.case = TRUE))
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step30c_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 30c validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 30c completed successfully.\n")
cat("validation.html now uses the WorldView favicon.\n")
cat("Close the old validation tab, restart the server, reopen validation.html, and force-refresh with Ctrl+F5.\n")
