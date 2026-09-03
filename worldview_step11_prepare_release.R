# WorldView Online - Step 11
# Prepare a clean, deployable static release and run final structural checks.

app_dir <- file.path(getwd(), "worldview_static_app")
release_root <- file.path(getwd(), "worldview_release")
release_dir <- file.path(release_root, "WorldView")
zip_path <- file.path(getwd(), "WorldView-static-v1.0.0.zip")

required_files <- c(
  "index.html",
  "validation.html",
  file.path("assets", "app.js"),
  file.path("assets", "summary.js"),
  file.path("assets", "visualisations.js"),
  file.path("assets", "models.js"),
  file.path("assets", "validation.js"),
  file.path("assets", "styles.css"),
  file.path("data", "manifest-v1.0.0.json"),
  file.path("data", "worldview-browser-data-v1.0.0.json"),
  file.path("data", "worldview-browser-data-v1.0.0.json.gz"),
  file.path("data", "worldview-codebook-v1.0.0.json"),
  file.path("data", "worldview-codebook-v1.0.0.csv"),
  file.path("data", "worldview-reference-fixtures-v1.0.0.json"),
  file.path("data", "worldview-wave7-teaching-data-v1.0.0.csv")
)

missing <- required_files[!file.exists(file.path(app_dir, required_files))]
if (length(missing) > 0L) {
  stop("Required release files are missing: ", paste(missing, collapse = ", "))
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

# Remove any earlier release copy so stale files cannot remain.
if (dir.exists(release_root)) {
  unlink(release_root, recursive = TRUE, force = TRUE)
}
dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(release_dir, "assets"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(release_dir, "data"), recursive = TRUE, showWarnings = FALSE)

# Copy only files needed for deployment. Development validation CSVs and prior
# build scripts are intentionally excluded.
copy_ok <- file.copy(
  from = file.path(app_dir, required_files),
  to = file.path(release_dir, required_files),
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = TRUE
)

if (!all(copy_ok)) {
  stop("At least one release file could not be copied.")
}

# Add deployment and data documentation.
readme <- c(
  "WorldView static application v1.0.0",
  "",
  "Deployment",
  "1. Upload the CONTENTS of this WorldView folder to the document root of the password-protected site.",
  "2. Keep the assets and data subfolders in their existing relative locations.",
  "3. Configure the host to serve index.html as the default document.",
  "4. Authentication must be supplied by the host. Do not add passwords to JavaScript files.",
  "5. After deployment, open validation.html and confirm that all browser checks pass.",
  "",
  "Student application",
  "Open index.html through the web host. Do not open it directly as a local file because browser security rules may block JSON loading.",
  "",
  "Data assets",
  "The student CSV contains an anonymous WorldView identifier, two country fields, and 29 approved variables.",
  "The browser JSON contains the numerical representations used for summaries, visualisations, and correlations.",
  "No country contributes more than 1,000 participants.",
  "",
  "Quality assurance",
  "validation.html compares browser summaries and correlations with R-generated reference fixtures.",
  "The validation page is not linked from student navigation."
)
writeLines(readme, file.path(release_dir, "README-DEPLOYMENT.txt"), useBytes = TRUE)

# Create a release manifest containing file sizes and checksums.
release_files <- list.files(release_dir, recursive = TRUE, full.names = TRUE)
release_files <- release_files[file.info(release_files)$isdir %in% FALSE]
relative_files <- substring(
  normalizePath(release_files, winslash = "/"),
  nchar(normalizePath(release_dir, winslash = "/")) + 2L
)

md5 <- unname(tools::md5sum(release_files))
file_info <- file.info(release_files)

release_inventory <- data.frame(
  file = relative_files,
  bytes = unname(file_info$size),
  md5 = md5,
  stringsAsFactors = FALSE
)
release_inventory <- release_inventory[order(release_inventory$file), , drop = FALSE]
row.names(release_inventory) <- NULL

write.csv(
  release_inventory,
  file.path(release_dir, "release-inventory-v1.0.0.csv"),
  row.names = FALSE,
  na = ""
)

manifest <- jsonlite::fromJSON(
  file.path(release_dir, "data", "manifest-v1.0.0.json"),
  simplifyVector = TRUE
)
codebook <- jsonlite::fromJSON(
  file.path(release_dir, "data", "worldview-codebook-v1.0.0.json"),
  simplifyVector = FALSE
)
browser_data <- jsonlite::fromJSON(
  file.path(release_dir, "data", "worldview-browser-data-v1.0.0.json"),
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

# Text checks for accidental secrets and development-only paths.
text_extensions <- c("html", "js", "css", "json", "txt", "csv")
text_files <- release_files[
  tolower(tools::file_ext(release_files)) %in% text_extensions
]

read_text_safely <- function(path) {
  size <- file.info(path)$size
  # Skip the large student CSV for regex scanning. Its columns and values are
  # checked separately below, and it should not contain application secrets.
  if (size > 15 * 1024^2 && grepl("teaching-data", basename(path), fixed = TRUE)) {
    return("")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

text_content <- vapply(text_files, read_text_safely, character(1))
secret_patterns <- c(
  "rsconnect::setAccountInfo",
  "setAccountInfo(",
  "SHINYAPPS_TOKEN",
  "SHINYAPPS_SECRET",
  "BEGIN PRIVATE KEY",
  "password =",
  "password="
)

secret_hits <- vapply(secret_patterns, function(pattern) {
  any(grepl(pattern, text_content, fixed = TRUE, ignore.case = TRUE))
}, logical(1))

development_patterns <- c(
  ".Rproj.user",
  ".Rhistory",
  "data-private",
  "worldview_processed_full_private.rds"
)

development_hits <- vapply(development_patterns, function(pattern) {
  any(grepl(pattern, text_content, fixed = TRUE, ignore.case = TRUE))
}, logical(1))

# Structural and data checks.
column_lengths <- vapply(browser_data, length, integer(1))
codebook_variables <- vapply(
  codebook$variables,
  function(variable) variable$id,
  character(1)
)

student_csv_header <- names(
  utils::read.csv(
    file.path(release_dir, "data", "worldview-wave7-teaching-data-v1.0.0.csv"),
    nrows = 1L,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)

validation <- data.frame(
  check = c(
    "all_required_release_files_present",
    "browser_columns_have_equal_lengths",
    "browser_row_count_matches_manifest",
    "manifest_reports_66_countries",
    "manifest_reports_29_student_variables",
    "codebook_contains_29_variables",
    "codebook_variable_ids_match_manifest",
    "student_csv_has_32_columns",
    "student_csv_has_worldview_id",
    "student_csv_has_country_fields",
    "no_known_secret_patterns_detected",
    "no_development_path_patterns_detected",
    "validation_page_included",
    "deployment_readme_created"
  ),
  passed = c(
    all(file.exists(file.path(release_dir, required_files))),
    length(unique(column_lengths)) == 1L,
    unique(column_lengths)[[1]] == manifest$participantCount,
    manifest$countryCount == 66L,
    manifest$studentVariableCount == 29L,
    length(codebook_variables) == 29L,
    identical(unname(codebook_variables), unname(manifest$variables)),
    length(student_csv_header) == 32L,
    "WORLDVIEW_ID" %in% student_csv_header,
    all(c("B_COUNTRY", "B_COUNTRY_ALPHA") %in% student_csv_header),
    !any(secret_hits),
    !any(development_hits),
    file.exists(file.path(release_dir, "validation.html")),
    file.exists(file.path(release_dir, "README-DEPLOYMENT.txt"))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(release_dir, "release-validation-v1.0.0.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Release validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

# Rebuild inventory so it also includes the inventory and validation reports.
release_files <- list.files(release_dir, recursive = TRUE, full.names = TRUE)
release_files <- release_files[file.info(release_files)$isdir %in% FALSE]
relative_files <- substring(
  normalizePath(release_files, winslash = "/"),
  nchar(normalizePath(release_dir, winslash = "/")) + 2L
)
file_info <- file.info(release_files)
release_inventory <- data.frame(
  file = relative_files,
  bytes = unname(file_info$size),
  md5 = unname(tools::md5sum(release_files)),
  stringsAsFactors = FALSE
)
release_inventory <- release_inventory[order(release_inventory$file), , drop = FALSE]
write.csv(
  release_inventory,
  file.path(release_dir, "release-inventory-v1.0.0.csv"),
  row.names = FALSE,
  na = ""
)

# Create a ZIP archive using R's available ZIP method.
if (file.exists(zip_path)) file.remove(zip_path)
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(release_root)

zip_result <- tryCatch({
  utils::zip(
    zipfile = zip_path,
    files = "WorldView",
    flags = "-r9Xq"
  )
  TRUE
}, error = function(e) {
  message("ZIP creation failed: ", conditionMessage(e))
  FALSE
})

setwd(old_wd)

if (!zip_result || !file.exists(zip_path)) {
  warning(
    "The validated release folder was created, but R could not create the ZIP. ",
    "Zip the WorldView folder manually if needed."
  )
}

summary_lines <- c(
  "WorldView static release v1.0.0",
  paste("Release folder:", release_dir),
  paste("Release validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Release files:", nrow(release_inventory)),
  paste("Release size MB:", round(sum(release_inventory$bytes) / 1024^2, 3)),
  paste("ZIP created:", file.exists(zip_path)),
  paste("ZIP path:", zip_path)
)
writeLines(summary_lines, file.path(release_dir, "release-summary-v1.0.0.txt"))

cat("\nStep 11 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Before deployment, run the release copy locally with:\n")
cat("  servr::httd(\"worldview_release/WorldView\", browser = TRUE)\n")
cat("Then open validation.html and confirm all browser checks pass.\n")
