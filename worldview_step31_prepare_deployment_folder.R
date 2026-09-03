# WorldView Online - Step 31
# Create a clean deployment folder from the tested static application.

source_dir <- "worldview_static_app"
deploy_dir <- "worldview_deployment"

if (!dir.exists(source_dir)) stop("worldview_static_app was not found.")

required <- c(
  "index.html",
  "validation.html",
  file.path("assets", "styles.css"),
  file.path("assets", "app.js"),
  file.path("assets", "validation.js"),
  file.path("assets", "worldview-favicon.svg"),
  file.path("data", "manifest-v1.0.0.json"),
  file.path("data", "worldview-browser-data-v1.0.0.json"),
  file.path("data", "worldview-codebook-v1.0.0.json")
)
missing <- required[!file.exists(file.path(source_dir, required))]
if (length(missing)) stop("Required production files are missing: ", paste(missing, collapse = ", "))

if (dir.exists(deploy_dir)) unlink(deploy_dir, recursive = TRUE, force = TRUE)
dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)

# Copy the complete static app first, preserving its working folder structure.
items <- list.files(source_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
ok <- file.copy(items, deploy_dir, recursive = TRUE, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
if (!all(ok)) stop("One or more application items could not be copied.")

# Remove development reports, backups, and obsolete downloadable data.
remove_patterns <- c(
  "^step[0-9].*\\.(csv|txt)$",
  "^before_step",
  "^worldview-wave7-teaching-data-v1\\.0\\.0\\.csv$",
  "^worldview-codebook-v1\\.0\\.0\\.csv$"
)
all_paths <- list.files(deploy_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
relative <- substring(all_paths, nchar(normalizePath(deploy_dir, winslash = "/", mustWork = TRUE)) + 2L)
remove <- rep(FALSE, length(all_paths))
for (pattern in remove_patterns) {
  remove <- remove | grepl(pattern, basename(all_paths), ignore.case = TRUE) |
    grepl(pattern, relative, ignore.case = TRUE)
}
if (any(remove)) unlink(all_paths[remove], recursive = TRUE, force = TRUE)

# Remove any private/source data formats if they were accidentally copied.
unsafe <- list.files(
  deploy_dir,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$",
  ignore.case = TRUE
)
if (length(unsafe)) unlink(unsafe, force = TRUE)

# Verify all local file references in the two HTML documents exist.
extract_refs <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  refs <- unlist(regmatches(text, gregexpr('(?:src|href)="[^"]+"', text, perl = TRUE)))
  refs <- sub('^[^=]+="', "", refs)
  refs <- sub('"$', "", refs)
  refs <- refs[!grepl("^(https?:|mailto:|#|/\\.auth)", refs)]
  refs
}
html_files <- c("index.html", "validation.html")
ref_rows <- do.call(rbind, lapply(html_files, function(file) {
  refs <- extract_refs(file.path(deploy_dir, file))
  data.frame(
    html = file,
    reference = refs,
    exists = file.exists(file.path(deploy_dir, refs)),
    stringsAsFactors = FALSE
  )
}))
write.csv(ref_rows, file.path(deploy_dir, "deployment-reference-check.csv"), row.names = FALSE)

# Inventory and safety scan.
files <- list.files(deploy_dir, recursive = TRUE, full.names = TRUE)
files <- files[!file.info(files)$isdir]
root <- normalizePath(deploy_dir, winslash = "/", mustWork = TRUE)
rel <- substring(normalizePath(files, winslash = "/"), nchar(root) + 2L)
inventory <- data.frame(
  file = rel,
  bytes = unname(file.info(files)$size),
  md5 = unname(tools::md5sum(files)),
  stringsAsFactors = FALSE
)
inventory <- inventory[order(inventory$file), ]
write.csv(inventory, file.path(deploy_dir, "deployment-inventory.csv"), row.names = FALSE)

text_files <- files[grepl("\\.(html|js|css|json|txt|csv)$", files, ignore.case = TRUE)]
content <- vapply(text_files, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"), character(1))
unsafe_terms <- c(
  "BEGIN PRIVATE KEY",
  "SHINYAPPS_TOKEN",
  "SHINYAPPS_SECRET",
  "worldview_processed_full_private",
  "WVS7_Individual.rds"
)
unsafe_hits <- vapply(unsafe_terms, function(term) any(grepl(term, content, fixed = TRUE)), logical(1))

checks <- data.frame(
  check = c(
    "required_files_present",
    "all_local_html_references_exist",
    "no_private_data_file_types",
    "no_known_secret_or_private_source_references",
    "downloadable_teaching_csv_removed",
    "downloadable_codebook_csv_removed",
    "validation_page_retained",
    "worldview_favicon_retained"
  ),
  passed = c(
    all(file.exists(file.path(deploy_dir, required))),
    nrow(ref_rows) > 0 && all(ref_rows$exists),
    !length(list.files(deploy_dir, recursive = TRUE, pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$", ignore.case = TRUE)),
    !any(unsafe_hits),
    !file.exists(file.path(deploy_dir, "data", "worldview-wave7-teaching-data-v1.0.0.csv")),
    !file.exists(file.path(deploy_dir, "data", "worldview-codebook-v1.0.0.csv")),
    file.exists(file.path(deploy_dir, "validation.html")),
    file.exists(file.path(deploy_dir, "assets", "worldview-favicon.svg"))
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(deploy_dir, "deployment-checks.csv"), row.names = FALSE)

if (!all(checks$passed)) {
  stop("Deployment preparation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31 completed successfully.\n\n")
cat("Deployment folder: ", normalizePath(deploy_dir, winslash = "/"), "\n", sep = "")
cat("Files: ", nrow(inventory), "\n", sep = "")
cat("Size MB: ", round(sum(inventory$bytes) / 1024^2, 2), "\n", sep = "")
cat("Checks passed: ", sum(checks$passed), " of ", nrow(checks), "\n\n", sep = "")
cat("Final local test:\n")
cat("  servr::httd(\"worldview_deployment\", browser = TRUE)\n")
