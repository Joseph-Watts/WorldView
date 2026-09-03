# WorldView Online - Step 31c
# Prepare the deployment folder using parse-safe reference checking.

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

missing_required <- required[
  !file.exists(file.path(source_dir, required))
]
if (length(missing_required)) {
  stop(
    "Required production files are missing: ",
    paste(missing_required, collapse = ", ")
  )
}

if (dir.exists(deploy_dir)) {
  unlink(deploy_dir, recursive = TRUE, force = TRUE)
}
dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)

items <- list.files(
  source_dir,
  all.files = TRUE,
  no.. = TRUE,
  full.names = TRUE
)
copy_ok <- file.copy(
  items,
  deploy_dir,
  recursive = TRUE,
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copy_ok)) stop("One or more application items could not be copied.")

# Remove development reports, backups, and discontinued downloadable CSV files.
all_paths <- list.files(
  deploy_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
remove <- grepl(
  "(^|/)step[0-9].*\\.(csv|txt)$",
  all_paths,
  ignore.case = TRUE
) |
  grepl("(^|/)before_step", all_paths, ignore.case = TRUE) |
  grepl(
    "worldview-wave7-teaching-data-v1\\.0\\.0\\.csv$",
    all_paths,
    ignore.case = TRUE
  ) |
  grepl(
    "worldview-codebook-v1\\.0\\.0\\.csv$",
    all_paths,
    ignore.case = TRUE
  )
if (any(remove)) {
  unlink(all_paths[remove], recursive = TRUE, force = TRUE)
}

unsafe_files <- list.files(
  deploy_dir,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$",
  ignore.case = TRUE
)
if (length(unsafe_files)) unlink(unsafe_files, force = TRUE)

# Extract src and href values that use double quotes. The app HTML uses this
# convention, avoiding the quote-escaping problem in Step 31b.
extract_double_quoted_refs <- function(path) {
  text <- paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  src_matches <- unlist(regmatches(
    text,
    gregexpr("src=\\\"[^\\\"]+\\\"", text, perl = TRUE)
  ))
  href_matches <- unlist(regmatches(
    text,
    gregexpr("href=\\\"[^\\\"]+\\\"", text, perl = TRUE)
  ))

  matches <- c(src_matches, href_matches)
  if (!length(matches)) return(character())

  refs <- sub("^[^=]+=\\\"", "", matches)
  refs <- sub("\\\"$", "", refs)
  unique(refs)
}

check_reference <- function(html_file, reference) {
  ignored <- grepl(
    "^(https?:|mailto:|tel:|data:|blob:|javascript:|#|//|/\\.auth)",
    reference,
    ignore.case = TRUE
  ) || reference %in% c("/", "./", "../")

  clean <- sub("[?#].*$", "", reference)
  clean <- URLdecode(clean)
  clean <- sub("^/", "", clean)
  if (!nzchar(clean)) ignored <- TRUE

  target <- ""
  exists <- TRUE
  if (!ignored) {
    target <- normalizePath(
      file.path(
        dirname(file.path(deploy_dir, html_file)),
        clean
      ),
      winslash = "/",
      mustWork = FALSE
    )
    exists <- file.exists(target)
  }

  data.frame(
    html = html_file,
    reference = reference,
    normalized_reference = clean,
    ignored_as_route_or_external = ignored,
    target = target,
    exists = exists,
    stringsAsFactors = FALSE
  )
}

html_files <- c("index.html", "validation.html")
reference_parts <- lapply(html_files, function(html_file) {
  refs <- extract_double_quoted_refs(
    file.path(deploy_dir, html_file)
  )
  if (!length(refs)) return(NULL)
  do.call(
    rbind,
    lapply(refs, function(ref) check_reference(html_file, ref))
  )
})
reference_parts <- Filter(Negate(is.null), reference_parts)
if (!length(reference_parts)) {
  stop("No src or href references were found in the deployment HTML.")
}
reference_rows <- do.call(rbind, reference_parts)

write.csv(
  reference_rows,
  file.path(deploy_dir, "deployment-reference-check.csv"),
  row.names = FALSE,
  na = ""
)

missing_refs <- reference_rows[
  !reference_rows$exists,
  ,
  drop = FALSE
]
if (nrow(missing_refs)) {
  write.csv(
    missing_refs,
    file.path(deploy_dir, "deployment-missing-references.csv"),
    row.names = FALSE,
    na = ""
  )
  stop(
    "Deployment has missing local references: ",
    paste(unique(missing_refs$reference), collapse = ", "),
    ". See worldview_deployment/deployment-missing-references.csv"
  )
}

remaining_unsafe <- list.files(
  deploy_dir,
  recursive = TRUE,
  pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$",
  ignore.case = TRUE
)

checks <- data.frame(
  check = c(
    "required_files_present",
    "local_html_references_exist",
    "no_private_data_file_types",
    "teaching_csv_removed",
    "codebook_csv_removed",
    "validation_page_retained",
    "worldview_favicon_retained"
  ),
  passed = c(
    all(file.exists(file.path(deploy_dir, required))),
    all(reference_rows$exists),
    length(remaining_unsafe) == 0L,
    !file.exists(file.path(
      deploy_dir,
      "data",
      "worldview-wave7-teaching-data-v1.0.0.csv"
    )),
    !file.exists(file.path(
      deploy_dir,
      "data",
      "worldview-codebook-v1.0.0.csv"
    )),
    file.exists(file.path(deploy_dir, "validation.html")),
    file.exists(file.path(
      deploy_dir,
      "assets",
      "worldview-favicon.svg"
    ))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(deploy_dir, "deployment-checks.csv"),
  row.names = FALSE,
  na = ""
)
if (!all(checks$passed)) {
  stop(
    "Deployment preparation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

files <- list.files(
  deploy_dir,
  recursive = TRUE,
  full.names = TRUE
)
files <- files[!file.info(files)$isdir]
root <- normalizePath(deploy_dir, winslash = "/", mustWork = TRUE)
relative <- substring(
  normalizePath(files, winslash = "/"),
  nchar(root) + 2L
)
inventory <- data.frame(
  file = relative,
  bytes = unname(file.info(files)$size),
  md5 = unname(tools::md5sum(files)),
  stringsAsFactors = FALSE
)
inventory <- inventory[order(inventory$file), ]
write.csv(
  inventory,
  file.path(deploy_dir, "deployment-inventory.csv"),
  row.names = FALSE,
  na = ""
)

cat("\nStep 31c completed successfully.\n\n")
cat(
  "Deployment folder: ",
  normalizePath(deploy_dir, winslash = "/"),
  "\n",
  sep = ""
)
cat("Checks passed: ", sum(checks$passed), " of ", nrow(checks), "\n", sep = "")
cat(
  "Local file references checked: ",
  sum(!reference_rows$ignored_as_route_or_external),
  "\n\n",
  sep = ""
)
cat("Final local test:\n")
cat("  servr::httd(\"worldview_deployment\", browser = TRUE)\n")
