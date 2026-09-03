# WorldView Online - Step 31b
# Re-check deployment HTML references using browser-appropriate URL handling.

source_dir <- "worldview_static_app"
deploy_dir <- "worldview_deployment"

if (!dir.exists(source_dir)) stop("worldview_static_app was not found.")

# Recreate the deployment folder from the current tested app.
if (dir.exists(deploy_dir)) unlink(deploy_dir, recursive = TRUE, force = TRUE)
dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)
items <- list.files(source_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
ok <- file.copy(items, deploy_dir, recursive = TRUE, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE)
if (!all(ok)) stop("One or more application items could not be copied.")

# Remove development reports, backups, and files no longer offered for download.
all_paths <- list.files(deploy_dir, recursive = TRUE, full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
remove <- grepl("(^|/)step[0-9].*\\.(csv|txt)$", all_paths,
                ignore.case = TRUE) |
          grepl("(^|/)before_step", all_paths, ignore.case = TRUE) |
          grepl("worldview-wave7-teaching-data-v1\\.0\\.0\\.csv$",
                all_paths, ignore.case = TRUE) |
          grepl("worldview-codebook-v1\\.0\\.0\\.csv$",
                all_paths, ignore.case = TRUE)
if (any(remove)) unlink(all_paths[remove], recursive = TRUE, force = TRUE)

unsafe <- list.files(
  deploy_dir, recursive = TRUE, full.names = TRUE,
  pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$",
  ignore.case = TRUE
)
if (length(unsafe)) unlink(unsafe, force = TRUE)

extract_refs <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  matches <- unlist(regmatches(
    text,
    gregexpr('(?:src|href)=["\\'][^"\\']+["\\']', text, perl = TRUE)
  ))
  refs <- sub('^[^=]+=["\\']', "", matches, perl = TRUE)
  refs <- sub('["\\']$', "", refs, perl = TRUE)
  unique(refs)
}

check_ref <- function(html_file, ref) {
  original <- ref

  # References handled by the browser or the deployment host rather than files.
  ignored <- grepl(
    "^(https?:|mailto:|tel:|data:|blob:|javascript:|#|//|/\\.auth)",
    ref,
    ignore.case = TRUE
  ) || ref %in% c("/", "./", "../")

  # Strip query strings and fragments before checking the filesystem.
  clean <- sub("[?#].*$", "", ref)
  clean <- URLdecode(clean)
  clean <- sub("^/", "", clean)

  if (!nzchar(clean)) ignored <- TRUE

  target <- if (ignored) NA_character_ else {
    normalizePath(
      file.path(dirname(file.path(deploy_dir, html_file)), clean),
      winslash = "/",
      mustWork = FALSE
    )
  }

  exists <- if (ignored) TRUE else file.exists(target)

  data.frame(
    html = html_file,
    reference = original,
    normalized_reference = clean,
    ignored_as_route_or_external = ignored,
    target = if (ignored) "" else target,
    exists = exists,
    stringsAsFactors = FALSE
  )
}

html_files <- c("index.html", "validation.html")
rows <- do.call(rbind, lapply(html_files, function(file) {
  refs <- extract_refs(file.path(deploy_dir, file))
  if (!length(refs)) return(NULL)
  do.call(rbind, lapply(refs, function(ref) check_ref(file, ref)))
}))

write.csv(
  rows,
  file.path(deploy_dir, "deployment-reference-check.csv"),
  row.names = FALSE,
  na = ""
)

missing <- rows[!rows$exists, , drop = FALSE]
if (nrow(missing)) {
  write.csv(
    missing,
    file.path(deploy_dir, "deployment-missing-references.csv"),
    row.names = FALSE,
    na = ""
  )
  stop(
    "Deployment still has missing local references: ",
    paste(unique(missing$reference), collapse = ", "),
    ". See worldview_deployment/deployment-missing-references.csv"
  )
}

required <- c(
  "index.html", "validation.html",
  file.path("assets", "styles.css"),
  file.path("assets", "app.js"),
  file.path("assets", "validation.js"),
  file.path("assets", "worldview-favicon.svg"),
  file.path("data", "manifest-v1.0.0.json"),
  file.path("data", "worldview-browser-data-v1.0.0.json"),
  file.path("data", "worldview-codebook-v1.0.0.json")
)

checks <- data.frame(
  check = c(
    "required_files_present",
    "all_true_local_html_references_exist",
    "no_private_data_file_types",
    "downloadable_teaching_csv_removed",
    "downloadable_codebook_csv_removed",
    "validation_page_retained",
    "worldview_favicon_retained"
  ),
  passed = c(
    all(file.exists(file.path(deploy_dir, required))),
    all(rows$exists),
    !length(list.files(
      deploy_dir, recursive = TRUE,
      pattern = "\\.(rds|rdata|rda|sav|dta|xlsx|xls)$",
      ignore.case = TRUE
    )),
    !file.exists(file.path(
      deploy_dir, "data", "worldview-wave7-teaching-data-v1.0.0.csv"
    )),
    !file.exists(file.path(
      deploy_dir, "data", "worldview-codebook-v1.0.0.csv"
    )),
    file.exists(file.path(deploy_dir, "validation.html")),
    file.exists(file.path(
      deploy_dir, "assets", "worldview-favicon.svg"
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

files <- list.files(deploy_dir, recursive = TRUE, full.names = TRUE)
files <- files[!file.info(files)$isdir]
root <- normalizePath(deploy_dir, winslash = "/", mustWork = TRUE)
relative <- substring(normalizePath(files, winslash = "/"), nchar(root) + 2L)
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
  row.names = FALSE
)

cat("\nStep 31b completed successfully.\n\n")
cat("Deployment folder: ", normalizePath(deploy_dir, winslash = "/"), "\n", sep = "")
cat("Checks passed: ", sum(checks$passed), " of ", nrow(checks), "\n", sep = "")
cat("Local references checked: ", sum(!rows$ignored_as_route_or_external), "\n", sep = "")
cat("Final local test:\n")
cat("  servr::httd(\"worldview_deployment\", browser = TRUE)\n")
