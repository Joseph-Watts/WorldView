# WorldView Online - Step 31d
# Remove obsolete CSV download references before preparing deployment.

source_dir <- "worldview_static_app"
deploy_dir <- "worldview_deployment"

if (!dir.exists(source_dir)) stop("worldview_static_app was not found.")

# First remove obsolete links from the tested source app so future deployment
# builds do not reintroduce them.
html_files <- c(
  file.path(source_dir, "index.html"),
  file.path(source_dir, "validation.html")
)
html_files <- html_files[file.exists(html_files)]

obsolete <- c(
  "data/worldview-wave7-teaching-data-v1.0.0.csv",
  "data/worldview-codebook-v1.0.0.csv"
)

for (path in html_files) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Remove complete anchor elements that point to either discontinued CSV.
  for (target in obsolete) {
    escaped <- gsub("([.])", "\\\\\\1", target)
    pattern <- paste0(
      "<a[^>]*href=\\\"", escaped,
      "(?:[?#][^\\\"]*)?\\\"[^>]*>[\\s\\S]*?</a>"
    )
    html <- gsub(pattern, "", html, perl = TRUE, ignore.case = TRUE)
  }

  # Remove an obsolete Download page and its navigation control if still present.
  html <- gsub(
    "<section[^>]*id=\\\"page-download\\\"[\\s\\S]*?</section>",
    "",
    html,
    perl = TRUE,
    ignore.case = TRUE
  )
  html <- gsub(
    "<button[^>]*data-page=\\\"download\\\"[^>]*>[\\s\\S]*?</button>",
    "",
    html,
    perl = TRUE,
    ignore.case = TRUE
  )

  # Final safety cleanup for any bare href attributes left in unusual markup.
  for (target in obsolete) {
    html <- gsub(
      paste0("href=\\\"", gsub("([.])", "\\\\\\1", target), "\\\""),
      "",
      html,
      perl = TRUE
    )
  }

  writeLines(html, path, useBytes = TRUE)
}

# Confirm neither reference remains in the source application.
source_text <- paste(
  unlist(lapply(html_files, readLines, warn = FALSE, encoding = "UTF-8")),
  collapse = "\n"
)
remaining <- obsolete[vapply(obsolete, function(x) grepl(x, source_text, fixed = TRUE), logical(1))]
if (length(remaining)) {
  stop("Obsolete references remain in the source HTML: ", paste(remaining, collapse = ", "))
}

# Recreate the deployment directory from the cleaned source.
if (dir.exists(deploy_dir)) unlink(deploy_dir, recursive = TRUE, force = TRUE)
dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)
items <- list.files(source_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
copy_ok <- file.copy(
  items, deploy_dir,
  recursive = TRUE, overwrite = TRUE,
  copy.mode = TRUE, copy.date = TRUE
)
if (!all(copy_ok)) stop("One or more application items could not be copied.")

# Remove development-only files, backups, source data, and discontinued CSVs.
all_paths <- list.files(
  deploy_dir, recursive = TRUE, full.names = TRUE,
  all.files = TRUE, no.. = TRUE
)
remove <- grepl("(^|/)step[0-9].*\\.(csv|txt)$", all_paths, ignore.case = TRUE) |
  grepl("(^|/)before_step", all_paths, ignore.case = TRUE) |
  grepl("worldview-wave7-teaching-data-v1\\.0\\.0\\.csv$", all_paths, ignore.case = TRUE) |
  grepl("worldview-codebook-v1\\.0\\.0\\.csv$", all_paths, ignore.case = TRUE) |
  grepl("\\.(rds|rdata|rda|sav|dta|xlsx|xls)$", all_paths, ignore.case = TRUE)
if (any(remove)) unlink(all_paths[remove], recursive = TRUE, force = TRUE)

# Parse double-quoted src and href attributes and validate only local file paths.
extract_refs <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  src <- unlist(regmatches(text, gregexpr("src=\\\"[^\\\"]+\\\"", text, perl = TRUE)))
  href <- unlist(regmatches(text, gregexpr("href=\\\"[^\\\"]+\\\"", text, perl = TRUE)))
  refs <- c(src, href)
  if (!length(refs)) return(character())
  refs <- sub("^[^=]+=\\\"", "", refs)
  refs <- sub("\\\"$", "", refs)
  unique(refs)
}

check_ref <- function(html_file, ref) {
  ignored <- grepl(
    "^(https?:|mailto:|tel:|data:|blob:|javascript:|#|//|/\\.auth)",
    ref, ignore.case = TRUE
  ) || ref %in% c("/", "./", "../")
  clean <- URLdecode(sub("[?#].*$", "", ref))
  clean <- sub("^/", "", clean)
  if (!nzchar(clean)) ignored <- TRUE
  target <- if (ignored) "" else normalizePath(
    file.path(dirname(file.path(deploy_dir, html_file)), clean),
    winslash = "/", mustWork = FALSE
  )
  data.frame(
    html = html_file,
    reference = ref,
    ignored = ignored,
    exists = if (ignored) TRUE else file.exists(target),
    stringsAsFactors = FALSE
  )
}

html_names <- c("index.html", "validation.html")
rows <- do.call(rbind, lapply(html_names, function(name) {
  refs <- extract_refs(file.path(deploy_dir, name))
  if (!length(refs)) return(NULL)
  do.call(rbind, lapply(refs, function(ref) check_ref(name, ref)))
}))
write.csv(rows, file.path(deploy_dir, "deployment-reference-check.csv"), row.names = FALSE)

missing <- rows[!rows$exists, , drop = FALSE]
if (nrow(missing)) {
  write.csv(missing, file.path(deploy_dir, "deployment-missing-references.csv"), row.names = FALSE)
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
    "obsolete_html_references_removed",
    "download_page_removed",
    "required_files_present",
    "all_local_references_exist",
    "teaching_csv_removed",
    "codebook_csv_removed"
  ),
  passed = c(
    !any(vapply(obsolete, function(x) any(grepl(x, rows$reference, fixed = TRUE)), logical(1))),
    !grepl("page-download", paste(readLines(file.path(deploy_dir, "index.html"), warn = FALSE), collapse = "\n"), fixed = TRUE),
    all(file.exists(file.path(deploy_dir, required))),
    all(rows$exists),
    !file.exists(file.path(deploy_dir, obsolete[1])),
    !file.exists(file.path(deploy_dir, obsolete[2]))
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(deploy_dir, "deployment-checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31d validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31d completed successfully.\n\n")
cat("The obsolete CSV links and Download page were removed from the source app.\n")
cat("A clean deployment folder was recreated at worldview_deployment.\n")
cat("Final local test:\n")
cat("  servr::httd(\"worldview_deployment\", browser = TRUE)\n")
