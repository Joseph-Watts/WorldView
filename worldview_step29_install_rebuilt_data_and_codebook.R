# WorldView Online - Step 29
# Install the rebuilt Step 3 public assets into the static app and add complete
# codebook metadata for Q18-Q26 and Q238.

app_dir <- "worldview_static_app"
public_dir <- file.path("worldview_step3_output", "public_data")
processed_path <- file.path(
  "worldview_step2_output",
  "worldview_processed_full_private.rds"
)
codebook_path <- file.path(
  app_dir, "data", "worldview-codebook-v1.0.0.json"
)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

public_files <- c(
  "worldview-browser-data-v1.0.0.json",
  "worldview-browser-data-v1.0.0.json.gz",
  "worldview-wave7-teaching-data-v1.0.0.csv",
  "manifest-v1.0.0.json"
)

missing <- public_files[!file.exists(file.path(public_dir, public_files))]
if (length(missing)) {
  stop(
    "Rebuilt Step 3 files are missing: ",
    paste(missing, collapse = ", "),
    ". Run worldview_step3_rebuild_with_new_variables.R first."
  )
}
if (!file.exists(processed_path)) {
  stop("Processed Step 2 RDS was not found: ", processed_path)
}
if (!file.exists(codebook_path)) {
  stop("Static-app codebook was not found: ", codebook_path)
}

requested <- c(paste0("Q", 18:26), "Q238")
labels <- c(
  Q18 = "One of my main goals in life has been to make my parents proud",
  Q19 = "Parents have a duty to do their best for their children",
  Q20 = "Respect and love for parents regardless of their qualities and faults",
  Q21 = "Men make better political leaders than women do",
  Q22 = "University is more important for a boy than for a girl",
  Q23 = "Men make better business executives than women do",
  Q24 = "Being a housewife is just as fulfilling as working for pay",
  Q25 = "When jobs are scarce, men should have more right to a job than women",
  Q26 = "It is a problem if women have more income than their husbands",
  Q238 = paste(
    "Having a strong leader who does not have to bother with",
    "parliament and elections"
  )
)

# Validate rebuilt assets before installing them.
browser <- jsonlite::fromJSON(
  file.path(public_dir, "worldview-browser-data-v1.0.0.json"),
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
manifest <- jsonlite::fromJSON(
  file.path(public_dir, "manifest-v1.0.0.json")
)

if (!all(requested %in% names(browser))) {
  stop(
    "The rebuilt browser JSON does not contain: ",
    paste(setdiff(requested, names(browser)), collapse = ", ")
  )
}

column_lengths <- vapply(browser, length, integer(1))
if (length(unique(column_lengths)) != 1L) {
  stop("The rebuilt browser JSON contains unequal column lengths.")
}

if (!all(requested %in% manifest$variables)) {
  stop("The rebuilt manifest does not list all requested variables.")
}

# Preserve a backup of the previous runtime assets.
backup_dir <- file.path(app_dir, "data", "before_step29")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
for (file in public_files) {
  existing <- file.path(app_dir, "data", file)
  if (file.exists(existing)) {
    file.copy(existing, file.path(backup_dir, file), overwrite = TRUE)
  }
}
file.copy(
  codebook_path,
  file.path(backup_dir, basename(codebook_path)),
  overwrite = TRUE
)

copied <- file.copy(
  file.path(public_dir, public_files),
  file.path(app_dir, "data", public_files),
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copied)) {
  stop(
    "Could not install: ",
    paste(public_files[!copied], collapse = ", ")
  )
}

# Extract factor labels and order from the processed source where available.
processed <- readRDS(processed_path)
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)

make_levels <- function(x) {
  if (!is.factor(x)) return(NULL)
  labels_local <- levels(x)
  if (!length(labels_local)) return(NULL)
  lapply(seq_along(labels_local), function(i) {
    list(
      value = i,
      label = labels_local[[i]],
      displayOrder = i
    )
  })
}

for (id in requested) {
  metadata <- list(
    id = id,
    displayName = unname(labels[[id]]),
    analysisType = "ordinal",
    correlationEligible = TRUE,
    correlationRepresentation = paste(
      "Ordered response categories represented by their WorldView order"
    ),
    topic = if (id == "Q238") {
      "Political culture and political regimes"
    } else {
      "Social values, norms and stereotypes"
    }
  )

  levels_local <- make_levels(processed[[id]])
  if (!is.null(levels_local)) metadata$levels <- levels_local

  position <- which(vapply(
    codebook$variables,
    function(variable) identical(variable$id, id),
    logical(1)
  ))

  if (length(position)) {
    codebook$variables[[position[[1]]]] <- modifyList(
      codebook$variables[[position[[1]]]],
      metadata
    )
  } else {
    codebook$variables[[length(codebook$variables) + 1L]] <- metadata
  }
}

# Keep variables in numerical question order, with non-question fields last.
variable_number <- vapply(codebook$variables, function(variable) {
  value <- suppressWarnings(as.integer(sub("^Q", "", variable$id)))
  if (is.na(value)) Inf else value
}, numeric(1))
codebook$variables <- codebook$variables[order(variable_number)]

jsonlite::write_json(
  codebook,
  codebook_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  null = "null",
  digits = NA
)

# Null-coalescing helper for optional metadata fields.
`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0L) right else left
}

# Regenerate the CSV codebook if that asset is present.
codebook_csv_path <- file.path(
  app_dir, "data", "worldview-codebook-v1.0.0.csv"
)
if (file.exists(codebook_csv_path)) {
  codebook_rows <- do.call(rbind, lapply(codebook$variables, function(variable) {
    data.frame(
      variable = variable$id,
      display_name = variable$displayName %||% "",
      topic = variable$topic %||% "",
      analysis_type = variable$analysisType %||% "",
      correlation_eligible = variable$correlationEligible %||% FALSE,
      correlation_representation = variable$correlationRepresentation %||% "",
      stringsAsFactors = FALSE
    )
  }))
  write.csv(
    codebook_rows,
    codebook_csv_path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


installed_browser <- jsonlite::fromJSON(
  file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
installed_codebook <- jsonlite::fromJSON(
  codebook_path,
  simplifyVector = FALSE
)
installed_ids <- vapply(
  installed_codebook$variables,
  function(variable) variable$id,
  character(1)
)

checks <- data.frame(
  check = c(
    "four_rebuilt_assets_installed",
    "all_runtime_columns_have_equal_length",
    "participant_count_matches_manifest",
    "all_requested_runtime_variables_present",
    "all_requested_codebook_records_present",
    "all_requested_variables_have_topics",
    "backup_created"
  ),
  passed = c(
    all(file.exists(file.path(app_dir, "data", public_files))),
    length(unique(vapply(installed_browser, length, integer(1)))) == 1L,
    length(installed_browser[[1]]) == manifest$participantCount,
    all(requested %in% names(installed_browser)),
    all(requested %in% installed_ids),
    all(vapply(requested, function(id) {
      record <- installed_codebook$variables[[match(id, installed_ids)]]
      !is.null(record$topic) && nzchar(record$topic)
    }, logical(1))),
    dir.exists(backup_dir)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29 validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

summary_lines <- c(
  "WorldView Step 29 data installation",
  paste("Participant rows:", length(installed_browser[[1]])),
  paste("Runtime columns:", length(installed_browser)),
  paste("Student variables in manifest:", manifest$studentVariableCount),
  paste("Data version:", manifest$dataVersion),
  paste("Checks passed:", sum(checks$passed), "of", nrow(checks)),
  paste("Backup folder:", backup_dir)
)
writeLines(summary_lines, file.path(app_dir, "step29_summary.txt"))

cat("\nStep 29 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
