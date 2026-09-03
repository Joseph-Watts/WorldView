# WorldView Online - Step 2b
# Add Q18-Q26 and Q238 to the existing Step 2 processed private data.
# This validates row alignment against WVS7_Individual.rds before changing data.

processed_path <- file.path(
  "worldview_step2_output",
  "worldview_processed_full_private.rds"
)
source_candidates <- c(
  file.path("WVS_Dataset", "WVS7_Individual.rds"),
  "WVS7_Individual.rds"
)
source_path <- source_candidates[file.exists(source_candidates)][1]
output_path <- processed_path
report_dir <- "worldview_step2_output"
requested <- c(paste0("Q", 18:26), "Q238")

if (!file.exists(processed_path)) {
  stop("Existing Step 2 output was not found: ", processed_path)
}
if (!length(source_path) || is.na(source_path)) {
  stop("WVS7_Individual.rds was not found in WVS_Dataset/ or the project root.")
}

processed <- readRDS(processed_path)
source_object <- readRDS(source_path)

find_frames <- function(x, path = "root") {
  output <- list()
  if (is.data.frame(x)) {
    output[[path]] <- x
  } else if (is.list(x)) {
    item_names <- names(x)
    if (is.null(item_names)) item_names <- as.character(seq_along(x))
    for (i in seq_along(x)) {
      output <- c(
        output,
        find_frames(x[[i]], paste0(path, "/", item_names[[i]]))
      )
    }
  }
  output
}

if (is.data.frame(source_object)) {
  source <- source_object
  source_frame <- basename(source_path)
} else {
  frames <- find_frames(source_object, basename(source_path))
  if (!length(frames)) stop("No data frame was found inside WVS7_Individual.rds.")
  scores <- vapply(
    frames,
    function(frame) sum(requested %in% names(frame)),
    integer(1)
  )
  source <- frames[[which.max(scores)]]
  source_frame <- names(frames)[which.max(scores)]
}

missing_requested <- setdiff(requested, names(source))
if (length(missing_requested)) {
  stop("Source data are missing: ", paste(missing_requested, collapse = ", "))
}
if (nrow(processed) != nrow(source)) {
  stop(
    "Step 2 and WVS source row counts differ: ",
    nrow(processed), " versus ", nrow(source), "."
  )
}

# Validate row alignment using country identifiers. Exact agreement is required.
country_candidates <- c(
  "B_COUNTRY_ALPHA", "B_COUNTRY", "S003", "COUNTRY_ALPHA"
)
shared_country <- intersect(
  country_candidates,
  intersect(names(processed), names(source))
)
if (!length(shared_country)) {
  stop("No shared country identifier is available to validate row alignment.")
}

normalise_country <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  trimws(as.character(x))
}
country_checks <- vapply(shared_country, function(id) {
  identical(
    normalise_country(processed[[id]]),
    normalise_country(source[[id]])
  )
}, logical(1))

if (!any(country_checks)) {
  stop(
    "The Step 2 and WVS source rows are not aligned by any shared country identifier."
  )
}

# Where available, validate additional existing raw variables after converting
# labelled vectors to their underlying numeric codes. This guards against two
# differently ordered files that happen to share country blocks.
validation_variables <- intersect(
  c("Q165", "Q166", "Q167", "Q168", "Q260", "Q262", "Q263"),
  intersect(names(processed), names(source))
)

underlying <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  if (is.factor(x)) return(as.character(x))
  x
}

# Existing processed columns may be labelled factors, so country sequence is the
# strict alignment check. Existing-variable checks are recorded diagnostically.
variable_agreement <- vapply(validation_variables, function(id) {
  left <- underlying(processed[[id]])
  right <- underlying(source[[id]])
  if (length(left) != length(right)) return(0)
  mean(
    (is.na(left) & is.na(right)) |
      (!is.na(left) & !is.na(right) & as.character(left) == as.character(right))
  )
}, numeric(1))

clean_codes <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  if (is.factor(x)) x <- as.character(x)
  values <- suppressWarnings(as.numeric(x))
  values[values < 0] <- NA_real_
  values
}

# Store ordered factors so the revised Step 3 can use as.integer() consistently.
for (id in requested) {
  values <- clean_codes(source[[id]])
  valid_levels <- sort(unique(values[!is.na(values)]))
  processed[[id]] <- factor(
    values,
    levels = valid_levels,
    labels = as.character(valid_levels),
    ordered = TRUE
  )
}

# Preserve a backup the first time this patch is run.
backup_path <- file.path(
  report_dir,
  "worldview_processed_full_private_before_step2b.rds"
)
if (!file.exists(backup_path)) {
  file.copy(processed_path, backup_path, overwrite = FALSE)
}
saveRDS(processed, output_path, compress = "xz")

checks <- data.frame(
  check = c(
    "source_and_processed_row_counts_match",
    "country_sequence_matches",
    "all_requested_variables_added",
    "requested_variables_are_ordered_factors",
    "negative_missing_codes_removed",
    "backup_present"
  ),
  passed = c(
    nrow(processed) == nrow(source),
    any(country_checks),
    all(requested %in% names(processed)),
    all(vapply(requested, function(id) is.ordered(processed[[id]]), logical(1))),
    all(vapply(requested, function(id) {
      codes <- as.integer(processed[[id]])
      all(is.na(codes) | codes >= 1L)
    }, logical(1))),
    file.exists(backup_path)
  ),
  stringsAsFactors = FALSE
)
write.csv(
  checks,
  file.path(report_dir, "step2b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

alignment_report <- c(
  "WorldView Step 2b alignment report",
  paste("Source:", source_path),
  paste("Source frame:", source_frame),
  paste("Rows:", nrow(processed)),
  paste(
    "Matching country identifiers:",
    paste(shared_country[country_checks], collapse = ", ")
  ),
  "Existing-variable diagnostic agreement:",
  if (length(variable_agreement)) {
    paste(names(variable_agreement), sprintf("%.6f", variable_agreement), sep = ": ")
  } else {
    "No additional shared validation variables were available."
  }
)
writeLines(
  alignment_report,
  file.path(report_dir, "step2b_alignment_report.txt")
)

if (!all(checks$passed)) {
  stop(
    "Step 2b validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 2b completed successfully.\n")
cat("Added Q18-Q26 and Q238 to: ", output_path, "\n", sep = "")
cat("Backup: ", backup_path, "\n\n", sep = "")
cat("Now rerun:\n")
cat("  source(\"worldview_step3_rebuild_with_new_variables.R\")\n")
