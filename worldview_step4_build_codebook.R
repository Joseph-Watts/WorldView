# WorldView Online - Step 4
# Build machine-readable and student-readable codebooks from the validated
# processed dataset and the original source metadata.

student_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288", "Q289"
)

binary_variables <- c(
  "Q165", "Q166", "Q167", "Q168", "Q260", "Q263"
)

ordered_variables <- c(
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195", "Q275", "Q288"
)

integer_variables <- "Q262"
nominal_variables <- "Q289"

source_path <- file.path(
  "WVS_Dataset",
  "WVS_Cross-National_Wave_7_rds_v6_0.rds"
)

processed_path <- file.path(
  "worldview_step2_output",
  "worldview_processed_full_private.rds"
)

output_dir <- file.path(getwd(), "worldview_step4_output")
public_dir <- file.path(
  getwd(),
  "worldview_step3_output",
  "public_data"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(source_path)) {
  stop("Source RDS not found: ", source_path)
}

if (!file.exists(processed_path)) {
  stop("Processed private RDS not found: ", processed_path)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

cat("Reading source and processed data...\n")
source_data <- readRDS(source_path)
processed_data <- readRDS(processed_path)

missing_source <- setdiff(student_variables, names(source_data))
missing_processed <- setdiff(student_variables, names(processed_data))

if (length(missing_source) > 0L) {
  stop("Missing source variables: ", paste(missing_source, collapse = ", "))
}

if (length(missing_processed) > 0L) {
  stop("Missing processed variables: ", paste(missing_processed, collapse = ", "))
}

source_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || length(label) == 0L) return(NA_character_)
  as.character(label[[1]])
}

variable_group <- function(id) {
  number <- as.integer(sub("Q", "", id, fixed = TRUE))

  if (number >= 165L && number <= 168L) {
    return("Religious values")
  }

  if (number >= 177L && number <= 195L) {
    return("Ethical values and norms")
  }

  if (number >= 260L && number <= 289L) {
    return("Demographics")
  }

  "Other"
}

analysis_type <- function(id) {
  if (id %in% binary_variables) return("binary")
  if (id %in% ordered_variables) return("ordinal")
  if (id %in% integer_variables) return("integer")
  if (id %in% nominal_variables) return("nominal")
  stop("No analysis type specified for ", id)
}

display_type <- function(id) {
  if (id %in% binary_variables) return("factor")
  if (id %in% ordered_variables) return("ordered factor")
  if (id %in% integer_variables) return("integer")
  if (id %in% nominal_variables) return("factor")
  stop("No display type specified for ", id)
}

correlation_eligible <- function(id) {
  !id %in% nominal_variables
}

correlation_representation <- function(id) {
  if (id %in% c("Q165", "Q166", "Q167", "Q168")) {
    return("No = 0; Yes = 1")
  }

  if (id == "Q260") {
    return("Female = 0; Male = 1")
  }

  if (id == "Q263") {
    return("Immigrant = 0; born in this country = 1")
  }

  if (id %in% ordered_variables) {
    return("Categories represented by their WorldView order, beginning at 1")
  }

  if (id %in% integer_variables) {
    return("Processed integer value")
  }

  "Not available for Pearson correlation"
}

missing_rule <- function(id) {
  paste0(
    "Negative source values, including non-response and not-asked codes, ",
    "are treated as missing. Missing rows are excluded only from the ",
    "specific summary or correlation being calculated."
  )
}

interpretation_note <- function(id) {
  if (id %in% ordered_variables) {
    return(
      paste0(
        "Response categories are ordered. For summaries and correlations, ",
        "WorldView uses their ordered numerical positions."
      )
    )
  }

  if (id %in% binary_variables) {
    return(
      paste0(
        "The two displayed categories are represented numerically for ",
        "correlation analysis as documented in correlationRepresentation."
      )
    )
  }

  if (id %in% integer_variables) {
    return("The processed integer value is used directly in analyses.")
  }

  paste0(
    "This is an unordered multi-category variable. It is available for ",
    "summaries and bar graphs but not Pearson correlation."
  )
}

level_records <- function(x) {
  if (!is.factor(x)) return(list())

  lev <- levels(x)
  lapply(seq_along(lev), function(index) {
    list(
      displayOrder = as.integer(index),
      label = lev[[index]]
    )
  })
}

valid_range <- function(x) {
  if (is.factor(x)) return(NULL)
  values <- x[!is.na(x)]
  if (length(values) == 0L) return(NULL)
  list(
    minimum = min(values),
    maximum = max(values)
  )
}

variable_records <- lapply(student_variables, function(id) {
  processed <- processed_data[[id]]

  list(
    id = id,
    displayName = source_label(source_data[[id]]),
    group = variable_group(id),
    sourceQuestionLabel = source_label(source_data[[id]]),
    displayType = display_type(id),
    analysisType = analysis_type(id),
    ordered = is.ordered(processed),
    correlationEligible = correlation_eligible(id),
    correlationRepresentation = correlation_representation(id),
    missingRule = missing_rule(id),
    interpretationNote = interpretation_note(id),
    validCountInFullProcessedData = sum(!is.na(processed)),
    missingCountInFullProcessedData = sum(is.na(processed)),
    levels = level_records(processed),
    validRange = valid_range(processed)
  )
})

codebook <- list(
  title = "WorldView WVS Wave 7 Codebook",
  version = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  variableCount = length(variable_records),
  generalPreprocessing = list(
    source = "World Values Survey Wave 7",
    scope = paste0(
      "WorldView teaching subset containing 29 displayed variables and no ",
      "more than 1,000 participants per country."
    ),
    missingValues = paste0(
      "Negative source values are converted to missing values. The original ",
      "WVS negative codes are not treated as substantive response categories."
    ),
    categoricalValues = paste0(
      "Student-facing categories use the processed labels and ordering from ",
      "the WorldView pipeline."
    ),
    correlations = paste0(
      "Pearson correlations use pairwise complete observations and the ",
      "documented numerical representations. Multi-category nominal ",
      "variables are excluded."
    )
  ),
  variables = variable_records
)

json_path <- file.path(public_dir, "worldview-codebook-v1.0.0.json")
jsonlite::write_json(
  codebook,
  json_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  null = "null"
)

# Flat CSV for human review and student download.
flatten_levels <- function(x) {
  if (!is.factor(x)) return(NA_character_)
  paste(
    paste0(seq_along(levels(x)), " = ", levels(x)),
    collapse = " | "
  )
}

codebook_csv <- data.frame(
  variable = student_variables,
  display_name = vapply(
    student_variables,
    function(id) source_label(source_data[[id]]),
    character(1)
  ),
  group = vapply(student_variables, variable_group, character(1)),
  display_type = vapply(student_variables, display_type, character(1)),
  analysis_type = vapply(student_variables, analysis_type, character(1)),
  ordered = vapply(
    student_variables,
    function(id) is.ordered(processed_data[[id]]),
    logical(1)
  ),
  correlation_eligible = vapply(
    student_variables,
    correlation_eligible,
    logical(1)
  ),
  correlation_representation = vapply(
    student_variables,
    correlation_representation,
    character(1)
  ),
  processed_levels = vapply(
    student_variables,
    function(id) flatten_levels(processed_data[[id]]),
    character(1)
  ),
  missing_rule = vapply(student_variables, missing_rule, character(1)),
  interpretation_note = vapply(
    student_variables,
    interpretation_note,
    character(1)
  ),
  stringsAsFactors = FALSE
)

csv_path <- file.path(public_dir, "worldview-codebook-v1.0.0.csv")
write.csv(
  codebook_csv,
  csv_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

validation <- data.frame(
  check = c(
    "exactly_29_codebook_records",
    "variable_ids_match_approved_list",
    "all_display_names_present",
    "all_variables_have_analysis_type",
    "all_factor_variables_have_levels",
    "only_q289_is_correlation_ineligible",
    "json_file_created",
    "csv_file_created"
  ),
  passed = c(
    length(variable_records) == 29L,
    identical(vapply(variable_records, `[[`, character(1), "id"), student_variables),
    all(nzchar(codebook_csv$display_name)),
    all(nzchar(codebook_csv$analysis_type)),
    all(vapply(student_variables, function(id) {
      x <- processed_data[[id]]
      !is.factor(x) || length(levels(x)) > 0L
    }, logical(1))),
    identical(student_variables[!codebook_csv$correlation_eligible], "Q289"),
    file.exists(json_path),
    file.exists(csv_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "step4_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 4 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

asset_info <- file.info(c(json_path, csv_path))
asset_sizes <- data.frame(
  file = basename(row.names(asset_info)),
  bytes = asset_info$size,
  kilobytes = round(asset_info$size / 1024, 2),
  stringsAsFactors = FALSE
)

write.csv(
  asset_sizes,
  file.path(output_dir, "step4_asset_sizes.csv"),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "WorldView Step 4 codebook build",
  paste("Codebook variables:", length(variable_records)),
  paste("Correlation-eligible variables:", sum(codebook_csv$correlation_eligible)),
  paste("Correlation-ineligible variables:", paste(
    codebook_csv$variable[!codebook_csv$correlation_eligible],
    collapse = ", "
  )),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("JSON output:", json_path),
  paste("CSV output:", csv_path)
)

writeLines(summary_lines, file.path(output_dir, "step4_summary.txt"))

cat("\nStep 4 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step4_output:\n")
cat("  step4_summary.txt\n")
cat("  step4_validation_checks.csv\n")
cat("  step4_asset_sizes.csv\n\n")
cat("Also share the student-readable codebook CSV from:\n")
cat("  worldview_step3_output/public_data/worldview-codebook-v1.0.0.csv\n")
