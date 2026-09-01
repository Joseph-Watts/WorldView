# WorldView Online - Step 2
# Apply the existing WorldView processing rules to the approved variables
# and create validation outputs. This script does not export participant rows.

student_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288", "Q289"
)

factor_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q260", "Q263", "Q289"
)

ordered_variables <- c(
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q275", "Q288"
)

integer_variables <- "Q262"
country_variables <- c("B_COUNTRY", "B_COUNTRY_ALPHA")
required_variables <- c(country_variables, student_variables)

source_path <- file.path(
  "WVS_Dataset",
  "WVS_Cross-National_Wave_7_rds_v6_0.rds"
)

if (!file.exists(source_path)) {
  stop("Source file not found: ", source_path)
}

output_dir <- file.path(getwd(), "worldview_step2_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Reading source data...\n")
source_data <- readRDS(source_path)

missing_variables <- setdiff(required_variables, names(source_data))
if (length(missing_variables) > 0L) {
  stop("Missing required variables: ", paste(missing_variables, collapse = ", "))
}

# Remove haven/vctrs classes while retaining the underlying stored values.
underlying_values <- function(x) {
  if (inherits(x, "haven_labelled")) {
    attributes_to_keep <- attributes(x)[c("names", "dim", "dimnames")]
    attributes(x) <- attributes_to_keep
  }
  x
}

underlying_numeric <- function(x) {
  suppressWarnings(as.numeric(underlying_values(x)))
}

value_label_table <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || length(labels) == 0L) {
    return(data.frame(label = character(), value = numeric()))
  }
  data.frame(
    label = names(labels),
    value = underlying_numeric(labels),
    stringsAsFactors = FALSE
  )
}

# Match the existing WorldView preprocessing:
# negative source values become NA; non-negative labelled values become factors.
process_labelled_factor <- function(x, ordered = FALSE) {
  raw <- underlying_numeric(x)
  raw[raw < 0] <- NA_real_

  lookup <- value_label_table(x)
  lookup <- lookup[lookup$value >= 0, , drop = FALSE]
  lookup <- lookup[!duplicated(lookup$label), , drop = FALSE]

  if (nrow(lookup) == 0L) {
    stop("No non-negative value labels found.")
  }

  matched <- match(raw, lookup$value)
  displayed <- rep(NA_character_, length(raw))
  displayed[!is.na(matched)] <- lookup$label[matched[!is.na(matched)]]

  factor(
    displayed,
    levels = lookup$label,
    ordered = ordered
  )
}

process_integer <- function(x) {
  raw <- underlying_numeric(x)
  raw[raw < 0] <- NA_real_
  as.integer(raw)
}

process_country_name <- function(x) {
  raw <- underlying_numeric(x)
  lookup <- value_label_table(x)
  matched <- match(raw, lookup$value)
  output <- rep(NA_character_, length(raw))
  output[!is.na(matched)] <- lookup$label[matched[!is.na(matched)]]
  output
}

cat("Applying WorldView preprocessing...\n")

processed_data <- data.frame(
  B_COUNTRY = process_country_name(source_data$B_COUNTRY),
  B_COUNTRY_ALPHA = as.character(source_data$B_COUNTRY_ALPHA),
  stringsAsFactors = FALSE
)

for (id in factor_variables) {
  processed_data[[id]] <- process_labelled_factor(
    source_data[[id]],
    ordered = FALSE
  )
}

for (id in ordered_variables) {
  processed_data[[id]] <- process_labelled_factor(
    source_data[[id]],
    ordered = TRUE
  )
}

for (id in integer_variables) {
  processed_data[[id]] <- process_integer(source_data[[id]])
}

processed_data <- processed_data[, required_variables, drop = FALSE]

# Helper for compact level reporting.
level_string <- function(x) {
  if (!is.factor(x)) return(NA_character_)
  paste(levels(x), collapse = " | ")
}

numeric_range <- function(x) {
  values <- if (is.factor(x)) as.numeric(x) else suppressWarnings(as.numeric(x))
  values <- values[!is.na(values)]
  if (length(values) == 0L) return(c(NA_real_, NA_real_))
  c(min(values), max(values))
}

profile_rows <- lapply(student_variables, function(id) {
  before <- source_data[[id]]
  after <- processed_data[[id]]
  range_after <- numeric_range(after)

  data.frame(
    variable = id,
    source_class = paste(class(before), collapse = " "),
    processed_class = paste(class(after), collapse = " "),
    source_negative_codes = sum(underlying_numeric(before) < 0, na.rm = TRUE),
    processed_missing = sum(is.na(after)),
    processed_valid = sum(!is.na(after)),
    processed_distinct_values = length(unique(after[!is.na(after)])),
    processed_numeric_min = range_after[[1]],
    processed_numeric_max = range_after[[2]],
    processed_levels = level_string(after),
    stringsAsFactors = FALSE
  )
})

processed_profile <- do.call(rbind, profile_rows)
write.csv(
  processed_profile,
  file.path(output_dir, "processed_variable_profile.csv"),
  row.names = FALSE,
  na = ""
)

# Long frequency table for checking every processed response level.
frequency_rows <- lapply(student_variables, function(id) {
  x <- processed_data[[id]]

  if (is.factor(x)) {
    counts <- table(x, useNA = "ifany")
    value <- names(counts)
    value[is.na(value) | value == "<NA>"] <- "NA"
  } else {
    counts <- table(x, useNA = "ifany")
    value <- names(counts)
    value[is.na(value) | value == "<NA>"] <- "NA"
  }

  data.frame(
    variable = id,
    processed_value = value,
    count = as.integer(counts),
    stringsAsFactors = FALSE
  )
})

processed_frequencies <- do.call(rbind, frequency_rows)
write.csv(
  processed_frequencies,
  file.path(output_dir, "processed_value_frequencies.csv"),
  row.names = FALSE,
  na = ""
)

# Correct country counts using country labels rather than numeric codes.
country_counts_processed <- aggregate(
  rep(1L, nrow(processed_data)),
  by = list(
    B_COUNTRY_ALPHA = processed_data$B_COUNTRY_ALPHA,
    B_COUNTRY = processed_data$B_COUNTRY
  ),
  FUN = sum
)
names(country_counts_processed)[3] <- "source_participants"
country_counts_processed <- country_counts_processed[
  order(country_counts_processed$B_COUNTRY_ALPHA),
  , drop = FALSE
]
row.names(country_counts_processed) <- NULL

write.csv(
  country_counts_processed,
  file.path(output_dir, "country_counts_with_names.csv"),
  row.names = FALSE,
  na = ""
)

# Validation checks.
validation <- data.frame(
  check = c(
    "exactly_29_student_variables",
    "all_required_columns_present",
    "country_names_all_resolved",
    "country_alpha_all_present",
    "no_negative_processed_numeric_values",
    "all_rows_retained_before_sampling"
  ),
  passed = c(
    length(student_variables) == 29L,
    identical(names(processed_data), required_variables),
    !anyNA(processed_data$B_COUNTRY),
    !anyNA(processed_data$B_COUNTRY_ALPHA),
    all(vapply(
      student_variables,
      function(id) {
        x <- processed_data[[id]]
        values <- if (is.factor(x)) as.numeric(x) else as.numeric(x)
        !any(values < 0, na.rm = TRUE)
      },
      logical(1)
    )),
    nrow(processed_data) == nrow(source_data)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  failed <- validation$check[!validation$passed]
  stop("Validation failed: ", paste(failed, collapse = ", "))
}

# Save an R object for Step 3 only. This remains local and is not a public export.
saveRDS(
  processed_data,
  file.path(output_dir, "worldview_processed_full_private.rds"),
  compress = "xz"
)

summary_lines <- c(
  "WorldView Step 2 processing and validation",
  paste("Source rows:", nrow(source_data)),
  paste("Processed rows:", nrow(processed_data)),
  paste("Processed columns:", ncol(processed_data)),
  paste("Countries:", length(unique(processed_data$B_COUNTRY_ALPHA))),
  paste("Student variables:", length(student_variables)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  "",
  "The private RDS contains participant-level processed data and should not be uploaded or committed.",
  "The CSV validation outputs contain aggregate metadata only."
)

writeLines(
  summary_lines,
  file.path(output_dir, "processing_summary.txt")
)

cat("\nStep 2 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step2_output:\n")
cat("  processing_summary.txt\n")
cat("  validation_checks.csv\n")
cat("  processed_variable_profile.csv\n")
cat("  processed_value_frequencies.csv\n")
cat("  country_counts_with_names.csv\n\n")
cat("Do not share worldview_processed_full_private.rds.\n")
