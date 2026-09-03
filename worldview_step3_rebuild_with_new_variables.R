# WorldView Online - Step 3 revised
# Rebuild the reproducible teaching sample with Q18-Q26 and Q238 included.

student_variables <- c(
  paste0("Q", 18:26),
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q238", "Q260", "Q262", "Q263", "Q275", "Q288", "Q289"
)

binary_variables <- c("Q165", "Q166", "Q167", "Q168", "Q260", "Q263")
ordered_variables <- c(
  paste0("Q", 18:26),
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q238", "Q275", "Q288"
)
integer_variables <- "Q262"
nominal_variables <- "Q289"
country_variables <- c("B_COUNTRY", "B_COUNTRY_ALPHA")

input_path <- file.path("worldview_step2_output", "worldview_processed_full_private.rds")
output_dir <- file.path(getwd(), "worldview_step3_output")
public_dir <- file.path(output_dir, "public_data")
dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)

maximum_per_country <- 1000L
sampling_seed <- 382L
data_version <- "WV7-WORLDVIEW-1.1.0"

if (!file.exists(input_path)) stop("Step 2 private RDS not found: ", input_path)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")

cat("Reading processed private data...\n")
processed_data <- readRDS(input_path)
required_columns <- c(country_variables, student_variables)
missing_columns <- setdiff(required_columns, names(processed_data))
if (length(missing_columns)) stop("Step 2 output is missing: ", paste(missing_columns, collapse = ", "))
processed_data <- processed_data[, required_columns, drop = FALSE]

# Reproduce the original stratified sample exactly. Adding columns does not alter
# the random selections because the seed, row order, grouping, and sample sizes
# are unchanged.
processed_data$.source_row <- seq_len(nrow(processed_data))
set.seed(sampling_seed)
country_groups <- split(processed_data, processed_data$B_COUNTRY_ALPHA, drop = TRUE)
sampled_groups <- lapply(country_groups, function(country_data) {
  country_data <- country_data[order(country_data$.source_row), , drop = FALSE]
  retain_n <- min(nrow(country_data), maximum_per_country)
  selected <- sample.int(nrow(country_data), size = retain_n, replace = FALSE)
  country_data[selected, , drop = FALSE]
})
sampled_data <- do.call(rbind, sampled_groups)
row.names(sampled_data) <- NULL
sampled_data <- sampled_data[order(sampled_data$B_COUNTRY_ALPHA, sampled_data$.source_row), , drop = FALSE]

sampled_data$WORLDVIEW_ID <- ave(
  sampled_data$.source_row,
  sampled_data$B_COUNTRY_ALPHA,
  FUN = function(x) sprintf("%04d", seq_along(x))
)
sampled_data$WORLDVIEW_ID <- paste0("WV7-", sampled_data$B_COUNTRY_ALPHA, "-", sampled_data$WORLDVIEW_ID)
sampled_data$.source_row <- NULL
sampled_data <- sampled_data[, c("WORLDVIEW_ID", country_variables, student_variables), drop = FALSE]

# Student-readable CSV.
download_data <- sampled_data
for (id in student_variables) if (is.factor(download_data[[id]])) download_data[[id]] <- as.character(download_data[[id]])
write.csv(
  download_data,
  file.path(public_dir, "worldview-wave7-teaching-data-v1.0.0.csv"),
  row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

# Browser analysis codes.
browser_data <- data.frame(
  WORLDVIEW_ID = sampled_data$WORLDVIEW_ID,
  B_COUNTRY = sampled_data$B_COUNTRY,
  B_COUNTRY_ALPHA = sampled_data$B_COUNTRY_ALPHA,
  stringsAsFactors = FALSE
)
for (id in c("Q165", "Q166", "Q167", "Q168")) {
  browser_data[[id]] <- ifelse(is.na(sampled_data[[id]]), NA_integer_, ifelse(as.character(sampled_data[[id]]) == "Yes", 1L, 0L))
}
browser_data$Q260 <- ifelse(is.na(sampled_data$Q260), NA_integer_, ifelse(as.character(sampled_data$Q260) == "Male", 1L, 0L))
browser_data$Q263 <- ifelse(
  is.na(sampled_data$Q263), NA_integer_,
  ifelse(as.character(sampled_data$Q263) == "I am born in this country", 1L, 0L)
)
for (id in ordered_variables) browser_data[[id]] <- as.integer(sampled_data[[id]])
browser_data$Q262 <- as.integer(sampled_data$Q262)
browser_data$Q289 <- as.character(sampled_data$Q289)
browser_data <- browser_data[, c("WORLDVIEW_ID", country_variables, student_variables), drop = FALSE]

jsonlite::write_json(
  browser_data,
  file.path(public_dir, "worldview-browser-data-v1.0.0.json"),
  dataframe = "columns", na = "null", auto_unbox = TRUE, digits = NA
)
json_connection <- gzfile(file.path(public_dir, "worldview-browser-data-v1.0.0.json.gz"), open = "wt", encoding = "UTF-8")
jsonlite::write_json(browser_data, json_connection, dataframe = "columns", na = "null", auto_unbox = TRUE, digits = NA)
close(json_connection)

sample_counts <- aggregate(
  rep(1L, nrow(sampled_data)),
  by = list(B_COUNTRY_ALPHA = sampled_data$B_COUNTRY_ALPHA, B_COUNTRY = sampled_data$B_COUNTRY),
  FUN = sum
)
names(sample_counts)[3] <- "sampled_participants"
sample_counts <- sample_counts[order(sample_counts$B_COUNTRY_ALPHA), ]
row.names(sample_counts) <- NULL
write.csv(sample_counts, file.path(output_dir, "sample_counts.csv"), row.names = FALSE, na = "")

missingness <- data.frame(
  variable = student_variables,
  sampled_missing = vapply(student_variables, function(id) sum(is.na(sampled_data[[id]])), integer(1)),
  sampled_valid = vapply(student_variables, function(id) sum(!is.na(sampled_data[[id]])), integer(1)),
  stringsAsFactors = FALSE
)
write.csv(missingness, file.path(output_dir, "sampled_variable_missingness.csv"), row.names = FALSE, na = "")

file_info <- file.info(list.files(public_dir, full.names = TRUE))
asset_report <- data.frame(
  file = basename(row.names(file_info)), bytes = file_info$size,
  megabytes = round(file_info$size / 1024^2, 3), stringsAsFactors = FALSE
)
write.csv(asset_report, file.path(output_dir, "asset_sizes.csv"), row.names = FALSE, na = "")

expected_student_variables <- 39L
expected_export_columns <- 42L
validation <- data.frame(
  check = c(
    "exactly_39_student_variables", "exactly_42_export_columns",
    "no_country_exceeds_1000", "worldview_ids_unique",
    "country_names_present", "country_alpha_present",
    "browser_and_download_rows_equal", "q289_not_numeric_in_browser_data",
    "binary_browser_values_are_0_or_1", "requested_variables_present"
  ),
  passed = c(
    length(student_variables) == expected_student_variables,
    ncol(sampled_data) == expected_export_columns,
    all(sample_counts$sampled_participants <= maximum_per_country),
    !anyDuplicated(sampled_data$WORLDVIEW_ID), !anyNA(sampled_data$B_COUNTRY),
    !anyNA(sampled_data$B_COUNTRY_ALPHA), nrow(browser_data) == nrow(download_data),
    is.character(browser_data$Q289),
    all(vapply(binary_variables, function(id) all(stats::na.omit(browser_data[[id]]) %in% c(0L, 1L)), logical(1))),
    all(c(paste0("Q", 18:26), "Q238") %in% names(browser_data))
  ),
  stringsAsFactors = FALSE
)
write.csv(validation, file.path(output_dir, "step3_validation_checks.csv"), row.names = FALSE, na = "")
if (!all(validation$passed)) stop("Step 3 validation failed: ", paste(validation$check[!validation$passed], collapse = ", "))

manifest <- list(
  dataVersion = data_version, source = "World Values Survey Wave 7",
  samplingSeed = sampling_seed, maximumParticipantsPerCountry = maximum_per_country,
  countryCount = length(unique(sampled_data$B_COUNTRY_ALPHA)),
  participantCount = nrow(sampled_data), studentVariableCount = length(student_variables),
  exportColumnCount = ncol(sampled_data), variables = student_variables
)
jsonlite::write_json(manifest, file.path(public_dir, "manifest-v1.0.0.json"), pretty = TRUE, auto_unbox = TRUE)

summary_lines <- c(
  "WorldView Step 3 revised sample and export",
  paste("Sampled rows:", nrow(sampled_data)),
  paste("Countries:", length(unique(sampled_data$B_COUNTRY_ALPHA))),
  paste("Maximum participants in any country:", max(sample_counts$sampled_participants)),
  paste("Export columns:", ncol(sampled_data)),
  paste("Student variables:", length(student_variables)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Sampling seed:", sampling_seed), paste("Data version:", data_version)
)
writeLines(summary_lines, file.path(output_dir, "step3_summary.txt"))
cat("\nStep 3 revised completed successfully.\n\n", paste(summary_lines, collapse = "\n"), "\n", sep = "")
