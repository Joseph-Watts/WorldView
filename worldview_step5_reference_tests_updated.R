# WorldView Online - Step 5
# Generate R reference results for testing browser summary statistics and
# Pearson correlations against the sampled WorldView dataset.

input_path <- file.path(
  "worldview_step3_output",
  "public_data",
  "worldview-browser-data-v1.0.0.json"
)

output_dir <- file.path(getwd(), "worldview_step5_output")
fixture_dir <- file.path(
  getwd(),
  "worldview_step3_output",
  "public_data"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

if (!file.exists(input_path)) {
  stop("Browser data not found: ", input_path)
}

cat("Reading sampled browser data...\n")

# Step 3 wrote the JSON in column-oriented form. jsonlite therefore reads it
# as a named list of equal-length columns rather than directly as a data frame.
json_columns <- jsonlite::fromJSON(
  input_path,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

if (!is.list(json_columns) || is.null(names(json_columns))) {
  stop("Browser JSON did not load as a named column list.")
}

column_lengths <- vapply(json_columns, length, integer(1))

if (length(unique(column_lengths)) != 1L) {
  stop(
    "Browser JSON columns have different lengths: ",
    paste(names(column_lengths), column_lengths, sep = "=", collapse = ", ")
  )
}

data <- as.data.frame(
  json_columns,
  stringsAsFactors = FALSE,
  optional = TRUE
)

if (!is.data.frame(data)) {
  stop("Browser JSON could not be converted to a data frame.")
}

numeric_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288"
)

required_columns <- c(
  "WORLDVIEW_ID", "B_COUNTRY", "B_COUNTRY_ALPHA",
  numeric_variables, "Q289"
)

missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0L) {
  stop("Missing browser columns: ", paste(missing_columns, collapse = ", "))
}

for (id in numeric_variables) {
  data[[id]] <- as.numeric(data[[id]])
}

# Numeric descriptive statistics using R type-7 quartiles.
numeric_summary <- function(x) {
  valid <- x[!is.na(x)]
  missing_n <- sum(is.na(x))

  if (length(valid) == 0L) {
    return(list(
      totalN = length(x), validN = 0L, missingN = missing_n,
      mean = NULL, standardDeviation = NULL, median = NULL,
      minimum = NULL, firstQuartile = NULL, thirdQuartile = NULL,
      maximum = NULL
    ))
  }

  quartiles <- stats::quantile(
    valid,
    probs = c(0.25, 0.75),
    type = 7,
    names = FALSE
  )

  list(
    totalN = length(x),
    validN = length(valid),
    missingN = missing_n,
    mean = mean(valid),
    standardDeviation = if (length(valid) > 1L) stats::sd(valid) else NULL,
    median = stats::median(valid),
    minimum = min(valid),
    firstQuartile = quartiles[[1]],
    thirdQuartile = quartiles[[2]],
    maximum = max(valid)
  )
}

# Frequency summary for categorical variables. Missing values are reported
# separately and percentages use valid responses as the denominator.
frequency_summary <- function(x) {
  valid <- x[!is.na(x)]
  counts <- table(valid, useNA = "no")
  valid_n <- length(valid)

  categories <- lapply(seq_along(counts), function(i) {
    count <- as.integer(counts[[i]])
    list(
      value = names(counts)[[i]],
      count = count,
      percentageOfValid = if (valid_n > 0L) 100 * count / valid_n else NULL
    )
  })

  list(
    totalN = length(x),
    validN = valid_n,
    missingN = sum(is.na(x)),
    categories = categories
  )
}

pearson_reference <- function(data, x_id, y_id, countries = NULL) {
  working <- data

  if (!is.null(countries)) {
    working <- working[
      working$B_COUNTRY_ALPHA %in% countries,
      , drop = FALSE
    ]
  }

  x <- working[[x_id]]
  y <- working[[y_id]]
  complete <- stats::complete.cases(x, y)
  x_complete <- x[complete]
  y_complete <- y[complete]
  n <- length(x_complete)

  result <- list(
    variable1 = x_id,
    variable2 = y_id,
    countries = if (is.null(countries)) "ALL" else countries,
    filteredRows = nrow(working),
    completePairs = n,
    excludedMissingEither = nrow(working) - n
  )

  if (n < 10L) {
    result$status <- "insufficient_complete_pairs"
    return(result)
  }

  if (length(unique(x_complete)) < 2L || length(unique(y_complete)) < 2L) {
    result$status <- "no_variation"
    return(result)
  }

  test <- stats::cor.test(
    x_complete,
    y_complete,
    method = "pearson",
    conf.level = 0.95
  )

  result$status <- "ok"
  result$correlation <- unname(test$estimate)
  result$confidenceLevel <- 0.95
  result$confidenceLower <- unname(test$conf.int[[1]])
  result$confidenceUpper <- unname(test$conf.int[[2]])
  result$pValue <- test$p.value
  result
}

# Fixed reference cases cover all supported variable-type pairings.
correlation_cases <- list(
  pearson_reference(data, "Q165", "Q167", "NZL"),
  pearson_reference(data, "Q165", "Q177", "NZL"),
  pearson_reference(data, "Q165", "Q262", "NZL"),
  pearson_reference(data, "Q177", "Q184", "NZL"),
  pearson_reference(data, "Q177", "Q262", "NZL"),
  pearson_reference(data, "Q262", "Q288", "NZL"),
  pearson_reference(data, "Q167", "Q262", c("NZL", "AUS")),
  pearson_reference(data, "Q167", "Q262", NULL)
)

summary_cases <- list(
  list(
    id = "NZL_Q262_numeric",
    country = "NZL",
    variable = "Q262",
    result = numeric_summary(data$Q262[data$B_COUNTRY_ALPHA == "NZL"])
  ),
  list(
    id = "NZL_Q177_numeric",
    country = "NZL",
    variable = "Q177",
    result = numeric_summary(data$Q177[data$B_COUNTRY_ALPHA == "NZL"])
  ),
  list(
    id = "NZL_Q165_frequency",
    country = "NZL",
    variable = "Q165",
    result = frequency_summary(data$Q165[data$B_COUNTRY_ALPHA == "NZL"])
  ),
  list(
    id = "NZL_Q289_frequency",
    country = "NZL",
    variable = "Q289",
    result = frequency_summary(data$Q289[data$B_COUNTRY_ALPHA == "NZL"])
  ),
  list(
    id = "ALL_Q262_numeric",
    country = "ALL",
    variable = "Q262",
    result = numeric_summary(data$Q262)
  )
)

fixtures <- list(
  fixtureVersion = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  calculationRules = list(
    missingData = "Pairwise complete observations for correlations",
    correlation = "Pearson correlation",
    confidenceInterval = "R cor.test 95% confidence interval",
    minimumCompletePairs = 10,
    standardDeviation = "Sample standard deviation using denominator n - 1",
    quartiles = "R quantile type 7",
    percentages = "Percentage of valid responses"
  ),
  summaryCases = summary_cases,
  correlationCases = correlation_cases
)

fixture_path <- file.path(
  fixture_dir,
  "worldview-reference-fixtures-v1.0.0.json"
)

jsonlite::write_json(
  fixtures,
  fixture_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  null = "null",
  digits = NA
)

# Flat correlation results for easy inspection.
correlation_review <- do.call(
  rbind,
  lapply(correlation_cases, function(x) {
    data.frame(
      variable1 = x$variable1,
      variable2 = x$variable2,
      countries = paste(x$countries, collapse = ";"),
      filtered_rows = x$filteredRows,
      complete_pairs = x$completePairs,
      excluded_missing_either = x$excludedMissingEither,
      status = x$status,
      correlation = if (!is.null(x$correlation)) x$correlation else NA_real_,
      ci_lower = if (!is.null(x$confidenceLower)) x$confidenceLower else NA_real_,
      ci_upper = if (!is.null(x$confidenceUpper)) x$confidenceUpper else NA_real_,
      p_value = if (!is.null(x$pValue)) x$pValue else NA_real_,
      stringsAsFactors = FALSE
    )
  })
)

write.csv(
  correlation_review,
  file.path(output_dir, "reference_correlations.csv"),
  row.names = FALSE,
  na = ""
)

validation <- data.frame(
  check = c(
    "browser_data_has_65447_rows",
    "all_required_columns_present",
    "all_numeric_variables_are_numeric",
    "eight_correlation_cases_created",
    "five_summary_cases_created",
    "all_reference_correlations_succeeded",
    "fixture_json_created"
  ),
  passed = c(
    nrow(data) == 65447L,
    length(missing_columns) == 0L,
    all(vapply(data[numeric_variables], is.numeric, logical(1))),
    length(correlation_cases) == 8L,
    length(summary_cases) == 5L,
    all(vapply(correlation_cases, function(x) identical(x$status, "ok"), logical(1))),
    file.exists(fixture_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "step5_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 5 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

fixture_size <- file.info(fixture_path)$size
summary_lines <- c(
  "WorldView Step 5 reference tests",
  paste("Rows tested:", nrow(data)),
  paste("Summary reference cases:", length(summary_cases)),
  paste("Correlation reference cases:", length(correlation_cases)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Fixture bytes:", fixture_size),
  paste("Fixture output:", fixture_path)
)

writeLines(
  summary_lines,
  file.path(output_dir, "step5_summary.txt")
)

cat("\nStep 5 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step5_output:\n")
cat("  step5_summary.txt\n")
cat("  step5_validation_checks.csv\n")
cat("  reference_correlations.csv\n")
