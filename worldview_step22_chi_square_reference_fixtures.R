# WorldView Online - Step 22
# Generate R reference fixtures for the chi-square test of independence.

input_path <- file.path(
  "worldview_step3_output", "public_data",
  "worldview-browser-data-v1.0.0.json"
)
output_dir <- file.path(getwd(), "worldview_step22_output")
fixture_path <- file.path(
  "worldview_static_app", "data",
  "worldview-chi-square-fixtures-v1.0.0.json"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}
if (!file.exists(input_path)) stop("Browser data not found: ", input_path)

columns <- jsonlite::fromJSON(
  input_path,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
data <- as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)

binary_labels <- list(
  Q165 = c("0" = "No", "1" = "Yes"),
  Q166 = c("0" = "No", "1" = "Yes"),
  Q167 = c("0" = "No", "1" = "Yes"),
  Q168 = c("0" = "No", "1" = "Yes"),
  Q260 = c("0" = "Female", "1" = "Male"),
  Q263 = c(
    "0" = "Immigrant to this country",
    "1" = "Born in this country"
  )
)

label_values <- function(values, variable, country_names = NULL) {
  if (variable == "COUNTRY") return(as.character(country_names))
  if (variable %in% names(binary_labels)) {
    return(unname(binary_labels[[variable]][as.character(values)]))
  }
  as.character(values)
}

fit_case <- function(id, row_variable, column_variable, countries = NULL) {
  working <- data
  country_label <- "ALL"
  if (!is.null(countries)) {
    working <- working[working$B_COUNTRY_ALPHA %in% countries, , drop = FALSE]
    country_label <- countries
  }

  row_raw <- if (row_variable == "COUNTRY") {
    working$B_COUNTRY_ALPHA
  } else {
    working[[row_variable]]
  }
  column_raw <- if (column_variable == "COUNTRY") {
    working$B_COUNTRY_ALPHA
  } else {
    working[[column_variable]]
  }

  row_labels <- label_values(row_raw, row_variable, working$B_COUNTRY)
  column_labels <- label_values(column_raw, column_variable, working$B_COUNTRY)

  model_data <- data.frame(
    row = row_labels,
    column = column_labels,
    stringsAsFactors = FALSE
  )
  model_data <- model_data[complete.cases(model_data), , drop = FALSE]
  contingency <- table(model_data$row, model_data$column)
  result <- suppressWarnings(stats::chisq.test(contingency, correct = FALSE))

  observed <- unclass(contingency)
  expected <- unclass(result$expected)
  row_percentages <- prop.table(contingency, margin = 1) * 100
  column_percentages <- prop.table(contingency, margin = 2) * 100
  cell_records <- list()

  for (i in seq_len(nrow(observed))) {
    for (j in seq_len(ncol(observed))) {
      cell_records[[length(cell_records) + 1L]] <- list(
        row = rownames(observed)[i],
        column = colnames(observed)[j],
        observed = unname(observed[i, j]),
        expected = unname(expected[i, j]),
        rowPercentage = unname(row_percentages[i, j]),
        columnPercentage = unname(column_percentages[i, j])
      )
    }
  }

  minimum_dimension <- min(nrow(observed) - 1L, ncol(observed) - 1L)
  cramers_v <- sqrt(
    unname(result$statistic) /
      (sum(observed) * minimum_dimension)
  )

  list(
    id = id,
    rowVariable = row_variable,
    columnVariable = column_variable,
    countries = country_label,
    completeObservations = sum(observed),
    rowCount = nrow(observed),
    columnCount = ncol(observed),
    degreesFreedom = unname(result$parameter),
    chiSquare = unname(result$statistic),
    pValue = unname(result$p.value),
    cramersV = cramers_v,
    expectedBelowFive = sum(expected < 5),
    totalCells = length(expected),
    cells = cell_records
  )
}

cases <- list(
  fit_case("NZL_Q165_BY_Q260", "Q165", "Q260", "NZL"),
  fit_case("NZL_Q289_BY_Q165", "Q289", "Q165", "NZL"),
  fit_case("AUS_NZL_COUNTRY_BY_Q167", "COUNTRY", "Q167", c("AUS", "NZL")),
  fit_case("ALL_Q165_BY_Q167", "Q165", "Q167", NULL),
  fit_case("NZL_Q177_BY_Q260", "Q177", "Q260", "NZL")
)

fixtures <- list(
  fixtureVersion = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  model = "Pearson chi-square test of independence",
  rules = list(
    referenceImplementation = "R stats::chisq.test without continuity correction",
    missingData = "Complete cases for both categorical variables",
    effectSize = "Cramers V"
  ),
  cases = cases
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

review <- do.call(rbind, lapply(cases, function(case) {
  do.call(rbind, lapply(case$cells, function(cell) {
    data.frame(
      case = case$id,
      row_variable = case$rowVariable,
      column_variable = case$columnVariable,
      countries = paste(case$countries, collapse = ";"),
      row = cell$row,
      column = cell$column,
      observed = cell$observed,
      expected = cell$expected,
      row_percentage = cell$rowPercentage,
      column_percentage = cell$columnPercentage,
      chi_square = case$chiSquare,
      degrees_freedom = case$degreesFreedom,
      p_value = case$pValue,
      cramers_v = case$cramersV,
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(
  review,
  file.path(output_dir, "chi_square_reference_results.csv"),
  row.names = FALSE,
  na = ""
)

validation <- data.frame(
  check = c(
    "five_reference_cases_created",
    "binary_by_binary_case_created",
    "q289_case_created",
    "country_case_created",
    "ordered_as_categorical_case_created",
    "all_statistics_finite",
    "fixture_created"
  ),
  passed = c(
    length(cases) == 5L,
    any(vapply(cases, function(x) x$rowVariable == "Q165" && x$columnVariable == "Q260", logical(1))),
    any(vapply(cases, function(x) x$rowVariable == "Q289", logical(1))),
    any(vapply(cases, function(x) x$rowVariable == "COUNTRY", logical(1))),
    any(vapply(cases, function(x) x$rowVariable == "Q177", logical(1))),
    all(vapply(cases, function(x) is.finite(x$chiSquare) && is.finite(x$cramersV), logical(1))),
    file.exists(fixture_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "step22_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 22 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

summary_lines <- c(
  "WorldView Step 22 chi-square fixtures",
  paste("Reference cases:", length(cases)),
  paste("Cell result rows:", nrow(review)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Fixture output:", fixture_path)
)
writeLines(summary_lines, file.path(output_dir, "step22_summary.txt"))

cat("\nStep 22 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step22_output:\n")
cat("  step22_summary.txt\n")
cat("  step22_validation_checks.csv\n")
cat("  chi_square_reference_results.csv\n")
