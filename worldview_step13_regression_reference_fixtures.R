# WorldView Online - Step 13
# Generate R reference fixtures for multiple linear regression.

browser_data_path <- file.path(
  "worldview_step3_output", "public_data",
  "worldview-browser-data-v1.0.0.json"
)

output_dir <- file.path(getwd(), "worldview_step13_output")
fixture_path <- file.path(
  "worldview_static_app", "data",
  "worldview-regression-fixtures-v1.0.0.json"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}
if (!file.exists(browser_data_path)) {
  stop("Browser data not found: ", browser_data_path)
}

raw_columns <- jsonlite::fromJSON(
  browser_data_path,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

data <- as.data.frame(raw_columns, stringsAsFactors = FALSE, optional = TRUE)

numeric_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288"
)

for (id in numeric_variables) data[[id]] <- as.numeric(data[[id]])

fit_fixture <- function(id, outcome, predictors, countries = NULL) {
  working <- data
  country_label <- "ALL"

  if (!is.null(countries)) {
    working <- working[working$B_COUNTRY_ALPHA %in% countries, , drop = FALSE]
    country_label <- countries
  }

  model_data <- working[, c(outcome, predictors), drop = FALSE]
  complete <- stats::complete.cases(model_data)
  complete_data <- model_data[complete, , drop = FALSE]

  formula_text <- paste(outcome, "~", paste(predictors, collapse = " + "))
  model <- stats::lm(stats::as.formula(formula_text), data = complete_data)
  model_summary <- summary(model)
  coefficients <- coef(model_summary)
  confidence <- stats::confint(model, level = 0.95)

  coefficient_records <- lapply(seq_len(nrow(coefficients)), function(i) {
    list(
      term = rownames(coefficients)[i],
      estimate = unname(coefficients[i, "Estimate"]),
      standardError = unname(coefficients[i, "Std. Error"]),
      tValue = unname(coefficients[i, "t value"]),
      pValue = unname(coefficients[i, "Pr(>|t|)"]),
      confidenceLower = unname(confidence[i, 1]),
      confidenceUpper = unname(confidence[i, 2])
    )
  })

  f <- unname(model_summary$fstatistic)

  list(
    id = id,
    outcome = outcome,
    predictors = predictors,
    countries = country_label,
    filteredRows = nrow(working),
    completeObservations = nrow(complete_data),
    excludedMissingAny = nrow(working) - nrow(complete_data),
    residualDegreesFreedom = stats::df.residual(model),
    rSquared = unname(model_summary$r.squared),
    adjustedRSquared = unname(model_summary$adj.r.squared),
    residualStandardError = unname(model_summary$sigma),
    fStatistic = f[[1]],
    numeratorDegreesFreedom = f[[2]],
    denominatorDegreesFreedom = f[[3]],
    coefficients = coefficient_records
  )
}

cases <- list(
  fit_fixture(
    "NZL_Q177_on_Q165_Q260_Q262",
    "Q177", c("Q165", "Q260", "Q262"), "NZL"
  ),
  fit_fixture(
    "NZL_Q184_on_five_predictors",
    "Q184", c("Q165", "Q167", "Q260", "Q262", "Q275"), "NZL"
  ),
  fit_fixture(
    "NZL_AUS_Q177_combined",
    "Q177", c("Q165", "Q260", "Q262"), c("NZL", "AUS")
  ),
  fit_fixture(
    "ALL_Q177_on_Q165_Q260_Q262",
    "Q177", c("Q165", "Q260", "Q262"), NULL
  ),
  fit_fixture(
    "NZL_BINARY_LINEAR_PROBABILITY",
    "Q167", c("Q165", "Q260", "Q262"), "NZL"
  )
)

fixtures <- list(
  fixtureVersion = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  model = "Multiple ordinary least-squares regression",
  rules = list(
    maximumPredictors = 5,
    missingData = "Complete cases across outcome and all predictors",
    intercept = TRUE,
    confidenceLevel = 0.95,
    referenceImplementation = "R stats::lm"
  ),
  cases = cases
)

jsonlite::write_json(
  fixtures, fixture_path,
  pretty = TRUE, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA
)

review_rows <- do.call(rbind, lapply(cases, function(case) {
  do.call(rbind, lapply(case$coefficients, function(coef) {
    data.frame(
      case = case$id,
      countries = paste(case$countries, collapse = ";"),
      outcome = case$outcome,
      predictors = paste(case$predictors, collapse = ";"),
      complete_observations = case$completeObservations,
      term = coef$term,
      estimate = coef$estimate,
      standard_error = coef$standardError,
      t_value = coef$tValue,
      p_value = coef$pValue,
      ci_lower = coef$confidenceLower,
      ci_upper = coef$confidenceUpper,
      r_squared = case$rSquared,
      adjusted_r_squared = case$adjustedRSquared,
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(
  review_rows,
  file.path(output_dir, "regression_reference_results.csv"),
  row.names = FALSE,
  na = ""
)

validation <- data.frame(
  check = c(
    "five_reference_cases_created",
    "five_predictor_case_created",
    "binary_linear_probability_case_created",
    "all_cases_have_at_least_10_complete_rows",
    "all_coefficient_estimates_finite",
    "fixture_json_created"
  ),
  passed = c(
    length(cases) == 5L,
    any(vapply(cases, function(x) length(x$predictors) == 5L, logical(1))),
    any(vapply(cases, function(x) x$outcome == "Q167", logical(1))),
    all(vapply(cases, function(x) x$completeObservations >= 10L, logical(1))),
    all(is.finite(review_rows$estimate)),
    file.exists(fixture_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "step13_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 13 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

summary_lines <- c(
  "WorldView Step 13 regression fixtures",
  paste("Reference cases:", length(cases)),
  paste("Coefficient rows:", nrow(review_rows)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Fixture output:", fixture_path)
)
writeLines(summary_lines, file.path(output_dir, "step13_summary.txt"))

cat("\nStep 13 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step13_output:\n")
cat("  step13_summary.txt\n")
cat("  step13_validation_checks.csv\n")
cat("  regression_reference_results.csv\n")
