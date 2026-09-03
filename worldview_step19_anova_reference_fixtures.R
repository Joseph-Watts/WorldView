# WorldView Online - Step 19
# Generate R reference fixtures for flexible one-way ANOVA.

input_path <- file.path(
  "worldview_step3_output", "public_data",
  "worldview-browser-data-v1.0.0.json"
)
output_dir <- file.path(getwd(), "worldview_step19_output")
fixture_path <- file.path(
  "worldview_static_app", "data",
  "worldview-anova-fixtures-v1.0.0.json"
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

numeric_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288"
)
for (id in numeric_variables) data[[id]] <- as.numeric(data[[id]])

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

group_labels <- function(values, grouping_variable, country_names = NULL) {
  if (grouping_variable == "COUNTRY") return(as.character(country_names))
  if (grouping_variable %in% names(binary_labels)) {
    lookup <- binary_labels[[grouping_variable]]
    return(unname(lookup[as.character(values)]))
  }
  as.character(values)
}

fit_case <- function(id, outcome, grouping_variable, countries = NULL) {
  working <- data
  country_label <- "ALL"

  if (!is.null(countries)) {
    working <- working[
      working$B_COUNTRY_ALPHA %in% countries,
      , drop = FALSE
    ]
    country_label <- countries
  }

  if (grouping_variable == "COUNTRY") {
    raw_group <- working$B_COUNTRY_ALPHA
    displayed_group <- working$B_COUNTRY
  } else {
    raw_group <- working[[grouping_variable]]
    displayed_group <- group_labels(raw_group, grouping_variable)
  }

  model_data <- data.frame(
    outcome = working[[outcome]],
    group = factor(displayed_group),
    stringsAsFactors = FALSE
  )
  model_data <- model_data[complete.cases(model_data), , drop = FALSE]
  group_counts <- table(model_data$group)
  retained_levels <- names(group_counts[group_counts >= 2L])
  model_data <- model_data[
    as.character(model_data$group) %in% retained_levels,
    , drop = FALSE
  ]
  model_data$group <- droplevels(model_data$group)

  model <- stats::aov(outcome ~ group, data = model_data)
  table_result <- summary(model)[[1]]

  ss_between <- unname(table_result["group", "Sum Sq"])
  ss_within <- unname(table_result["Residuals", "Sum Sq"])
  df_between <- unname(table_result["group", "Df"])
  df_within <- unname(table_result["Residuals", "Df"])
  ms_between <- unname(table_result["group", "Mean Sq"])
  ms_within <- unname(table_result["Residuals", "Mean Sq"])
  f_statistic <- unname(table_result["group", "F value"])
  p_value <- unname(table_result["group", "Pr(>F)"])
  eta_squared <- ss_between / (ss_between + ss_within)
  omega_squared <- (
    ss_between - df_between * ms_within
  ) / (
    ss_between + ss_within + ms_within
  )

  split_values <- split(model_data$outcome, model_data$group)
  group_records <- lapply(names(split_values), function(group_name) {
    values <- split_values[[group_name]]
    list(
      group = group_name,
      validN = length(values),
      mean = mean(values),
      standardDeviation = stats::sd(values)
    )
  })

  list(
    id = id,
    outcome = outcome,
    groupingVariable = grouping_variable,
    countries = country_label,
    validObservations = nrow(model_data),
    groupCount = nlevels(model_data$group),
    groups = group_records,
    sumSquaresBetween = ss_between,
    sumSquaresWithin = ss_within,
    degreesFreedomBetween = df_between,
    degreesFreedomWithin = df_within,
    meanSquareBetween = ms_between,
    meanSquareWithin = ms_within,
    fStatistic = f_statistic,
    pValue = p_value,
    etaSquared = eta_squared,
    omegaSquared = omega_squared
  )
}

cases <- list(
  fit_case(
    "Q177_BY_COUNTRY_AUS_NZL",
    "Q177", "COUNTRY", c("AUS", "NZL")
  ),
  fit_case(
    "Q177_BY_SEX_NZL",
    "Q177", "Q260", "NZL"
  ),
  fit_case(
    "Q184_BY_SEX_AUS_NZL",
    "Q184", "Q260", c("AUS", "NZL")
  ),
  fit_case(
    "Q177_BY_DENOMINATION_NZL",
    "Q177", "Q289", "NZL"
  ),
  fit_case(
    "Q262_BY_BELIEF_IN_GOD_ALL",
    "Q262", "Q165", NULL
  )
)

fixtures <- list(
  fixtureVersion = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  model = "Flexible one-way ANOVA",
  rules = list(
    referenceImplementation = "R stats::aov",
    minimumValidPerGroup = 2,
    missingData = "Complete cases for outcome and grouping variable",
    supportedGroupingVariables = c(
      "COUNTRY", "Q165", "Q166", "Q167", "Q168",
      "Q260", "Q263", "Q289"
    )
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
  do.call(rbind, lapply(case$groups, function(group) {
    data.frame(
      case = case$id,
      outcome = case$outcome,
      grouping_variable = case$groupingVariable,
      countries = paste(case$countries, collapse = ";"),
      group = group$group,
      valid_n = group$validN,
      mean = group$mean,
      standard_deviation = group$standardDeviation,
      f_statistic = case$fStatistic,
      p_value = case$pValue,
      eta_squared = case$etaSquared,
      omega_squared = case$omegaSquared,
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(
  review,
  file.path(output_dir, "anova_reference_results.csv"),
  row.names = FALSE,
  na = ""
)

validation <- data.frame(
  check = c(
    "five_reference_cases_created",
    "country_grouping_case_created",
    "binary_grouping_case_created",
    "q289_grouping_case_created",
    "all_cases_have_two_or_more_groups",
    "all_f_statistics_finite",
    "fixture_created"
  ),
  passed = c(
    length(cases) == 5L,
    any(vapply(cases, function(x) x$groupingVariable == "COUNTRY", logical(1))),
    any(vapply(cases, function(x) x$groupingVariable == "Q260", logical(1))),
    any(vapply(cases, function(x) x$groupingVariable == "Q289", logical(1))),
    all(vapply(cases, function(x) x$groupCount >= 2L, logical(1))),
    all(vapply(cases, function(x) is.finite(x$fStatistic), logical(1))),
    file.exists(fixture_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(output_dir, "step19_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 19 validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

summary_lines <- c(
  "WorldView Step 19 ANOVA fixtures",
  paste("Reference cases:", length(cases)),
  paste("Group result rows:", nrow(review)),
  paste("Validation checks passed:", sum(validation$passed), "of", nrow(validation)),
  paste("Fixture output:", fixture_path)
)
writeLines(summary_lines, file.path(output_dir, "step19_summary.txt"))

cat("\nStep 19 completed successfully.\n\n")
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Share these files from worldview_step19_output:\n")
cat("  step19_summary.txt\n")
cat("  step19_validation_checks.csv\n")
cat("  anova_reference_results.csv\n")
