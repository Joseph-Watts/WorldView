# WorldView Online - Step 16
# Generate R glm(binomial) reference fixtures for binary logistic regression.

input_path <- file.path("worldview_step3_output", "public_data", "worldview-browser-data-v1.0.0.json")
output_dir <- file.path(getwd(), "worldview_step16_output")
fixture_path <- file.path("worldview_static_app", "data", "worldview-logistic-fixtures-v1.0.0.json")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite')")
if (!file.exists(input_path)) stop("Browser data not found: ", input_path)

columns <- jsonlite::fromJSON(input_path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
data <- as.data.frame(columns, stringsAsFactors = FALSE, optional = TRUE)

numeric_variables <- c("Q165","Q166","Q167","Q168","Q177","Q178","Q179","Q180","Q181","Q182","Q183","Q184","Q185","Q186","Q187","Q188","Q189","Q190","Q191","Q192","Q193","Q194","Q195","Q260","Q262","Q263","Q275","Q288")
for (id in numeric_variables) data[[id]] <- as.numeric(data[[id]])

fit_case <- function(id, outcome, predictors, countries = NULL) {
  working <- data
  country_label <- "ALL"
  if (!is.null(countries)) {
    working <- working[working$B_COUNTRY_ALPHA %in% countries, , drop = FALSE]
    country_label <- countries
  }
  model_data <- working[, c(outcome, predictors), drop = FALSE]
  complete <- complete.cases(model_data)
  model_data <- model_data[complete, , drop = FALSE]
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = model_data, family = binomial(link = "logit"), control = glm.control(maxit = 50, epsilon = 1e-8))
  sm <- summary(model)
  co <- coef(sm)
  coefficient_records <- lapply(seq_len(nrow(co)), function(i) list(
    term = rownames(co)[i],
    estimate = unname(co[i, "Estimate"]),
    standardError = unname(co[i, "Std. Error"]),
    zValue = unname(co[i, "z value"]),
    pValue = unname(co[i, "Pr(>|z|)"]),
    oddsRatio = exp(unname(co[i, "Estimate"])),
    oddsRatioLower = exp(unname(co[i, "Estimate"] - 1.959963984540054 * co[i, "Std. Error"])),
    oddsRatioUpper = exp(unname(co[i, "Estimate"] + 1.959963984540054 * co[i, "Std. Error"]))
  ))
  null_model <- glm(model_data[[outcome]] ~ 1, family = binomial(link = "logit"))
  list(
    id = id,
    outcome = outcome,
    predictors = predictors,
    countries = country_label,
    filteredRows = nrow(working),
    completeObservations = nrow(model_data),
    excludedMissingAny = nrow(working) - nrow(model_data),
    events = sum(model_data[[outcome]] == 1),
    nonEvents = sum(model_data[[outcome]] == 0),
    converged = isTRUE(model$converged),
    iterations = model$iter,
    logLikelihood = unname(as.numeric(logLik(model))),
    aic = AIC(model),
    mcfaddenPseudoRSquared = 1 - as.numeric(logLik(model)) / as.numeric(logLik(null_model)),
    coefficients = coefficient_records
  )
}

cases <- list(
  fit_case("NZL_Q167_Q165_Q260_Q262", "Q167", c("Q165","Q260","Q262"), "NZL"),
  fit_case("NZL_Q165_FIVE_PREDICTORS", "Q165", c("Q167","Q177","Q260","Q262","Q275"), "NZL"),
  fit_case("NZL_AUS_Q167_COMBINED", "Q167", c("Q165","Q260","Q262"), c("NZL","AUS")),
  fit_case("ALL_Q167_Q165_Q260_Q262", "Q167", c("Q165","Q260","Q262"), NULL),
  fit_case("NZL_Q263_Q165_Q260_Q262", "Q263", c("Q165","Q260","Q262"), "NZL")
)

fixtures <- list(
  fixtureVersion = "1.0.0",
  dataVersion = "WV7-WORLDVIEW-1.0.0",
  model = "Binary logistic regression",
  rules = list(referenceImplementation = "R stats::glm family binomial logit", maximumPredictors = 5, missingData = "Complete cases"),
  cases = cases
)
jsonlite::write_json(fixtures, fixture_path, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)

review <- do.call(rbind, lapply(cases, function(case) do.call(rbind, lapply(case$coefficients, function(coef) data.frame(
  case = case$id, countries = paste(case$countries, collapse = ";"), outcome = case$outcome,
  predictors = paste(case$predictors, collapse = ";"), complete_observations = case$completeObservations,
  events = case$events, term = coef$term, estimate = coef$estimate, standard_error = coef$standardError,
  z_value = coef$zValue, p_value = coef$pValue, odds_ratio = coef$oddsRatio,
  or_ci_lower = coef$oddsRatioLower, or_ci_upper = coef$oddsRatioUpper,
  aic = case$aic, pseudo_r_squared = case$mcfaddenPseudoRSquared, converged = case$converged
)))))
write.csv(review, file.path(output_dir, "logistic_reference_results.csv"), row.names = FALSE, na = "")

validation <- data.frame(
  check = c("five_cases_created","five_predictor_case_created","all_models_converged","all_events_binary","all_estimates_finite","fixture_created"),
  passed = c(length(cases)==5L, any(vapply(cases,function(x)length(x$predictors)==5L,logical(1))), all(vapply(cases,function(x)x$converged,logical(1))), all(vapply(cases,function(x)x$events+x$nonEvents==x$completeObservations,logical(1))), all(is.finite(review$estimate)), file.exists(fixture_path))
)
write.csv(validation, file.path(output_dir, "step16_validation_checks.csv"), row.names = FALSE)
if (!all(validation$passed)) stop("Step 16 validation failed: ", paste(validation$check[!validation$passed], collapse = ", "))
summary_lines <- c("WorldView Step 16 logistic fixtures", paste("Reference cases:",length(cases)), paste("Coefficient rows:",nrow(review)), paste("Validation checks passed:",sum(validation$passed),"of",nrow(validation)), paste("Fixture output:",fixture_path))
writeLines(summary_lines, file.path(output_dir, "step16_summary.txt"))
cat("\nStep 16 completed successfully.\n\n", paste(summary_lines, collapse="\n"), "\n\nShare step16_summary.txt, step16_validation_checks.csv, and logistic_reference_results.csv\n", sep="")
