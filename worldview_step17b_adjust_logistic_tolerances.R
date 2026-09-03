# WorldView Online - Step 17b
# Adjust logistic validation tolerances to allow minor numerical differences
# between R glm() and the browser IRLS implementation.

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")

if (!file.exists(validation_js_path)) {
  stop("validation.js was not found. Run Step 17 first.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

# Add a logistic-specific comparison that combines absolute and relative
# tolerances. This is more appropriate for odds ratios, which may be large.
marker <- "function testLogisticFixtures(data, logisticFixtures) {"
helper <- 'function logisticCloseEnough(actual, expected, absoluteTolerance = 1e-5, relativeTolerance = 1e-6) {
  if (actual === null || expected === null) return actual === expected;
  if (!Number.isFinite(actual) || !Number.isFinite(expected)) return false;
  const difference = Math.abs(actual - expected);
  const scale = Math.max(1, Math.abs(actual), Math.abs(expected));
  return difference <= Math.max(absoluteTolerance, relativeTolerance * scale);
}

'

if (!grepl("function logisticCloseEnough", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) {
    stop("Could not find the logistic validation function.")
  }
  js <- sub(marker, paste0(helper, marker), js, fixed = TRUE)
}

# Replace only the logistic comparisons. Counts and convergence remain exact.
replacements <- c(
  "closeEnough(actual.logLikelihood, test.logLikelihood, 1e-6)" =
    "logisticCloseEnough(actual.logLikelihood, test.logLikelihood, 1e-5, 1e-8)",
  "closeEnough(actual.aic, test.aic, 1e-6)" =
    "logisticCloseEnough(actual.aic, test.aic, 1e-5, 1e-8)",
  "closeEnough(actual.mcfaddenPseudoRSquared, test.mcfaddenPseudoRSquared, 1e-8)" =
    "logisticCloseEnough(actual.mcfaddenPseudoRSquared, test.mcfaddenPseudoRSquared, 1e-7, 1e-7)",
  "closeEnough(observed.estimate, expected.estimate, 1e-7)" =
    "logisticCloseEnough(observed.estimate, expected.estimate, 1e-5, 1e-6)",
  "closeEnough(observed.standardError, expected.standardError, 1e-7)" =
    "logisticCloseEnough(observed.standardError, expected.standardError, 1e-5, 1e-6)",
  "closeEnough(observed.zValue, expected.zValue, 1e-6)" =
    "logisticCloseEnough(observed.zValue, expected.zValue, 1e-5, 1e-6)",
  "closeEnough(observed.oddsRatio, expected.oddsRatio, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatio, expected.oddsRatio, 1e-5, 1e-6)",
  "closeEnough(observed.oddsRatioLower, expected.oddsRatioLower, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatioLower, expected.oddsRatioLower, 1e-5, 1e-6)",
  "closeEnough(observed.oddsRatioUpper, expected.oddsRatioUpper, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatioUpper, expected.oddsRatioUpper, 1e-5, 1e-6)"
)

for (old in names(replacements)) {
  js <- gsub(old, replacements[[old]], js, fixed = TRUE)
}

writeLines(js, validation_js_path, useBytes = TRUE)

validation <- data.frame(
  check = c(
    "logistic_comparison_helper_added",
    "z_tolerance_updated",
    "estimate_tolerance_updated",
    "standard_error_tolerance_updated",
    "odds_ratio_relative_tolerance_added",
    "exact_count_checks_retained",
    "exact_convergence_check_retained"
  ),
  passed = c(
    grepl("function logisticCloseEnough", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.zValue", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.estimate", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.standardError", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.oddsRatio", js, fixed = TRUE),
    grepl("test[field] === actual[field]", js, fixed = TRUE),
    grepl("test.converged === actual.converged", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step17b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 17b validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 17b completed successfully.\n\n")
cat("Logistic validation now uses combined absolute and relative tolerances.\n")
cat("Counts, event totals, missing-row totals, and convergence remain exact.\n\n")
cat("Restart the server and force-refresh validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5 in the browser.\n")
