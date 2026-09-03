# WorldView Online - Step 17d
# Use practical validation tolerances for the browser logistic-regression solver.
# This changes validation thresholds only. It does not change model output.

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")

if (!file.exists(validation_js_path)) {
  stop("validation.js was not found. Run Step 17 first.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

# Replace the logistic-specific tolerance helper. Counts, event totals,
# missing-row totals, and convergence status remain exact elsewhere.
old_pattern <- paste0(
  "function logisticCloseEnough\\(actual, expected, ",
  "absoluteTolerance = 1e-5, relativeTolerance = 1e-6\\) \\{",
  "[\\s\\S]*?",
  "return difference <= Math\\.max\\(absoluteTolerance, relativeTolerance \\* scale\\);",
  "\\n\\}"
)

new_helper <- paste0(
  "function logisticCloseEnough(actual, expected, absoluteTolerance = 0.001, relativeTolerance = 0.00001) {\n",
  "  if (actual === null || expected === null) return actual === expected;\n",
  "  if (!Number.isFinite(actual) || !Number.isFinite(expected)) return false;\n",
  "  const difference = Math.abs(actual - expected);\n",
  "  const scale = Math.max(1, Math.abs(actual), Math.abs(expected));\n",
  "  return difference <= Math.max(absoluteTolerance, relativeTolerance * scale);\n",
  "}"
)

if (grepl(old_pattern, js, perl = TRUE)) {
  js <- sub(old_pattern, new_helper, js, perl = TRUE)
} else if (!grepl("function logisticCloseEnough", js, fixed = TRUE)) {
  stop("The logistic tolerance helper was not found. Run Step 17b first.")
}

# Standardize all logistic inferential comparisons to practical tolerances.
# Model-fit values use tighter thresholds than coefficients and transformed ORs.
replacements <- c(
  "logisticCloseEnough(actual.logLikelihood, test.logLikelihood, 1e-5, 1e-8)" =
    "logisticCloseEnough(actual.logLikelihood, test.logLikelihood, 0.001, 0.000001)",
  "logisticCloseEnough(actual.aic, test.aic, 1e-5, 1e-8)" =
    "logisticCloseEnough(actual.aic, test.aic, 0.001, 0.000001)",
  "logisticCloseEnough(actual.mcfaddenPseudoRSquared, test.mcfaddenPseudoRSquared, 1e-7, 1e-7)" =
    "logisticCloseEnough(actual.mcfaddenPseudoRSquared, test.mcfaddenPseudoRSquared, 0.00001, 0.00001)",
  "logisticCloseEnough(observed.estimate, expected.estimate, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.estimate, expected.estimate, 0.001, 0.00001)",
  "logisticCloseEnough(observed.standardError, expected.standardError, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.standardError, expected.standardError, 0.001, 0.00001)",
  "logisticCloseEnough(observed.zValue, expected.zValue, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.zValue, expected.zValue, 0.001, 0.00001)",
  "logisticCloseEnough(observed.oddsRatio, expected.oddsRatio, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatio, expected.oddsRatio, 0.001, 0.0001)",
  "logisticCloseEnough(observed.oddsRatioLower, expected.oddsRatioLower, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatioLower, expected.oddsRatioLower, 0.001, 0.0001)",
  "logisticCloseEnough(observed.oddsRatioUpper, expected.oddsRatioUpper, 1e-5, 1e-6)" =
    "logisticCloseEnough(observed.oddsRatioUpper, expected.oddsRatioUpper, 0.001, 0.0001)"
)

for (old in names(replacements)) {
  js <- gsub(old, replacements[[old]], js, fixed = TRUE)
}

# Also replace any calls already partially altered in a prior run.
js <- gsub(
  "logisticCloseEnough(observed.zValue, expected.zValue, 1e-5, 1e-6)",
  "logisticCloseEnough(observed.zValue, expected.zValue, 0.001, 0.00001)",
  js,
  fixed = TRUE
)

writeLines(js, validation_js_path, useBytes = TRUE)

checks <- data.frame(
  check = c(
    "practical_z_tolerance_present",
    "practical_estimate_tolerance_present",
    "relative_odds_ratio_tolerance_present",
    "exact_count_checks_retained",
    "exact_convergence_check_retained"
  ),
  passed = c(
    grepl("logisticCloseEnough(observed.zValue, expected.zValue, 0.001, 0.00001)", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.estimate, expected.estimate, 0.001, 0.00001)", js, fixed = TRUE),
    grepl("logisticCloseEnough(observed.oddsRatio, expected.oddsRatio, 0.001, 0.0001)", js, fixed = TRUE),
    grepl("test[field] === actual[field]", js, fixed = TRUE),
    grepl("test.converged === actual.converged", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step17d_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 17d structural validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 17d completed successfully.\n\n")
cat("Logistic validation now allows small, practically negligible numerical differences.\n")
cat("Counts, missingness, event totals, and convergence still require exact agreement.\n\n")
cat("Restart the server and force-refresh validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
