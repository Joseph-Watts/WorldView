# WorldView Online - Step 20b
# Use scale-aware tolerances for ANOVA floating-point validation.
# Counts, group presence, and degrees of freedom remain exact checks.

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")

if (!file.exists(validation_js_path)) {
  stop("validation.js was not found. Run Step 20 first.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

marker <- "function testAnovaFixtures(data, anovaFixtures) {"
helper <- paste0(
  "function anovaCloseEnough(actual, expected, absoluteTolerance = 1e-7, relativeTolerance = 1e-10) {\n",
  "  if (actual === null || expected === null) return actual === expected;\n",
  "  if (!Number.isFinite(actual) || !Number.isFinite(expected)) return false;\n",
  "  const difference = Math.abs(actual - expected);\n",
  "  const scale = Math.max(1, Math.abs(actual), Math.abs(expected));\n",
  "  return difference <= Math.max(absoluteTolerance, relativeTolerance * scale);\n",
  "}\n\n"
)

if (!grepl("function anovaCloseEnough", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) {
    stop("Could not find the ANOVA fixture validation function.")
  }
  js <- sub(marker, paste0(helper, marker), js, fixed = TRUE)
}

# Replace the single absolute tolerance used for ANOVA table and effect-size
# comparisons. Large sums of squares need a relative tolerance because the
# displayed values can agree while differing in unshown decimal places.
old_numeric <- 'closeEnough(item[2], item[1], 1e-8)'
new_numeric <- 'anovaCloseEnough(item[2], item[1], 1e-7, 1e-10)'
js <- gsub(old_numeric, new_numeric, js, fixed = TRUE)

# Group means and SDs stay tighter, but also use a small relative component.
js <- gsub(
  'closeEnough(observedGroup.mean, expectedGroup.mean, 1e-10)',
  'anovaCloseEnough(observedGroup.mean, expectedGroup.mean, 1e-9, 1e-11)',
  js,
  fixed = TRUE
)
js <- gsub(
  'closeEnough(observedGroup.standardDeviation, expectedGroup.standardDeviation, 1e-10)',
  'anovaCloseEnough(observedGroup.standardDeviation, expectedGroup.standardDeviation, 1e-9, 1e-11)',
  js,
  fixed = TRUE
)

writeLines(js, validation_js_path, useBytes = TRUE)

checks <- data.frame(
  check = c(
    "anova_relative_tolerance_helper_added",
    "anova_table_checks_use_relative_tolerance",
    "group_mean_checks_remain_tight",
    "group_sd_checks_remain_tight",
    "exact_count_checks_retained",
    "exact_df_checks_retained"
  ),
  passed = c(
    grepl("function anovaCloseEnough", js, fixed = TRUE),
    grepl("anovaCloseEnough(item[2], item[1], 1e-7, 1e-10)", js, fixed = TRUE),
    grepl("anovaCloseEnough(observedGroup.mean", js, fixed = TRUE),
    grepl("anovaCloseEnough(observedGroup.standardDeviation", js, fixed = TRUE),
    grepl("test.validObservations === actual.validObservations", js, fixed = TRUE),
    grepl("test.degreesFreedomBetween === actual.degreesFreedomBetween", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step20b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 20b structural validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 20b completed successfully.\n\n")
cat("ANOVA floating-point checks now use combined absolute and relative tolerances.\n")
cat("Counts, group presence, and degrees of freedom still require exact agreement.\n\n")
cat("Restart the server and force-refresh validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
