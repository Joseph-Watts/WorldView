# WorldView Online - Step 28b
# Repairs the Step 28 structural validation failure after a partial Step 28 run.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
map_path <- file.path(app_dir, "assets", "world-map.js")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
helper_path <- file.path(app_dir, "assets", "step28-enhancements.js")
favicon_path <- file.path(app_dir, "assets", "worldview-favicon.svg")

required <- c(index_path, map_path, codebook_path, helper_path, favicon_path)
if (!all(file.exists(required))) {
  stop("Step 28 did not create all expected files. Missing: ",
       paste(required[!file.exists(required)], collapse = ", "))
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
map_js <- paste(readLines(map_path, warn = FALSE), collapse = "\n")
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)

# Step 28 first added the hover instruction to the status message and then a
# broad replacement removed it again. Restore it only in the status message.
map_js <- gsub(
  '`${rows.length} WorldView countries mapped.`',
  '`${rows.length} WorldView countries mapped. Hover over a country to see its value and valid sample size.`',
  map_js,
  fixed = TRUE
)
map_js <- gsub(
  '`${rows.length} WorldView countries mapped. Hover over a country for its value.`',
  '`${rows.length} WorldView countries mapped. Hover over a country to see its value and valid sample size.`',
  map_js,
  fixed = TRUE
)

# Ensure the figure caption itself does not repeat the hover instruction.
map_js <- gsub(
  ' Hover over a country to see its value and valid sample size.',
  '',
  map_js,
  fixed = TRUE
)
# The previous line also affects the status message, so restore that exact
# status string once more after caption cleanup.
map_js <- gsub(
  '`${rows.length} WorldView countries mapped.`',
  '`${rows.length} WorldView countries mapped. Hover over a country to see its value and valid sample size.`',
  map_js,
  fixed = TRUE
)
writeLines(map_js, map_path, useBytes = TRUE)

# Verify topics are genuinely present and non-empty.
topics_ok <- all(vapply(
  codebook$variables,
  function(variable) {
    !is.null(variable$topic) &&
      length(variable$topic) == 1L &&
      nzchar(variable$topic)
  },
  logical(1)
))

checks <- data.frame(
  check = c(
    "map_hover_instruction_above_map",
    "map_caption_does_not_repeat_hover_instruction",
    "correlation_method_selector_present",
    "analysis_guide_present",
    "custom_favicon_present",
    "codebook_topics_nonempty",
    "topic_grouping_helper_present"
  ),
  passed = c(
    grepl(
      "WorldView countries mapped. Hover over a country to see its value and valid sample size.",
      map_js,
      fixed = TRUE
    ),
    !grepl(
      "Country shading shows the ${measureLabel} for ${variable.id}: ${variable.displayName}. Hover over a country",
      map_js,
      fixed = TRUE
    ),
    grepl('id="correlation-method"', html, fixed = TRUE),
    grepl('id="analysis-guide"', html, fixed = TRUE),
    file.exists(favicon_path),
    topics_ok,
    file.exists(helper_path)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step28b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  failed <- checks$check[!checks$passed]
  cat("\nStep 28b checks that still failed:\n")
  cat(paste0("  - ", failed, collapse = "\n"), "\n")
  stop("Step 28b validation failed. See worldview_static_app/step28b_validation_checks.csv")
}

cat("\nStep 28b completed successfully.\n\n")
cat("All seven repaired structural checks passed.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
