# WorldView Online - Step 30
# Update browser validation expectations after expanding from 29 to 39 variables.

app_dir <- "worldview_static_app"
validation_path <- file.path(app_dir, "assets", "validation.js")
if (!file.exists(validation_path)) {
  stop("validation.js was not found: ", validation_path)
}

js <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")
original <- js

# Update only the two named count checks. This avoids changing unrelated uses of 29.
patterns <- list(
  c(
    '(addCheck\\(["`][^"`]*Manifest variable count[^"`]*["`][^;]*?,\\s*)29(\\s*,)',
    '\\139\\2'
  ),
  c(
    '(addCheck\\(["`][^"`]*Codebook variable count[^"`]*["`][^;]*?,\\s*)29(\\s*,)',
    '\\139\\2'
  )
)

for (item in patterns) {
  js <- gsub(item[[1]], item[[2]], js, perl = TRUE)
}

# Handle common object/property forms if the checks are written over multiple lines.
js <- gsub(
  '(Manifest variable count[\\s\\S]{0,180}?expected\\s*[:=]\\s*)29',
  '\\139',
  js,
  perl = TRUE
)
js <- gsub(
  '(Codebook variable count[\\s\\S]{0,180}?expected\\s*[:=]\\s*)29',
  '\\139',
  js,
  perl = TRUE
)

# Handle direct equality expressions associated with the named checks.
js <- gsub(
  '(Manifest variable count[\\s\\S]{0,220}?===\\s*)29',
  '\\139',
  js,
  perl = TRUE
)
js <- gsub(
  '(Codebook variable count[\\s\\S]{0,220}?===\\s*)29',
  '\\139',
  js,
  perl = TRUE
)

# Last-resort targeted line replacement. Only lines containing the exact check name
# are changed, so historical fixture counts elsewhere remain untouched.
lines <- strsplit(js, "\n", fixed = TRUE)[[1]]
manifest_lines <- grep("Manifest variable count", lines, fixed = TRUE)
codebook_lines <- grep("Codebook variable count", lines, fixed = TRUE)
for (i in unique(c(manifest_lines, codebook_lines))) {
  lines[[i]] <- gsub("\\b29\\b", "39", lines[[i]], perl = TRUE)
}
js <- paste(lines, collapse = "\n")

if (identical(js, original)) {
  stop(
    "The two count checks could not be located in validation.js. ",
    "No file was changed."
  )
}

writeLines(js, validation_path, useBytes = TRUE)

# Sanity checks: both labels must exist and neither labelled check may still expect 29.
updated_lines <- strsplit(js, "\n", fixed = TRUE)[[1]]
manifest_text <- paste(updated_lines[grep("Manifest variable count", updated_lines, fixed = TRUE)], collapse = " ")
codebook_text <- paste(updated_lines[grep("Codebook variable count", updated_lines, fixed = TRUE)], collapse = " ")

checks <- data.frame(
  check = c(
    "manifest_count_check_found",
    "codebook_count_check_found",
    "manifest_expected_count_updated_to_39",
    "codebook_expected_count_updated_to_39",
    "manifest_check_no_longer_contains_29",
    "codebook_check_no_longer_contains_29"
  ),
  passed = c(
    nzchar(manifest_text),
    nzchar(codebook_text),
    grepl("39", manifest_text, fixed = TRUE),
    grepl("39", codebook_text, fixed = TRUE),
    !grepl("\\b29\\b", manifest_text, perl = TRUE),
    !grepl("\\b29\\b", codebook_text, perl = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step30_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 30 validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 30 completed successfully.\n\n")
cat("Updated validation expectations:\n")
cat("  Manifest variable count: 39\n")
cat("  Codebook variable count: 39\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5 and reopen validation.html.\n")
