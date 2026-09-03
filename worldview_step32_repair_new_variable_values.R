# WorldView Online - Step 32
# Repair Q18-Q26 and Q238 by reproducing the original Step 3 sample directly
# from WVS7_Individual.rds and joining on deterministic WORLDVIEW_ID values.

app_dir <- "worldview_static_app"
data_path <- file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
source_candidates <- c(file.path("WVS_Dataset", "WVS7_Individual.rds"), "WVS7_Individual.rds")
source_path <- source_candidates[file.exists(source_candidates)][1]

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")
if (!file.exists(data_path)) stop("Browser JSON was not found: ", data_path)
if (!file.exists(codebook_path)) stop("Codebook JSON was not found: ", codebook_path)
if (!length(source_path) || is.na(source_path)) stop("WVS7_Individual.rds was not found.")

requested <- c(paste0("Q", 18:26), "Q238")
maximum_per_country <- 1000L
sampling_seed <- 382L

browser <- jsonlite::fromJSON(
  data_path,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
browser_df <- as.data.frame(browser, stringsAsFactors = FALSE, optional = TRUE)
source_object <- readRDS(source_path)

collect_frames <- function(x) {
  if (is.data.frame(x)) return(list(x))
  if (!is.list(x)) return(list())
  unlist(lapply(x, collect_frames), recursive = FALSE)
}
frames <- collect_frames(source_object)
if (!length(frames)) stop("No data frame was found inside the source RDS.")
scores <- vapply(frames, function(x) sum(c("B_COUNTRY_ALPHA", requested) %in% names(x)), integer(1))
source <- frames[[which.max(scores)]]
missing <- setdiff(c("B_COUNTRY_ALPHA", requested), names(source))
if (length(missing)) stop("The source frame is missing: ", paste(missing, collapse = ", "))

# Reproduce Step 3's exact sampling operations.
source$.source_row <- seq_len(nrow(source))
set.seed(sampling_seed)
groups <- split(source, source$B_COUNTRY_ALPHA, drop = TRUE)
sampled_groups <- lapply(groups, function(country_data) {
  country_data <- country_data[order(country_data$.source_row), , drop = FALSE]
  retain_n <- min(nrow(country_data), maximum_per_country)
  selected <- sample.int(nrow(country_data), size = retain_n, replace = FALSE)
  country_data[selected, , drop = FALSE]
})
sampled <- do.call(rbind, sampled_groups)
row.names(sampled) <- NULL
sampled <- sampled[order(sampled$B_COUNTRY_ALPHA, sampled$.source_row), , drop = FALSE]
sampled$WORLDVIEW_ID <- ave(
  sampled$.source_row,
  sampled$B_COUNTRY_ALPHA,
  FUN = function(x) sprintf("%04d", seq_along(x))
)
sampled$WORLDVIEW_ID <- paste0("WV7-", sampled$B_COUNTRY_ALPHA, "-", sampled$WORLDVIEW_ID)

# Require an exact one-to-one deterministic ID match before modifying runtime data.
if (anyDuplicated(sampled$WORLDVIEW_ID)) stop("Reproduced sample IDs are not unique.
")
if (anyDuplicated(browser_df$WORLDVIEW_ID)) stop("Browser WORLDVIEW_ID values are not unique.")
match_index <- match(browser_df$WORLDVIEW_ID, sampled$WORLDVIEW_ID)
if (anyNA(match_index)) {
  stop(sum(is.na(match_index)), " browser IDs were not found in the reproduced sample. No data were changed.")
}
aligned <- sampled[match_index, , drop = FALSE]
if (!identical(as.character(aligned$WORLDVIEW_ID), as.character(browser_df$WORLDVIEW_ID))) {
  stop("WORLDVIEW_ID alignment verification failed. No data were changed.")
}

clean_numeric_codes <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  if (is.factor(x)) x <- as.character(x)
  values <- suppressWarnings(as.numeric(x))
  values[values < 0] <- NA_real_
  unname(values)
}

before_valid <- vapply(requested, function(id) {
  if (!id %in% names(browser)) return(0L)
  sum(!is.na(browser[[id]]))
}, integer(1))

for (id in requested) browser[[id]] <- clean_numeric_codes(aligned[[id]])
after_valid <- vapply(requested, function(id) sum(!is.na(browser[[id]])), integer(1))

if (any(after_valid < 10L)) {
  stop(
    "The reproduced source still has fewer than 10 valid values for: ",
    paste(requested[after_valid < 10L], collapse = ", "),
    ". No JSON was written."
  )
}

# Back up and write the repaired runtime JSON.
backup_path <- file.path(app_dir, "data", "worldview-browser-data-before-step32.json")
if (!file.exists(backup_path)) file.copy(data_path, backup_path, overwrite = FALSE)
jsonlite::write_json(
  browser, data_path,
  pretty = FALSE, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA
)

# Update valid/missing counts in codebook records when those fields are present.
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
for (id in requested) {
  position <- which(vapply(codebook$variables, function(v) identical(v$id, id), logical(1)))
  if (!length(position)) next
  i <- position[[1]]
  codebook$variables[[i]]$validCountInFullProcessedData <- after_valid[[id]]
  codebook$variables[[i]]$missingCountInFullProcessedData <- length(browser[[id]]) - after_valid[[id]]
}
jsonlite::write_json(
  codebook, codebook_path,
  pretty = TRUE, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA
)

# Keep the current deployment copy synchronized if it exists.
if (dir.exists("worldview_deployment")) {
  file.copy(data_path, file.path("worldview_deployment", "data", basename(data_path)), overwrite = TRUE)
  file.copy(codebook_path, file.path("worldview_deployment", "data", basename(codebook_path)), overwrite = TRUE)
}

report <- data.frame(
  variable = requested,
  valid_before = before_valid,
  valid_after = after_valid,
  missing_after = nrow(browser_df) - after_valid,
  stringsAsFactors = FALSE
)
write.csv(report, file.path(app_dir, "step32_new_variable_valid_counts.csv"), row.names = FALSE)

checks <- data.frame(
  check = c(
    "all_browser_ids_matched",
    "all_requested_variables_present",
    "all_requested_variables_have_at_least_10_valid",
    "all_column_lengths_match",
    "backup_created"
  ),
  passed = c(
    !anyNA(match_index),
    all(requested %in% names(browser)),
    all(after_valid >= 10L),
    length(unique(vapply(browser, length, integer(1)))) == 1L,
    file.exists(backup_path)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step32_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Step 32 validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))

cat("\nStep 32 completed successfully.\n\n")
print(report, row.names = FALSE)
cat("\nRestart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5 and test Q18 in Summary statistics and Bar chart.\n")
