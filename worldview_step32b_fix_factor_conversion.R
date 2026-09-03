# WorldView Online - Step 32b
# Correct Q18-Q26 and Q238 by preserving ordered-factor integer codes.

app_dir <- "worldview_static_app"
data_path <- file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
source_candidates <- c(file.path("WVS_Dataset", "WVS7_Individual.rds"), "WVS7_Individual.rds")
source_path <- source_candidates[file.exists(source_candidates)][1]

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")
if (!file.exists(data_path)) stop("Browser JSON was not found.")
if (!file.exists(codebook_path)) stop("Codebook JSON was not found.")
if (!length(source_path) || is.na(source_path)) stop("WVS7_Individual.rds was not found.")

requested <- c(paste0("Q", 18:26), "Q238")
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
if (length(missing)) stop("Source frame is missing: ", paste(missing, collapse = ", "))

browser <- jsonlite::fromJSON(data_path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
browser_df <- as.data.frame(browser, stringsAsFactors = FALSE, optional = TRUE)

# Reproduce the original Step 3 sample exactly.
source$.source_row <- seq_len(nrow(source))
set.seed(382L)
groups <- split(source, source$B_COUNTRY_ALPHA, drop = TRUE)
sampled <- do.call(rbind, lapply(groups, function(country_data) {
  country_data <- country_data[order(country_data$.source_row), , drop = FALSE]
  selected <- sample.int(nrow(country_data), min(nrow(country_data), 1000L), replace = FALSE)
  country_data[selected, , drop = FALSE]
}))
row.names(sampled) <- NULL
sampled <- sampled[order(sampled$B_COUNTRY_ALPHA, sampled$.source_row), , drop = FALSE]
sampled$WORLDVIEW_ID <- ave(
  sampled$.source_row,
  sampled$B_COUNTRY_ALPHA,
  FUN = function(x) sprintf("%04d", seq_along(x))
)
sampled$WORLDVIEW_ID <- paste0("WV7-", sampled$B_COUNTRY_ALPHA, "-", sampled$WORLDVIEW_ID)

match_index <- match(browser_df$WORLDVIEW_ID, sampled$WORLDVIEW_ID)
if (anyNA(match_index)) stop("Not all browser IDs matched the reproduced sample. No data were changed.")
aligned <- sampled[match_index, , drop = FALSE]
if (!identical(as.character(aligned$WORLDVIEW_ID), as.character(browser_df$WORLDVIEW_ID))) {
  stop("WORLDVIEW_ID order did not match. No data were changed.")
}

# Critical fix: as.numeric() or as.integer() must be applied directly to factors.
# Converting these factors to character first produces labels such as 'Mentioned'
# and therefore turns nearly every response into NA.
encode_ordered <- function(x) {
  if (is.factor(x)) return(unname(as.integer(x)))
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  values <- suppressWarnings(as.numeric(x))
  values[values < 0] <- NA_real_
  unname(values)
}

before <- vapply(requested, function(id) {
  if (id %in% names(browser)) sum(!is.na(browser[[id]])) else 0L
}, integer(1))
for (id in requested) browser[[id]] <- encode_ordered(aligned[[id]])
after <- vapply(requested, function(id) sum(!is.na(browser[[id]])), integer(1))

if (any(after < 10L)) stop("Factor-safe conversion unexpectedly produced fewer than 10 values for: ", paste(requested[after < 10L], collapse = ", "))
if (length(unique(vapply(browser, length, integer(1)))) != 1L) stop("Browser columns have unequal lengths.")

backup_path <- file.path(app_dir, "data", "worldview-browser-data-before-step32b.json")
if (!file.exists(backup_path)) file.copy(data_path, backup_path, overwrite = FALSE)
jsonlite::write_json(browser, data_path, pretty = FALSE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)

# Correct question descriptions and response labels.
question_labels <- c(
  Q18 = "Important child quality: independence",
  Q19 = "Important child quality: hard work",
  Q20 = "Important child quality: feeling of responsibility",
  Q21 = "Important child quality: imagination",
  Q22 = "Important child quality: tolerance and respect for other people",
  Q23 = "Important child quality: thrift, saving money and things",
  Q24 = "Important child quality: determination and perseverance",
  Q25 = "Important child quality: religious faith",
  Q26 = "Important child quality: unselfishness",
  Q238 = "Having a strong leader who does not have to bother with parliament and elections"
)
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
for (id in requested) {
  pos <- which(vapply(codebook$variables, function(v) identical(v$id, id), logical(1)))
  if (!length(pos)) next
  i <- pos[[1]]
  codebook$variables[[i]]$displayName <- unname(question_labels[[id]])
  codebook$variables[[i]]$analysisType <- "ordinal"
  codebook$variables[[i]]$topic <- if (id == "Q238") "Political culture and political regimes" else "Social values, norms and stereotypes"
  codebook$variables[[i]]$validCountInFullProcessedData <- after[[id]]
  codebook$variables[[i]]$missingCountInFullProcessedData <- length(browser[[id]]) - after[[id]]
  source_levels <- levels(aligned[[id]])
  codebook$variables[[i]]$levels <- lapply(seq_along(source_levels), function(j) {
    list(value = j, label = source_levels[[j]], displayOrder = j)
  })
}
jsonlite::write_json(codebook, codebook_path, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)

if (dir.exists("worldview_deployment")) {
  file.copy(data_path, file.path("worldview_deployment", "data", basename(data_path)), overwrite = TRUE)
  file.copy(codebook_path, file.path("worldview_deployment", "data", basename(codebook_path)), overwrite = TRUE)
}

report <- data.frame(variable = requested, valid_before = before, valid_after = after, missing_after = nrow(browser_df) - after)
write.csv(report, file.path(app_dir, "step32b_new_variable_valid_counts.csv"), row.names = FALSE)
checks <- data.frame(
  check = c("all_ids_matched", "factor_codes_preserved", "all_variables_have_10_valid", "all_lengths_equal", "backup_created"),
  passed = c(!anyNA(match_index), all(after > before), all(after >= 10L), length(unique(vapply(browser, length, integer(1)))) == 1L, file.exists(backup_path))
)
write.csv(checks, file.path(app_dir, "step32b_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Step 32b validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))

cat("\nStep 32b completed successfully.\n\n")
print(report, row.names = FALSE)
cat("\nRestart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then test Q18 in Summary statistics and Bar chart.\n")
