# WorldView Online - Step 28e
# Robustly imports Q18-Q26 and Q238 from WVS7_Individual.rds.
# Handles nested RDS objects and aligns records by a shared respondent ID.

app_dir <- "worldview_static_app"
data_path <- file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
index_path <- file.path(app_dir, "index.html")

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")
if (!all(file.exists(c(data_path, codebook_path, index_path)))) stop("Required WorldView files are missing.")

requested <- c(paste0("Q", 18:26), "Q238")
source_candidates <- c(
  file.path("WVS_Dataset", "WVS7_Individual.rds"),
  "WVS7_Individual.rds",
  file.path("worldview_step3_output", "private_data", "worldview_processed_full_private.rds"),
  "worldview_processed_full_private.rds"
)
source_candidates <- source_candidates[file.exists(source_candidates)]
if (!length(source_candidates)) stop("No WVS7_Individual.rds or private processed RDS was found in the project.")

browser <- jsonlite::fromJSON(data_path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
browser_df <- as.data.frame(browser, stringsAsFactors = FALSE, optional = TRUE)

# Recursively find data frames inside an RDS object.
collect_frames <- function(x, path = "root") {
  out <- list()
  if (is.data.frame(x)) {
    out[[path]] <- x
  } else if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) nms <- as.character(seq_along(x))
    for (i in seq_along(x)) {
      out <- c(out, collect_frames(x[[i]], paste0(path, "/", nms[[i]])))
    }
  }
  out
}

frames <- list()
for (path in source_candidates) {
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.null(obj)) {
    found <- collect_frames(obj, basename(path))
    frames <- c(frames, found)
  }
}
if (!length(frames)) stop("The RDS file was found, but no data frame could be extracted from it.")

scores <- vapply(frames, function(x) sum(requested %in% names(x)), integer(1))
frames <- frames[scores == max(scores)]
source_df <- frames[[1]]
source_name <- names(frames)[1]
missing_columns <- setdiff(requested, names(source_df))
if (length(missing_columns)) {
  stop("The best data frame in the RDS is missing: ", paste(missing_columns, collapse = ", "))
}

# Prefer exact row count; otherwise align by a shared unique respondent key.
if (nrow(source_df) == nrow(browser_df)) {
  aligned <- source_df
  alignment <- "exact row order and row count"
} else {
  preferred_ids <- c("S007", "RESPONDENT_ID", "respondent_id", "ID", "id", "CASEID", "caseid")
  shared <- intersect(preferred_ids, intersect(names(source_df), names(browser_df)))
  valid <- shared[vapply(shared, function(id) {
    !anyNA(source_df[[id]]) && !anyNA(browser_df[[id]]) &&
      !anyDuplicated(source_df[[id]]) && !anyDuplicated(browser_df[[id]])
  }, logical(1))]
  if (!length(valid)) {
    report <- c(
      paste("Browser rows:", nrow(browser_df)),
      paste("Source rows:", nrow(source_df)),
      paste("Shared columns:", paste(intersect(names(source_df), names(browser_df)), collapse = ", ")),
      "No shared unique respondent identifier was available."
    )
    writeLines(report, file.path(app_dir, "step28e_alignment_diagnostic.txt"))
    stop("The source row count differs and no shared unique respondent ID was found. See step28e_alignment_diagnostic.txt.")
  }
  id <- valid[[1]]
  match_index <- match(browser_df[[id]], source_df[[id]])
  if (anyNA(match_index)) stop("Some browser respondent IDs were not found in the source RDS.")
  aligned <- source_df[match_index, , drop = FALSE]
  alignment <- paste("matched by", id)
}

clean_wvs <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  if (is.factor(x)) x <- as.character(x)
  numeric_x <- suppressWarnings(as.numeric(x))
  nonmissing_original <- sum(!is.na(x))
  if (nonmissing_original == 0L) return(rep(NA_real_, length(x)))
  if (sum(!is.na(numeric_x)) >= nonmissing_original * 0.8) {
    numeric_x[numeric_x < 0] <- NA_real_
    return(unname(numeric_x))
  }
  x <- as.character(x)
  x[x %in% c("-1", "-2", "-3", "-4", "-5")] <- NA_character_
  unname(x)
}

labels <- c(
  Q18="One of my main goals in life has been to make my parents proud",
  Q19="Parents have a duty to do their best for their children",
  Q20="Respect and love for parents regardless of their qualities and faults",
  Q21="Men make better political leaders than women do",
  Q22="University is more important for a boy than for a girl",
  Q23="Men make better business executives than women do",
  Q24="Being a housewife is just as fulfilling as working for pay",
  Q25="When jobs are scarce, men should have more right to a job than women",
  Q26="It is a problem if women have more income than their husbands",
  Q238="Having a strong leader who does not have to bother with parliament and elections"
)

for (id in requested) browser[[id]] <- clean_wvs(aligned[[id]])

for (id in requested) {
  record <- list(
    id=id, displayName=unname(labels[[id]]), analysisType="ordinal",
    correlationEligible=TRUE,
    correlationRepresentation="Original WVS response coding; negative missing-value codes excluded",
    topic=if (id == "Q238") "Political culture and political regimes" else "Social values, norms and stereotypes"
  )
  existing <- which(vapply(codebook$variables, function(v) identical(v$id, id), logical(1)))
  if (length(existing)) codebook$variables[[existing[[1]]]] <- modifyList(codebook$variables[[existing[[1]]]], record)
  else codebook$variables[[length(codebook$variables)+1L]] <- record
}

jsonlite::write_json(browser, data_path, pretty=FALSE, auto_unbox=TRUE, na="null", null="null", digits=NA)
jsonlite::write_json(codebook, codebook_path, pretty=TRUE, auto_unbox=TRUE, na="null", null="null", digits=NA)

# Ensure all developers are credited even if the earlier script stopped.
html <- paste(readLines(index_path, warn=FALSE), collapse="\n")
credit_text <- "WorldView was developed by Joseph Watts, Nicki Cartlidge, and André De Vito."
if (grepl("WorldView was developed by Joseph Watts", html, fixed=TRUE)) {
  html <- gsub("WorldView was developed by Joseph Watts[^<]*", credit_text, html)
}
writeLines(html, index_path, useBytes=TRUE)

checks <- data.frame(
  check=c("all_variables_added","lengths_match_browser","negative_codes_removed","codebook_records_added","developer_credits_present"),
  passed=c(
    all(requested %in% names(browser)),
    all(vapply(requested, function(id) length(browser[[id]]) == nrow(browser_df), logical(1))),
    all(vapply(requested, function(id) all(is.na(browser[[id]]) | suppressWarnings(as.numeric(browser[[id]])) >= 0), logical(1))),
    all(requested %in% vapply(codebook$variables, function(v) v$id, character(1))),
    all(vapply(c("Joseph Watts","Nicki Cartlidge","André De Vito"), grepl, logical(1), x=html, fixed=TRUE))
  )
)
write.csv(checks, file.path(app_dir,"step28e_validation_checks.csv"), row.names=FALSE)
if (!all(checks$passed)) stop("Step 28e validation failed: ", paste(checks$check[!checks$passed], collapse=", "))
cat("\nStep 28e completed successfully.\nSource frame: ", source_name, "\nAlignment: ", alignment, "\n", sep="")
cat("Restart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
