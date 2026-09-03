# WorldView Online - Step 28f
# Aligns the reduced browser data to the full WVS source using an exact
# multi-variable response signature when no respondent ID is available.

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
if (!length(source_candidates)) stop("No source RDS was found.")

browser <- jsonlite::fromJSON(data_path, simplifyVector=TRUE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE)
browser_df <- as.data.frame(browser, stringsAsFactors=FALSE, optional=TRUE)
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector=FALSE)

collect_frames <- function(x, path="root") {
  out <- list()
  if (is.data.frame(x)) out[[path]] <- x
  else if (is.list(x)) {
    nms <- names(x); if (is.null(nms)) nms <- as.character(seq_along(x))
    for (i in seq_along(x)) out <- c(out, collect_frames(x[[i]], paste0(path,"/",nms[i])))
  }
  out
}
frames <- list()
for (path in source_candidates) {
  object <- tryCatch(readRDS(path), error=function(e) NULL)
  if (!is.null(object)) frames <- c(frames, collect_frames(object, basename(path)))
}
if (!length(frames)) stop("No data frame was found inside the source RDS.")
scores <- vapply(frames, function(x) sum(requested %in% names(x)), integer(1))
source_df <- frames[[which.max(scores)]]
source_name <- names(frames)[which.max(scores)]
if (!all(requested %in% names(source_df))) stop("The selected source frame does not contain all requested variables.")

# Normalize values identically before constructing signatures.
normalise <- function(x) {
  if (inherits(x,"haven_labelled")) x <- unclass(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    x[x < 0] <- NA_real_
    return(ifelse(is.na(x), "<NA>", format(x, scientific=FALSE, trim=TRUE, digits=15)))
  }
  x <- trimws(as.character(x))
  x[x %in% c("","-1","-2","-3","-4","-5","NA","NaN")] <- NA_character_
  ifelse(is.na(x), "<NA>", x)
}

# Prefer stable survey and country fields, then add other shared variables until
# at least 99.9% of browser rows have a unique signature.
shared <- intersect(names(browser_df), names(source_df))
shared <- setdiff(shared, requested)
preferred <- c("B_COUNTRY_ALPHA","B_COUNTRY","Q260","Q262","Q263","Q165","Q166","Q167","Q168","Q177","Q178","Q179","Q180","Q181","Q182","Q183","Q184","Q185","Q186","Q187","Q188","Q189","Q190","Q191","Q192","Q193","Q194","Q195","Q275","Q288","Q289")
columns <- c(intersect(preferred,shared), setdiff(shared,preferred))
if (length(columns) < 3) stop("Too few shared variables are available for safe signature alignment.")

make_signature <- function(df, cols) {
  values <- lapply(df[cols], normalise)
  do.call(paste, c(values, sep="\u001f"))
}

chosen <- character(); browser_sig <- NULL; source_sig <- NULL
for (column in columns) {
  chosen <- c(chosen,column)
  if (length(chosen) < 5) next
  browser_sig <- make_signature(browser_df, chosen)
  source_sig <- make_signature(source_df, chosen)
  unique_rate <- mean(!duplicated(browser_sig) & !duplicated(browser_sig, fromLast=TRUE))
  if (unique_rate >= 0.999) break
}

# Match only signatures that occur exactly once in both datasets.
source_counts <- table(source_sig)
browser_counts <- table(browser_sig)
unique_ok <- browser_counts[browser_sig] == 1 & source_counts[browser_sig] == 1
match_index <- match(browser_sig, source_sig)
matched <- unique_ok & !is.na(match_index)
match_rate <- mean(matched)

writeLines(c(
  paste("Source frame:", source_name),
  paste("Browser rows:", nrow(browser_df)),
  paste("Source rows:", nrow(source_df)),
  paste("Signature columns:", paste(chosen,collapse=", ")),
  paste("Unambiguous match rate:", sprintf("%.6f",match_rate)),
  paste("Unmatched or ambiguous rows:", sum(!matched))
), file.path(app_dir,"step28f_alignment_report.txt"))

if (!all(matched)) {
  stop("Signature alignment was not exact for all browser rows. No data were changed. See worldview_static_app/step28f_alignment_report.txt.")
}

aligned <- source_df[match_index, , drop=FALSE]
clean_wvs <- function(x) {
  if (inherits(x,"haven_labelled")) x <- unclass(x)
  if (is.factor(x)) x <- as.character(x)
  y <- suppressWarnings(as.numeric(x))
  if (sum(!is.na(y)) >= 0.8 * sum(!is.na(x))) {
    y[y < 0] <- NA_real_
    return(unname(y))
  }
  x <- as.character(x); x[x %in% c("-1","-2","-3","-4","-5")] <- NA_character_; unname(x)
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
 Q238="Having a strong leader who does not have to bother with parliament and elections")

for (id in requested) browser[[id]] <- clean_wvs(aligned[[id]])
for (id in requested) {
  record <- list(id=id,displayName=unname(labels[id]),analysisType="ordinal",correlationEligible=TRUE,correlationRepresentation="Original WVS response coding; negative missing-value codes excluded",topic=if(id=="Q238")"Political culture and political regimes" else "Social values, norms and stereotypes")
  existing <- which(vapply(codebook$variables,function(v)identical(v$id,id),logical(1)))
  if(length(existing)) codebook$variables[[existing[1]]] <- modifyList(codebook$variables[[existing[1]]],record)
  else codebook$variables[[length(codebook$variables)+1L]] <- record
}
jsonlite::write_json(browser,data_path,pretty=FALSE,auto_unbox=TRUE,na="null",null="null",digits=NA)
jsonlite::write_json(codebook,codebook_path,pretty=TRUE,auto_unbox=TRUE,na="null",null="null",digits=NA)

checks <- data.frame(check=c("all_rows_matched","requested_columns_added","requested_codebook_records_added","all_lengths_match"),passed=c(all(matched),all(requested%in%names(browser)),all(requested%in%vapply(codebook$variables,function(v)v$id,character(1))),all(vapply(requested,function(id)length(browser[[id]])==nrow(browser_df),logical(1)))))
write.csv(checks,file.path(app_dir,"step28f_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 28f post-import validation failed.")
cat("\nStep 28f completed successfully.\nAll ",nrow(browser_df)," browser rows were aligned using an exact multi-variable response signature.\nRestart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n",sep="")
