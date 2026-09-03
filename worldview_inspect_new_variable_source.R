# WorldView diagnostic: inspect Q18-Q26 and Q238 in WVS7_Individual.rds

source_candidates <- c(
  file.path("WVS_Dataset", "WVS7_Individual.rds"),
  "WVS7_Individual.rds"
)
source_path <- source_candidates[file.exists(source_candidates)][1]
output_path <- "worldview_new_variable_source_diagnostic.txt"
requested <- c(paste0("Q", 18:26), "Q238")

if (!length(source_path) || is.na(source_path)) {
  stop("WVS7_Individual.rds was not found.")
}

object <- readRDS(source_path)

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

frames <- collect_frames(object, basename(source_path))
if (!length(frames)) stop("No data frame was found inside the RDS.")

frame_summary <- do.call(rbind, lapply(names(frames), function(name) {
  x <- frames[[name]]
  data.frame(
    frame = name,
    rows = nrow(x),
    columns = ncol(x),
    requested_found = sum(requested %in% names(x)),
    requested_names = paste(intersect(requested, names(x)), collapse = ", "),
    stringsAsFactors = FALSE
  )
}))

best_index <- which.max(frame_summary$requested_found)
data <- frames[[best_index]]
best_name <- names(frames)[best_index]

inspect_one <- function(id) {
  if (!id %in% names(data)) {
    return(c(paste("VARIABLE", id), "COLUMN ABSENT", ""))
  }

  x <- data[[id]]
  raw_unclass <- tryCatch(unclass(x), error = function(e) NULL)
  numeric_direct <- suppressWarnings(as.numeric(x))
  numeric_character <- suppressWarnings(as.numeric(as.character(x)))
  factor_codes <- if (is.factor(x)) as.integer(x) else rep(NA_integer_, length(x))
  value_labels <- attr(x, "labels", exact = TRUE)

  c(
    paste("VARIABLE", id),
    paste("class:", paste(class(x), collapse = ", ")),
    paste("typeof:", typeof(x)),
    paste("length:", length(x)),
    paste("nonmissing original:", sum(!is.na(x))),
    paste("unique original:", length(unique(x[!is.na(x)]))),
    paste("direct numeric nonmissing:", sum(!is.na(numeric_direct))),
    paste("character numeric nonmissing:", sum(!is.na(numeric_character))),
    paste("factor-code nonmissing:", sum(!is.na(factor_codes))),
    paste("levels:", if (is.factor(x)) paste(utils::head(levels(x), 20), collapse = " | ") else "[not a factor]"),
    paste("label attribute:", paste(attr(x, "label", exact = TRUE), collapse = " | ")),
    paste("labels attribute:", if (is.null(value_labels)) "[none]" else paste(names(value_labels), value_labels, sep = "=", collapse = " | ")),
    paste("first original values:", paste(utils::head(as.character(x), 20), collapse = " | ")),
    paste("first direct numeric:", paste(utils::head(numeric_direct, 20), collapse = " | ")),
    paste("unclass typeof:", if (is.null(raw_unclass)) "[failed]" else typeof(raw_unclass)),
    ""
  )
}

report <- c(
  "WORLDVIEW NEW-VARIABLE SOURCE DIAGNOSTIC",
  paste("Source:", source_path),
  paste("Selected frame:", best_name),
  "",
  "DATA FRAMES FOUND",
  capture.output(print(frame_summary, row.names = FALSE)),
  "",
  unlist(lapply(requested, inspect_one), use.names = FALSE)
)

writeLines(report, output_path, useBytes = TRUE)
cat("Diagnostic created:\n  ", output_path, "\n", sep = "")
