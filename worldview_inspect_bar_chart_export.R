# WorldView diagnostic: inspect Bar chart markup and export handlers.

app_dir <- "worldview_static_app"
files <- c(
  file.path(app_dir, "index.html"),
  file.path(app_dir, "assets", "visualisations.js"),
  file.path(app_dir, "assets", "visualisation-export.js")
)
output <- file.path(app_dir, "bar_chart_export_diagnostic.txt")

missing <- files[!file.exists(files)]
if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "))

terms <- c(
  "Download graph data", "Save Image", "download", "bar chart", "bar-chart",
  "bar_graph", "visual-results", "visual-chart", "createElement", "Blob",
  "text/csv", "image/png", "toDataURL", "addEventListener"
)

extract_context <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hits <- unique(unlist(lapply(terms, function(term) {
    grep(term, lines, fixed = TRUE, ignore.case = TRUE)
  })))
  if (!length(hits)) return(c(paste("FILE:", path), "[No matching lines]", ""))
  show <- sort(unique(unlist(lapply(hits, function(i) {
    seq.int(max(1L, i - 4L), min(length(lines), i + 8L))
  }))))
  c(
    paste("FILE:", path),
    sprintf("%5d | %s", show, lines[show]),
    ""
  )
}

report <- c(
  "WORLDVIEW BAR CHART EXPORT DIAGNOSTIC",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  unlist(lapply(files, extract_context), use.names = FALSE)
)
writeLines(report, output, useBytes = TRUE)
cat("Diagnostic created:\n  ", output, "\n", sep = "")
