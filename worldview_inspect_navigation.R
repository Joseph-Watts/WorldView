# WorldView navigation diagnostic
# Produces a compact report of the actual header/tab markup and app routing code.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
app_js_path <- file.path(app_dir, "assets", "app.js")
output_path <- file.path(app_dir, "navigation_diagnostic.txt")

if (!file.exists(index_path)) stop("Missing: ", index_path)
if (!file.exists(app_js_path)) stop("Missing: ", app_js_path)

html_lines <- readLines(index_path, warn = FALSE, encoding = "UTF-8")
js_lines <- readLines(app_js_path, warn = FALSE, encoding = "UTF-8")

html_hits <- grep(
  "Home|Codebook|Summary Statistics|Visualisations|Models|Download|About|page-about|page-download|data-page|data-target|aria-controls|tab",
  html_lines,
  ignore.case = TRUE
)
js_hits <- grep(
  "home|codebook|summary|visual|model|download|about|querySelector|hidden|active|hash|page-",
  js_lines,
  ignore.case = TRUE
)

expand <- function(hits, total, before = 2L, after = 2L) {
  if (!length(hits)) return(integer())
  unique(unlist(lapply(hits, function(i) seq.int(max(1L, i-before), min(total, i+after)))))
}

html_show <- expand(html_hits, length(html_lines), 3L, 3L)
js_show <- expand(js_hits, length(js_lines), 2L, 2L)

numbered <- function(lines, ids) {
  if (!length(ids)) return("[No matching lines found]")
  sprintf("%5d | %s", ids, lines[ids])
}

report <- c(
  "WORLDVIEW NAVIGATION DIAGNOSTIC",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "=== index.html relevant lines ===",
  numbered(html_lines, html_show),
  "",
  "=== assets/app.js relevant lines ===",
  numbered(js_lines, js_show),
  "",
  "=== Counts ===",
  paste("index.html lines:", length(html_lines)),
  paste("app.js lines:", length(js_lines)),
  paste("data-page occurrences:", sum(grepl("data-page", html_lines, fixed = TRUE))),
  paste("data-target occurrences:", sum(grepl("data-target", html_lines, fixed = TRUE))),
  paste("About controls:", sum(grepl("About", html_lines, fixed = TRUE))),
  paste("About sections:", sum(grepl("page-about", html_lines, fixed = TRUE))),
  paste("Download controls/sections:", sum(grepl("Download|page-download", html_lines, ignore.case = TRUE)))
)

writeLines(report, output_path, useBytes = TRUE)
cat("Navigation diagnostic created:\n  ", output_path, "\n", sep = "")
cat("Please share navigation_diagnostic.txt.\n")
