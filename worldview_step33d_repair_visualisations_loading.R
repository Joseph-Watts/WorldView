# WorldView Online - Step 33d
# Repair visualisations.js after the previous PNG patch introduced a parse error.
# Replaces only downloadGraphData() with a simpler, syntax-safe canvas PNG export.

app_dir <- "worldview_static_app"
js_path <- file.path(app_dir, "assets", "visualisations.js")
index_path <- file.path(app_dir, "index.html")
backup_path <- file.path(app_dir, "assets", "visualisations-before-step33d.js")

if (!file.exists(js_path)) stop("visualisations.js was not found.")
if (!file.exists(index_path)) stop("index.html was not found.")

lines <- readLines(js_path, warn = FALSE, encoding = "UTF-8")
if (!file.exists(backup_path)) file.copy(js_path, backup_path, overwrite = FALSE)

start <- grep("^function downloadGraphData\\(\\)", lines)
end <- grep("^function updateAgeControls\\(\\)", lines)
if (length(start) != 1L || length(end) != 1L || end <= start) {
  stop("Could not isolate downloadGraphData() safely. No file was changed.")
}

replacement <- c(
'function downloadGraphData() {',
'  if (!visualState.lastRows || !visualState.lastRows.length) return;',
'',
'  const rows = visualState.lastRows;',
'  const groups = [...new Set(rows.map(row => row.country_selection))];',
'  const responses = [...new Set(rows.map(row => row.response))];',
'  const measureControl = document.getElementById("visual-measure");',
'  const useCounts = measureControl && measureControl.value === "count";',
'  const getValue = row => useCounts ? Number(row.count) : Number(row.percentage_of_valid);',
'  const maximum = Math.max(1, ...rows.map(getValue).filter(Number.isFinite));',
'',
'  const width = Math.max(960, groups.length * 190);',
'  const height = 640;',
'  const scale = 2;',
'  const margin = { top: 70, right: 36, bottom: 170, left: 86 };',
'  const plotWidth = width - margin.left - margin.right;',
'  const plotHeight = height - margin.top - margin.bottom;',
'  const canvas = document.createElement("canvas");',
'  canvas.width = width * scale;',
'  canvas.height = height * scale;',
'  const context = canvas.getContext("2d");',
'  context.scale(scale, scale);',
'  context.fillStyle = "#ffffff";',
'  context.fillRect(0, 0, width, height);',
'  context.font = "14px Arial, sans-serif";',
'  context.textBaseline = "middle";',
'',
'  context.fillStyle = "#173d38";',
'  context.font = "bold 22px Arial, sans-serif";',
'  context.textAlign = "center";',
'  context.fillText(document.getElementById("visual-title").textContent, width / 2, 32);',
'',
'  context.font = "13px Arial, sans-serif";',
'  for (let tick = 0; tick <= 5; tick += 1) {',
'    const value = maximum * tick / 5;',
'    const y = margin.top + plotHeight - (value / maximum) * plotHeight;',
'    context.strokeStyle = "#d7dfdd";',
'    context.beginPath();',
'    context.moveTo(margin.left, y);',
'    context.lineTo(width - margin.right, y);',
'    context.stroke();',
'    context.fillStyle = "#334a47";',
'    context.textAlign = "right";',
'    context.fillText(useCounts ? String(Math.round(value)) : value.toFixed(0) + "%", margin.left - 10, y);',
'  }',
'',
'  const colours = ["#005b53", "#4f9189", "#90c0ba", "#c7dfdc", "#6c5b7b", "#aa6f73"];',
'  const groupWidth = plotWidth / Math.max(1, groups.length);',
'  const gap = 5;',
'  const barWidth = Math.max(7, Math.min(42, groupWidth / Math.max(1, responses.length) - gap));',
'',
'  groups.forEach((group, groupIndex) => {',
'    const groupRows = rows.filter(row => row.country_selection === group);',
'    const usedWidth = responses.length * (barWidth + gap);',
'    const startX = margin.left + groupIndex * groupWidth + (groupWidth - usedWidth) / 2;',
'    responses.forEach((response, responseIndex) => {',
'      const row = groupRows.find(item => item.response === response);',
'      if (!row) return;',
'      const value = getValue(row);',
'      if (!Number.isFinite(value)) return;',
'      const barHeight = value / maximum * plotHeight;',
'      context.fillStyle = colours[responseIndex % colours.length];',
'      context.fillRect(startX + responseIndex * (barWidth + gap), margin.top + plotHeight - barHeight, barWidth, barHeight);',
'    });',
'',
'    context.save();',
'    context.translate(margin.left + groupIndex * groupWidth + groupWidth / 2, margin.top + plotHeight + 20);',
'    context.rotate(-Math.PI / 5);',
'    context.fillStyle = "#334a47";',
'    context.textAlign = "right";',
'    context.font = "13px Arial, sans-serif";',
'    context.fillText(group, 0, 0);',
'    context.restore();',
'  });',
'',
'  context.font = "12px Arial, sans-serif";',
'  context.textAlign = "left";',
'  responses.forEach((response, index) => {',
'    const x = margin.left + index * Math.min(220, plotWidth / Math.max(1, responses.length));',
'    const y = height - 36;',
'    context.fillStyle = colours[index % colours.length];',
'    context.fillRect(x, y - 7, 14, 14);',
'    context.fillStyle = "#334a47";',
'    context.fillText(response, x + 20, y);',
'  });',
'',
'  canvas.toBlob(blob => {',
'    if (!blob) return;',
'    const link = document.createElement("a");',
'    link.href = URL.createObjectURL(blob);',
'    link.download = "worldview-bar-chart-" + document.getElementById("visual-variable").value + ".png";',
'    document.body.appendChild(link);',
'    link.click();',
'    setTimeout(() => { URL.revokeObjectURL(link.href); link.remove(); }, 0);',
'  }, "image/png");',
'}',
''
)

new_lines <- c(lines[seq_len(start - 1L)], replacement, lines[end:length(lines)])
writeLines(new_lines, js_path, useBytes = TRUE)

# Remove the obsolete generic exporter if it is still referenced.
html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html <- gsub('<script[^>]*src="assets/visualisation-export.js"[^>]*></script>', "", html, perl = TRUE, ignore.case = TRUE)
writeLines(html, index_path, useBytes = TRUE)
old_export <- file.path(app_dir, "assets", "visualisation-export.js")
if (file.exists(old_export)) file.remove(old_export)

# Use Node for an actual JavaScript parse check when available.
node <- Sys.which("node")
parse_ok <- TRUE
parse_output <- "Node was not available; structural checks were used."
if (nzchar(node)) {
  result <- suppressWarnings(system2(node, c("--check", shQuote(js_path)), stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  parse_ok <- is.null(status) || identical(status, 0L)
  parse_output <- paste(result, collapse = "\n")
}
if (!parse_ok) {
  file.copy(backup_path, js_path, overwrite = TRUE)
  stop("The repaired JavaScript did not parse, so the backup was restored.\n", parse_output)
}

updated <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c("javascript_parses", "initialiser_retained", "loading_hide_retained", "csv_export_removed", "png_export_present", "single_download_function"),
  passed = c(
    parse_ok,
    grepl("async function initialiseVisualisations", updated, fixed = TRUE),
    grepl("loading.hidden = true", updated, fixed = TRUE),
    !grepl("text/csv", updated, fixed = TRUE),
    grepl('canvas.toBlob', updated, fixed = TRUE) && grepl('"image/png"', updated, fixed = TRUE),
    lengths(regmatches(updated, gregexpr("function downloadGraphData", updated, fixed = TRUE))) == 1L
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step33d_validation_checks.csv"), row.names = FALSE)
writeLines(parse_output, file.path(app_dir, "step33d_javascript_parse_check.txt"))
if (!all(checks$passed)) stop("Step 33d validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))

if (dir.exists("worldview_deployment")) {
  file.copy(js_path, file.path("worldview_deployment", "assets", "visualisations.js"), overwrite = TRUE)
  file.copy(index_path, file.path("worldview_deployment", "index.html"), overwrite = TRUE)
  deployed_export <- file.path("worldview_deployment", "assets", "visualisation-export.js")
  if (file.exists(deployed_export)) file.remove(deployed_export)
}

cat("\nStep 33d completed successfully.\n")
cat("visualisations.js passed the available parse and structural checks.\n")
cat("Restart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
