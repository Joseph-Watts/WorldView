# WorldView Online - Step 33c
# Replace the original Bar chart CSV handler directly with a PNG renderer.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
visual_path <- file.path(app_dir, "assets", "visualisations.js")
export_path <- file.path(app_dir, "assets", "visualisation-export.js")

if (!all(file.exists(c(index_path, visual_path)))) stop("Required files were not found.")

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js <- paste(readLines(visual_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Remove the broken generic exporter. The original visualisations.js listener
# will now call a genuine PNG function instead of the CSV function.
html <- gsub(
  '<script[^>]*src="assets/visualisation-export.js"[^>]*></script>',
  "", html, perl = TRUE, ignore.case = TRUE
)
if (file.exists(export_path)) file.remove(export_path)

start <- regexpr("function downloadGraphData\\(\\) \\{", js, perl = TRUE)
end_marker <- regexpr("\\nfunction updateAgeControls\\(\\)", js, perl = TRUE)
if (start[1] < 0 || end_marker[1] < 0 || end_marker[1] <= start[1]) {
  stop("Could not locate downloadGraphData() in visualisations.js.")
}

png_function <- 'function downloadGraphData() {
  if (!visualState.lastRows.length) return;

  const rows = visualState.lastRows;
  const groupNames = [...new Set(rows.map(row => row.country_selection))];
  const responseNames = [...new Set(rows.map(row => row.response))];
  const measureElement = document.getElementById("visual-measure");
  const measure = measureElement ? measureElement.value : "percentage";
  const valueFor = row => measure === "count" ? Number(row.count) : Number(row.percentage_of_valid);
  const maximum = Math.max(1, ...rows.map(valueFor));

  const width = Math.max(900, groupNames.length * Math.max(180, responseNames.length * 54));
  const height = 620;
  const margin = { top: 80, right: 40, bottom: 170, left: 85 };
  const plotWidth = width - margin.left - margin.right;
  const plotHeight = height - margin.top - margin.bottom;
  const groupWidth = plotWidth / groupNames.length;
  const barGap = 5;
  const barWidth = Math.max(8, Math.min(42, (groupWidth - 24) / Math.max(1, responseNames.length) - barGap));
  const colours = ["#005b53", "#2f7f77", "#65a39c", "#9bc7c2", "#d2e6e3", "#6c5b7b", "#aa6f73", "#d09a74"];
  const escapeXml = value => String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;");
  const y = value => margin.top + plotHeight - (value / maximum) * plotHeight;

  const ticks = 5;
  let body = `<rect width="${width}" height="${height}" fill="white"/>`;
  body += `<text x="${width / 2}" y="34" text-anchor="middle" font-family="Arial" font-size="22" font-weight="700">${escapeXml(document.getElementById("visual-title").textContent)}</text>`;

  for (let i = 0; i <= ticks; i += 1) {
    const value = maximum * i / ticks;
    const yy = y(value);
    body += `<line x1="${margin.left}" y1="${yy}" x2="${width - margin.right}" y2="${yy}" stroke="#d7dfdd"/>`;
    body += `<text x="${margin.left - 12}" y="${yy + 5}" text-anchor="end" font-family="Arial" font-size="13">${measure === "count" ? Math.round(value) : value.toFixed(0) + "%"}</text>`;
  }

  groupNames.forEach((groupName, groupIndex) => {
    const groupRows = rows.filter(row => row.country_selection === groupName);
    const usedWidth = responseNames.length * (barWidth + barGap);
    const groupStart = margin.left + groupIndex * groupWidth + (groupWidth - usedWidth) / 2;
    responseNames.forEach((response, responseIndex) => {
      const row = groupRows.find(item => item.response === response);
      if (!row) return;
      const value = valueFor(row);
      const x = groupStart + responseIndex * (barWidth + barGap);
      const yy = y(value);
      const barHeight = margin.top + plotHeight - yy;
      body += `<rect x="${x}" y="${yy}" width="${barWidth}" height="${barHeight}" rx="2" fill="${colours[responseIndex % colours.length]}"/>`;
    });
    const labelX = margin.left + groupIndex * groupWidth + groupWidth / 2;
    body += `<text x="${labelX}" y="${margin.top + plotHeight + 28}" text-anchor="end" transform="rotate(-35 ${labelX} ${margin.top + plotHeight + 28})" font-family="Arial" font-size="13">${escapeXml(groupName)}</text>`;
  });

  const legendY = height - 42;
  responseNames.forEach((response, index) => {
    const itemWidth = Math.min(220, plotWidth / Math.max(1, responseNames.length));
    const x = margin.left + index * itemWidth;
    body += `<rect x="${x}" y="${legendY - 13}" width="14" height="14" fill="${colours[index % colours.length]}"/>`;
    body += `<text x="${x + 20}" y="${legendY}" font-family="Arial" font-size="12">${escapeXml(response)}</text>`;
  });

  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">${body}</svg>`;
  const blob = new Blob([svg], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const image = new Image();
  image.onload = () => {
    const scale = 2;
    const canvas = document.createElement("canvas");
    canvas.width = width * scale;
    canvas.height = height * scale;
    const context = canvas.getContext("2d");
    context.scale(scale, scale);
    context.drawImage(image, 0, 0, width, height);
    URL.revokeObjectURL(url);
    canvas.toBlob(pngBlob => {
      const link = document.createElement("a");
      link.href = URL.createObjectURL(pngBlob);
      link.download = `worldview-bar-chart-${document.getElementById("visual-variable").value}.png`;
      document.body.appendChild(link);
      link.click();
      setTimeout(() => { URL.revokeObjectURL(link.href); link.remove(); }, 0);
    }, "image/png");
  };
  image.onerror = () => {
    URL.revokeObjectURL(url);
    document.getElementById("visual-message").textContent = "The PNG image could not be created in this browser.";
  };
  image.src = url;
}
'

js <- paste0(
  substring(js, 1, start[1] - 1L),
  png_function,
  substring(js, end_marker[1] + 1L)
)

# Standardise the three visualisation labels explicitly.
html <- gsub('(<button id="visual-download"[^>]*>)[^<]*(</button>)', '\\1Save Image\\2', html, perl = TRUE)
html <- gsub('(<button id="map-download"[^>]*>)[^<]*(</button>)', '\\1Save Image\\2', html, perl = TRUE)
html <- gsub('(<button id="phylo-save"[^>]*>)[^<]*(</button>)', '\\1Save Image\\2', html, perl = TRUE)

writeLines(js, visual_path, useBytes = TRUE)
writeLines(html, index_path, useBytes = TRUE)

if (dir.exists("worldview_deployment")) {
  file.copy(visual_path, file.path("worldview_deployment", "assets", "visualisations.js"), overwrite = TRUE)
  file.copy(index_path, file.path("worldview_deployment", "index.html"), overwrite = TRUE)
  deployed_export <- file.path("worldview_deployment", "assets", "visualisation-export.js")
  if (file.exists(deployed_export)) file.remove(deployed_export)
}

updated_js <- paste(readLines(visual_path, warn = FALSE), collapse = "\n")
updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c("csv_blob_removed", "csv_filename_removed", "png_blob_added", "png_filename_added", "original_listener_retained", "generic_exporter_removed", "all_three_labels_standardised"),
  passed = c(
    !grepl('text/csv', updated_js, fixed = TRUE),
    !grepl('worldview-bar-graph-', updated_js, fixed = TRUE),
    grepl('canvas.toBlob', updated_js, fixed = TRUE) && grepl('"image/png"', updated_js, fixed = TRUE),
    grepl('worldview-bar-chart-', updated_js, fixed = TRUE) && grepl('.png', updated_js, fixed = TRUE),
    grepl('visual-download").addEventListener("click", downloadGraphData)', updated_js, fixed = TRUE),
    !grepl('assets/visualisation-export.js', updated_html, fixed = TRUE),
    all(vapply(c("visual-download", "map-download", "phylo-save"), function(id) grepl(paste0('id="', id, '"'), updated_html, fixed = TRUE), logical(1))) && lengths(regmatches(updated_html, gregexpr('>Save Image</button>', updated_html, fixed = TRUE))) >= 3L
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step33c_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Step 33c validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))

cat("\nStep 33c completed successfully.\n")
cat("The original Bar chart button now creates a real PNG instead of a CSV/text file.\n")
cat("Restart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
