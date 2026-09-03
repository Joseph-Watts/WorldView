# WorldView Online - Step 26b
# Remove the map-measure selector and show the active measure in a dynamic caption.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
map_js_path <- file.path(app_dir, "assets", "world-map.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, map_js_path, css_path)))) {
  stop("Run Step 26 first. Required world-map files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Remove the measure selector.
measure_control <- '<label class="field"><span>Map measure</span><select id="map-measure"><option value="auto">Mean or percentage coded 1</option><option value="valid">Valid response count</option></select></label>\n            '
html <- gsub(measure_control, "", html, fixed = TRUE)

# Add a dynamic figure caption below the map.
old_map_block <- '<div id="world-map" class="world-map-container" aria-live="polite"></div>\n            <div id="map-legend" class="map-legend"></div>'
new_map_block <- '<figure class="world-map-figure">\n              <div id="world-map" class="world-map-container" aria-live="polite"></div>\n              <figcaption id="map-caption" class="map-caption">The caption will describe the selected country-level measure.</figcaption>\n            </figure>\n            <div id="map-legend" class="map-legend"></div>'
if (grepl(old_map_block, html, fixed = TRUE)) {
  html <- sub(old_map_block, new_map_block, html, fixed = TRUE)
} else if (!grepl('id="map-caption"', html, fixed = TRUE)) {
  stop("Could not locate the world-map display block.")
}

writeLines(html, index_path, useBytes = TRUE)

# Replace aggregation so maps always show the substantively meaningful measure.
old_aggregate <- 'function wmAggregate(variableId,measure,allowed){const map=new Map();for(let i=0;i<wmState.data.length;i++){const code=wmState.data.columns.B_COUNTRY_ALPHA[i];if(allowed.size&&!allowed.has(code))continue;const raw=wmState.data.columns[variableId][i];if(raw===null||raw===""||!Number.isFinite(Number(raw)))continue;if(!map.has(code))map.set(code,{code,country:wmState.data.columns.B_COUNTRY[i],n:0,sum:0});const row=map.get(code);row.n++;row.sum+=Number(raw);}const variable=wmState.variables.find(v=>v.id===variableId);return[...map.values()].map(row=>({...row,value:measure==="valid"?row.n:variable.analysisType==="binary"?100*row.sum/row.n:row.sum/row.n,measure:measure==="valid"?"Valid responses":variable.analysisType==="binary"?"Percentage coded 1":"Mean"}));}'
new_aggregate <- 'function wmAggregate(variableId,allowed){const map=new Map();for(let i=0;i<wmState.data.length;i++){const code=wmState.data.columns.B_COUNTRY_ALPHA[i];if(allowed.size&&!allowed.has(code))continue;const raw=wmState.data.columns[variableId][i];if(raw===null||raw===""||!Number.isFinite(Number(raw)))continue;if(!map.has(code))map.set(code,{code,country:wmState.data.columns.B_COUNTRY[i],n:0,sum:0});const row=map.get(code);row.n++;row.sum+=Number(raw);}const variable=wmState.variables.find(v=>v.id===variableId),measure=variable.analysisType==="binary"?"Percentage coded 1":"Mean";return[...map.values()].map(row=>({...row,value:variable.analysisType==="binary"?100*row.sum/row.n:row.sum/row.n,measure}));}'
if (grepl(old_aggregate, js, fixed = TRUE)) {
  js <- sub(old_aggregate, new_aggregate, js, fixed = TRUE)
} else if (!grepl("function wmAggregate(variableId,allowed)", js, fixed = TRUE)) {
  stop("Could not locate wmAggregate() in world-map.js.")
}

# Replace map creation to remove map-measure and generate a clear caption.
old_start <- 'function createWorldMap(event){event.preventDefault();const variableId=document.getElementById("map-variable").value,measure=document.getElementById("map-measure").value,allowed=new Set(wmSelected()),rows=wmAggregate(variableId,measure,allowed),values='
new_start <- 'function createWorldMap(event){event.preventDefault();const variableId=document.getElementById("map-variable").value,allowed=new Set(wmSelected()),variable=wmState.variables.find(v=>v.id===variableId),rows=wmAggregate(variableId,allowed),values='
if (grepl(old_start, js, fixed = TRUE)) {
  js <- sub(old_start, new_start, js, fixed = TRUE)
} else if (!grepl("rows=wmAggregate(variableId,allowed)", js, fixed = TRUE)) {
  stop("Could not update createWorldMap().")
}

old_title <- 'document.getElementById("map-title").textContent=wmState.variables.find(v=>v.id===variableId).displayName;'
new_title <- 'document.getElementById("map-title").textContent=variable.displayName;'
js <- gsub(old_title, new_title, js, fixed = TRUE)

old_legend <- 'document.getElementById("map-legend").innerHTML=`<span>${wmFmt(min)}</span><span class="map-gradient"></span><span>${wmFmt(max)}</span>`;'
new_legend <- 'const measureLabel=variable.analysisType==="binary"?"percentage of valid responses coded 1":"mean response";document.getElementById("map-caption").textContent=`Country shading shows the ${measureLabel} for ${variable.id}: ${variable.displayName}. Hover over a country to see its value and valid sample size.`;document.getElementById("map-legend").innerHTML=`<span>${wmFmt(min)}${variable.analysisType==="binary"?"%":""}</span><span class="map-gradient"></span><span>${wmFmt(max)}${variable.analysisType==="binary"?"%":""}</span>`;'
if (grepl(old_legend, js, fixed = TRUE)) {
  js <- sub(old_legend, new_legend, js, fixed = TRUE)
} else if (!grepl("measureLabel=variable.analysisType", js, fixed = TRUE)) {
  stop("Could not add the dynamic map caption.")
}

old_reset <- 'document.getElementById("map-legend").innerHTML="";document.getElementById("map-download").hidden=true;'
new_reset <- 'document.getElementById("map-caption").textContent="The caption will describe the selected country-level measure.";document.getElementById("map-legend").innerHTML="";document.getElementById("map-download").hidden=true;'
js <- gsub(old_reset, new_reset, js, fixed = TRUE)

writeLines(js, map_js_path, useBytes = TRUE)

if (!grepl("Step 26b: map caption", css, fixed = TRUE)) {
  cat('\n/* Step 26b: map caption */\n.world-map-figure { margin: 0; }\n.map-caption { margin-top: 10px; color: var(--muted); font-size: .92rem; line-height: 1.45; }\n', file = css_path, append = TRUE)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "measure_selector_removed",
    "dynamic_caption_added",
    "valid_count_mapping_removed",
    "binary_percentage_retained",
    "mean_mapping_retained",
    "caption_describes_measure",
    "legend_adds_percent_symbol_for_binary"
  ),
  passed = c(
    !grepl('id="map-measure"', updated_html, fixed = TRUE),
    grepl('id="map-caption"', updated_html, fixed = TRUE),
    !grepl('measure==="valid"', updated_js, fixed = TRUE),
    grepl('variable.analysisType==="binary"?100*row.sum/row.n', updated_js, fixed = TRUE),
    grepl('row.sum/row.n', updated_js, fixed = TRUE),
    grepl("Country shading shows the ${measureLabel}", updated_js, fixed = TRUE),
    grepl('variable.analysisType==="binary"?"%":""', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step26b_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 26b validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 26b completed successfully.\n\n")
cat("The map now automatically displays either a mean or the percentage coded 1.\n")
cat("The selected measure is explained in the changing figure caption.\n\n")
cat("Restart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
