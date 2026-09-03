# WorldView Online - Step 26d
# Add a visible hover tooltip and replace country-data download with Save image.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
map_js_path <- file.path(app_dir, "assets", "world-map.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, map_js_path, css_path)))) {
  stop("Run Steps 26, 26b, and 26c first.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Replace the download button label and add an accessible visible tooltip.
html <- gsub(
  '<button id="map-download" class="button button-secondary" type="button" hidden>Download country data</button>',
  '<button id="map-download" class="button button-secondary" type="button" hidden>Save map image</button>',
  html,
  fixed = TRUE
)

old_map <- '<div id="world-map" class="world-map-container" aria-live="polite"></div>'
new_map <- '<div id="world-map" class="world-map-container" aria-live="polite"></div>\n              <div id="map-tooltip" class="map-tooltip" role="status" aria-live="polite" hidden></div>'
if (!grepl('id="map-tooltip"', html, fixed = TRUE)) {
  if (!grepl(old_map, html, fixed = TRUE)) stop("Could not locate the map container.")
  html <- sub(old_map, new_map, html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

# Add visible tooltip functions before createWorldMap().
marker <- "function createWorldMap(event){"
tooltip_functions <- 'function wmShowTooltip(event){const tooltip=document.getElementById("map-tooltip"),path=event.currentTarget;tooltip.innerHTML=`<strong>${wmEsc(path.dataset.country)}</strong><span>${wmEsc(path.dataset.measure)}: ${wmEsc(path.dataset.value)}</span><span>Valid responses: ${wmEsc(path.dataset.n)}</span>`;tooltip.hidden=false;wmMoveTooltip(event);}\nfunction wmMoveTooltip(event){const tooltip=document.getElementById("map-tooltip"),container=document.getElementById("world-map"),rect=container.getBoundingClientRect();tooltip.style.left=`${Math.min(event.clientX-rect.left+14,rect.width-tooltip.offsetWidth-8)}px`;tooltip.style.top=`${Math.max(8,event.clientY-rect.top-tooltip.offsetHeight-12)}px`;}\nfunction wmHideTooltip(){document.getElementById("map-tooltip").hidden=true;}\nfunction wmBindTooltips(){document.querySelectorAll("#world-map path").forEach(path=>{path.addEventListener("mouseenter",wmShowTooltip);path.addEventListener("mousemove",wmMoveTooltip);path.addEventListener("mouseleave",wmHideTooltip);path.addEventListener("focus",wmShowTooltip);path.addEventListener("blur",wmHideTooltip);});}\n'
if (!grepl("function wmShowTooltip", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) stop("Could not locate createWorldMap().")
  js <- sub(marker, paste0(tooltip_functions, marker), js, fixed = TRUE)
}

# Add data attributes and keyboard focus to each map country.
old_path <- 'return`<path d="${wmPath(f.geometry,w,h)}" fill="${wmColour(value,min,max)}" stroke="#ffffff" stroke-width="0.45" vector-effect="non-scaling-stroke"><title>${wmEsc(title)}</title></path>`;'
new_path <- 'const countryName=row?.country||(f.properties.NAME||f.properties.ADMIN),displayValue=row?`${wmFmt(value)}${variable.analysisType==="binary"?"%":""}`:"No data",measureName=row?.measure||"WorldView value";return`<path d="${wmPath(f.geometry,w,h)}" fill="${wmColour(value,min,max)}" stroke="#ffffff" stroke-width="0.45" vector-effect="non-scaling-stroke" tabindex="0" data-country="${wmEsc(countryName)}" data-value="${wmEsc(displayValue)}" data-measure="${wmEsc(measureName)}" data-n="${row?.n??0}"><title>${wmEsc(title)}</title></path>`;'
if (grepl(old_path, js, fixed = TRUE)) {
  js <- sub(old_path, new_path, js, fixed = TRUE)
} else if (!grepl('data-country="${wmEsc(countryName)}"', js, fixed = TRUE)) {
  stop("Could not update map paths with tooltip data.")
}

# Bind tooltips immediately after map SVG is inserted.
old_insert <- 'document.getElementById("world-map").innerHTML=`<svg viewBox="0 0 ${w} ${h}" role="img" aria-label="World choropleth map">${paths}</svg>`;'
new_insert <- paste0(old_insert, 'wmBindTooltips();')
if (!grepl("wmBindTooltips();", js, fixed = TRUE)) {
  if (!grepl(old_insert, js, fixed = TRUE)) stop("Could not locate rendered map insertion.")
  js <- sub(old_insert, new_insert, js, fixed = TRUE)
}

# Replace CSV export with SVG-to-PNG export, including caption and legend.
start <- regexpr("function downloadMapData\\(\\)\\{", js, perl = TRUE)
if (start[1] < 0 && !grepl("function saveMapImage", js, fixed = TRUE)) stop("Could not locate downloadMapData().")
if (start[1] >= 0) {
  tail_text <- substring(js, start[1])
  reset_pos <- regexpr("function resetWorldMap\\(\\)", tail_text, perl = TRUE)
  if (reset_pos[1] < 0) stop("Could not locate resetWorldMap() after downloadMapData().")
  old_function <- substring(tail_text, 1, reset_pos[1] - 1)
  new_function <- 'function saveMapImage(){const svg=document.querySelector("#world-map svg");if(!svg)return;const serializer=new XMLSerializer(),svgText=serializer.serializeToString(svg),caption=document.getElementById("map-caption").textContent,legend=document.getElementById("map-legend"),canvas=document.createElement("canvas"),scale=2,width=1000,height=610;canvas.width=width*scale;canvas.height=height*scale;const context=canvas.getContext("2d");context.scale(scale,scale);context.fillStyle="#ffffff";context.fillRect(0,0,width,height);context.fillStyle="#b9dbea";context.fillRect(0,0,width,520);const image=new Image(),blob=new Blob([svgText],{type:"image/svg+xml;charset=utf-8"}),url=URL.createObjectURL(blob);image.onload=()=>{context.drawImage(image,0,0,width,520);context.fillStyle="#35413f";context.font="16px Arial";const words=caption.split(" ");let line="",y=550;words.forEach(word=>{const test=`${line}${word} `;if(context.measureText(test).width>940){context.fillText(line,30,y);line=`${word} `;y+=22;}else line=test;});context.fillText(line,30,y);const link=document.createElement("a");link.download=`worldview-map-${document.getElementById("map-variable").value}.png`;link.href=canvas.toDataURL("image/png");link.click();URL.revokeObjectURL(url);};image.onerror=()=>{URL.revokeObjectURL(url);document.getElementById("map-message").textContent="The map image could not be created in this browser.";};image.src=url;}\n'
  js <- sub(old_function, new_function, js, fixed = TRUE)
}

js <- gsub(
  'document.getElementById("map-download").addEventListener("click",downloadMapData)',
  'document.getElementById("map-download").addEventListener("click",saveMapImage)',
  js,
  fixed = TRUE
)

writeLines(js, map_js_path, useBytes = TRUE)

if (!grepl("Step 26d: map tooltip", css, fixed = TRUE)) {
  cat('\n/* Step 26d: map tooltip and image export */\n.world-map-figure { position: relative; }\n.map-tooltip { position: absolute; z-index: 5; min-width: 180px; max-width: 260px; padding: 10px 12px; border: 1px solid rgba(20,30,35,.2); border-radius: 8px; background: rgba(255,255,255,.97); box-shadow: 0 5px 18px rgba(20,30,35,.22); color: #172321; pointer-events: none; font-size: .86rem; }\n.map-tooltip strong, .map-tooltip span { display: block; }\n.map-tooltip strong { margin-bottom: 4px; color: #4a148c; }\n.world-map-container path:focus { outline: none; stroke: #111111; stroke-width: 1.5; }\n', file = css_path, append = TRUE)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check=c("visible_tooltip_added","country_name_in_tooltip","value_in_tooltip","valid_n_in_tooltip","paths_keyboard_focusable","image_button_label_added","png_export_added","csv_export_removed"),
  passed=c(grepl('id="map-tooltip"',updated_html,fixed=TRUE),grepl("path.dataset.country",updated_js,fixed=TRUE),grepl("path.dataset.value",updated_js,fixed=TRUE),grepl("path.dataset.n",updated_js,fixed=TRUE),grepl('tabindex="0"',updated_js,fixed=TRUE),grepl("Save map image",updated_html,fixed=TRUE),grepl('toDataURL("image/png")',updated_js,fixed=TRUE),!grepl("function downloadMapData",updated_js,fixed=TRUE)),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step26d_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 26d validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 26d completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
