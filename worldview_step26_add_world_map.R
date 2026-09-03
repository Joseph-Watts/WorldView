# WorldView Online - Step 26
# Add a World map sub-tab to Visualisations using a local Natural Earth GeoJSON.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
map_js_path <- file.path(app_dir, "assets", "world-map.js")
visual_tabs_path <- file.path(app_dir, "assets", "visualisation-tabs.js")
geojson_path <- file.path(app_dir, "data", "natural-earth-110m-countries.geojson")

required <- c(index_path, css_path,
              file.path(app_dir, "assets", "visualisations.js"),
              file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
              file.path(app_dir, "data", "worldview-codebook-v1.0.0.json"))
if (!all(file.exists(required))) stop("Required visualisation files were not found.")

# Download a local copy so the deployed map has no runtime CDN dependency.
geo_url <- "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson"
if (!file.exists(geojson_path)) {
  tryCatch(
    utils::download.file(geo_url, geojson_path, mode = "wb", quiet = FALSE),
    error = function(e) stop("Could not download Natural Earth GeoJSON: ", conditionMessage(e))
  )
}
if (!file.exists(geojson_path) || file.info(geojson_path)$size < 100000) {
  stop("The Natural Earth GeoJSON download is missing or unexpectedly small.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

if (!grepl('id="visualisation-subtabs"', html, fixed = TRUE)) {
  start <- '      <div id="visual-loading" class="notice" aria-live="polite">Loading data...</div>'
  start_new <- paste0(
    '      <div id="visualisation-subtabs" class="model-subtabs" role="tablist" aria-label="Visualisations">\n',
    '        <button id="tab-visual-bar" class="model-tab active" type="button" role="tab" aria-selected="true" aria-controls="panel-visual-bar" data-visual-tab="bar">Bar chart</button>\n',
    '        <button id="tab-visual-map" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-visual-map" data-visual-tab="map">World map</button>\n',
    '      </div>\n\n',
    '      <div id="panel-visual-bar" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-visual-bar">\n',
    '      <div id="visual-loading" class="notice" aria-live="polite">Loading data...</div>'
  )
  if (!grepl(start, html, fixed = TRUE)) stop("Could not find Visualisations loading block.")
  html <- sub(start, start_new, html, fixed = TRUE)

  boundary <- '      </form>\n    </section>\n\n    <section id="page-models"'
  replacement <- '      </form>\n      </div>\n\n      <div id="panel-visual-map" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-visual-map" hidden>\n        <div class="page-heading model-subheading">\n          <p class="eyebrow">Compare countries geographically</p>\n          <h2>World map</h2>\n          <p>Map country-level means, percentages, or valid sample sizes for variables included in WorldView.</p>\n        </div>\n        <div id="map-loading" class="notice" aria-live="polite">Loading map data...</div>\n        <form id="map-form" class="analysis-layout" hidden>\n          <aside class="control-panel">\n            <h2>Map settings</h2>\n            <label class="field"><span>Variable</span><select id="map-variable" required></select></label>\n            <label class="field"><span>Map measure</span><select id="map-measure"><option value="auto">Mean or percentage coded 1</option><option value="valid">Valid response count</option></select></label>\n            <label class="field"><span>Countries included</span><select id="map-countries" multiple size="10"></select></label>\n            <p class="field-help">If no countries are selected, all available WorldView countries are mapped.</p>\n            <div class="button-row compact"><button class="button button-primary" type="submit">Create map</button><button id="map-reset" class="button button-secondary" type="button">Reset</button></div>\n          </aside>\n          <section class="results-panel" aria-labelledby="map-title">\n            <div class="results-heading"><div><p class="eyebrow">World map</p><h2 id="map-title">Select data to begin</h2></div><button id="map-download" class="button button-secondary" type="button" hidden>Download country data</button></div>\n            <div id="map-message" class="notice subtle">Choose a variable and create the map.</div>\n            <div id="world-map" class="world-map-container" aria-live="polite"></div>\n            <div id="map-legend" class="map-legend"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-models"'
  if (!grepl(boundary, html, fixed = TRUE)) stop("Could not find boundary after Visualisations.")
  html <- sub(boundary, replacement, html, fixed = TRUE)
}

if (!grepl("assets/world-map.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/visualisation-tabs.js" defer></script>\n  <script src="assets/world-map.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

visual_tabs_js <- '"use strict";
function activateVisualTab(name){const tabs=document.querySelectorAll("[data-visual-tab]"),panels={bar:document.getElementById("panel-visual-bar"),map:document.getElementById("panel-visual-map")};tabs.forEach(tab=>{const active=tab.dataset.visualTab===name;tab.classList.toggle("active",active);tab.setAttribute("aria-selected",String(active));tab.setAttribute("tabindex",active?"0":"-1");});Object.entries(panels).forEach(([key,panel])=>{if(panel)panel.hidden=key!==name;});}
document.addEventListener("DOMContentLoaded",()=>{const tabs=[...document.querySelectorAll("[data-visual-tab]")];tabs.forEach((tab,index)=>{tab.addEventListener("click",()=>activateVisualTab(tab.dataset.visualTab));tab.addEventListener("keydown",event=>{if(!["ArrowLeft","ArrowRight","Home","End"].includes(event.key))return;event.preventDefault();let next=index;if(event.key==="ArrowLeft")next=(index-1+tabs.length)%tabs.length;if(event.key==="ArrowRight")next=(index+1)%tabs.length;if(event.key==="Home")next=0;if(event.key==="End")next=tabs.length-1;tabs[next].focus();activateVisualTab(tabs[next].dataset.visualTab);});});activateVisualTab("bar");});'
writeLines(visual_tabs_js, visual_tabs_path, useBytes = TRUE)

map_js <- '"use strict";
const wmPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json",geo:"data/natural-earth-110m-countries.geojson"};
const wmState={data:null,variables:[],countries:[],geo:null,lastRows:[]};
const wmEsc=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const wmFmt=(v,d=2)=>v==null||!Number.isFinite(v)?"No data":new Intl.NumberFormat("en-NZ",{maximumFractionDigits:d}).format(v);
function wmParse(o){const n=Object.keys(o),l=o[n[0]].length;if(!n.length||!n.every(k=>o[k].length===l))throw Error("Invalid data");return{columns:o,length:l};}
function wmSelected(){return[...document.getElementById("map-countries").selectedOptions].map(o=>o.value);}
function wmProject(point,w,h){const lon=point[0],lat=Math.max(-85,Math.min(85,point[1]));return[(lon+180)/360*w,(90-lat)/180*h];}
function wmPathRing(ring,w,h){return ring.map((p,i)=>{const q=wmProject(p,w,h);return`${i?"L":"M"}${q[0].toFixed(2)},${q[1].toFixed(2)}`;}).join(" ")+" Z";}
function wmPath(geometry,w,h){if(!geometry)return"";if(geometry.type==="Polygon")return geometry.coordinates.map(r=>wmPathRing(r,w,h)).join(" ");if(geometry.type==="MultiPolygon")return geometry.coordinates.flatMap(p=>p.map(r=>wmPathRing(r,w,h))).join(" ");return"";}
function wmColour(value,min,max){if(value==null||!Number.isFinite(value))return"#e5e7e7";const t=max===min?.5:Math.max(0,Math.min(1,(value-min)/(max-min))),a=[238,242,239],b=[0,91,83];return`rgb(${Math.round(a[0]+t*(b[0]-a[0]))},${Math.round(a[1]+t*(b[1]-a[1]))},${Math.round(a[2]+t*(b[2]-a[2]))})`;}
function wmAggregate(variableId,measure,allowed){const map=new Map();for(let i=0;i<wmState.data.length;i++){const code=wmState.data.columns.B_COUNTRY_ALPHA[i];if(allowed.size&&!allowed.has(code))continue;const raw=wmState.data.columns[variableId][i];if(raw===null||raw===""||!Number.isFinite(Number(raw)))continue;if(!map.has(code))map.set(code,{code,country:wmState.data.columns.B_COUNTRY[i],n:0,sum:0});const row=map.get(code);row.n++;row.sum+=Number(raw);}const variable=wmState.variables.find(v=>v.id===variableId);return[...map.values()].map(row=>({...row,value:measure==="valid"?row.n:variable.analysisType==="binary"?100*row.sum/row.n:row.sum/row.n,measure:measure==="valid"?"Valid responses":variable.analysisType==="binary"?"Percentage coded 1":"Mean"}));}
function createWorldMap(event){event.preventDefault();const variableId=document.getElementById("map-variable").value,measure=document.getElementById("map-measure").value,allowed=new Set(wmSelected()),rows=wmAggregate(variableId,measure,allowed),values=rows.map(r=>r.value).filter(Number.isFinite),min=Math.min(...values),max=Math.max(...values),lookup=new Map(rows.map(r=>[r.code,r])),w=1000,h=520,features=wmState.geo.features.filter(f=>f.properties.ADM0_A3!=="ATA"),paths=features.map(f=>{const code=f.properties.ADM0_A3||f.properties.ISO_A3,row=lookup.get(code),value=row?.value,title=row?`${row.country}: ${wmFmt(value)} (${row.measure}; n=${row.n})`:`${f.properties.NAME||f.properties.ADMIN}: No WorldView data`;return`<path d="${wmPath(f.geometry,w,h)}" fill="${wmColour(value,min,max)}" stroke="#ffffff" stroke-width="0.45" vector-effect="non-scaling-stroke"><title>${wmEsc(title)}</title></path>`;}).join("");document.getElementById("map-title").textContent=wmState.variables.find(v=>v.id===variableId).displayName;document.getElementById("map-message").textContent=rows.length?`${rows.length} WorldView countries mapped. Hover over a country for its value.`:"No valid country-level values are available.";document.getElementById("world-map").innerHTML=`<svg viewBox="0 0 ${w} ${h}" role="img" aria-label="World choropleth map">${paths}</svg>`;document.getElementById("map-legend").innerHTML=`<span>${wmFmt(min)}</span><span class="map-gradient"></span><span>${wmFmt(max)}</span>`;document.getElementById("map-download").hidden=false;wmState.lastRows=rows;}
function downloadMapData(){if(!wmState.lastRows.length)return;const cols=["country","code","measure","value","n"],q=v=>`"${String(v??"").replaceAll("\\\"","\\\"\\\"")}"`,text=[cols.join(","),...wmState.lastRows.map(r=>cols.map(c=>q(r[c])).join(","))].join("\\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-world-map-data.csv";a.click();URL.revokeObjectURL(a.href);}
function resetWorldMap(){document.getElementById("map-form").reset();document.getElementById("map-variable").value="Q177";[...document.getElementById("map-countries").options].forEach(o=>o.selected=false);document.getElementById("world-map").innerHTML="";document.getElementById("map-legend").innerHTML="";document.getElementById("map-download").hidden=true;}
async function initialiseWorldMap(){const loading=document.getElementById("map-loading");try{const[data,codebook,geo]=await Promise.all([fetch(wmPaths.data).then(r=>r.json()),fetch(wmPaths.codebook).then(r=>r.json()),fetch(wmPaths.geo).then(r=>r.json())]);wmState.data=wmParse(data);wmState.variables=codebook.variables.filter(v=>v.analysisType!=="nominal");wmState.geo=geo;const pairs=new Map();for(let i=0;i<wmState.data.length;i++)pairs.set(wmState.data.columns.B_COUNTRY_ALPHA[i],wmState.data.columns.B_COUNTRY[i]);wmState.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const variable=document.getElementById("map-variable");wmState.variables.forEach(v=>variable.add(new Option(`${v.id}: ${v.displayName}`,v.id)));variable.value="Q177";const countries=document.getElementById("map-countries");wmState.countries.forEach(c=>countries.add(new Option(`${c.name} (${c.code})`,c.code)));loading.hidden=true;document.getElementById("map-form").hidden=false;}catch(error){loading.textContent=`World map data could not be loaded: ${error.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("map-form").addEventListener("submit",createWorldMap);document.getElementById("map-reset").addEventListener("click",resetWorldMap);document.getElementById("map-download").addEventListener("click",downloadMapData);initialiseWorldMap();});'
writeLines(map_js, map_js_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 26: world map", css, fixed = TRUE)) {
  cat('\n/* Step 26: world map */\n.world-map-container { margin-top: 18px; border: 1px solid var(--line); border-radius: 14px; overflow: hidden; background: #dcebf0; }\n.world-map-container svg { display: block; width: 100%; height: auto; }\n.world-map-container path { transition: opacity .15s ease; }\n.world-map-container path:hover { opacity: .68; stroke: #1d2726; stroke-width: 1.2; }\n.map-legend { display: flex; align-items: center; gap: 10px; margin-top: 12px; color: var(--muted); font-size: .85rem; }\n.map-gradient { display: inline-block; width: min(320px, 60%); height: 14px; border-radius: 999px; background: linear-gradient(90deg, rgb(238,242,239), rgb(0,91,83)); }\n#map-countries { min-height: 220px; }\n', file = css_path, append = TRUE)
}

checks <- data.frame(
  check=c("geojson_downloaded","visualisation_subtabs_added","bar_panel_added","map_panel_added","map_scripts_linked","country_aggregation_added","binary_percentage_added","svg_map_added","download_added"),
  passed=c(file.exists(geojson_path)&&file.info(geojson_path)$size>100000,grepl('id="visualisation-subtabs"',html,fixed=TRUE),grepl('id="panel-visual-bar"',html,fixed=TRUE),grepl('id="panel-visual-map"',html,fixed=TRUE),grepl("assets/world-map.js",html,fixed=TRUE),grepl("wmAggregate",map_js,fixed=TRUE),grepl('variable.analysisType==="binary"',map_js,fixed=TRUE),grepl("World choropleth map",map_js,fixed=TRUE),grepl('id="map-download"',html,fixed=TRUE)),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step26_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 26 validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 26 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
