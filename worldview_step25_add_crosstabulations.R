# WorldView Online - Step 25
# Add Univariate and Cross-tabulations sub-tabs to Summary Statistics.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
crosstab_path <- file.path(app_dir, "assets", "crosstabulations.js")
summary_tabs_path <- file.path(app_dir, "assets", "summary-tabs.js")

required <- c(index_path, css_path,
              file.path(app_dir, "assets", "summary.js"),
              file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
              file.path(app_dir, "data", "worldview-codebook-v1.0.0.json"))
if (!all(file.exists(required))) stop("Required summary-statistics files were not found.")

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Wrap existing summary interface in a Univariate panel and add Cross-tabulations.
if (!grepl('id="summary-subtabs"', html, fixed = TRUE)) {
  start <- '      <div id="summary-loading" class="notice" aria-live="polite">Loading data...</div>'
  start_new <- paste0(
    '      <div id="summary-subtabs" class="model-subtabs" role="tablist" aria-label="Summary statistics">\n',
    '        <button id="tab-summary-univariate" class="model-tab active" type="button" role="tab" aria-selected="true" aria-controls="panel-summary-univariate" data-summary-tab="univariate">Univariate statistics</button>\n',
    '        <button id="tab-summary-crosstab" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-summary-crosstab" data-summary-tab="crosstab">Cross-tabulations</button>\n',
    '      </div>\n\n',
    '      <div id="panel-summary-univariate" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-summary-univariate">\n',
    '      <div id="summary-loading" class="notice" aria-live="polite">Loading data...</div>'
  )
  if (!grepl(start, html, fixed = TRUE)) stop("Could not find Summary Statistics loading block.")
  html <- sub(start, start_new, html, fixed = TRUE)

  # Close univariate after its form, then add crosstab panel before Visualisations.
  boundary <- '      </form>\n    </section>\n\n    <section id="page-visualisations"'
  replacement <- '      </form>\n      </div>\n\n      <div id="panel-summary-crosstab" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-summary-crosstab" hidden>\n        <div class="page-heading model-subheading">\n          <p class="eyebrow">Compare response distributions</p>\n          <h2>Cross-tabulations</h2>\n          <p>Compare two categorical variables using counts, row percentages, column percentages, or total percentages.</p>\n        </div>\n        <div id="crosstab-loading" class="notice" aria-live="polite">Loading cross-tabulation data...</div>\n        <form id="crosstab-form" class="analysis-layout" hidden>\n          <aside class="control-panel">\n            <h2>Choose variables</h2>\n            <label class="field"><span>Row variable</span><select id="crosstab-row" required></select></label>\n            <label class="field"><span>Column variable</span><select id="crosstab-column" required></select></label>\n            <label class="field"><span>Countries included</span><select id="crosstab-countries" multiple size="10"></select></label>\n            <p class="field-help">Selected countries are pooled. If none are selected, all countries are included.</p>\n            <label class="field"><span>Cell display</span><select id="crosstab-display"><option value="count">Counts</option><option value="row">Row percentages</option><option value="column">Column percentages</option><option value="total">Total percentages</option><option value="count-row">Counts and row percentages</option></select></label>\n            <div class="button-row compact">\n              <button class="button button-primary" type="submit">Create cross-tabulation</button>\n              <button id="crosstab-reset" class="button button-secondary" type="button">Reset</button>\n            </div>\n          </aside>\n          <section class="results-panel" aria-labelledby="crosstab-title">\n            <div class="results-heading"><div><p class="eyebrow">Cross-tabulation results</p><h2 id="crosstab-title">Select data to begin</h2></div><button id="crosstab-download" class="button button-secondary" type="button" hidden>Download table</button></div>\n            <div id="crosstab-message" class="notice subtle">Choose two different variables.</div>\n            <div id="crosstab-results" aria-live="polite"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-visualisations"'
  if (!grepl(boundary, html, fixed = TRUE)) stop("Could not find boundary after Summary Statistics.")
  html <- sub(boundary, replacement, html, fixed = TRUE)
}

if (!grepl("assets/summary-tabs.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/summary-tabs.js" defer></script>\n  <script src="assets/crosstabulations.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

summary_tabs_js <- '"use strict";
function activateSummaryTab(name){const tabs=document.querySelectorAll("[data-summary-tab]"),panels={univariate:document.getElementById("panel-summary-univariate"),crosstab:document.getElementById("panel-summary-crosstab")};tabs.forEach(tab=>{const active=tab.dataset.summaryTab===name;tab.classList.toggle("active",active);tab.setAttribute("aria-selected",String(active));tab.setAttribute("tabindex",active?"0":"-1");});Object.entries(panels).forEach(([key,panel])=>{if(panel)panel.hidden=key!==name;});}
document.addEventListener("DOMContentLoaded",()=>{const tabs=[...document.querySelectorAll("[data-summary-tab]")];tabs.forEach((tab,index)=>{tab.addEventListener("click",()=>activateSummaryTab(tab.dataset.summaryTab));tab.addEventListener("keydown",event=>{if(!["ArrowLeft","ArrowRight","Home","End"].includes(event.key))return;event.preventDefault();let next=index;if(event.key==="ArrowLeft")next=(index-1+tabs.length)%tabs.length;if(event.key==="ArrowRight")next=(index+1)%tabs.length;if(event.key==="Home")next=0;if(event.key==="End")next=tabs.length-1;tabs[next].focus();activateSummaryTab(tabs[next].dataset.summaryTab);});});activateSummaryTab("univariate");});'
writeLines(summary_tabs_js, summary_tabs_path, useBytes = TRUE)

crosstab_js <- '"use strict";
const xtPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json"};
const xtState={data:null,variables:[],countries:[],rows:[]};
const xtEsc=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const xtFmt=(v,d=1)=>new Intl.NumberFormat("en-NZ",{maximumFractionDigits:d}).format(v);
const xtSelected=id=>[...document.getElementById(id).selectedOptions].map(o=>o.value);
function xtParse(o){const n=Object.keys(o),l=o[n[0]].length;if(!n.length||!n.every(k=>o[k].length===l))throw Error("Invalid data");return{columns:o,length:l};}
function xtLabel(v,x,countryName){if(v.id==="COUNTRY")return countryName;if(v.id==="Q289")return String(x);if(["Q165","Q166","Q167","Q168"].includes(v.id))return Number(x)===1?"Yes":"No";if(v.id==="Q260")return Number(x)===1?"Male":"Female";if(v.id==="Q263")return Number(x)===1?"Born in this country":"Immigrant to this country";if(Array.isArray(v.levels)){const m=v.levels.find(level=>Number(level.displayOrder)===Number(x));if(m)return m.label;}return String(x);}
function xtSort(labels){return labels.sort((a,b)=>{const na=Number(a),nb=Number(b);return Number.isFinite(na)&&Number.isFinite(nb)?na-nb:a.localeCompare(b);});}
function calculateXt(event){event.preventDefault();const rowId=document.getElementById("crosstab-row").value,columnId=document.getElementById("crosstab-column").value;if(rowId===columnId){document.getElementById("crosstab-message").textContent="Choose two different variables.";return;}const included=new Set(xtSelected("crosstab-countries")),rowVar=xtState.variables.find(v=>v.id===rowId),columnVar=xtState.variables.find(v=>v.id===columnId),rowLabels=[],columnLabels=[],counts=new Map();let n=0;for(let i=0;i<xtState.data.length;i++){const countryCode=xtState.data.columns.B_COUNTRY_ALPHA[i];if(included.size&&!included.has(countryCode))continue;const rawRow=rowId==="COUNTRY"?countryCode:xtState.data.columns[rowId][i],rawColumn=columnId==="COUNTRY"?countryCode:xtState.data.columns[columnId][i];if(rawRow===null||rawColumn===null||rawRow===""||rawColumn==="")continue;const row=xtLabel(rowVar,rawRow,xtState.data.columns.B_COUNTRY[i]),column=xtLabel(columnVar,rawColumn,xtState.data.columns.B_COUNTRY[i]);if(!rowLabels.includes(row))rowLabels.push(row);if(!columnLabels.includes(column))columnLabels.push(column);const key=`${row}|||${column}`;counts.set(key,(counts.get(key)||0)+1);n++;}xtSort(rowLabels);xtSort(columnLabels);if(!n){document.getElementById("crosstab-message").textContent="No complete observations are available.";return;}const rowTotals=rowLabels.map(row=>columnLabels.reduce((sum,column)=>sum+(counts.get(`${row}|||${column}`)||0),0)),columnTotals=columnLabels.map(column=>rowLabels.reduce((sum,row)=>sum+(counts.get(`${row}|||${column}`)||0),0)),display=document.getElementById("crosstab-display").value,cells=[];rowLabels.forEach((row,i)=>columnLabels.forEach((column,j)=>{const count=counts.get(`${row}|||${column}`)||0;cells.push({row,column,count,rowPercentage:100*count/rowTotals[i],columnPercentage:100*count/columnTotals[j],totalPercentage:100*count/n});}));const shown=cell=>display==="count"?xtFmt(cell.count,0):display==="row"?`${xtFmt(cell.rowPercentage)}%`:display==="column"?`${xtFmt(cell.columnPercentage)}%`:display==="total"?`${xtFmt(cell.totalPercentage)}%`:`${xtFmt(cell.count,0)} (${xtFmt(cell.rowPercentage)}%)`;document.getElementById("crosstab-title").textContent=`${rowId} by ${columnId}`;document.getElementById("crosstab-message").textContent=`Complete observations: ${xtFmt(n,0)}. Missing either selected variable is excluded.`;document.getElementById("crosstab-results").innerHTML=`<div class="table-wrap"><table class="results-table"><thead><tr><th>${xtEsc(rowVar.displayName)}</th>${columnLabels.map(column=>`<th>${xtEsc(column)}</th>`).join("")}<th>Total</th></tr></thead><tbody>${rowLabels.map((row,i)=>`<tr><td>${xtEsc(row)}</td>${columnLabels.map(column=>`<td>${shown(cells.find(cell=>cell.row===row&&cell.column===column))}</td>`).join("")}<td>${xtFmt(rowTotals[i],0)}</td></tr>`).join("")}<tr><th>Total</th>${columnTotals.map(total=>`<th>${xtFmt(total,0)}</th>`).join("")}<th>${xtFmt(n,0)}</th></tr></tbody></table></div>`;document.getElementById("crosstab-download").hidden=false;xtState.rows=cells.map(cell=>({...cell,row_variable:rowId,column_variable:columnId,complete_observations:n}));}
function downloadXt(){if(!xtState.rows.length)return;const cols=Object.keys(xtState.rows[0]),q=v=>`"${String(v??"").replaceAll("\\\"","\\\"\\\"")}"`,text=[cols.join(","),...xtState.rows.map(row=>cols.map(c=>q(row[c])).join(","))].join("\\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-cross-tabulation.csv";a.click();URL.revokeObjectURL(a.href);}
function resetXt(){document.getElementById("crosstab-form").reset();document.getElementById("crosstab-row").value="Q165";document.getElementById("crosstab-column").value="Q260";[...document.getElementById("crosstab-countries").options].forEach(o=>o.selected=o.value==="NZL");document.getElementById("crosstab-results").innerHTML="";document.getElementById("crosstab-download").hidden=true;}
async function initialiseXt(){const loading=document.getElementById("crosstab-loading");try{const[data,codebook]=await Promise.all([fetch(xtPaths.data).then(r=>r.json()),fetch(xtPaths.codebook).then(r=>r.json())]);xtState.data=xtParse(data);xtState.variables=[{id:"COUNTRY",displayName:"Country",analysisType:"nominal"},...codebook.variables.filter(v=>v.analysisType!=="integer")];const pairs=new Map();for(let i=0;i<xtState.data.length;i++)pairs.set(xtState.data.columns.B_COUNTRY_ALPHA[i],xtState.data.columns.B_COUNTRY[i]);xtState.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const row=document.getElementById("crosstab-row"),column=document.getElementById("crosstab-column");xtState.variables.forEach(v=>{const label=v.id==="COUNTRY"?"Country":`${v.id}: ${v.displayName}`;row.add(new Option(label,v.id));column.add(new Option(label,v.id));});row.value="Q165";column.value="Q260";const countries=document.getElementById("crosstab-countries");xtState.countries.forEach(c=>{const option=new Option(`${c.name} (${c.code})`,c.code);if(c.code==="NZL")option.selected=true;countries.add(option);});loading.hidden=true;document.getElementById("crosstab-form").hidden=false;}catch(error){loading.textContent=`Cross-tabulation data could not be loaded: ${error.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("crosstab-form").addEventListener("submit",calculateXt);document.getElementById("crosstab-reset").addEventListener("click",resetXt);document.getElementById("crosstab-download").addEventListener("click",downloadXt);initialiseXt();});'
writeLines(crosstab_js, crosstab_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 25: cross-tabulations", css, fixed = TRUE)) {
  cat('\n/* Step 25: cross-tabulations */\n#crosstab-countries { min-height: 220px; }\n', file = css_path, append = TRUE)
}

checks <- data.frame(
  check=c("summary_subtabs_added","univariate_panel_added","crosstab_panel_added","scripts_linked","two_variable_selectors_added","five_display_modes_added","country_filter_added","download_added"),
  passed=c(grepl('id="summary-subtabs"',html,fixed=TRUE),grepl('id="panel-summary-univariate"',html,fixed=TRUE),grepl('id="panel-summary-crosstab"',html,fixed=TRUE),grepl("assets/crosstabulations.js",html,fixed=TRUE),grepl('id="crosstab-row"',html,fixed=TRUE)&&grepl('id="crosstab-column"',html,fixed=TRUE),grepl('value="count-row"',html,fixed=TRUE),grepl('id="crosstab-countries"',html,fixed=TRUE),grepl('id="crosstab-download"',html,fixed=TRUE)),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step25_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 25 validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 25 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
