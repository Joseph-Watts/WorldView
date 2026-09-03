# WorldView Online - Step 21
# Add a chi-square test of independence as its own Models sub-tab.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
tabs_path <- file.path(app_dir, "assets", "model-tabs.js")
css_path <- file.path(app_dir, "assets", "styles.css")
chi_path <- file.path(app_dir, "assets", "chi-square.js")

required <- c(index_path, tabs_path, css_path,
              file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
              file.path(app_dir, "data", "worldview-codebook-v1.0.0.json"))
if (!all(file.exists(required))) stop("Required app files were not found.")

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Add tab after ANOVA.
old_tab <- '<button id="tab-anova" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-anova" data-model-tab="anova">One-way ANOVA</button>'
new_tabs <- paste0(
  old_tab,
  '\n        <button id="tab-chi-square" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-chi-square" data-model-tab="chi-square">Chi-square</button>'
)
if (!grepl('data-model-tab="chi-square"', html, fixed = TRUE)) {
  if (!grepl(old_tab, html, fixed = TRUE)) stop("ANOVA tab not found.")
  html <- sub(old_tab, new_tabs, html, fixed = TRUE)
}

marker <- '      </form>\n      </div>\n    </section>\n\n    <section id="page-download"'
panel <- '      </form>\n      </div>\n\n      <div id="panel-chi-square" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-chi-square" hidden>\n        <div class="page-heading model-subheading">\n          <p class="eyebrow">Examine categorical associations</p>\n          <h2>Chi-square test of independence</h2>\n          <p>Test whether the response distributions of two categorical variables are associated.</p>\n        </div>\n        <div id="chi-loading" class="notice" aria-live="polite">Loading chi-square data...</div>\n        <form id="chi-form" class="analysis-layout" hidden>\n          <aside class="control-panel">\n            <h2>Specify comparison</h2>\n            <label class="field"><span>Row variable</span><select id="chi-row" required></select></label>\n            <label class="field"><span>Column variable</span><select id="chi-column" required></select></label>\n            <label class="field"><span>Countries included</span><select id="chi-countries" multiple size="10"></select></label>\n            <p class="field-help">Selected countries are pooled. If none are selected, all countries are included.</p>\n            <label class="field"><span>Cell display</span><select id="chi-display"><option value="count">Observed counts</option><option value="row">Row percentages</option><option value="column">Column percentages</option><option value="expected">Expected counts</option></select></label>\n            <div class="button-row compact">\n              <button class="button button-primary" type="submit">Run chi-square test</button>\n              <button id="chi-reset" class="button button-secondary" type="button">Reset</button>\n            </div>\n          </aside>\n          <section class="results-panel" aria-labelledby="chi-title">\n            <div class="results-heading"><div><p class="eyebrow">Chi-square results</p><h2 id="chi-title">Specify a comparison to begin</h2></div>\n              <button id="chi-download" class="button button-secondary" type="button" hidden>Download results</button></div>\n            <div id="chi-message" class="notice subtle">Choose two different categorical variables.</div>\n            <div id="chi-results" aria-live="polite"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-download"'

if (!grepl('id="panel-chi-square"', html, fixed = TRUE)) {
  if (!grepl(marker, html, fixed = TRUE)) stop("Could not locate the end of Models.")
  html <- sub(marker, panel, html, fixed = TRUE)
}
if (!grepl("assets/chi-square.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/chi-square.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

# Update model-tab routing.
tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
if (!grepl('"chi-square": document.getElementById("panel-chi-square")', tabs, fixed = TRUE)) {
  tabs <- sub(
    'anova: document.getElementById("panel-anova")',
    'anova: document.getElementById("panel-anova"),\n    "chi-square": document.getElementById("panel-chi-square")',
    tabs,
    fixed = TRUE
  )
}
tabs <- sub(
  '["linear-regression", "binary-logistic", "anova"].includes(requested)',
  '["linear-regression", "binary-logistic", "anova", "chi-square"].includes(requested)',
  tabs,
  fixed = TRUE
)
writeLines(tabs, tabs_path, useBytes = TRUE)

chi_js <- '"use strict";
const chiPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json"};
const chiState={data:null,variables:[],countries:[],lastRows:[]};
const cEsc=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const cFmt=(v,d=2)=>v==null||!Number.isFinite(v)?"Not available":new Intl.NumberFormat("en-NZ",{maximumFractionDigits:d}).format(v);
const cSelected=id=>[...document.getElementById(id).selectedOptions].map(o=>o.value);
function cParse(o){const n=Object.keys(o),l=o[n[0]].length;if(!n.length||!n.every(k=>o[k].length===l))throw Error("Invalid data");return{columns:o,length:l};}
function cLabel(v,x,countryName){if(v.id==="COUNTRY")return countryName;if(v.id==="Q289")return String(x);if(["Q165","Q166","Q167","Q168"].includes(v.id))return Number(x)===1?"Yes":"No";if(v.id==="Q260")return Number(x)===1?"Male":"Female";if(v.id==="Q263")return Number(x)===1?"Born in this country":"Immigrant to this country";if(Array.isArray(v.levels)){const m=v.levels.find(l=>Number(l.displayOrder)===Number(x));if(m)return m.label;}return String(x);}
function logGamma(z){const p=[.99999999999980993,676.5203681218851,-1259.1392167224028,771.32342877765313,-176.6150291621406,12.507343278686905,-.13857109526572012,9.9843695780195716e-6,1.5056327351493116e-7];if(z<.5)return Math.log(Math.PI)-Math.log(Math.sin(Math.PI*z))-logGamma(1-z);z-=1;let x=p[0];for(let i=1;i<p.length;i++)x+=p[i]/(z+i);const t=z+p.length-1.5;return .5*Math.log(2*Math.PI)+(z+.5)*Math.log(t)-t+Math.log(x);}
function gammaQ(a,x){if(x<0||a<=0)return null;if(x===0)return 1;if(x<a+1){let ap=a,sum=1/a,del=sum;for(let n=1;n<=200;n++){ap++;del*=x/ap;sum+=del;if(Math.abs(del)<Math.abs(sum)*1e-14)break;}return 1-sum*Math.exp(-x+a*Math.log(x)-logGamma(a));}let b=x+1-a,c=1/1e-300,d=1/b,h=d;for(let i=1;i<=200;i++){const an=-i*(i-a);b+=2;d=an*d+b;if(Math.abs(d)<1e-300)d=1e-300;c=b+an/c;if(Math.abs(c)<1e-300)c=1e-300;d=1/d;const del=d*c;h*=del;if(Math.abs(del-1)<1e-14)break;}return Math.exp(-x+a*Math.log(x)-logGamma(a))*h;}
function calculateChi(e){e.preventDefault();const rowId=document.getElementById("chi-row").value,colId=document.getElementById("chi-column").value;if(rowId===colId){document.getElementById("chi-message").textContent="Choose two different variables.";return;}const countries=new Set(cSelected("chi-countries")),rowVar=chiState.variables.find(v=>v.id===rowId),colVar=chiState.variables.find(v=>v.id===colId),rows=[],cols=[],counts=new Map();let n=0;for(let i=0;i<chiState.data.length;i++){const cc=chiState.data.columns.B_COUNTRY_ALPHA[i];if(countries.size&&!countries.has(cc))continue;const rawR=rowId==="COUNTRY"?cc:chiState.data.columns[rowId][i],rawC=colId==="COUNTRY"?cc:chiState.data.columns[colId][i];if(rawR===null||rawC===null||rawR===""||rawC==="")continue;const r=cLabel(rowVar,rawR,chiState.data.columns.B_COUNTRY[i]),c=cLabel(colVar,rawC,chiState.data.columns.B_COUNTRY[i]);if(!rows.includes(r))rows.push(r);if(!cols.includes(c))cols.push(c);const key=`${r}|||${c}`;counts.set(key,(counts.get(key)||0)+1);n++;}rows.sort();cols.sort();if(rows.length<2||cols.length<2){document.getElementById("chi-message").textContent="Each variable must have at least two observed categories.";return;}const rowTotals=rows.map(r=>cols.reduce((s,c)=>s+(counts.get(`${r}|||${c}`)||0),0)),colTotals=cols.map(c=>rows.reduce((s,r)=>s+(counts.get(`${r}|||${c}`)||0),0));let chi=0,small=0;const cells=[];rows.forEach((r,i)=>cols.forEach((c,j)=>{const observed=counts.get(`${r}|||${c}`)||0,expected=rowTotals[i]*colTotals[j]/n;if(expected<5)small++;chi+=(observed-expected)**2/expected;cells.push({row:r,column:c,observed,expected,rowPercentage:100*observed/rowTotals[i],columnPercentage:100*observed/colTotals[j]});}));const df=(rows.length-1)*(cols.length-1),p=gammaQ(df/2,chi/2),minDim=Math.min(rows.length-1,cols.length-1),cramers=Math.sqrt(chi/(n*minDim)),display=document.getElementById("chi-display").value,label=display==="count"?"Observed count":display==="row"?"Row %":display==="column"?"Column %":"Expected count",value=cell=>display==="count"?cFmt(cell.observed,0):display==="row"?`${cFmt(cell.rowPercentage,1)}%`:display==="column"?`${cFmt(cell.columnPercentage,1)}%`:cFmt(cell.expected,2);document.getElementById("chi-title").textContent=`${rowId} by ${colId}`;document.getElementById("chi-message").textContent=`Complete observations are used. ${small} of ${cells.length} expected counts are below 5.`;document.getElementById("chi-results").innerHTML=`<section class="regression-card"><div class="model-fit-grid"><div><strong>${cFmt(n,0)}</strong><span>Complete observations</span></div><div><strong>${cFmt(chi,3)}</strong><span>Chi-square</span></div><div><strong>${df}</strong><span>Degrees of freedom</span></div><div><strong>${p<.001?"&lt; 0.001":cFmt(p,3)}</strong><span>p-value</span></div><div><strong>${cFmt(cramers,3)}</strong><span>Cramers V</span></div></div>${small/cells.length>.2?`<p class="suppression-note">More than 20% of expected cell counts are below 5. Interpret the chi-square approximation cautiously.</p>`:""}<div class="table-wrap"><table class="results-table"><thead><tr><th>${cEsc(rowVar.displayName)}</th>${cols.map(c=>`<th>${cEsc(c)}</th>`).join("")}<th>Total</th></tr></thead><tbody>${rows.map((r,i)=>`<tr><td>${cEsc(r)}</td>${cols.map(c=>`<td>${value(cells.find(x=>x.row===r&&x.column===c))}</td>`).join("")}<td>${cFmt(rowTotals[i],0)}</td></tr>`).join("")}</tbody></table></div><p class="model-note">Cell display: ${label}. Cramers V describes the strength of association but not its direction.</p></section>`;document.getElementById("chi-download").hidden=false;chiState.lastRows=cells.map(cell=>({...cell,row_variable:rowId,column_variable:colId,total_n:n,chi_square:chi,degrees_freedom:df,p_value:p,cramers_v:cramers}));}
function downloadChi(){if(!chiState.lastRows.length)return;const cols=Object.keys(chiState.lastRows[0]),q=v=>`"${String(v??"").replaceAll("\\\"","\\\"\\\"")}"`,text=[cols.join(","),...chiState.lastRows.map(r=>cols.map(c=>q(r[c])).join(","))].join("\\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-chi-square.csv";a.click();URL.revokeObjectURL(a.href);}
function resetChi(){document.getElementById("chi-form").reset();document.getElementById("chi-row").value="Q165";document.getElementById("chi-column").value="Q260";[...document.getElementById("chi-countries").options].forEach(o=>o.selected=o.value==="NZL");document.getElementById("chi-results").innerHTML="";document.getElementById("chi-download").hidden=true;}
async function initialiseChi(){const loading=document.getElementById("chi-loading");try{const[d,c]=await Promise.all([fetch(chiPaths.data).then(r=>r.json()),fetch(chiPaths.codebook).then(r=>r.json())]);chiState.data=cParse(d);chiState.variables=[{id:"COUNTRY",displayName:"Country",analysisType:"nominal"},...c.variables.filter(v=>v.analysisType!=="integer")];const pairs=new Map();for(let i=0;i<chiState.data.length;i++)pairs.set(chiState.data.columns.B_COUNTRY_ALPHA[i],chiState.data.columns.B_COUNTRY[i]);chiState.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const row=document.getElementById("chi-row"),col=document.getElementById("chi-column");chiState.variables.forEach(v=>{const label=v.id==="COUNTRY"?"Country":`${v.id}: ${v.displayName}`;row.add(new Option(label,v.id));col.add(new Option(label,v.id));});row.value="Q165";col.value="Q260";const cs=document.getElementById("chi-countries");chiState.countries.forEach(c=>{const o=new Option(`${c.name} (${c.code})`,c.code);if(c.code==="NZL")o.selected=true;cs.add(o);});loading.hidden=true;document.getElementById("chi-form").hidden=false;}catch(error){loading.textContent=`Chi-square data could not be loaded: ${error.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("chi-form").addEventListener("submit",calculateChi);document.getElementById("chi-reset").addEventListener("click",resetChi);document.getElementById("chi-download").addEventListener("click",downloadChi);initialiseChi();});'

writeLines(chi_js, chi_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 21: chi-square", css, fixed = TRUE)) {
  cat('\n/* Step 21: chi-square */\n#chi-countries { min-height: 220px; }\n', file = css_path, append = TRUE)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c("chi_tab_added","chi_panel_added","script_created","two_variable_selectors","country_filter_added","expected_counts_added","cramers_v_added","small_cell_warning_added","tab_controller_updated"),
  passed = c(grepl('data-model-tab="chi-square"',updated_html,fixed=TRUE),grepl('id="panel-chi-square"',updated_html,fixed=TRUE),file.exists(chi_path),grepl('id="chi-row"',updated_html,fixed=TRUE)&&grepl('id="chi-column"',updated_html,fixed=TRUE),grepl('id="chi-countries"',updated_html,fixed=TRUE),grepl("expected=rowTotals",chi_js,fixed=TRUE),grepl("cramers",chi_js,fixed=TRUE),grepl("expected cell counts are below 5",chi_js,fixed=TRUE),grepl('"chi-square"',updated_tabs,fixed=TRUE)),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step21_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Step 21 validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
cat("\nStep 21 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
