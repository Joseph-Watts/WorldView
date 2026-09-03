# WorldView Online - Step 18
# Add one-way ANOVA as a Models sub-tab.
# The selected scale-like variable is the outcome and country is the grouping predictor.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
tabs_path <- file.path(app_dir, "assets", "model-tabs.js")
css_path <- file.path(app_dir, "assets", "styles.css")
anova_path <- file.path(app_dir, "assets", "anova.js")

required <- c(index_path, tabs_path, css_path,
              file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
              file.path(app_dir, "data", "worldview-codebook-v1.0.0.json"))
if (!all(file.exists(required))) stop("Required app files were not found.")

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Add ANOVA tab after binary logistic regression.
old_tab <- '<button id="tab-binary-logistic" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-binary-logistic" data-model-tab="binary-logistic">Binary logistic regression</button>'
new_tabs <- paste0(old_tab,
  '\n        <button id="tab-anova" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-anova" data-model-tab="anova">One-way ANOVA</button>')
if (!grepl('data-model-tab="anova"', html, fixed = TRUE)) {
  if (!grepl(old_tab, html, fixed = TRUE)) stop("Binary logistic tab not found.")
  html <- sub(old_tab, new_tabs, html, fixed = TRUE)
}

marker <- '      </form>\n      </div>\n    </section>\n\n    <section id="page-download"'
panel <- '      </form>\n      </div>\n\n      <div id="panel-anova" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-anova" hidden>\n        <div class="page-heading model-subheading">\n          <p class="eyebrow">Compare country means</p>\n          <h2>One-way ANOVA</h2>\n          <p>Compare the mean of one numeric or scale-like outcome across two or more selected countries.</p>\n        </div>\n        <div id="anova-loading" class="notice" aria-live="polite">Loading ANOVA data...</div>\n        <form id="anova-form" class="analysis-layout" hidden>\n          <aside class="control-panel">\n            <h2>Specify comparison</h2>\n            <label class="field"><span>Outcome variable</span><select id="anova-outcome" required></select></label>\n            <label class="field"><span>Countries</span><select id="anova-countries" multiple size="12" required></select></label>\n            <p class="field-help">Select between two and ten countries. Country is the categorical predictor.</p>\n            <div class="button-row compact">\n              <button class="button button-primary" type="submit">Run ANOVA</button>\n              <button id="anova-reset" class="button button-secondary" type="button">Reset</button>\n            </div>\n          </aside>\n          <section class="results-panel" aria-labelledby="anova-title">\n            <div class="results-heading"><div><p class="eyebrow">ANOVA results</p><h2 id="anova-title">Specify a comparison to begin</h2></div>\n              <button id="anova-download" class="button button-secondary" type="button" hidden>Download results</button></div>\n            <div id="anova-message" class="notice subtle">Choose an outcome and at least two countries.</div>\n            <div id="anova-results" aria-live="polite"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-download"'
if (!grepl('id="panel-anova"', html, fixed = TRUE)) {
  if (!grepl(marker, html, fixed = TRUE)) stop("Could not locate the end of Models.")
  html <- sub(marker, panel, html, fixed = TRUE)
}
if (!grepl("assets/anova.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/anova.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

# Update tab controller.
tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
if (!grepl('anova: document.getElementById("panel-anova")', tabs, fixed = TRUE)) {
  tabs <- sub(
    '"binary-logistic": document.getElementById("panel-binary-logistic")',
    '"binary-logistic": document.getElementById("panel-binary-logistic"),\n    anova: document.getElementById("panel-anova")',
    tabs, fixed = TRUE
  )
}
tabs <- sub(
  '["linear-regression", "binary-logistic"].includes(requested)',
  '["linear-regression", "binary-logistic", "anova"].includes(requested)',
  tabs, fixed = TRUE
)
writeLines(tabs, tabs_path, useBytes = TRUE)

anova_js <- '"use strict";
const anovaPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json"};
const anovaState={data:null,variables:[],countries:[],lastRows:[]};
const anovaEscape=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const anovaFormat=(v,d=3)=>v==null||!Number.isFinite(v)?"Not available":new Intl.NumberFormat("en-NZ",{minimumFractionDigits:d,maximumFractionDigits:d}).format(v);
const anovaSelected=id=>[...document.getElementById(id).selectedOptions].map(o=>o.value);
function anovaParse(o){const names=Object.keys(o),length=o[names[0]].length;if(!names.length||!names.every(n=>o[n].length===length))throw Error("Invalid data");return{columns:o,length};}
function anovaNormalCdf(x){const s=x<0?-1:1,z=Math.abs(x)/Math.sqrt(2),t=1/(1+.3275911*z),e=1-(((((1.061405429*t-1.453152027)*t)+1.421413741)*t-.284496736)*t+.254829592)*t*Math.exp(-z*z);return .5*(1+s*e);}
function anovaFCdfApprox(f,df1,df2){if(!Number.isFinite(f)||f<0)return null;const z=(Math.log(f)-Math.log((df2)/(df2-2)))/Math.sqrt(2/df1+2/(df2-4));return anovaNormalCdf(z);}
function calculateAnova(e){e.preventDefault();const outcome=document.getElementById("anova-outcome").value,codes=anovaSelected("anova-countries");if(codes.length<2||codes.length>10){document.getElementById("anova-message").textContent="Select between two and ten countries.";return;}const map=new Map(anovaState.countries.map(c=>[c.code,c.name])),groups=codes.map(code=>{const values=[];for(let i=0;i<anovaState.data.length;i++){if(anovaState.data.columns.B_COUNTRY_ALPHA[i]!==code)continue;const value=anovaState.data.columns[outcome][i];if(value!==null&&value!==""){const numeric=Number(value);if(Number.isFinite(numeric))values.push(numeric);}}const mean=values.reduce((s,v)=>s+v,0)/values.length,variance=values.length>1?values.reduce((s,v)=>s+(v-mean)**2,0)/(values.length-1):null;return{code,name:map.get(code)||code,values,n:values.length,mean,sd:variance==null?null:Math.sqrt(variance)};});if(groups.some(g=>g.n<2)){document.getElementById("anova-message").textContent="Each selected country must have at least two valid observations.";return;}const totalN=groups.reduce((s,g)=>s+g.n,0),grand=groups.reduce((s,g)=>s+g.mean*g.n,0)/totalN,ssBetween=groups.reduce((s,g)=>s+g.n*(g.mean-grand)**2,0),ssWithin=groups.reduce((s,g)=>s+g.values.reduce((a,v)=>a+(v-g.mean)**2,0),0),dfBetween=groups.length-1,dfWithin=totalN-groups.length,msBetween=ssBetween/dfBetween,msWithin=ssWithin/dfWithin,f=msBetween/msWithin,eta2=ssBetween/(ssBetween+ssWithin),omega2=(ssBetween-dfBetween*msWithin)/(ssBetween+ssWithin+msWithin),pApprox=1-anovaFCdfApprox(f,dfBetween,dfWithin);const variable=anovaState.variables.find(v=>v.id===outcome);document.getElementById("anova-title").textContent=`${outcome} by country`;document.getElementById("anova-message").textContent=`Country is treated as the categorical predictor. ${variable.analysisType==="ordinal"?"The ordered outcome is treated as an equally spaced numerical scale.":""}`;document.getElementById("anova-results").innerHTML=`<section class="regression-card"><div class="model-fit-grid"><div><strong>${anovaFormat(totalN,0)}</strong><span>Valid observations</span></div><div><strong>${anovaFormat(f)}</strong><span>F(${dfBetween}, ${dfWithin})</span></div><div><strong>${pApprox<.001?"&lt; 0.001":anovaFormat(pApprox)}</strong><span>Approximate p-value</span></div><div><strong>${anovaFormat(eta2)}</strong><span>Eta squared</span></div><div><strong>${anovaFormat(omega2)}</strong><span>Omega squared</span></div></div><div class="table-wrap"><table class="results-table"><thead><tr><th>Country</th><th>Valid n</th><th>Mean</th><th>SD</th></tr></thead><tbody>${groups.map(g=>`<tr><td>${anovaEscape(g.name)}</td><td>${anovaFormat(g.n,0)}</td><td>${anovaFormat(g.mean)}</td><td>${anovaFormat(g.sd)}</td></tr>`).join("")}</tbody></table></div><h4>ANOVA table</h4><div class="table-wrap"><table class="results-table"><thead><tr><th>Source</th><th>SS</th><th>df</th><th>MS</th><th>F</th></tr></thead><tbody><tr><td>Between countries</td><td>${anovaFormat(ssBetween)}</td><td>${dfBetween}</td><td>${anovaFormat(msBetween)}</td><td>${anovaFormat(f)}</td></tr><tr><td>Within countries</td><td>${anovaFormat(ssWithin)}</td><td>${dfWithin}</td><td>${anovaFormat(msWithin)}</td><td></td></tr></tbody></table></div><p class="model-note">This omnibus test indicates whether at least one selected country mean differs. It does not identify which pairs differ. The p-value is a browser approximation.</p></section>`;document.getElementById("anova-download").hidden=false;anovaState.lastRows=groups.map(g=>({outcome,country:g.name,country_code:g.code,valid_n:g.n,mean:g.mean,standard_deviation:g.sd,f_statistic:f,df_between:dfBetween,df_within:dfWithin,approximate_p_value:pApprox,eta_squared:eta2,omega_squared:omega2}));}
function downloadAnova(){if(!anovaState.lastRows.length)return;const cols=Object.keys(anovaState.lastRows[0]),quote=v=>`"${String(v??"").replaceAll("\\\"","\\\"\\\"")}"`,text=[cols.join(","),...anovaState.lastRows.map(r=>cols.map(c=>quote(r[c])).join(","))].join("\\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-one-way-anova.csv";a.click();URL.revokeObjectURL(a.href);}
function resetAnova(){document.getElementById("anova-form").reset();document.getElementById("anova-outcome").value="Q177";[...document.getElementById("anova-countries").options].forEach(o=>o.selected=["AUS","NZL"].includes(o.value));document.getElementById("anova-results").innerHTML="";document.getElementById("anova-download").hidden=true;}
function limitAnovaCountries(){const options=[...document.getElementById("anova-countries").selectedOptions];if(options.length>10){options.slice(10).forEach(o=>o.selected=false);document.getElementById("anova-message").textContent="Select no more than ten countries.";}}
async function initialiseAnova(){const loading=document.getElementById("anova-loading");try{const[d,c]=await Promise.all([fetch(anovaPaths.data).then(r=>r.json()),fetch(anovaPaths.codebook).then(r=>r.json())]);anovaState.data=anovaParse(d);anovaState.variables=c.variables.filter(v=>v.analysisType==="integer"||v.analysisType==="ordinal");const pairs=new Map();for(let i=0;i<anovaState.data.length;i++)pairs.set(anovaState.data.columns.B_COUNTRY_ALPHA[i],anovaState.data.columns.B_COUNTRY[i]);anovaState.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const outcome=document.getElementById("anova-outcome");anovaState.variables.forEach(v=>outcome.add(new Option(`${v.id}: ${v.displayName}`,v.id)));outcome.value="Q177";const countries=document.getElementById("anova-countries");anovaState.countries.forEach(c=>{const option=new Option(`${c.name} (${c.code})`,c.code);if(["AUS","NZL"].includes(c.code))option.selected=true;countries.add(option);});loading.hidden=true;document.getElementById("anova-form").hidden=false;}catch(error){loading.textContent=`ANOVA data could not be loaded: ${error.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("anova-form").addEventListener("submit",calculateAnova);document.getElementById("anova-reset").addEventListener("click",resetAnova);document.getElementById("anova-download").addEventListener("click",downloadAnova);document.getElementById("anova-countries").addEventListener("change",limitAnovaCountries);initialiseAnova();});'
writeLines(anova_js, anova_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 18: one-way ANOVA", css, fixed = TRUE)) cat('\n/* Step 18: one-way ANOVA */\n#anova-countries { min-height: 260px; }\n', file=css_path, append=TRUE)

updated_html <- paste(readLines(index_path,warn=FALSE),collapse="\n")
updated_tabs <- paste(readLines(tabs_path,warn=FALSE),collapse="\n")
checks <- data.frame(check=c("anova_tab_added","anova_panel_added","anova_script_added","numeric_outcomes_only","country_limit_added","anova_calculation_added","effect_sizes_added","tab_controller_updated"),passed=c(grepl('data-model-tab="anova"',updated_html,fixed=TRUE),grepl('id="panel-anova"',updated_html,fixed=TRUE),file.exists(anova_path),grepl('v.analysisType==="integer"||v.analysisType==="ordinal"',anova_js,fixed=TRUE),grepl("options.length>10",anova_js,fixed=TRUE),grepl("ssBetween",anova_js,fixed=TRUE),grepl("omega2",anova_js,fixed=TRUE),grepl('"anova"',updated_tabs,fixed=TRUE)))
write.csv(checks,file.path(app_dir,"step18_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 18 validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 18 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
