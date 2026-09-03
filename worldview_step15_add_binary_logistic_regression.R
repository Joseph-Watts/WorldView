# WorldView Online - Step 15
# Add binary logistic regression as its own Models sub-tab.
# Supports one binary outcome and one to five numeric/binary/ordinal predictors.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
tabs_path <- file.path(app_dir, "assets", "model-tabs.js")
css_path <- file.path(app_dir, "assets", "styles.css")
logistic_path <- file.path(app_dir, "assets", "logistic.js")

required <- c(index_path, tabs_path, css_path,
              file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json"),
              file.path(app_dir, "data", "worldview-codebook-v1.0.0.json"))
if (!all(file.exists(required))) stop("Run Steps 12c and 14 first.")

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Add tab button.
old_tab <- '<button id="tab-linear-regression" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-linear-regression" data-model-tab="linear-regression">Linear regression</button>'
new_tabs <- paste0(old_tab,
  '\n        <button id="tab-binary-logistic" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-binary-logistic" data-model-tab="binary-logistic">Binary logistic regression</button>')
if (!grepl('data-model-tab="binary-logistic"', html, fixed = TRUE)) {
  if (!grepl(old_tab, html, fixed = TRUE)) stop("Linear regression tab not found.")
  html <- sub(old_tab, new_tabs, html, fixed = TRUE)
}

# Insert logistic panel after linear regression panel and before Models closes.
marker <- '      </form>\n      </div>\n    </section>\n\n    <section id="page-download"'
panel <- '      </form>\n      </div>\n\n      <div id="panel-binary-logistic" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-binary-logistic" hidden>\n        <div class="page-heading model-subheading">\n          <p class="eyebrow">Model a binary outcome</p>\n          <h2>Binary logistic regression</h2>\n          <p>Estimate the probability of a selected event using one to five predictor variables.</p>\n        </div>\n        <div id="logistic-loading" class="notice" aria-live="polite">Loading logistic regression data...</div>\n        <form id="logistic-form" class="analysis-layout" hidden>\n          <aside class="control-panel">\n            <h2>Specify model</h2>\n            <label class="field"><span>Binary outcome</span><select id="logistic-outcome" required></select></label>\n            <label class="field"><span>Predictor variables</span><select id="logistic-predictors" multiple size="8" required></select></label>\n            <p class="field-help">Select between one and five predictors. Use Ctrl or Command to select several.</p>\n            <label class="field"><span>Countries</span><select id="logistic-countries" multiple size="10"></select></label>\n            <p class="field-help">If none are selected, all countries are combined.</p>\n            <fieldset class="radio-group"><legend>Multiple-country output</legend>\n              <label><input type="radio" name="logistic-country-mode" value="combined" checked> Combine selected countries</label>\n              <label><input type="radio" name="logistic-country-mode" value="separate"> Show each country separately</label>\n            </fieldset>\n            <div class="button-row compact">\n              <button class="button button-primary" type="submit">Fit logistic regression</button>\n              <button id="logistic-reset" class="button button-secondary" type="button">Reset</button>\n            </div>\n          </aside>\n          <section class="results-panel" aria-labelledby="logistic-title">\n            <div class="results-heading"><div><p class="eyebrow">Logistic regression results</p><h2 id="logistic-title">Specify a model to begin</h2></div>\n              <button id="logistic-download" class="button button-secondary" type="button" hidden>Download results</button></div>\n            <div id="logistic-message" class="notice subtle">Choose a binary outcome, predictors, and countries.</div>\n            <div id="logistic-results" aria-live="polite"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-download"'
if (!grepl('id="panel-binary-logistic"', html, fixed = TRUE)) {
  if (!grepl(marker, html, fixed = TRUE)) stop("Could not locate the end of Models.")
  html <- sub(marker, panel, html, fixed = TRUE)
}
if (!grepl("assets/logistic.js", html, fixed = TRUE)) {
  html <- sub("</head>", '  <script src="assets/logistic.js" defer></script>\n</head>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

# Update tab controller.
tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
tabs <- sub(
  '"linear-regression": document.getElementById("panel-linear-regression")',
  '"linear-regression": document.getElementById("panel-linear-regression"),\n    "binary-logistic": document.getElementById("panel-binary-logistic")',
  tabs, fixed = TRUE
)
tabs <- sub(
  'activateModelTab(requested === "linear-regression" ? "linear-regression" : "correlation");',
  'activateModelTab(["linear-regression", "binary-logistic"].includes(requested) ? requested : "correlation");',
  tabs, fixed = TRUE
)
writeLines(tabs, tabs_path, useBytes = TRUE)

logistic_js <- '"use strict";
const lp={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json"};
const ls={data:null,variables:[],binary:[],countries:[],rows:[]};
const E=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const F=(v,d=3)=>v==null||!Number.isFinite(v)?"Not available":new Intl.NumberFormat("en-NZ",{minimumFractionDigits:d,maximumFractionDigits:d}).format(v);
const selected=id=>[...document.getElementById(id).selectedOptions].map(o=>o.value);
function parse(o){const n=Object.keys(o),l=o[n[0]].length;if(!n.length||!n.every(k=>o[k].length===l))throw Error("Invalid data");return{columns:o,length:l};}
function transpose(a){return a[0].map((_,i)=>a.map(r=>r[i]));}
function multiply(a,b){const bt=transpose(b);return a.map(r=>bt.map(c=>r.reduce((s,v,i)=>s+v*c[i],0)));}
function invert(a){const n=a.length,m=a.map((r,i)=>[...r,...Array.from({length:n},(_,j)=>i===j?1:0)]);for(let i=0;i<n;i++){let p=i;for(let r=i+1;r<n;r++)if(Math.abs(m[r][i])>Math.abs(m[p][i]))p=r;if(Math.abs(m[p][i])<1e-10)return null;[m[i],m[p]]=[m[p],m[i]];const q=m[i][i];m[i]=m[i].map(v=>v/q);for(let r=0;r<n;r++){if(r===i)continue;const f=m[r][i];m[r]=m[r].map((v,j)=>v-f*m[i][j]);}}return m.map(r=>r.slice(n));}
function sigmoid(z){if(z>=0){const e=Math.exp(-z);return 1/(1+e);}const e=Math.exp(z);return e/(1+e);}
function fit(outcome,predictors,codes,title){const allow=new Set(codes),cc=ls.data.columns.B_COUNTRY_ALPHA,X=[],y=[];let filtered=0;for(let i=0;i<ls.data.length;i++){if(allow.size&&!allow.has(cc[i]))continue;filtered++;const vals=[ls.data.columns[outcome][i],...predictors.map(p=>ls.data.columns[p][i])];if(vals.some(v=>v===null||v===""))continue;const nums=vals.map(Number);if(nums.every(Number.isFinite)){y.push(nums[0]);X.push([1,...nums.slice(1)]);}}const n=y.length,k=predictors.length+1,events=y.reduce((s,v)=>s+v,0),base={title,filtered,n,excluded:filtered-n,events,nonEvents:n-events};if(n<Math.max(20,k+5))return{...base,status:"insufficient"};if(events===0||events===n)return{...base,status:"one_class"};let beta=Array(k).fill(0),inv=null,converged=false,iterations=0;for(let iter=0;iter<50;iter++){iterations=iter+1;const p=X.map(r=>sigmoid(r.reduce((s,v,j)=>s+v*beta[j],0))),w=p.map(v=>Math.max(v*(1-v),1e-9)),h=Array.from({length:k},()=>Array(k).fill(0)),g=Array(k).fill(0);for(let i=0;i<n;i++){for(let a=0;a<k;a++){g[a]+=X[i][a]*(y[i]-p[i]);for(let b=0;b<k;b++)h[a][b]+=X[i][a]*w[i]*X[i][b];}}inv=invert(h);if(!inv)return{...base,status:"singular"};const step=multiply(inv,g.map(v=>[v])).map(r=>r[0]);beta=beta.map((v,j)=>v+step[j]);if(Math.max(...step.map(Math.abs))<1e-8){converged=true;break;}}if(!converged)return{...base,status:"not_converged"};const se=inv.map((r,i)=>Math.sqrt(r[i])),terms=["Intercept",...predictors],coef=beta.map((b,i)=>({term:terms[i],estimate:b,se:se[i],z:b/se[i],oddsRatio:Math.exp(b),orLower:Math.exp(b-1.959963984540054*se[i]),orUpper:Math.exp(b+1.959963984540054*se[i])}));const p=X.map(r=>sigmoid(r.reduce((s,v,j)=>s+v*beta[j],0))),logLik=y.reduce((s,v,i)=>s+v*Math.log(Math.max(p[i],1e-15))+(1-v)*Math.log(Math.max(1-p[i],1e-15)),0),mean=events/n,nullLL=y.reduce((s,v)=>s+v*Math.log(mean)+(1-v)*Math.log(1-mean),0);return{...base,status:"ok",iterations,coef,logLik,aic:-2*logLik+2*k,pseudoR2:1-logLik/nullLL};}
function card(r,out){if(r.status!=="ok")return`<section class="regression-card"><h3>${E(r.title)}</h3><p class="suppression-note">${r.status==="one_class"?"Only one outcome category is present.":r.status==="singular"?"The model matrix is singular.":r.status==="not_converged"?"The model did not converge.":"Too few complete observations are available."}</p></section>`;const rows=r.coef.map(c=>`<tr><td>${E(c.term)}</td><td>${F(c.estimate)}</td><td>${F(c.se)}</td><td>${F(c.z)}</td><td>${F(c.oddsRatio)}</td><td>${F(c.orLower)} to ${F(c.orUpper)}</td></tr>`).join("");return`<section class="regression-card"><h3>${E(r.title)}</h3><div class="model-fit-grid"><div><strong>${F(r.n,0)}</strong><span>Complete observations</span></div><div><strong>${F(r.events,0)}</strong><span>Events (${E(out.correlationRepresentation.split(";").pop().trim())})</span></div><div><strong>${F(r.aic)}</strong><span>AIC</span></div><div><strong>${F(r.pseudoR2)}</strong><span>McFadden pseudo R squared</span></div></div><div class="table-wrap"><table class="results-table"><thead><tr><th>Term</th><th>Log-odds</th><th>SE</th><th>z</th><th>Odds ratio</th><th>Approx. 95% OR CI</th></tr></thead><tbody>${rows}</tbody></table></div><p class="model-note">The event is coded 1. Odds ratios above 1 indicate higher odds of the event as the predictor increases; values below 1 indicate lower odds.</p><p class="model-note">Rows missing any model variable were excluded (${r.excluded}). Iterations: ${r.iterations}.</p></section>`;}
function calculate(e){e.preventDefault();const outcome=document.getElementById("logistic-outcome").value,preds=selected("logistic-predictors");if(!preds.length||preds.length>5||preds.includes(outcome)){document.getElementById("logistic-message").textContent=preds.length>5?"Select no more than five predictors.":preds.includes(outcome)?"The outcome cannot also be a predictor.":"Select at least one predictor.";return;}const countries=selected("logistic-countries"),map=new Map(ls.countries.map(c=>[c.code,c.name])),sep=document.querySelector("input[name=logistic-country-mode]:checked").value==="separate",specs=sep&&countries.length>1?countries.map(c=>({title:map.get(c)||c,codes:[c]})):[{title:countries.length?countries.map(c=>map.get(c)||c).join(", "):"All countries combined",codes:countries}],results=specs.map(s=>fit(outcome,preds,s.codes,s.title)),out=ls.binary.find(v=>v.id===outcome);document.getElementById("logistic-title").textContent=`${outcome} predicted by ${preds.join(", ")}`;document.getElementById("logistic-message").textContent=`Event coding: ${out.correlationRepresentation}.`;document.getElementById("logistic-results").innerHTML=results.map(r=>card(r,out)).join("");document.getElementById("logistic-download").hidden=false;ls.rows=results.flatMap(r=>r.status==="ok"?r.coef.map(c=>({country:r.title,outcome,predictors:preds.join(";"),term:c.term,log_odds:c.estimate,se:c.se,z:c.z,odds_ratio:c.oddsRatio,or_ci_lower:c.orLower,or_ci_upper:c.orUpper,n:r.n,events:r.events,aic:r.aic,pseudo_r_squared:r.pseudoR2})):[]);}
function download(){if(!ls.rows.length)return;const cols=Object.keys(ls.rows[0]),q=v=>`"${String(v??"").replaceAll("\\\"","\\\"\\\"")}"`,text=[cols.join(","),...ls.rows.map(r=>cols.map(c=>q(r[c])).join(","))].join("\\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-binary-logistic-regression.csv";a.click();URL.revokeObjectURL(a.href);}
function reset(){document.getElementById("logistic-form").reset();document.getElementById("logistic-outcome").value="Q167";[...document.getElementById("logistic-predictors").options].forEach(o=>o.selected=["Q165","Q260","Q262"].includes(o.value));[...document.getElementById("logistic-countries").options].forEach(o=>o.selected=o.value==="NZL");document.getElementById("logistic-results").innerHTML="";document.getElementById("logistic-download").hidden=true;}
function limit(){const s=document.getElementById("logistic-predictors"),x=[...s.selectedOptions];if(x.length>5){x.slice(5).forEach(o=>o.selected=false);document.getElementById("logistic-message").textContent="Select no more than five predictors.";}}
async function init(){const loading=document.getElementById("logistic-loading");try{const[d,c]=await Promise.all([fetch(lp.data).then(r=>r.json()),fetch(lp.codebook).then(r=>r.json())]);ls.data=parse(d);ls.variables=c.variables.filter(v=>v.correlationEligible);ls.binary=c.variables.filter(v=>v.analysisType==="binary");const pairs=new Map();for(let i=0;i<ls.data.length;i++)pairs.set(ls.data.columns.B_COUNTRY_ALPHA[i],ls.data.columns.B_COUNTRY[i]);ls.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const o=document.getElementById("logistic-outcome"),p=document.getElementById("logistic-predictors");ls.binary.forEach(v=>o.add(new Option(`${v.id}: ${v.displayName}`,v.id)));ls.variables.forEach(v=>p.add(new Option(`${v.id}: ${v.displayName}`,v.id)));o.value="Q167";[...p.options].forEach(x=>x.selected=["Q165","Q260","Q262"].includes(x.value));const cs=document.getElementById("logistic-countries");ls.countries.forEach(c=>{const x=new Option(`${c.name} (${c.code})`,c.code);if(c.code==="NZL")x.selected=true;cs.add(x);});loading.hidden=true;document.getElementById("logistic-form").hidden=false;}catch(e){loading.textContent=`Logistic regression data could not be loaded: ${e.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("logistic-form").addEventListener("submit",calculate);document.getElementById("logistic-reset").addEventListener("click",reset);document.getElementById("logistic-download").addEventListener("click",download);document.getElementById("logistic-predictors").addEventListener("change",limit);init();});
'
writeLines(logistic_js, logistic_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 15: binary logistic regression", css, fixed = TRUE)) {
  cat('\n/* Step 15: binary logistic regression */\n#logistic-predictors { min-height: 210px; }\n', file = css_path, append = TRUE)
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
validation <- data.frame(
  check=c("tab_added","panel_added","script_added","binary_outcome_selector","five_predictor_limit","irls_added","odds_ratios_added","country_modes_added","tab_controller_updated"),
  passed=c(grepl('data-model-tab="binary-logistic"',updated_html,fixed=TRUE),grepl('id="panel-binary-logistic"',updated_html,fixed=TRUE),file.exists(logistic_path),grepl('id="logistic-outcome"',updated_html,fixed=TRUE),grepl("preds.length>5",logistic_js,fixed=TRUE),grepl("for(let iter=0;iter<50",logistic_js,fixed=TRUE),grepl("oddsRatio",logistic_js,fixed=TRUE),grepl('name="logistic-country-mode"',updated_html,fixed=TRUE),grepl('"binary-logistic"',updated_tabs,fixed=TRUE)),
  stringsAsFactors=FALSE
)
write.csv(validation,file.path(app_dir,"step15_validation_checks.csv"),row.names=FALSE)
if(!all(validation$passed))stop("Step 15 validation failed: ",paste(validation$check[!validation$passed],collapse=", "))
cat("\nStep 15 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
