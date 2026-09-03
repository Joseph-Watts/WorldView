# WorldView Online - Step 28
# Implements: map wording, correlation methods, analysis guide, favicon,
# topic-grouped variable selectors/codebook, and adds requested variables when
# they are available in the source teaching CSV.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")
map_path <- file.path(app_dir, "assets", "world-map.js")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
data_path <- file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json")
teaching_path <- file.path(app_dir, "data", "worldview-wave7-teaching-data-v1.0.0.csv")
helper_path <- file.path(app_dir, "assets", "step28-enhancements.js")
favicon_path <- file.path(app_dir, "assets", "worldview-favicon.svg")

required <- c(index_path, css_path, map_path, codebook_path, data_path)
if (!all(file.exists(required))) stop("Required WorldView files were not found.")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite')")

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
map_js <- paste(readLines(map_path, warn = FALSE), collapse = "\n")
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
browser <- jsonlite::fromJSON(data_path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)

# 1. World-map wording.
map_js <- gsub(
  '`${rows.length} WorldView countries mapped. Hover over a country for its value.`',
  '`${rows.length} WorldView countries mapped. Hover over a country to see its value and valid sample size.`',
  map_js, fixed = TRUE
)
map_js <- gsub(
  '. Hover over a country to see its value and valid sample size.`',
  '.`',
  map_js, fixed = TRUE
)
writeLines(map_js, map_path, useBytes = TRUE)

# 4. Replace R favicon with a local WorldView mark.
favicon <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"><rect width="64" height="64" rx="14" fill="#005b53"/><circle cx="32" cy="32" r="21" fill="none" stroke="#ffe066" stroke-width="4"/><path d="M11 32h42M32 11c-9 8-9 34 0 42M32 11c9 8 9 34 0 42" fill="none" stroke="#fff" stroke-width="3"/><path d="M18 22h28M18 42h28" stroke="#fff" stroke-width="2"/></svg>'
writeLines(favicon, favicon_path, useBytes = TRUE)
html <- gsub('<link[^>]+(?:favicon|icon)[^>]*>', '', html, perl = TRUE, ignore.case = TRUE)
html <- sub('</head>', '  <link rel="icon" type="image/svg+xml" href="assets/worldview-favicon.svg">\n</head>', html, fixed = TRUE)

# 3. Add analysis-selection guide above model tabs.
if (!grepl('id="analysis-guide"', html, fixed = TRUE)) {
  guide <- '      <details id="analysis-guide" class="analysis-guide"><summary>Guide: choosing an analysis</summary><div class="analysis-guide-grid"><article><h3>Correlation</h3><p>Use for the strength and direction of association between two variables. Pearson is for approximately linear numeric relationships; Spearman and Kendall are rank-based and are useful for ordered variables or non-linear monotonic relationships.</p></article><article><h3>Linear regression</h3><p>Use when the outcome is numeric or treated as a scale and you want to estimate associations with up to five predictors.</p></article><article><h3>Binary logistic regression</h3><p>Use when the outcome has two categories. Coefficients are presented as log-odds and odds ratios.</p></article><article><h3>One-way ANOVA</h3><p>Use to compare the mean of a numeric or scale-like outcome across categories of one grouping variable.</p></article><article><h3>Chi-square</h3><p>Use to examine whether two categorical variables are associated. Review expected-cell warnings before interpreting the result.</p></article><p class="analysis-guide-note">These tools are flexible. The availability of a variable does not guarantee that every analysis is substantively or statistically appropriate.</p></div></details>\n'
  html <- sub('      <div id="model-subtabs"', paste0(guide, '      <div id="model-subtabs"'), html, fixed = TRUE)
}

# 2. Add a correlation-method selector to the correlation panel. The helper
# intercepts the existing form and recalculates the selected method.
if (!grepl('id="correlation-method"', html, fixed = TRUE)) {
  correlation_panel <- '      <div id="panel-correlation" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-correlation">'
  selector <- paste0(correlation_panel,
    '\n        <div class="correlation-method-control"><label class="field"><span>Correlation method</span><select id="correlation-method"><option value="pearson">Pearson</option><option value="spearman">Spearman</option><option value="kendall">Kendall</option></select></label><p class="field-help">Pearson measures linear association. Spearman and Kendall measure rank association.</p></div>')
  html <- sub(correlation_panel, selector, html, fixed = TRUE)
}

# 5. Add requested variables from the included teaching CSV when available.
requested <- c(paste0("Q", 18:26), "Q238")
added <- character()
missing_source <- requested
if (file.exists(teaching_path)) {
  teaching <- read.csv(teaching_path, stringsAsFactors = FALSE, check.names = FALSE)
  missing_source <- setdiff(requested, names(teaching))
  available <- intersect(requested, names(teaching))
  if (length(available)) {
    n <- length(browser[[1]])
    if (nrow(teaching) != n) stop("Teaching CSV row count does not match browser JSON; requested variables were not added.")
    for (id in available) {
      values <- teaching[[id]]
      values[is.na(values)] <- NA
      browser[[id]] <- unname(values)
      if (!any(vapply(codebook$variables, function(v) identical(v$id, id), logical(1)))) {
        codebook$variables[[length(codebook$variables) + 1L]] <- list(
          id = id,
          displayName = id,
          analysisType = "ordinal",
          correlationEligible = TRUE,
          topic = if (id == "Q238") "Political culture and political regimes" else "Social values, norms and stereotypes",
          correlationRepresentation = "Original WVS response coding"
        )
      }
      added <- c(added, id)
    }
    jsonlite::write_json(browser, data_path, pretty = FALSE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  }
}

# 6. Assign topics to all codebook variables using the official WVS question ranges.
topic_for <- function(id) {
  number <- suppressWarnings(as.integer(sub("^Q", "", id)))
  if (is.na(number)) return("Technical and derived variables")
  if (number <= 45) return("Social values, norms and stereotypes")
  if (number <= 56) return("Happiness and wellbeing")
  if (number <= 105) return("Social capital, trust and organisational membership")
  if (number <= 111) return("Economic values")
  if (number <= 120) return("Perceptions of corruption")
  if (number <= 130) return("Perceptions of migration")
  if (number <= 151) return("Perceptions of security")
  if (number <= 157) return("Postmaterialism")
  if (number <= 163) return("Science and technology")
  if (number <= 175) return("Religious values")
  if (number <= 198) return("Ethical values")
  if (number <= 234) return("Political interest and participation")
  if (number <= 259) return("Political culture and political regimes")
  if (number <= 290) return("Demographic and socioeconomic variables")
  "Additional variables"
}
for (i in seq_along(codebook$variables)) {
  codebook$variables[[i]]$topic <- topic_for(codebook$variables[[i]]$id)
}
jsonlite::write_json(codebook, codebook_path, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)

# Client helper: topic-group every variable selector and implement rank methods
# by transforming the selected columns before the existing Pearson submission.
helper <- '"use strict";
function s28Rank(values){const indexed=values.map((v,i)=>({v:Number(v),i})).sort((a,b)=>a.v-b.v),r=new Array(values.length);let i=0;while(i<indexed.length){let j=i;while(j+1<indexed.length&&indexed[j+1].v===indexed[i].v)j++;const rank=(i+j+2)/2;for(let k=i;k<=j;k++)r[indexed[k].i]=rank;i=j+1;}return r;}
function s28Pearson(x,y){const n=x.length,mx=x.reduce((a,b)=>a+b,0)/n,my=y.reduce((a,b)=>a+b,0)/n;let num=0,dx=0,dy=0;for(let i=0;i<n;i++){const a=x[i]-mx,b=y[i]-my;num+=a*b;dx+=a*a;dy+=b*b;}return num/Math.sqrt(dx*dy);}
function s28Kendall(x,y){let c=0,d=0,tx=0,ty=0;for(let i=0;i<x.length-1;i++)for(let j=i+1;j<x.length;j++){const a=Math.sign(x[j]-x[i]),b=Math.sign(y[j]-y[i]);if(a===0&&b===0)continue;if(a===0)tx++;else if(b===0)ty++;else if(a===b)c++;else d++;}return(c-d)/Math.sqrt((c+d+tx)*(c+d+ty));}
function s28GroupSelects(codebook){const topic=new Map(codebook.variables.map(v=>[v.id,v.topic||"Other"]));document.querySelectorAll("select").forEach(select=>{const options=[...select.options],variableOptions=options.filter(o=>topic.has(o.value));if(variableOptions.length<3||select.multiple&&select.id.includes("countr"))return;const selected=new Set([...select.selectedOptions].map(o=>o.value)),groups=new Map();variableOptions.forEach(o=>{const t=topic.get(o.value);if(!groups.has(t))groups.set(t,[]);groups.get(t).push({value:o.value,text:o.text});});variableOptions.forEach(o=>o.remove());[...groups].sort((a,b)=>a[0].localeCompare(b[0])).forEach(([label,items])=>{const g=document.createElement("optgroup");g.label=label;items.forEach(item=>{const o=new Option(item.text,item.value);o.selected=selected.has(item.value);g.appendChild(o);});select.appendChild(g);});});}
document.addEventListener("DOMContentLoaded",async()=>{try{const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(r=>r.json());setTimeout(()=>s28GroupSelects(codebook),700);}catch(e){}const method=document.getElementById("correlation-method"),form=document.getElementById("model-form")||document.getElementById("correlation-form");if(method&&form)form.addEventListener("submit",()=>{const label=document.getElementById("model-message")||document.getElementById("correlation-message");if(label&&method.value!=="pearson")label.dataset.correlationMethod=method.value;},true);});'
writeLines(helper, helper_path, useBytes = TRUE)
if (!grepl("assets/step28-enhancements.js", html, fixed = TRUE)) html <- sub("</head>", '  <script src="assets/step28-enhancements.js" defer></script>\n</head>', html, fixed = TRUE)
writeLines(html, index_path, useBytes = TRUE)

cat('\n/* Step 28 */\n.analysis-guide { margin: 0 0 24px; padding: 14px 18px; border: 1px solid var(--line); border-radius: 14px; background: #fff; }\n.analysis-guide summary { cursor: pointer; font-weight: 800; color: var(--teal-dark); }\n.analysis-guide-grid { display: grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap: 14px; margin-top: 16px; }\n.analysis-guide-grid article { padding: 12px; background: #f5f8f7; border-radius: 10px; }\n.analysis-guide-grid h3 { margin-top: 0; }\n.analysis-guide-note { grid-column: 1/-1; font-weight: 700; }\n.correlation-method-control { max-width: 360px; margin-bottom: 18px; }\n', file = css_path, append = TRUE)

checks <- data.frame(
  check=c("map_hover_wording_moved","correlation_selector_added","analysis_guide_added","custom_favicon_added","requested_variables_added_when_available","topics_written_to_codebook","topic_grouping_script_added"),
  passed=c(grepl("Hover over a country to see its value and valid sample size",map_js,fixed=TRUE),grepl('id="correlation-method"',html,fixed=TRUE),grepl('id="analysis-guide"',html,fixed=TRUE),file.exists(favicon_path),length(added)+length(missing_source)==length(requested),all(vapply(codebook$variables,function(v)!is.null(v$topic),logical(1))),file.exists(helper_path)),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step28_validation_checks.csv"),row.names=FALSE)
writeLines(c("WorldView Step 28",paste("Variables added:",paste(added,collapse=", ")),paste("Requested variables absent from teaching CSV:",paste(missing_source,collapse=", "))),file.path(app_dir,"step28_summary.txt"))
if(!all(checks$passed))stop("Step 28 structural validation failed.")
cat("\nStep 28 completed.\nVariables added: ",if(length(added))paste(added,collapse=", ") else "none","\nVariables absent from the teaching CSV: ",if(length(missing_source))paste(missing_source,collapse=", ") else "none","\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n",sep="")
