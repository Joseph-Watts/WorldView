# WorldView Online - Step 28c
# Completes requested UI changes after Step 28/28b.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir,"index.html")
css_path <- file.path(app_dir,"assets","styles.css")
codebook_path <- file.path(app_dir,"data","worldview-codebook-v1.0.0.json")
data_path <- file.path(app_dir,"data","worldview-browser-data-v1.0.0.json")
bar_path <- file.path(app_dir,"assets","visualisations.js")
models_path <- file.path(app_dir,"assets","models.js")
helper_path <- file.path(app_dir,"assets","step28-enhancements.js")
about_path <- file.path(app_dir,"assets","about.js")
if(!all(file.exists(c(index_path,css_path,codebook_path,data_path,bar_path,models_path)))) stop("Required app files are missing.")
if(!requireNamespace("jsonlite",quietly=TRUE)) stop("Install jsonlite first.")

html <- paste(readLines(index_path,warn=FALSE),collapse="\n")
css <- paste(readLines(css_path,warn=FALSE),collapse="\n")
codebook <- jsonlite::fromJSON(codebook_path,simplifyVector=FALSE)
browser <- jsonlite::fromJSON(data_path,simplifyVector=TRUE,simplifyDataFrame=FALSE,simplifyMatrix=FALSE)
requested <- c(paste0("Q",18:26),"Q238")

# Add requested variables from the best available local source. The script never
# fabricates values and requires exact row alignment.
source_candidates <- c(
 file.path(app_dir,"data","worldview-wave7-teaching-data-v1.0.0.csv"),
 file.path("WVS_Dataset","WVS7_Individual.csv"),"WVS7_Individual.csv"
)
source_file <- source_candidates[file.exists(source_candidates)][1]
added <- character(); missing <- requested
if(length(source_file)&&!is.na(source_file)){
 src <- read.csv(source_file,stringsAsFactors=FALSE,check.names=FALSE)
 available <- intersect(requested,names(src)); missing <- setdiff(requested,available)
 if(length(available)){
   if(nrow(src)!=length(browser[[1]])) stop("Source containing requested variables has a different row count; cannot safely align respondents.")
   labels <- c(Q18="One of main goals in life has been to make parents proud",Q19="Parents duty to do their best for their children",Q20="Respect and love for parents regardless of qualities and faults",Q21="Men make better political leaders than women do",Q22="University is more important for a boy than for a girl",Q23="Men make better business executives than women do",Q24="Being a housewife is just as fulfilling as working for pay",Q25="When jobs are scarce, men should have more right to a job",Q26="Problem if women have more income than husbands",Q238="Having a strong leader who does not have to bother with parliament and elections")
   for(id in available){
     browser[[id]] <- unname(src[[id]])
     if(!any(vapply(codebook$variables,function(v)identical(v$id,id),logical(1)))) codebook$variables[[length(codebook$variables)+1L]] <- list(id=id,displayName=unname(labels[id]),analysisType="ordinal",correlationEligible=TRUE,correlationRepresentation="Original WVS response coding",topic=if(id=="Q238")"Political culture and political regimes" else "Social values, norms and stereotypes")
     added <- c(added,id)
   }
 }
}
if(length(missing)) stop("Requested variables are absent from the available respondent-level source: ",paste(missing,collapse=", "),". Place a row-aligned WVS7_Individual.csv containing these columns in the project root and rerun.")
jsonlite::write_json(browser,data_path,pretty=FALSE,auto_unbox=TRUE,na="null",null="null",digits=NA)
jsonlite::write_json(codebook,codebook_path,pretty=TRUE,auto_unbox=TRUE,na="null",null="null",digits=NA)

# Provide more space below the correlation-method selector.
css <- paste0(css,'\n/* Step 28c */\n.correlation-method-control { margin-bottom: 34px; }\n.correlation-method-control .field-help { display:block; margin-top:10px; }\n.codebook-topic { margin: 0 0 24px; }\n.codebook-topic > summary { cursor:pointer; padding:12px 14px; border-radius:10px; background:#edf4f2; color:var(--teal-dark); font-weight:800; }\n.about-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(240px,1fr)); gap:18px; }\n.about-card { padding:18px; border:1px solid var(--line); border-radius:14px; background:#fff; }\n')
writeLines(css,css_path,useBytes=TRUE)

# Remove Download navigation and page.
html <- gsub('<[^>]+data-page="download"[^>]*>.*?</[^>]+>','',html,perl=TRUE)
html <- gsub('<a[^>]+href="#download"[^>]*>.*?</a>','',html,perl=TRUE)
html <- sub('<section id="page-download"[\\s\\S]*?</section>','',html,perl=TRUE)

# Add About navigation and page before closing main.
if(!grepl('data-page="about"',html,fixed=TRUE)){
 nav_anchor <- '</nav>'
 html <- sub(nav_anchor,'  <button class="nav-link" type="button" data-page="about">About</button>\n    </nav>',html,fixed=TRUE)
}
if(!grepl('id="page-about"',html,fixed=TRUE)){
 about <- '<section id="page-about" class="page" hidden><div class="page-heading"><p class="eyebrow">About WorldView</p><h1>About</h1><p>WorldView is a teaching application for exploring and visualising World Values Survey data without requiring users to process the source data or write R code.</p></div><div class="about-grid"><article class="about-card"><h2>Purpose</h2><p>The application supports teaching in culture and cognition by providing summary statistics, cross-tabulations, visualisations, phylogenetic and geographic comparisons, and introductory statistical models.</p></article><article class="about-card"><h2>Development</h2><p>WorldView was developed by Joseph Watts at the University of Canterbury. The application builds on the World Values Survey and open-source R and web technologies.</p><p>Additional contributors should be listed here when their preferred credit wording has been confirmed.</p></article><article class="about-card"><h2>Use and interpretation</h2><p>The application is intentionally flexible. Users remain responsible for selecting analyses appropriate to the variables, assumptions, and research question.</p></article><article class="about-card"><h2>Sources</h2><p>Survey data and question wording are derived from World Values Survey Wave 7. Country boundaries use Natural Earth. The phylogeny uses the country-language tree and mapping supplied with the WorldView project.</p></article></div></section>'
 html <- sub('</main>',paste0(about,'\n  </main>'),html,fixed=TRUE)
}

# Replace Bar Graph data download label. JS patch exports the rendered SVG as PNG.
html <- gsub('>Download graph data</button>','>Save graph image</button>',html,fixed=TRUE)
bar <- paste(readLines(bar_path,warn=FALSE),collapse="\n")
# Add a generic exporter and redirect the existing download button listener.
if(!grepl("function saveBarGraphImage",bar,fixed=TRUE)){
 bar <- paste0(bar,'\nfunction saveBarGraphImage(){const svg=document.querySelector("#visual-results svg, #visual-chart svg, .chart-container svg");if(!svg)return;const box=svg.viewBox&&svg.viewBox.baseVal.width?svg.viewBox.baseVal:{width:svg.clientWidth||1000,height:svg.clientHeight||600},clone=svg.cloneNode(true);clone.setAttribute("xmlns","http://www.w3.org/2000/svg");clone.setAttribute("width",box.width);clone.setAttribute("height",box.height);const blob=new Blob([new XMLSerializer().serializeToString(clone)],{type:"image/svg+xml;charset=utf-8"}),url=URL.createObjectURL(blob),img=new Image(),canvas=document.createElement("canvas"),scale=2;canvas.width=box.width*scale;canvas.height=box.height*scale;img.onload=()=>{const c=canvas.getContext("2d");c.scale(scale,scale);c.fillStyle="#fff";c.fillRect(0,0,box.width,box.height);c.drawImage(img,0,0);const a=document.createElement("a");a.download="worldview-bar-graph.png";a.href=canvas.toDataURL("image/png");a.click();URL.revokeObjectURL(url);};img.src=url;}\ndocument.addEventListener("DOMContentLoaded",()=>{const b=[...document.querySelectorAll("button")].find(x=>x.textContent.trim()==="Save graph image");if(b){const n=b.cloneNode(true);b.replaceWith(n);n.addEventListener("click",saveBarGraphImage);}});\n')
}
writeLines(bar,bar_path,useBytes=TRUE)

# Replace Step 28 helper with a real topic nesting pass for the visible codebook.
helper <- '"use strict";
function topicFor(id){const n=Number(String(id).replace(/^Q/,""));if(!Number.isFinite(n))return"Technical and derived variables";if(n<=45)return"Social values, norms and stereotypes";if(n<=56)return"Happiness and wellbeing";if(n<=105)return"Social capital, trust and organisational membership";if(n<=111)return"Economic values";if(n<=120)return"Perceptions of corruption";if(n<=130)return"Perceptions of migration";if(n<=151)return"Perceptions of security";if(n<=157)return"Postmaterialism";if(n<=163)return"Science and technology";if(n<=175)return"Religious values";if(n<=198)return"Ethical values";if(n<=234)return"Political interest and participation";if(n<=259)return"Political culture and political regimes";return"Demographic and socioeconomic variables";}
function groupSelects(cb){const topics=new Map(cb.variables.map(v=>[v.id,v.topic||topicFor(v.id)]));document.querySelectorAll("select").forEach(s=>{const opts=[...s.options].filter(o=>topics.has(o.value));if(opts.length<3)return;const chosen=new Set([...s.selectedOptions].map(o=>o.value)),groups=new Map();opts.forEach(o=>{const t=topics.get(o.value);if(!groups.has(t))groups.set(t,[]);groups.get(t).push([o.value,o.text]);o.remove();});[...groups].forEach(([t,items])=>{const g=document.createElement("optgroup");g.label=t;items.forEach(([v,text])=>{const o=new Option(text,v);o.selected=chosen.has(v);g.append(o);});s.append(g);});});}
function nestCodebook(cb){const page=document.getElementById("page-codebook");if(!page||page.dataset.grouped)return;const elements=[...page.querySelectorAll("[data-variable-id], article, .codebook-card, .variable-card")].filter(el=>{const text=el.dataset.variableId||el.textContent;return /Q\\d+/.test(text);});if(elements.length<2)return;const groups=new Map();elements.forEach(el=>{const id=(el.dataset.variableId||el.textContent.match(/Q\\d+/)?.[0]);if(!id)return;const t=cb.variables.find(v=>v.id===id)?.topic||topicFor(id);if(!groups.has(t))groups.set(t,[]);groups.get(t).push(el);});const host=document.createElement("div");host.className="codebook-topics";[...groups].forEach(([topic,els])=>{const d=document.createElement("details");d.className="codebook-topic";d.open=true;const s=document.createElement("summary");s.textContent=`${topic} (${els.length})`;d.append(s);els.forEach(el=>d.append(el));host.append(d);});page.append(host);page.dataset.grouped="true";}
document.addEventListener("DOMContentLoaded",async()=>{const cb=await fetch("data/worldview-codebook-v1.0.0.json").then(r=>r.json());setTimeout(()=>{groupSelects(cb);nestCodebook(cb);},900);});'
writeLines(helper,helper_path,useBytes=TRUE)
if(!grepl("assets/step28-enhancements.js",html,fixed=TRUE)) html <- sub("</head>",'  <script src="assets/step28-enhancements.js" defer></script>\n</head>',html,fixed=TRUE)
writeLines(html,index_path,useBytes=TRUE)

checks <- data.frame(check=c("requested_variables_present","correlation_spacing_added","codebook_topic_nesting_added","about_page_added","download_page_removed","bar_image_button_added","bar_png_export_added"),passed=c(all(requested%in%names(browser)),grepl("margin-bottom: 34px",css,fixed=TRUE),grepl("function nestCodebook",helper,fixed=TRUE),grepl('id="page-about"',html,fixed=TRUE),!grepl('id="page-download"',html,fixed=TRUE),grepl("Save graph image",html,fixed=TRUE),grepl('toDataURL("image/png")',bar,fixed=TRUE)),stringsAsFactors=FALSE)
write.csv(checks,file.path(app_dir,"step28c_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 28c validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 28c completed successfully.\nRestart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
