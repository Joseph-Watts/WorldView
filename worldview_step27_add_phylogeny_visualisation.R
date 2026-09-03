# WorldView Online - Step 27
# Add a Phylogeny sub-tab under Visualisations.
# Before running, place these files in the working directory or their original paths:
#   country_phylogeny_tree.tree
#   country_phylogeny.csv

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
tabs_path <- file.path(app_dir, "assets", "visualisation-tabs.js")
css_path <- file.path(app_dir, "assets", "styles.css")
js_path <- file.path(app_dir, "assets", "phylogeny.js")
data_dir <- file.path(app_dir, "data", "phylogeny")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

find_first <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit)) hit[[1]] else NA_character_
}

tree_source <- find_first(c(
  "country_phylogeny_tree.tree",
  file.path("WVS_Dataset", "phylogeny", "country_phylogeny_tree.tree"),
  file.path("data", "phylogeny", "country_phylogeny_tree.tree")
))
map_source <- find_first(c(
  "country_phylogeny.csv",
  file.path("WVS_Dataset", "phylogeny", "country_phylogeny.csv"),
  file.path("data", "phylogeny", "country_phylogeny.csv")
))

if (is.na(tree_source)) stop("country_phylogeny_tree.tree was not found in the working directory or known phylogeny folders.")
if (is.na(map_source)) stop("country_phylogeny.csv was not found in the working directory or known phylogeny folders.")
if (!all(file.exists(c(index_path, tabs_path, css_path)))) stop("Run Steps 25 and 26 first.")

# Basic Newick checks before copying.
tree_text <- paste(readLines(tree_source, warn = FALSE), collapse = "")
if (!grepl(";\\s*$", tree_text) || !grepl("\\(", tree_text) || !grepl("\\)", tree_text)) {
  stop("The tree file does not appear to contain a complete Newick tree.")
}
map_data <- read.csv(map_source, stringsAsFactors = FALSE, check.names = FALSE)
required_map <- c("country_name", "iso3166alpha3", "glottocode", "language_name", "family_language_name")
if (!all(required_map %in% names(map_data))) stop("country_phylogeny.csv is missing required columns.")

file.copy(tree_source, file.path(data_dir, "country_phylogeny_tree.tree"), overwrite = TRUE)
file.copy(map_source, file.path(data_dir, "country_phylogeny.csv"), overwrite = TRUE)

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
old_tab <- '<button id="tab-visual-map" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-visual-map" data-visual-tab="map">World map</button>'
new_tabs <- paste0(old_tab, '\n        <button id="tab-visual-phylogeny" class="model-tab" type="button" role="tab" aria-selected="false" aria-controls="panel-visual-phylogeny" data-visual-tab="phylogeny">Phylogeny</button>')
if (!grepl('data-visual-tab="phylogeny"', html, fixed = TRUE)) {
  if (!grepl(old_tab, html, fixed = TRUE)) stop("World map visualisation tab was not found.")
  html <- sub(old_tab, new_tabs, html, fixed = TRUE)
}

boundary <- '      </div>\n    </section>\n\n    <section id="page-models"'
panel <- '      </div>\n\n      <div id="panel-visual-phylogeny" class="model-tab-panel" role="tabpanel" aria-labelledby="tab-visual-phylogeny" hidden>\n        <div class="page-heading model-subheading"><p class="eyebrow">Compare countries in linguistic context</p><h2>Phylogeny</h2><p>Display country-level WorldView values on the language phylogeny used by the original application.</p></div>\n        <div id="phylo-loading" class="notice" aria-live="polite">Loading phylogeny...</div>\n        <form id="phylo-form" class="analysis-layout" hidden>\n          <aside class="control-panel"><h2>Tree settings</h2>\n            <label class="field"><span>Variable</span><select id="phylo-variable" required></select></label>\n            <label class="field"><span>Countries included</span><select id="phylo-countries" multiple size="10"></select></label>\n            <p class="field-help">All matched WorldView countries are selected by default. Deselect countries to prune them from the displayed tree.</p>\n            <label class="field"><span>Tip labels</span><select id="phylo-label"><option value="country">Country</option><option value="language">Language</option><option value="both">Country and language</option></select></label>\n            <div class="button-row compact"><button class="button button-primary" type="submit">Create phylogeny</button><button id="phylo-reset" class="button button-secondary" type="button">Reset</button></div>\n          </aside>\n          <section class="results-panel" aria-labelledby="phylo-title"><div class="results-heading"><div><p class="eyebrow">Phylogeny</p><h2 id="phylo-title">Select data to begin</h2></div><button id="phylo-save" class="button button-secondary" type="button" hidden>Save tree image</button></div>\n            <div id="phylo-message" class="notice subtle">Choose a variable and create the phylogeny.</div>\n            <figure class="phylo-figure"><div id="phylo-chart" class="phylo-chart"></div><div id="phylo-tooltip" class="map-tooltip" hidden></div><figcaption id="phylo-caption" class="map-caption"></figcaption></figure>\n            <div id="phylo-legend" class="map-legend"></div>\n          </section>\n        </form>\n      </div>\n    </section>\n\n    <section id="page-models"'
if (!grepl('id="panel-visual-phylogeny"', html, fixed = TRUE)) {
  pos <- regexpr(boundary, html, fixed = TRUE)
  if (pos[1] < 0) stop("Could not locate the end of Visualisations.")
  html <- sub(boundary, panel, html, fixed = TRUE)
}
if (!grepl("assets/phylogeny.js", html, fixed = TRUE)) html <- sub("</head>", '  <script src="assets/phylogeny.js" defer></script>\n</head>', html, fixed = TRUE)
writeLines(html, index_path, useBytes = TRUE)

tabs <- paste(readLines(tabs_path, warn = FALSE), collapse = "\n")
tabs <- sub('map:document.getElementById("panel-visual-map")', 'map:document.getElementById("panel-visual-map"),phylogeny:document.getElementById("panel-visual-phylogeny")', tabs, fixed = TRUE)
writeLines(tabs, tabs_path, useBytes = TRUE)

js <- '"use strict";
const phyPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json",tree:"data/phylogeny/country_phylogeny_tree.tree",mapping:"data/phylogeny/country_phylogeny.csv"};
const phy={data:null,vars:[],tree:null,map:new Map(),countries:[],rows:[]};
const pe=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll("\\\"","&quot;");
const pf=(v,d=2)=>new Intl.NumberFormat("en-NZ",{maximumFractionDigits:d}).format(v);
function csv(text){const lines=text.trim().split(/\\r?\\n/),parse=line=>{const out=[];let s="",q=false;for(let i=0;i<line.length;i++){const c=line[i];if(c==="\\\""){if(q&&line[i+1]==="\\\""){s+="\\\"";i++;}else q=!q;}else if(c===","&&!q){out.push(s);s="";}else s+=c;}out.push(s);return out;},head=parse(lines.shift());return lines.map(line=>Object.fromEntries(parse(line).map((v,i)=>[head[i],v])));}
function newick(text){let i=0,id=0;function node(parent=null){const n={id:id++,parent,children:[],name:"",length:0};if(text[i]==="("){i++;while(true){n.children.push(node(n));if(text[i]===","){i++;continue;}if(text[i]===")"){i++;break;}}}let name="";while(i<text.length&&!",():;".includes(text[i]))name+=text[i++];n.name=name.trim();if(text[i]===":"){i++;let x="";while(i<text.length&&!",();".includes(text[i]))x+=text[i++];n.length=Number(x)||0;}return n;}return node();}
function tips(n,out=[]){if(!n.children.length)out.push(n);else n.children.forEach(c=>tips(c,out));return out;}
function selected(){return new Set([...document.getElementById("phylo-countries").selectedOptions].map(o=>o.value));}
function aggregate(id){const m=new Map();for(let i=0;i<phy.data.length;i++){const code=phy.data.columns.B_COUNTRY_ALPHA[i],raw=phy.data.columns[id][i];if(raw==null||raw===""||!Number.isFinite(Number(raw)))continue;if(!m.has(code))m.set(code,{n:0,sum:0});const r=m.get(code);r.n++;r.sum+=Number(raw);}const v=phy.vars.find(x=>x.id===id);return new Map([...m].map(([code,r])=>[code,{n:r.n,value:v.analysisType==="binary"?100*r.sum/r.n:r.sum/r.n}]));}
function layout(root,keep){function prune(n){n.children=n.children.map(prune).filter(Boolean);if(!n.children.length)return keep.has(n.name)?n:null;return n.children.length?n:null;}root=prune(root);if(!root)return null;let y=0,max=0;function place(n,x){n.x=x+n.length;max=Math.max(max,n.x);if(!n.children.length)n.y=y++;else{n.children.forEach(c=>place(c,n.x));n.y=n.children.reduce((s,c)=>s+c.y,0)/n.children.length;}}place(root,0);return{root,count:y,max};}
function clone(n,p=null){const x={...n,parent:p,children:[]};x.children=n.children.map(c=>clone(c,x));return x;}
function colour(v,min,max){if(v==null)return"#d1d5d8";const t=max===min?.5:(v-min)/(max-min),a=[255,224,102],b=[74,20,140];return`rgb(${Math.round(a[0]+t*(b[0]-a[0]))},${Math.round(a[1]+t*(b[1]-a[1]))},${Math.round(a[2]+t*(b[2]-a[2]))})`;}
function draw(e){e.preventDefault();const vid=document.getElementById("phylo-variable").value,v=phy.vars.find(x=>x.id===vid),allow=selected(),agg=aggregate(vid),allTips=tips(phy.tree),keep=new Set(allTips.filter(t=>{const code=t.name.split("_").pop();return allow.has(code)&&agg.has(code)&&phy.map.has(code);}).map(t=>t.name)),lay=layout(clone(phy.tree),keep);if(!lay||lay.count<2){document.getElementById("phylo-message").textContent="At least two matched countries with valid data are required.";return;}const width=1100,rowH=22,height=Math.max(420,lay.count*rowH+40),left=25,right=285,sx=x=>left+x/lay.max*(width-left-right),sy=y=>25+y*rowH,nodes=[];function walk(n){nodes.push(n);n.children.forEach(walk);}walk(lay.root);const vals=[...keep].map(name=>agg.get(name.split("_").pop()).value),min=Math.min(...vals),max=Math.max(...vals),labelMode=document.getElementById("phylo-label").value;let svg=`<svg viewBox="0 0 ${width} ${height}" xmlns="http://www.w3.org/2000/svg">`;nodes.filter(n=>n.children.length).forEach(n=>{const ys=n.children.map(c=>sy(c.y));svg+=`<line x1="${sx(n.x)}" y1="${Math.min(...ys)}" x2="${sx(n.x)}" y2="${Math.max(...ys)}" class="phy-branch"/>`;n.children.forEach(c=>svg+=`<line x1="${sx(n.x)}" y1="${sy(c.y)}" x2="${sx(c.x)}" y2="${sy(c.y)}" class="phy-branch"/>`);});nodes.filter(n=>!n.children.length).forEach(n=>{const parts=n.name.split("_"),code=parts.pop(),glot=parts.join("_"),meta=phy.map.get(code),a=agg.get(code),label=labelMode==="language"?meta.language_name:labelMode==="both"?`${meta.country_name} (${meta.language_name})`:meta.country_name,display=`${pf(a.value)}${v.analysisType==="binary"?"%":""}`;svg+=`<circle cx="${sx(n.x)}" cy="${sy(n.y)}" r="6" fill="${colour(a.value,min,max)}" class="phy-tip" tabindex="0" data-country="${pe(meta.country_name)}" data-language="${pe(meta.language_name)}" data-family="${pe(meta.family_language_name)}" data-value="${display}" data-n="${a.n}"/><text x="${sx(n.x)+10}" y="${sy(n.y)+4}" class="phy-label">${pe(label)}</text>`;});svg+="</svg>";document.getElementById("phylo-chart").innerHTML=svg;bind();document.getElementById("phylo-title").textContent=v.displayName;const measure=v.analysisType==="binary"?"percentage of valid responses coded 1":"mean response";document.getElementById("phylo-caption").textContent=`Tip colours show the ${measure} for ${v.id}: ${v.displayName}. Branch lengths follow the supplied country-language phylogeny.`;document.getElementById("phylo-legend").innerHTML=`<span>${pf(min)}${v.analysisType==="binary"?"%":""}</span><span class="map-gradient"></span><span>${pf(max)}${v.analysisType==="binary"?"%":""}</span>`;document.getElementById("phylo-message").textContent=`${lay.count} matched country tips displayed.`;document.getElementById("phylo-save").hidden=false;}
function bind(){document.querySelectorAll(".phy-tip").forEach(t=>{const show=e=>{const x=document.getElementById("phylo-tooltip");x.innerHTML=`<strong>${pe(t.dataset.country)}</strong><span>Language: ${pe(t.dataset.language)}</span><span>Family: ${pe(t.dataset.family)}</span><span>Value: ${pe(t.dataset.value)}</span><span>Valid responses: ${t.dataset.n}</span>`;x.hidden=false;x.style.left=`${e.offsetX+14}px`;x.style.top=`${e.offsetY+14}px`;};t.addEventListener("mouseenter",show);t.addEventListener("mousemove",show);t.addEventListener("mouseleave",()=>document.getElementById("phylo-tooltip").hidden=true);t.addEventListener("focus",show);t.addEventListener("blur",()=>document.getElementById("phylo-tooltip").hidden=true);});}
function save(){const svg=document.querySelector("#phylo-chart svg");if(!svg)return;const blob=new Blob([new XMLSerializer().serializeToString(svg)],{type:"image/svg+xml"}),a=document.createElement("a");a.href=URL.createObjectURL(blob);a.download=`worldview-phylogeny-${document.getElementById("phylo-variable").value}.svg`;a.click();URL.revokeObjectURL(a.href);}
function reset(){document.getElementById("phylo-variable").value="Q177";[...document.getElementById("phylo-countries").options].forEach(o=>o.selected=true);document.getElementById("phylo-chart").innerHTML="";document.getElementById("phylo-save").hidden=true;}
async function init(){const loading=document.getElementById("phylo-loading");try{const[d,c,t,m]=await Promise.all([fetch(phyPaths.data).then(r=>r.json()),fetch(phyPaths.codebook).then(r=>r.json()),fetch(phyPaths.tree).then(r=>r.text()),fetch(phyPaths.mapping).then(r=>r.text())]);const names=Object.keys(d),len=d[names[0]].length;phy.data={columns:d,length:len};phy.vars=c.variables.filter(v=>v.analysisType!=="nominal");phy.tree=newick(t.trim());csv(m).forEach(r=>phy.map.set(r.iso3166alpha3,r));const codes=new Set(tips(phy.tree).map(x=>x.name.split("_").pop()));const pairs=new Map();for(let i=0;i<len;i++){const code=d.B_COUNTRY_ALPHA[i];if(codes.has(code)&&phy.map.has(code))pairs.set(code,d.B_COUNTRY[i]);}phy.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const vs=document.getElementById("phylo-variable");phy.vars.forEach(v=>vs.add(new Option(`${v.id}: ${v.displayName}`,v.id)));vs.value="Q177";const cs=document.getElementById("phylo-countries");phy.countries.forEach(c=>{const o=new Option(`${c.name} (${c.code})`,c.code);o.selected=true;cs.add(o);});loading.hidden=true;document.getElementById("phylo-form").hidden=false;}catch(err){loading.textContent=`Phylogeny could not be loaded: ${err.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("phylo-form").addEventListener("submit",draw);document.getElementById("phylo-reset").addEventListener("click",reset);document.getElementById("phylo-save").addEventListener("click",save);init();});'
writeLines(js, js_path, useBytes = TRUE)

cat('\n/* Step 27: phylogeny */\n.phylo-figure { position: relative; margin: 0; }\n.phylo-chart { max-height: 760px; overflow: auto; border: 1px solid var(--line); border-radius: 14px; background: #fff; }\n.phylo-chart svg { display: block; min-width: 900px; width: 100%; }\n.phy-branch { stroke: #677572; stroke-width: 1.1; fill: none; }\n.phy-tip { stroke: #222; stroke-width: .7; cursor: pointer; }\n.phy-tip:hover, .phy-tip:focus { stroke: #000; stroke-width: 2; outline: none; }\n.phy-label { font: 11px Arial, sans-serif; fill: #24302e; }\n#phylo-countries { min-height: 220px; }\n', file = css_path, append = TRUE)

checks <- data.frame(check=c("tree_copied","mapping_copied","tab_added","panel_added","newick_parser_added","branch_lengths_used","tip_mapping_added","hover_added","save_svg_added","all_countries_selected"),passed=c(file.exists(file.path(data_dir,"country_phylogeny_tree.tree")),file.exists(file.path(data_dir,"country_phylogeny.csv")),grepl('data-visual-tab="phylogeny"',html,fixed=TRUE),grepl('id="panel-visual-phylogeny"',html,fixed=TRUE),grepl("function newick",js,fixed=TRUE),grepl("x+n.length",js,fixed=TRUE),grepl("phy.map.get(code)",js,fixed=TRUE),grepl("function bind",js,fixed=TRUE),grepl("image/svg+xml",js,fixed=TRUE),grepl("o.selected=true",js,fixed=TRUE)))
write.csv(checks,file.path(app_dir,"step27_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 27 validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 27 completed successfully.\nRestart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
