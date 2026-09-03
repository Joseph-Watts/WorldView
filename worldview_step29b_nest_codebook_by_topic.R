# WorldView Online - Step 29b
# Nest the visible codebook variable list under WVS topic headings.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")
css_path <- file.path(app_dir, "assets", "styles.css")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")

if (!all(file.exists(c(index_path, codebook_path, css_path)))) {
  stop("Required WorldView files were not found.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)

# Assign authoritative WVS Wave 7 topic headings by question number.
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
  if (number <= 234) return("Political interest and political participation")
  if (number <= 259) return("Political culture and political regimes")
  if (number <= 290) return("Demographic and socioeconomic variables")
  "Additional variables"
}

for (i in seq_along(codebook$variables)) {
  codebook$variables[[i]]$topic <- topic_for(codebook$variables[[i]]$id)
}

jsonlite::write_json(
  codebook,
  codebook_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  null = "null",
  digits = NA
)

# This script waits for the existing codebook renderer, identifies each rendered
# variable from its Q-number, and moves the cards into expandable topic sections.
js <- '"use strict";
const codebookTopicOrder=[
  "Social values, norms and stereotypes",
  "Happiness and wellbeing",
  "Social capital, trust and organisational membership",
  "Economic values",
  "Perceptions of corruption",
  "Perceptions of migration",
  "Perceptions of security",
  "Postmaterialism",
  "Science and technology",
  "Religious values",
  "Ethical values",
  "Political interest and political participation",
  "Political culture and political regimes",
  "Demographic and socioeconomic variables",
  "Technical and derived variables",
  "Additional variables"
];
function codebookVariableId(element){
  const explicit=element.dataset.variableId||element.getAttribute("data-variable")||element.getAttribute("data-id");
  if(explicit&&/^Q\\d+$/.test(explicit))return explicit;
  const match=(element.textContent||"").match(/\\bQ\\d+\\b/);
  return match?match[0]:null;
}
function codebookCards(page){
  const selectors=["[data-variable-id]","[data-variable]",".codebook-card",".variable-card",".dictionary-card","article"];
  const found=[];
  selectors.forEach(selector=>page.querySelectorAll(selector).forEach(element=>{
    if(codebookVariableId(element)&&!found.includes(element)&&!element.closest(".codebook-topic"))found.push(element);
  }));
  return found.filter(element=>!found.some(other=>other!==element&&other.contains(element)));
}
function renderCodebookTopics(codebook){
  const page=document.getElementById("page-codebook");
  if(!page||page.dataset.topicNested==="true")return false;
  const cards=codebookCards(page);
  if(cards.length<2)return false;
  const topicById=new Map(codebook.variables.map(variable=>[variable.id,variable.topic||"Additional variables"]));
  const groups=new Map();
  cards.forEach(card=>{
    const id=codebookVariableId(card),topic=topicById.get(id)||"Additional variables";
    if(!groups.has(topic))groups.set(topic,[]);
    groups.get(topic).push({id,card});
  });
  const host=document.createElement("div");
  host.id="codebook-topic-groups";
  host.className="codebook-topic-groups";
  codebookTopicOrder.filter(topic=>groups.has(topic)).forEach(topic=>{
    const entries=groups.get(topic).sort((a,b)=>Number(a.id.slice(1))-Number(b.id.slice(1)));
    const section=document.createElement("section");
    section.className="codebook-topic-section";
    section.dataset.topic=topic;
    const heading=document.createElement("h2");
    heading.className="codebook-topic-heading";
    heading.textContent=topic;
    const list=document.createElement("div");
    list.className="codebook-topic-list";
    entries.forEach(entry=>list.appendChild(entry.card));
    section.append(heading,list);
    host.appendChild(section);
  });
  const firstCard=cards[0];
  const originalParent=firstCard.parentElement;
  originalParent.insertBefore(host,firstCard);
  page.dataset.topicNested="true";
  return true;
}
document.addEventListener("DOMContentLoaded",async()=>{
  try{
    const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(response=>response.json());
    let attempts=0;
    const timer=setInterval(()=>{
      attempts+=1;
      if(renderCodebookTopics(codebook)||attempts>=30)clearInterval(timer);
    },200);
  }catch(error){console.error("Codebook topic grouping failed",error);}
});'
writeLines(js, script_path, useBytes = TRUE)

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
if (!grepl("assets/codebook-topics.js", html, fixed = TRUE)) {
  html <- sub(
    "</head>",
    '  <script src="assets/codebook-topics.js" defer></script>\n</head>',
    html,
    fixed = TRUE
  )
}
writeLines(html, index_path, useBytes = TRUE)

css <- '\n/* Step 29b: codebook topic sections */
.codebook-topic-groups { display: grid; gap: 30px; margin-top: 24px; }
.codebook-topic-section { scroll-margin-top: 100px; }
.codebook-topic-heading { margin: 0 0 14px; padding: 12px 16px; border-left: 6px solid #005b53; border-radius: 8px; background: #e7f1ef; color: #004840; font-size: 1.25rem; }
.codebook-topic-list { display: grid; gap: 14px; }
.codebook-topic-list .codebook-card,
.codebook-topic-list .variable-card,
.codebook-topic-list .dictionary-card,
.codebook-topic-list article { margin: 0; }
'
cat(css, file = css_path, append = TRUE)

ids <- vapply(codebook$variables, function(variable) variable$id, character(1))
topics <- vapply(codebook$variables, function(variable) variable$topic, character(1))
religious_ids <- ids[topics == "Religious values"]

checks <- data.frame(
  check = c(
    "topic_script_created",
    "topic_script_linked",
    "all_variables_have_topics",
    "q165_to_q168_are_religious_values",
    "topic_heading_renderer_added",
    "variables_sorted_within_topics"
  ),
  passed = c(
    file.exists(script_path),
    grepl("assets/codebook-topics.js", html, fixed = TRUE),
    all(nzchar(topics)),
    all(paste0("Q", 165:168) %in% religious_ids),
    grepl("codebook-topic-heading", js, fixed = TRUE),
    grepl("Number(a.id.slice(1))-Number(b.id.slice(1))", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step29b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 29b validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 29b completed successfully.\n\n")
cat("The codebook now renders variables under visible topic headings.\n")
cat("Q165-Q168 are grouped under Religious values.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
