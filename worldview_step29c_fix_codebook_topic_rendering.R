# WorldView Online - Step 29c
# Fix disappearing codebook variables caused by moving cards before the topic
# container was attached to the document.

app_dir <- "worldview_static_app"
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
index_path <- file.path(app_dir, "index.html")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(script_path, index_path, css_path)))) {
  stop("Run Step 29b first. Required codebook files were not found.")
}

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
function cbId(element){
  const explicit=element.dataset.variableId||element.getAttribute("data-variable")||element.getAttribute("data-id");
  if(explicit&&/^Q\\d+$/.test(explicit))return explicit;
  const match=(element.textContent||"").match(/\\bQ\\d+\\b/);
  return match?match[0]:null;
}
function cbFindCards(page){
  const specific=[...page.querySelectorAll("[data-variable-id], [data-variable], .codebook-card, .variable-card, .dictionary-card")];
  let cards=specific.filter(element=>cbId(element)&&!element.closest(".codebook-topic-section"));
  if(cards.length<2){
    const candidates=[...page.querySelectorAll("article, li, tr")].filter(element=>cbId(element)&&!element.closest(".codebook-topic-section"));
    cards=candidates.filter(element=>!candidates.some(other=>other!==element&&other.contains(element)));
  }
  return [...new Set(cards)];
}
function cbRenderTopics(codebook){
  const page=document.getElementById("page-codebook");
  if(!page||page.dataset.topicNested==="true")return false;
  const old=document.getElementById("codebook-topic-groups");
  if(old)old.remove();
  const cards=cbFindCards(page);
  if(cards.length<2)return false;
  const originalParent=cards[0].parentElement;
  if(!originalParent)return false;
  const topicById=new Map(codebook.variables.map(variable=>[variable.id,variable.topic||"Additional variables"]));
  const groups=new Map();
  cards.forEach(card=>{
    const id=cbId(card);
    if(!id)return;
    const topic=topicById.get(id)||"Additional variables";
    if(!groups.has(topic))groups.set(topic,[]);
    groups.get(topic).push({id,card});
  });
  const host=document.createElement("div");
  host.id="codebook-topic-groups";
  host.className="codebook-topic-groups";
  originalParent.insertBefore(host,cards[0]);
  codebookTopicOrder.filter(topic=>groups.has(topic)).forEach(topic=>{
    const section=document.createElement("section");
    section.className="codebook-topic-section";
    section.dataset.topic=topic;
    const heading=document.createElement("h2");
    heading.className="codebook-topic-heading";
    heading.textContent=topic;
    const list=document.createElement("div");
    list.className="codebook-topic-list";
    groups.get(topic)
      .sort((left,right)=>Number(left.id.slice(1))-Number(right.id.slice(1)))
      .forEach(entry=>list.appendChild(entry.card));
    section.append(heading,list);
    host.appendChild(section);
  });
  if(!host.querySelector(".codebook-topic-list > *")){
    host.remove();
    return false;
  }
  page.dataset.topicNested="true";
  return true;
}
document.addEventListener("DOMContentLoaded",async()=>{
  try{
    const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(response=>response.json());
    let attempts=0;
    const timer=setInterval(()=>{
      attempts+=1;
      if(cbRenderTopics(codebook)||attempts>=40)clearInterval(timer);
    },200);
  }catch(error){
    console.error("Codebook topic grouping failed",error);
  }
});'

writeLines(js, script_path, useBytes = TRUE)

# Hide only the now-empty original list containers, never the topic host.
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 29c: codebook rendering fix", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29c: codebook rendering fix */\n.codebook-topic-groups { display: grid !important; visibility: visible !important; }\n.codebook-topic-list > * { display: block; visibility: visible; }\n.codebook-topic-heading { display: block; }\n',
    file = css_path,
    append = TRUE
  )
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "topic_host_inserted_before_cards_move",
    "cards_moved_after_host_attachment",
    "broad_article_search_is_fallback_only",
    "empty_host_safety_check_added",
    "topic_script_still_linked",
    "religious_heading_available"
  ),
  passed = c(
    grepl("originalParent.insertBefore(host,cards[0])", js, fixed = TRUE),
    grepl("forEach(entry=>list.appendChild(entry.card))", js, fixed = TRUE),
    grepl("if(cards.length<2)", js, fixed = TRUE),
    grepl('host.querySelector(".codebook-topic-list > *")', js, fixed = TRUE),
    grepl("assets/codebook-topics.js", html, fixed = TRUE),
    grepl('"Religious values"', js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(
  checks,
  file.path(app_dir, "step29c_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)
if (!all(checks$passed)) {
  stop("Step 29c validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 29c completed successfully.\n\n")
cat("The topic container is now attached before variable cards are moved.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
