# WorldView Online - Step 29e
# Remove the codebook flash and delayed polling by grouping variables as soon as
# the existing codebook renderer inserts them into the page.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
script_path <- file.path(app_dir, "assets", "codebook-topics.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(index_path, script_path, css_path)))) {
  stop("Run Step 29d first. Required codebook files were not found.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Put the Codebook page into a pending state in the source HTML, before any
# deferred JavaScript runs. This prevents the ungrouped list from flashing.
html <- sub(
  '<section id="page-codebook" class="page"',
  '<section id="page-codebook" class="page codebook-topic-pending"',
  html,
  fixed = TRUE
)

# If class order differs, add the pending class directly to the matched section.
if (!grepl('id="page-codebook" class="page codebook-topic-pending"', html, fixed = TRUE)) {
  html <- sub(
    'id="page-codebook"',
    'id="page-codebook" data-codebook-pending="true"',
    html,
    fixed = TRUE
  )
}
writeLines(html, index_path, useBytes = TRUE)

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
function cbFinish(page){
  page.classList.remove("codebook-topic-pending");
  page.removeAttribute("data-codebook-pending");
  const loading=document.getElementById("codebook-topic-loading");
  if(loading)loading.remove();
}
function cbRenderTopics(page,codebook){
  if(page.dataset.topicNested==="true")return true;
  const cards=cbFindCards(page);
  if(cards.length<2)return false;
  const originalParent=cards[0].parentElement;
  if(!originalParent)return false;
  const previous=document.getElementById("codebook-topic-groups");
  if(previous)previous.remove();
  const topicById=new Map(codebook.variables.map(variable=>[variable.id,variable.topic||"Additional variables"]));
  const groups=new Map();
  cards.forEach(card=>{
    const id=cbId(card);
    if(!id)return;
    const topic=topicById.get(id)||"Additional variables";
    if(!groups.has(topic))groups.set(topic,[]);
    groups.get(topic).push({id,card});
  });
  if(!groups.size)return false;
  const host=document.createElement("div");
  host.id="codebook-topic-groups";
  host.className="codebook-topic-groups";
  originalParent.insertBefore(host,cards[0]);
  codebookTopicOrder.filter(topic=>groups.has(topic)).forEach(topic=>{
    const section=document.createElement("details");
    section.className="codebook-topic-section";
    section.dataset.topic=topic;
    section.open=false;
    const heading=document.createElement("summary");
    heading.className="codebook-topic-heading";
    heading.textContent=`${topic} (${groups.get(topic).length})`;
    heading.setAttribute("aria-label",`${topic}: ${groups.get(topic).length} variables`);
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
  cbFinish(page);
  return true;
}
document.addEventListener("DOMContentLoaded",async()=>{
  const page=document.getElementById("page-codebook");
  if(!page)return;
  const loading=document.createElement("div");
  loading.id="codebook-topic-loading";
  loading.className="notice subtle codebook-topic-loading";
  loading.textContent="Preparing codebook topics...";
  const heading=page.querySelector(".page-heading");
  if(heading)heading.insertAdjacentElement("afterend",loading);
  try{
    const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(response=>response.json());
    if(cbRenderTopics(page,codebook))return;
    const observer=new MutationObserver(()=>{
      if(cbRenderTopics(page,codebook))observer.disconnect();
    });
    observer.observe(page,{childList:true,subtree:true});
    window.setTimeout(()=>{
      observer.disconnect();
      if(!cbRenderTopics(page,codebook)){
        cbFinish(page);
        console.warn("Codebook topic grouping was not applied because variable cards were not found.");
      }
    },5000);
  }catch(error){
    cbFinish(page);
    console.error("Codebook topic grouping failed",error);
  }
});'
writeLines(js, script_path, useBytes = TRUE)

css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
if (!grepl("Step 29e: flash-free codebook topics", css, fixed = TRUE)) {
  cat(
    '\n/* Step 29e: flash-free codebook topics */\n#page-codebook.codebook-topic-pending .codebook-card,\n#page-codebook.codebook-topic-pending .variable-card,\n#page-codebook.codebook-topic-pending .dictionary-card,\n#page-codebook.codebook-topic-pending [data-variable-id],\n#page-codebook.codebook-topic-pending [data-variable],\n#page-codebook[data-codebook-pending="true"] .codebook-card,\n#page-codebook[data-codebook-pending="true"] .variable-card,\n#page-codebook[data-codebook-pending="true"] .dictionary-card,\n#page-codebook[data-codebook-pending="true"] [data-variable-id],\n#page-codebook[data-codebook-pending="true"] [data-variable] { visibility: hidden; }\n.codebook-topic-loading { margin: 18px 0; }\n#page-codebook[data-topic-nested="true"] .codebook-topic-groups { animation: codebookTopicsIn .12s ease-out; }\n@keyframes codebookTopicsIn { from { opacity: 0; } to { opacity: 1; } }\n',
    file = css_path,
    append = TRUE
  )
}

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "pending_state_present_in_source_html",
    "mutation_observer_added",
    "polling_interval_removed",
    "ungrouped_cards_hidden_while_pending",
    "loading_message_added",
    "topics_closed_by_default",
    "multiple_topics_remain_independent",
    "pending_state_removed_after_grouping"
  ),
  passed = c(
    grepl("codebook-topic-pending", updated_html, fixed = TRUE) || grepl("data-codebook-pending", updated_html, fixed = TRUE),
    grepl("new MutationObserver", updated_js, fixed = TRUE),
    !grepl("setInterval", updated_js, fixed = TRUE),
    grepl("codebook-topic-pending", updated_css, fixed = TRUE),
    grepl("Preparing codebook topics", updated_js, fixed = TRUE),
    grepl("section.open=false", updated_js, fixed = TRUE),
    !grepl('name="codebook-topic"', updated_js, fixed = TRUE),
    grepl('page.classList.remove("codebook-topic-pending")', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step29e_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 29e validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 29e completed successfully.\n\n")
cat("The ungrouped codebook list is hidden before first paint.\n")
cat("A MutationObserver now groups cards immediately when they are rendered.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
