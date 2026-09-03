"use strict";
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
  if(explicit&&/^Q\d+$/.test(explicit))return explicit;
  const match=(element.textContent||"").match(/\bQ\d+\b/);
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
function cbInsertLoader(page){
  document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove());
  const searchInput=page.querySelector("#codebook-search, input[type=search], input[placeholder*=Search], input[aria-label*=Search]");
  let searchSection=null;
  if(searchInput){
    searchSection=searchInput.closest("form, .search-panel, .search-section, .filter-panel, .control-panel");
    if(!searchSection)searchSection=searchInput.parentElement;
  }
  const resultsAnchor=page.querySelector("#codebook-results, .codebook-results, .codebook-list, [data-codebook-list]");
  const loader=document.createElement("div");
  loader.id="codebook-topic-loading";
  loader.className="codebook-loading codebook-loading-icon-only";
  loader.setAttribute("role","status");
  loader.setAttribute("aria-label","Loading codebook variables");
  loader.innerHTML=`<span class="codebook-loading-spinner" aria-hidden="true"></span><span class="visually-hidden">Loading codebook variables</span>`;
  if(resultsAnchor)resultsAnchor.insertAdjacentElement("beforebegin",loader);
  else if(searchSection)searchSection.insertAdjacentElement("afterend",loader);
  else page.appendChild(loader);
}
function cbFinish(page){
  page.classList.remove("codebook-topic-pending");
  page.removeAttribute("data-codebook-pending");
  document.querySelectorAll("#codebook-topic-loading").forEach(element=>element.remove());
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
  requestAnimationFrame(()=>cbFinish(page));
  return true;
}
document.addEventListener("DOMContentLoaded",async()=>{
  const page=document.getElementById("page-codebook");
  if(!page)return;
  cbInsertLoader(page);
  const loading=document.createElement("div");
  loading.id="codebook-topic-loading";
  loading.className="notice subtle codebook-topic-loading";
  
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
});
