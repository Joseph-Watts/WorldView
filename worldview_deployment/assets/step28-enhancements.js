"use strict";
function s28Rank(values){const indexed=values.map((v,i)=>({v:Number(v),i})).sort((a,b)=>a.v-b.v),r=new Array(values.length);let i=0;while(i<indexed.length){let j=i;while(j+1<indexed.length&&indexed[j+1].v===indexed[i].v)j++;const rank=(i+j+2)/2;for(let k=i;k<=j;k++)r[indexed[k].i]=rank;i=j+1;}return r;}
function s28Pearson(x,y){const n=x.length,mx=x.reduce((a,b)=>a+b,0)/n,my=y.reduce((a,b)=>a+b,0)/n;let num=0,dx=0,dy=0;for(let i=0;i<n;i++){const a=x[i]-mx,b=y[i]-my;num+=a*b;dx+=a*a;dy+=b*b;}return num/Math.sqrt(dx*dy);}
function s28Kendall(x,y){let c=0,d=0,tx=0,ty=0;for(let i=0;i<x.length-1;i++)for(let j=i+1;j<x.length;j++){const a=Math.sign(x[j]-x[i]),b=Math.sign(y[j]-y[i]);if(a===0&&b===0)continue;if(a===0)tx++;else if(b===0)ty++;else if(a===b)c++;else d++;}return(c-d)/Math.sqrt((c+d+tx)*(c+d+ty));}
function s28GroupSelects(codebook){const topic=new Map(codebook.variables.map(v=>[v.id,v.topic||"Other"]));document.querySelectorAll("select").forEach(select=>{const options=[...select.options],variableOptions=options.filter(o=>topic.has(o.value));if(variableOptions.length<3||select.multiple&&select.id.includes("countr"))return;const selected=new Set([...select.selectedOptions].map(o=>o.value)),groups=new Map();variableOptions.forEach(o=>{const t=topic.get(o.value);if(!groups.has(t))groups.set(t,[]);groups.get(t).push({value:o.value,text:o.text});});variableOptions.forEach(o=>o.remove());[...groups].sort((a,b)=>a[0].localeCompare(b[0])).forEach(([label,items])=>{const g=document.createElement("optgroup");g.label=label;items.forEach(item=>{const o=new Option(item.text,item.value);o.selected=selected.has(item.value);g.appendChild(o);});select.appendChild(g);});});}

const WORLDVIEW_WVS_URL="https://www.worldvaluessurvey.org/";
const WORLDVIEW_CC_URL="https://creativecommons.org/licenses/by/4.0/";
const WORLDVIEW_CODE_URL="https://opensource.org/license/mit";

function worldviewLoadBrandingStyles(){
  if(document.querySelector('link[data-worldview-branding]'))return;
  const link=document.createElement("link");
  link.rel="stylesheet";
  link.href="assets/branding.css";
  link.dataset.worldviewBranding="true";
  document.head.appendChild(link);
}

function worldviewExternalLink(label,url){
  const a=document.createElement("a");
  a.href=url;
  a.target="_blank";
  a.rel="noopener noreferrer";
  a.textContent=label;
  return a;
}

function worldviewReplaceBrandMark(){
  const mark=document.querySelector(".brand-mark");
  if(!mark)return;
  mark.textContent="";
  const img=document.createElement("img");
  img.src="assets/worldview-logo.png";
  img.alt="";
  img.width=48;
  img.height=48;
  mark.appendChild(img);
}

function worldviewLinkWVSReferences(){
  const selectors=[
    ".information-panel p",
    "#page-about .about-card p",
    ".site-footer p"
  ];

  selectors.forEach(selector=>{
    document.querySelectorAll(selector).forEach(node=>{
      if(node.querySelector('a[href*="worldvaluessurvey.org"]'))return;
      const text=node.textContent;
      const phrase="World Values Survey";
      const idx=text.indexOf(phrase);
      if(idx<0)return;

      node.textContent="";
      node.append(document.createTextNode(text.slice(0,idx)));
      node.append(worldviewExternalLink(phrase,WORLDVIEW_WVS_URL));
      node.append(document.createTextNode(text.slice(idx+phrase.length)));
    });
  });
}

function worldviewAddLicenceCard(){
  const grid=document.querySelector("#page-about .about-grid");
  if(!grid || grid.querySelector(".licence-card"))return;

  const card=document.createElement("article");
  card.className="about-card licence-card";

  const heading=document.createElement("h2");
  heading.textContent="Licence and reuse";
  card.appendChild(heading);

  const p1=document.createElement("p");
  p1.append(document.createTextNode("WorldView software code is available under the "));
  p1.append(worldviewExternalLink("MIT Licence",WORLDVIEW_CODE_URL));
  p1.append(document.createTextNode(". Original WorldView website content, documentation, and visual assets are licensed under "));
  p1.append(worldviewExternalLink("Creative Commons Attribution 4.0 International (CC BY 4.0)",WORLDVIEW_CC_URL));
  p1.append(document.createTextNode("."));
  card.appendChild(p1);

  const p2=document.createElement("p");
  p2.append(document.createTextNode("World Values Survey data and other third-party materials are not covered by these licences and remain subject to their respective terms. See the "));
  p2.append(worldviewExternalLink("World Values Survey website",WORLDVIEW_WVS_URL));
  p2.append(document.createTextNode(" for WVS information and conditions of use."));
  card.appendChild(p2);

  grid.appendChild(card);
}

function worldviewUpdateFooter(){
  const footer=document.querySelector(".site-footer");
  if(!footer || footer.querySelector(".footer-licence"))return;

  const existing=[...footer.children];
  const wrapper=document.createElement("div");
  wrapper.className="footer-licence";

  if(existing[0]){
    const p=existing[0];
    const text=p.textContent;
    p.textContent="";
    const phrase="World Values Survey";
    const idx=text.indexOf(phrase);
    if(idx>=0){
      p.append(document.createTextNode(text.slice(0,idx)));
      p.append(worldviewExternalLink(phrase,WORLDVIEW_WVS_URL));
      p.append(document.createTextNode(text.slice(idx+phrase.length)));
    }else{
      p.textContent=text;
    }
    wrapper.appendChild(p);
  }

  const licence=document.createElement("p");
  licence.className="licence-line";
  licence.append(document.createTextNode("WorldView © 2026 Joseph Watts and contributors · Code: "));
  licence.append(worldviewExternalLink("MIT",WORLDVIEW_CODE_URL));
  licence.append(document.createTextNode(" · Original content and visual assets: "));
  licence.append(worldviewExternalLink("CC BY 4.0",WORLDVIEW_CC_URL));
  licence.append(document.createTextNode(" · WVS data subject to WVS terms."));
  wrapper.appendChild(licence);

  footer.insertBefore(wrapper,footer.firstChild);
}


const WORLDVIEW_GITHUB_URL="https://github.com/Joseph-Watts/WorldView";

function worldviewUpdateHeroHeading(){
  const heading=document.querySelector("#page-home .hero-content h1");
  if(heading)heading.textContent="Analyse values across countries in your browser";
}

function worldviewUpdateDevelopers(){
  const cards=[...document.querySelectorAll("#page-about .about-card")];
  const card=cards.find(c=>c.querySelector("h2")?.textContent.trim()==="Development team");
  if(!card)return;

  card.innerHTML="";

  const heading=document.createElement("h2");
  heading.textContent="Development team";
  card.appendChild(heading);

  const intro=document.createElement("p");
  intro.textContent="WorldView was developed by:";
  card.appendChild(intro);

  const developers=[
    {name:"Joseph Watts",url:"https://josephwatts.org"},
    {name:"Nicki Cartlidge",url:"https://www.linkedin.com/in/nicki-cartlidge-571b3b51"},
    {name:"André De Vito",url:"https://www.linkedin.com/in/andre-de-vito"},
    {name:"Prisca Mbikou",url:null},
    {name:"Kong Weiming (William)",url:null}
  ];

  const list=document.createElement("ul");
  developers.forEach(dev=>{
    const li=document.createElement("li");
    if(dev.url){
      li.appendChild(worldviewExternalLink(dev.name,dev.url));
    }else{
      li.textContent=dev.name;
    }
    list.appendChild(li);
  });
  card.appendChild(list);

  const note=document.createElement("p");
  note.textContent="The current static-browser version was adapted from the original WorldView Shiny application for teaching use at the University of Canterbury.";
  card.appendChild(note);
}

function worldviewAddGithubFooterLink(){
  const footer=document.querySelector(".site-footer");
  if(!footer || footer.querySelector('a[href="'+WORLDVIEW_GITHUB_URL+'"]'))return;

  const wrapper=footer.querySelector(".footer-licence") || footer.firstElementChild;
  if(!wrapper)return;

  const p=document.createElement("p");
  p.className="licence-line";
  p.append(document.createTextNode("Source code: "));
  p.append(worldviewExternalLink("GitHub repository",WORLDVIEW_GITHUB_URL));
  wrapper.appendChild(p);
}


function worldviewUpdateDescribeCompareCard(){
  const cards=[...document.querySelectorAll("#page-home .feature-card")];
  const card=cards.find(c=>c.querySelector("h2")?.textContent.trim()==="Describe and compare");
  if(!card)return;

  const paragraph=card.querySelector("p");
  if(paragraph){
    paragraph.textContent="Calculate summaries, compare response distributions, and create visualisations to explore patterns within and across countries.";
  }

  const existingLinks=[...card.querySelectorAll("a")];
  existingLinks.forEach(a=>a.remove());

  const links=document.createElement("div");
  links.className="feature-links";

  const summary=document.createElement("a");
  summary.href="#summary";
  summary.textContent="Open summary statistics";

  const visualisations=document.createElement("a");
  visualisations.href="#visualisations";
  visualisations.textContent="Open visualisations";

  links.appendChild(summary);
  links.appendChild(document.createTextNode(" · "));
  links.appendChild(visualisations);
  card.appendChild(links);
}

function worldviewSpaceCorrelationMethodHelp(){
  const targetText="Pearson measures linear association. Spearman and Kendall measure rank association.";
  const candidates=[...document.querySelectorAll("#page-models p, #page-models .field-help, #page-models .small-text, #page-models div")];
  const target=candidates.find(el=>el.textContent.trim()===targetText);
  if(target){
    target.style.marginTop="0.9rem";
    target.style.display="block";
  }
}


function worldviewUpdateHeaderSubtitle(){
  const subtitle=document.querySelector(".brand small");
  if(subtitle)subtitle.textContent="World Values Survey Data Tool";
}


function worldviewRemoveModelsHeadingText(){
  const heading=document.querySelector("#page-models .page-heading");
  if(!heading)return;
  const target=[...heading.querySelectorAll("p")].find(
    p=>p.textContent.trim()==="Calculate Pearson correlations between eligible processed variables."
  );
  if(target)target.remove();
}


function worldviewUpdateModelsCopy(){
  // Home-page feature card 03
  const cards=[...document.querySelectorAll("#page-home .feature-card")];
  const card=cards.find(c=>{
    const number=c.querySelector(".feature-number");
    return number && number.textContent.trim()==="03";
  });

  if(card){
    const heading=card.querySelector("h2");
    const paragraph=card.querySelector("p");
    const link=card.querySelector("a");

    if(heading)heading.textContent="Model relationships and differences";
    if(paragraph){
      paragraph.textContent="Explore associations and group differences using correlations, linear and logistic regression, ANOVA, and chi-square tests.";
    }
    if(link)link.textContent="Open models";
  }

  // Models-page introductory copy
  const pageHeading=document.querySelector("#page-models .page-heading");
  if(pageHeading){
    let intro=[...pageHeading.querySelectorAll("p")].find(
      p=>!p.classList.contains("eyebrow")
    );

    if(!intro){
      intro=document.createElement("p");
      pageHeading.appendChild(intro);
    }

    intro.textContent="Explore associations, predictions, and group differences using correlations, linear and logistic regression, ANOVA, and chi-square tests.";
  }
}


function worldviewAddCodebookInstructions(){
  const page=document.getElementById("page-codebook");
  if(!page || page.querySelector(".codebook-expand-help"))return;

  const heading=page.querySelector(".page-heading");
  if(!heading)return;

  const help=document.createElement("div");
  help.className="notice subtle codebook-expand-help";
  help.innerHTML=
    "<strong>How to browse the codebook:</strong> " +
    "Select a topic bar to expand or collapse the variables in that category. " +
    "Then select an individual variable row to open its question, coding, response values, and interpretation details. " +
    "The downward arrow indicates an expandable section.";
  heading.insertAdjacentElement("afterend",help);
}
document.addEventListener("DOMContentLoaded",async()=>{
  worldviewLoadBrandingStyles();
  worldviewReplaceBrandMark();
  worldviewUpdateHeaderSubtitle();
  worldviewUpdateHeroHeading();
  worldviewRemoveModelsHeadingText();
  worldviewUpdateModelsCopy();
  worldviewAddCodebookInstructions();
  worldviewUpdateDescribeCompareCard();
  worldviewUpdateDevelopers();
  worldviewSpaceCorrelationMethodHelp();
  worldviewLinkWVSReferences();
  worldviewAddLicenceCard();
  worldviewUpdateFooter();
  worldviewAddGithubFooterLink();

  try{
    const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(r=>r.json());
    setTimeout(()=>s28GroupSelects(codebook),700);
  }catch(e){}

  const method=document.getElementById("correlation-method"),
        form=document.getElementById("model-form")||document.getElementById("correlation-form");
  if(method&&form)form.addEventListener("submit",()=>{
    const label=document.getElementById("model-message")||document.getElementById("correlation-message");
    if(label&&method.value!=="pearson")label.dataset.correlationMethod=method.value;
  },true);
});
