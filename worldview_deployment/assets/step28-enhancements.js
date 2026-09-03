"use strict";
function s28Rank(values){const indexed=values.map((v,i)=>({v:Number(v),i})).sort((a,b)=>a.v-b.v),r=new Array(values.length);let i=0;while(i<indexed.length){let j=i;while(j+1<indexed.length&&indexed[j+1].v===indexed[i].v)j++;const rank=(i+j+2)/2;for(let k=i;k<=j;k++)r[indexed[k].i]=rank;i=j+1;}return r;}
function s28Pearson(x,y){const n=x.length,mx=x.reduce((a,b)=>a+b,0)/n,my=y.reduce((a,b)=>a+b,0)/n;let num=0,dx=0,dy=0;for(let i=0;i<n;i++){const a=x[i]-mx,b=y[i]-my;num+=a*b;dx+=a*a;dy+=b*b;}return num/Math.sqrt(dx*dy);}
function s28Kendall(x,y){let c=0,d=0,tx=0,ty=0;for(let i=0;i<x.length-1;i++)for(let j=i+1;j<x.length;j++){const a=Math.sign(x[j]-x[i]),b=Math.sign(y[j]-y[i]);if(a===0&&b===0)continue;if(a===0)tx++;else if(b===0)ty++;else if(a===b)c++;else d++;}return(c-d)/Math.sqrt((c+d+tx)*(c+d+ty));}
function s28GroupSelects(codebook){
  const topic=new Map(codebook.variables.map(v=>[v.id,v.topic||"Other"]));
  document.querySelectorAll("select").forEach(select=>{
    if(select.multiple&&select.id.includes("countr"))return;

    const variableOptions=[...select.options].filter(option=>topic.has(option.value));
    if(variableOptions.length<3)return;

    // If another analysis script has already grouped this selector, leave it alone.
    if(variableOptions.every(option=>option.parentElement?.tagName==="OPTGROUP"))return;

    const selected=new Set([...select.selectedOptions].map(option=>option.value));
    const groups=new Map();

    variableOptions.forEach(option=>{
      const label=topic.get(option.value);
      if(!groups.has(label))groups.set(label,[]);
      groups.get(label).push({value:option.value,text:option.text});
    });

    variableOptions.forEach(option=>option.remove());
    [...select.querySelectorAll("optgroup")].forEach(group=>{
      if(!group.children.length)group.remove();
    });

    [...groups]
      .sort((a,b)=>a[0].localeCompare(b[0]))
      .forEach(([label,items])=>{
        const group=document.createElement("optgroup");
        group.label=label;
        items.forEach(item=>{
          const option=new Option(item.text,item.value);
          option.selected=selected.has(item.value);
          group.appendChild(option);
        });
        select.appendChild(group);
      });
  });
}

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

const WORLDVIEW_PHYLOGENY_SOURCE_URL="https://doi.org/10.31235/osf.io/f8tr6";

function worldviewPhylogenyCitation(container){
  const p=document.createElement("p");
  p.className="phylogeny-source";
  p.append(document.createTextNode("Phylogeny source: Bouckaert, R., Redding, D., Sheehan, O., Kyritsis, T., Gray, R., Jones, K. E., & Atkinson, Q. (2022, 1 July). "));
  const title=worldviewExternalLink(
    "Global language diversification is linked to socio-ecology and threat status",
    WORLDVIEW_PHYLOGENY_SOURCE_URL
  );
  p.append(title);
  p.append(document.createTextNode(". doi:10.31235/osf.io/f8tr6"));
  container.appendChild(p);
}

function worldviewAddPhylogenyAttribution(){
  const phylogenyHeading=document.querySelector("#panel-visual-phylogeny .model-subheading");
  if(phylogenyHeading && !phylogenyHeading.querySelector(".phylogeny-source")){
    worldviewPhylogenyCitation(phylogenyHeading);
  }

  const sourceCards=[...document.querySelectorAll("#page-about .about-card")];
  const sourcesCard=sourceCards.find(card=>card.querySelector("h2")?.textContent.trim()==="Sources");
  if(sourcesCard && !sourcesCard.querySelector(".phylogeny-source")){
    worldviewPhylogenyCitation(sourcesCard);
  }
}


function worldviewAddSelectAllCountryHelp(){
  const instruction="To select all countries, click one country in the list and then press Ctrl+A (Windows/Linux) or Command+A (Mac).";

  document.querySelectorAll('select[multiple][id*="countr"]').forEach(select=>{
    let help=null;
    const describedBy=(select.getAttribute("aria-describedby")||"").split(/\s+/).filter(Boolean);
    for(const id of describedBy){
      const candidate=document.getElementById(id);
      if(candidate){help=candidate;break;}
    }

    if(!help){
      const field=select.closest(".field");
      const next=field?.nextElementSibling;
      if(next?.classList.contains("field-help"))help=next;
    }

    if(!help){
      help=document.createElement("p");
      help.className="field-help country-select-all-help";
      const field=select.closest(".field");
      if(field)field.insertAdjacentElement("afterend",help);
      else select.insertAdjacentElement("afterend",help);
    }

    if(!help.textContent.includes("Ctrl+A")){
      const spacer=help.textContent.trim() ? " " : "";
      help.append(document.createTextNode(spacer+instruction));
    }
  });
}

function worldviewDrawWrappedText(context,text,x,y,maxWidth,lineHeight,maxLines=4){
  const words=String(text||"").split(/\s+/);
  let line="",lines=[];
  words.forEach(word=>{
    const test=line ? `${line} ${word}` : word;
    if(context.measureText(test).width>maxWidth && line){
      lines.push(line);
      line=word;
    }else{
      line=test;
    }
  });
  if(line)lines.push(line);

  if(lines.length>maxLines){
    lines=lines.slice(0,maxLines);
    let last=lines[maxLines-1];
    while(last.length>1 && context.measureText(last+"…").width>maxWidth){
      last=last.slice(0,-1);
    }
    lines[maxLines-1]=last+"…";
  }

  lines.forEach((value,index)=>context.fillText(value,x,y+index*lineHeight));
  return y+lines.length*lineHeight;
}

function worldviewDownloadBarGraph(){
  if(typeof visualState==="undefined" || !visualState.lastRows?.length)return;

  const rows=visualState.lastRows;
  const groups=[...new Set(rows.map(row=>row.country_selection))];
  const responses=[...new Set(rows.map(row=>row.response))];
  const useCounts=document.querySelector('input[name="visual-measure"]:checked')?.value==="count";
  const getValue=row=>useCounts?Number(row.count):Number(row.percentage_of_valid);
  const maximum=Math.max(1,...rows.map(getValue).filter(Number.isFinite));

  const palette=[
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#6A3D9A",
    "#A65628",
    "#1B9E77",
    "#E7298A"
  ];

  const width=Math.max(1000,groups.length*210);
  const height=720;
  const scale=2;
  const margin={top:70,right:40,bottom:235,left:90};
  const plotWidth=width-margin.left-margin.right;
  const plotHeight=height-margin.top-margin.bottom;

  const canvas=document.createElement("canvas");
  canvas.width=width*scale;
  canvas.height=height*scale;
  const context=canvas.getContext("2d");
  context.scale(scale,scale);
  context.fillStyle="#ffffff";
  context.fillRect(0,0,width,height);
  context.textBaseline="middle";

  context.fillStyle="#173d38";
  context.font="bold 22px Arial, sans-serif";
  context.textAlign="center";
  context.fillText(document.getElementById("visual-title").textContent,width/2,32);

  context.font="13px Arial, sans-serif";
  for(let tick=0;tick<=5;tick+=1){
    const value=maximum*tick/5;
    const y=margin.top+plotHeight-(value/maximum)*plotHeight;
    context.strokeStyle="#d7dfdd";
    context.lineWidth=1;
    context.beginPath();
    context.moveTo(margin.left,y);
    context.lineTo(width-margin.right,y);
    context.stroke();

    context.fillStyle="#334a47";
    context.textAlign="right";
    context.fillText(
      useCounts?String(Math.round(value)):value.toFixed(0)+"%",
      margin.left-10,
      y
    );
  }

  const groupWidth=plotWidth/Math.max(1,groups.length);
  const gap=5;
  const barWidth=Math.max(7,Math.min(42,groupWidth/Math.max(1,responses.length)-gap));

  groups.forEach((group,groupIndex)=>{
    const groupRows=rows.filter(row=>row.country_selection===group);
    const usedWidth=responses.length*(barWidth+gap);
    const startX=margin.left+groupIndex*groupWidth+(groupWidth-usedWidth)/2;

    responses.forEach((response,responseIndex)=>{
      const row=groupRows.find(item=>item.response===response);
      if(!row)return;
      const value=getValue(row);
      if(!Number.isFinite(value))return;

      const barHeight=value/maximum*plotHeight;
      const x=startX+responseIndex*(barWidth+gap);
      const y=margin.top+plotHeight-barHeight;

      context.fillStyle=palette[responseIndex%palette.length];
      context.fillRect(x,y,barWidth,barHeight);
      context.strokeStyle="#303030";
      context.lineWidth=0.7;
      context.strokeRect(x,y,barWidth,barHeight);
    });

    context.save();
    context.translate(
      margin.left+groupIndex*groupWidth+groupWidth/2,
      margin.top+plotHeight+20
    );
    context.rotate(-Math.PI/5);
    context.fillStyle="#334a47";
    context.textAlign="right";
    context.font="13px Arial, sans-serif";
    context.fillText(group,0,0);
    context.restore();
  });

  context.fillStyle="#334a47";
  context.font="bold 13px Arial, sans-serif";
  context.textAlign="left";
  context.fillText("Response values",margin.left,height-142);

  const legendTop=height-118;
  const columns=Math.min(4,Math.max(1,responses.length));
  const itemWidth=plotWidth/columns;
  const itemHeight=42;

  context.font="12px Arial, sans-serif";
  responses.forEach((response,index)=>{
    const column=index%columns;
    const row=Math.floor(index/columns);
    const x=margin.left+column*itemWidth;
    const y=legendTop+row*itemHeight;

    context.fillStyle=palette[index%palette.length];
    context.fillRect(x,y-7,15,15);
    context.strokeStyle="#303030";
    context.lineWidth=0.7;
    context.strokeRect(x,y-7,15,15);

    context.fillStyle="#334a47";
    context.textAlign="left";
    worldviewDrawWrappedText(context,response,x+22,y,itemWidth-30,14,2);
  });

  canvas.toBlob(blob=>{
    if(!blob)return;
    const link=document.createElement("a");
    link.href=URL.createObjectURL(blob);
    link.download="worldview-bar-chart-"+document.getElementById("visual-variable").value+".png";
    document.body.appendChild(link);
    link.click();
    setTimeout(()=>{
      URL.revokeObjectURL(link.href);
      link.remove();
    },0);
  },"image/png");
}

function worldviewGradient(context,x,y,width,height){
  const gradient=context.createLinearGradient(x,y,x+width,y);
  gradient.addColorStop(0,"rgb(255,224,102)");
  gradient.addColorStop(1,"rgb(74,20,140)");
  context.fillStyle=gradient;
  context.fillRect(x,y,width,height);
  context.strokeStyle="rgba(32,40,48,.45)";
  context.lineWidth=1;
  context.strokeRect(x,y,width,height);
}

function worldviewSaveMapImage(){
  const svg=document.querySelector("#world-map svg");
  if(!svg)return;

  const variableId=document.getElementById("map-variable").value;
  const variable=typeof wmState!=="undefined"
    ? wmState.variables.find(v=>v.id===variableId)
    : null;
  const rows=(typeof wmState!=="undefined" && Array.isArray(wmState.lastRows))
    ? wmState.lastRows
    : [];
  const values=rows.map(row=>Number(row.value)).filter(Number.isFinite);
  const min=values.length?Math.min(...values):null;
  const max=values.length?Math.max(...values):null;
  const isPercent=variable?.analysisType==="binary";
  const measureLabel=isPercent
    ?"Percentage of valid responses coded 1"
    :"Mean response";

  const serializer=new XMLSerializer();
  const svgText=serializer.serializeToString(svg);
  const caption=document.getElementById("map-caption").textContent;

  const scale=2;
  const width=1000;
  const mapHeight=520;
  const height=700;
  const canvas=document.createElement("canvas");
  canvas.width=width*scale;
  canvas.height=height*scale;
  const context=canvas.getContext("2d");
  context.scale(scale,scale);

  context.fillStyle="#ffffff";
  context.fillRect(0,0,width,height);

  const image=new Image();
  const blob=new Blob([svgText],{type:"image/svg+xml;charset=utf-8"});
  const url=URL.createObjectURL(blob);

  image.onload=()=>{
    context.drawImage(image,0,0,width,mapHeight);

    const legendX=32,legendY=555,legendW=320,legendH=16;
    context.fillStyle="#35413f";
    context.font="bold 14px Arial, sans-serif";
    context.textAlign="left";
    context.fillText(`Colour scale — ${measureLabel}`,legendX,legendY-14);

    worldviewGradient(context,legendX,legendY,legendW,legendH);

    context.font="13px Arial, sans-serif";
    context.textBaseline="top";
    context.fillStyle="#35413f";
    context.textAlign="left";
    context.fillText(
      min===null?"No data":`${wmFmt(min)}${isPercent?"%":""}`,
      legendX,
      legendY+22
    );
    context.textAlign="right";
    context.fillText(
      max===null?"No data":`${wmFmt(max)}${isPercent?"%":""}`,
      legendX+legendW,
      legendY+22
    );

    context.fillStyle="#d1d5d8";
    context.fillRect(410,legendY,16,16);
    context.strokeStyle="#6b7478";
    context.strokeRect(410,legendY,16,16);
    context.fillStyle="#35413f";
    context.textAlign="left";
    context.fillText("No WorldView data",434,legendY+1);

    context.textBaseline="alphabetic";
    context.font="15px Arial, sans-serif";
    context.fillStyle="#35413f";
    worldviewDrawWrappedText(context,caption,32,635,935,20,3);

    const link=document.createElement("a");
    link.download=`worldview-map-${variableId}.png`;
    link.href=canvas.toDataURL("image/png");
    link.click();
    URL.revokeObjectURL(url);
  };

  image.onerror=()=>{
    URL.revokeObjectURL(url);
    document.getElementById("map-message").textContent=
      "The map image could not be created in this browser.";
  };
  image.src=url;
}

function worldviewSavePhylogenyImage(){
  const source=document.querySelector("#phylo-chart svg");
  if(!source)return;

  const variableId=document.getElementById("phylo-variable").value;
  const variable=(typeof phy!=="undefined")
    ? phy.vars.find(v=>v.id===variableId)
    : null;
  const isPercent=variable?.analysisType==="binary";
  const measureLabel=isPercent
    ?"Percentage of valid responses coded 1"
    :"Mean response";

  const legend=document.getElementById("phylo-legend");
  const legendSpans=legend?[...legend.querySelectorAll("span")]:[];
  const minLabel=legendSpans[0]?.textContent?.trim()||"Minimum";
  const maxLabel=legendSpans[legendSpans.length-1]?.textContent?.trim()||"Maximum";

  const clone=source.cloneNode(true);
  const box=source.viewBox.baseVal;
  const width=Math.max(1100,Math.ceil(box.width));
  const treeHeight=Math.ceil(box.height);
  const extra=165;
  const scale=2;

  clone.setAttribute("width",width);
  clone.setAttribute("height",treeHeight);
  clone.setAttribute("xmlns","http://www.w3.org/2000/svg");

  const svgText=new XMLSerializer().serializeToString(clone);
  const blob=new Blob([svgText],{type:"image/svg+xml;charset=utf-8"});
  const url=URL.createObjectURL(blob);
  const image=new Image();

  const canvas=document.createElement("canvas");
  canvas.width=width*scale;
  canvas.height=(treeHeight+extra)*scale;
  const context=canvas.getContext("2d");

  image.onload=()=>{
    context.scale(scale,scale);
    context.fillStyle="#ffffff";
    context.fillRect(0,0,width,treeHeight+extra);
    context.drawImage(image,0,0,width,treeHeight);

    const legendX=30;
    const legendY=treeHeight+30;
    const legendW=320;
    const legendH=16;

    context.fillStyle="#35413f";
    context.font="bold 14px Arial, sans-serif";
    context.textAlign="left";
    context.fillText(`Tip colour scale — ${measureLabel}`,legendX,legendY-13);
    worldviewGradient(context,legendX,legendY,legendW,legendH);

    context.font="13px Arial, sans-serif";
    context.textBaseline="top";
    context.fillStyle="#35413f";
    context.textAlign="left";
    context.fillText(minLabel,legendX,legendY+22);
    context.textAlign="right";
    context.fillText(maxLabel,legendX+legendW,legendY+22);

    context.textBaseline="alphabetic";
    context.textAlign="left";
    context.font="15px Arial, sans-serif";
    context.fillStyle="#35413f";
    const caption=document.getElementById("phylo-caption").textContent;
    worldviewDrawWrappedText(context,caption,30,treeHeight+100,width-60,20,3);

    const link=document.createElement("a");
    link.download=`worldview-phylogeny-${variableId}.png`;
    link.href=canvas.toDataURL("image/png");
    link.click();
    URL.revokeObjectURL(url);
  };

  image.onerror=()=>{
    URL.revokeObjectURL(url);
    document.getElementById("phylo-message").textContent=
      "The tree image could not be created in this browser.";
  };
  image.src=url;
}

/*
 * Replace the existing export handlers before DOMContentLoaded registers
 * their click listeners.
 */
if(typeof downloadGraphData==="function")downloadGraphData=worldviewDownloadBarGraph;
if(typeof saveMapImage==="function")saveMapImage=worldviewSaveMapImage;
if(typeof save==="function")save=worldviewSavePhylogenyImage;


function worldviewUpdatePhylogenyPage(){
  const panel=document.getElementById("panel-visual-phylogeny");
  if(!panel)return;

  const heading=panel.querySelector(".model-subheading");
  if(heading){
    const description=[...heading.querySelectorAll("p")].find(p=>!p.classList.contains("eyebrow")&&!p.classList.contains("phylogeny-source"));
    if(description){
      description.textContent="Display country-level WorldView values on a language phylogeny.";
    }
  }

  const form=document.getElementById("phylo-form");
  const controlPanel=form?.querySelector(".control-panel");
  if(!controlPanel || document.getElementById("phylo-display-controls"))return;

  const controls=document.createElement("div");
  controls.id="phylo-display-controls";
  controls.className="phylo-display-controls";
  controls.innerHTML=`
    <h3>Tree display</h3>
    <label class="field">
      <span>Tree layout</span>
      <select id="phylo-layout">
        <option value="rectangular" selected>Rectangular</option>
        <option value="slanted">Slanted</option>
      </select>
    </label>
    <label class="field">
      <span>Branch scaling</span>
      <select id="phylo-branch-scale">
        <option value="original" selected>Original phylogenetic branch lengths</option>
        <option value="equal">Equal branch lengths (cladogram)</option>
        <option value="sqrt">Square-root transform — compress long branches</option>
        <option value="square">Squared transform — emphasise long branches</option>
      </select>
    </label>
    <label class="field">
      <span>Tip spacing</span>
      <select id="phylo-tip-spacing">
        <option value="17">Compact</option>
        <option value="22" selected>Standard</option>
        <option value="28">Spacious</option>
        <option value="34">Extra spacious</option>
      </select>
    </label>
    <label class="field">
      <span>Tip label size</span>
      <select id="phylo-label-size">
        <option value="10">Small</option>
        <option value="11" selected>Standard</option>
        <option value="13">Large</option>
        <option value="15">Extra large</option>
      </select>
    </label>
    <p class="field-help phylo-scaling-help">Original branch lengths preserve the distances supplied by the source phylogeny. Equal branch lengths emphasise topology only. The square-root and squared transforms change relative branch spacing for display without changing the tree topology.</p>`;

  const buttonRow=controlPanel.querySelector(".button-row");
  if(buttonRow)controlPanel.insertBefore(controls,buttonRow);
  else controlPanel.appendChild(controls);
}

document.addEventListener("DOMContentLoaded",async()=>{
  worldviewLoadBrandingStyles();
  worldviewReplaceBrandMark();
  worldviewUpdateHeaderSubtitle();
  worldviewUpdateHeroHeading();
  worldviewRemoveModelsHeadingText();
  worldviewUpdateModelsCopy();
  worldviewAddCodebookInstructions();
  worldviewAddSelectAllCountryHelp();
  worldviewUpdatePhylogenyPage();
  worldviewUpdateDescribeCompareCard();
  worldviewUpdateDevelopers();
  worldviewAddPhylogenyAttribution();
  worldviewSpaceCorrelationMethodHelp();
  worldviewLinkWVSReferences();
  worldviewAddLicenceCard();
  worldviewUpdateFooter();
  worldviewAddGithubFooterLink();

  try{
    const codebook=await fetch("data/worldview-codebook-v1.0.0.json").then(r=>r.json());

    // Group variable selectors immediately, and keep watching briefly for
    // analysis controls (such as the World Map) that populate asynchronously.
    s28GroupSelects(codebook);
    const visualisations=document.getElementById("page-visualisations");
    if(visualisations){
      const observer=new MutationObserver(()=>s28GroupSelects(codebook));
      observer.observe(visualisations,{childList:true,subtree:true});
      setTimeout(()=>observer.disconnect(),5000);
    }
    [400,1000,2000,4000].forEach(delay=>
      setTimeout(()=>s28GroupSelects(codebook),delay)
    );
  }catch(e){}

  const method=document.getElementById("correlation-method"),
        form=document.getElementById("model-form")||document.getElementById("correlation-form");
  if(method&&form)form.addEventListener("submit",()=>{
    const label=document.getElementById("model-message")||document.getElementById("correlation-message");
    if(label&&method.value!=="pearson")label.dataset.correlationMethod=method.value;
  },true);
});
