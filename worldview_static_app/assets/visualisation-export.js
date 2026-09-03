"use strict";
(function(){
  const isVisible=element=>{
    if(!element)return false;
    const style=getComputedStyle(element),rect=element.getBoundingClientRect();
    return style.display!=="none"&&style.visibility!=="hidden"&&rect.width>1&&rect.height>1;
  };
  const saveCanvas=(canvas,name)=>{
    const link=document.createElement("a");
    link.download=name;
    link.href=canvas.toDataURL("image/png");
    link.click();
  };
  const panelFor=button=>button.closest("[role=tabpanel], .visualisation-panel, .tab-panel, section")||document.getElementById("page-visualisations");
  const filename=panel=>{
    const title=panel.querySelector("h2,h3")?.textContent?.trim()||"visualisation";
    return `worldview-${title.toLowerCase().replace(/[^a-z0-9]+/g,"-").replace(/^-|-$/g,"")}.png`;
  };
  function svgToPng(svg,name){
    const clone=svg.cloneNode(true),rect=svg.getBoundingClientRect(),view=svg.viewBox?.baseVal;
    const width=Math.ceil(view?.width||rect.width),height=Math.ceil(view?.height||rect.height),scale=2;
    clone.setAttribute("xmlns","http://www.w3.org/2000/svg");clone.setAttribute("width",width);clone.setAttribute("height",height);
    const blob=new Blob([new XMLSerializer().serializeToString(clone)],{type:"image/svg+xml;charset=utf-8"});
    const url=URL.createObjectURL(blob),image=new Image();
    image.onload=()=>{const canvas=document.createElement("canvas"),context=canvas.getContext("2d");canvas.width=width*scale;canvas.height=height*scale;context.scale(scale,scale);context.fillStyle="#fff";context.fillRect(0,0,width,height);context.drawImage(image,0,0,width,height);URL.revokeObjectURL(url);saveCanvas(canvas,name);};
    image.onerror=()=>URL.revokeObjectURL(url);image.src=url;
  }
  function htmlToPng(element,name){
    const rect=element.getBoundingClientRect(),width=Math.ceil(rect.width),height=Math.ceil(rect.height),scale=2;
    const clone=element.cloneNode(true);
    clone.querySelectorAll("button").forEach(button=>button.remove());
    const styles=[...document.styleSheets].map(sheet=>{try{return [...sheet.cssRules].map(rule=>rule.cssText).join("
");}catch(error){return "";}}).join("
");
    const markup=new XMLSerializer().serializeToString(clone);
    const svg=`<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="100%" height="100%" fill="#fff"/><foreignObject x="0" y="0" width="100%" height="100%"><div xmlns="http://www.w3.org/1999/xhtml"><style>${styles} body{margin:0;background:#fff}</style>${markup}</div></foreignObject></svg>`;
    const blob=new Blob([svg],{type:"image/svg+xml;charset=utf-8"}),url=URL.createObjectURL(blob),image=new Image();
    image.onload=()=>{const canvas=document.createElement("canvas"),context=canvas.getContext("2d");canvas.width=width*scale;canvas.height=height*scale;context.scale(scale,scale);context.fillStyle="#fff";context.fillRect(0,0,width,height);context.drawImage(image,0,0,width,height);URL.revokeObjectURL(url);saveCanvas(canvas,name);};
    image.onerror=()=>URL.revokeObjectURL(url);image.src=url;
  }
  function chartFor(button,panel){
    const selectors=["#bar-chart","#bar-chart-output","#visual-chart","#visual-results",".bar-chart",".chart-container",".visualisation-output",".results-output","figure"];
    for(const selector of selectors){const candidate=panel.querySelector(selector)||document.querySelector(`#page-visualisations ${selector}`);if(isVisible(candidate)&&candidate!==button&&candidate.querySelectorAll("button").length<5)return candidate;}
    const candidates=[...panel.querySelectorAll("svg,canvas,figure,[class*=chart],[class*=graph],[class*=result]")].filter(isVisible);
    return candidates.find(candidate=>!candidate.contains(button))||null;
  }
  function saveImage(button){
    const panel=panelFor(button),name=filename(panel),chart=chartFor(button,panel);
    if(!chart)return false;
    if(chart.tagName.toLowerCase()==="canvas"){saveCanvas(chart,name);return true;}
    const canvas=[...chart.querySelectorAll("canvas")].find(isVisible);if(canvas){saveCanvas(canvas,name);return true;}
    const svg=chart.tagName.toLowerCase()==="svg"?chart:[...chart.querySelectorAll("svg")].find(isVisible);if(svg){svgToPng(svg,name);return true;}
    htmlToPng(chart,name);return true;
  }
  document.addEventListener("click",event=>{
    const button=event.target.closest("button");
    if(!button||button.textContent.trim()!=="Save Image"||!button.closest("#page-visualisations"))return;
    event.preventDefault();event.stopImmediatePropagation();
    if(!saveImage(button)){
      const panel=panelFor(button),message=panel.querySelector("[aria-live],.notice,.message");
      if(message)message.textContent="Create a visualisation before saving an image.";
    }
  },true);
})();
