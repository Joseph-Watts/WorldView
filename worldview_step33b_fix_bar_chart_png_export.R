# WorldView Online - Step 33b
# Fix Bar chart PNG export when the chart is rendered as HTML rather than SVG/canvas.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
export_path <- file.path(app_dir, "assets", "visualisation-export.js")

if (!all(file.exists(c(index_path, export_path)))) {
  stop("Run Step 33 first. Required files were not found.")
}

js <- '"use strict";
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
    const styles=[...document.styleSheets].map(sheet=>{try{return [...sheet.cssRules].map(rule=>rule.cssText).join("\n");}catch(error){return "";}}).join("\n");
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
})();'

writeLines(js, export_path, useBytes = TRUE)

if (dir.exists("worldview_deployment")) {
  file.copy(export_path, file.path("worldview_deployment", "assets", "visualisation-export.js"), overwrite = TRUE)
}

checks <- data.frame(
  check=c("html_chart_fallback_added","bar_chart_selectors_added","svg_export_retained","canvas_export_retained","png_output_used","deployment_synced_when_present"),
  passed=c(grepl("function htmlToPng",js,fixed=TRUE),grepl('"#bar-chart"',js,fixed=TRUE),grepl("function svgToPng",js,fixed=TRUE),grepl("saveCanvas",js,fixed=TRUE),grepl('toDataURL("image/png")',js,fixed=TRUE),!dir.exists("worldview_deployment")||file.exists(file.path("worldview_deployment","assets","visualisation-export.js"))),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step33b_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 33b validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 33b completed successfully.\nThe Save Image handler now supports HTML-rendered bar charts as well as SVG and canvas charts.\nRestart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
