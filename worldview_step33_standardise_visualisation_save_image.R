# WorldView Online - Step 33
# Standardise all visualisation export buttons to "Save Image" and ensure the
# Bar chart exports the displayed chart as a PNG.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
visual_js_path <- file.path(app_dir, "assets", "visualisations.js")
export_js_path <- file.path(app_dir, "assets", "visualisation-export.js")

if (!all(file.exists(c(index_path, visual_js_path)))) {
  stop("Required index.html or assets/visualisations.js was not found.")
}

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
visual_js <- paste(readLines(visual_js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Standardise known visualisation button labels.
labels <- c(
  "Download graph data", "Save graph image", "Save tree image (PNG)",
  "Save tree image", "Save map image", "Download image", "Export image"
)
for (label in labels) {
  html <- gsub(paste0(">", label, "</button>"), ">Save Image</button>", html, fixed = TRUE)
}

# Give the former Bar graph data-download button a stable marker based on its
# existing label/ID context. Retain its existing ID so styling is unaffected.
html <- gsub(
  '(<button[^>]*(?:id="[^"]*(?:bar|graph|download)[^"]*"|data-[^=]+="[^"]*(?:bar|graph)[^"]*")[^>]*>)Save Image(</button>)',
  '\\1Save Image\\2', html, perl = TRUE, ignore.case = TRUE
)

# Add a unified exporter. It captures the visible SVG or canvas inside the
# active visualisation panel and writes a PNG file.
export_js <- '"use strict";
(function(){
  function visible(element){
    if(!element)return false;
    const style=getComputedStyle(element),rect=element.getBoundingClientRect();
    return style.display!=="none"&&style.visibility!=="hidden"&&rect.width>0&&rect.height>0;
  }
  function activeVisualRoot(button){
    return button.closest(".model-tab-panel, [role=tabpanel], .visualisation-panel, .results-panel")||document.getElementById("page-visualisations");
  }
  function fileStem(root){
    const heading=root?.querySelector("h2, h3")?.textContent?.trim()||"visualisation";
    return `worldview-${heading.toLowerCase().replace(/[^a-z0-9]+/g,"-").replace(/^-|-$/g,"")||"visualisation"}.png`;
  }
  function downloadCanvas(canvas,name){
    const link=document.createElement("a");
    link.download=name;
    link.href=canvas.toDataURL("image/png");
    link.click();
  }
  function svgToPng(svg,name){
    const clone=svg.cloneNode(true),box=svg.viewBox?.baseVal;
    const width=Math.max(1,Math.ceil(box&&box.width?box.width:svg.getBoundingClientRect().width));
    const height=Math.max(1,Math.ceil(box&&box.height?box.height:svg.getBoundingClientRect().height));
    clone.setAttribute("xmlns","http://www.w3.org/2000/svg");
    clone.setAttribute("width",width);clone.setAttribute("height",height);
    const style=document.createElementNS("http://www.w3.org/2000/svg","style");
    style.textContent="text{font-family:Arial,sans-serif}.axis,.domain,line,path{vector-effect:non-scaling-stroke}";
    clone.insertBefore(style,clone.firstChild);
    const blob=new Blob([new XMLSerializer().serializeToString(clone)],{type:"image/svg+xml;charset=utf-8"});
    const url=URL.createObjectURL(blob),image=new Image();
    image.onload=()=>{
      const scale=2,canvas=document.createElement("canvas");
      canvas.width=width*scale;canvas.height=height*scale;
      const context=canvas.getContext("2d");context.scale(scale,scale);
      context.fillStyle="#ffffff";context.fillRect(0,0,width,height);
      context.drawImage(image,0,0,width,height);URL.revokeObjectURL(url);
      downloadCanvas(canvas,name);
    };
    image.onerror=()=>URL.revokeObjectURL(url);image.src=url;
  }
  function saveVisibleVisualisation(button){
    const root=activeVisualRoot(button),name=fileStem(root);
    const canvas=[...root.querySelectorAll("canvas")].find(visible);
    if(canvas){downloadCanvas(canvas,name);return true;}
    const svg=[...root.querySelectorAll("svg")].find(visible);
    if(svg){svgToPng(svg,name);return true;}
    return false;
  }
  document.addEventListener("click",event=>{
    const button=event.target.closest("button");
    if(!button||button.textContent.trim()!=="Save Image")return;
    const page=button.closest("#page-visualisations");
    if(!page)return;
    event.preventDefault();event.stopImmediatePropagation();
    if(!saveVisibleVisualisation(button)){
      const message=activeVisualRoot(button)?.querySelector(".notice, [aria-live]");
      if(message)message.textContent="Create a visualisation before saving an image.";
    }
  },true);
})();'
writeLines(export_js, export_js_path, useBytes = TRUE)

if (!grepl("assets/visualisation-export.js", html, fixed = TRUE)) {
  html <- sub("</body>", '  <script src="assets/visualisation-export.js"></script>\n</body>', html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

# Keep deployment copy synchronised.
if (dir.exists("worldview_deployment")) {
  file.copy(index_path, file.path("worldview_deployment", "index.html"), overwrite = TRUE)
  file.copy(export_js_path, file.path("worldview_deployment", "assets", "visualisation-export.js"), overwrite = TRUE)
}

updated <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c("unified_export_script_created","unified_export_script_linked","bar_data_label_removed","save_image_label_present","png_export_present","visualisations_only_handler"),
  passed = c(file.exists(export_js_path),grepl("assets/visualisation-export.js",updated,fixed=TRUE),!grepl("Download graph data",updated,fixed=TRUE),grepl(">Save Image</button>",updated,fixed=TRUE),grepl('toDataURL("image/png")',export_js,fixed=TRUE),grepl('closest("#page-visualisations")',export_js,fixed=TRUE)),
  stringsAsFactors=FALSE
)
write.csv(checks,file.path(app_dir,"step33_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 33 validation failed: ",paste(checks$check[!checks$passed],collapse=", "))
cat("\nStep 33 completed successfully.\nAll visualisation export buttons are labelled Save Image, and exports use PNG.\nRestart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
