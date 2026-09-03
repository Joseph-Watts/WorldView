# WorldView Online - Step 27b
# Fix phylogeny branch rendering and replace raw SVG export with a styled PNG.

app_dir <- "worldview_static_app"
js_path <- file.path(app_dir, "assets", "phylogeny.js")
css_path <- file.path(app_dir, "assets", "styles.css")
index_path <- file.path(app_dir, "index.html")

if (!all(file.exists(c(js_path, css_path, index_path)))) {
  stop("Run Step 27 first. Required phylogeny files were not found.")
}

js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

# Give branches explicit SVG attributes. This avoids reliance on an external
# stylesheet both in the browser and in exported images.
js <- gsub(
  'class="phy-branch"/>',
  'class="phy-branch" stroke="#495957" stroke-width="1.35" fill="none" vector-effect="non-scaling-stroke"/>',
  js,
  fixed = TRUE
)

# Give tip labels explicit SVG attributes so they survive serialization.
js <- gsub(
  'class="phy-label">',
  'class="phy-label" font-family="Arial, sans-serif" font-size="11" fill="#24302e">',
  js,
  fixed = TRUE
)

# Add a white plotting background inside the SVG itself.
old_svg_start <- 'let svg=`<svg viewBox="0 0 ${width} ${height}" xmlns="http://www.w3.org/2000/svg">`;'
new_svg_start <- 'let svg=`<svg viewBox="0 0 ${width} ${height}" width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg"><rect x="0" y="0" width="${width}" height="${height}" fill="#ffffff"/>`;'
if (grepl(old_svg_start, js, fixed = TRUE)) {
  js <- sub(old_svg_start, new_svg_start, js, fixed = TRUE)
} else if (!grepl('fill="#ffffff"/>`;', js, fixed = TRUE)) {
  stop("Could not locate the phylogeny SVG creation block.")
}

# Replace the previous raw-SVG download with a PNG export. The old export did
# not include external CSS, so branches and labels could disappear.
start <- regexpr("function save\\(\\)\\{", js, perl = TRUE)
if (start[1] < 0) stop("Could not find the phylogeny save function.")
tail_text <- substring(js, start[1])
reset_pos <- regexpr("function reset\\(\\)", tail_text, perl = TRUE)
if (reset_pos[1] < 0) stop("Could not find reset() after save().")
old_save <- substring(tail_text, 1, reset_pos[1] - 1)

new_save <- 'function save(){const source=document.querySelector("#phylo-chart svg");if(!source)return;const clone=source.cloneNode(true),box=source.viewBox.baseVal,width=Math.max(1100,Math.ceil(box.width)),height=Math.ceil(box.height),caption=document.getElementById("phylo-caption").textContent,extra=80,scale=2;clone.setAttribute("width",width);clone.setAttribute("height",height);clone.setAttribute("xmlns","http://www.w3.org/2000/svg");const svgText=new XMLSerializer().serializeToString(clone),blob=new Blob([svgText],{type:"image/svg+xml;charset=utf-8"}),url=URL.createObjectURL(blob),image=new Image(),canvas=document.createElement("canvas");canvas.width=width*scale;canvas.height=(height+extra)*scale;const context=canvas.getContext("2d");image.onload=()=>{context.scale(scale,scale);context.fillStyle="#ffffff";context.fillRect(0,0,width,height+extra);context.drawImage(image,0,0,width,height);context.fillStyle="#35413f";context.font="15px Arial";const words=caption.split(" ");let line="",y=height+28;words.forEach(word=>{const test=`${line}${word} `;if(context.measureText(test).width>width-50){context.fillText(line,25,y);line=`${word} `;y+=20;}else line=test;});context.fillText(line,25,y);const link=document.createElement("a");link.download=`worldview-phylogeny-${document.getElementById("phylo-variable").value}.png`;link.href=canvas.toDataURL("image/png");link.click();URL.revokeObjectURL(url);};image.onerror=()=>{URL.revokeObjectURL(url);document.getElementById("phylo-message").textContent="The tree image could not be created in this browser.";};image.src=url;}\n'
js <- sub(old_save, new_save, js, fixed = TRUE)

# Clarify the button label.
html <- gsub(
  '>Save tree image</button>',
  '>Save tree image (PNG)</button>',
  html,
  fixed = TRUE
)

# Make branches visually stronger in the live tree too.
css <- gsub(
  ".phy-branch { stroke: #677572; stroke-width: 1.1; fill: none; }",
  ".phy-branch { stroke: #495957; stroke-width: 1.35; fill: none; }",
  css,
  fixed = TRUE
)
if (!grepl("Step 27b: phylogeny rendering", css, fixed = TRUE)) {
  cat('\n/* Step 27b: phylogeny rendering and export */\n.phylo-chart { background: #ffffff; }\n.phylo-chart .phy-branch { stroke: #495957 !important; stroke-width: 1.35 !important; }\n', file = css_path, append = TRUE)
}

writeLines(js, js_path, useBytes = TRUE)
writeLines(html, index_path, useBytes = TRUE)
writeLines(css, css_path, useBytes = TRUE)

updated_js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "branches_have_inline_stroke",
    "labels_have_inline_font",
    "svg_has_white_background",
    "png_export_added",
    "caption_included_in_export",
    "button_identifies_png",
    "raw_svg_download_removed"
  ),
  passed = c(
    grepl('stroke="#495957" stroke-width="1.35"', updated_js, fixed = TRUE),
    grepl('font-family="Arial, sans-serif"', updated_js, fixed = TRUE),
    grepl('width="${width}" height="${height}" fill="#ffffff"', updated_js, fixed = TRUE),
    grepl('toDataURL("image/png")', updated_js, fixed = TRUE),
    grepl('caption=document.getElementById("phylo-caption")', updated_js, fixed = TRUE),
    grepl("Save tree image (PNG)", updated_html, fixed = TRUE),
    !grepl('a.download=`worldview-phylogeny-${document.getElementById("phylo-variable").value}.svg`', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step27b_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 27b validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 27b completed successfully.\n\n")
cat("Fixes:\n")
cat("  - Tree branches now have explicit SVG stroke attributes.\n")
cat("  - Tip labels now have explicit SVG font attributes.\n")
cat("  - Saved images are PNG files with the tree structure and caption.\n\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5 in the browser.\n")
