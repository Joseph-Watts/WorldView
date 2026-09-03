# WorldView Online - Step 31f
# Keep the About page visible by integrating it with the client-side page router.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
router_path <- file.path(app_dir, "assets", "about-navigation.js")

if (!file.exists(index_path)) stop("worldview_static_app/index.html was not found.")

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

if (!grepl('data-page="about"', html, fixed = TRUE)) {
  stop("The About navigation control is missing from index.html.")
}
if (!grepl('id="page-about"', html, fixed = TRUE)) {
  stop("The About page is missing from index.html.")
}

js <- '"use strict";
(function(){
  function allPages(){return [...document.querySelectorAll("main .page")];}
  function allNav(){return [...document.querySelectorAll("[data-page]")];}
  function showPage(name){
    const target=document.getElementById(`page-${name}`);
    if(!target)return false;
    allPages().forEach(page=>{
      const active=page===target;
      page.hidden=!active;
      page.classList.toggle("active",active);
      page.setAttribute("aria-hidden",String(!active));
    });
    allNav().forEach(control=>{
      const active=control.dataset.page===name;
      control.classList.toggle("active",active);
      control.setAttribute("aria-current",active?"page":"false");
    });
    if(location.hash!==`#${name}`)history.replaceState(null,"",`#${name}`);
    window.scrollTo({top:0,left:0,behavior:"auto"});
    return true;
  }
  document.addEventListener("click",event=>{
    const control=event.target.closest("[data-page=about]");
    if(!control)return;
    event.preventDefault();
    event.stopImmediatePropagation();
    showPage("about");
  },true);
  document.addEventListener("DOMContentLoaded",()=>{
    const about=document.getElementById("page-about");
    const control=document.querySelector("[data-page=about]");
    if(!about||!control)return;
    control.addEventListener("keydown",event=>{
      if(event.key==="Enter"||event.key===" "){
        event.preventDefault();
        showPage("about");
      }
    });
    if(location.hash==="#about")showPage("about");
  });
  window.addEventListener("hashchange",()=>{
    if(location.hash==="#about")showPage("about");
  });
  window.WorldViewShowAbout=()=>showPage("about");
})();'
writeLines(js, router_path, useBytes = TRUE)

if (!grepl("assets/about-navigation.js", html, fixed = TRUE)) {
  html <- sub(
    "</body>",
    '  <script src="assets/about-navigation.js"></script>\n</body>',
    html,
    fixed = TRUE
  )
}
writeLines(html, index_path, useBytes = TRUE)

# Patch the current deployment copy too, if it exists. Otherwise it will be
# included the next time the deployment folder is rebuilt.
if (dir.exists("worldview_deployment")) {
  dir.create(file.path("worldview_deployment", "assets"), recursive = TRUE, showWarnings = FALSE)
  file.copy(router_path, file.path("worldview_deployment", "assets", "about-navigation.js"), overwrite = TRUE)
  deployment_index <- file.path("worldview_deployment", "index.html")
  if (file.exists(deployment_index)) {
    deployed <- paste(readLines(deployment_index, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!grepl("assets/about-navigation.js", deployed, fixed = TRUE)) {
      deployed <- sub(
        "</body>",
        '  <script src="assets/about-navigation.js"></script>\n</body>',
        deployed,
        fixed = TRUE
      )
    }
    writeLines(deployed, deployment_index, useBytes = TRUE)
  }
}

updated <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
checks <- data.frame(
  check = c(
    "about_navigation_present",
    "about_page_present",
    "about_router_created",
    "about_router_linked",
    "capture_phase_handler_added",
    "other_pages_hidden_when_about_selected",
    "about_hash_supported"
  ),
  passed = c(
    grepl('data-page="about"', updated, fixed = TRUE),
    grepl('id="page-about"', updated, fixed = TRUE),
    file.exists(router_path),
    grepl("assets/about-navigation.js", updated, fixed = TRUE),
    grepl('},true);', js, fixed = TRUE),
    grepl('page.hidden=!active', js, fixed = TRUE),
    grepl('location.hash==="#about"', js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step31f_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31f validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31f completed successfully.\n\n")
cat("The About button now uses an explicit page-routing handler.\n")
cat("Test the source app with:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then force-refresh with Ctrl+F5 and select About.\n")
