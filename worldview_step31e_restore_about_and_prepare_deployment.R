# WorldView Online - Step 31e
# Ensure Download is replaced by About, then recreate the clean deployment folder.

source_dir <- "worldview_static_app"
deploy_dir <- "worldview_deployment"
index_path <- file.path(source_dir, "index.html")

if (!file.exists(index_path)) stop("worldview_static_app/index.html was not found.")

html <- paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Remove any remaining Download navigation controls and page.
html <- gsub(
  "<button[^>]*data-page=\\\"download\\\"[^>]*>[\\s\\S]*?</button>",
  "", html, perl = TRUE, ignore.case = TRUE
)
html <- gsub(
  "<a[^>]*href=\\\"#download\\\"[^>]*>[\\s\\S]*?</a>",
  "", html, perl = TRUE, ignore.case = TRUE
)
html <- gsub(
  "<section[^>]*id=\\\"page-download\\\"[\\s\\S]*?</section>",
  "", html, perl = TRUE, ignore.case = TRUE
)

# Ensure an About navigation control exists.
if (!grepl('data-page="about"', html, fixed = TRUE)) {
  html <- sub(
    "</nav>",
    '  <button class="nav-link" type="button" data-page="about">About</button>\n    </nav>',
    html,
    fixed = TRUE
  )
}

# Ensure an About page exists with the confirmed developer credits.
if (!grepl('id="page-about"', html, fixed = TRUE)) {
  about <- paste0(
    '<section id="page-about" class="page" hidden>',
    '<div class="page-heading">',
    '<p class="eyebrow">About WorldView</p>',
    '<h1>About</h1>',
    '<p>WorldView is a teaching application for exploring and visualising World Values Survey data without requiring users to process the source data or write R code.</p>',
    '</div>',
    '<div class="about-grid">',
    '<article class="about-card"><h2>Purpose</h2><p>The application supports learning about cultural variation through summary statistics, cross-tabulations, visualisations, geographic and phylogenetic comparisons, and introductory statistical models.</p></article>',
    '<article class="about-card"><h2>Development team</h2><p>WorldView was developed by Joseph Watts, Nicki Cartlidge, and André De Vito.</p><ul><li>Joseph Watts</li><li>Nicki Cartlidge</li><li>André De Vito</li></ul><p>The current static-browser version was adapted from the original WorldView Shiny application for teaching use at the University of Canterbury.</p></article>',
    '<article class="about-card"><h2>Use and interpretation</h2><p>WorldView is intentionally flexible. Users remain responsible for selecting analyses appropriate to their variables, assumptions, and research questions.</p></article>',
    '<article class="about-card"><h2>Sources</h2><p>Survey data and question wording are derived from World Values Survey Wave 7. Country boundaries use Natural Earth. The phylogeny uses the country-language tree and mapping supplied with the WorldView project.</p></article>',
    '</div>',
    '</section>'
  )
  html <- sub("</main>", paste0(about, "\n  </main>"), html, fixed = TRUE)
}

# Remove obsolete CSV links without removing About.
obsolete <- c(
  "data/worldview-wave7-teaching-data-v1.0.0.csv",
  "data/worldview-codebook-v1.0.0.csv"
)
for (target in obsolete) {
  escaped <- gsub("([.])", "\\\\\\1", target)
  html <- gsub(
    paste0("<a[^>]*href=\\\"", escaped, "(?:[?#][^\\\"]*)?\\\"[^>]*>[\\s\\S]*?</a>"),
    "", html, perl = TRUE, ignore.case = TRUE
  )
}

writeLines(html, index_path, useBytes = TRUE)

# Recreate deployment folder from the corrected source app.
if (dir.exists(deploy_dir)) unlink(deploy_dir, recursive = TRUE, force = TRUE)
dir.create(deploy_dir, recursive = TRUE, showWarnings = FALSE)
items <- list.files(source_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
ok <- file.copy(items, deploy_dir, recursive = TRUE, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE)
if (!all(ok)) stop("One or more application items could not be copied.")

# Remove development reports, backups, discontinued CSVs, and private/source data.
all_paths <- list.files(deploy_dir, recursive = TRUE, full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
remove <- grepl("(^|/)step[0-9].*\\.(csv|txt)$", all_paths, ignore.case = TRUE) |
  grepl("(^|/)before_step", all_paths, ignore.case = TRUE) |
  grepl("worldview-wave7-teaching-data-v1\\.0\\.0\\.csv$", all_paths, ignore.case = TRUE) |
  grepl("worldview-codebook-v1\\.0\\.0\\.csv$", all_paths, ignore.case = TRUE) |
  grepl("\\.(rds|rdata|rda|sav|dta|xlsx|xls)$", all_paths, ignore.case = TRUE)
if (any(remove)) unlink(all_paths[remove], recursive = TRUE, force = TRUE)

# Validate the exact requested page replacement and required runtime assets.
deployed_html <- paste(readLines(file.path(deploy_dir, "index.html"), warn = FALSE), collapse = "\n")
required <- c(
  "index.html", "validation.html",
  file.path("assets", "styles.css"),
  file.path("assets", "app.js"),
  file.path("assets", "validation.js"),
  file.path("assets", "worldview-favicon.svg"),
  file.path("data", "manifest-v1.0.0.json"),
  file.path("data", "worldview-browser-data-v1.0.0.json"),
  file.path("data", "worldview-codebook-v1.0.0.json")
)

checks <- data.frame(
  check = c(
    "about_navigation_present",
    "about_page_present",
    "download_navigation_absent",
    "download_page_absent",
    "all_three_developers_credited",
    "obsolete_csv_links_absent",
    "required_runtime_files_present",
    "discontinued_csv_files_absent"
  ),
  passed = c(
    grepl('data-page="about"', deployed_html, fixed = TRUE),
    grepl('id="page-about"', deployed_html, fixed = TRUE),
    !grepl('data-page="download"', deployed_html, fixed = TRUE),
    !grepl('id="page-download"', deployed_html, fixed = TRUE),
    all(vapply(c("Joseph Watts", "Nicki Cartlidge", "André De Vito"), grepl,
               logical(1), x = deployed_html, fixed = TRUE)),
    !any(vapply(obsolete, grepl, logical(1), x = deployed_html, fixed = TRUE)),
    all(file.exists(file.path(deploy_dir, required))),
    !any(file.exists(file.path(deploy_dir, obsolete)))
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(deploy_dir, "deployment-checks.csv"), row.names = FALSE)
if (!all(checks$passed)) {
  stop("Step 31e validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 31e completed successfully.\n\n")
cat("Download has been replaced by About in both the source and deployment copies.\n")
cat("Deployment folder: ", normalizePath(deploy_dir, winslash = "/"), "\n", sep = "")
cat("Final local test:\n")
cat("  servr::httd(\"worldview_deployment\", browser = TRUE)\n")
