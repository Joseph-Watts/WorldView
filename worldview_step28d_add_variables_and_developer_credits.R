# WorldView Online - Step 28d
# Adds Q18-Q26 and Q238 from the original row-aligned source, and updates About credits.
# This patch does not undo the UI updates already applied by Step 28c before it stopped.

app_dir <- "worldview_static_app"
index_path <- file.path(app_dir, "index.html")
data_path <- file.path(app_dir, "data", "worldview-browser-data-v1.0.0.json")
codebook_path <- file.path(app_dir, "data", "worldview-codebook-v1.0.0.json")

if (!all(file.exists(c(index_path, data_path, codebook_path)))) {
  stop("Required WorldView files were not found.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite first: install.packages('jsonlite')")
}

requested <- c(paste0("Q", 18:26), "Q238")
labels <- c(
  Q18 = "One of my main goals in life has been to make my parents proud",
  Q19 = "Parents have a duty to do their best for their children",
  Q20 = "Respect and love for parents regardless of their qualities and faults",
  Q21 = "Men make better political leaders than women do",
  Q22 = "University is more important for a boy than for a girl",
  Q23 = "Men make better business executives than women do",
  Q24 = "Being a housewife is just as fulfilling as working for pay",
  Q25 = "When jobs are scarce, men should have more right to a job than women",
  Q26 = "It is a problem if women have more income than their husbands",
  Q238 = "Having a strong leader who does not have to bother with parliament and elections"
)

browser <- jsonlite::fromJSON(
  data_path,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
expected_rows <- length(browser[[1]])

# Search the likely source files used during the earlier WorldView processing steps.
source_candidates <- c(
  file.path("worldview_step3_output", "private_data", "worldview_processed_full_private.rds"),
  file.path("worldview_step3_output", "private", "worldview_processed_full_private.rds"),
  "worldview_processed_full_private.rds",
  file.path("WVS_Dataset", "WVS7_Individual.rds"),
  "WVS7_Individual.rds",
  file.path("WVS_Dataset", "WVS7_Individual.csv"),
  "WVS7_Individual.csv",
  file.path(app_dir, "data", "worldview-wave7-teaching-data-v1.0.0.csv")
)
source_candidates <- unique(source_candidates[file.exists(source_candidates)])

read_candidate <- function(path) {
  tryCatch({
    if (grepl("\\.rds$", path, ignore.case = TRUE)) {
      object <- readRDS(path)
      if (is.data.frame(object)) return(object)
      if (is.list(object)) {
        frames <- Filter(is.data.frame, object)
        if (length(frames)) {
          scores <- vapply(frames, function(x) sum(requested %in% names(x)), integer(1))
          return(frames[[which.max(scores)]])
        }
      }
      NULL
    } else {
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) NULL)
}

source_data <- NULL
source_used <- NA_character_
for (path in source_candidates) {
  candidate <- read_candidate(path)
  if (is.null(candidate)) next
  if (all(requested %in% names(candidate)) && nrow(candidate) == expected_rows) {
    source_data <- candidate
    source_used <- path
    break
  }
}

if (is.null(source_data)) {
  diagnostic <- vapply(source_candidates, function(path) {
    candidate <- read_candidate(path)
    if (is.null(candidate)) return(paste(path, "could not be read"))
    paste0(
      path, ": rows=", nrow(candidate),
      "; requested columns found=", paste(intersect(requested, names(candidate)), collapse = ",")
    )
  }, character(1))
  writeLines(
    c(
      "Step 28d could not find a row-aligned source containing all requested variables.",
      paste("Expected rows:", expected_rows),
      diagnostic
    ),
    file.path(app_dir, "step28d_source_diagnostic.txt")
  )
  stop(
    "No row-aligned source containing Q18-Q26 and Q238 was found. ",
    "See worldview_static_app/step28d_source_diagnostic.txt. ",
    "The preferred source is worldview_step3_output/private_data/worldview_processed_full_private.rds ",
    "or WVS_Dataset/WVS7_Individual.rds."
  )
}

# Apply the original missing-value convention used by WVS source data.
clean_wvs <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  numeric <- suppressWarnings(as.numeric(x))
  if (sum(!is.na(numeric)) >= sum(!is.na(x)) * 0.8) {
    numeric[numeric < 0] <- NA_real_
    return(unname(numeric))
  }
  x[x %in% c("-1", "-2", "-3", "-4", "-5")] <- NA
  unname(x)
}

for (id in requested) browser[[id]] <- clean_wvs(source_data[[id]])

for (id in requested) {
  existing <- which(vapply(codebook$variables, function(v) identical(v$id, id), logical(1)))
  metadata <- list(
    id = id,
    displayName = unname(labels[id]),
    analysisType = "ordinal",
    correlationEligible = TRUE,
    correlationRepresentation = "Original WVS response coding; negative missing-value codes excluded",
    topic = if (id == "Q238") {
      "Political culture and political regimes"
    } else {
      "Social values, norms and stereotypes"
    }
  )
  if (length(existing)) {
    codebook$variables[[existing[1]]] <- modifyList(codebook$variables[[existing[1]]], metadata)
  } else {
    codebook$variables[[length(codebook$variables) + 1L]] <- metadata
  }
}

jsonlite::write_json(
  browser, data_path,
  pretty = FALSE, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA
)
jsonlite::write_json(
  codebook, codebook_path,
  pretty = TRUE, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA
)

# Update the About page with the developers explicitly credited by the repository.
html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
credits <- paste0(
  '<article class="about-card"><h2>Development team</h2>',
  '<p>WorldView was developed by Joseph Watts, Nicki Cartlidge, and André De Vito.</p>',
  '<ul><li>Joseph Watts</li><li>Nicki Cartlidge</li><li>André De Vito</li></ul>',
  '<p>The current static-browser adaptation was developed from the original WorldView Shiny application for teaching use at the University of Canterbury.</p></article>'
)

# Replace the temporary single-developer card if present.
html <- gsub(
  '<article class="about-card"><h2>Development</h2>[\\s\\S]*?</article>',
  credits,
  html,
  perl = TRUE
)

# If Step 28c stopped before adding About, create the page now.
if (!grepl('id="page-about"', html, fixed = TRUE)) {
  if (!grepl('data-page="about"', html, fixed = TRUE)) {
    html <- sub(
      '</nav>',
      '  <button class="nav-link" type="button" data-page="about">About</button>\n    </nav>',
      html,
      fixed = TRUE
    )
  }
  about <- paste0(
    '<section id="page-about" class="page" hidden>',
    '<div class="page-heading"><p class="eyebrow">About WorldView</p><h1>About</h1>',
    '<p>WorldView is a teaching application for exploring and visualising World Values Survey data without requiring users to process the source data or write R code.</p></div>',
    '<div class="about-grid">', credits,
    '<article class="about-card"><h2>Purpose</h2><p>The application supports learning about cultural variation using summary statistics, cross-tabulations, visualisations, phylogenetic and geographic comparisons, and introductory statistical models.</p></article>',
    '<article class="about-card"><h2>Use and interpretation</h2><p>The application is intentionally flexible. Users remain responsible for selecting analyses appropriate to the variables, assumptions, and research question.</p></article>',
    '<article class="about-card"><h2>Sources</h2><p>Survey data and question wording are derived from World Values Survey Wave 7. Country boundaries use Natural Earth. The phylogeny uses the country-language tree and mapping supplied with the WorldView project.</p></article>',
    '</div></section>'
  )
  html <- sub('</main>', paste0(about, '\n  </main>'), html, fixed = TRUE)
}
writeLines(html, index_path, useBytes = TRUE)

checks <- data.frame(
  check = c(
    "all_requested_columns_added",
    "all_requested_codebook_records_added",
    "requested_values_have_expected_length",
    "negative_missing_codes_removed",
    "joseph_watts_credited",
    "nicki_cartlidge_credited",
    "andre_de_vito_credited",
    "about_page_present"
  ),
  passed = c(
    all(requested %in% names(browser)),
    all(requested %in% vapply(codebook$variables, function(v) v$id, character(1))),
    all(vapply(requested, function(id) length(browser[[id]]) == expected_rows, logical(1))),
    all(vapply(requested, function(id) all(is.na(browser[[id]]) | browser[[id]] >= 0), logical(1))),
    grepl("Joseph Watts", html, fixed = TRUE),
    grepl("Nicki Cartlidge", html, fixed = TRUE),
    grepl("André De Vito", html, fixed = TRUE),
    grepl('id="page-about"', html, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step28d_validation_checks.csv"), row.names = FALSE)
writeLines(
  c(
    "WorldView Step 28d",
    paste("Source used:", source_used),
    paste("Respondent rows:", expected_rows),
    paste("Variables added:", paste(requested, collapse = ", ")),
    paste("Checks passed:", sum(checks$passed), "of", nrow(checks))
  ),
  file.path(app_dir, "step28d_summary.txt")
)
if (!all(checks$passed)) {
  stop("Step 28d validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 28d completed successfully.\n")
cat("Source used: ", source_used, "\n", sep = "")
cat("Added Q18-Q26 and Q238.\n")
cat("About credits now include Joseph Watts, Nicki Cartlidge, and André De Vito.\n")
cat("Restart and force-refresh:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
