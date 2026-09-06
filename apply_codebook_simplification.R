# WorldView codebook simplification
# Run from the WorldView repository root.

required <- c("jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required package(s): ", paste(missing, collapse = ", "))
}

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
codebook_path <- file.path(repo_root, "worldview_deployment", "data", "worldview-codebook-v1.0.0.json")
app_path <- file.path(repo_root, "worldview_deployment", "assets", "app.js")
build_path <- file.path(repo_root, "R", "worldview_build.R")

for (path in c(codebook_path, app_path, build_path)) {
  if (!file.exists(path)) stop("Required file not found: ", path)
}

# ---- 1. Normalise codebook metadata -------------------------------------

cb <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)

binary_levels <- list(
  Q165 = list(
    list(value = 0, label = "No", displayOrder = 0),
    list(value = 1, label = "Yes", displayOrder = 1)
  ),
  Q166 = list(
    list(value = 0, label = "No", displayOrder = 0),
    list(value = 1, label = "Yes", displayOrder = 1)
  ),
  Q167 = list(
    list(value = 0, label = "No", displayOrder = 0),
    list(value = 1, label = "Yes", displayOrder = 1)
  ),
  Q168 = list(
    list(value = 0, label = "No", displayOrder = 0),
    list(value = 1, label = "Yes", displayOrder = 1)
  ),
  Q260 = list(
    list(value = 0, label = "Female", displayOrder = 0),
    list(value = 1, label = "Male", displayOrder = 1)
  ),
  Q263 = list(
    list(value = 0, label = "Immigrant to this country (born outside this country)", displayOrder = 0),
    list(value = 1, label = "Born in this country", displayOrder = 1)
  )
)

binary_representation <- c(
  Q165 = "0 = No; 1 = Yes",
  Q166 = "0 = No; 1 = Yes",
  Q167 = "0 = No; 1 = Yes",
  Q168 = "0 = No; 1 = Yes",
  Q260 = "0 = Female; 1 = Male",
  Q263 = "0 = Immigrant to this country; 1 = Born in this country"
)

normalise_numeric_levels <- function(levels) {
  if (is.null(levels) || !length(levels)) return(levels)

  has_numeric_value <- vapply(levels, function(level) {
    !is.null(level$value) &&
      length(level$value) == 1L &&
      !is.na(suppressWarnings(as.numeric(level$value)))
  }, logical(1))

  if (!all(has_numeric_value)) return(levels)

  values <- vapply(levels, function(level) as.numeric(level$value), numeric(1))
  levels <- levels[order(values)]

  lapply(levels, function(level) {
    level$displayOrder <- as.numeric(level$value)
    level
  })
}

for (i in seq_along(cb$variables)) {
  v <- cb$variables[[i]]
  id <- v$id

  if (id %in% names(binary_levels)) {
    v$levels <- binary_levels[[id]]
    v$correlationRepresentation <- unname(binary_representation[[id]])
    v$validRange <- list(minimum = 0, maximum = 1)
  } else {
    v$levels <- normalise_numeric_levels(v$levels)
  }

  if (identical(id, "Q263")) {
    v$displayName <- "Respondent immigrant status"
  }

  if (identical(v$analysisType, "binary")) {
    v$interpretationNote <- "Analyses use the processed 0/1 values shown above."
  } else if (identical(v$analysisType, "ordinal")) {
    v$interpretationNote <- "Analyses use the ordered processed values shown above."
  } else if (identical(v$analysisType, "integer")) {
    v$interpretationNote <- "Analyses use the processed integer value directly."
  }

  cb$variables[[i]] <- v
}

if (!is.null(cb$generalPreprocessing)) {
  cb$generalPreprocessing$categoricalValues <-
    "The processed values shown in the codebook are the values used throughout WorldView analyses and visualisations."
  cb$generalPreprocessing$correlations <-
    "Numerical analyses use the processed values shown for each variable. Multi-category nominal variables are excluded from numerical correlation."
}

jsonlite::write_json(
  cb,
  codebook_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  na = "null"
)

# ---- 2. Simplify Codebook page rendering --------------------------------

app <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

old_render_levels <- paste0(
  'function renderLevels(variable) {\n',
  '  if (!Array.isArray(variable.levels) || variable.levels.length === 0) {\n',
  '    if (variable.validRange) {\n',
  '      return `<p>${escapeHtml(variable.validRange.minimum)} to ${escapeHtml(variable.validRange.maximum)}</p>`;\n',
  '    }\n',
  '    return "<p>Not specified</p>";\n',
  '  }\n\n',
  '  return `<ol class="level-list">${variable.levels.map(level =>\n',
  '    `<li value="${escapeHtml(level.displayOrder)}">${escapeHtml(level.label)}</li>`\n',
  '  ).join("")}</ol>`;\n',
  '}'
)

new_render_levels <- paste0(
  'function renderLevels(variable) {\n',
  '  if (!Array.isArray(variable.levels) || variable.levels.length === 0) {\n',
  '    if (variable.validRange) {\n',
  '      return `<p>${escapeHtml(variable.validRange.minimum)} to ${escapeHtml(variable.validRange.maximum)}</p>`;\n',
  '    }\n',
  '    return "<p>Not specified</p>";\n',
  '  }\n\n',
  '  return `<div class="table-wrap"><table class="results-table codebook-values-table">\n',
  '    <thead><tr><th>Value</th><th>Label</th></tr></thead>\n',
  '    <tbody>${variable.levels.map(level => `<tr>\n',
  '      <td>${escapeHtml(level.value ?? level.displayOrder ?? "")}</td>\n',
  '      <td>${escapeHtml(level.label)}</td>\n',
  '    </tr>`).join("")}</tbody>\n',
  '  </table></div>`;\n',
  '}'
)

if (!grepl(old_render_levels, app, fixed = TRUE)) {
  stop("Could not find expected renderLevels() block in app.js.")
}
app <- sub(old_render_levels, new_render_levels, app, fixed = TRUE)

start_marker <- 'function variableCard(variable) {'
end_marker <- '\n\nfunction populateFilters()'
start_pos <- regexpr(start_marker, app, fixed = TRUE)[1]
end_pos <- regexpr(end_marker, app, fixed = TRUE)[1]

if (start_pos < 1 || end_pos < 1 || end_pos <= start_pos) {
  stop("Could not locate variableCard() in app.js.")
}

new_card <- paste0(
  'function variableCard(variable) {\n',
  '  return `\n',
  '    <details class="variable-card" id="${escapeHtml(variable.id)}">\n',
  '      <summary>\n',
  '        <span class="variable-id">${escapeHtml(variable.id)}</span>\n',
  '        <span class="variable-title">${escapeHtml(variable.displayName)}</span>\n',
  '        <span class="variable-badge">${escapeHtml(variable.analysisType)}</span>\n',
  '      </summary>\n',
  '      <div class="variable-details">\n',
  '        <h3>Question / description</h3>\n',
  '        <p>${escapeHtml(variable.sourceQuestionLabel || variable.displayName)}</p>\n',
  '        <dl class="detail-grid">\n',
  '          <div><dt>Topic</dt><dd>${escapeHtml(variable.group)}</dd></div>\n',
  '          <div><dt>Variable type</dt><dd>${escapeHtml(variable.analysisType)}</dd></div>\n',
  '        </dl>\n',
  '        <h3>Processed values</h3>\n',
  '        ${renderLevels(variable)}\n',
  '        <h3>Missing-data treatment</h3>\n',
  '        <p>${escapeHtml(variable.missingRule)}</p>\n',
  '      </div>\n',
  '    </details>`;\n',
  '}'
)

before <- substr(app, 1, start_pos - 1)
after <- substr(app, end_pos + 2, nchar(app))
app <- paste0(before, new_card, "\n\n", after)

writeLines(app, app_path, useBytes = TRUE)

# ---- 3. Future-proof the build helper -----------------------------------

build <- paste(readLines(build_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

old_levels <- paste0(
  '  levels <- template_record$levels %||% wv_level_records(\n',
  '    base_value,\n',
  '    recoded_value,\n',
  '    display_type,\n',
  '    analysis_type,\n',
  '    row[["R_recode"]],\n',
  '    var_id\n',
  '  )'
)

new_levels <- paste0(
  '  generated_levels <- wv_level_records(\n',
  '    base_value,\n',
  '    recoded_value,\n',
  '    display_type,\n',
  '    analysis_type,\n',
  '    row[["R_recode"]],\n',
  '    var_id\n',
  '  )\n',
  '  # Numeric processed values are the source of truth for category ordering.\n',
  '  levels <- if (length(generated_levels)) generated_levels else template_record$levels %||% list()'
)

if (!grepl(old_levels, build, fixed = TRUE)) {
  stop("Could not find expected level-generation block in R/worldview_build.R.")
}
build <- sub(old_levels, new_levels, build, fixed = TRUE)

display_anchor <- paste0(
  '  display_name <- template_record$displayName %||% wv_first_text(\n',
  '    row[["Variable_Text_ChatGPT"]],\n',
  '    row[["Variable Text"]],\n',
  '    wv_source_label(raw_value),\n',
  '    var_id\n',
  '  )'
)

display_replacement <- paste0(
  display_anchor,
  '\n  if (identical(var_id, "Q263")) display_name <- "Respondent immigrant status"'
)

if (!grepl(display_anchor, build, fixed = TRUE)) {
  stop("Could not find expected display-name block in R/worldview_build.R.")
}
build <- sub(display_anchor, display_replacement, build, fixed = TRUE)

writeLines(build, build_path, useBytes = TRUE)

message("WorldView codebook simplification applied successfully.")
message("Q165-Q168: 0 = No; 1 = Yes")
message("Q260: 0 = Female; 1 = Male")
message("Q263: 0 = Immigrant; 1 = Born in this country")
message("Q263 renamed to 'Respondent immigrant status'.")
message("Browser data was not changed.")
