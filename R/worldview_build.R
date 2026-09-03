# WorldView data build helpers
#
# The static application in worldview_deployment/ is the student-facing source
# of truth. This file only rebuilds its data/configuration assets.

`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0L) return(right)
  if (length(left) == 1L && is.atomic(left) && is.na(left)) return(right)
  left
}

wv_clean <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# WVS .rds files store many numeric variables as haven_labelled/vctrs
# vectors. Recent haven/vctrs versions deliberately reject direct
# as.numeric()/as.integer() coercion on those classes. Strip only the
# vector class while retaining the underlying numeric values.
wv_as_numeric <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss") || inherits(x, "vctrs_vctr")) {
    return(suppressWarnings(as.numeric(unclass(x))))
  }
  suppressWarnings(as.numeric(x))
}

wv_normalise_logical <- function(x) {
  y <- toupper(wv_clean(x))
  y %in% c("TRUE", "T", "1", "YES", "Y")
}

wv_require_packages <- function() {
  required <- c("readxl", "jsonlite", "dplyr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Install the required R packages before building WorldView: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

wv_read_settings <- function(path) {
  if (!file.exists(path)) stop("WorldView config was not found: ", path, call. = FALSE)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("setting", "value") %in% names(x))) {
    stop("Config must contain 'setting' and 'value' columns: ", path, call. = FALSE)
  }
  values <- setNames(as.character(x$value), x$setting)
  required <- c("sampling_seed", "maximum_participants_per_country", "data_version")
  missing <- setdiff(required, names(values))
  if (length(missing)) stop("Config is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  seed <- suppressWarnings(as.integer(values[["sampling_seed"]]))
  max_n <- suppressWarnings(as.integer(values[["maximum_participants_per_country"]]))
  if (is.na(seed)) stop("sampling_seed must be an integer.", call. = FALSE)
  if (is.na(max_n) || max_n < 1L) stop("maximum_participants_per_country must be a positive integer.", call. = FALSE)

  list(
    sampling_seed = seed,
    maximum_participants_per_country = max_n,
    data_version = values[["data_version"]]
  )
}

wv_process_raw_variable <- function(x, display_type, var_id) {
  display_type <- wv_clean(display_type)[1]

  if (identical(var_id, "B_COUNTRY_ALPHA")) {
    return(factor(as.character(x)))
  }

  if (display_type %in% c("factor_ordered", "factor")) {
    x_numeric <- wv_as_numeric(x)
    x_numeric[x_numeric < 0] <- NA
    x_labels <- attr(x, "labels", exact = TRUE)

    if (!is.null(x_labels)) {
      match_idx <- match(x_numeric, unname(x_labels))
      x_out <- ifelse(is.na(match_idx), NA_character_, names(x_labels)[match_idx])
      x_levels <- names(x_labels)[unname(x_labels) >= 0]
      x_levels <- x_levels[!duplicated(x_levels)]
      return(factor(
        x_out,
        levels = x_levels,
        ordered = identical(display_type, "factor_ordered")
      ))
    }

    x_out <- if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss") || inherits(x, "vctrs_vctr")) {
      as.character(unclass(x))
    } else {
      as.character(x)
    }
    x_raw_numeric <- wv_as_numeric(x)
    x_out[!is.na(x_raw_numeric) & x_raw_numeric < 0] <- NA
    return(factor(x_out, ordered = identical(display_type, "factor_ordered")))
  }

  if (identical(display_type, "integer")) {
    x_int <- suppressWarnings(as.integer(wv_as_numeric(x)))
    x_int[x_int < 0] <- NA_integer_
    return(x_int)
  }

  stop(
    "Unsupported Variable_Display_Type '", display_type,
    "' for ", var_id, ".",
    call. = FALSE
  )
}

wv_apply_recode <- function(x, expression, var_id) {
  expression <- wv_clean(expression)[1]
  if (!nzchar(expression)) return(x)

  env <- new.env(parent = globalenv())
  env$.x <- x
  tryCatch(
    eval(parse(text = expression), envir = env),
    error = function(e) {
      stop(
        "R_recode failed for ", var_id, ": ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

wv_browser_value <- function(base_value, recoded_value, display_type, var_id) {
  display_type <- wv_clean(display_type)[1]

  if (identical(display_type, "factor_ordered")) {
    return(as.integer(recoded_value))
  }

  if (identical(display_type, "integer")) {
    return(suppressWarnings(as.integer(recoded_value)))
  }

  # Preserve the current static app convention for Q289: religious
  # denomination remains a readable nominal label. Other factor variables with
  # an explicit numeric R_recode retain that recode, preserving useful Shiny-era
  # processing for newly enabled variables.
  if (identical(var_id, "Q289")) return(as.character(base_value))
  if (identical(display_type, "factor") && (is.numeric(recoded_value) || is.integer(recoded_value))) {
    return(as.numeric(recoded_value))
  }

  as.character(base_value)
}

wv_source_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || !length(label) || is.na(label[[1]])) "" else as.character(label[[1]])
}

wv_first_text <- function(...) {
  values <- list(...)
  for (value in values) {
    value <- wv_clean(value)
    if (length(value) && nzchar(value[[1]])) return(value[[1]])
  }
  ""
}

wv_question_text <- function(row) {
  if ("Question_Text" %in% names(row)) {
    value <- wv_clean(row[["Question_Text"]])
    if (length(value) && nzchar(value[[1]])) return(value[[1]])
  }
  parts <- wv_clean(c(row[["Pretext"]], row[["Variable Text"]], row[["Additional Text"]]))
  paste(parts[nzchar(parts)], collapse = " ")
}

wv_topic <- function(section) {
  section <- wv_clean(section)[1]
  lookup <- c(
    "SOCIAL VALUES, ATTITUDES & STEREOTYPES" = "Social values, norms and stereotypes",
    "HAPPINESS AND WELL-BEING" = "Happiness and wellbeing",
    "SOCIAL CAPITAL, TRUST AND ORGANIZATIONAL MEMBERSHIP" = "Social capital, trust and organisational membership",
    "ECONOMIC VALUES" = "Economic values",
    "PERCEPTIONS OF CORRUPTION" = "Perceptions of corruption",
    "PERCEPTIONS OF MIGRATION" = "Perceptions of migration",
    "PERCEPTIONS OF SECURITY" = "Perceptions of security",
    "POSTMATERIALISM" = "Postmaterialism",
    "SCIENCE & TECHNOLOGY" = "Science and technology",
    "RELIGIOUS VALUES" = "Religious values",
    "ETHICAL VALUES AND NORMS" = "Ethical values",
    "POLITICAL INTEREST & POLITICAL PARTICIPATION" = "Political interest and political participation",
    "POLITICAL CULTURE & POLITICAL REGIMES" = "Political culture and political regimes",
    "DEMOGRAPHICS" = "Demographic and socioeconomic variables"
  )
  if (section %in% names(lookup)) unname(lookup[[section]]) else if (nzchar(section)) section else "Additional variables"
}

wv_display_type_label <- function(display_type) {
  switch(
    wv_clean(display_type)[1],
    factor_ordered = "ordered factor",
    factor = "factor",
    integer = "integer",
    wv_clean(display_type)[1]
  )
}

wv_infer_analysis_type <- function(display_type, browser_value) {
  display_type <- wv_clean(display_type)[1]
  if (identical(display_type, "factor_ordered")) return("ordinal")
  if (identical(display_type, "integer")) return("integer")
  if (identical(display_type, "factor")) {
    nonmissing <- browser_value[!is.na(browser_value) & browser_value != ""]
    if (is.numeric(browser_value) || is.integer(browser_value)) {
      if (length(unique(nonmissing)) <= 2L) return("binary")
      return("ordinal")
    }
    return("nominal")
  }
  "nominal"
}

wv_default_representation <- function(analysis_type) {
  switch(
    analysis_type,
    binary = "Binary values represented using the codebook-defined numeric recode",
    ordinal = "Ordered response categories represented by their WorldView order",
    integer = "Processed integer value",
    nominal = "Not available for numerical correlation",
    "Not specified"
  )
}

wv_default_interpretation <- function(analysis_type) {
  switch(
    analysis_type,
    binary = "The two response categories are represented numerically for analyses as documented in the codebook.",
    ordinal = "Response categories are ordered. WorldView uses their ordered numerical positions for numerical analyses.",
    integer = "The processed integer value is used directly in numerical analyses.",
    nominal = "This is an unordered multi-category variable. It is available for categorical summaries and analyses but not numerical correlation.",
    ""
  )
}

wv_level_records <- function(base_value, recoded_value, display_type, analysis_type, recode_expression, var_id) {
  if (!is.factor(base_value)) return(list())
  labels <- levels(base_value)
  if (!length(labels)) return(list())

  if (identical(analysis_type, "nominal")) {
    return(lapply(seq_along(labels), function(i) {
      list(value = labels[[i]], label = labels[[i]], displayOrder = i)
    }))
  }

  if (identical(wv_clean(display_type)[1], "factor_ordered")) {
    return(lapply(seq_along(labels), function(i) {
      list(value = i, label = labels[[i]], displayOrder = i)
    }))
  }

  lapply(seq_along(labels), function(i) {
    test <- factor(labels[[i]], levels = labels, ordered = is.ordered(base_value))
    code <- suppressWarnings(wv_apply_recode(test, recode_expression, var_id))
    code <- if (length(code) && !is.na(code[[1]])) as.numeric(code[[1]]) else i
    list(value = code, label = labels[[i]], displayOrder = code)
  })
}

wv_valid_range <- function(x) {
  if (!is.numeric(x) && !is.integer(x)) return(NULL)
  values <- x[is.finite(x)]
  if (!length(values)) return(NULL)
  list(minimum = min(values), maximum = max(values))
}

wv_template_map <- function(path) {
  if (!file.exists(path)) return(list(template = list(), records = list()))
  template <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  records <- setNames(template$variables %||% list(), vapply(template$variables %||% list(), function(x) x$id, character(1)))
  list(template = template, records = records)
}

wv_make_record <- function(var_id, row, raw_value, base_value, recoded_value, browser_value, template_record = NULL) {
  template_record <- template_record %||% list()
  display_type <- wv_clean(row[["Variable_Display_Type"]])[1]
  inferred_type <- wv_infer_analysis_type(display_type, browser_value)
  analysis_type <- template_record$analysisType %||% inferred_type
  topic <- template_record$topic %||% template_record$group %||% wv_topic(row[["Section"]])
  source_question <- template_record$sourceQuestionLabel %||% wv_question_text(row)
  display_name <- template_record$displayName %||% wv_first_text(
    row[["Variable_Text_ChatGPT"]],
    row[["Variable Text"]],
    wv_source_label(raw_value),
    var_id
  )
  levels <- template_record$levels %||% wv_level_records(
    base_value,
    recoded_value,
    display_type,
    analysis_type,
    row[["R_recode"]],
    var_id
  )

  list(
    id = var_id,
    displayName = display_name,
    group = template_record$group %||% topic,
    topic = topic,
    sourceQuestionLabel = source_question,
    displayType = template_record$displayType %||% wv_display_type_label(display_type),
    analysisType = analysis_type,
    ordered = template_record$ordered %||% identical(display_type, "factor_ordered"),
    correlationEligible = template_record$correlationEligible %||% !identical(analysis_type, "nominal"),
    correlationRepresentation = template_record$correlationRepresentation %||% wv_default_representation(analysis_type),
    missingRule = template_record$missingRule %||% "Negative WVS missing/non-response codes are converted to missing values before WorldView analyses.",
    interpretationNote = template_record$interpretationNote %||% wv_default_interpretation(analysis_type),
    validCountInFullProcessedData = sum(!is.na(browser_value) & browser_value != ""),
    missingCountInFullProcessedData = sum(is.na(browser_value) | browser_value == ""),
    levels = levels,
    validRange = template_record$validRange %||% wv_valid_range(browser_value)
  )
}

wv_sample_by_country <- function(data, maximum_per_country, seed) {
  data$.source_row <- seq_len(nrow(data))
  set.seed(seed)
  groups <- split(data, data$B_COUNTRY_ALPHA, drop = TRUE)
  sampled <- lapply(groups, function(country_data) {
    country_data <- country_data[order(country_data$.source_row), , drop = FALSE]
    retain_n <- min(nrow(country_data), maximum_per_country)
    selected <- sample.int(nrow(country_data), size = retain_n, replace = FALSE)
    country_data[selected, , drop = FALSE]
  })
  out <- do.call(rbind, sampled)
  row.names(out) <- NULL
  out <- out[order(out$B_COUNTRY_ALPHA, out$.source_row), , drop = FALSE]

  local_id <- ave(out$.source_row, out$B_COUNTRY_ALPHA, FUN = function(x) sprintf("%04d", seq_along(x)))
  out$WORLDVIEW_ID <- paste0("WV7-", out$B_COUNTRY_ALPHA, "-", local_id)
  out$.source_row <- NULL
  out[, c("WORLDVIEW_ID", setdiff(names(out), "WORLDVIEW_ID")), drop = FALSE]
}

wv_write_gz_json <- function(data, path) {
  con <- gzfile(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  jsonlite::write_json(data, con, dataframe = "columns", na = "null", auto_unbox = TRUE, digits = NA)
}

build_worldview <- function(
  raw_data_path = file.path("WVS_Dataset", "WVS_Cross-National_Wave_7_rds_v6_0.rds"),
  codebook_path = file.path("config", "WVS7_codebook_index.xlsx"),
  settings_path = file.path("config", "worldview_config.csv"),
  deployment_dir = "worldview_deployment"
) {
  wv_require_packages()
  settings <- wv_read_settings(settings_path)

  if (!file.exists(raw_data_path)) stop("Raw WVS data was not found: ", raw_data_path, call. = FALSE)
  if (!file.exists(codebook_path)) stop("Codebook configuration was not found: ", codebook_path, call. = FALSE)
  if (!dir.exists(deployment_dir)) stop("Static app directory was not found: ", deployment_dir, call. = FALSE)

  codebook <- readxl::read_xlsx(codebook_path, sheet = "Codebook index")
  required_columns <- c("Col_ID", "Variable_Display_Type", "Variable_Display_Logical", "R_recode")
  missing_columns <- setdiff(required_columns, names(codebook))
  if (length(missing_columns)) stop("Codebook is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)

  codebook$Variable_Display_Logical <- wv_normalise_logical(codebook$Variable_Display_Logical)
  ignored <- c("Q223", "Q266", "Q267", "Q268", "Q272", "Q290")
  selected_vars <- unique(codebook$Col_ID[codebook$Variable_Display_Logical])
  selected_vars <- setdiff(selected_vars, ignored)
  selected_vars <- selected_vars[nzchar(selected_vars)]
  if (!length(selected_vars)) stop("No student variables are enabled in Variable_Display_Logical.", call. = FALSE)

  raw <- readRDS(raw_data_path)
  required_ids <- c("B_COUNTRY", "B_COUNTRY_ALPHA")
  missing_raw <- setdiff(c(required_ids, selected_vars), names(raw))
  if (length(missing_raw)) stop("Raw WVS data is missing selected variables: ", paste(missing_raw, collapse = ", "), call. = FALSE)

  row_for <- function(id) {
    hit <- which(codebook$Col_ID == id)
    if (!length(hit)) stop("No codebook row found for ", id, call. = FALSE)
    codebook[hit[[1]], , drop = FALSE]
  }

  base_values <- list()
  recoded_values <- list()
  browser_values <- list()

  country_row <- row_for("B_COUNTRY")
  base_country <- wv_process_raw_variable(raw[["B_COUNTRY"]], country_row$Variable_Display_Type[[1]], "B_COUNTRY")
  base_alpha <- wv_process_raw_variable(raw[["B_COUNTRY_ALPHA"]], "factor", "B_COUNTRY_ALPHA")

  for (id in selected_vars) {
    row <- row_for(id)
    base <- wv_process_raw_variable(raw[[id]], row$Variable_Display_Type[[1]], id)
    recoded <- wv_apply_recode(base, row$R_recode[[1]], id)
    browser <- wv_browser_value(base, recoded, row$Variable_Display_Type[[1]], id)
    base_values[[id]] <- base
    recoded_values[[id]] <- recoded
    browser_values[[id]] <- browser
  }

  full_browser <- data.frame(
    B_COUNTRY = as.character(base_country),
    B_COUNTRY_ALPHA = as.character(base_alpha),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (id in selected_vars) full_browser[[id]] <- browser_values[[id]]

  valid_country <- !is.na(full_browser$B_COUNTRY_ALPHA) & nzchar(full_browser$B_COUNTRY_ALPHA)
  full_browser <- full_browser[valid_country, , drop = FALSE]
  for (id in selected_vars) {
    base_values[[id]] <- base_values[[id]][valid_country]
    recoded_values[[id]] <- recoded_values[[id]][valid_country]
    browser_values[[id]] <- browser_values[[id]][valid_country]
  }

  sampled <- wv_sample_by_country(
    full_browser,
    maximum_per_country = settings$maximum_participants_per_country,
    seed = settings$sampling_seed
  )

  data_dir <- file.path(deployment_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  browser_json <- file.path(data_dir, "worldview-browser-data-v1.0.0.json")
  browser_gz <- file.path(data_dir, "worldview-browser-data-v1.0.0.json.gz")
  codebook_json <- file.path(data_dir, "worldview-codebook-v1.0.0.json")
  manifest_json <- file.path(data_dir, "manifest-v1.0.0.json")

  jsonlite::write_json(sampled, browser_json, dataframe = "columns", na = "null", auto_unbox = TRUE, digits = NA)
  wv_write_gz_json(sampled, browser_gz)

  template <- wv_template_map(codebook_json)
  records <- lapply(selected_vars, function(id) {
    row <- row_for(id)
    wv_make_record(
      id,
      row,
      raw[[id]][valid_country],
      base_values[[id]],
      recoded_values[[id]],
      browser_values[[id]],
      template$records[[id]]
    )
  })

  preprocessing <- template$template$generalPreprocessing %||% list()
  preprocessing$source <- "World Values Survey Wave 7"
  preprocessing$scope <- paste0(
    "WorldView teaching subset containing ", length(selected_vars),
    " displayed variables and no more than ", settings$maximum_participants_per_country,
    " participants per country."
  )
  preprocessing$missingValues <- preprocessing$missingValues %||% "Negative WVS missing/non-response codes are converted to missing values before analysis."
  preprocessing$categoricalValues <- preprocessing$categoricalValues %||% "Student-facing categories use the processed labels and ordering defined by the WorldView codebook."

  built_codebook <- list(
    title = template$template$title %||% "WorldView WVS Wave 7 Codebook",
    version = template$template$version %||% "1.0.0",
    dataVersion = settings$data_version,
    variableCount = length(records),
    generalPreprocessing = preprocessing,
    variables = records
  )
  jsonlite::write_json(built_codebook, codebook_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)

  manifest <- list(
    dataVersion = settings$data_version,
    source = "World Values Survey Wave 7",
    samplingSeed = settings$sampling_seed,
    maximumParticipantsPerCountry = settings$maximum_participants_per_country,
    countryCount = length(unique(sampled$B_COUNTRY_ALPHA)),
    participantCount = nrow(sampled),
    studentVariableCount = length(selected_vars),
    exportColumnCount = ncol(sampled),
    variables = selected_vars
  )
  jsonlite::write_json(manifest, manifest_json, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  counts <- table(sampled$B_COUNTRY_ALPHA)
  checks <- data.frame(
    check = c(
      "manifest_variable_count_matches_codebook",
      "manifest_variables_match_codebook",
      "browser_rows_match_manifest",
      "no_country_exceeds_configured_maximum",
      "worldview_ids_are_unique"
    ),
    passed = c(
      manifest$studentVariableCount == built_codebook$variableCount,
      identical(manifest$variables, vapply(built_codebook$variables, function(x) x$id, character(1))),
      nrow(sampled) == manifest$participantCount,
      all(counts <= settings$maximum_participants_per_country),
      !anyDuplicated(sampled$WORLDVIEW_ID)
    ),
    stringsAsFactors = FALSE
  )
  write.csv(checks, file.path(deployment_dir, "deployment-checks.csv"), row.names = FALSE, na = "")
  if (!all(checks$passed)) stop("WorldView build validation failed: ", paste(checks$check[!checks$passed], collapse = ", "), call. = FALSE)

  message("WorldView build complete.")
  message("  Variables: ", length(selected_vars))
  message("  Participants: ", nrow(sampled))
  message("  Countries: ", manifest$countryCount)
  message("  Sampling seed: ", settings$sampling_seed)
  invisible(manifest)
}
