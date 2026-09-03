# WorldView Online - Step 1
# Inspect the source WVS Wave 7 RDS before building the export pipeline.
# This script does not modify the source data and does not export participant rows.

student_variables <- c(
  "Q165", "Q166", "Q167", "Q168",
  "Q177", "Q178", "Q179", "Q180", "Q181", "Q182", "Q183",
  "Q184", "Q185", "Q186", "Q187", "Q188", "Q189", "Q190",
  "Q191", "Q192", "Q193", "Q194", "Q195",
  "Q260", "Q262", "Q263", "Q275", "Q288", "Q289"
)

country_variables <- c("B_COUNTRY", "B_COUNTRY_ALPHA")
required_variables <- c(country_variables, student_variables)

cat("WorldView Step 1: inspect the WVS source RDS\n")
cat("Choose the original WVS Wave 7 .rds file in the file window.\n\n")

source_path <- file.choose()
cat("Reading:", source_path, "\n")
source_data <- readRDS(source_path)

if (!is.data.frame(source_data)) {
  stop("The selected RDS does not contain a data.frame-like object.")
}

output_dir <- file.path(getwd(), "worldview_step1_output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

present <- required_variables %in% names(source_data)
variable_presence <- data.frame(
  variable = required_variables,
  required_role = c(rep("country identifier", length(country_variables)),
                    rep("student variable", length(student_variables))),
  present = present,
  stringsAsFactors = FALSE
)
write.csv(variable_presence,
          file.path(output_dir, "variable_presence.csv"),
          row.names = FALSE,
          na = "")

missing_variables <- required_variables[!present]
if (length(missing_variables) > 0) {
  writeLines(missing_variables,
             file.path(output_dir, "missing_variables.txt"))
  stop(
    paste0(
      "Required variables are missing: ",
      paste(missing_variables, collapse = ", "),
      ". See worldview_step1_output/variable_presence.csv."
    )
  )
}

safe_label <- function(x) {
  value <- attr(x, "label", exact = TRUE)
  if (is.null(value) || length(value) == 0) return(NA_character_)
  as.character(value[[1]])
}

safe_value_labels <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || length(labels) == 0) return(NA_character_)
  paste(paste0(names(labels), "=", as.character(unname(labels))), collapse = " | ")
}

count_negative_numeric <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  if (length(values) == 0 || all(is.na(values))) return(NA_integer_)
  as.integer(sum(values < 0, na.rm = TRUE))
}

variable_profile <- do.call(
  rbind,
  lapply(required_variables, function(id) {
    x <- source_data[[id]]
    values_numeric <- suppressWarnings(as.numeric(x))
    nonmissing_numeric <- values_numeric[!is.na(values_numeric)]

    data.frame(
      variable = id,
      r_class = paste(class(x), collapse = " "),
      source_label = safe_label(x),
      value_labels = safe_value_labels(x),
      rows = length(x),
      source_NA = sum(is.na(x)),
      negative_numeric_codes = count_negative_numeric(x),
      numeric_min = if (length(nonmissing_numeric)) min(nonmissing_numeric) else NA_real_,
      numeric_max = if (length(nonmissing_numeric)) max(nonmissing_numeric) else NA_real_,
      distinct_nonmissing_numeric = length(unique(nonmissing_numeric)),
      stringsAsFactors = FALSE
    )
  })
)

write.csv(variable_profile,
          file.path(output_dir, "variable_profile.csv"),
          row.names = FALSE,
          na = "")

country_alpha <- as.character(source_data[["B_COUNTRY_ALPHA"]])
country_name <- as.character(source_data[["B_COUNTRY"]])

country_counts <- aggregate(
  rep(1L, nrow(source_data)),
  by = list(
    B_COUNTRY_ALPHA = country_alpha,
    B_COUNTRY = country_name
  ),
  FUN = sum
)
names(country_counts)[3] <- "source_participants"
country_counts <- country_counts[order(country_counts$B_COUNTRY_ALPHA,
                                       country_counts$B_COUNTRY), ]
row.names(country_counts) <- NULL

write.csv(country_counts,
          file.path(output_dir, "country_counts.csv"),
          row.names = FALSE,
          na = "")

summary_lines <- c(
  "WorldView Step 1 source inspection",
  paste("Source file:", normalizePath(source_path, winslash = "/", mustWork = FALSE)),
  paste("Rows:", nrow(source_data)),
  paste("Columns:", ncol(source_data)),
  paste("Required variables found:", sum(present), "of", length(required_variables)),
  paste("Countries represented by non-missing alpha code:",
        length(unique(country_alpha[!is.na(country_alpha)]))),
  paste("Countries with more than 1,000 source participants:",
        sum(country_counts$source_participants > 1000, na.rm = TRUE)),
  "",
  "No participant-level rows were written by this inspection script."
)
writeLines(summary_lines, file.path(output_dir, "inspection_summary.txt"))

cat("\nInspection completed successfully.\n")
cat("Output folder:\n", normalizePath(output_dir, winslash = "/"), "\n\n", sep = "")
cat(paste(summary_lines, collapse = "\n"), "\n")
cat("\nPlease share these four files:\n")
cat("  inspection_summary.txt\n")
cat("  variable_presence.csv\n")
cat("  variable_profile.csv\n")
cat("  country_counts.csv\n")
