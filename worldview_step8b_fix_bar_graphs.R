# WorldView Online - Step 8b
# Fix binary bar ordering/counts and add combined/separate country display.

app_dir <- file.path(getwd(), "worldview_static_app")
index_path <- file.path(app_dir, "index.html")
js_path <- file.path(app_dir, "assets", "visualisations.js")

if (!file.exists(index_path) || !file.exists(js_path)) {
  stop("Step 8 app files were not found. Run Step 8 first.")
}

html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

# 1. Add combined/separate display controls after the countries help text.
country_help <- '          <p id="visual-country-help" class="field-help">Select one or more countries. Use Ctrl or Command to select several. If none are selected, all countries are combined.</p>'

country_mode <- paste0(
  country_help,
  '\n\n          <fieldset class="radio-group">',
  '\n            <legend>Multiple-country display</legend>',
  '\n            <label><input type="radio" name="visual-country-mode" value="combined" checked> Combine selected countries on one plot</label>',
  '\n            <label><input type="radio" name="visual-country-mode" value="separate"> Show each country separately</label>',
  '\n          </fieldset>'
)

if (!grepl('name="visual-country-mode"', html, fixed = TRUE)) {
  if (!grepl(country_help, html, fixed = TRUE)) {
    stop("Could not find the visual country help block in index.html.")
  }
  html <- sub(country_help, country_mode, html, fixed = TRUE)
}

# 2. Add a helper for the new display control.
measure_function <- 'function visualMeasure() {
  return document.querySelector("input[name=visual-measure]:checked").value;
}'

mode_function <- paste0(
  measure_function,
  '\n\nfunction visualCountryMode() {',
  '\n  return document.querySelector("input[name=visual-country-mode]:checked").value;',
  '\n}'
)

if (!grepl("function visualCountryMode()", js, fixed = TRUE)) {
  if (!grepl(measure_function, js, fixed = TRUE)) {
    stop("Could not find visualMeasure() in visualisations.js.")
  }
  js <- sub(measure_function, mode_function, js, fixed = TRUE)
}

# 3. Fix category ordering for binary variables.
# Browser data uses 0/1, while the codebook displayOrder values begin at 1.
category_start <- 'function categoryOrder(variable, values) {
  if (variable.id === "Q262") {'

category_fixed <- 'function categoryOrder(variable, values) {
  if (["Q165", "Q166", "Q167", "Q168", "Q260", "Q263"].includes(variable.id)) {
    return ["0", "1"];
  }

  if (variable.id === "Q262") {'

if (!grepl('return ["0", "1"]', js, fixed = TRUE)) {
  if (!grepl(category_start, js, fixed = TRUE)) {
    stop("Could not find categoryOrder() in visualisations.js.")
  }
  js <- sub(category_start, category_fixed, js, fixed = TRUE)
}

# 4. Replace the country grouping logic in createGraph().
old_grouping <- '  const countries = visualSelectedCountries();
  const measure = visualMeasure();
  const countryMap = new Map(visualState.countries.map(country => [country.code, country.name]));

  const specifications = countries.length > 1
    ? countries.map(code => ({ title: countryMap.get(code) || code, codes: [code] }))
    : [{
        title: countries.length ? (countryMap.get(countries[0]) || countries[0]) : "All countries combined",
        codes: countries
      }];'

new_grouping <- '  const countries = visualSelectedCountries();
  const measure = visualMeasure();
  const countryMode = visualCountryMode();
  const countryMap = new Map(visualState.countries.map(country => [country.code, country.name]));

  let specifications;

  if (countryMode === "separate" && countries.length > 1) {
    specifications = countries.map(code => ({
      title: countryMap.get(code) || code,
      codes: [code]
    }));
  } else {
    specifications = [{
      title: countries.length
        ? countries.map(code => countryMap.get(code) || code).join(", ")
        : "All countries combined",
      codes: countries
    }];
  }'

if (grepl(old_grouping, js, fixed = TRUE)) {
  js <- sub(old_grouping, new_grouping, js, fixed = TRUE)
} else if (!grepl("const countryMode = visualCountryMode();", js, fixed = TRUE)) {
  stop("Could not find the Step 8 country grouping block in visualisations.js.")
}

writeLines(html, index_path, useBytes = TRUE)
writeLines(js, js_path, useBytes = TRUE)

updated_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
updated_js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

validation <- data.frame(
  check = c(
    "country_mode_control_added",
    "country_mode_function_added",
    "combined_mode_logic_added",
    "separate_mode_logic_retained",
    "binary_categories_use_browser_codes_0_1",
    "binary_labels_remain_no_yes"
  ),
  passed = c(
    grepl('name="visual-country-mode"', updated_html, fixed = TRUE),
    grepl("function visualCountryMode()", updated_js, fixed = TRUE),
    grepl('countryMode === "separate"', updated_js, fixed = TRUE),
    grepl("countries.map(code =>", updated_js, fixed = TRUE),
    grepl('return ["0", "1"]', updated_js, fixed = TRUE),
    grepl('Number(value) === 1 ? "Yes" : "No"', updated_js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  validation,
  file.path(app_dir, "step8b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 8b validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 8b completed successfully.\n\n")
cat("Changes:\n")
cat("  - Added combined versus separate country display.\n")
cat("  - Fixed binary counts to use browser codes 0 and 1.\n")
cat("  - Fixed binary ordering to display No before Yes.\n\n")
cat("Restart the local server and refresh the browser:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
