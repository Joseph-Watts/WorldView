# WorldView Online - Step 26c
# Increase map contrast against the water background and select all countries by default.

app_dir <- "worldview_static_app"
map_js_path <- file.path(app_dir, "assets", "world-map.js")
css_path <- file.path(app_dir, "assets", "styles.css")

if (!all(file.exists(c(map_js_path, css_path)))) {
  stop("Run Steps 26 and 26b first. Required world-map files were not found.")
}

js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

# Use a warm yellow-to-deep-purple scale that is visually distinct from blue water.
old_colour <- 'function wmColour(value,min,max){if(value==null||!Number.isFinite(value))return"#e5e7e7";const t=max===min?.5:Math.max(0,Math.min(1,(value-min)/(max-min))),a=[238,242,239],b=[0,91,83];return`rgb(${Math.round(a[0]+t*(b[0]-a[0]))},${Math.round(a[1]+t*(b[1]-a[1]))},${Math.round(a[2]+t*(b[2]-a[2]))})`;}'
new_colour <- 'function wmColour(value,min,max){if(value==null||!Number.isFinite(value))return"#d1d5d8";const t=max===min?.5:Math.max(0,Math.min(1,(value-min)/(max-min))),a=[255,224,102],b=[74,20,140];return`rgb(${Math.round(a[0]+t*(b[0]-a[0]))},${Math.round(a[1]+t*(b[1]-a[1]))},${Math.round(a[2]+t*(b[2]-a[2]))})`;}'

if (grepl(old_colour, js, fixed = TRUE)) {
  js <- sub(old_colour, new_colour, js, fixed = TRUE)
} else if (!grepl("a=[255,224,102],b=[74,20,140]", js, fixed = TRUE)) {
  stop("Could not locate wmColour() in world-map.js.")
}

# Select every available WorldView country when options are created.
old_country_option <- 'wmState.countries.forEach(c=>countries.add(new Option(`${c.name} (${c.code})`,c.code)));'
new_country_option <- 'wmState.countries.forEach(c=>{const option=new Option(`${c.name} (${c.code})`,c.code);option.selected=true;countries.add(option);});'

if (grepl(old_country_option, js, fixed = TRUE)) {
  js <- sub(old_country_option, new_country_option, js, fixed = TRUE)
} else if (!grepl("option.selected=true;countries.add(option)", js, fixed = TRUE)) {
  stop("Could not locate country-option creation in world-map.js.")
}

# Reset restores the new default of all countries selected.
old_reset_selection <- '[...document.getElementById("map-countries").options].forEach(o=>o.selected=false);'
new_reset_selection <- '[...document.getElementById("map-countries").options].forEach(o=>o.selected=true);'
js <- gsub(old_reset_selection, new_reset_selection, js, fixed = TRUE)

writeLines(js, map_js_path, useBytes = TRUE)

# Update the displayed key and water background.
css <- gsub(
  "background: #dcebf0;",
  "background: #b9dbea;",
  css,
  fixed = TRUE
)
css <- gsub(
  "background: linear-gradient(90deg, rgb(238,242,239), rgb(0,91,83));",
  "background: linear-gradient(90deg, rgb(255,224,102), rgb(74,20,140));",
  css,
  fixed = TRUE
)

if (!grepl("Step 26c: stronger map contrast", css, fixed = TRUE)) {
  cat(
    '\n/* Step 26c: stronger map contrast */\n.world-map-container { background: #b9dbea; }\n.map-gradient { background: linear-gradient(90deg, rgb(255,224,102), rgb(74,20,140)); border: 1px solid rgba(32, 40, 48, .28); }\n',
    file = css_path,
    append = TRUE
  )
}

updated_js <- paste(readLines(map_js_path, warn = FALSE), collapse = "\n")
updated_css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

checks <- data.frame(
  check = c(
    "warm_high_contrast_colour_scale_added",
    "no_data_colour_retained",
    "all_countries_selected_on_load",
    "reset_selects_all_countries",
    "legend_matches_map_scale",
    "water_background_contrasts_with_scale"
  ),
  passed = c(
    grepl("a=[255,224,102],b=[74,20,140]", updated_js, fixed = TRUE),
    grepl('return"#d1d5d8"', updated_js, fixed = TRUE),
    grepl("option.selected=true;countries.add(option)", updated_js, fixed = TRUE),
    grepl('options].forEach(o=>o.selected=true)', updated_js, fixed = TRUE),
    grepl("rgb(255,224,102), rgb(74,20,140)", updated_css, fixed = TRUE),
    grepl("background: #b9dbea", updated_css, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step26c_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop(
    "Step 26c validation failed: ",
    paste(checks$check[!checks$passed], collapse = ", ")
  )
}

cat("\nStep 26c completed successfully.\n\n")
cat("Changes:\n")
cat("  - Map scale changed to yellow through deep purple.\n")
cat("  - Water background changed to a clearer light blue.\n")
cat("  - All WorldView countries are selected by default.\n")
cat("  - Reset restores all countries as selected.\n\n")
cat("Restart with:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
