# WorldView Online - Step 15b
# Fix logistic page loading by isolating logistic.js from global names used by
# the linear regression and correlation scripts.

app_dir <- "worldview_static_app"
js_path <- file.path(app_dir, "assets", "logistic.js")

if (!file.exists(js_path)) {
  stop("logistic.js was not found. Run Step 15 first.")
}

js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

# The Step 15 script declared `const selected` in the global scope. The multiple
# regression script already uses the same global name, so the browser rejects
# logistic.js before initialiseLogistic can run. Rename the logistic helper.
js <- gsub(
  "const selected=id=>",
  "const logisticSelected=id=>",
  js,
  fixed = TRUE
)

js <- gsub(
  'preds=selected("logistic-predictors")',
  'preds=logisticSelected("logistic-predictors")',
  js,
  fixed = TRUE
)

js <- gsub(
  'countries=selected("logistic-countries")',
  'countries=logisticSelected("logistic-countries")',
  js,
  fixed = TRUE
)

# Rename generic matrix and probability helper functions as well. This avoids
# accidental interaction with functions declared by other model scripts.
replacements <- c(
  "function parse(o)" = "function logisticParse(o)",
  "function transpose(a)" = "function logisticTranspose(a)",
  "function multiply(a,b)" = "function logisticMultiply(a,b)",
  "function invert(a)" = "function logisticInvert(a)",
  "function sigmoid(z)" = "function logisticSigmoid(z)",
  "function fit(outcome,predictors,codes,title)" = "function logisticFit(outcome,predictors,codes,title)",
  "function card(r,out)" = "function logisticCard(r,out)",
  "function calculate(e)" = "function calculateLogistic(e)",
  "function download()" = "function downloadLogistic()",
  "function reset()" = "function resetLogistic()",
  "function limit()" = "function limitLogisticPredictors()",
  "function init()" = "function initialiseLogistic()",
  "transpose(b)" = "logisticTranspose(b)",
  "invert(h)" = "logisticInvert(h)",
  "multiply(inv,g.map" = "logisticMultiply(inv,g.map",
  "sigmoid(r.reduce" = "logisticSigmoid(r.reduce",
  "specs.map(s=>fit(" = "specs.map(s=>logisticFit(",
  "results.map(r=>card(" = "results.map(r=>logisticCard(",
  "ls.data=parse(d)" = "ls.data=logisticParse(d)",
  'addEventListener("submit",calculate)' = 'addEventListener("submit",calculateLogistic)',
  'addEventListener("click",reset)' = 'addEventListener("click",resetLogistic)',
  'addEventListener("click",download)' = 'addEventListener("click",downloadLogistic)',
  'addEventListener("change",limit)' = 'addEventListener("change",limitLogisticPredictors)',
  "init();" = "initialiseLogistic();"
)

for (old in names(replacements)) {
  js <- gsub(old, replacements[[old]], js, fixed = TRUE)
}

# Check that the conflicting global declaration is gone and the renamed
# initialisation path is present.
validation <- data.frame(
  check = c(
    "global_selected_constant_removed",
    "logistic_selected_helper_added",
    "predictor_selection_updated",
    "country_selection_updated",
    "logistic_initialiser_added",
    "logistic_submit_handler_updated"
  ),
  passed = c(
    !grepl("const selected=id=>", js, fixed = TRUE),
    grepl("const logisticSelected=id=>", js, fixed = TRUE),
    grepl('preds=logisticSelected("logistic-predictors")', js, fixed = TRUE),
    grepl('countries=logisticSelected("logistic-countries")', js, fixed = TRUE),
    grepl("function initialiseLogistic()", js, fixed = TRUE),
    grepl('addEventListener("submit",calculateLogistic)', js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

writeLines(js, js_path, useBytes = TRUE)
write.csv(
  validation,
  file.path(app_dir, "step15b_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(validation$passed)) {
  stop(
    "Step 15b validation failed: ",
    paste(validation$check[!validation$passed], collapse = ", ")
  )
}

cat("\nStep 15b completed successfully.\n\n")
cat("Restart the server and force-refresh the browser:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n\n")
cat("On Windows browsers, use Ctrl+F5 to bypass the cached logistic.js file.\n")
