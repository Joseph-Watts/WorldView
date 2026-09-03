# WorldView Online - Step 17c
# Recompute the logistic information matrix at the final fitted coefficients.
# This corrects small SE, z, odds-ratio CI, AIC, and validation differences.

app_dir <- "worldview_static_app"
production_path <- file.path(app_dir, "assets", "logistic.js")
validation_path <- file.path(app_dir, "assets", "validation.js")

if (!all(file.exists(c(production_path, validation_path)))) {
  stop("Required logistic.js or validation.js file is missing.")
}

production <- paste(readLines(production_path, warn = FALSE), collapse = "\n")
validation <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")

# In production logistic.js, Step 15b renamed functions with logistic prefixes.
old_production <- 'if(!converged)return{...base,status:"not_converged"};const se=inv.map((r,i)=>Math.sqrt(r[i])),terms=["Intercept",...predictors]'
new_production <- 'if(!converged)return{...base,status:"not_converged"};const finalP=X.map(r=>logisticSigmoid(r.reduce((s,v,j)=>s+v*beta[j],0))),finalW=finalP.map(v=>Math.max(v*(1-v),1e-12)),finalH=Array.from({length:k},()=>Array(k).fill(0));for(let i=0;i<n;i++){for(let a=0;a<k;a++){for(let b=0;b<k;b++)finalH[a][b]+=X[i][a]*finalW[i]*X[i][b];}}inv=logisticInvert(finalH);if(!inv)return{...base,status:"singular"};const se=inv.map((r,i)=>Math.sqrt(r[i])),terms=["Intercept",...predictors]'

if (grepl(old_production, production, fixed = TRUE)) {
  production <- sub(old_production, new_production, production, fixed = TRUE)
} else if (!grepl("const finalP=X.map", production, fixed = TRUE)) {
  stop("Could not locate the final-standard-error block in logistic.js.")
}

# In validation.js, recompute the information matrix after convergence and
# before extracting standard errors.
old_validation <- '  const standardErrors = informationInverse.map((row, i) => Math.sqrt(row[i]));\n  const probabilities = x.map(row =>\n    validationSigmoid(row.reduce((sum, value, j) => sum + value * beta[j], 0))\n  );'

new_validation <- '  const probabilities = x.map(row =>\n    validationSigmoid(row.reduce((sum, value, j) => sum + value * beta[j], 0))\n  );\n  const finalWeights = probabilities.map(value => Math.max(value * (1 - value), 1e-12));\n  const finalInformation = Array.from(\n    { length: parameterCount },\n    () => Array(parameterCount).fill(0)\n  );\n  for (let i = 0; i < n; i += 1) {\n    for (let a = 0; a < parameterCount; a += 1) {\n      for (let b = 0; b < parameterCount; b += 1) {\n        finalInformation[a][b] += x[i][a] * finalWeights[i] * x[i][b];\n      }\n    }\n  }\n  informationInverse = matrixInverse(finalInformation);\n  if (!informationInverse) throw new Error(`Singular final logistic information matrix: ${test.id}`);\n  const standardErrors = informationInverse.map((row, i) => Math.sqrt(row[i]));'

if (grepl(old_validation, validation, fixed = TRUE)) {
  validation <- sub(old_validation, new_validation, validation, fixed = TRUE)
} else if (!grepl("const finalWeights = probabilities.map", validation, fixed = TRUE)) {
  stop("Could not locate the final-standard-error block in validation.js.")
}

writeLines(production, production_path, useBytes = TRUE)
writeLines(validation, validation_path, useBytes = TRUE)

checks <- data.frame(
  check = c(
    "production_final_probabilities_added",
    "production_final_hessian_added",
    "production_final_inverse_added",
    "validation_final_weights_added",
    "validation_final_information_added",
    "validation_final_inverse_added"
  ),
  passed = c(
    grepl("const finalP=X.map", production, fixed = TRUE),
    grepl("finalH=Array.from", production, fixed = TRUE),
    grepl("inv=logisticInvert(finalH)", production, fixed = TRUE),
    grepl("const finalWeights = probabilities.map", validation, fixed = TRUE),
    grepl("const finalInformation = Array.from", validation, fixed = TRUE),
    grepl("informationInverse = matrixInverse(finalInformation)", validation, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step17c_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop("Step 17c structural validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 17c completed successfully.\n\n")
cat("The production model and validation model now recompute standard errors\n")
cat("from the information matrix at the final fitted coefficients.\n\n")
cat("Restart the server and force-refresh validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("Then use Ctrl+F5.\n")
