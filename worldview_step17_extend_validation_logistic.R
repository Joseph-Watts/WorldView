# WorldView Online - Step 17
# Extend validation.html with binary logistic regression checks against R glm().

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")
fixture_path <- file.path(app_dir, "data", "worldview-logistic-fixtures-v1.0.0.json")

if (!all(file.exists(c(validation_js_path, fixture_path)))) {
  stop("Required validation.js or logistic fixture file is missing.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

# Add fixture path to the existing paths object.
old_path <- '  regressionFixtures: "data/worldview-regression-fixtures-v1.0.0.json"'
new_path <- paste0(
  old_path,
  ',\n  logisticFixtures: "data/worldview-logistic-fixtures-v1.0.0.json"'
)

if (!grepl("logisticFixtures:", js, fixed = TRUE)) {
  if (!grepl(old_path, js, fixed = TRUE)) {
    stop("Could not locate regressionFixtures in validation.js.")
  }
  js <- sub(old_path, new_path, js, fixed = TRUE)
}

marker <- 'function displayValue(value) {'

logistic_validation <- '
function validationSigmoid(value) {
  if (value >= 0) {
    const exponential = Math.exp(-value);
    return 1 / (1 + exponential);
  }
  const exponential = Math.exp(value);
  return exponential / (1 + exponential);
}

function browserLogisticRegression(data, test) {
  const countryList = test.countries === "ALL"
    ? []
    : (Array.isArray(test.countries) ? test.countries : [test.countries]);
  const allowed = new Set(countryList);
  const x = [];
  const y = [];
  let filteredRows = 0;

  for (let i = 0; i < data.length; i += 1) {
    if (allowed.size && !allowed.has(data.columns.B_COUNTRY_ALPHA[i])) continue;
    filteredRows += 1;
    const values = [
      data.columns[test.outcome][i],
      ...test.predictors.map(id => data.columns[id][i])
    ];
    if (values.some(value => value === null || value === "")) continue;
    const numeric = values.map(Number);
    if (!numeric.every(Number.isFinite)) continue;
    y.push(numeric[0]);
    x.push([1, ...numeric.slice(1)]);
  }

  const n = y.length;
  const parameterCount = test.predictors.length + 1;
  let beta = Array(parameterCount).fill(0);
  let informationInverse = null;
  let iterations = 0;
  let converged = false;

  for (let iteration = 0; iteration < 50; iteration += 1) {
    iterations = iteration + 1;
    const probabilities = x.map(row =>
      validationSigmoid(row.reduce((sum, value, j) => sum + value * beta[j], 0))
    );
    const weights = probabilities.map(value => Math.max(value * (1 - value), 1e-9));
    const information = Array.from(
      { length: parameterCount },
      () => Array(parameterCount).fill(0)
    );
    const score = Array(parameterCount).fill(0);

    for (let i = 0; i < n; i += 1) {
      for (let a = 0; a < parameterCount; a += 1) {
        score[a] += x[i][a] * (y[i] - probabilities[i]);
        for (let b = 0; b < parameterCount; b += 1) {
          information[a][b] += x[i][a] * weights[i] * x[i][b];
        }
      }
    }

    informationInverse = matrixInverse(information);
    if (!informationInverse) throw new Error(`Singular logistic fixture: ${test.id}`);
    const step = matrixMultiply(
      informationInverse,
      score.map(value => [value])
    ).map(row => row[0]);
    beta = beta.map((value, j) => value + step[j]);

    if (Math.max(...step.map(Math.abs)) < 1e-8) {
      converged = true;
      break;
    }
  }

  const standardErrors = informationInverse.map((row, i) => Math.sqrt(row[i]));
  const probabilities = x.map(row =>
    validationSigmoid(row.reduce((sum, value, j) => sum + value * beta[j], 0))
  );
  const logLikelihood = y.reduce((sum, value, i) =>
    sum + value * Math.log(Math.max(probabilities[i], 1e-15)) +
    (1 - value) * Math.log(Math.max(1 - probabilities[i], 1e-15)), 0
  );
  const eventCount = y.reduce((sum, value) => sum + value, 0);
  const eventRate = eventCount / n;
  const nullLogLikelihood = y.reduce((sum, value) =>
    sum + value * Math.log(eventRate) + (1 - value) * Math.log(1 - eventRate), 0
  );

  return {
    filteredRows,
    completeObservations: n,
    excludedMissingAny: filteredRows - n,
    events: eventCount,
    nonEvents: n - eventCount,
    converged,
    iterations,
    logLikelihood,
    aic: -2 * logLikelihood + 2 * parameterCount,
    mcfaddenPseudoRSquared: 1 - logLikelihood / nullLogLikelihood,
    coefficients: beta.map((estimate, i) => ({
      term: i === 0 ? "(Intercept)" : test.predictors[i - 1],
      estimate,
      standardError: standardErrors[i],
      zValue: estimate / standardErrors[i],
      oddsRatio: Math.exp(estimate),
      oddsRatioLower: Math.exp(estimate - 1.959963984540054 * standardErrors[i]),
      oddsRatioUpper: Math.exp(estimate + 1.959963984540054 * standardErrors[i])
    }))
  };
}

function testLogisticFixtures(data, logisticFixtures) {
  logisticFixtures.cases.forEach(test => {
    const actual = browserLogisticRegression(data, test);
    const label = `Logistic ${test.id}`;

    ["filteredRows", "completeObservations", "excludedMissingAny", "events", "nonEvents"].forEach(field =>
      addCheck(`${label}: ${field}`, test[field], actual[field], test[field] === actual[field])
    );
    addCheck(`${label}: converged`, test.converged, actual.converged,
      test.converged === actual.converged);
    addCheck(`${label}: log likelihood`, test.logLikelihood, actual.logLikelihood,
      closeEnough(actual.logLikelihood, test.logLikelihood, 1e-6));
    addCheck(`${label}: AIC`, test.aic, actual.aic,
      closeEnough(actual.aic, test.aic, 1e-6));
    addCheck(`${label}: pseudo R squared`, test.mcfaddenPseudoRSquared,
      actual.mcfaddenPseudoRSquared,
      closeEnough(actual.mcfaddenPseudoRSquared, test.mcfaddenPseudoRSquared, 1e-8));

    test.coefficients.forEach((expected, index) => {
      const observed = actual.coefficients[index];
      addCheck(`${label}: ${expected.term} estimate`, expected.estimate,
        observed.estimate, closeEnough(observed.estimate, expected.estimate, 1e-7));
      addCheck(`${label}: ${expected.term} SE`, expected.standardError,
        observed.standardError, closeEnough(observed.standardError, expected.standardError, 1e-7));
      addCheck(`${label}: ${expected.term} z`, expected.zValue,
        observed.zValue, closeEnough(observed.zValue, expected.zValue, 1e-6));
      addCheck(`${label}: ${expected.term} odds ratio`, expected.oddsRatio,
        observed.oddsRatio, closeEnough(observed.oddsRatio, expected.oddsRatio, 1e-6));
      addCheck(`${label}: ${expected.term} OR CI lower`, expected.oddsRatioLower,
        observed.oddsRatioLower,
        closeEnough(observed.oddsRatioLower, expected.oddsRatioLower, 1e-6));
      addCheck(`${label}: ${expected.term} OR CI upper`, expected.oddsRatioUpper,
        observed.oddsRatioUpper,
        closeEnough(observed.oddsRatioUpper, expected.oddsRatioUpper, 1e-6));
    });
  });
}

'

if (!grepl("function testLogisticFixtures", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) stop("Could not find displayValue marker.")
  js <- sub(marker, paste0(logistic_validation, marker), js, fixed = TRUE)
}

old_load <- 'const [manifest, rawData, codebook, fixtures, regressionFixtures] = await Promise.all('
new_load <- 'const [manifest, rawData, codebook, fixtures, regressionFixtures, logisticFixtures] = await Promise.all('
if (grepl(old_load, js, fixed = TRUE)) {
  js <- sub(old_load, new_load, js, fixed = TRUE)
}

old_call <- '    testRegressionFixtures(data, regressionFixtures);\n    render();'
new_call <- '    testRegressionFixtures(data, regressionFixtures);\n    testLogisticFixtures(data, logisticFixtures);\n    render();'
if (!grepl("testLogisticFixtures(data, logisticFixtures);", js, fixed = TRUE)) {
  if (!grepl(old_call, js, fixed = TRUE)) stop("Could not find regression validation call.")
  js <- sub(old_call, new_call, js, fixed = TRUE)
}

writeLines(js, validation_js_path, useBytes = TRUE)

validation <- data.frame(
  check = c(
    "logistic_fixture_path_added",
    "browser_logistic_calculation_added",
    "logistic_cases_invoked",
    "coefficient_checks_added",
    "odds_ratio_checks_added",
    "model_fit_checks_added"
  ),
  passed = c(
    grepl("logisticFixtures:", js, fixed = TRUE),
    grepl("function browserLogisticRegression", js, fixed = TRUE),
    grepl("logisticFixtures.cases.forEach", js, fixed = TRUE),
    grepl("expected.term} estimate", js, fixed = TRUE),
    grepl("odds ratio", js, fixed = TRUE),
    grepl("pseudo R squared", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(validation, file.path(app_dir, "step17_validation_checks.csv"), row.names = FALSE)
if (!all(validation$passed)) {
  stop("Step 17 validation failed: ", paste(validation$check[!validation$passed], collapse = ", "))
}

cat("\nStep 17 completed successfully.\n\n")
cat("Restart the site and open validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("The validation total should increase beyond 188.\n")
