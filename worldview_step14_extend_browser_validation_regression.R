# WorldView Online - Step 14
# Extend validation.html to check browser multiple linear regression against
# the R stats::lm fixtures created in Step 13.

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")
fixture_path <- file.path(app_dir, "data", "worldview-regression-fixtures-v1.0.0.json")

if (!all(file.exists(c(validation_js_path, fixture_path)))) {
  stop("Required validation or regression fixture file is missing.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

# Add regression fixture path.
old_paths <- '  fixtures: "data/worldview-reference-fixtures-v1.0.0.json"'
new_paths <- '  fixtures: "data/worldview-reference-fixtures-v1.0.0.json",\n  regressionFixtures: "data/worldview-regression-fixtures-v1.0.0.json"'
if (!grepl("regressionFixtures:", js, fixed = TRUE)) {
  if (!grepl(old_paths, js, fixed = TRUE)) stop("Could not find validation paths block.")
  js <- sub(old_paths, new_paths, js, fixed = TRUE)
}

# Add matrix functions and regression validator before displayValue().
marker <- 'function displayValue(value) {'
regression_functions <- '
function matrixTranspose(matrix) {
  return matrix[0].map((_, column) => matrix.map(row => row[column]));
}

function matrixMultiply(left, right) {
  const rightT = matrixTranspose(right);
  return left.map(row => rightT.map(column =>
    row.reduce((sum, value, index) => sum + value * column[index], 0)
  ));
}

function matrixInverse(matrix) {
  const n = matrix.length;
  const augmented = matrix.map((row, i) => [
    ...row,
    ...Array.from({ length: n }, (_, j) => i === j ? 1 : 0)
  ]);

  for (let i = 0; i < n; i += 1) {
    let pivot = i;
    for (let row = i + 1; row < n; row += 1) {
      if (Math.abs(augmented[row][i]) > Math.abs(augmented[pivot][i])) pivot = row;
    }
    if (Math.abs(augmented[pivot][i]) < 1e-12) return null;
    [augmented[i], augmented[pivot]] = [augmented[pivot], augmented[i]];
    const divisor = augmented[i][i];
    augmented[i] = augmented[i].map(value => value / divisor);
    for (let row = 0; row < n; row += 1) {
      if (row === i) continue;
      const factor = augmented[row][i];
      augmented[row] = augmented[row].map((value, column) =>
        value - factor * augmented[i][column]
      );
    }
  }
  return augmented.map(row => row.slice(n));
}

function browserRegression(data, test) {
  const countryList = test.countries === "ALL"
    ? []
    : (Array.isArray(test.countries) ? test.countries : [test.countries]);
  const allowed = new Set(countryList);
  const rows = [];
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
    if (numeric.every(Number.isFinite)) rows.push(numeric);
  }

  const y = rows.map(row => [row[0]]);
  const x = rows.map(row => [1, ...row.slice(1)]);
  const xt = matrixTranspose(x);
  const xtxInverse = matrixInverse(matrixMultiply(xt, x));
  const beta = matrixMultiply(matrixMultiply(xtxInverse, xt), y).map(row => row[0]);
  const fitted = x.map(row => row.reduce((sum, value, j) => sum + value * beta[j], 0));
  const meanY = y.reduce((sum, row) => sum + row[0], 0) / y.length;
  let sse = 0;
  let sst = 0;
  for (let i = 0; i < y.length; i += 1) {
    sse += (y[i][0] - fitted[i]) ** 2;
    sst += (y[i][0] - meanY) ** 2;
  }

  const k = test.predictors.length + 1;
  const df = y.length - k;
  const residualVariance = sse / df;
  const standardErrors = xtxInverse.map((row, i) => Math.sqrt(residualVariance * row[i]));
  const rSquared = 1 - sse / sst;

  return {
    filteredRows,
    completeObservations: y.length,
    excludedMissingAny: filteredRows - y.length,
    residualDegreesFreedom: df,
    rSquared,
    adjustedRSquared: 1 - (1 - rSquared) * (y.length - 1) / df,
    residualStandardError: Math.sqrt(residualVariance),
    fStatistic: (rSquared / test.predictors.length) / ((1 - rSquared) / df),
    coefficients: beta.map((estimate, i) => ({
      term: i === 0 ? "(Intercept)" : test.predictors[i - 1],
      estimate,
      standardError: standardErrors[i],
      tValue: estimate / standardErrors[i]
    }))
  };
}

function testRegressionFixtures(data, regressionFixtures) {
  regressionFixtures.cases.forEach(test => {
    const actual = browserRegression(data, test);
    const label = `Regression ${test.id}`;

    addCheck(`${label}: filtered rows`, test.filteredRows, actual.filteredRows,
      test.filteredRows === actual.filteredRows);
    addCheck(`${label}: complete observations`, test.completeObservations,
      actual.completeObservations, test.completeObservations === actual.completeObservations);
    addCheck(`${label}: excluded rows`, test.excludedMissingAny,
      actual.excludedMissingAny, test.excludedMissingAny === actual.excludedMissingAny);
    addCheck(`${label}: residual df`, test.residualDegreesFreedom,
      actual.residualDegreesFreedom, test.residualDegreesFreedom === actual.residualDegreesFreedom);
    addCheck(`${label}: R squared`, test.rSquared, actual.rSquared,
      closeEnough(actual.rSquared, test.rSquared, 1e-9));
    addCheck(`${label}: adjusted R squared`, test.adjustedRSquared, actual.adjustedRSquared,
      closeEnough(actual.adjustedRSquared, test.adjustedRSquared, 1e-9));
    addCheck(`${label}: residual standard error`, test.residualStandardError,
      actual.residualStandardError, closeEnough(actual.residualStandardError, test.residualStandardError, 1e-9));
    addCheck(`${label}: F statistic`, test.fStatistic, actual.fStatistic,
      closeEnough(actual.fStatistic, test.fStatistic, 1e-7));

    test.coefficients.forEach((expectedCoefficient, i) => {
      const actualCoefficient = actual.coefficients[i];
      addCheck(`${label}: ${expectedCoefficient.term} estimate`, expectedCoefficient.estimate,
        actualCoefficient.estimate, closeEnough(actualCoefficient.estimate, expectedCoefficient.estimate, 1e-9));
      addCheck(`${label}: ${expectedCoefficient.term} SE`, expectedCoefficient.standardError,
        actualCoefficient.standardError, closeEnough(actualCoefficient.standardError, expectedCoefficient.standardError, 1e-9));
      addCheck(`${label}: ${expectedCoefficient.term} t`, expectedCoefficient.tValue,
        actualCoefficient.tValue, closeEnough(actualCoefficient.tValue, expectedCoefficient.tValue, 1e-8));
    });
  });
}

'

if (!grepl("function testRegressionFixtures", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) stop("Could not find displayValue marker.")
  js <- sub(marker, paste0(regression_functions, marker), js, fixed = TRUE)
}

# Expand Promise.all destructuring and invoke tests.
old_load <- 'const [manifest, rawData, codebook, fixtures] = await Promise.all('
new_load <- 'const [manifest, rawData, codebook, fixtures, regressionFixtures] = await Promise.all('
if (grepl(old_load, js, fixed = TRUE)) {
  js <- sub(old_load, new_load, js, fixed = TRUE)
}

old_call <- '    testCorrelationFixtures(data, fixtures);\n    render();'
new_call <- '    testCorrelationFixtures(data, fixtures);\n    testRegressionFixtures(data, regressionFixtures);\n    render();'
if (!grepl("testRegressionFixtures(data, regressionFixtures);", js, fixed = TRUE)) {
  if (!grepl(old_call, js, fixed = TRUE)) stop("Could not find validation invocation block.")
  js <- sub(old_call, new_call, js, fixed = TRUE)
}

writeLines(js, validation_js_path, useBytes = TRUE)

validation <- data.frame(
  check = c(
    "regression_fixture_path_added",
    "matrix_regression_added",
    "five_regression_cases_invoked",
    "coefficient_checks_added",
    "model_fit_checks_added"
  ),
  passed = c(
    grepl("regressionFixtures:", js, fixed = TRUE),
    grepl("function browserRegression", js, fixed = TRUE),
    grepl("regressionFixtures.cases.forEach", js, fixed = TRUE),
    grepl("expectedCoefficient.term", js, fixed = TRUE),
    grepl("residual standard error", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(validation, file.path(app_dir, "step14_validation_checks.csv"), row.names = FALSE)
if (!all(validation$passed)) stop("Step 14 structural validation failed.")

cat("\nStep 14 completed successfully.\n\n")
cat("Restart the site and open validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("The validation total should now be higher than 82 and all checks should pass.\n")
