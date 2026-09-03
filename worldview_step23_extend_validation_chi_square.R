# WorldView Online - Step 23
# Extend validation.html with chi-square checks against R chisq.test().

app_dir <- "worldview_static_app"
js_path <- file.path(app_dir, "assets", "validation.js")
fixture_path <- file.path(app_dir, "data", "worldview-chi-square-fixtures-v1.0.0.json")
if (!all(file.exists(c(js_path, fixture_path)))) stop("Required validation.js or chi-square fixture is missing.")

js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

anchor <- '  anovaFixtures: "data/worldview-anova-fixtures-v1.0.0.json"'
replacement <- paste0(anchor, ',\n  chiSquareFixtures: "data/worldview-chi-square-fixtures-v1.0.0.json"')
if (!grepl("chiSquareFixtures:", js, fixed = TRUE)) {
  if (!grepl(anchor, js, fixed = TRUE)) stop("Could not find anovaFixtures path.")
  js <- sub(anchor, replacement, js, fixed = TRUE)
}

marker <- 'function displayValue(value) {'
functions <- '
function validationChiLabel(variable, value, countryName) {
  if (variable === "COUNTRY") return countryName;
  if (variable === "Q289") return String(value);
  if (["Q165", "Q166", "Q167", "Q168"].includes(variable)) return Number(value) === 1 ? "Yes" : "No";
  if (variable === "Q260") return Number(value) === 1 ? "Male" : "Female";
  if (variable === "Q263") return Number(value) === 1 ? "Born in this country" : "Immigrant to this country";
  return String(value);
}

function browserChiSquare(data, test) {
  const countries = test.countries === "ALL" ? [] : (Array.isArray(test.countries) ? test.countries : [test.countries]);
  const allowed = new Set(countries), rows = [], columns = [], counts = new Map();
  let completeObservations = 0;
  for (let i = 0; i < data.length; i += 1) {
    const countryCode = data.columns.B_COUNTRY_ALPHA[i];
    if (allowed.size && !allowed.has(countryCode)) continue;
    const rawRow = test.rowVariable === "COUNTRY" ? countryCode : data.columns[test.rowVariable][i];
    const rawColumn = test.columnVariable === "COUNTRY" ? countryCode : data.columns[test.columnVariable][i];
    if (rawRow === null || rawColumn === null || rawRow === "" || rawColumn === "") continue;
    const row = validationChiLabel(test.rowVariable, rawRow, data.columns.B_COUNTRY[i]);
    const column = validationChiLabel(test.columnVariable, rawColumn, data.columns.B_COUNTRY[i]);
    if (!rows.includes(row)) rows.push(row);
    if (!columns.includes(column)) columns.push(column);
    const key = `${row}|||${column}`;
    counts.set(key, (counts.get(key) || 0) + 1);
    completeObservations += 1;
  }
  rows.sort(); columns.sort();
  const rowTotals = rows.map(row => columns.reduce((sum, column) => sum + (counts.get(`${row}|||${column}`) || 0), 0));
  const columnTotals = columns.map(column => rows.reduce((sum, row) => sum + (counts.get(`${row}|||${column}`) || 0), 0));
  let chiSquare = 0, expectedBelowFive = 0;
  const cells = [];
  rows.forEach((row, i) => columns.forEach((column, j) => {
    const observed = counts.get(`${row}|||${column}`) || 0;
    const expected = rowTotals[i] * columnTotals[j] / completeObservations;
    if (expected < 5) expectedBelowFive += 1;
    chiSquare += (observed - expected) ** 2 / expected;
    cells.push({
      row, column, observed, expected,
      rowPercentage: 100 * observed / rowTotals[i],
      columnPercentage: 100 * observed / columnTotals[j]
    });
  }));
  const degreesFreedom = (rows.length - 1) * (columns.length - 1);
  const minimumDimension = Math.min(rows.length - 1, columns.length - 1);
  return {
    completeObservations,
    rowCount: rows.length,
    columnCount: columns.length,
    degreesFreedom,
    chiSquare,
    cramersV: Math.sqrt(chiSquare / (completeObservations * minimumDimension)),
    expectedBelowFive,
    totalCells: cells.length,
    cells
  };
}

function testChiSquareFixtures(data, chiSquareFixtures) {
  chiSquareFixtures.cases.forEach(test => {
    const actual = browserChiSquare(data, test);
    const label = `Chi-square ${test.id}`;
    ["completeObservations", "rowCount", "columnCount", "degreesFreedom", "expectedBelowFive", "totalCells"].forEach(field =>
      addCheck(`${label}: ${field}`, test[field], actual[field], test[field] === actual[field])
    );
    addCheck(`${label}: statistic`, test.chiSquare, actual.chiSquare, anovaCloseEnough(actual.chiSquare, test.chiSquare, 1e-8, 1e-10));
    addCheck(`${label}: Cramers V`, test.cramersV, actual.cramersV, anovaCloseEnough(actual.cramersV, test.cramersV, 1e-10, 1e-10));
    test.cells.forEach(expected => {
      const observed = actual.cells.find(cell => cell.row === expected.row && cell.column === expected.column);
      addCheck(`${label}: ${expected.row}/${expected.column} present`, true, Boolean(observed), Boolean(observed));
      if (!observed) return;
      addCheck(`${label}: ${expected.row}/${expected.column} observed`, expected.observed, observed.observed, expected.observed === observed.observed);
      addCheck(`${label}: ${expected.row}/${expected.column} expected`, expected.expected, observed.expected, anovaCloseEnough(observed.expected, expected.expected, 1e-9, 1e-10));
      addCheck(`${label}: ${expected.row}/${expected.column} row %`, expected.rowPercentage, observed.rowPercentage, anovaCloseEnough(observed.rowPercentage, expected.rowPercentage, 1e-9, 1e-10));
      addCheck(`${label}: ${expected.row}/${expected.column} column %`, expected.columnPercentage, observed.columnPercentage, anovaCloseEnough(observed.columnPercentage, expected.columnPercentage, 1e-9, 1e-10));
    });
  });
}

'
if (!grepl("function testChiSquareFixtures", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) stop("Could not find displayValue marker.")
  js <- sub(marker, paste0(functions, marker), js, fixed = TRUE)
}

old_load <- 'const [manifest, rawData, codebook, fixtures, regressionFixtures, logisticFixtures, anovaFixtures] = await Promise.all('
new_load <- 'const [manifest, rawData, codebook, fixtures, regressionFixtures, logisticFixtures, anovaFixtures, chiSquareFixtures] = await Promise.all('
if (grepl(old_load, js, fixed = TRUE)) js <- sub(old_load, new_load, js, fixed = TRUE)

old_call <- '    testAnovaFixtures(data, anovaFixtures);\n    render();'
new_call <- '    testAnovaFixtures(data, anovaFixtures);\n    testChiSquareFixtures(data, chiSquareFixtures);\n    render();'
if (!grepl("testChiSquareFixtures(data, chiSquareFixtures);", js, fixed = TRUE)) {
  if (!grepl(old_call, js, fixed = TRUE)) stop("Could not find ANOVA validation call.")
  js <- sub(old_call, new_call, js, fixed = TRUE)
}

writeLines(js, js_path, useBytes = TRUE)
checks <- data.frame(
  check = c("fixture_path_added","browser_chi_square_added","fixture_tests_invoked","exact_counts_added","expected_counts_added","percentage_checks_added","cramers_v_check_added"),
  passed = c(grepl("chiSquareFixtures:",js,fixed=TRUE),grepl("function browserChiSquare",js,fixed=TRUE),grepl("testChiSquareFixtures(data, chiSquareFixtures)",js,fixed=TRUE),grepl("expected.observed === observed.observed",js,fixed=TRUE),grepl("expected.expected",js,fixed=TRUE),grepl("expected.rowPercentage",js,fixed=TRUE),grepl("Cramers V",js,fixed=TRUE)),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(app_dir, "step23_validation_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Step 23 validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
cat("\nStep 23 completed successfully.\nRestart and open validation.html:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
