# WorldView Online - Step 20
# Extend validation.html with flexible one-way ANOVA checks against R aov().

app_dir <- "worldview_static_app"
validation_js_path <- file.path(app_dir, "assets", "validation.js")
fixture_path <- file.path(app_dir, "data", "worldview-anova-fixtures-v1.0.0.json")

if (!all(file.exists(c(validation_js_path, fixture_path)))) {
  stop("Required validation.js or ANOVA fixture file is missing.")
}

js <- paste(readLines(validation_js_path, warn = FALSE), collapse = "\n")

# Add the ANOVA fixture path.
path_anchor <- '  logisticFixtures: "data/worldview-logistic-fixtures-v1.0.0.json"'
path_replacement <- paste0(
  path_anchor,
  ',\n  anovaFixtures: "data/worldview-anova-fixtures-v1.0.0.json"'
)

if (!grepl("anovaFixtures:", js, fixed = TRUE)) {
  if (!grepl(path_anchor, js, fixed = TRUE)) {
    stop("Could not locate logisticFixtures in validation.js.")
  }
  js <- sub(path_anchor, path_replacement, js, fixed = TRUE)
}

marker <- 'function displayValue(value) {'

anova_validation <- '
function validationAnovaLabel(groupingVariable, value, countryName) {
  if (groupingVariable === "COUNTRY") return countryName;
  if (groupingVariable === "Q289") return String(value);
  if (["Q165", "Q166", "Q167", "Q168"].includes(groupingVariable)) {
    return Number(value) === 1 ? "Yes" : "No";
  }
  if (groupingVariable === "Q260") return Number(value) === 1 ? "Male" : "Female";
  if (groupingVariable === "Q263") {
    return Number(value) === 1 ? "Born in this country" : "Immigrant to this country";
  }
  return String(value);
}

function browserAnova(data, test) {
  const selectedCountries = test.countries === "ALL"
    ? []
    : (Array.isArray(test.countries) ? test.countries : [test.countries]);
  const allowed = new Set(selectedCountries);
  const groups = new Map();

  for (let i = 0; i < data.length; i += 1) {
    const countryCode = data.columns.B_COUNTRY_ALPHA[i];
    if (allowed.size && !allowed.has(countryCode)) continue;

    const outcome = data.columns[test.outcome][i];
    if (outcome === null || outcome === "" || !Number.isFinite(Number(outcome))) continue;

    let rawGroup;
    let groupLabel;
    if (test.groupingVariable === "COUNTRY") {
      rawGroup = countryCode;
      groupLabel = data.columns.B_COUNTRY[i];
    } else {
      rawGroup = data.columns[test.groupingVariable][i];
      if (rawGroup === null || rawGroup === "") continue;
      groupLabel = validationAnovaLabel(
        test.groupingVariable,
        rawGroup,
        data.columns.B_COUNTRY[i]
      );
    }

    const key = String(rawGroup);
    if (!groups.has(key)) groups.set(key, { group: groupLabel, values: [] });
    groups.get(key).values.push(Number(outcome));
  }

  const retained = [...groups.values()]
    .filter(group => group.values.length >= 2)
    .map(group => {
      const validN = group.values.length;
      const mean = group.values.reduce((sum, value) => sum + value, 0) / validN;
      const variance = group.values.reduce(
        (sum, value) => sum + (value - mean) ** 2,
        0
      ) / (validN - 1);
      return {
        group: group.group,
        values: group.values,
        validN,
        mean,
        standardDeviation: Math.sqrt(variance)
      };
    })
    .sort((left, right) => left.group.localeCompare(right.group));

  const validObservations = retained.reduce((sum, group) => sum + group.validN, 0);
  const grandMean = retained.reduce(
    (sum, group) => sum + group.validN * group.mean,
    0
  ) / validObservations;
  const sumSquaresBetween = retained.reduce(
    (sum, group) => sum + group.validN * (group.mean - grandMean) ** 2,
    0
  );
  const sumSquaresWithin = retained.reduce(
    (sum, group) => sum + group.values.reduce(
      (inner, value) => inner + (value - group.mean) ** 2,
      0
    ),
    0
  );
  const degreesFreedomBetween = retained.length - 1;
  const degreesFreedomWithin = validObservations - retained.length;
  const meanSquareBetween = sumSquaresBetween / degreesFreedomBetween;
  const meanSquareWithin = sumSquaresWithin / degreesFreedomWithin;
  const fStatistic = meanSquareBetween / meanSquareWithin;
  const etaSquared = sumSquaresBetween / (sumSquaresBetween + sumSquaresWithin);
  const omegaSquared = (
    sumSquaresBetween - degreesFreedomBetween * meanSquareWithin
  ) / (
    sumSquaresBetween + sumSquaresWithin + meanSquareWithin
  );

  return {
    validObservations,
    groupCount: retained.length,
    groups: retained,
    sumSquaresBetween,
    sumSquaresWithin,
    degreesFreedomBetween,
    degreesFreedomWithin,
    meanSquareBetween,
    meanSquareWithin,
    fStatistic,
    etaSquared,
    omegaSquared
  };
}

function testAnovaFixtures(data, anovaFixtures) {
  anovaFixtures.cases.forEach(test => {
    const actual = browserAnova(data, test);
    const label = `ANOVA ${test.id}`;

    addCheck(`${label}: valid observations`, test.validObservations,
      actual.validObservations, test.validObservations === actual.validObservations);
    addCheck(`${label}: group count`, test.groupCount,
      actual.groupCount, test.groupCount === actual.groupCount);
    addCheck(`${label}: df between`, test.degreesFreedomBetween,
      actual.degreesFreedomBetween,
      test.degreesFreedomBetween === actual.degreesFreedomBetween);
    addCheck(`${label}: df within`, test.degreesFreedomWithin,
      actual.degreesFreedomWithin,
      test.degreesFreedomWithin === actual.degreesFreedomWithin);

    [
      ["SS between", test.sumSquaresBetween, actual.sumSquaresBetween],
      ["SS within", test.sumSquaresWithin, actual.sumSquaresWithin],
      ["MS between", test.meanSquareBetween, actual.meanSquareBetween],
      ["MS within", test.meanSquareWithin, actual.meanSquareWithin],
      ["F statistic", test.fStatistic, actual.fStatistic],
      ["eta squared", test.etaSquared, actual.etaSquared],
      ["omega squared", test.omegaSquared, actual.omegaSquared]
    ].forEach(item =>
      addCheck(`${label}: ${item[0]}`, item[1], item[2],
        closeEnough(item[2], item[1], 1e-8))
    );

    test.groups.forEach(expectedGroup => {
      const observedGroup = actual.groups.find(group => group.group === expectedGroup.group);
      addCheck(`${label}: group ${expectedGroup.group} present`, true,
        Boolean(observedGroup), Boolean(observedGroup));
      if (!observedGroup) return;
      addCheck(`${label}: ${expectedGroup.group} valid n`, expectedGroup.validN,
        observedGroup.validN, expectedGroup.validN === observedGroup.validN);
      addCheck(`${label}: ${expectedGroup.group} mean`, expectedGroup.mean,
        observedGroup.mean, closeEnough(observedGroup.mean, expectedGroup.mean, 1e-10));
      addCheck(`${label}: ${expectedGroup.group} SD`, expectedGroup.standardDeviation,
        observedGroup.standardDeviation,
        closeEnough(observedGroup.standardDeviation, expectedGroup.standardDeviation, 1e-10));
    });
  });
}

'

if (!grepl("function testAnovaFixtures", js, fixed = TRUE)) {
  if (!grepl(marker, js, fixed = TRUE)) stop("Could not find displayValue marker.")
  js <- sub(marker, paste0(anova_validation, marker), js, fixed = TRUE)
}

old_load <- paste0(
  "const [manifest, rawData, codebook, fixtures, regressionFixtures, ",
  "logisticFixtures] = await Promise.all("
)
new_load <- paste0(
  "const [manifest, rawData, codebook, fixtures, regressionFixtures, ",
  "logisticFixtures, anovaFixtures] = await Promise.all("
)
if (grepl(old_load, js, fixed = TRUE)) {
  js <- sub(old_load, new_load, js, fixed = TRUE)
}

old_call <- '    testLogisticFixtures(data, logisticFixtures);\n    render();'
new_call <- '    testLogisticFixtures(data, logisticFixtures);\n    testAnovaFixtures(data, anovaFixtures);\n    render();'
if (!grepl("testAnovaFixtures(data, anovaFixtures);", js, fixed = TRUE)) {
  if (!grepl(old_call, js, fixed = TRUE)) stop("Could not find logistic validation call.")
  js <- sub(old_call, new_call, js, fixed = TRUE)
}

writeLines(js, validation_js_path, useBytes = TRUE)

checks <- data.frame(
  check = c(
    "anova_fixture_path_added",
    "browser_anova_added",
    "anova_cases_invoked",
    "group_summary_checks_added",
    "anova_table_checks_added",
    "effect_size_checks_added"
  ),
  passed = c(
    grepl("anovaFixtures:", js, fixed = TRUE),
    grepl("function browserAnova", js, fixed = TRUE),
    grepl("anovaFixtures.cases.forEach", js, fixed = TRUE),
    grepl("expectedGroup.standardDeviation", js, fixed = TRUE),
    grepl("sumSquaresBetween", js, fixed = TRUE),
    grepl("omegaSquared", js, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  checks,
  file.path(app_dir, "step20_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

if (!all(checks$passed)) {
  stop("Step 20 structural validation failed: ", paste(checks$check[!checks$passed], collapse = ", "))
}

cat("\nStep 20 completed successfully.\n\n")
cat("Restart the site and open validation.html:\n")
cat("  servr::httd(\"worldview_static_app\", browser = TRUE)\n")
cat("The validation total should increase and the ANOVA checks should pass.\n")
