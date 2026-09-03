"use strict";

const paths = {
  manifest: "data/manifest-v1.0.0.json",
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json",
  fixtures: "data/worldview-reference-fixtures-v1.0.0.json",
  regressionFixtures: "data/worldview-regression-fixtures-v1.0.0.json",
  logisticFixtures: "data/worldview-logistic-fixtures-v1.0.0.json",
  anovaFixtures: "data/worldview-anova-fixtures-v1.0.0.json",
  chiSquareFixtures: "data/worldview-chi-square-fixtures-v1.0.0.json"
};

const checks = [];

function addCheck(name, expected, actual, passed) {
  checks.push({ name, expected, actual, passed, status: passed ? "pass" : "fail" });
}

function addWarningCheck(name, expected, actual, passed, warning) {
  checks.push({
    name,
    expected,
    actual,
    passed: passed || warning,
    status: passed ? "pass" : warning ? "warning" : "fail"
  });
}

function closeEnough(actual, expected, tolerance) {
  if (actual === null || expected === null) return actual === expected;
  return Number.isFinite(actual) && Number.isFinite(expected) && Math.abs(actual - expected) <= tolerance;
}

function columnsFrom(object) {
  const names = Object.keys(object);
  const length = object[names[0]].length;
  if (!names.length || !names.every(name => object[name].length === length)) throw new Error("Invalid column-oriented data.");
  return { columns: object, length };
}

function indicesFor(data, country) {
  const output = [];
  for (let i = 0; i < data.length; i += 1) {
    if (country === "ALL" || data.columns.B_COUNTRY_ALPHA[i] === country) output.push(i);
  }
  return output;
}

function numericValues(data, variable, indices) {
  return indices.map(i => data.columns[variable][i]).filter(value => value !== null && value !== "").map(Number).filter(Number.isFinite);
}

function quantileType7(sorted, probability) {
  const position = (sorted.length - 1) * probability;
  const lower = Math.floor(position), upper = Math.ceil(position);
  if (lower === upper) return sorted[lower];
  return sorted[lower] + (position - lower) * (sorted[upper] - sorted[lower]);
}

function numericSummary(values, totalN) {
  const sorted = [...values].sort((a, b) => a - b);
  const n = sorted.length;
  const mean = sorted.reduce((a, b) => a + b, 0) / n;
  const variance = sorted.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (n - 1);
  return {
    totalN, validN: n, missingN: totalN - n, mean,
    standardDeviation: Math.sqrt(variance), median: quantileType7(sorted, 0.5),
    minimum: sorted[0], firstQuartile: quantileType7(sorted, 0.25),
    thirdQuartile: quantileType7(sorted, 0.75), maximum: sorted[n - 1]
  };
}

function pearson(x, y) {
  const n = x.length;
  const meanX = x.reduce((a, b) => a + b, 0) / n;
  const meanY = y.reduce((a, b) => a + b, 0) / n;
  let numerator = 0, sumX = 0, sumY = 0;
  for (let i = 0; i < n; i += 1) {
    const dx = x[i] - meanX, dy = y[i] - meanY;
    numerator += dx * dy; sumX += dx * dx; sumY += dy * dy;
  }
  return numerator / Math.sqrt(sumX * sumY);
}

function correlation(data, fixture) {
  const selected = fixture.countries === "ALL" ? [] : (Array.isArray(fixture.countries) ? fixture.countries : [fixture.countries]);
  const allowed = new Set(selected);
  const x = [], y = [];
  let filteredRows = 0;
  for (let i = 0; i < data.length; i += 1) {
    if (allowed.size && !allowed.has(data.columns.B_COUNTRY_ALPHA[i])) continue;
    filteredRows += 1;
    const a = data.columns[fixture.variable1][i], b = data.columns[fixture.variable2][i];
    if (a === null || b === null || a === "" || b === "") continue;
    x.push(Number(a)); y.push(Number(b));
  }
  const r = pearson(x, y);
  const z = 0.5 * Math.log((1 + r) / (1 - r));
  const se = 1 / Math.sqrt(x.length - 3);
  return {
    filteredRows, completePairs: x.length, excludedMissingEither: filteredRows - x.length,
    correlation: r,
    confidenceLower: Math.tanh(z - 1.959963984540054 * se),
    confidenceUpper: Math.tanh(z + 1.959963984540054 * se)
  };
}

function testSummaryFixtures(data, fixtures) {
  fixtures.summaryCases.forEach(test => {
    if (!test.id.includes("numeric")) return;
    const indices = indicesFor(data, test.country);
    const actual = numericSummary(numericValues(data, test.variable, indices), indices.length);
    ["totalN", "validN", "missingN"].forEach(field => addCheck(`${test.id}: ${field}`, test.result[field], actual[field], test.result[field] === actual[field]));
    ["mean", "standardDeviation", "median", "minimum", "firstQuartile", "thirdQuartile", "maximum"].forEach(field => addCheck(`${test.id}: ${field}`, test.result[field], actual[field], closeEnough(actual[field], test.result[field], 1e-10)));
  });
}

function testCorrelationFixtures(data, fixtures) {
  fixtures.correlationCases.forEach((test, index) => {
    const actual = correlation(data, test);
    const label = `Correlation ${index + 1}: ${test.variable1}/${test.variable2}`;
    addCheck(`${label}: filtered rows`, test.filteredRows, actual.filteredRows, test.filteredRows === actual.filteredRows);
    addCheck(`${label}: complete pairs`, test.completePairs, actual.completePairs, test.completePairs === actual.completePairs);
    addCheck(`${label}: excluded rows`, test.excludedMissingEither, actual.excludedMissingEither, test.excludedMissingEither === actual.excludedMissingEither);
    addCheck(`${label}: r`, test.correlation, actual.correlation, closeEnough(actual.correlation, test.correlation, 1e-10));
    addCheck(`${label}: CI lower`, test.confidenceLower, actual.confidenceLower, closeEnough(actual.confidenceLower, test.confidenceLower, 1e-8));
    addCheck(`${label}: CI upper`, test.confidenceUpper, actual.confidenceUpper, closeEnough(actual.confidenceUpper, test.confidenceUpper, 1e-8));
  });
}


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

  const probabilities = x.map(row =>
    validationSigmoid(row.reduce((sum, value, j) => sum + value * beta[j], 0))
  );
  const finalWeights = probabilities.map(value => Math.max(value * (1 - value), 1e-12));
  const finalInformation = Array.from(
    { length: parameterCount },
    () => Array(parameterCount).fill(0)
  );
  for (let i = 0; i < n; i += 1) {
    for (let a = 0; a < parameterCount; a += 1) {
      for (let b = 0; b < parameterCount; b += 1) {
        finalInformation[a][b] += x[i][a] * finalWeights[i] * x[i][b];
      }
    }
  }
  informationInverse = matrixInverse(finalInformation);
  if (!informationInverse) throw new Error(`Singular final logistic information matrix: ${test.id}`);
  const standardErrors = informationInverse.map((row, i) => Math.sqrt(row[i]));
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

function logisticCloseEnough(actual, expected, absoluteTolerance = 0.001, relativeTolerance = 0.00001) {
  if (actual === null || expected === null) return actual === expected;
  if (!Number.isFinite(actual) || !Number.isFinite(expected)) return false;
  const difference = Math.abs(actual - expected);
  const scale = Math.max(1, Math.abs(actual), Math.abs(expected));
  return difference <= Math.max(absoluteTolerance, relativeTolerance * scale);
}

function addLogisticNumericCheck(name, expected, actual, strictAbsolute, strictRelative, warningAbsolute = 0.02, warningRelative = 0.0005) {
  const strictPass = logisticCloseEnough(actual, expected, strictAbsolute, strictRelative);
  const warningMatch = !strictPass && logisticCloseEnough(
    actual,
    expected,
    warningAbsolute,
    warningRelative
  );
  addWarningCheck(name, expected, actual, strictPass, warningMatch);
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
    addLogisticNumericCheck(`${label}: log likelihood`, test.logLikelihood, actual.logLikelihood, 0.001, 0.000001, 0.05, 0.00001);
    addLogisticNumericCheck(`${label}: AIC`, test.aic, actual.aic, 0.001, 0.000001, 0.05, 0.00001);
    addLogisticNumericCheck(`${label}: pseudo R squared`, test.mcfaddenPseudoRSquared, actual.mcfaddenPseudoRSquared, 0.00001, 0.00001, 0.0005, 0.001);

    test.coefficients.forEach((expected, index) => {
      const observed = actual.coefficients[index];
      addLogisticNumericCheck(`${label}: ${expected.term} estimate`, expected.estimate, observed.estimate, 0.001, 0.00001);
      addLogisticNumericCheck(`${label}: ${expected.term} SE`, expected.standardError, observed.standardError, 0.001, 0.00001);
      addLogisticNumericCheck(`${label}: ${expected.term} z`, expected.zValue, observed.zValue, 0.001, 0.00001);
      addLogisticNumericCheck(`${label}: ${expected.term} odds ratio`, expected.oddsRatio, observed.oddsRatio, 0.001, 0.0001, 0.02, 0.001);
      addLogisticNumericCheck(`${label}: ${expected.term} OR CI lower`, expected.oddsRatioLower, observed.oddsRatioLower, 0.001, 0.0001, 0.02, 0.001);
      addLogisticNumericCheck(`${label}: ${expected.term} OR CI upper`, expected.oddsRatioUpper, observed.oddsRatioUpper, 0.001, 0.0001, 0.02, 0.001);
    });
  });
}


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

function anovaCloseEnough(actual, expected, absoluteTolerance = 1e-7, relativeTolerance = 1e-10) {
  if (actual === null || expected === null) return actual === expected;
  if (!Number.isFinite(actual) || !Number.isFinite(expected)) return false;
  const difference = Math.abs(actual - expected);
  const scale = Math.max(1, Math.abs(actual), Math.abs(expected));
  return difference <= Math.max(absoluteTolerance, relativeTolerance * scale);
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
        anovaCloseEnough(item[2], item[1], 1e-7, 1e-10))
    );

    test.groups.forEach(expectedGroup => {
      const observedGroup = actual.groups.find(group => group.group === expectedGroup.group);
      addCheck(`${label}: group ${expectedGroup.group} present`, true,
        Boolean(observedGroup), Boolean(observedGroup));
      if (!observedGroup) return;
      addCheck(`${label}: ${expectedGroup.group} valid n`, expectedGroup.validN,
        observedGroup.validN, expectedGroup.validN === observedGroup.validN);
      addCheck(`${label}: ${expectedGroup.group} mean`, expectedGroup.mean,
        observedGroup.mean, anovaCloseEnough(observedGroup.mean, expectedGroup.mean, 1e-9, 1e-11));
      addCheck(`${label}: ${expectedGroup.group} SD`, expectedGroup.standardDeviation,
        observedGroup.standardDeviation,
        anovaCloseEnough(observedGroup.standardDeviation, expectedGroup.standardDeviation, 1e-9, 1e-11));
    });
  });
}


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

function displayValue(value) {
  if (typeof value === "number") return Number.isInteger(value) ? String(value) : value.toPrecision(10);
  return String(value);
}

function render() {
  const body = document.getElementById("validation-results");
  body.innerHTML = checks.map(check => `<tr>
    <td>${check.name}</td><td>${displayValue(check.expected)}</td><td>${displayValue(check.actual)}</td>
    <td><span class="validation-badge ${check.status}">${check.status === "pass" ? "Pass" : check.status === "warning" ? "Warning" : "Fail"}</span></td>
  </tr>`).join("");
  const passed = checks.filter(check => check.status === "pass").length;
  const warnings = checks.filter(check => check.status === "warning").length;
  const failed = checks.filter(check => check.status === "fail").length;
  document.getElementById("validation-total").textContent = checks.length;
  document.getElementById("validation-passed").textContent = passed;
  document.getElementById("validation-warnings").textContent = warnings;
  document.getElementById("validation-failed").textContent = failed;
  const status = document.getElementById("validation-status");
  status.textContent = failed
    ? `${failed} validation checks failed; ${warnings} warnings.`
    : warnings
      ? `No validation failures; ${warnings} small numerical differences reported as warnings.`
      : `All ${passed} validation checks passed.`;
  status.classList.toggle("validation-success", failed === 0);
  status.classList.toggle("validation-failure", failed > 0);
}

async function runValidation() {
  try {
    const [manifest, rawData, codebook, fixtures, regressionFixtures, logisticFixtures, anovaFixtures, chiSquareFixtures] = await Promise.all(
      Object.values(paths).map(path => fetch(path).then(response => {
        if (!response.ok) throw new Error(`${path} returned ${response.status}`);
        return response.json();
      }))
    );
    const data = columnsFrom(rawData);
    addCheck("Manifest participant count", manifest.participantCount, data.length, manifest.participantCount === data.length);
    addCheck("Manifest variable count", 39, manifest.studentVariableCount, manifest.studentVariableCount === 39);
    addCheck("Codebook variable count", 39, codebook.variables.length, codebook.variables.length === 39);
    addCheck("Only Q289 is correlation-ineligible", "Q289", codebook.variables.filter(v => !v.correlationEligible).map(v => v.id).join(","), codebook.variables.filter(v => !v.correlationEligible).map(v => v.id).join(",") === "Q289");
    testSummaryFixtures(data, fixtures);
    testCorrelationFixtures(data, fixtures);
    testRegressionFixtures(data, regressionFixtures);
    testLogisticFixtures(data, logisticFixtures);
    testAnovaFixtures(data, anovaFixtures);
    testChiSquareFixtures(data, chiSquareFixtures);
    render();
  } catch (error) {
    console.error(error);
    document.getElementById("validation-status").textContent = `Validation could not run: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", runValidation);
