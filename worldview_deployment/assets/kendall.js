"use strict";

// Kendall's tau-b and its two-sided, tie-adjusted asymptotic test (no continuity correction).
function kendallSummary(x, y) {
  const n = x.length;
  const yCounts = new Map();
  for (const value of y) yCounts.set(value, (yCounts.get(value) || 0) + 1);
  const ranks = new Map([...yCounts.keys()].sort((a, b) => a - b).map((value, i) => [value, i + 1]));
  const order = x.map((_, i) => i).sort((a, b) => x[a] - x[b]);
  const tree = new Float64Array(ranks.size + 1);

  // Fenwick prefix counts give the number of earlier observations at or below a y rank.
  function prefix(rank) {
    let count = 0;
    for (; rank > 0; rank -= rank & -rank) count += tree[rank];
    return count;
  }

  let score = 0;
  const xCounts = [];
  for (let start = 0; start < n;) {
    let end = start + 1;
    while (end < n && x[order[end]] === x[order[start]]) end++;
    xCounts.push(end - start);
    // Query the entire group before inserting it so tied x pairs contribute zero.
    for (let i = start; i < end; i++) {
      const rank = ranks.get(y[order[i]]);
      score += prefix(rank - 1) - (start - prefix(rank));
    }
    for (let i = start; i < end; i++) {
      for (let rank = ranks.get(y[order[i]]); rank < tree.length; rank += rank & -rank) tree[rank]++;
    }
    start = end;
  }

  function tieSums(counts) {
    let pairs = 0, triples = 0, weighted = 0;
    for (const count of counts) {
      const pair = count * (count - 1);
      pairs += pair;
      triples += pair * (count - 2);
      weighted += pair * (2 * count + 5);
    }
    return { pairs, triples, weighted };
  }
  const tx = tieSums(xCounts);
  const ty = tieSums(yCounts.values());
  const pairs = n * (n - 1);
  const denominator = Math.sqrt((pairs - tx.pairs) * (pairs - ty.pairs)) / 2;
  if (!denominator || n < 3) return { statistic: null, pApprox: null, lower: null, upper: null };
  const variance = (pairs * (2 * n + 5) - tx.weighted - ty.weighted) / 18
    + tx.pairs * ty.pairs / (2 * pairs)
    + tx.triples * ty.triples / (9 * pairs * (n - 2));
  const pApprox = 2 * worldviewNormalCdf(-Math.abs(score) / Math.sqrt(variance));
  return { statistic: score / denominator, pApprox, lower: null, upper: null };
}

function worldviewNormalCdf(x) {
  if (x === 0) return 0.5;
  const sign = x < 0 ? -1 : 1;
  const z = Math.abs(x) / Math.sqrt(2);
  const t = 1 / (1 + 0.3275911 * z);
  const erf =
    1 -
    (((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) *
      t *
      Math.exp(-z * z));
  return 0.5 * (1 + sign * erf);
}
