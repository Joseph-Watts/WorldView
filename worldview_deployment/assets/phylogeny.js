"use strict";

const phyPaths = {
  data: "data/worldview-browser-data-v1.0.0.json",
  codebook: "data/worldview-codebook-v1.0.0.json",
  tree: "data/phylogeny/country_phylogeny_tree.tree",
  mapping: "data/phylogeny/country_phylogeny.csv"
};

const phy = {
  data: null,
  vars: [],
  tree: null,
  map: new Map(),
  countries: [],
  rows: []
};

const PHY_TOPIC_ORDER = [
  "Social values, norms and stereotypes",
  "Happiness and wellbeing",
  "Social capital, trust and organisational membership",
  "Economic values",
  "Perceptions of corruption",
  "Perceptions of migration",
  "Perceptions of security",
  "Postmaterialism",
  "Science and technology",
  "Religious values",
  "Ethical values",
  "Political interest and political participation",
  "Political culture and political regimes",
  "Demographic and socioeconomic variables",
  "Technical and derived variables",
  "Additional variables"
];

const pe = value => String(value ?? "")
  .replaceAll("&", "&amp;")
  .replaceAll("<", "&lt;")
  .replaceAll(">", "&gt;")
  .replaceAll("\"", "&quot;");

const pf = (value, digits = 2) =>
  new Intl.NumberFormat("en-NZ", { maximumFractionDigits: digits }).format(value);

function csv(text) {
  const lines = text.trim().split(/\r?\n/);
  const parse = line => {
    const out = [];
    let cell = "";
    let quoted = false;
    for (let i = 0; i < line.length; i += 1) {
      const character = line[i];
      if (character === "\"") {
        if (quoted && line[i + 1] === "\"") {
          cell += "\"";
          i += 1;
        } else {
          quoted = !quoted;
        }
      } else if (character === "," && !quoted) {
        out.push(cell);
        cell = "";
      } else {
        cell += character;
      }
    }
    out.push(cell);
    return out;
  };

  const header = parse(lines.shift());
  return lines.map(line =>
    Object.fromEntries(parse(line).map((value, index) => [header[index], value]))
  );
}

function newick(text) {
  let index = 0;
  let id = 0;

  function node(parent = null) {
    const current = { id: id++, parent, children: [], name: "", length: 0 };

    if (text[index] === "(") {
      index += 1;
      while (true) {
        current.children.push(node(current));
        if (text[index] === ",") {
          index += 1;
          continue;
        }
        if (text[index] === ")") {
          index += 1;
          break;
        }
      }
    }

    let name = "";
    while (index < text.length && !",():;".includes(text[index])) name += text[index++];
    current.name = name.trim();

    if (text[index] === ":") {
      index += 1;
      let length = "";
      while (index < text.length && !",();".includes(text[index])) length += text[index++];
      current.length = Number(length) || 0;
    }

    return current;
  }

  return node();
}

function tips(node, output = []) {
  if (!node.children.length) output.push(node);
  else node.children.forEach(child => tips(child, output));
  return output;
}

function selected() {
  return new Set(
    [...document.getElementById("phylo-countries").selectedOptions].map(option => option.value)
  );
}

function aggregate(variableId) {
  const values = new Map();

  for (let i = 0; i < phy.data.length; i += 1) {
    const code = phy.data.columns.B_COUNTRY_ALPHA[i];
    const raw = phy.data.columns[variableId][i];
    if (raw == null || raw === "" || !Number.isFinite(Number(raw))) continue;

    if (!values.has(code)) values.set(code, { n: 0, sum: 0 });
    const row = values.get(code);
    row.n += 1;
    row.sum += Number(raw);
  }

  const variable = phy.vars.find(item => item.id === variableId);
  return new Map(
    [...values].map(([code, row]) => [
      code,
      {
        n: row.n,
        value: variable.analysisType === "binary"
          ? 100 * row.sum / row.n
          : row.sum / row.n
      }
    ])
  );
}

function branchLength(length, mode) {
  const value = Math.max(0, Number(length) || 0);
  if (mode === "equal") return 1;
  if (mode === "sqrt") return Math.sqrt(value);
  if (mode === "square") return value * value;
  return value;
}

function layout(root, keep, branchMode) {
  function prune(node) {
    node.children = node.children.map(prune).filter(Boolean);
    if (!node.children.length) return keep.has(node.name) ? node : null;
    return node.children.length ? node : null;
  }

  root = prune(root);
  if (!root) return null;

  let y = 0;
  let maximum = 0;

  function place(node, x) {
    const edgeLength = node.parent ? branchLength(node.length, branchMode) : 0;
    node.x = x + edgeLength;
    maximum = Math.max(maximum, node.x);

    if (!node.children.length) {
      node.y = y;
      y += 1;
    } else {
      node.children.forEach(child => place(child, node.x));
      node.y = node.children.reduce((sum, child) => sum + child.y, 0) / node.children.length;
    }
  }

  place(root, 0);

  // Very small or zero source branch lengths can otherwise collapse the plot.
  if (!(maximum > 0)) maximum = 1;

  return { root, count: y, max: maximum };
}

function clone(node, parent = null) {
  const copy = { ...node, parent, children: [] };
  copy.children = node.children.map(child => clone(child, copy));
  return copy;
}

function colour(value, minimum, maximum) {
  if (value == null) return "#d1d5d8";
  const t = maximum === minimum ? 0.5 : (value - minimum) / (maximum - minimum);
  const low = [255, 224, 102];
  const high = [74, 20, 140];
  return `rgb(${Math.round(low[0] + t * (high[0] - low[0]))},${Math.round(low[1] + t * (high[1] - low[1]))},${Math.round(low[2] + t * (high[2] - low[2]))})`;
}

function variableTopic(variable) {
  return variable.topic || variable.group || "Additional variables";
}

function populateVariableSelect(select, variables) {
  select.innerHTML = "";
  const groups = new Map();

  variables.forEach(variable => {
    const topic = variableTopic(variable);
    if (!groups.has(topic)) groups.set(topic, []);
    groups.get(topic).push(variable);
  });

  const orderedTopics = [
    ...PHY_TOPIC_ORDER.filter(topic => groups.has(topic)),
    ...[...groups.keys()].filter(topic => !PHY_TOPIC_ORDER.includes(topic)).sort((a, b) => a.localeCompare(b))
  ];

  orderedTopics.forEach(topic => {
    const optgroup = document.createElement("optgroup");
    optgroup.label = topic;

    groups.get(topic)
      .sort((left, right) => {
        const qLeft = Number(String(left.id).replace(/^Q/, ""));
        const qRight = Number(String(right.id).replace(/^Q/, ""));
        if (Number.isFinite(qLeft) && Number.isFinite(qRight)) return qLeft - qRight;
        return String(left.displayName).localeCompare(String(right.displayName));
      })
      .forEach(variable => {
        optgroup.appendChild(new Option(`${variable.id}: ${variable.displayName}`, variable.id));
      });

    select.appendChild(optgroup);
  });
}

function displaySettings() {
  return {
    layout: document.getElementById("phylo-layout")?.value || "rectangular",
    branchMode: document.getElementById("phylo-branch-scale")?.value || "original",
    rowHeight: Number(document.getElementById("phylo-tip-spacing")?.value || 22),
    labelSize: Number(document.getElementById("phylo-label-size")?.value || 11)
  };
}

function branchModeLabel(mode) {
  return {
    original: "original phylogenetic branch lengths",
    equal: "equal branch lengths (cladogram)",
    sqrt: "square-root transformed branch lengths",
    square: "squared branch lengths"
  }[mode] || "original phylogenetic branch lengths";
}

function draw(event) {
  event.preventDefault();

  const variableId = document.getElementById("phylo-variable").value;
  const variable = phy.vars.find(item => item.id === variableId);
  const allowed = selected();
  const aggregated = aggregate(variableId);
  const allTips = tips(phy.tree);
  const settings = displaySettings();

  const keep = new Set(
    allTips
      .filter(tip => {
        const code = tip.name.split("_").pop();
        return allowed.has(code) && aggregated.has(code) && phy.map.has(code);
      })
      .map(tip => tip.name)
  );

  const treeLayout = layout(clone(phy.tree), keep, settings.branchMode);

  if (!treeLayout || treeLayout.count < 2) {
    document.getElementById("phylo-message").textContent =
      "At least two matched countries with valid data are required.";
    return;
  }

  const width = 1100;
  const rowHeight = Math.max(14, settings.rowHeight);
  const height = Math.max(420, treeLayout.count * rowHeight + 40);
  const left = 25;
  const right = 285;
  const xScale = x => left + x / treeLayout.max * (width - left - right);
  const yScale = y => 25 + y * rowHeight;

  const nodes = [];
  function walk(node) {
    nodes.push(node);
    node.children.forEach(walk);
  }
  walk(treeLayout.root);

  const values = [...keep].map(name => aggregated.get(name.split("_").pop()).value);
  const minimum = Math.min(...values);
  const maximum = Math.max(...values);
  const labelMode = document.getElementById("phylo-label").value;

  let svg = `<svg viewBox="0 0 ${width} ${height}" width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg"><rect x="0" y="0" width="${width}" height="${height}" fill="#ffffff"/>`;

  if (settings.layout === "slanted") {
    nodes.filter(node => node.parent).forEach(node => {
      svg += `<line x1="${xScale(node.parent.x)}" y1="${yScale(node.parent.y)}" x2="${xScale(node.x)}" y2="${yScale(node.y)}" class="phy-branch" stroke="#495957" stroke-width="1.35" fill="none" vector-effect="non-scaling-stroke"/>`;
    });
  } else {
    nodes.filter(node => node.children.length).forEach(node => {
      const childYs = node.children.map(child => yScale(child.y));
      svg += `<line x1="${xScale(node.x)}" y1="${Math.min(...childYs)}" x2="${xScale(node.x)}" y2="${Math.max(...childYs)}" class="phy-branch" stroke="#495957" stroke-width="1.35" fill="none" vector-effect="non-scaling-stroke"/>`;
      node.children.forEach(child => {
        svg += `<line x1="${xScale(node.x)}" y1="${yScale(child.y)}" x2="${xScale(child.x)}" y2="${yScale(child.y)}" class="phy-branch" stroke="#495957" stroke-width="1.35" fill="none" vector-effect="non-scaling-stroke"/>`;
      });
    });
  }

  nodes.filter(node => !node.children.length).forEach(node => {
    const parts = node.name.split("_");
    const code = parts.pop();
    const metadata = phy.map.get(code);
    const aggregateRow = aggregated.get(code);

    const label = labelMode === "language"
      ? metadata.language_name
      : labelMode === "both"
        ? `${metadata.country_name} (${metadata.language_name})`
        : metadata.country_name;

    const display = `${pf(aggregateRow.value)}${variable.analysisType === "binary" ? "%" : ""}`;

    svg += `<circle cx="${xScale(node.x)}" cy="${yScale(node.y)}" r="6" fill="${colour(aggregateRow.value, minimum, maximum)}" class="phy-tip" tabindex="0" data-country="${pe(metadata.country_name)}" data-language="${pe(metadata.language_name)}" data-family="${pe(metadata.family_language_name)}" data-value="${display}" data-n="${aggregateRow.n}"/>`;
    svg += `<text x="${xScale(node.x) + 10}" y="${yScale(node.y) + settings.labelSize * 0.35}" class="phy-label" font-family="Arial, sans-serif" font-size="${settings.labelSize}" fill="#24302e">${pe(label)}</text>`;
  });

  svg += "</svg>";

  document.getElementById("phylo-chart").innerHTML = svg;
  bind();
  document.getElementById("phylo-title").textContent = variable.displayName;

  const measure = variable.analysisType === "binary"
    ? "percentage of valid responses coded 1"
    : "mean response";

  const layoutLabel = settings.layout === "slanted" ? "slanted" : "rectangular";
  document.getElementById("phylo-caption").textContent =
    `Tip colours show the ${measure} for ${variable.id}: ${variable.displayName}. ` +
    `Tree layout is ${layoutLabel}; branches use ${branchModeLabel(settings.branchMode)}.`;

  document.getElementById("phylo-legend").innerHTML =
    `<span>${pf(minimum)}${variable.analysisType === "binary" ? "%" : ""}</span><span class="map-gradient"></span><span>${pf(maximum)}${variable.analysisType === "binary" ? "%" : ""}</span>`;

  document.getElementById("phylo-message").textContent =
    `${treeLayout.count} matched country tips displayed.`;
  document.getElementById("phylo-save").hidden = false;

  phy.rows = [...keep].map(name => {
    const code = name.split("_").pop();
    const metadata = phy.map.get(code);
    const row = aggregated.get(code);
    return {
      country_code: code,
      country: metadata?.country_name || code,
      language: metadata?.language_name || "",
      n: row?.n ?? 0,
      value: row?.value ?? null
    };
  });
}

function bind() {
  document.querySelectorAll(".phy-tip").forEach(tip => {
    const show = event => {
      const tooltip = document.getElementById("phylo-tooltip");
      tooltip.innerHTML =
        `<strong>${pe(tip.dataset.country)}</strong>` +
        `<span>Language: ${pe(tip.dataset.language)}</span>` +
        `<span>Family: ${pe(tip.dataset.family)}</span>` +
        `<span>Value: ${pe(tip.dataset.value)}</span>` +
        `<span>Valid responses: ${tip.dataset.n}</span>`;
      tooltip.hidden = false;
      tooltip.style.left = `${event.offsetX + 14}px`;
      tooltip.style.top = `${event.offsetY + 14}px`;
    };

    tip.addEventListener("mouseenter", show);
    tip.addEventListener("mousemove", show);
    tip.addEventListener("mouseleave", () => {
      document.getElementById("phylo-tooltip").hidden = true;
    });
    tip.addEventListener("focus", show);
    tip.addEventListener("blur", () => {
      document.getElementById("phylo-tooltip").hidden = true;
    });
  });
}

// Standalone fallback export. The cumulative enhancement file replaces this
// with the richer export that includes the colour-scale key.
function save() {
  const source = document.querySelector("#phylo-chart svg");
  if (!source) return;

  const cloneSvg = source.cloneNode(true);
  const box = source.viewBox.baseVal;
  const width = Math.max(1100, Math.ceil(box.width));
  const height = Math.ceil(box.height);
  const caption = document.getElementById("phylo-caption").textContent;
  const extra = 80;
  const scale = 2;

  cloneSvg.setAttribute("width", width);
  cloneSvg.setAttribute("height", height);
  cloneSvg.setAttribute("xmlns", "http://www.w3.org/2000/svg");

  const svgText = new XMLSerializer().serializeToString(cloneSvg);
  const blob = new Blob([svgText], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const image = new Image();
  const canvas = document.createElement("canvas");
  canvas.width = width * scale;
  canvas.height = (height + extra) * scale;
  const context = canvas.getContext("2d");

  image.onload = () => {
    context.scale(scale, scale);
    context.fillStyle = "#ffffff";
    context.fillRect(0, 0, width, height + extra);
    context.drawImage(image, 0, 0, width, height);
    context.fillStyle = "#35413f";
    context.font = "15px Arial";

    const words = caption.split(" ");
    let line = "";
    let y = height + 28;
    words.forEach(word => {
      const test = `${line}${word} `;
      if (context.measureText(test).width > width - 50) {
        context.fillText(line, 25, y);
        line = `${word} `;
        y += 20;
      } else {
        line = test;
      }
    });
    context.fillText(line, 25, y);

    const link = document.createElement("a");
    link.download = `worldview-phylogeny-${document.getElementById("phylo-variable").value}.png`;
    link.href = canvas.toDataURL("image/png");
    link.click();
    URL.revokeObjectURL(url);
  };

  image.onerror = () => {
    URL.revokeObjectURL(url);
    document.getElementById("phylo-message").textContent =
      "The tree image could not be created in this browser.";
  };

  image.src = url;
}

function reset() {
  const defaultVariable = phy.vars.some(variable => variable.id === "Q177")
    ? "Q177"
    : phy.vars[0]?.id;

  if (defaultVariable) document.getElementById("phylo-variable").value = defaultVariable;
  [...document.getElementById("phylo-countries").options].forEach(option => {
    option.selected = true;
  });

  const layoutControl = document.getElementById("phylo-layout");
  const branchControl = document.getElementById("phylo-branch-scale");
  const spacingControl = document.getElementById("phylo-tip-spacing");
  const labelSizeControl = document.getElementById("phylo-label-size");
  if (layoutControl) layoutControl.value = "rectangular";
  if (branchControl) branchControl.value = "original";
  if (spacingControl) spacingControl.value = "22";
  if (labelSizeControl) labelSizeControl.value = "11";

  document.getElementById("phylo-chart").innerHTML = "";
  document.getElementById("phylo-legend").innerHTML = "";
  document.getElementById("phylo-caption").textContent = "";
  document.getElementById("phylo-title").textContent = "Select data to begin";
  document.getElementById("phylo-message").textContent =
    "Choose a variable and create the phylogeny.";
  document.getElementById("phylo-save").hidden = true;
}

async function init() {
  const loading = document.getElementById("phylo-loading");

  try {
    const [data, codebook, treeText, mappingText] = await Promise.all([
      fetch(phyPaths.data).then(response => {
        if (!response.ok) throw new Error(`Data request failed: ${response.status}`);
        return response.json();
      }),
      fetch(phyPaths.codebook).then(response => {
        if (!response.ok) throw new Error(`Codebook request failed: ${response.status}`);
        return response.json();
      }),
      fetch(phyPaths.tree).then(response => {
        if (!response.ok) throw new Error(`Tree request failed: ${response.status}`);
        return response.text();
      }),
      fetch(phyPaths.mapping).then(response => {
        if (!response.ok) throw new Error(`Mapping request failed: ${response.status}`);
        return response.text();
      })
    ]);

    const names = Object.keys(data);
    if (!names.length) throw new Error("Browser data contains no columns.");
    const length = data[names[0]].length;
    phy.data = { columns: data, length };

    phy.vars = codebook.variables.filter(variable => variable.analysisType !== "nominal");
    phy.tree = newick(treeText.trim());
    csv(mappingText).forEach(row => phy.map.set(row.iso3166alpha3, row));

    const treeCodes = new Set(tips(phy.tree).map(tip => tip.name.split("_").pop()));
    const pairs = new Map();
    for (let i = 0; i < length; i += 1) {
      const code = data.B_COUNTRY_ALPHA[i];
      if (treeCodes.has(code) && phy.map.has(code)) pairs.set(code, data.B_COUNTRY[i]);
    }

    phy.countries = [...pairs]
      .map(([code, name]) => ({ code, name }))
      .sort((left, right) => String(left.name).localeCompare(String(right.name)));

    const variableSelect = document.getElementById("phylo-variable");
    populateVariableSelect(variableSelect, phy.vars);
    variableSelect.value = phy.vars.some(variable => variable.id === "Q177")
      ? "Q177"
      : phy.vars[0]?.id || "";

    const countrySelect = document.getElementById("phylo-countries");
    phy.countries.forEach(country => {
      const option = new Option(`${country.name} (${country.code})`, country.code);
      option.selected = true;
      countrySelect.add(option);
    });

    loading.hidden = true;
    document.getElementById("phylo-form").hidden = false;
  } catch (error) {
    console.error("Phylogeny initialisation failed", error);
    loading.textContent = `Phylogeny could not be loaded: ${error.message}`;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  const form = document.getElementById("phylo-form");
  const resetButton = document.getElementById("phylo-reset");
  const saveButton = document.getElementById("phylo-save");
  if (!form || !resetButton || !saveButton) return;

  form.addEventListener("submit", draw);
  resetButton.addEventListener("click", reset);
  saveButton.addEventListener("click", save);
  init();
});
