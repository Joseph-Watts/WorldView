"use strict";
const xtPaths={data:"data/worldview-browser-data-v1.0.0.json",codebook:"data/worldview-codebook-v1.0.0.json"};
const xtState={data:null,variables:[],countries:[],rows:[]};
const xtEsc=v=>String(v??"").replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
const xtFmt=(v,d=1)=>new Intl.NumberFormat("en-NZ",{maximumFractionDigits:d}).format(v);
const xtSelected=id=>[...document.getElementById(id).selectedOptions].map(o=>o.value);
function xtParse(o){const n=Object.keys(o),l=o[n[0]].length;if(!n.length||!n.every(k=>o[k].length===l))throw Error("Invalid data");return{columns:o,length:l};}
function calculateXt(event) {
  event.preventDefault();
  const rowId = document.getElementById("crosstab-row").value;
  const columnId = document.getElementById("crosstab-column").value;
  const message = document.getElementById("crosstab-message");
  const rowVar = xtState.variables.find(v => v.id === rowId);
  const columnVar = xtState.variables.find(v => v.id === columnId);
  if (rowId === columnId) {
    message.textContent = "Choose two different variables.";
    return;
  }
  const countries = new Set(xtSelected("crosstab-countries"));
  const { rows, cols, n, count } = contingencyTable(xtState.data, rowVar, columnVar, countries);
  if (!n) {
    message.textContent = "No complete observations are available.";
    return;
  }

  const display = document.getElementById("crosstab-display").value;
  const cells = [];
  const body = rows.map(row => {
    const entries = cols.map(col => {
      const observed = count(row, col);
      const cell = { row: row.label, column: col.label, count: observed,
        rowPercentage: 100 * observed / row.total, columnPercentage: 100 * observed / col.total,
        totalPercentage: 100 * observed / n, row_variable: rowId, column_variable: columnId,
        complete_observations: n };
      cells.push(cell);
      if (display === "count") return `<td>${xtFmt(observed, 0)}</td>`;
      if (display === "count-row") return `<td>${xtFmt(observed, 0)} (${xtFmt(cell.rowPercentage)}%)</td>`;
      const percentage = display === "row" ? cell.rowPercentage
        : display === "column" ? cell.columnPercentage : cell.totalPercentage;
      return `<td>${xtFmt(percentage)}%</td>`;
    });
    return `<tr><td>${xtEsc(row.label)}</td>${entries.join("")}<td>${xtFmt(row.total, 0)}</td></tr>`;
  });

  document.getElementById("crosstab-title").textContent = `${rowId} by ${columnId}`;
  message.textContent = `Complete observations: ${xtFmt(n, 0)}. Missing either selected variable is excluded.`;
  document.getElementById("crosstab-results").innerHTML = `<div class="table-wrap"><table class="results-table">
    <thead><tr><th>${xtEsc(rowVar.displayName)}</th>${cols.map(c => `<th>${xtEsc(c.label)}</th>`).join("")}<th>Total</th></tr></thead>
    <tbody>${body.join("")}<tr><th>Total</th>${cols.map(c => `<th>${xtFmt(c.total, 0)}</th>`).join("")}<th>${xtFmt(n, 0)}</th></tr></tbody>
    </table></div>`;
  xtState.rows = cells;
  document.getElementById("crosstab-download").hidden = false;
}
function downloadXt(){if(!xtState.rows.length)return;const cols=Object.keys(xtState.rows[0]),q=v=>`"${String(v??"").replaceAll("\"","\"\"")}"`,text=[cols.join(","),...xtState.rows.map(row=>cols.map(c=>q(row[c])).join(","))].join("\n"),a=document.createElement("a");a.href=URL.createObjectURL(new Blob([text],{type:"text/csv"}));a.download="worldview-cross-tabulation.csv";a.click();URL.revokeObjectURL(a.href);}
function resetXt(){document.getElementById("crosstab-form").reset();document.getElementById("crosstab-row").value="Q165";document.getElementById("crosstab-column").value="Q260";[...document.getElementById("crosstab-countries").options].forEach(o=>o.selected=o.value==="NZL");document.getElementById("crosstab-results").innerHTML="";document.getElementById("crosstab-download").hidden=true;}
async function initialiseXt(){const loading=document.getElementById("crosstab-loading");try{const[data,codebook]=await Promise.all([fetch(xtPaths.data).then(r=>r.json()),fetch(xtPaths.codebook).then(r=>r.json())]);xtState.data=xtParse(data);xtState.variables=[{id:"COUNTRY",displayName:"Country",analysisType:"nominal"},...codebook.variables.filter(v=>v.analysisType!=="integer")];const pairs=new Map();for(let i=0;i<xtState.data.length;i++)pairs.set(xtState.data.columns.B_COUNTRY_ALPHA[i],xtState.data.columns.B_COUNTRY[i]);xtState.countries=[...pairs].map(([code,name])=>({code,name})).sort((a,b)=>a.name.localeCompare(b.name));const row=document.getElementById("crosstab-row"),column=document.getElementById("crosstab-column");xtState.variables.forEach(v=>{const label=v.id==="COUNTRY"?"Country":`${v.id}: ${v.displayName}`;row.add(new Option(label,v.id));column.add(new Option(label,v.id));});row.value="Q165";column.value="Q260";const countries=document.getElementById("crosstab-countries");xtState.countries.forEach(c=>{const option=new Option(`${c.name} (${c.code})`,c.code);if(c.code==="NZL")option.selected=true;countries.add(option);});loading.hidden=true;document.getElementById("crosstab-form").hidden=false;}catch(error){loading.textContent=`Cross-tabulation data could not be loaded: ${error.message}`;}}
document.addEventListener("DOMContentLoaded",()=>{document.getElementById("crosstab-form").addEventListener("submit",calculateXt);document.getElementById("crosstab-reset").addEventListener("click",resetXt);document.getElementById("crosstab-download").addEventListener("click",downloadXt);initialiseXt();});
