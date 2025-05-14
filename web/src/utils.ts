import { ChartOptions, Tick } from "chart.js/auto";

/**
 * Appends a new chart div using the given index to make a unique id.
 *
 * @param container The outer container.
 * @param idx The index, for creating unique ids.
 * @returns The new chart-div and inner canvase id.
 */
export function mkChartDiv(
  container: HTMLDivElement,
  idx: number,
): [HTMLDivElement, string] {
  // create canvas element
  const canvas = document.createElement("canvas");
  const canvasId = `canvas-${idx}`;
  canvas.setAttribute("id", canvasId);

  // create div
  const chartDiv = document.createElement("div");
  const chartDivId = `chart-div-${idx}`;
  chartDiv.setAttribute("id", chartDivId);
  chartDiv.setAttribute("class", "chart-div");
  if (idx != 0) {
    chartDiv.hidden = true;
  }
  chartDiv.appendChild(canvas);

  container.appendChild(chartDiv);
  return [chartDiv, canvasId];
}

/**
 * Adds a new chart selector option.
 *
 * @param selector The selector.
 * @param idx The new option index, for use in lookups.
 * @param title The displayed title (i.e. chart title).
 */
export function addChartSelectorOption(
  selector: HTMLSelectElement,
  idx: number,
  title: string,
): void {
  const selectOpt = document.createElement("option");
  selectOpt.setAttribute("value", `${idx}`);
  if (idx == 0) {
    selectOpt.append("selected");
  }
  selectOpt.innerHTML = title;
  selector.appendChild(selectOpt);
}

export function pad2(n: number): string {
  const prefix = n < 10 ? "0" : "";
  return `${prefix}${n}`;
}

export function format_seconds(value: number): string {
  const n = Number(value);

  const h = Math.floor(n / 3600);
  const m = Math.floor((n % 3600) / 60);
  const s = Math.floor(n % 60);

  const h_str = h > 0 ? `${h}h ` : "";
  const s_str = pad2(s);

  return `${h_str}${m}'${s_str}"`;
}

export function format_opts_seconds(
  value: string,
  index: number,
  labels: Tick[],
): string {
  return format_seconds(Number(value));
}
