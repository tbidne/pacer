import { ChartOptions, Tick } from "chart.js/auto";

/**
 * Appends a new canvas element with the given id.
 */
export function appendCanvasId(canvasId: string): void {
  const container = document.getElementById("chart-container-id");
  const element = document.createElement("canvas");
  element.setAttribute("id", canvasId);
  container.appendChild(element);
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
