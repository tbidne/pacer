import { ChartOptions } from "chart.js/auto";

/**
 * Appends a new canvas element with the given id.
 */
export function appendCanvasId(canvasId: string): void {
  const container = document.getElementById("chart-container-id");
  const element = document.createElement("canvas");
  element.setAttribute("id", canvasId);
  container.appendChild(element);
}
