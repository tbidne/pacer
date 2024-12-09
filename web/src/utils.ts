import { ChartOptions } from "chart.js/auto";

/**
 * Appends a new canvas element with the given id.
*/
export function appendCanvasId(canvasId: string): void {
  const container = document.getElementById('chart-container-id');
  const element = document.createElement('canvas');
  element.setAttribute('id', canvasId)
  container.appendChild(element);
}

/**
 * Common options for a chart with one y axis.
 */
function mkChartOptions(
  title: string,
  y_label: string): ChartOptions<'line'> {
  return {
    plugins: {
      title: {
        display: true,
        text: title,
      }
    },
    responsive: true,
    maintainAspectRatio: false,
    scales: {
      x: {
        title: {
          display: true,
          text: 'datetime'
        }
      },
      y: {
        min: 0,
        position: 'left',
        title: {
          display: true,
          text: y_label
        }
      }
    }
  };
}

/**
 * Common options for a chart with two y axes.
 */
function mkChartOptions2Y(
  title: string,
  y_label: string,
  y1_label: string): ChartOptions<'line'> {
  const opts = mkChartOptions(title, y_label);

  opts.scales.y1 = {
    min: 0,
    position: 'right',
    title: {
      display: true,
      text: y1_label
    }
  };

  return opts;
}

export { mkChartOptions, mkChartOptions2Y };
