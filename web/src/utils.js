/**
 * Appends a new canvas element with the given id.
*/
export function appendCanvasId(canvasId) {
  const container = document.getElementById('chart-container-id');
  const element = document.createElement('canvas');
  element.setAttribute('id', canvasId)
  container.appendChild(element);
}

/**
 * Common options for charts.
 */
const chartOptions = {
  responsive: true,
  maintainAspectRatio: false,
  scales: {
    y: {
      min: 0
    }
  }
};

export { chartOptions };
