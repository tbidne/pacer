import { Chart, ChartOptions } from "chart.js/auto";
import * as charts from "../data/input/charts.json";
import { appendCanvasId, mkChartOptions, mkChartOptions2Y } from "./utils";

function mkDataSet(x: { values: number[]; label: string }) {
  return {
    label: x.label,
    data: x.values,
    fill: false,
    tension: 0,
  };
}

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
const charts_typed = charts as {
  data: {
    x: string[];
    y: {
      label: string;
      values: number[];
    };
    y1: {
      label: string;
      values: number[];
    };
  };
  title: string;
}[];

for (var i = 0; i <= charts_typed.length; i++) {
  const elemId = `chart${i}`;
  appendCanvasId(elemId);

  const chart = charts_typed[i];
  const title = chart.title;

  let opts: ChartOptions<"line"> = null;
  const datasets = [mkDataSet(chart.data.y)];
  if (chart.data.y1 != undefined) {
    datasets[1] = mkDataSet(chart.data.y1);
    opts = mkChartOptions2Y(title, chart.data.y.label, chart.data.y1.label);
  } else {
    opts = mkChartOptions(title, chart.data.y.label);
  }

  new Chart(elemId, {
    type: "line",
    data: {
      labels: chart.data.x,
      datasets: datasets,
    },
    options: opts,
  });
}
