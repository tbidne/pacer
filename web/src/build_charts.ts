import { Chart, ChartOptions } from "chart.js/auto";
import "chartjs-adapter-date-fns";
import * as charts from "../data/input/charts.json";
import { appendCanvasId } from "./utils";

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
type chart_type = {
  options: ChartOptions;
  datasets: {
    xAxis: string[];
    yAxes: y_type[];
  };
};

type y_type = {
  label: string;
  data: number[];
  fill: boolean;
  tension: number;
};

const charts_typed = charts as chart_type[];

for (var i = 0; i <= charts_typed.length; i++) {
  const elemId = `chart${i}`;
  appendCanvasId(elemId);

  const chart = charts_typed[i];

  new Chart(elemId, {
    type: "line",
    data: {
      labels: chart.datasets.xAxis,
      datasets: chart.datasets.yAxes,
    },
    options: chart.options,
  });
}
