import { Chart, ChartOptions } from "chart.js/auto";
import { Tick } from "chart.js/dist/core/core.scale";
import "chartjs-adapter-date-fns";
import * as charts from "../data/input/charts.json";
import { PChart, PChartOpts, PTitle, PYAxisElem, PYOptT } from "./types";
import { appendCanvasId } from "./utils";

function is_time(s: string): boolean {
  return s == "time" || s.startsWith("pace");
}

function set_ytime_callback<A>(yOpt: PYOptT<A>): void {
  const yType = yOpt.title.text;
  if (is_time(yType)) {
    yOpt.ticks = {
      callback: format_seconds,
    };
  }
}

function js_to_chartjs_opts(opts: PChartOpts): ChartOptions {
  set_ytime_callback(opts.scales.y);

  const y1 = opts.scales.y1;
  if (y1 != null) {
    set_ytime_callback(y1);
  }

  return opts;
}

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
const charts_typed = charts as PChart[];

function pad2(n: number): string {
  const prefix = n < 10 ? "0" : "";
  return `${prefix}${n}`;
}

function format_seconds(value: string, index: number, labels: Tick[]): string {
  const n = Number(value);

  const h = Math.floor(n / 3600);
  const m = Math.floor((n % 3600) / 60);
  const s = Math.floor(n % 60);

  const h_str = h > 0 ? `${h}h ` : "";
  const s_str = pad2(s);

  return `${h_str}${m}'${s_str}"`;
}

for (var i = 0; i < charts_typed.length; i++) {
  const elemId = `chart${i}`;
  appendCanvasId(elemId);

  const chart = charts_typed[i];

  const opts = js_to_chartjs_opts(chart.options);

  new Chart(elemId, {
    type: "line",
    data: {
      labels: chart.datasets.xAxis,
      datasets: chart.datasets.yAxes,
    },
    options: opts,
  });
}
