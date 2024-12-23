import { Chart, ChartOptions } from "chart.js/auto";
import "chartjs-adapter-date-fns";
import * as charts from "../data/input/charts.json";
import { PChart, PChartOpts, PYAxisElem, PYOptT } from "./types";
import { appendCanvasId, format_opts_seconds, format_seconds } from "./utils";

function is_time(s: string): boolean {
  return s == "time" || s.startsWith("pace");
}

function set_ytime_callbacks<A>(yAxisElem: PYAxisElem, yOpt: PYOptT<A>): void {
  const yType = yOpt.title.text;
  if (is_time(yType)) {
    yAxisElem.tooltip = {
      callbacks: {
        // Previously we used formattedValue, which is an actual string,
        // but unfortunately it contains commas, which fails to parse as a
        // number. Instead we use raw, which seems to work, though the
        // safety is dubious.
        label: (item) => format_seconds(Number(item.raw as string)),
      },
    };
    yOpt.ticks = {
      callback: format_opts_seconds,
    };
  }
}

function js_to_chartjs_opts(
  yAxisElems: PYAxisElem[],
  opts: PChartOpts,
): ChartOptions {
  if (yAxisElems.length < 1) {
    throw new Error("No y-axis elements, impossible!");
  }
  const yAxisElem = yAxisElems[0];
  set_ytime_callbacks(yAxisElem, opts.scales.y);

  const y1 = opts.scales.y1;
  if (y1 != null) {
    if (yAxisElems.length < 2) {
      throw new Error(
        "y1 exists on the options but not in the datasets, impossible!",
      );
    }
    const y1AxisElem = yAxisElems[1];
    set_ytime_callbacks(y1AxisElem, y1);
  }

  return opts;
}

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
const charts_typed = charts as PChart[];

function main() {
  for (var i = 0; i < charts_typed.length; i++) {
    const elemId = `chart${i}`;
    appendCanvasId(elemId);

    const chart = charts_typed[i];

    // The original value is modified, so the new assignment is mostly
    // unnecessary. The only reason we do so is the cast to the final type
    // ChartOptions, maybe it catches some errors more quickly?
    const opts = js_to_chartjs_opts(chart.datasets.yAxes, chart.options);

    new Chart(elemId, {
      type: "line",
      data: {
        labels: chart.datasets.xAxis,
        datasets: chart.datasets.yAxes,
      },
      options: opts,
    });
  }
}

main();
