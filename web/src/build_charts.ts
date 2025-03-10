import { Chart, ChartOptions } from "chart.js/auto";
import * as charts from "../data/charts.json";
import { PChart, PChartExtra, PChartOpts, PYAxisElem, PYOptT } from "./types";
import { appendCanvasId, format_opts_seconds, format_seconds } from "./utils";

function get_ytime_prefix(s: string): "duration" | "pace" | null {
  if (s == "time") return "duration";
  else if (s.startsWith("pace")) return "pace";
  else return null;
}

function set_ytime_callbacks<A>(yAxisElem: PYAxisElem, yOpt: PYOptT<A>): void {
  const yType = yOpt.title.text;
  const yTimePrefix = get_ytime_prefix(yType);
  if (yTimePrefix != null) {
    yAxisElem.tooltip = {
      callbacks: {
        // Previously we used formattedValue, which is an actual string,
        // but unfortunately it contains commas, which fails to parse as a
        // number. Instead we use raw, which seems to work, though the
        // safety is dubious.
        label: (item) =>
          `${yTimePrefix}: ${format_seconds(Number(item.raw as string))}`,
      },
    };
    // Ticks object may be set on backend opts, so we need to ensure we do
    // not override it if it exists. We merely want to set the callback
    // field.
    const ticks = yOpt.ticks ? yOpt.ticks : {};
    ticks.callback = format_opts_seconds;
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

function handle_extra(id: number, extra: PChartExtra): void {
  const desc = extra.description;
  if (desc != null) {
    const container = document.getElementById("chart-container-id");

    const title = document.createElement("h2");
    const titleId = `desc-title-${id}`;
    title.setAttribute("id", titleId);
    title.innerHTML = "Description";

    const element = document.createElement("p");
    const descId = `desc-${id}`;
    element.setAttribute("id", descId);
    element.innerHTML = desc;

    container.appendChild(title);
    container.appendChild(element);
  }
}

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
const charts_typed = charts as PChart[];

function main() {
  for (var i = 0; i < charts_typed.length; i++) {
    const chart = charts_typed[i];

    const elemId = appendCanvasId(i);
    // TODO: Should make this more robus.
    handle_extra(i, chart.extra);

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
