import { Chart, ChartOptions } from "chart.js/auto";
import * as charts from "../data/charts.json";
import { PChart, PChartExtra, PChartOpts, PYAxisElem, PYOptT } from "./types";
import {
  mkChartDiv,
  addChartSelectorOption,
  format_opts_seconds,
  format_seconds,
} from "./utils";

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

function handle_extra(
  chartDiv: HTMLDivElement,
  id: number,
  extra: PChartExtra,
): void {
  const descTxt: string = extra.description;
  if (descTxt != null) {
    const title = document.createElement("h4");
    const titleId = `description-title-${id}`;
    title.setAttribute("id", titleId);
    title.setAttribute("class", "description-title");
    title.innerHTML = "Description";

    const description = document.createElement("p");
    const descId = `description-${id}`;
    description.setAttribute("id", descId);
    description.setAttribute("class", "description");
    description.innerHTML = descTxt;

    chartDiv.appendChild(title);
    chartDiv.appendChild(description);
  }
}

function getChartSelector(): HTMLSelectElement {
  return <HTMLSelectElement>document.getElementById("chart-selector");
}

function getChartContainer(): HTMLDivElement {
  return <HTMLDivElement>document.getElementById("chart-container-id");
}

function addChartSelectorOnClick(selector: HTMLSelectElement): void {
  let prevIdx: number = 0;
  function onSelect(): void {
    const newIdx = +selector.value;

    // Previously we retrieved these with
    // getElementById(`chart-div-${prevIdx}`). But using a map instead
    // feels slightly nicer.
    const prevElem = chart_divs.get(prevIdx);
    const prevChartDiv = prevElem[1];
    prevChartDiv.hidden = true;

    const newElem = chart_divs.get(newIdx);
    const newTitle = newElem[0];
    const newDiv = newElem[1];
    newDiv.hidden = false;
    prevIdx = newIdx;

    selector.setAttribute("title", newTitle);
  }
  selector.addEventListener("click", onSelect);
}

/**
 * We widen charts to charts_typed since if charts.json does not have any
 * charts with a y1 axis, ts will infer the y1 prop does not exist,
 * hence the y1 access will fail.
 */
const charts_typed = charts as PChart[];

/**
 * Map from index to chart div. We use this to change the chart visibility,
 * when a new option is selected.
 *
 * The first element is the div's chart's title, so we can set the select
 * option's hover.
 */
const chart_divs: Map<number, [string, HTMLDivElement]> = new Map([]);

function main() {
  const chartContainer = getChartContainer();
  const chartSelector = getChartSelector();

  for (var i = 0; i < charts_typed.length; i++) {
    const chart = charts_typed[i];

    // TODO: Should make this more robust.
    let title = chart.options.plugins.title.text;
    addChartSelectorOption(chartSelector, i, title);
    const result = mkChartDiv(chartContainer, i);
    const chartDiv = result[0];
    const elemId = result[1];
    handle_extra(chartDiv, i, chart.extra);

    chart_divs.set(i, [title, chartDiv]);

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

  if (charts_typed.length > 0) {
    addChartSelectorOnClick(chartSelector);
  }
}

main();
