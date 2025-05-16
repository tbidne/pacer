import * as charts from "../data/charts.json";
import { POutput, PChartExtra } from "./marshal/pacer";
import { create_chart } from "./marshal/chartjs";
import { mkChartDiv, addChartSelectorOption } from "./utils";

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
const charts_typed = charts as POutput;

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

    let title = chart.title;
    addChartSelectorOption(chartSelector, i, title);
    const result = mkChartDiv(chartContainer, i);
    const chartDiv = result[0];
    const elemId = result[1];
    handle_extra(chartDiv, i, chart.extra);

    chart_divs.set(i, [title, chartDiv]);

    create_chart(title, elemId, chart.datasets.xAxis, chart.datasets.yAxes);
  }

  if (charts_typed.length > 0) {
    addChartSelectorOnClick(chartSelector);
  }
}

main();
