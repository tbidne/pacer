import { PCharts, PChartExtra } from "./marshal/pacer";
import * as ModChartJs from "./marshal/chartjs";
import * as ModUtils from "./utils";
import { ChartElement, ChartElements } from "./theme";
import { ThemeConfig } from "./theme";
import * as ModTheme from "./theme";

async function getChartsJson(): Promise<PCharts> {
  const response = await fetch("/api/charts");
  return await response.json();
}

function handleExtra(
  chartDiv: HTMLDivElement,
  id: number,
  extra: PChartExtra,
): [HTMLHeadingElement, HTMLParagraphElement] | null {
  const descTxt: string = extra.description;
  if (descTxt != null) {
    const title = <HTMLHeadingElement>document.createElement("h4");
    const titleId = `description-title-${id}`;
    title.setAttribute("id", titleId);
    title.setAttribute("class", "description-title");
    title.innerHTML = "Description";

    const description = <HTMLParagraphElement>document.createElement("p");
    const descId = `description-${id}`;
    description.setAttribute("id", descId);
    description.setAttribute("class", "description");
    description.innerHTML = descTxt;

    chartDiv.appendChild(title);
    chartDiv.appendChild(description);
    return [title, description];
  }
  return null;
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
    const prevElem = chartDivs.get(prevIdx);
    const prevChartDiv = prevElem[1];
    prevChartDiv.hidden = true;

    const newElem = chartDivs.get(newIdx);
    const newTitle = newElem[0];
    const newDiv = newElem[1];
    newDiv.hidden = false;
    prevIdx = newIdx;

    selector.setAttribute("title", newTitle);
  }
  selector.addEventListener("click", onSelect);
}

/**
 * Map from index to chart div. We use this to change the chart visibility,
 * when a new option is selected.
 *
 * The first element is the div's chart's title, so we can set the select
 * option's hover.
 */
const chartDivs: Map<number, [string, HTMLDivElement]> = new Map([]);

function main(pacerCharts: PCharts) {
  // setup themes
  let themeConfig: ThemeConfig = null;
  if (pacerCharts.theme != null) {
    themeConfig = pacerCharts.theme;
  }

  const themeResult = ModTheme.setup(themeConfig);
  const initThemeName = themeResult[0];
  const allThemes = themeResult[1];
  const themeSelector = themeResult[2];
  const initTheme = allThemes.get(initThemeName);

  const chartContainer = getChartContainer();
  const chartSelector = getChartSelector();

  const pcharts = pacerCharts.charts;

  const allChartData: ChartElements = [];

  for (var i = 0; i < pcharts.length; i++) {
    const pchart = pcharts[i];

    let title = pchart.title;
    ModUtils.addChartSelectorOption(chartSelector, i, title);
    const result = ModUtils.mkChartDiv(chartContainer, i);
    const chartDiv = result[0];
    const elemId = result[1];

    chartDivs.set(i, [title, chartDiv]);

    const extra = handleExtra(chartDiv, i, pchart.extra);
    const chart = ModChartJs.createChart(
      initTheme,
      title,
      elemId,
      pchart.extra,
      pchart.datasets.xAxis,
      pchart.datasets.yAxes,
    );

    const chartData: ChartElement = {
      chart: chart,
    };

    if (extra != null) {
      chartData.extra = extra;
    }
    allChartData.push(chartData);
  }

  if (pcharts.length > 0) {
    addChartSelectorOnClick(chartSelector);
    ModTheme.addThemeBtnOnClick(allChartData, themeSelector, allThemes);
  }
}

getChartsJson().then(main);
