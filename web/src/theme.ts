import { Chart, ChartOptions } from "chart.js";

const GRAY_DARK = "#393939";
const GRAY_LIGHT = "#c3c3c3";
const GRAY_LIGHT_OPAC = "rgba(204, 204, 204, 0.75)";
const GRAY_DARK_OPAC = "rgba(0, 0, 0, 0.75)";

const BOOTSTRAP_DARK_BG = "#212529";

type Color = string;

// This should probably just be a function that takes an enum themeName
// and returns the theme.

type Theme = {
  background: Color;
  grid: Color;
  text: Color;
  name: "light" | "dark";
  tooltipBackground: Color;
  tooltip: Color;
};

type ThemeKey = "dark" | "light";

type ThemeMap = Map<ThemeKey, Theme>;

const themeMap: ThemeMap = new Map([
  [
    "dark",
    {
      background: "#191b1c",
      grid: "#3d3d3d",
      name: "dark",
      text: GRAY_LIGHT,
      tooltipBackground: GRAY_LIGHT_OPAC,
      tooltip: GRAY_DARK,
    },
  ],
  [
    "light",
    {
      background: "white",
      grid: GRAY_DARK,
      name: "light",
      text: GRAY_DARK,
      tooltipBackground: GRAY_DARK_OPAC,
      tooltip: GRAY_LIGHT,
    },
  ],
]);

type ChartElements = ChartElement[];

/**
 * This is our created chart, with optional extras. We hold this data for
 * later updating the theme.
 */
type ChartElement = {
  chart: Chart<"line">;
  extra?: ChartExtra;
};

type ChartExtra = [HTMLHeadingElement, HTMLParagraphElement];

function setThemeKey(
  themeName: ThemeKey,
  charts: ChartElements,
  themeMap: Map<ThemeKey, Theme>,
): void {
  const theme = themeMap.get(themeName);

  document.documentElement.style.setProperty(
    "--global-background-color",
    theme.background,
  );
  document.documentElement.style.setProperty("--grid-color", theme.grid);
  document.documentElement.style.setProperty("--text-color", theme.text);
  document.documentElement.style.setProperty("--tooltip-color", theme.tooltip);
  document.documentElement.style.setProperty(
    "--tooltip-background-color",
    theme.tooltipBackground,
  );

  function updateScale(scale: any): void {
    scale.grid.color = theme.grid;
    scale.ticks.color = theme.text;
    scale.title.color = theme.text;
  }

  // charts
  charts.map((chartData) => {
    const chart = chartData.chart;
    const opts: ChartOptions<"line"> = chart.options;
    const plugins = opts.plugins;

    plugins.legend.labels.color = theme.text;
    plugins.legend.title.color = theme.text;
    plugins.tooltip.backgroundColor = theme.tooltipBackground;
    plugins.tooltip.bodyColor = theme.tooltip;
    plugins.tooltip.titleColor = theme.tooltip;

    const scales = chart.options.scales;
    updateScale(scales.x);
    updateScale(scales.y);

    if (scales.y1 != null) {
      updateScale(scales.y1);
    }

    // ugh this doesn't work w/ the var.
    (<any>plugins).customCanvasBackgroundColor.color = theme.background;
    chart.update();
  });

  // Set UI theme. We use a css theme to control as much as we can, which
  // turns out to be non-chart elements (e.g. selectors, raw html).
  // Unfortunately, the chart's values don't seems to respond to css vars
  // well, and when we try to grab them from the UI, they're too "old"
  // e.g. we grab the previous value, not the new selection. For now, let's
  // just change chart data manually here, and live with the two sources
  // of truth.
  //
  // Note this might make custom themes difficult, e.g. we can easily use
  // custom color values here, but how do we add a custom css theme?
  const html = <HTMLHtmlElement>document.getElementById("html_head");
  html.setAttribute("ui-theme", theme.name);
}

function setTheme(
  selector: HTMLSelectElement,
  charts: ChartElements,
  themeMap: Map<ThemeKey, Theme>,
): void {
  setThemeKey(<ThemeKey>selector.value, charts, themeMap);
}

function addThemeBtnOnClick(
  charts: ChartElements,
  selector: HTMLSelectElement,
  themeMap: Map<ThemeKey, Theme>,
): void {
  selector.addEventListener("change", () =>
    setTheme(selector, charts, themeMap),
  );
}

function addThemesToUI(
  selected: ThemeKey,
  selector: HTMLSelectElement,
  themeMap: Map<ThemeKey, Theme>,
): void {
  themeMap.forEach((_, k) => {
    const selectOpt = document.createElement("option");
    selectOpt.setAttribute("value", k);

    if (k == selected) {
      selectOpt.selected = true;
    }
    selectOpt.innerHTML = k;

    selector.appendChild(selectOpt);
  });
}

function getThemeSelector(): HTMLSelectElement {
  return <HTMLSelectElement>document.getElementById("theme-selector");
}

// TODO: If we ever support custom themes, they will probably be included
// here as a param.
function setup(selected: ThemeKey): [ThemeMap, HTMLSelectElement] {
  const themeSelector = getThemeSelector();
  const allThemes = themeMap;
  addThemesToUI(selected, themeSelector, allThemes);

  return [allThemes, themeSelector];
}

export {
  ChartElement,
  ChartElements,
  Theme,
  ThemeKey,
  ThemeMap,
  addThemeBtnOnClick,
  addThemesToUI,
  getThemeSelector,
  setTheme,
  setThemeKey,
  setup,
  themeMap,
};
