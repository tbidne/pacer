import { Chart, ChartOptions } from "chart.js";

type ThemeConfig = {
  default?: ThemeKey;
  themes?: Theme[];
};

type Color = string;

// This should probably just be a function that takes an enum themeName
// and returns the theme.

type Theme = {
  background: Color;
  grid: Color;
  name: "Light" | "Dark";
  selectorBorderColor: Color;
  text: Color;
  tooltipBackground: Color;
  tooltip: Color;
};

type ThemeKey = string;

type ThemeMap = Map<string, Theme>;

const themeMap: ThemeMap = new Map([
  [
    "Dark",
    {
      background: "#191b1c",
      grid: "#3d3d3d",
      name: "Dark",
      selectorBorderColor: "#495057",
      text: "#c3c3c3",
      tooltipBackground: "rgba(204, 204, 204, 0.75)",
      tooltip: "#393939",
    },
  ],
  [
    "Light",
    {
      background: "#e6e7ed",
      grid: "#828aad",
      name: "Light",
      selectorBorderColor: "#343b59",
      text: "#343b59",
      tooltipBackground: "rgba(0, 0, 0, 0.75)",
      tooltip: "#c3c3c3",
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

function setGlobalStyleProp(value: string, color: Color): void {
  document.documentElement.style.setProperty(value, color);
}

function setGlobalStyleProps(theme: Theme): void {
  setGlobalStyleProp("--global-background-color", theme.background);
  setGlobalStyleProp("--selector-border-color", theme.selectorBorderColor);
  setGlobalStyleProp("--text-color", theme.text);
}

function setThemeKey(
  themeName: ThemeKey,
  charts: ChartElements,
  themeMap: Map<ThemeKey, Theme>,
): void {
  const theme = themeMap.get(themeName);

  // Non-canvas value are controlled by css vars, so we need to update them
  // based on the selected theme.
  setGlobalStyleProps(theme);

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

    (<any>plugins).customCanvasBackgroundColor.color = theme.background;
    chart.update();
  });
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

function initialThemeKey(config: ThemeConfig): ThemeKey {
  if (config != null && config.default != null) {
    return config.default;
  } else {
    return "Dark";
  }
}

function userThemes(config: ThemeConfig): Theme[] {
  if (config != null && config.themes != null) {
    return config.themes;
  } else {
    return [];
  }
}

function setup(config: ThemeConfig): [ThemeKey, ThemeMap, HTMLSelectElement] {
  const themeSelector = getThemeSelector();
  const allThemes = themeMap;

  // add user themes.
  userThemes(config).forEach((t) => {
    allThemes.set(t.name, t);
  });

  // get selected.
  const selected = initialThemeKey(config);

  // populate selector.
  addThemesToUI(selected, themeSelector, allThemes);

  // set global css values.
  const selectedTheme = allThemes.get(selected);
  setGlobalStyleProps(selectedTheme);

  return [selected, allThemes, themeSelector];
}

export {
  ChartElement,
  ChartElements,
  Theme,
  ThemeConfig,
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
