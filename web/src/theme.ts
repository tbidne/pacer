import { Chart, ChartOptions } from "chart.js";

type ThemeConfig = {
  default?: ThemeKey;
  themes?: Theme[];
};

type Color = string;

const colorLiteralMap: Map<Color, ColorRgb> = new Map([
  ["white", { r: 255, g: 255, b: 255 }],
  ["silver", { r: 192, g: 192, b: 192 }],
  ["gray", { r: 128, g: 128, b: 128 }],
  ["grey", { r: 128, g: 128, b: 128 }],
  ["black", { r: 0, g: 0, b: 0 }],
  ["red", { r: 255, g: 0, b: 0 }],
  ["maroon", { r: 128, g: 0, b: 0 }],
  ["yellow", { r: 255, g: 255, b: 0 }],
  ["olive", { r: 128, g: 128, b: 0 }],
  ["lime", { r: 0, g: 255, b: 0 }],
  ["green", { r: 0, g: 128, b: 0 }],
  ["aqua", { r: 0, g: 255, b: 255 }],
  ["teal", { r: 0, g: 128, b: 128 }],
  ["blue", { r: 0, g: 0, b: 255 }],
  ["navy", { r: 0, g: 0, b: 128 }],
  ["fuscia", { r: 255, g: 0, b: 255 }],
  ["purple", { r: 128, g: 0, b: 128 }],
]);

// This should probably just be a function that takes an enum themeName
// and returns the theme.

type Theme = {
  background: Color;
  grid: Color;
  name: string;
  selectorBorder: Color;
  text: Color;
  tooltipBackground: Color;
  tooltip: Color;
  yBackground: Color;
  y1Background: Color;
  zoomDragBackground: Color;
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
      selectorBorder: "#495057",
      text: "#c3c3c3",
      tooltipBackground: "#2e3032",
      tooltip: "#c3c3c3",
      // NOTE: These are the default chartjs line colors, determined by
      // examining the values at runtime in setThemeKey.
      yBackground: "#36a2eb",
      y1Background: "#ff6384",
      zoomDragBackground: "#436dd0",
    },
  ],
  [
    "Dracula",
    {
      background: "#282a36",
      grid: "#5f6e9d",
      name: "Dark",
      selectorBorder: "#5f6e9d",
      text: "#9c7bcd",
      tooltipBackground: "#21222c",
      tooltip: "#898a8e",
      yBackground: "#568292",
      y1Background: "#b05d92",
      zoomDragBackground: "#9c7bcd",
    },
  ],
  [
    "Light",
    {
      background: "#e6e7ed",
      grid: "#828aad",
      name: "Light",
      selectorBorder: "#343b59",
      text: "#343b59",
      tooltipBackground: "#000000",
      tooltip: "#c3c3c3",
      yBackground: "#36a2eb",
      y1Background: "#ff6384",
      zoomDragBackground: "#436dd0",
    },
  ],
  [
    "Solarized Dark",
    {
      background: "#002b36",
      grid: "#218883",
      name: "Solarized Dark",
      selectorBorder: "#718688",
      text: "#718688",
      tooltipBackground: "#218883",
      tooltip: "#002b36",
      yBackground: "#1c72aa",
      y1Background: "#9e441d",
      zoomDragBackground: "#6e8609",
    },
  ],
  [
    "Tokyo Night",
    {
      background: "#1a1b26",
      grid: "#3a3c4e",
      name: "Tokyo Night",
      selectorBorder: "#3a3c4e",
      text: "#747894",
      tooltipBackground: "#14141b",
      tooltip: "#747894",
      yBackground: "#6482c6",
      y1Background: "#ad8fe6",
      zoomDragBackground: "#15596b",
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
  setGlobalStyleProp("--selector-border-color", theme.selectorBorder);
  setGlobalStyleProp("--text-color", theme.text);
}

type ColorRgb = { r: number; g: number; b: number };
function mkColorRgb(r: number, g: number, b: number): ColorRgb {
  return { r: r, g: g, b: b };
}

// The result of parsing a color string.
type ColorParse = { result: true; rgb: ColorRgb } | { result: false };

// Parses an rgb(a) string e.g. rgba(120, 30, 10, 0.4). Throws away the
// apha, if it exists
const rgbRegex =
  /rgba?\(([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*(?:,\s*([0-9.]+))?\)/;
function parseColorRgb(color: Color): ColorParse {
  const matches = color.match(rgbRegex);
  if (matches == null || matches.length < 4) {
    return { result: false };
  }
  const r = parseInt(matches[1], 10);
  const g = parseInt(matches[2], 10);
  const b = parseInt(matches[3], 10);
  if (isNaN(r) || isNaN(g) || isNaN(b)) {
    return { result: false };
  }

  return { result: true, rgb: mkColorRgb(r, g, b) };
}

// Parses a hex string.
const hexRegex = /#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})/;
function parseColorHex(color: Color): ColorParse {
  const matches = color.match(hexRegex);
  if (matches == null || matches.length < 4) {
    return { result: false };
  }
  const r = parseInt(matches[1], 16);
  const g = parseInt(matches[2], 16);
  const b = parseInt(matches[3], 16);

  if (isNaN(r) || isNaN(g) || isNaN(b)) {
    return { result: false };
  }

  return { result: true, rgb: mkColorRgb(r, g, b) };
}

function parseColorLiteral(color: Color): ColorParse {
  const lower = color.toLowerCase();

  const v = colorLiteralMap.get(color);
  if (!v) {
    return { result: false };
  }
  return { result: true, rgb: v };
}

// Parses a hex or rgb(a) string.
function parseColor(color: Color): ColorParse {
  const hexResult = parseColorHex(color);
  if (hexResult.result) {
    return hexResult;
  }
  const literalResult = parseColorLiteral(color);
  if (literalResult.result) {
    return literalResult;
  }
  return parseColorRgb(color);
}

// Given a color, returns a new rgba color with the desired alpha.
function newAlpha(color: Color, alpha: number): Color {
  if (alpha < 0 || alpha > 1) {
    throw new Error(`Invalid color alpha value: ${alpha}`);
  }

  const rgbResult = parseColor(color);
  if (!rgbResult.result) {
    console.warn(
      `newAlpha: color did not match expected hex, rgb(a), or literal format: ${color}`,
    );
    return color;
  }
  const rgbv = rgbResult.rgb;
  return `rgba(${rgbv.r}, ${rgbv.g}, ${rgbv.b}, ${alpha})`;
}

function alpha50(color: Color): Color {
  return newAlpha(color, 0.5);
}

function alpha75(color: Color): Color {
  return newAlpha(color, 0.75);
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
    plugins.tooltip.backgroundColor = alpha75(theme.tooltipBackground);
    plugins.tooltip.bodyColor = theme.tooltip;
    plugins.tooltip.titleColor = theme.tooltip;

    // update scales
    const scales = chart.options.scales;
    updateScale(scales.x);
    updateScale(scales.y);

    if (scales.y1 != null) {
      updateScale(scales.y1);
    }

    // update line colors
    const datasets = chart.data.datasets;
    const y0 = datasets[0];
    y0.backgroundColor = alpha50(theme.yBackground);
    y0.borderColor = theme.yBackground;

    if (datasets.length > 1) {
      const y1 = datasets[1];
      y1.backgroundColor = alpha50(theme.y1Background);
      y1.borderColor = theme.y1Background;
    }

    // update drag
    plugins.zoom.zoom.drag.backgroundColor = alpha50(theme.zoomDragBackground);
    plugins.zoom.zoom.drag.borderColor = theme.zoomDragBackground;

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

  const sortedThemes = new Map(
    [...allThemes.entries()].sort((a, b) => a[0].localeCompare(b[0])),
  );

  // populate selector.
  addThemesToUI(selected, themeSelector, sortedThemes);

  // set global css values.
  const selectedTheme = sortedThemes.get(selected);
  setGlobalStyleProps(selectedTheme);

  return [selected, sortedThemes, themeSelector];
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
  alpha50,
  alpha75,
  getThemeSelector,
  setTheme,
  setThemeKey,
  setup,
  themeMap,
};
