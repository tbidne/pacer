const GRAY_DARK = "#393939";
const GRAY_LIGHT = "#c3c3c3";
const GRAY_LIGHT_OPAC = "rgba(204, 204, 204, 0.75)";
const GRAY_DARK_OPAC = "rgba(0, 0, 0, 0.75)";

const BOOTSTRAP_DARK_BG = "#212529";

type Color = string;

// This should probably just be a function that takes an enum themeName
// and returns the theme.

type Theme = {
  bg: Color;
  grid: Color;
  labels: Color;
  name: "light" | "dark";
  tooltip_bg: Color;
  tooltip: Color;
};

type Themes = {
  dark: Theme;
  light: Theme;
};

const themes: Themes = {
  dark: {
    bg: BOOTSTRAP_DARK_BG,
    grid: GRAY_DARK,
    labels: GRAY_LIGHT,
    name: "dark",
    tooltip_bg: GRAY_LIGHT_OPAC,
    tooltip: GRAY_DARK,
  },
  light: {
    bg: "white",
    grid: GRAY_DARK,
    labels: GRAY_DARK,
    name: "light",
    tooltip_bg: GRAY_DARK_OPAC,
    tooltip: GRAY_LIGHT,
  },
};

export { Themes, Theme, themes };
