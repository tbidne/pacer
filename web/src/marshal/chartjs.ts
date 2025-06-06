import { Chart } from "chart.js/auto";
import { PYAxis } from "./pacer";
import { CChartOpts, CDataSets, CTicks, CYAxis, CYOptT } from "./chartjs/types";
import { YAxesT, YAxisLabel, mapYAxes } from "./common";
import { formatOptsSeconds, formatSeconds } from "../utils";
import { alpha50, alpha75, Theme } from "../theme";

const POINT_RADIUS = 20;
const AXIS_FONT_SIZE = 16;

function getYTimePrefix(s: YAxisLabel): "duration" | "pace" | null {
  if (s == "time") return "duration";
  else if (s.startsWith("pace")) return "pace";
  else return null;
}

function makeYAxis(theme: Theme, pYAxis: PYAxis): CYAxis {
  let backgroundColor = alpha50(theme.yBackground);
  let borderColor = theme.yBackground;
  if (pYAxis.id == "y1") {
    backgroundColor = alpha50(theme.y1Background);
    borderColor = theme.y1Background;
  }

  const cYAxis: CYAxis = {
    backgroundColor: backgroundColor,
    borderColor: borderColor,
    data: pYAxis.data,
    fill: false,
    label: pYAxis.type,
    pointHoverRadius: POINT_RADIUS,
    tension: 0,
    yAxisID: pYAxis.id,
  };

  const yTimePrefix = getYTimePrefix(pYAxis.label);
  if (yTimePrefix != null) {
    cYAxis.tooltip = {
      callbacks: {
        // Previously we used formattedValue, which is an actual string,
        // but unfortunately it contains commas, which fails to parse as a
        // number. Instead we use raw, which seems to work, though the
        // safety is dubious.
        label: (item) =>
          `${yTimePrefix}: ${formatSeconds(Number(item.raw as string))}`,
      },
    };
  }
  return cYAxis;
}

function makeYAxes(theme: Theme, yAxes: YAxesT<PYAxis>): YAxesT<CYAxis> {
  return mapYAxes((y) => makeYAxis(theme, y), yAxes);
}

function makeChartOpts(
  theme: Theme,
  title: string,
  yAxes: YAxesT<PYAxis>,
): CChartOpts {
  function makeTicks(yLabel: YAxisLabel): CTicks {
    const ticks: CTicks = {
      color: theme.text,
    };
    const yTimePrefix = getYTimePrefix(yLabel);
    if (yTimePrefix != null) {
      ticks.callback = formatOptsSeconds;
    }
    return ticks;
  }

  function makeYAxisOpts<A>(position: A, title: YAxisLabel): CYOptT<A> {
    return {
      grid: {
        color: theme.grid,
      },
      position: position,
      title: {
        color: theme.text,
        display: true,
        font: {
          size: AXIS_FONT_SIZE,
        },
        text: title,
      },
      ticks: makeTicks(title),
    };
  }

  const copts: CChartOpts = {
    maintainAspectRatio: false,
    plugins: {
      // see NOTE: [Background Color]
      customCanvasBackgroundColor: {
        color: theme.background,
      },
      legend: {
        labels: {
          color: theme.text,
        },
      },
      title: {
        align: "center",
        color: theme.text,
        // Disabled because the title is in the selector. Re-enable if we
        // ever change up the UI e.g. put the selector somewhere else.
        display: false,
        font: {
          size: 24,
        },
        text: title,
      },
      tooltip: {
        backgroundColor: alpha75(theme.tooltipBackground),
        bodyColor: theme.tooltip,
        titleColor: theme.tooltip,
      },
      zoom: {
        // Disable pan for now. While panning would be nice, I cannot get it
        // to play nicely with drag. Moreover, pan doesn't work too well
        // with our axes, even by itself.
        //
        // Therefore we give up on it for now. "Panning" can be substituted
        // by drag + zoom.
        pan: {
          enabled: false,
          mode: "x",
          modifier: "ctrl",
        },
        zoom: {
          drag: {
            backgroundColor: alpha50(theme.zoomDragBackground),
            borderColor: theme.zoomDragBackground,
            borderWidth: 1,
            enabled: true,
          },
          pinch: {
            enabled: true,
          },
          wheel: {
            enabled: true,
          },
          mode: "x",
        },
      },
    },
    pointHitRadius: POINT_RADIUS,
    responsive: true,
    scales: {
      x: {
        time: {
          displayFormats: {
            day: "dd MMM yy",
          },
          unit: "day",
        },
        grid: {
          color: theme.grid,
        },
        ticks: {
          color: theme.text,
        },
        title: {
          color: theme.text,
          display: true,
          font: {
            size: AXIS_FONT_SIZE,
          },
          text: "datetime",
        },
        type: "timeseries",
      },
      y: makeYAxisOpts("left", yAxes.y.label),
    },
  };

  const y1 = yAxes.y1;
  if (y1 != null) {
    copts.scales.y1 = makeYAxisOpts("right", y1.label);
  }
  return copts;
}

// NOTE: [Background Color]
//
// This is required to set the background color when saving charts.
//
// https://www.chartjs.org/docs/latest/configuration/canvas-background.html
const bg_plugin = {
  id: "customCanvasBackgroundColor",
  beforeDraw: (chart: any, args: any, options: any) => {
    const { ctx } = chart;
    ctx.save();
    ctx.globalCompositeOperation = "destination-over";
    ctx.fillStyle = options.color || "#99ffff";
    ctx.fillRect(0, 0, chart.width, chart.height);
    ctx.restore();
  },
};

/**
 * Creates a chart.
 * @param title The title of the chart.
 * @param elem_id The html id for the chart e.g. canvas-1.
 * @param xAxis X-axis data.
 * @param yAxes Y-axes data.
 * @returns The created chart.
 */
function createChart(
  theme: Theme,
  title: string,
  elemId: string,
  xAxis: string[],
  yAxes: YAxesT<PYAxis>,
): Chart<"line"> {
  const chartOpts = makeChartOpts(theme, title, yAxes);
  const cYAxes = makeYAxes(theme, yAxes);

  const datasets: CDataSets = [cYAxes.y];
  if (cYAxes.y1 != null) {
    datasets.push(cYAxes.y1);
  }

  return new Chart<"line">(elemId, {
    type: "line",
    data: {
      labels: xAxis,
      datasets: datasets,
    },
    options: chartOpts,
    plugins: [bg_plugin],
  });
}

export { createChart };
