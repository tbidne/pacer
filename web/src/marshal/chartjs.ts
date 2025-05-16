import { Chart } from "chart.js/auto";
import { PYAxis } from "./pacer";
import { CChartOpts, CDataSets, CTicks, CYAxis, CYOptT } from "./chartjs/types";
import { YAxesT, YAxisLabel, mapYAxes } from "./common";
import { formatOptsSeconds, formatSeconds } from "../utils";

// TODO: At some point, it would be nice to figure out a css solution here
// e.g. so we can have light/dark options.
const GRAY_DARK = "#393939";
const GRAY_LIGHT = "#c3c3c3";
const GRAY_LIGHT_OPAC = "rgba(204, 204, 204, 0.75)";
const POINT_RADIUS = 20;
const AXIS_FONT_SIZE = 16;

function getYTimePrefix(s: YAxisLabel): "duration" | "pace" | null {
  if (s == "time") return "duration";
  else if (s.startsWith("pace")) return "pace";
  else return null;
}

function makeYAxis(pYAxis: PYAxis): CYAxis {
  const cYAxis: CYAxis = {
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

function makeYAxes(yAxes: YAxesT<PYAxis>): YAxesT<CYAxis> {
  return mapYAxes(makeYAxis, yAxes);
}

function makeChartOpts(title: string, yAxes: YAxesT<PYAxis>): CChartOpts {
  function makeTicks(yLabel: YAxisLabel): CTicks {
    const ticks: CTicks = {
      color: GRAY_LIGHT,
    };
    const yTimePrefix = getYTimePrefix(yLabel);
    if (yTimePrefix != null) {
      ticks.callback = formatOptsSeconds;
    }
    return ticks;
  }

  function makeYAxis<A>(position: A, title: YAxisLabel): CYOptT<A> {
    return {
      grid: {
        color: GRAY_DARK,
      },
      position: position,
      title: {
        color: GRAY_LIGHT,
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
      legend: {
        labels: {
          color: GRAY_LIGHT,
        },
      },
      title: {
        align: "center",
        color: GRAY_LIGHT,
        // Disabled because the title is in the selector. Re-enable if we
        // ever change up the UI e.g. put the selector somewhere else.
        display: false,
        font: {
          size: 24,
        },
        text: title,
      },
      tooltip: {
        backgroundColor: GRAY_LIGHT_OPAC,
        bodyColor: "black",
        titleColor: "black",
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
          color: GRAY_DARK,
        },
        ticks: {
          color: GRAY_LIGHT,
        },
        title: {
          color: GRAY_LIGHT,
          display: true,
          font: {
            size: AXIS_FONT_SIZE,
          },
          text: "datetime",
        },
        type: "timeseries",
      },
      y: makeYAxis("left", yAxes.y.label),
    },
  };

  const y1 = yAxes.y1;
  if (y1 != null) {
    copts.scales.y1 = makeYAxis("right", y1.label);
  }
  return copts;
}

/**
 * Creates a chart.
 * @param title The title of the chart.
 * @param elem_id The html id for the chart e.g. canvas-1.
 * @param xAxis X-axis data.
 * @param yAxes Y-axes data.
 */
function createChart(
  title: string,
  elemId: string,
  xAxis: string[],
  yAxes: YAxesT<PYAxis>,
): void {
  const chartOpts = makeChartOpts(title, yAxes);
  const cYAxes = makeYAxes(yAxes);

  const datasets: CDataSets = [cYAxes.y];
  if (cYAxes.y1 != null) {
    datasets.push(cYAxes.y1);
  }

  new Chart(elemId, {
    type: "line",
    data: {
      labels: xAxis,
      datasets: datasets,
    },
    options: chartOpts,
  });
}

export { createChart };
