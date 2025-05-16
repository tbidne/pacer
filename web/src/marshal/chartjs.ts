import { Chart } from "chart.js/auto";
import { PYAxis } from "./pacer";
import { CChartOpts, CDataSets, CYAxis, CYOptT } from "./chartjs/types";
import { YAxesT, YAxisLabel, mapYAxes } from "./common";
import { format_opts_seconds, format_seconds } from "../utils";

// TODO: At some point, it would be nice to figure out a css solution here
// e.g. so we can have light/dark options.
const GRAY_DARK = "#393939";
const GRAY_LIGHT = "#c3c3c3";
const GRAY_LIGHT_OPAC = "rgba(204, 204, 204, 0.75)";
const POINT_RADIUS = 20;
const AXIS_FONT_SIZE = 16;

function get_ytime_prefix(s: YAxisLabel): "duration" | "pace" | null {
  if (s == "time") return "duration";
  else if (s.startsWith("pace")) return "pace";
  else return null;
}

function from_pacer_yAxis(pYAxis: PYAxis): CYAxis {
  const cYAxis: CYAxis = {
    data: pYAxis.data,
    fill: false,
    label: pYAxis.type,
    pointHoverRadius: POINT_RADIUS,
    tension: 0,
    yAxisID: pYAxis.id,
  };

  const yTimePrefix = get_ytime_prefix(pYAxis.label);
  if (yTimePrefix != null) {
    cYAxis.tooltip = {
      callbacks: {
        // Previously we used formattedValue, which is an actual string,
        // but unfortunately it contains commas, which fails to parse as a
        // number. Instead we use raw, which seems to work, though the
        // safety is dubious.
        label: (item) =>
          `${yTimePrefix}: ${format_seconds(Number(item.raw as string))}`,
      },
    };
  }
  return cYAxis;
}

function from_pacer_yAxes(yAxes: YAxesT<PYAxis>): YAxesT<CYAxis> {
  return mapYAxes(from_pacer_yAxis, yAxes);
}

function from_pacer_opts(title: string, yAxes: YAxesT<PYAxis>): CChartOpts {
  type Ticks = {
    color: string;
    callback?: (value: string, index: number, labels: any) => string;
  };

  function make_ticks(yLabel: YAxisLabel) {
    const ticks: Ticks = {
      color: GRAY_LIGHT,
    };
    const yTimePrefix = get_ytime_prefix(yLabel);
    if (yTimePrefix != null) {
      ticks.callback = format_opts_seconds;
    }
    return ticks;
  }

  function make_yaxis<A>(position: A, title: YAxisLabel): CYOptT<A> {
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
      ticks: make_ticks(title),
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
      y: make_yaxis("left", yAxes.y.label),
    },
  };

  const y1 = yAxes.y1;
  if (y1 != null) {
    copts.scales.y1 = make_yaxis("right", y1.label);
  }
  return copts;
}

function create_chart(
  title: string,
  elem_id: string,
  xAxis: string[],
  yAxes: YAxesT<PYAxis>,
): void {
  const chart_opts = from_pacer_opts(title, yAxes);
  const cYAxes = from_pacer_yAxes(yAxes);

  const datasets: CDataSets = [cYAxes.y];
  if (cYAxes.y1 != null) {
    datasets.push(cYAxes.y1);
  }

  new Chart(elem_id, {
    type: "line",
    data: {
      labels: xAxis,
      datasets: datasets,
    },
    options: chart_opts,
  });
}

export { create_chart };
