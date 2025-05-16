import { Chart } from "chart.js/auto";
import { PYAxis } from "./pacer";
import { CChartOpts, CDataSets, CYAxis, CYOptT } from "./chartjs/types";
import { YAxesT, YAxisLabel, YAxisType, mapYAxes } from "./common";
import { format_opts_seconds, format_seconds } from "../utils";

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
    pointHoverRadius: 20,
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
      color: "#c3c3c3",
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
        color: "#393939",
      },
      position: position,
      title: {
        color: "#c3c3c3",
        display: true,
        font: {
          size: 16,
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
          color: "#c3c3c3",
        },
      },
      title: {
        align: "center",
        color: "#c3c3c3",
        display: false,
        font: {
          size: 24,
        },
        text: title,
      },
      tooltip: {
        backgroundColor: "rgba(204, 204, 204, 0.75)",
        bodyColor: "black",
        titleColor: "black",
      },
    },
    pointHitRadius: 20,
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
          color: "#393939",
        },
        ticks: {
          color: "#c3c3c3",
        },
        title: {
          color: "#c3c3c3",
          display: true,
          font: {
            size: 16,
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
