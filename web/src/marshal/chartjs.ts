import { Chart } from "chart.js/auto";
import { PChartOpts, PYAxis, } from "./pacer";
import { CChartOpts, CYAxis, CYOptT } from "./chartjs/types";
import {
  format_opts_seconds,
  format_seconds,
} from "../utils";

function get_ytime_prefix(s: string): "duration" | "pace" | null {
  if (s == "time") return "duration";
  else if (s.startsWith("pace")) return "pace";
  else return null;
}

function from_pacer_yAxis(y_title: string, axis_id: "y" | "y1", pYAxis: PYAxis): CYAxis {
  const cYAxis: CYAxis = {
    data: pYAxis.data,
    fill: false,
    label: pYAxis.label,
    pointHoverRadius: 20,
    tension: 0,
    yAxisID: axis_id
  };

  const yTimePrefix = get_ytime_prefix(y_title);
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

function from_pacer_yAxes(opts: PChartOpts, yAxes: PYAxis[]): CYAxis[] {
  // from_pacer_yAxis requires a string title and the PYAxis. Unfortunately,
  // the title does not exist on the PYAxis; it is on the chart options.
  // When we move the title to the PYAxis we will be able to use this simpler
  // function.
  //
  //return yAxes.map (from_pacer_yAxis);

  const cAxes = [];

  if (yAxes.length < 1) {
    throw new Error("No y-axis elements, impossible!");
  }

  const y = opts.scales.y;
  const cYAxis = from_pacer_yAxis(y.title.text, "y", yAxes[0]);
  cAxes.push(cYAxis);

  const y1 = opts.scales.y1;
  if (y1 != null) {
    if (yAxes.length < 2) {
      throw new Error(
        "y1 exists on the options but not in the datasets, impossible!",
      );
    }
    const y1Axis = yAxes[1];
    const cY1Axis = from_pacer_yAxis(y1.title.text, "y1", y1Axis);
    cAxes.push(cY1Axis);
  }

  return cAxes;
}

function from_pacer_opts(pacer_opts: PChartOpts): CChartOpts {
  type Ticks = {
    color: string;
    callback?: (value: string, index: number, labels: any) => string;
  };

  function make_ticks(yType: string) {
    const ticks: Ticks = {
      color: "#c3c3c3"
    };
    const yTimePrefix = get_ytime_prefix(yType);
    if (yTimePrefix != null) {
      ticks.callback = format_opts_seconds;
    }
    return ticks;
  }

  function make_yaxis<A>(position: A, title: string): CYOptT<A> {
    return {
      grid: {
        color: "#393939"
      },
      position: position,
      title: {
        color: "#c3c3c3",
        display: true,
        font: {
          size: 16
        },
        text: title
      },
      ticks: make_ticks(title)

    }
  }

  const copts: CChartOpts = {
    maintainAspectRatio: pacer_opts.maintainAspectRatio,
    plugins: {
      legend: {
        labels: {
          color: "#c3c3c3"
        }
      },
      title: {
        align: "center",
        color: "#c3c3c3",
        display: false,
        font: {
          size: 24
        },
        text: pacer_opts.plugins.title.text
      },
      tooltip: {
        backgroundColor: "rgba(204, 204, 204, 0.75)",
        bodyColor: "black",
        titleColor: "black"
      }
    },
    pointHitRadius: pacer_opts.pointHitRadius,
    responsive: pacer_opts.responsive,
    scales: {
      x: {
        time: {
          displayFormats: {
            day: "dd MMM yy"
          },
          unit: "day",
        },
        grid: {
          color: "#393939"
        },
        ticks: {
          color: "#c3c3c3"
        },
        title: {
          color: "#c3c3c3",
          display: true,
          font: {
            "size": 16
          },
          text: "datetime"
        },
        type: "timeseries"
      },
      y: make_yaxis("left", pacer_opts.scales.y.title.text)
    }
  };

  const y1 = pacer_opts.scales.y1;
  if (y1 != null) {
    copts.scales.y1 = make_yaxis("right", y1.title.text);
  }
  return copts;
}

function create_chart(
  elem_id: string,
  pacer_opts: PChartOpts,
  xAxis: string[], yAxes: PYAxis[]): void {

  const chart_opts = from_pacer_opts(pacer_opts)
  const cYAxes = from_pacer_yAxes(pacer_opts, yAxes);

  new Chart(elem_id, {
    type: "line",
    data: {
      labels: xAxis,
      datasets: cYAxes,
    },
    options: chart_opts,
  });
}

export { create_chart };
