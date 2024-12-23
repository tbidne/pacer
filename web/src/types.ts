import { LineControllerChartOptions, TooltipItem } from "chart.js/auto";

/**
 * This is very similar to chart.js's ChartOptions. Why do we have a distinct
 * type? The json the backend returns isn't __quite__ the same as the Chart
 * that chart.json uses. For example
 *
 * - The json does not have a callback field, as we cannot currently serialize
 *   js functions, hence we do it here.
 *
 * - The secondary y1 axis causes type inference problems without explicit
 *   typing.
 *
 * - We can be overly specific here (e.g. require exact displayFormat) whereas
 *   chart's ChartOptions is more general.
 */
type PChartOpts = {
  maintainAspectRatio: boolean;
  plugins: {
    title: PTitle;
  };
  responsive: boolean;
  scales: {
    x: {
      time: {
        displayFormats: {
          day: "dd MMM yy";
        };
        unit: "day";
      };
      title: PTitle;
      type: "timeseries";
    };
    y: PYOpt;
    y1?: PY1Opt;
  };
};

type PChart = {
  options: PChartOpts;
  datasets: {
    xAxis: string[];
    yAxes: PYAxisElem[];
  };
};

type PTitle = {
  display: boolean;
  text: string;
};

type PYAxisElem = {
  data: number[];
  fill: boolean;
  label: string;
  tension: number;
  tooltip?: {
    callbacks: {
      label: (item: TooltipItem<"line">) => string;
    };
  };
};

type PYOpt = PYOptT<"left">;

type PY1Opt = PYOptT<"right">;

type PYOptT<A> = {
  position: A;
  title: PTitle;
  ticks?: {
    callback: (value: string, index: number, labels: any) => string;
  };
};

export { PChart, PChartOpts, PTitle, PYAxisElem, PYOptT };
