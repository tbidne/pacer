import { TooltipItem } from "chart.js/auto";

/**
 * This file contains types sent from pacer i.e. the haskell backend.
 * There are some discrepancies e.g. some types contain an optional key
 * 'callbacks', when cannot come from the backend; these are set here
 * on the frontend. This enables us to share the type more easily.
 * 
 * Eventually, we will cleanly separate these so the backend sends the minimum
 * required data (i.e. no css / ui stuff), and the types exactly reflect that.
 * 
 * The key type is POutput.
 */

type POutput = PChart[];

/**
 * The core type sent by pacer backend.
 */
type PChart = {
  options: PChartOpts;
  datasets: {
    xAxis: string[];
    yAxes: PYAxis[];
  };
  extra: PChartExtra;
};

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
  pointHitRadius: number;
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

type PChartExtra = {
  description?: string;
};

type PYAxis = {
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
    callback?: (value: string, index: number, labels: any) => string;
    color?: string;
  };
};

type PTitle = {
  display: boolean;
  text: string;
};

export { POutput, PChart, PChartExtra, PChartOpts, PYAxis, PYOptT };
