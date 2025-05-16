import { TooltipItem } from "chart.js/auto";

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
type CChartOpts = {
  maintainAspectRatio: boolean;
  plugins: {
    legend: {
      labels: {
        color: string
      }
    };
    title: CTitle;
    tooltip: {
      backgroundColor: string;
      bodyColor: string;
      titleColor: string;
    }
  };
  pointHitRadius: number;
  responsive: boolean;
  scales: {
    x: {
      grid: {
        color: string;
      };
      ticks: {
        color: string;
      };
      time: {
        displayFormats: {
          day: "dd MMM yy";
        };
        unit: "day";
      };
      title: CTitle;
      type: "timeseries";
    };
    y: CYOpt;
    y1?: CY1Opt;
  };
};

type CYAxis = {
  data: number[];
  fill: boolean;
  label: string;
  pointHoverRadius: number;
  tension: number;
  tooltip?: {
    callbacks: {
      label: (item: TooltipItem<"line">) => string;
    };
  };
  yAxisID: "y" | "y1";
};

type CYOpt = CYOptT<"left">;

type CY1Opt = CYOptT<"right">;

type CYOptT<A> = {
  grid: {
    color: string;
  }
  position: A;
  title: CTitle;
  ticks?: {
    callback?: (value: string, index: number, labels: any) => string;
    color?: string;
  };
};

type CTitle = {
  align?: 'start' | 'center' | 'end';
  color: string;
  display: boolean;
  font: {
    size: number;
  };
  text: string;
}

export { CChartOpts, CYAxis, CYOptT };
