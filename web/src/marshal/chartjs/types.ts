import { TooltipItem } from "chart.js/auto";
import { YAxisId, YAxisLabel } from "../common";

/**
 * This is very similar to chart.js's ChartOptions. Why do we have a distinct
 * type? For one, creating our own types makes it easy to see exactly what
 * data we want to end up setting vs. whatever else ChartOptions may have
 * on it that we do not care about. There are other differences too:
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
    customCanvasBackgroundColor: {
      color: string;
    };
    legend: {
      labels: {
        color: string;
      };
    };
    title: CTitle<string>;
    tooltip: {
      backgroundColor: string;
      bodyColor: string;
      titleColor: string;
    };
    zoom: {
      pan: {
        enabled: boolean;
        mode: "x" | "y" | "xy";
        modifier?: "ctrl" | "alt" | "shift" | "meta";
      };
      zoom: {
        drag: {
          backgroundColor: string;
          borderColor: string;
          borderWidth: number;
          enabled: boolean;
          modifier?: "ctrl" | "alt" | "shift" | "meta";
        };
        pinch: {
          enabled: true;
        };
        wheel: {
          enabled: true;
        };
        mode: "x" | "y" | "xy";
      };
    };
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
      title: CTitle<"datetime">;
      type: "timeseries";
    };
    y: CYOpt;
    y1?: CY1Opt;
  };
};

/**
 * Chart.js Y-axis.
 */
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
  yAxisID: YAxisId;
};

/**
 * The type for Y-axis data on the actual Chart.js chart.
 */
type CDataSets = CYAxis[];

type CYOpt = CYOptT<"left">;

type CY1Opt = CYOptT<"right">;

type CTicks = {
  color: string;
  callback?: (value: string, index: number, labels: any) => string;
};

/**
 * Chart options related to a Y axis.
 */
type CYOptT<A> = {
  grid: {
    color: string;
  };
  position: A;
  title: CTitle<YAxisLabel>;
  ticks?: CTicks;
};

/**
 * Chart title. The parameter allows for constraining the strings in some
 * scenarios e.g. labels.
 */
type CTitle<A> = {
  align?: "start" | "center" | "end";
  color: string;
  display: boolean;
  font: {
    size: number;
  };
  text: A;
};

export { CChartOpts, CDataSets, CYAxis, CYOptT, CTicks };
