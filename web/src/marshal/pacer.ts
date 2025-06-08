import { YAxesT, YAxisId, YAxisLabel, YAxisType } from "./common";
import { ThemeConfig } from "../theme";

/**
 * This file contains types sent from pacer i.e. the haskell backend.
 * There are some discrepancies e.g. some types contain an optional key
 * 'callbacks', when cannot come from the backend; these are set here
 * on the frontend. This enables us to share the type more easily.
 *
 * Eventually, we will cleanly separate these so the backend sends the minimum
 * required data (i.e. no css / ui stuff), and the types exactly reflect that.
 *
 * The key type is PCharts.
 */
type PCharts = {
  charts: PChart[];
  theme?: ThemeConfig;
};

/**
 * The core type sent by pacer backend.
 */
type PChart = {
  datasets: {
    xAxis: string[];
    yAxes: YAxesT<PYAxis>;
  };
  extra: PChartExtra;
  title: string;
};

type PChartExtra = {
  description?: string;
  smoothCurve?: number;
};

type PYAxis = {
  data: number[];
  id: YAxisId;
  label: YAxisLabel;
  type: YAxisType;
};

export { PCharts, PChart, PChartExtra, PYAxis };
