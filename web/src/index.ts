import { Chart } from "chart.js";
import zoomPlugin from "chartjs-plugin-zoom";
import "chartjs-adapter-date-fns";
import "./style.css";
import "./build_charts";

Chart.register(zoomPlugin);
