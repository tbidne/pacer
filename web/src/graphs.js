import Chart from 'chart.js/auto'
import {appendCanvasId, chartOptions} from './utils.js'

const data = [
  { year: 2010, count: 10 },
  { year: 2011, count: 20 },
  { year: 2012, count: 15 },
  { year: 2013, count: 25 },
  { year: 2014, count: 22 },
  { year: 2015, count: 30 },
  { year: 2016, count: 28 },
];

for (var i = 0; i <= 3; i++) {
  const elemId = `chart${i}`;
  appendCanvasId(elemId);
  new Chart(
    elemId,
    {
      type: 'line',
      data: {
        labels: data.map(row => row.year),
        datasets: [
          {
            label: `Acquisitions by year ${i}`,
            data: data.map(row => row.count)
          }
        ]
      },
      options: chartOptions
    }
  );
}
