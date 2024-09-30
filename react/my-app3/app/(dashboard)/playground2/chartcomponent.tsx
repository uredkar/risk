import React, { useState, useEffect } from 'react';
import { getChart } from '../../services/api';  // Import the API service
const ChartComponent = () => {
  
  const [chartUrl, setChartUrl] = useState<string | null>(null);
  const [xValues, setXValues] = useState<number[]>([1, 2, 3, 4]);  // Example x values
  const [yValues, setYValues] = useState<number[]>([10, 20, 30, 40]);  // Example y values

  useEffect(() => {
    const fetchChart = async () => {
      const url = await getChart(xValues, yValues);  // Call the centralized API service
      setChartUrl(url);
    };

    fetchChart();
  }, [xValues, yValues]);  // Fetch chart whenever xValues or yValues change


  return (
    <div>
      <h1>Chart Example2</h1>
    <div>
      {chartUrl ? <img src={chartUrl} alt="Generated Chart" /> : <p>Loading chart...</p>}
    </div>
    </div>
  );
};

export default ChartComponent;