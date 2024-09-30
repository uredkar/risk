
import axios from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:5000';

export const calculateRisk = async (stocks: any) => {
  try {
    const response = await axios.post(`${API_BASE_URL}/calculate-risk`, { stocks });
    return response.data;
  } catch (error) {
    console.error('Error calculating risk:', error);
    throw error;
  }
};

export const priceOption = async (optionData: any) => {
  try {
    const response = await axios.post(`${API_BASE_URL}/price-option`, optionData);
    return response.data; a
  } catch (error) {
    console.error('Error pricing option:', error);
    throw error;
  }
};


export const getChart = async (xValues: number[], yValues: number[]): Promise<string | null> => {
  // Convert arrays to comma-separated strings
  const xParam = xValues.join(',');
  const yParam = yValues.join(',');

  const response = await fetch(`${API_BASE_URL}/chart?x_values=${xParam}&y_values=${yParam}`);

  if (response.ok) {
    // Return the image URL
    return response.url;
  } else {
    console.error("Error fetching the chart");
    return null;
  }
};