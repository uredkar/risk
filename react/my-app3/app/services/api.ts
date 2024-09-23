
import axios from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

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
    return response.data;
  } catch (error) {
    console.error('Error pricing option:', error);
    throw error;
  }
};
