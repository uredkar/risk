"use client"; // This is a client component
import React, { useState } from 'react';
import { Button, TextField, Typography, Box } from '@mui/material';
import { calculateRisk, priceOption } from '../../services/api';

interface Stock {
  symbol: string;
  price: number;
  quantity: number;
}

const RiskPage: React.FC = () => {
  const [stocks, setStocks] = useState<Stock[]>([
    { symbol: 'AAPL', price: 150.12, quantity: 10 },
    { symbol: 'GOOGL', price: 2800.22, quantity: 5 },
  ]);
  const [risk, setRisk] = useState<any>(null);
  const [optionPrice, setOptionPrice] = useState<number | null>(null);

  const handleCalculateRisk = async () => {
    try {
      const result = await calculateRisk(stocks);
      setRisk(result);
    } catch (error) {
      // Handle error
    }
  };

  const handlePriceOption = async () => {
    const optionData = {
      underlying_price: 150.12,
      strike_price: 155,
      risk_free_rate: 0.01,
      volatility: 0.2,
      time_to_maturity: 0.5, // 6 months
      option_type: 'call',
    };
    try {
      const result = await priceOption(optionData);
      setOptionPrice(result.option_price);
    } catch (error) {
      // Handle error
    }
  };

  return (
    <Box sx={{ padding: 4 }}>
      <Typography variant="h4" gutterBottom>
        Portfolio Risk and Option Pricing
      </Typography>

      {/* Display Risk Calculation */}
      <Box sx={{ marginTop: 2 }}>
        <Button variant="contained" color="primary" onClick={handleCalculateRisk}>
          Calculate Risk
        </Button>
        {risk && (
          <Box sx={{ marginTop: 2 }}>
            <Typography>Total Value: ${risk.total_value.toFixed(2)}</Typography>
            <Typography>Portfolio Variance: {risk.portfolio_variance.toFixed(4)}</Typography>
            <Typography>Portfolio Volatility: {risk.portfolio_volatility.toFixed(4)}</Typography>
          </Box>
        )}
      </Box>

      {/* Display Option Pricing */}
      <Box sx={{ marginTop: 4 }}>
        <Button variant="contained" color="secondary" onClick={handlePriceOption}>
          Price Option
        </Button>
        {optionPrice !== null && (
          <Box sx={{ marginTop: 2 }}>
            <Typography>Option Price: ${optionPrice.toFixed(2)}</Typography>
          </Box>
        )}
      </Box>
    </Box>
  );
};

export default RiskPage;
