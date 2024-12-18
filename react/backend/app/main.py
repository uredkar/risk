# backend/main.py
from fastapi import FastAPI
from pydantic import BaseModel
from typing import List
import numpy as np
import QuantLib as ql  # Example for option pricing
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
from flask import Flask, send_file
import matplotlib.pyplot as plt
import io

app = FastAPI()

origins = [
    "http://localhost:3000",  # Next.js frontend
    "http://127.0.0.1:3000",  # Alternative localhost address
    # Add other origins as needed, e.g., production URLs
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Define request and response models
class Stock(BaseModel):
    symbol: str
    price: float
    quantity: int

class RiskCalculationRequest(BaseModel):
    stocks: List[Stock]

class RiskCalculationResponse(BaseModel):
    total_value: float
    portfolio_variance: float
    portfolio_volatility: float

class OptionPricingRequest(BaseModel):
    underlying_price: float
    strike_price: float
    risk_free_rate: float
    volatility: float
    time_to_maturity: float
    option_type: str  # "call" or "put"

class OptionPricingResponse(BaseModel):
    option_price: float


class OptionPricingImage(BaseModel):
    option_price: float

# Example Risk Calculation Endpoint
@app.post("/calculate-risk", response_model=RiskCalculationResponse)
def calculate_risk(request: RiskCalculationRequest):
    # Simplistic example: calculate total value and portfolio variance
    prices = np.array([stock.price for stock in request.stocks])
    quantities = np.array([stock.quantity for stock in request.stocks])
    total_value = np.sum(prices * quantities)
    
    # For demonstration, let's assume a covariance matrix (in reality, you'd fetch historical data)
    covariance_matrix = np.array([
        [0.04, 0.002],
        [0.002, 0.09]
    ])
    portfolio_variance = np.dot(quantities, np.dot(covariance_matrix, quantities))
    portfolio_volatility = np.sqrt(portfolio_variance)
    
    return RiskCalculationResponse(
        total_value=total_value,
        portfolio_variance=portfolio_variance,
        portfolio_volatility=portfolio_volatility
    )

# Example Option Pricing Endpoint using Black-Scholes
@app.post("/price-option", response_model=OptionPricingResponse)
def price_option(request: OptionPricingRequest):
    option_type = ql.Option.Call if request.option_type.lower() == "call" else ql.Option.Put
    payoff = ql.PlainVanillaPayoff(option_type, request.strike_price)
    exercise = ql.EuropeanExercise(ql.Date.todaysDate() + ql.Period(int(request.time_to_maturity * 365), ql.Days))
    
    european_option = ql.VanillaOption(payoff, exercise)
    
    # Market data
    spot_price = ql.SimpleQuote(request.underlying_price)
    flat_ts = ql.YieldTermStructureHandle(ql.FlatForward(ql.Date.todaysDate(), request.risk_free_rate, ql.Actual365Fixed()))
    flat_vol_ts = ql.BlackVolTermStructureHandle(ql.BlackConstantVol(ql.Date.todaysDate(), ql.NullCalendar(), request.volatility, ql.Actual365Fixed()))
    bsm_process = ql.BlackScholesMertonProcess(
        ql.QuoteHandle(spot_price),
        ql.YieldTermStructureHandle(ql.FlatForward(ql.Date.todaysDate(), 0.0, ql.Actual365Fixed())),  # Dividend yield
        flat_ts,
        flat_vol_ts
    )
    
    european_option.setPricingEngine(ql.AnalyticEuropeanEngine(bsm_process))
    price = european_option.NPV()
    
    return OptionPricingResponse(option_price=price)


@app.get("/")
def read_root():
    return {"message": "Welcome to FastAPI chart server!"}

@app.get("/chart")
def get_chart():
    # Generate a simple plot
    plt.figure(figsize=(6, 4))
    plt.plot([1, 2, 3, 4], [10, 20, 25, 30], label="Sample Data")
    plt.title("Sample Chart")
    plt.xlabel("X-axis")
    plt.ylabel("Y-axis")
    plt.legend()

    # Save the plot to a BytesIO object
    img = io.BytesIO()
    plt.savefig(img, format='png')
    img.seek(0)  # Rewind the file pointer to the start
    plt.close()  # Close the plt object to free memory

    # Return the image as a StreamingResponse
    return StreamingResponse(img, media_type="image/png")