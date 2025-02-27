import pandas as pd
import matplotlib.pyplot as plt
import glob

# Load the CSV (Spark saves in partitioned format)
file_path = glob.glob("drawdown_output/*.csv")[0]  # Get first partitioned file
df = pd.read_csv(file_path)

# Convert date to datetime
df['date'] = pd.to_datetime(df['date'])

# Plot cumulative return
plt.figure(figsize=(12, 6))
plt.plot(df['date'], df['cumulative_return'], label="Cumulative Return", color="blue", linewidth=2)
plt.fill_between(df['date'], df['drawdown'], 0, color="red", alpha=0.3, label="Drawdown")

# Labels & Title
plt.xlabel("Date")
plt.ylabel("Value")
plt.title("📉 Cumulative Return & Drawdown")
plt.legend()
plt.grid(True)

# Show plot
plt.show()

import yfinance as yf

# Download SPY data for the last 2 years
spy_data = yf.download('SPY', start='2019-02-26', end='2025-02-26')

# Display the first few rows
print(spy_data.head())
import pandas as pd

# Calculate daily returns
spy_data['Daily Return'] = spy_data['Close'].pct_change()

# Drop the first row with NaN value
spy_data = spy_data.dropna()

# Display the first few rows with daily returns
print(spy_data[['Close', 'Daily Return']].head())

# Calculate cumulative returns
spy_data['Cumulative Return'] = (1 + spy_data['Daily Return']).cumprod()

# Calculate running maximum
spy_data['Running Max'] = spy_data['Cumulative Return'].cummax()

# Calculate drawdown
spy_data['Drawdown'] = (spy_data['Cumulative Return'] - spy_data['Running Max']) / spy_data['Running Max']

# Display the first few rows with drawdown
print(spy_data[['Cumulative Return', 'Running Max', 'Drawdown']].head())

import matplotlib.pyplot as plt

# Plot cumulative return and drawdown
plt.figure(figsize=(14, 7))

# Plot cumulative return
plt.plot(spy_data.index, spy_data['Cumulative Return'], label='Cumulative Return', color='blue')

# Plot drawdown
plt.fill_between(spy_data.index, spy_data['Drawdown'], 0, color='red', alpha=0.3, label='Drawdown')

# Add labels and title
plt.xlabel('Date')
plt.ylabel('Return')
plt.title('SPY Cumulative Return and Drawdown (2019-2025)')
plt.axhline(0, color='black', linestyle='--', linewidth=0.5)
plt.axhline(-0.2, color='red', linestyle='--', linewidth=0.5, label='20% Drawdown Threshold')
plt.axhline(-0.5, color='orange', linestyle='--', linewidth=0.5, label='50% Drawdown Threshold')
plt.axhline(-0.7, color='yellow', linestyle='--', linewidth=0.5, label='70% Drawdown Threshold')
plt.axhline(-0.9, color='green', linestyle='--', linewidth=0.5, label='90% Drawdown Threshold') # Add legend and grid
plt.legend()
plt.grid(True)

# Show plot
plt.show()
