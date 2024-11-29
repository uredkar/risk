import pandas as pd
import matplotlib.pyplot as plt

# Load the data
data = pd.read_csv("pnl_data.csv")

# Plot the PnL diagram
plt.plot(data["SpotPrice"], data["PnL"], label="PnL")
plt.xlabel("Spot Price")
plt.ylabel("PnL")
plt.title("PnL Diagram")
plt.legend()
plt.grid()
plt.show()


data = pd.read_csv("strategy_pnl.csv")

# Plot the PnL diagram
plt.plot(data["SpotPrice"], data["PnL"], label="PnL")
plt.xlabel("Spot Price")
plt.ylabel("PnL")
plt.title("PnL Diagram")
plt.legend()
plt.grid()
plt.show()

data = pd.read_csv("individualoptions.csv")

plt.plot(data["SpotPrice"], data["PnL"], label="PnL")
plt.xlabel("Spot Price")
plt.ylabel("PnL")
plt.title("PnL Diagram")
plt.legend()
plt.grid()
plt.show()