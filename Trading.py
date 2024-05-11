import yfinance as yf
import pandas as pd

def get_fib_levels(high, low):
    fib_levels = {}
    fib_levels['0%'] = high
    fib_levels['23.6%'] = high - ((high - low) * 0.236)
    fib_levels['38.2%'] = high - ((high - low) * 0.382)
    fib_levels['50%'] = (high + low) / 2
    fib_levels['61.8%'] = high - ((high - low) * 0.618)
    fib_levels['100%'] = low
    return fib_levels

def calculate_rsi(prices, window=14):
    deltas = prices.diff()
    gain = (deltas.where(deltas > 0, 0)).rolling(window=window).mean()
    loss = (-deltas.where(deltas < 0, 0)).rolling(window=window).mean()
    rs = gain / loss
    return 100 - (100 / (1 + rs))

def check_buy_sell(signal, price):
    if signal == 'Buy' and price <= 0.236:
        return 'Buy'
    elif signal == 'Sell' and price >= 0.618:
        return 'Sell'
    else:
        return 'Hold'

def analyze_stocks():
    # Define a list of popular stock symbols
    popular_stocks = ['AAPL', 'MSFT', 'GOOGL', 'AMZN', 'TSLA', 'FB', 'NFLX', 'NVDA', 'AMD', 'SPY']
    
    for ticker in popular_stocks:
        stock = yf.Ticker(ticker)
        hist_data = stock.history(period="1d")
        if hist_data.empty:
            print(f"No data available for {ticker}")
            continue
        
        close_prices = hist_data['Close']
        
        # Calculate RSI
        rsi = calculate_rsi(close_prices)
        rsi_value = rsi[-1]
        
        # Determine RSI convergence/divergence
        rsi_signal = ''
        if rsi_value < 30:
            rsi_signal = 'Buy'
        elif rsi_value > 70:
            rsi_signal = 'Sell'
        else:
            rsi_signal = 'Hold'
        
        # Calculate Fibonacci levels
        high = max(hist_data['High'])
        low = min(hist_data['Low'])
        fib_levels = get_fib_levels(high, low)
        
        # Output analysis
        print(f"\nAnalysis for {ticker}:")
        print("Fibonacci Retracement Levels:")
        print(pd.DataFrame.from_dict(fib_levels, orient='index', columns=['Price']))
        print(f"\nRSI: {rsi_value} ({rsi_signal})")
        print(f"\nRecommendation based on Fib Levels and RSI: {check_buy_sell(rsi_signal, fib_levels['23.6%'])}\n")

if __name__ == "__main__":
    analyze_stocks()
