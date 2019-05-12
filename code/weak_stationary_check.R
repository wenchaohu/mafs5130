library(latex2exp)

data_dir = 'G:\\mafs\\group_project\\data\\'

fx_rate = read.csv(paste(data_dir, 'HKDCNY.csv', sep = ''), header = TRUE)

calc_lag_diff <- function(ts){
  d = diff(ts, lag = 1, differences = 1)
  return(d)
}

# merge the price of A and H stocks, adjust foreign exchange and return price diff of A - H
calc_price_diff <- function(ticker_a, ticker_h, price_type = "Close"){
  px_a = read.csv(paste(data_dir, ticker_a, '.csv', sep = ''), header = TRUE);
  px_h = read.csv(paste(data_dir, ticker_h, '.csv', sep = ''), header = TRUE);
  px_ah = merge(px_a, px_h, by = "Date", suffixes = c("_A","_H"));
  px_ah = merge(px_ah, fx_rate, by = "Date")
  px_ah[["FX_Adj_Px_A"]] = px_ah[paste(price_type, "_A", sep='')]*px_ah["HKDCNY"]
  h_px_col_name = paste(price_type, "_H", sep='')
  px_ah[["AH_Price_Diff"]] = px_ah["FX_Adj_Px_A"] - px_ah[h_px_col_name]
  px_ah["AH_Price_Log_Diff"] = log(px_ah["FX_Adj_Px_A"][,1]) - log(px_ah[h_px_col_name][,1])
  ord1_lag1_order_log_price_diff = diff(px_ah["AH_Price_Log_Diff"][,1], lag = 1, differences = 1)
  return(px_ah)
}

check_weak_stationary <- function(plot_name, ts, lag = 20) {
  # compute acf
  res.acf = acf(ts, lag.max = lag)
  plot(res.acf, main = latex2exp::TeX(plot_name))
  return(res.acf)
}

# China Life
#px_ah = calc_price_diff('2628.HK', '601628.SS')
# check the difference of raw prices
#check_weak_stationary('China Life', px_ah['AH_Price_Diff'][,1])
# check the difference of log prices
# check_weak_stationary('ACF of $\\Delta p_{i,t}$ of China Life}', px_ah["AH_Price_Log_Diff"][,1])

#check_weak_stationary('ACF of $\\Delta_1 (\\Delta p_{i,t})$ of China Life', calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 20)

# China Construction Bank
#px_ah = calc_price_diff('0939.HK', '601939.SS')
# check the difference of raw prices
#check_weak_stationary('China Life', px_ah['AH_Price_Diff'][,1])
# check the difference of log prices
#check_weak_stationary('ACF of $\\Delta p_{i,t}$ of China Construction Bank}', px_ah["AH_Price_Log_Diff"][,1])

# iterate all dual-listed tickers
dual_listed_tickers = read.csv(paste(data_dir, 'dual_tickers.csv', sep = ''), header = TRUE)

# the acf of lag-1 1st-order log price for all tickers
all_acf = matrix(, nrow = nrow(dual_listed_tickers), ncol = 21)

for (row in 1:nrow(dual_listed_tickers)) {
  h_ticker <- dual_listed_tickers[row, "H_Ticker"]
  a_ticker <- dual_listed_tickers[row, 'A_Ticker']
  print(sprintf('Checking lag-1 and 1st-order differenced log price for (%s, %s)...', a_ticker, h_ticker))
  px_ah = calc_price_diff(h_ticker[1:], '601939.SS')
  all_acf[row] = 
}
