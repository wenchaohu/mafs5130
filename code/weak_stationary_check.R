data_dir = 'F:/mafs/mafs5130/group_project/mafs5130/data/'

fx_rate = read.csv(paste(data_dir, 'HKDCNY.csv', sep = ''), header = TRUE)

# merge the price of A and H stocks, adjust foreign exchange and return price diff of A - H
calc_price_diff <- function(ticker_a, ticker_h, price_type = "Close"){
  px_a = read.csv(paste(data_dir, ticker_a, '.csv', sep = ''), header = TRUE);
  px_h = read.csv(paste(data_dir, ticker_h, '.csv', sep = ''), header = TRUE);
  px_ah = merge(px_a, px_h, by = "Date", suffixes = c("_A","_H"));
  px_ah = merge(px_ah, fx_rate, by = "Date")
  px_ah[["FX_Adj_Px_A"]] = px_ah[paste(price_type, "_A", sep='')]*px_ah["HKDCNY"]
  px_ah[["AH_Price_Diff"]] = px_ah["FX_Adj_Px_A"] - px_ah[paste(price_type, "_H", sep='')]
  return(px_ah)
}

check_weak_stationary <- function(firm_name, ts, lag = 20) {
  # compute acf
  res.acf = acf(ts, lag.max = lag)
  plot(res.acf, main = firm_name)
  return(res.acf)
}

px_ah = calc_price_diff('2628.HK', '601628.SS')
check_weak_stationary('China Life', px_ah['AH_Price_Diff'][,1])
