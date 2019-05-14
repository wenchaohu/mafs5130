library(latex2exp)

data_dir = 'F:/mafs/mafs5130/group_project/mafs5130/data/'

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
  res.acf = acf(ts, lag.max = lag, plot = FALSE)
  res.pacf = pacf(ts, lag.max = lag, plot = FALSE)
  plot(res.pacf, main = latex2exp::TeX(plot_name))
  return(list("acf" = res.acf, "pacf" = res.pacf))
}

calc_mean_before_after_stock_connect <- function(px_table) {
  sc_date = '2014-11-15'
  px_table["Date"][,1] = as.Date(px_table['Date'][,1])
  px_before = px_table[px_table['Date'] < sc_date,]
  px_after = px_table[px_table['Date'] >= sc_date,]
  if (nrow(px_before) > 0)
    px_before.lag_diff_mean = mean(calc_lag_diff(px_before["AH_Price_Log_Diff"][,1]))
  else
    # not issued before stock connect
    px_before.lag_diff_mean = NaN
  px_after.lag_diff_mean = mean(calc_lag_diff(px_after["AH_Price_Log_Diff"][,1]))
  return(list("before_sc_mean"=px_before.lag_diff_mean, "after_sc_mean"=px_after.lag_diff_mean))
}

fit_arma <- function(ts, max_p, max_q) {
  #aic_res = matrix(, nrow = max_p + 1, ncol = max_q + 1)
  #fit_res = matrix(, nrow = max_p + 1, ncol = max_q + 1)
  min_aic = .Machine$double.xmax
  min_aic_fit = NULL
  min_q = -1
  min_p = -1
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      arma_fit = arima(ts, order = c(p, 0, q), include.mean = TRUE)
      fit_aic = AIC(arma_fit)
      if (fit_aic < min_aic) {
        #aic_res[p+1, q+1] = fit_aic
        #fit_res[p+1, q+1] = arma_fit        
        min_aic = fit_aic
        min_aic_fit = arma_fit
        min_p = p
        min_q = q
      }
    }
  }
  # find the p and q with the minimum AIC
  #min_aic = min(aic_res)
  #min_aic_ind = which(aic_res == min_aic, arr.ind = TRUE)
  print(sprintf('When p = %d and q = %d, AIC gets minimum %.4f', min_p, min_q, min_aic))
  #return(list("arma_fit" = fit_res(min_aic_ind[1, 1]+1, min_aic_ind[1, 2]+1), "AIC"=min_aic))
  return(list("arma_fit" = min_aic_fit, "AIC"=min_aic, "p"=min_p, "q"=min_q))
}
# China Life
#px_ah = calc_price_diff('2628.HK', '601628.SS')
# check the difference of raw prices
#check_weak_stationary('China Life', px_ah['AH_Price_Diff'][,1])
# check the difference of log prices
#check_weak_stationary('ACF of $\\Delta p_{i,t}$ of China Life}', px_ah["AH_Price_Log_Diff"][,1])
#check_weak_stationary('PACF of $\\Delta_1 (\\Delta p_{i,t})$ of China Life', calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 20)
#fit_res = fit_arma(calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 6, 6)
#res = calc_mean_before_after_stock_connect(px_ah)

# China Construction Bank
#px_ah = calc_price_diff('0564.HK', '601717.SS')
# check the difference of raw prices
#check_weak_stationary('China Life', px_ah['AH_Price_Diff'][,1])
# check the difference of log prices
# check_weak_stationary('PACF of $\\Delta_1 (\\Delta p_{i,t})$ of China Construction Bank}', calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 20)
#fit_res = fit_arma(calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 6, 6)


# iterate all dual-listed tickers
dual_listed_tickers = read.csv(paste(data_dir, 'dual_tickers.csv', sep = ''), header = TRUE)
# the acf of lag-1 1st-order log price for all tickers
#all_acf = matrix(, nrow = nrow(dual_listed_tickers), ncol = 21)
all_pacf = matrix(, nrow = nrow(dual_listed_tickers), ncol = 20)
all_fit_coeff.ar = matrix(0, nrow = nrow(dual_listed_tickers), ncol = 8)
all_fit_coeff.ma = matrix(0, nrow = nrow(dual_listed_tickers), ncol = 8)
all_aic = matrix(0, nrow = nrow(dual_listed_tickers), ncol = 1)
all_mean = matrix(0, nrow = nrow(dual_listed_tickers), ncol = 2)

for (row in 1:nrow(dual_listed_tickers)) {
  h_ticker <- dual_listed_tickers[row, "H_Ticker"]
  a_ticker <- dual_listed_tickers[row, 'A_Ticker']
  if (a_ticker == '601869.SH')
    next
  print(sprintf('Checking lag-1 and 1st-order differenced log price for (%s, %s)...', a_ticker, h_ticker))
  px_ah = calc_price_diff(substr(h_ticker, 2, 8), paste(substr(a_ticker, 1, 8), 'S', sep = ''))
  #px_ah.corr = check_weak_stationary('No title', calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]))
  #all_acf[row,] = px_ah.corr$acf$acf
  #all_pacf[row,]= px_ah.corr$pacf$acf
  #fit_res <- fit_arma(calc_lag_diff(px_ah["AH_Price_Log_Diff"][,1]), 6, 6)
  #if (fit_res$p > 0)  {
  #  all_fit_coeff.ar[row, 1:(fit_res$p+1)] = append(c(fit_res$arma_fit$coef[(fit_res$p+fit_res$q)+1]), -fit_res$arma_fit$coef[1:(fit_res$p)])
  #}
  #else {
  #  all_fit_coeff.ar[row, 1] = fit_res$arma_fit$coef[(fit_res$p+fit_res$q)+1]
  #}
  #if (fit_res$q > 0) {
  #  all_fit_coeff.ma[row, 1:fit_res$q] = fit_res$arma_fit$coef[(fit_res$p+1):(fit_res$p+fit_res$q)]
  #}
  #all_aic[row, 1] = fit_res$AIC
  mean_res = calc_mean_before_after_stock_connect(px_ah)
  all_mean[row, 1] = mean_res$before_sc_mean
  all_mean[row, 2] = mean_res$after_sc_mean
}
