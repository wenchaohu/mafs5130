import os
import logging
import pandas as pd
import tushare as tsh
import fix_yahoo_finance as yf
import pandas_datareader as web
from datetime import datetime, timedelta

FORMAT = '%(asctime)-15s %(levelno)s %(message)s'
logging.basicConfig(format=FORMAT)
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# replace with your token on tushare
TOKEN = '6f1136763181298c689dbabcf23ce179c21037a51ee235215d8c1880'


def get_fx(fred_fx_ticker, start):
    fx_ts = web.DataReader(fred_fx_ticker, 'fred', start=start)
    # fill na
    while fx_ts.iloc[:,0].isnull().any():
        l = fx_ts.shape[0]
        for i in range(0, l):
            if pd.isna(fx_ts.iloc[i, 0]):
                if i == 0 or pd.isna(fx_ts.iloc[i-1, 0]):
                    fx_ts.iloc[i, 0] = fx_ts.iloc[i+1, 0]
                elif i == l - 1 or pd.isna(fx_ts.iloc[i+1, 0]):
                    fx_ts.iloc[i, 0] = fx_ts.iloc[i-1, 0]
                else:
                    fx_ts.iloc[i, 0] = (fx_ts.iloc[i-1, 0] + fx_ts.iloc[i+1, 0])/2
    return fx_ts


if __name__ == '__main__':

    data_dir = os.path.dirname(os.path.realpath(__file__))
    data_dir = os.path.join(data_dir, '../data')

    dual_tickers = pd.read_csv(os.path.join(data_dir, '../data/dual_tickers.csv'))

    tsh.set_token(TOKEN)
    pro = tsh.pro_api()
    stock_info = pro.stock_basic(exchange='SSE', list_status='L', fields='ts_code,symbol,name,area,industry,list_date')

    # save stock information
    stock_df = dual_tickers.join(stock_info.set_index('ts_code'), on='A_Ticker')
    stock_df.to_csv(os.path.join(data_dir, 'stock_info.csv'), index=False, encoding='utf_8_sig')
    period = 10  # in integral year

    for tickers in (list(dual_tickers['H_Ticker'].str.slice(start=1)), list(dual_tickers['A_Ticker'].str.replace('.SH', '.SS'))):
        for t in tickers:
            try:
                ticker = yf.Ticker(t)
                hist_data = ticker.history(period=str(period)+'y')
                hist_data = hist_data.reset_index()
                hist_data.to_csv(os.path.join(data_dir, '{}.csv'.format(t)), index=False)
                logger.info('Saved daily price of {}.'.format(t))
            except ValueError as e:
                logger.error('Error occurred while getting {}: {} '.format(t, str(e)))

    # compute hkd cny exchange rate
    look_back = datetime.today() - timedelta(days=period*365)
    # get hkd/usd
    logger.info('Fetching HKD/USD rates...')
    hkd_usd = get_fx('DEXHKUS', look_back.strftime('%Y-%m-%d'))
    # get cny/usd
    logger.info('Fetching CNY/USD rates...')
    cny_usd = get_fx('DEXCHUS', look_back.strftime('%Y-%m-%d'))
    hkd_cny = hkd_usd.join(cny_usd)
    hkd_cny['HKDCNY'] = hkd_cny['DEXHKUS']/hkd_cny['DEXCHUS']
    hkd_cny = hkd_cny.drop(columns=['DEXHKUS', 'DEXCHUS']).reset_index()
    hkd_cny = hkd_cny.rename(columns={'DATE': 'Date'})
    hkd_cny.to_csv(os.path.join(data_dir, 'HKDCNY.csv'), index=False)
    logger.info('Saved HKD/CNY rates.')


