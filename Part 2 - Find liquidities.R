library(xts)
library(highfrequency)
library(tidyverse)

Sys.setenv(TZ='GMT')

read_tqdata <- function(base_path='new_data/processed', 
                        file_extension,
                        symbol_to_use){
  qfile = paste0(base_path, '/', file_extension, '_q.csv')
  tfile = paste0(base_path, '/', file_extension, '_t.csv')
  print(qfile)
  print(tfile)
  qdataframe = read.csv(qfile)
  qdataframe['SYMBOL'] = symbol_to_use
  tdataframe = read.csv(paste0(base_path, '/', file_extension, '_t.csv'))
  tdataframe['SYMBOL'] = symbol_to_use
  
  qdata <- xts(qdataframe[,-1], order.by=as.POSIXct(qdataframe[,1]))
  tdata <- xts(tdataframe[,-1], order.by=as.POSIXct(tdataframe[,1]))
  
  # join quotes and trade data
  tqdata <- matchTradesQuotes(tdata, qdata)
  
  # give liquidities
  liq = getLiquidityMeasures(tqdata)

  # save
  tqfile = paste0("new_data/processed/liquidities/", file_extension, '_tq.csv')
  liqfile = paste0("new_data/processed/liquidities/", file_extension, '_liq.csv')
  print(tqfile)
  print(liqfile)
  write.csv(as.data.frame(tqdata), tqfile)
  write.csv(as.data.frame(liq), liqfile)

}

# Read processed data and calculate liquidity measures for all stocks
read_tqdata(file_extension='AAPL_1', symbol_to_use='AAPL')
read_tqdata(file_extension='AAPL_2', symbol_to_use='AAPL')
read_tqdata(file_extension='AAPL_3', symbol_to_use='AAPL')

read_tqdata(file_extension='Amazon_1', symbol_to_use='AMZN')
read_tqdata(file_extension='Amazon_2', symbol_to_use='AMZN')
read_tqdata(file_extension='Amazon_3', symbol_to_use='AMZN')

read_tqdata(file_extension='Facebook_1', symbol_to_use='FB')
read_tqdata(file_extension='Facebook_2', symbol_to_use='FB')
read_tqdata(file_extension='Facebook_3', symbol_to_use='FB')

read_tqdata(file_extension='TSLA_1', symbol_to_use='TSLA')
read_tqdata(file_extension='TSLA_2', symbol_to_use='TSLA')
read_tqdata(file_extension='TSLA_3', symbol_to_use='TSLA')

read_tqdata(file_extension='UAL_1', symbol_to_use='UAL')
read_tqdata(file_extension='UAL_2', symbol_to_use='UAL')
read_tqdata(file_extension='UAL_3', symbol_to_use='UAL')
