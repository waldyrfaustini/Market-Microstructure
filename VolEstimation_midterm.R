library(xts)
library(highfrequency)

#Sys.setenv(TZ='EST')
Sys.setenv(TZ='GMT')

Sys.timezone()

# sample data. Irregular times.

qdata <- sample_qdata
tdata <- sample_tdata

head(qdata) # quotes bid/ask
head(tdata) # trades

dim(tdata)

# match the trade and quote data

tqdata <- matchTradesQuotes(tdata, qdata)

head(tqdata)
dim(tqdata)

tail(tqdata)

plot(as.numeric(tqdata$PRICE[550:600]), col="blue",main="tqdata")
lines(as.numeric(tqdata$OFR[550:600]), col="green")
lines(as.numeric(tqdata$BID[550:600]), col="red")

px <- tqdata$PRICE
p <- as.numeric(px)

# plot the trade prices
plot(p, col="red", type="l", ylab="Trade price", xlab="Trade no.")
plot(100:200, p[100:200], col="red", type="l", ylab="Trade price", xlab="Trade no.")

#Trade volume and trading profile

dailyvolume <- sum(as.numeric(tqdata$SIZE))

tradevolume <- as.numeric(tqdata$SIZE)

volaveragedprice <- 1/dailyvolume*sum(p*tradevolume)

ylim2 <- c(0,5000)
plot(tradevolume, ylim=ylim2, type="l", 
     main="Daily trading volume profile")


# Draw the signature plot = RV(sampling freq)
# Signature plot = plot of RV as a function of the sampling freq

realizedVarLog <- function(q){rCov(diff(log(p), lag=q))/q}
realizedVar <- function(q){rCov(diff(p, lag=q, differences=1))/q}

realizedVarLog(1)
sqrt(realizedVarLog(1))

realizedVar(1)
sqrt(realizedVar(1))

1/p[1]*sqrt(realizedVar(1))

# compute the signature plot RV(lag)

rv_data <- NULL
rv_dataLog <- NULL

for(q in 1:200){
  
  rv_data <- c(rv_data, realizedVar(q))
  rv_dataLog <- c(rv_dataLog, realizedVarLog(q))
  
}

plot(rv_dataLog, type ="l", main="Signature plot for log(price)")

# q5min is the number of trades per 5 mins. 
# Compute q5min = Trades/5mins. Hint: there are 390 mins in a trading day
# Use it to compute the realized variance by sampling every 5 mins

n.trades <- dim(tqdata)[1]

q5min <- n.trades*5/390

  rv5 = realizedVar(q5min)
  rv5log = realizedVarLog(q5min)
  
  sqrt(rv5)
  sqrt(rv5log)
  
  abline(h=rv5log,col="red")

# Signature plot for prices p (as opposed to log p)
  
plot(rv_data, type ="l", main="Signature plot for prices")
abline(h=rv5,col="red")


# Roll model estimate of the volatility the tqdata
# recall p are the trade prices, dp = p(t) - p(t-1) are price changes

dp = diff(p)

# compute the covariance of the price changes, for the Roll model analysis
covdp <- acf(dp, lag.max=10, 
            type="covariance", plot=TRUE,
            main="Autocovariance of price changes")

gamma0 <- covdp$acf[1]
gamma1 <- covdp$acf[2]

sig2u = gamma0 + 2*gamma1 

rvRoll <- sig2u*n.trades

sigRoll <- sqrt(sig2u*n.trades)

1/p[1]*sigRoll

##############################################################
## Compare with the two-scale estimator of ZMA (not for exam)

rvts <- rTSCov(pdata = tqdata$PRICE,K=104,J=1)

rvts

abline(h=rvts, col="blue") #overlay on top of the log(price) signature plot
sqrt(rvts)
