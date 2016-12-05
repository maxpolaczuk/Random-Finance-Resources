
#read in BTC:
setwd('h:/research')

btc = read.csv('bitcoin_average.csv')


#ACF and PACF

par(mfrow=c(2,1))

acf(btc$Returns,main="Serial Correlation of Bitcoin Returns") 

## hist
hist(btc$Returns, main="", prob=TRUE,xlab = "Bitcoin Returns")

################

# df test for stationary returns...
install.packages('tseries')
require(tseries)
adf.test(btc[,2],k=1, "s")


plot(btc$Returns, main="BTC Returns", type='l')

#################

# FINDING BEST AR(p):
names(arima(btc$Returns,order = c(1,0,0)) )

K = 13
AICs = list()

for( i in 1:K){
AICs[[i]] = arima(btc$Returns,order = c(i,0,0))$aic
}

aics = unlist(AICs)
par(mfrow=c(3,1))
plot(aics, xlab = "Lag Order (q)",ylab="AIC", main = "Akaike Information Criterion of AR(q)", type = 'l', lwd=2) 

## Autocorrelation of the sq residuals plkotted for AR1:

acf(  (arima(btc$Returns,order = c(1,0,0))$residuals)^2, main = "Squared Residuals of AR(1)")

# BOX TEST

serie = (arima(btc$Returns,order = c(1,0,0))$residuals)^2
J =30
store = rep(NA, J)
for(i in 1:J){
	store[i] = Box.test(serie, type="Ljung",lag=i,fitdf=1)$statistic
}

plot(store,main='Ljung-Box Test Statistics of Squared Residuals',type='l', lwd = 2, xlab = "Lag Order", ylab = "Q-Statistic")


##########################
###### Cross correlations

names(btc)
ccf(btc$Total.Volume,btc$Return)











