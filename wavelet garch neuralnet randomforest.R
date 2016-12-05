# tapped delay line pseudocode
install.packages('waveslim')
install.packages('randomForest')
install.packages('wavethresh')#require(WaveletCo)
require(wavethresh)
require(waveslim)
require(randomForest
install.packages('neuralnet')
require(neuralnet)
#x = rnorm(800,0,1)  # just using white noise for the moment
#x = rnorm(800,mean=0,sd=abs(rnorm(1,0,3)) )  # transform to nonstationary

setwd('h:/research')
#setwd('C:/Users/Maximilian/Documents/413')
btc = read.csv('btc.csv')
btcret = read.csv('btcret.csv')

merged = read.csv('Merged_data.csv')

names(merged)




###################################
## FUNCTION
#####################################
returnizer = function(x,date){
  # x is an input that goes from earliest date to latest date
  
  N = length(x)-1
  newx = rep(NA, N) # set up null vector of returns
  for(i in 1:N){
    newx[i] = log(x[i+1])-log(x[i]) 
  }
  
  return(data.frame(date[1:N],newx))
}
####################################
####################################
names(btc)
btcret = returnizer(rev(btc$X24h.Average), rev(btc$Date))




# do the wavelet forest for gold

wf = waveletforest(goldret$newx,len=8000,xvalid=1000,lagg=5)
paste('the mean of real ret is:', mean(wf[,1]),'the mean of predicted is',mean(wf[,2]))
paste('the sd of real ret is:', sd(wf[,1]),'the sd of predicted is', sd(wf[,2]))

cor(wf[,1],wf[,2])

# taking $100 and using the predicted vs real returns gives

sim_real = rep(NA, length(test.r))
sim_pred = rep(NA, length(test.r))
sim_real[1]=rev(btc$X24h.Average)[nrow(btc)-500];sim_pred[1]=rev(btc$X24h.Average)[nrow(btc)-500]
for( i in 2:length(sim_pred)){
  sim_real[i] = sim_real[i-1]*(1+test.r[i])
  sim_pred[i] = sim_pred[i-1]*(1+pr.nn_[i])
}
par(mfrow=c(1,1))
plot(sim_pred, main= "Real vs Simulated Price Series", ylab = "$",xlab="Days", type='l',col='grey1',lty=2)
lines(sim_real,type='l', col= 'black',lwd = 2)



############################################################
# make the process a function...

 
## x is returns squared
len = length(merged$Bitcoin); xvalid = 500;lagg=1; X = sapply(merged$Bitcoin,function(x){x^2}) # preselecting params
x= merged$Bitcoin

waveletforest = function(x, len, xvalid = 100, lagg){
  
x = x[1:len] # subsetting based on how many rows we want
N = length(x)
ar1 = x[c(N:(N-lagg))] # this is saving the last value of x - as we are lagging by 1 value
x = x[-c(N:(N-lagg))] # removing that ar1 value...

y= rep(NA, length(x)) # target storage vector


## coefficients for memory - closer are more importan

# algo complexity is less than O(N**2)...
for(i in 1:length(x)){
  
  weights = rep(NA, i) # null vector for weights
  # weights are the ith value exponentially decaying...
  for(j in 1:i){
    weights[j] =  (j/i) # the jth weight is (j/i)^2 - so exponentially decaying
  }

	store = rep(NA,i) # this is an empty list which we replace for each i

	for(k in 1:i){
		store[k] = x[k] * weights[k] ## dot product of filter and signal
	}
	
	y[i] = sum(store) # our desired output

}


par(mfrow=c(2,1))
plot(X, type = 'l')
plot(x, type = 'l')
#### GRAPHICS #######

par(mfrow=c(1,1))
#plot(x, main= "Bitcoin Returns",type='l',xlab="Days",col="green3")
plot(y, main = "Tapped Delay Line Bitcoin Volatility (Memory)",type= 'l',xlab="Days", ylab='', col="turquoise3", lwd=2)		
	

#### WAVELET TRANSFORM #####

wt = modwt(y)  # Using MODWT

# GRAPHICS
dev.off()
par(mfrow=c(3,2))
for(i in 1:length(wt)){
	plot(wt[[i]],type='l',xlab ='Time (Days)',ylab = 'Frequency' , main=paste('Wavelet Transform Resolution',i))
}
plot(y, main = "Original Signal (TDL Bitcoin Returns)",type= 'l')	

lagged = c(x[(length(ar1)+1):length(x)],ar1) #this makes our lagged vector


### MAKE A RANDOM FOREST REGRESSION PREDICTIVE MODEL WITH INPUTS AS THE WAVELET DECOMPOSITION
names(merged)
# Creating the inputs and targets
# add gold, vix, 
data = data.frame(target = X[1:length(x)], wt[[1]],wt[[2]],wt[[3]],wt[[4]],wt[[5]], lagged, merged$Gold[1:length(x)], merged$SP500[1:length(x)],merged$Interest_Change[1:length(x)],merged$VIX_Level[1:length(x)])
head(data)
length(merged$Gold)
nrow(data)

# do a random forest algorithm...
if(rforest == TRUE){

	test_ix = sample(c(1:nrow(data)),replace = F)[1:xvalid] # test data indexing 
	#test_ix = c( (nrow(data)-xvalid):nrow(data) )

	train_data = data[-test_ix,] # subsetting for the testing index
	test_data = data[test_ix,]

	n <- names(train_data)
	f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))
	rf = randomForest( f, data = train_data)
	
}




# reminder to add lagged values for the other contexts as well (atm just a vector should be matrix)...


### NOW DO NORAMLIZING:
maxs
maxs <- apply(data, 2, max)
mins = apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
head(scaled)


test_data

#  Note that the target is the first column in the data frame...
test_ix = sample(c(1:nrow(data)),replace = F)[1:xvalid] # test data indexing 
#test_ix = c( (nrow(data)-xvalid):nrow(data) )


train_data = scaled[-test_ix,] # subsetting for the testing index
test_data = scaled[test_ix,]


#neural network:
n <- names(train_data)
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))

### Build in epochs
k =Sys.time();
#for(i in 1:)
nn <- neuralnet(f,data=train_data,hidden=c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15),linear.output=T, act.fct = 'tanh',err.fct = 'sse', rep=1); 
#weight = nn$weights

Sys.time()-k


plot(nn) # visualise the neuralnet
nn$result.matrix#[2,]
pr.nn <- compute(nn,test_data[,-1])
pr.nn_ <- abs(pr.nn$net.result*(max(data[,1])-min(data[,1]))+min(data[,1]) )

test.r <- (test_data[,1])*(max(data[,1])-min(data[,1]))+min(data[,1])

par(mfrow=c(3,1))
plot(pr.nn_, main = 'Neural Network Volatility Forecasts',type = 'l')

plot(test.r, main = 'Real Volatility',type = 'l')

cor(pr.nn_, test.r)




### MODELLING 


# linear model:

reg = lm(f, data = train_data)
#summary(reg)

# predict with lin reg:

lr_predz = predict(reg, newdata = test_data[,-1])*(max(data[,1])-min(data[,1]))+min(data[,1])
plot(lr_predz,type='l',main="Linear Regression Predictions",xlab="Days",ylab="Return")



fram = data.frame(Real = test.r, NNet = pr.nn_, Reg = lr_predz)
write.csv(fram, 'Forecast Results.csv', row.names = FALSE)



rf = randomForest(x = train_data[,-1],y = train_data[,1], ntree= 500)
# PRED VALUES
rethat = predict(rf, newdata = test_data[,-1])
# comparing
par(mfrow=c(3,1))
plot(test_data[,1],main="Real (Simulated) Returns",type='l',ylab = "Returns")
plot(rethat,main="Predicted (Simulated) Returns", type='l',ylab = "Returns")
plot(rf,main="MSE of the Random Forest")

return(data.frame(pred = rethat, real = test_data[,1],sq.difference = (test_data[,1]-rethat)^2))
}


## This looks very random, which is good since we have used a random data series
#####################################################################
# Now apply this to real data...

# Start with VIX Returns:
vix = read.csv('vixret.csv')
plot(vix[,2], type= 'l', main= "VIX Log Returns")


# Use the function:
vix_WF = waveletforest(vix[,2],2000,500)

cor(vix_WF[,1],vix_WF[,2])

# There is not a lot of explanation of the test data, so now we try 
# include the AR(n) period lag of the real series in order to assist 
# predictive power


# note that the predicted series is forecasting much to conservatively
# on either side...



########## NEURAL NETWORK


install.packages('neuralnet')
require('neuralnet')


maxs <- apply(data, 2, max)


nn = neuralnet








################
# NGARCH modelling with the same inputs....


install.packages('rugarch')
require(rugarch)


# use modwt series - 
# run 5 separate GARCH(1,1)s

K = 200
test.ix = c(1:K)
test.ix
train = x[-test.ix]
test = x[test.ix]

wt = modwt(train)

lv1 = wt[[1]]
lv2 = wt[[2]]
lv3 = wt[[3]]
lv4 = wt[[4]]
smooth = wt[[5]]

par(mfrow=c(2,2))
hist(lv1)
hist(lv2)
hist(lv3)
hist(lv4)
hist(smooth)

install.packages('moments')
require(moments)
require(stats)

skewness(smooth)


par(mfrow=c(3,2))
### LV 1

## plotting absolute value of wavelet coefficients
plot(abs(lv1), type = 'l', )

ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
distribution.model = "std")
egarch.fit = ugarchfit(data = abs(lv1), spec = spec,
solver = "solnp", solver.control = ctrl)
egarch.fit

## LV2

plot(abs(lv2), type = 'l')
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
distribution.model = "std")
egarch.fit2 = ugarchfit(data = abs(lv2), spec = spec,
solver = "solnp", solver.control = ctrl)
egarch.fit2

## LV3
plot(abs(lv3), type = 'l')
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
distribution.model = "std")
egarch.fit3 = ugarchfit(data = abs(lv3), spec = spec,
solver = "solnp", solver.control = ctrl)
egarch.fit3

## LV4
plot(abs(lv4), type = 'l')
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
distribution.model = "std")
egarch.fit4 = ugarchfit(data = abs(lv4), spec = spec,
solver = "solnp", solver.control = ctrl)
egarch.fit4

## LV5
plot(abs(smooth), type = 'l')
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
distribution.model = "std")
egarch.fit5 = ugarchfit(data = abs(smooth), spec = spec,
solver = "solnp", solver.control = ctrl)
egarch.fit5

##########


plot(egarch.fit)

## NOW FOR A TEST SET - predict each decomposition level
# RECONSTRUCT EACH GARCH AND OBTAIN THE STANDARDIZED (out of sample) METRIC (MSE)...


wt_test = modwt(test)
lv1_test = wt_test[[1]]
lv2_test = wt_test[[2]]
lv3_test = wt_test[[3]]
lv4_test = wt_test[[4]]
smooth_test = wt_test[[5]]


predict(lv1_test, egarch.fit)


forecast = ugarchforecast(spec, n.ahead = 1, n.roll = 2579, data = mydata[1:2580, ,drop=FALSE], out.sample = 2579);






















