# shapiro wilk test 



setwd('C:/Users/Maximilian/Documents/413')
btc = read.csv('btc.csv')


plot(btc$Total.Volume,type='l')



gold = read.csv('gold.csv')
sp500 = read.csv('sp500.csv')
vix = read.csv('vix.csv')

# returnizer:
returnizer = function(x,date){
  # x is an input that goes from earliest date to latest date
  
  N = length(x)-1
  newx = rep(NA, N) # set up null vector of returns
  for(i in 1:N){
    newx[i] = log(x[i+1])-log(x[i]) 
  }
  
  return(data.frame(date[1:N],newx))
}

goldret = returnizer(gold$Value, gold$Date)
sp500ret = returnizer(sp500$Adjusted.Close,sp500$Date)
bit = returnizer(btc$X24h.Average,btc$Date)
vixret = returnizer(vix$Adjusted.Close, vix$Date)
# do the shapiro wilk test
require(stats)
# need to use subset of samples as must be < 5000
shapiro.test(goldret$newx[8000:nrow(goldret)]) # reject the null

shapiro.test(sp500ret$newx[(nrow(sp500ret)-4000):nrow(sp500ret)]) # reject null as well

shapiro.test(bit$newx) # reject null

shapiro.test(vixret$newx[(nrow(vixret)-4000):nrow(vixret)]) # reject null (normality)

# now run qqplots
install.packages('ggplot2')
require(ggplot2)


gg_qq <- function(x, obj,distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    ggtitle(paste("Empirical", obj ,"Daily Returns")) + 
    xlab("Theoretical") + ylab("Real")
  #if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  coef
}

dev.off()
par(mfrow=c(3,1))
gg_qq(sp500ret$newx[(nrow(sp500ret)-4000):nrow(sp500ret)], conf=0.99, obj = "S&P 500") # S&P
gg_qq(goldret$newx[8000:nrow(goldret)], conf = 0.99, obj="Gold")
gg_qq(bit$newx, conf = 0.99, obj="Bitcoin")

gg_qq(vixret$newx,conf=0.99,obj="VIX")












