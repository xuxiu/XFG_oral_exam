# ------------------------------------------------------------------------------
# Book: XFG
# ------------------------------------------------------------------------------
# Quantlet: XFG DCC GARCH
# ------------------------------------------------------------------------------
# Description: 
# ------------------------------------------------------------------------------
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: three time series of stock prices, weights, initial value of investment 
# ------------------------------------------------------------------------------
# Output: two plots of the value at risk at 95% and 99% percent, estimation of uni and multi 
# Garch model 
# ------------------------------------------------------------------------------
# Keywords: uni Garch, Dcc Garch, value at risk 
# ------------------------------------------------------------------------------ 
# Author: Yinan Wu, 2016/05/16
# -----------------------------------------------------------------------------


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "scales", "scatterplot3d")
library(ccgarch)
library(fGarch)
library(tseries)
library(rmgarch)
library(rugarch)

# Read data 
stock1 = read.csv('/Users/YinanWu/Downloads/dccg/apple.csv',header = TRUE,sep=',')
stock2 = read.csv('/Users/YinanWu/Downloads/dccg/nike.csv',header = TRUE,sep=',')
stock3 = read.csv('/Users/YinanWu/Downloads/dccg/mi.csv',header = TRUE,sep=',')
stock4 = read.csv('/Users/YinanWu/Downloads/dccg/google.csv',header=TRUE,sep=',')

# order the time series 
stock1 = stock1[order(stock1$Date),]
stock2 = stock2[order(stock2$Date),]
stock3 = stock3[order(stock3$Date),]
stock4 = stock4[order(stock4$Date),]
# Calculate log returns

stock1_return = diff(log(stock1$Adj.Close))
stock2_return = diff(log(stock2$Adj.Close))
stock3_return = diff(log(stock3$Adj.Close))
stock4_return = diff(log(stock4$Adj.Close))

date= as.Date(stock1$Date[c(1:754)],"%Y-%m-%d") 
#plot acf pacf
par(mfrow=c(2,2))
plot(date,stock1_return^2,type="l",ylab="squared log returns", col = alpha("blue", alpha = 0.7))
plot(date,stock2_return^2,type="l",ylab="squared log returns", col = alpha("yellow", alpha = 0.7))
plot(date,stock3_return^2,type="l",ylab="squared log returns", col = alpha("red", alpha = 0.7))
plot(date,stock4_return^2,type="l",ylab="squared log returns", col = alpha("black", alpha = 0.7))
par(mfrow = c(2,2))
acf(stock1_return) 
acf(stock2_return)
acf(stock3_return)
acf(stock4_return)
pacf(stock1_return)
pacf(stock2_return)
pacf(stock3_return)
pacf(stock4_return)

lr =data.frame(stock1_return,stock2_return,stock3_return,stock4_return)
# model estimation 
uspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
spec1 = dccspec(uspec = multispec(replicate(4, uspec1)), dccOrder = c(1,1), 
                distribution = "mvnorm")
uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "std")
spec2 = dccspec(uspec = multispec(replicate(4, uspec2)), dccOrder = c(1,1), 
                distribution = "mvt")
uspec3 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(garchOrder = c(2,1), model = "sGARCH"), distribution.model = "std")
spec3 = dccspec(uspec = multispec(replicate(4, uspec3)), dccOrder = c(2,1), 
                distribution = "mvt")
window = 100                      # forecasting window
invest = 10000                    # portfolio value
pro =0.05                         # probability
w  = matrix(c(rep(1/4,4)))        # weights
VaR = matrix(1,ncol=3,nrow=window)

  for(j in 1:window){
     s1 =  lr[c(j:(753-window+j)),1]
     s2 =  lr[c(j:(753-window+j)),2]
     s3 =  lr[c(j:(753-window+j)),3]
     s4 =  lr[c(j:(753-window+j)),4]
     T  =  length(s1)
     p  =  cbind(s1,s2,s3,s4)
     fit1 = dccfit(spec1, data = p, fit.control = list(eval.se=T))
     dccf1 = dccforecast(fit1,n.ahead = 1)
     fit2=  dccfit(spec2, data = p, fit.control = list(eval.se=T))
     dccf2 = dccforecast(fit2,n.ahead = 1)
     fit3=  dccfit(spec3, data = p, fit.control = list(eval.se=T))
     dccf3 = dccforecast(fit3,n.ahead = 1)
     H1=matrix(unlist(dccf1@ mforecast$H),ncol=4,nrow=4)
     H2=matrix(unlist(dccf2@ mforecast$H),ncol=4,nrow=4)
     H3=matrix(unlist(dccf3@ mforecast$H),ncol=4,nrow=4)
     mu2=matrix(unlist(dccf2@ mforecast$mu),ncol=4,nrow=4)
     mu2 =sum(mu2[,1])
     mu3=matrix(unlist(dccf3@ mforecast$mu),ncol=4,nrow=4)
     mu3 =sum(mu3[,1])
     VaR[j,1]=as.numeric(-sqrt(t(w)%*%H1%*%w)*qnorm(0.05)*invest)
     VaR[j,2]=as.numeric(mu2-sqrt(t(w)%*%H2%*%w)*qt(0.05,754-33-window)*invest)
     VaR[j,3]=as.numeric(mu3-sqrt(t(w)%*%H3%*%w)*qt(0.05,754-38-window)*invest)
     
  }
dev.off()
plot(time,VaR[,1],type="o",ylab='value at risk',col='blue',main='Vaule at risk ')
lines(time,VaR[,2],type="l" ,col='red')
lines(time,VaR[,3],type="l",col='black')

# actual loss
a <- 2500*stock1_return[c((755-window):754)]+2500*stock2_return[c((755-window):754)]+
  2500*stock4_return[c((755-window):754)]+2500*stock3_return[c((755-window):754)]
a =-a

time= as.Date(stock1$Date[c((755-window):754)],"%Y-%m-%d")   #set time 
# draw graphic
dev.off()
VaR20 = VaR[735:754,]
time20 =time[81:100]
a20 =a[81:100]
plot(time,VaR[,1],type="l",ylim=c(150,240),ylab='value at risk',col='blue',main='Vaule at risk ',lty=1)
lines(time,VaR[,2],type="l" ,col='red',lty=2)
lines(time,VaR[,3],type="l",col='black',lty=3)
legend("topleft", c('normal','student t','student t'), cex=0.8, col=c('blue','red','black'), 
       lty=1:3, lwd=2, bty="n")

plot(time20,VaR20[,1],type="o",ylim=c(-200,300),ylab='value at risk',col='blue',main='Vaule at risk ')
lines(time20,VaR20[,2],type="l" ,col='red')
lines(time20,VaR20[,3],type="l",col='black')
lines(time20,a20 ,type="o", col="purple")

plot(time,VaR[,1],type="o",ylim=c(-200,300),ylab='value at risk',col='blue',main='Vaule at risk ')
lines(time,VaR[,2],type="l" ,col='red')
lines(time,VaR[,3],type="l",col='black')
lines(time,a ,type="o", col="purple")



