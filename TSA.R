data("UKgas")
head(UKgas,5)
plot((UKgas),type="l") # multiplicative
length(UKgas)
plot(log(UKgas),type="l") # additive

######################################
#Non-parametric EDA in TS data
######################################

#Filtering and Smoothing

# quarterly data - hence length = 4
# average of ((x1+x2+x3+x4)/4 + (x2+x3+x4+x5)/4) - hence 1/8, 1/4, 1/4, 1/4, 1/8
install.packages("stochvol")
install.packages("qrmtools")
library(stochvol)
library(qrmtools)
gas.trend=filter(log(UKgas),filter = c(1/8,1/4,1/4,1/4,1/8),sides = 2)
lines(gas.trend,col=4,lty=2,lwd=2) # lty=line type; ltw=line width

# some arbitrary numbers given - so that total is 1.. just to show this will not be the same as above
gas.trend2= filter(log(UKgas),filter = c(1/11,1/4,1/4,1/4,7/44),sides = 2)
lines(gas.trend2,col=5,lty=2,lwd=3)

gas.trend3= filter(log(UKgas),filter = c(1/5,5),sides = 2)
lines(gas.trend3,col=6,lty=3,lwd=5)

#Filtering techniques: Exponential Smoothing

install.packages("forecast")
library(forecast)

fit.ses=ses(UKgas)
summary(fit.ses)
plot(fit.ses)

holt.ses=holt(UKgas)
summary(holt.ses)
plot (holt.ses)

hw.ses=hw(UKgas,seasonal = "mult")
summary(hw.ses)
plot(hw.ses)
hw.ses$mean
##########################################################
# Application of MA filter and smoother:MACD

install.packages("TTR")
install.packages("quantmod")
library(TTR)
library(quantmod)

getSymbols("^BSESN",from="2019-01-01") 
head(BSESN,2)
barChart(BSESN,theme = chartTheme("white"))
addMACD( fast =  5,slow = 20,signal = 9,histogram = TRUE)


getSymbols("NSRGY",from="2009-01-01") 
head(NSRGY,2)

barChart(NSRGY,theme = chartTheme("white"))
addMACD( fast =  12,slow = 26,signal = 9,histogram = TRUE)

#########################################################################

# TSA for mean
#
#training Sample

T=length(UKgas)
gas.test=tail(UKgas,12)
gas.train=ts(UKgas[1:(T-12)],start = c(1960,1),frequency = 4)
head(gas.train)

# Stationary Time Series Models:
tm=c(1:length(gas.train))
gas.trnd=lm(log(gas.train)~tm)
summary(gas.trnd)

#Seasonality
gas.decomp=decompose(UKgas,type="mult")
plot(gas.decomp)
head(gas.decomp$random,4)

napos=which(is.na(gas.decomp$random))
gas.stoch=gas.decomp$random[-napos]



#Presence of Stoch Trend
acf(gas.stoch)
pacf(gas.decomp$random,na.action=na.pass)

#Test of stochastic trend
Box.test(gas.stoch,lag = 2,type = "Ljung")

#ARIMA TREND\
library(forecast)
arima.gas=auto.arima(gas.stoch)
summary(arima.gas)

arimares.gas=arima.gas$residuals

acf(arimares.gas,na.action = na.pass)
pacf(arimares.gas,na.action=na.pass)

# Confirmatory test for no leftover mean
Box.test(arimares.gas,lag = 20,type = "Ljung")


# Forecast:
gas.forecast=ts((gas.trnd$coefficients[1]+gas.trnd$coefficients[2]*c(97:108)),start=c(1984,1),frequency = 4)


#Seasonality Adjustment
SF=gas.decomp$seasonal[1:12]
forecast.nonstoch=exp(gas.forecast)*SF

#Stochastic Forecast
library(forecast)
forecast.stoch=ts(forecast(arima.gas,h=12)$mean,start=c(1984,1),freq=4)
forecast.gas=forecast.nonstoch+forecast.stoch

plot(gas.test,type="b")
lines(forecast.gas,col=2,lty=2)

######################################################

# Non-constant variance models
install.packages("fGarch")
library(fGarch)
library(quantmod)
library(forecast)

frm=Sys.Date()-240
getSymbols("^SSEC",from=as.Date("2019-01-01"))
SSEC$SSEC.Adjusted
T=nrow(SSEC)
SNP.train=GSPC[1:(T-30)]
SNP.test=GSPC[(T-29):T]

plot(SSEC$SSEC.Adjusted,type="l")

# Tracing Stochastic trend
acf(SSEC$SSEC.Adjusted,na.action=na.pass)
pacf(SSEC$SSEC.Adjusted,na.action=na.pass)

Box.test(SSEC$SSEC.Adjusted,lag=10,type="Ljung")

#ARIMA:
arima.SSEC=auto.arima(SSEC$SSEC.Adjusted)
summary(arima.SSEC)
# SSEC.adj(t)=SSEC.adj(t-1)+a(t): Random Walk

#Residuals
SSEC.res=arima.SSEC$residuals

acf(SSEC.res,na.action=na.pass)
pacf(SSEC.res,na.action=na.pass)

Box.test(SSEC.res,lag=10,type="Ljung")

# Test of ARCH effect
acf(SNP.res^2)
Box.test(SNP.res^2,lag=10,type="Ljung")

#Conditional Heteroscedastic model:
library(fGarch)
SNP.garch=garchFit(formula =~1+garch(1,1),data= SNP.res)

#volatility forecast
vol.SNP=sqrt(predict(SNP.garch,1)$meanForecast)

#Stochastic Volatility
library(stochvol)
draws <- svlsample(SNP.res, draws = 4000, burnin = 3000, priormu = c(-10, 1),priorphi = c(20, 1.2), priorsigma = 0.2,priorrho = c(1, 1))

para.summ=summary(draws$para)
vol.summ=summary(draws$latent)

h.future=numeric(180)
h.future[1]=para.summ$statistics[1,1]+para.summ$statistics[2,1]*(tail(vol.summ$statistics[,1],1)-para.summ$statistics[1,1])
for(i in 2:180){
  h.future[i]=para.summ$statistics[1,1]+para.summ$statistics[2,1]*(h.future[i-1]-para.summ$statistics[1,1])
}
SNP.volfor=exp(h.future/2)

library(qrmtools)
Black_Scholes(t=0,S = 2488.65,r = 0.04,sigma = vol.SNP,K = 2488.65,T = 1/252,type = "call")
