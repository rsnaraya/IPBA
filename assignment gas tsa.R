## Case study

## Business Problem
## User: Carl Lipke - Marketing VP of a propane-gas distributor
## Type of business: Propone gas distribution
## Problem Statement: Forecast of sales on a quarterly basis and asks for a time-series decomposition model
## Data: 1996 to 2007. Propane Gas Sales in Millions of Pounds (total at end of each quarter)

## Clean up and read the source data
rm(list=ls())
gassales<-read.csv("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Analytics course//IPBA//Time Series Analysis//GAS.csv",head=T)

## have a look at the data
head(gassales)

## change the column names for easier processing
colnames(gassales)[1] <-"SalesQtr"
colnames(gassales)[2] <-"Sales"

## continue the quick data check
str(gassales)
class(gassales)
summary(gassales)
## which colums have null values and if missing values have to be treated
colSums(is.na(gassales))
## indicates no missing values. 

salestimeseries <- ts(gassales[2],frequency = 4,start=c(1996,1))
plot(salestimeseries)
salestimeseriesdecompose <- decompose(salestimeseries,type="additive")
plot(salestimeseriesdecompose)
summary(salestimeseriesdecompose)
salestimeseriesdecompose$seasonal
salestimeseriesdecompose$trend

salestimeseriesdecompose$trend[,1]
summary(salestimeseriesdecompose$trend[,1])
summary(salestimeseriesdecompose$trend[13])

hw.ses=hw(salestimeseries,seasonal = "mult")
summary(hw.ses)
plot(hw.ses)
hw.ses$mean


## since the series is multiplicative, we need to make this additive. And we do that by using the log function
logsalestimeseries <- log (salestimeseries)
plot(logsalestimeseries)
logsalestimeseriesdecompose <- decompose(logsalestimeseries,type="additive")
plot(logsalestimeseriesdecompose)

## no need to transform since the series is additive

## Simple exponential forecasting - when NO TREND or SEASONALITY (Only Random)
es <-ses(salestimeseries,h=10)
plot(es)
accuracy(es) #MAPE=18.587 - NOT ok since it should be less than 7 or 7.3
plot.ts(es$residuals) 
Box.test(es$residuals,lag=20,type="Ljung-Box") # p (2.2e-16)<0.05 - reject Null; data exhibits serial correlation; hence need ACF
acf(es$residuals) # see that there are significant spikes in residuals - that means there is trend or seasonality in the data
hist(es$residuals,col="red",freq=FALSE) #  skewed left - positive errors are not cancelling the negative errors
## alternative way to get residuals plot, acf and histogram
checkresiduals(es) 

## Holt function - considers TREND but NO SEASONALITY
library(forecast)
hol <- holt(salestimeseries,h=10)
plot(hol)
summary(hol) #MAPE=18.15, AIC=191.54
Box.test(hol$residuals,lag=20,type="Ljung-Box") # p (2.2e-16)<0.05 - reject Null; data exhibits serial correlation; hence need ACF
checkresiduals(hol) # p=2.2e-16 < 0.05 - do not reject null -  serial autocorrelation depicted; histogram is skewed left  and acf - lines cross the blue line; no pattern exhibited in residuals plot
# hence need to go for HW model since seasonality being seen

## Holt-Winters function - consider for RANDOm, TREND, SEASONALITY
install.packages("forecast")
library(forecast)

## single exponential model in HW model
hwforecasts <- HoltWinters(salestimeseries,beta=NULL,gamma=NULL)
plot(hwforecasts)
summary(hwforecasts) # alpha=1 (closer to 1 - so this means recent values taken)
plot(hwforecasts$fitted)
checkresiduals(hwforecasts) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF
hwforecasts$residuals

hwforecasts2 <- forecast:::forecast.HoltWinters (hwforecasts,h=10)
plot(hwforecasts2)
summary(hwforecasts2) # alpha=0.55; beta=0.083 - no recent effect; gamma=0.65 - some recent effect
# MAPE= 4.507
Box.test(hwforecasts2$residuals,lag=20,type="Ljung-Box") # p (0.9948)>0.05 - reject Null; data DOES NOT exhibits serial correlation - data is smoothened
checkresiduals(hwforecasts2) # p=0.9948 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
## alternative way of the doing the same below
##hwforecasts3 <- forecast(hwforecasts,h=10)
##plot(hwforecasts3)
##summary(hwforecasts3) # alpha=0.556; beta=0.363

## Double exponential model in HW model
hwforecastsdouble <- HoltWinters(salestimeseries,gamma=NULL)
plot(hwforecastsdouble)
summary(hwforecastsdouble) # alpha=1, beta=1, gamma=1 (closer to 1 - so this means recent values taken)
plot(hwforecastsdouble$fitted)
checkresiduals(hwforecastsdouble) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2double <- forecast:::forecast.HoltWinters (hwforecastsdouble,h=10)
plot(hwforecasts2double)
summary(hwforecasts2double) # alpha=0.55; beta=0.083; gamma=0.65; MAPE=4.507
Box.test(hwforecasts2double$residuals,lag=20,type="Ljung-Box") # p (0.9948)>0.05 - do not reject Null; data DOES NOT exhibits serial correlation; hence NO need ACF
checkresiduals(hwforecasts2double) # p=0.9948 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2double$residuals)
hist(hwforecasts2double$residuals)

## Triple exponential model in HW model
hwforecaststriple <- HoltWinters(salestimeseries)
plot(hwforecaststriple)
summary(hwforecaststriple) # alpha=1, beta=1, gamma=1 (closer to 1 - so this means recent values taken)
plot(hwforecaststriple$fitted)
checkresiduals(hwforecaststriple) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2triple <- forecast:::forecast.HoltWinters (hwforecaststriple,h=10)
plot(hwforecasts2triple)
summary(hwforecasts2triple) # alpha=0.55; beta=0.083, gamma=0.65; MAPE=4.507
Box.test(hwforecasts2triple$residuals,lag=20,type="Ljung-Box") # p (0.9948)>0.05 - reject Null; data DOES NOT exhibits serial correlation; hence need ACF
checkresiduals(hwforecasts2triple) # p=0.271 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2triple$residuals)
Acf(hwforecasts2triple$residuals,lag.max = 20)
hist(hwforecasts2triple$residuals)

## overall  smoothened enough

## no need to know the components but we can use the ets function instead
fit <- ets(salestimeseries)
fcast <- forecast.ets(fit,h=10)
summary(fcast) # MAPE=4.028, alpha=0.7032, AIC=58.248

# train and test
str(salestimeseries)
tail(salestimeseries,12)
training <- window (salestimeseries, start = c(1996,1),end=c(2001,4),frequency=4)
testing <- window (salestimeseries,start=c(2002,1), end=c(2007,4),frequency=4)
tr <- ets(training)
test <- ets(testing,model=tr)
accuracy(test) # MAPE=3.927
accuracy(forecast(tr,10),testing) # test MAPE = 3.23 which is well within 7 or 7.3

## Operationalize BOX COX TRANSFORMATION
lam <- BoxCox.lambda(salestimeseries)
fitcox <- ets(salestimeseries,lambda = lam)
fcastcox <- forecast.ets(fitcox,h=10)
summary(fcastcox) # MAPE=4.26; AIC=194.59

## ARIMA -- timeseries needs to be stationary (d), Auto-Rregressive (p - PACF), Moving Average (q - ACF)
plot(salestimeseries)
salestimeseriesdiff1 <- diff(salestimeseries,differences = 1)
plot(salestimeseriesdiff1) # not completely white noise
salestimeseriesdiff2 <- diff(salestimeseries,differences = 20)
plot(salestimeseriesdiff2) # better from a white noise perspective
# check for stationary - use Augmented Dickey fuller test (ADF) - p=0.01; ADF statistic should be -12.51 for pure white noise
install.packages("tseries")
library(tseries)
adf.test(salestimeseries) # p=0.7171; DF= -1.6419
adf.test(salestimeseriesdiff1) # p=0.032; DF= -3.7349
adf.test(salestimeseriesdiff2) # p=0.01; DF= -9.3611 # now stationary.. hence d=2

# to determine p and q - use pacf and acf plots respectively
install.packages("forecast")
library(forecast)
Acf(salestimeseriesdiff2,lag.max=20) # sine curve, q=20
pacf(salestimeseriesdiff2,lag.max=20) # p=3, significant decrease after lag 3 (principe of parsimony - no more than 3), no sine wave pattern - hence q=0
salestimeseriesarima <- Arima(y=salestimeseriesdiff2,order=c(3,2,3))
summary(salestimeseriesarima) # MAPE=2.976, AIC=47.84
salestimeseriesforecasts <- forecast (salestimeseriesarima, h=10)
plot(salestimeseriesforecasts)
Acf(salestimeseriesforecasts$residuals,lag.max=20) # 
plot.ts(salestimeseriesforecasts$residuals) # residuals looks random
Box.test(salestimeseriesforecasts$residuals,lag=10,type="Ljung-Box") # p (1)>0.05 - hence p-value is insignificant and arima has done a good job of forecasting

## Shortcut for ARIMA - auto.arima
salestimeseriesautoarima <- auto.arima(salestimeseries) # give 1,1,1 for p,d,q; AIC=514.74 and BIC=523.75
summary(salestimeseriesautoarima) # MAPE=3.396; AIC=11.63

salestimeseriesforecastsautoarima <- forecast (salestimeseriesautoarima, h=10)
plot(salestimeseriesforecastsautoarima)
Acf(salestimeseriesforecastsautoarima$residuals,lag.max=20) # all within the blue lines
plot.ts(salestimeseriesforecastsautoarima$residuals) # residuals looks random
Box.test(salestimeseriesforecastsautoarima$residuals,lag=20,type="Ljung-Box") # p (0.9834)>0.05 - hence p-value is insignificant and arima has done a good job of forecasting
accuracy(salestimeseriesforecastsautoarima) # MAPE=3.396


