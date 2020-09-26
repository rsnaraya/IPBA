## Case study

## Business Problem
## User: Carl Lipke - Marketing VP of a propane-gas distributor
## Type of business: Propone gas distribution
## Problem Statement: Forecast of sales on a quarterly basis and asks for a time-series decomposition model
## Data: 1996 to 2007. Propane Gas Sales in Millions of Pounds (total at end of each quarter)

## Clean up and read the source data
rm(list=ls())
gapsales<-read.csv("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Analytics course//IPBA//Time Series Analysis//GAP.csv",head=T)

## have a look at the data
head(gapsales)

## change the column names for easier processing
colnames(gapsales)[1] <-"SalesQtr"
colnames(gapsales)[2] <-"Sales"

## continue the quick data check
str(gapsales)
class(gapsales)
summary(gapsales)
## which colums have null values and if missing values have to be treated
colSums(is.na(gapsales))
## indicates no missing values. 

salestimeseries <- ts(gapsales[2],frequency = 4,start=c(1985,1))
plot(salestimeseries)
salestimeseriesdecompose <- decompose(salestimeseries,type="multiplicative")
plot(salestimeseriesdecompose)

## since the series is multiplicative, we need to make this additive. And we do that by using the log function
logsalestimeseries <- log (salestimeseries)
plot(logsalestimeseries)
logsalestimeseriesdecompose <- decompose(logsalestimeseries,type="additive")
plot(logsalestimeseriesdecompose)

## Simple exponential forecasting - when NO TREND or SEASONALITY (Only Random)
es <-ses(logsalestimeseries,h=10)
plot(es)
accuracy(es) #MAPE=Inf - hence near ZERO amd is ok since it should be less than 7 or 7.3
plot.ts(es$residuals) 
Box.test(es$residuals,lag=20,type="Ljung-Box") # p (0.022)<0.05 - reject Null; data exhibits serial correlation; hence need ACF
acf(es$residuals) # see that there are significant spikes in residuals - that means there is trend or seasonality in the data
hist(es$residuals,col="red",freq=FALSE) # slightly right skewed - positive errors are not cancelling the negative errors
## alternative way to get residuals plot, acf and histogram
checkresiduals(es) 

## Holt function - considers TREND but NO SEASONALITY
library(forecast)
hol <- holt(logsalestimeseries,h=10)
plot(hol)
summary(hol) #MAPE=Inf, AIC=550.562
checkresiduals(hol) # p=1.367e-05 < 0.05 - do not reject null -  serial autocorrelation depicted; histogram is skewed left  and acf - lines cross the blue line; no pattern exhibited in residuals plot
# hence need to go for HW model since seasonality being seen

## Holt-Winters function - consider for RANDOm, TREND, SEASONALITY
install.packages("forecast")
library(forecast)

## single exponential model in HW model
hwforecasts <- HoltWinters(logsalestimeseries,beta=NULL,gamma=NULL)
plot(hwforecasts)
summary(hwforecasts) # alpha=0.9999 (closer to 1 - so this means recent values taken)
plot(hwforecasts$fitted)
checkresiduals(hwforecasts) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF
hwforecasts$residuals
Box.test(hwforecasts$residuals,lag=20,type="Ljung-Box") # null values

hwforecasts2 <- forecast:::forecast.HoltWinters (hwforecasts,h=10)
plot(hwforecasts2)
summary(hwforecasts2) # alpha=0.254; beta=0.019 - no recent effect; gamma=0.52 - some recent effect
Box.test(hwforecasts2$residuals,lag=20,type="Ljung-Box") # p (0.271)>0.05 - reject Null; data DOES NOT exhibits serial correlation - data is smoothened
checkresiduals(hwforecasts2) # p=0.271 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
## alternative way of the doing the same below
##hwforecasts3 <- forecast(hwforecasts,h=10)
##plot(hwforecasts3)
##summary(hwforecasts3) # alpha=0.556; beta=0.363

## Double exponential model in HW model
hwforecastsdouble <- HoltWinters(logsalestimeseries,gamma=NULL)
plot(hwforecastsdouble)
summary(hwforecastsdouble) # alpha=1, beta=1, gamma=1 (closer to 1 - so this means recent values taken)
plot(hwforecastsdouble$fitted)
checkresiduals(hwforecastsdouble) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2double <- forecast:::forecast.HoltWinters (hwforecastsdouble,h=10)
plot(hwforecasts2double)
summary(hwforecasts2double) # alpha=0.255; beta=0.019; gamma=0.52; MAPE=Inf
Box.test(hwforecasts2double$residuals,lag=20,type="Ljung-Box") # p (0.271)>0.05 - do not reject Null; data DOES NOT exhibits serial correlation; hence NO need ACF
checkresiduals(hwforecasts2double) # p=0.271 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2double$residuals)
hist(hwforecasts2double$residuals)

## Triple exponential model in HW model
logsalestimeseries <- log(salestimeseries)
hwforecaststriple <- HoltWinters(logsalestimeseries)
plot(hwforecaststriple)
summary(hwforecaststriple) # alpha=1, beta=1, gamma=1 (closer to 1 - so this means recent values taken)
plot(hwforecaststriple$fitted)
checkresiduals(hwforecaststriple) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2triple <- forecast:::forecast.HoltWinters (hwforecaststriple,h=10)
plot(hwforecasts2triple)
summary(hwforecasts2triple) # alpha=0.254; beta=0.019, gamma=0.52; MAPE=Inf
Box.test(hwforecasts2triple$residuals,lag=20,type="Ljung-Box") # p<0.05 - reject Null; data exhibits serial correlation; hence need ACF
checkresiduals(hwforecasts2triple) # p=0.271 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2triple$residuals)
Acf(hwforecasts2triple$residuals,lag.max = 20)
hist(hwforecasts2triple$residuals)

## overall not smoothened enough

## no need to know the components but we can use the ets function instead
fit <- ets(logsalestimeseries)
fcast <- forecast.ets(fit,h=10)
summary(fcast) # MAPE=0.459, alpha=0.96, beta=0.29, AIC=848.52

fit2 <- ets(logsalestimeseries)
fcast2 <- forecast.ets(fit2,h=48)
summary(fcast2) # MAPE=Inf, alpha=0.3173; AIC=546.77

# train and test
str(salestimeseries)
tail(salestimeseries,12)
training <- window (logsalestimeseries, start = c(1985,1),end=c(2010,4),frequency=4)
testing <- window (logsalestimeseries,start=c(2011,1), end=c(2017,4),frequency=4)
tr <- ets(training)
test <- ets(testing,model=tr)
accuracy(test) # MAPE=4.39
accuracy(forecast(tr,10),testing) # test MAPE - 6.01 which is well within 7 or 7.3

## Operationalize BOX COX TRANSFORMATION
lam <- BoxCox.lambda(salestimeseries)
fitcox <- ets(salestimeseries,lambda = lam)
fcastcox <- forecast.ets(fitcox,h=10)
summary(fcastcox) # MAPE=152.73; AIC=617.3364

# do the same on log of salestimeseries
lam1 <- BoxCox.lambda(logsalestimeseries)
fitcox1 <- ets(logsalestimeseries,lambda = lam1)
fcastcox1 <- forecast.ets(fitcox1,h=10)
summary(fcastcox1) # MAPE=Inf; AIC=815.9749

## both give the same results

## ARIMA -- timeseries needs to be stationary (d), Auto-Rregressive (p - PACF), Moving Average (q - ACF)
logsalestimeseries <- log(salestimeseries)
plot(logsalestimeseries)
Acf(logsalestimeseries,lag.max=20) # sine curve, q=3


salestimeseriesdiff1 <- diff(logsalestimeseries,differences = 1)
plot(salestimeseriesdiff1) # not completely white noise
salestimeseriesdiff2 <- diff(logsalestimeseries,differences = 2)
plot(salestimeseriesdiff2) # better from a white noise perspective
salestimeseriesdiff3 <- diff(logsalestimeseries,differences = 3)
plot(salestimeseriesdiff3) # better from a white noise perspective
# check for stationary - use Augmented Dickey fuller test (ADF) - p=0.01; ADF statistic should be -12.51 for pure white noise
install.packages("tseries")
library(tseries)
adf.test(logsalestimeseries) # p=0.53; DF= -2.11
adf.test(salestimeseriesdiff1) # p=0.01; DF= -5.3722
adf.test(salestimeseriesdiff2) # p=0.01; DF= -6.561 # now stationary.. 
adf.test(salestimeseriesdiff3) # p=0.01; DF= -12.102 # now stationary.. hence d=3

# to determine p and q - use pacf and acf plots respectively
install.packages("forecast")
library(forecast)
Acf(salestimeseriesdiff3,lag.max=20) # sine curve, q=3

pacf(salestimeseriesdiff3,lag.max=20) # p=3, significant decrease after lag 3 (principe of parsimony - no more than 3), no sine wave pattern - hence q=0
salestimeseriesarima <- Arima(y=logsalestimeseries,order=c(3,2,3))
summary(salestimeseriesarima) # MAPE=Inf, AIC=28.89
salestimeseriesforecasts <- forecast (salestimeseriesarima, h=10)
plot(salestimeseriesforecasts)
Acf(salestimeseriesforecasts$residuals,lag.max=20) # 
plot.ts(salestimeseriesforecasts$residuals) # residuals looks random
Box.test(salestimeseriesforecasts$residuals,lag=20,type="Ljung-Box") # p (0.6566)>0.05 - hence p-value is insignificant and arima has done a good job of forecasting

## Shortcut for ARIMA - auto.arima
salestimeseriesautoarima <- auto.arima(salestimeseries) # give 1,1,1 for p,d,q; AIC=514.74 and BIC=523.75
summary(salestimeseriesautoarima) # MAPE=183.98

# since auto.arima does not transform - we need to take a log in this case
salestimeseriesautoarimalog <- auto.arima(log(salestimeseries)) # give 1,1,1 for p,d,q; AIC=514.74 and BIC=523.75
summary(salestimeseriesautoarimalog) # MAPE=Inf; AIC= 270.06 - hence a btter model
salestimeseriesforecastsautoarimalog <- forecast (salestimeseriesautoarimalog, h=10)
plot(salestimeseriesforecastsautoarimalog)
Acf(salestimeseriesforecastsautoarimalog$residuals,lag.max=20) # all within the blue lines
plot.ts(salestimeseriesforecastsautoarimalog$residuals) # residuals looks random
Box.test(salestimeseriesforecastsautoarimalog$residuals,lag=20,type="Ljung-Box") # p (0.5178)>0.05 - hence p-value is insignificant and arima has done a good job of forecasting
accuracy(salestimeseriesforecastsautoarimalog) # MAPE=Inf


