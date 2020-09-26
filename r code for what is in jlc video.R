sales <- scan ("http://robjhyndman.com/tsdldata/data/sales.dat")
head(sales)
salestimeseries <- ts(sales,frequency = 12,start=c(1987,1))
plot(salestimeseries)
salestimeseriesdecompose <- decompose(salestimeseries,type="multiplicative")
plot(salestimeseriesdecompose)

logsalestimeseries <- log(salestimeseries)
plot(logsalestimeseries)
logsalestimeseriesdecompose <- decompose(logsalestimeseries,type="additive")
plot(logsalestimeseriesdecompose)


## Simple exponential forecasting - when NO TREND or SEASONALITY (Only Random)
es <-ses(salestimeseries,h=10)
plot(es)
accuracy(es) #MAPE=0.507 - hence ok since it should be less than 7 or 7.3
plot.ts(es$residuals)
Box.test(es$residuals,lag=20,type="Ljung-Box") # p<0.05 - reject Null; data exhibits serial correlation; hence need ACF
acf(es$residuals) # see that there are significant spikes in residuals - that means there is trend or seasonality in the data
hist(es$residuals,col="red",freq=FALSE) # slightly right skewed - positive errors are not cancelling the negative errors
## alternative way to get residuals plot, acf and histogram
checkresiduals(es) 

## Holt function - considers TREND but NO SEASONALITY
library(forecast)
hol <- holt(salestimeseries,h=10)
plot(hol)
summary(hol) #MAPE=0.469, AIC=853.1295 - so getting better than ses
checkresiduals(hol) # p=0.5812 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
# hence no need to go for HW model since no seasonality being seen

## Holt-Winters function - consider for RANDOm, TREND, SEASONALITY
install.packages("forecast")
library(forecast)

## single exponential model in HW model
hwforecasts <- HoltWinters(salestimeseries,beta=NULL,gamma=NULL)
plot(hwforecasts)
summary(hwforecasts) # alpha=0.9999 (closer to 1 - so this means recent values taken)
plot(hwforecasts$fitted)
checkresiduals(hwforecasts) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2 <- forecast:::forecast.HoltWinters (hwforecasts,h=10)
plot(hwforecasts2)
summary(hwforecasts2) # alpha=0.556; beta=0.363
Box.test(hwforecasts2$residuals,lag=20,type="Ljung-Box") # p<0.05 - reject Null; data exhibits serial correlation; hence need ACF
checkresiduals(hwforecasts2) # p=0.5812 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
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
summary(hwforecasts2double) # alpha=0.556; beta=0.363; MAPE=0.622
Box.test(hwforecasts2double$residuals,lag=20,type="Ljung-Box") # p<0.05 - reject Null; data exhibits serial correlation; hence need ACF
checkresiduals(hwforecasts2double) # p=0.5812 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2double$residuals)
hist(hwforecasts2double$residuals)

## Triple exponential model in HW model
logsalestimeseries <- log(salestimeseries)
plot(logsalestimeseries)
logsalestimeseriesdecompose <- decompose(logsalestimeseries,type="additive")
plot(logsalestimeseriesdecompose)

hwforecaststriple <- HoltWinters(logsalestimeseries)
plot(hwforecaststriple)
summary(hwforecaststriple) # alpha=1, beta=1, gamma=1 (closer to 1 - so this means recent values taken)
plot(hwforecaststriple$fitted)
checkresiduals(hwforecaststriple) # p=0.001 - reject null, so there is some seasonality or trend though no lines cross in ACF

hwforecasts2triple <- forecast:::forecast.HoltWinters (hwforecaststriple,h=48)
plot(hwforecasts2triple)
summary(hwforecasts2triple) # alpha=0.553; beta=0.347, gamma=1; MAPE=0.116
Box.test(hwforecasts2triple$residuals,lag=20,type="Ljung-Box") # p<0.05 - reject Null; data exhibits serial correlation; hence need ACF
checkresiduals(hwforecasts2triple) # p=0.5812 - do not reject null - no serial autocorrelation depicted; histogram is symmetric and acf - no lines cross the blue line; no pattern exhibited in residuals plot
plot.ts(hwforecasts2triple$residuals)
Acf(hwforecasts2triple$residuals,lag.max = 20)
hist(hwforecasts2triple$residuals)

## overall not smoothened enough

## no need to know the components but we can use the ets function instead
fit <- ets(salestimeseries)
fcast <- forecast.ets(fit,h=48)
summary(fcast) # MAPE=0.459, alpha=0.96, beta=0.29, AIC=848.52

fit2 <- ets(logsalestimeseries)
fcast2 <- forecast.ets(fit2,h=48)
summary(fcast2) # MAPE=0.011, alpha=0.96, beta=0.29; AIC=-775.9

# train and test
str(salestimeseries)
tail(salestimeseries,12)
training <- window (salestimeseries, start = c(1987,1),end=c(1997,1),frequency=12)
testing <- window (salestimeseries,start=c(1997,2), end=c(1999,6),frequency=12)
tr <- ets(training)
test <- ets(testing,model=tr)
accuracy(test) # MAPE=0.257
accuracy(forecast(tr,48),testing)

## Operationalize BOX COX TRANSFORMATION
lam <- BoxCox.lambda(salestimeseries)
fitcox <- ets(salestimeseries,lambda = lam)
fcastcox <- forecast.ets(fitcox,h=48)
summary(fcastcox) # MAPE=0.46; AIC=2477

# do the same on log of salestimeseries
lam1 <- BoxCox.lambda(logsalestimeseries)
fitcox1 <- ets(salestimeseries,lambda = lam1)
fcastcox1 <- forecast.ets(fitcox1,h=48)
summary(fcastcox1) # MAPE=0.46; AIC=2477

## both give the same results

## ARIMA -- timeseries needs to be stationary (d), Auto-Rregressive (p - PACF), Moving Average (q - ACF)
plot(salestimeseries)
salestimeseriesdiff1 <- diff(salestimeseries,differences = 1)
plot(salestimeseriesdiff1) # not completely white noise
salestimeseriesdiff2 <- diff(salestimeseries,differences = 2)
plot(salestimeseriesdiff2) # better from a white noise perspective
# check for stationary - use Augmented Dickey fuller test (ADF) - p=0.01; ADF statistic should be -12.51 for pure white noise
install.packages("tseries")
library(tseries)
adf.test(salestimeseries) # p=0.53; DF= -2.11
adf.test(salestimeseriesdiff1) # p=0.06; DF= -3.35
adf.test(salestimeseriesdiff2) # p=0.01; DF= -6.562 # now stationary.. hence d=2

# to determine p and q - use pacf and acf plots respectively
install.packages("forecast")
library(forecast)
Acf(salestimeseriesdiff2,lag.max=20) # q=0
pacf(salestimeseriesdiff2,lag.max=20) # p=2, significant decrease after lag 2 (principe of parsimony - no more than 3), no sine wave pattern - hence q=0
salestimeseriesarima <- Arima(y=salestimeseries,order=c(2,2,0))
summary(salestimeseriesarima) # MAPE=0.49, AIC=532.3
salestimeseriesforecasts <- forecast (salestimeseriesarima, h=48)
plot(salestimeseriesforecasts)
Acf(salestimeseriesforecasts$residuals,lag.max=20) # 
plot.ts(salestimeseriesforecasts$residuals) # residuals looks random
Box.test(salestimeseriesforecasts$residuals,lag=20,type="Ljung-Box") # p>0.05 - hence p-value is insignificant and arima has done a good job of forecasting

## Shortcut for ARIMA - auto.arima
salestimeseriesautoarima <- auto.arima(salestimeseries) # give 1,1,1 for p,d,q; AIC=514.74 and BIC=523.75
summary(salestimeseriesautoarima) # MAPE=0.46

# since auto.arima does not transform - we need to take a log in this case
salestimeseriesautoarimalog <- auto.arima(log(salestimeseries)) # give 1,1,1 for p,d,q; AIC=514.74 and BIC=523.75
summary(salestimeseriesautoarimalog) # MAPE=0.085; AIC= -1098.95 - hence a btter model
salestimeseriesforecastsautoarimalog <- forecast (salestimeseriesautoarimalog, h=48)
plot(salestimeseriesforecastsautoarimalog)
Acf(salestimeseriesforecastsautoarimalog$residuals,lag.max=20) # all within the blue lines
plot.ts(salestimeseriesforecastsautoarimalog$residuals) # residuals looks random
Box.test(salestimeseriesforecastsautoarimalog$residuals,lag=20,type="Ljung-Box") # p>0.05 - hence p-value is insignificant and arima has done a good job of forecasting
accuracy(salestimeseriesforecastsautoarimalog)


