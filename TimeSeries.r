######TIME SERIES
remove(list = ls())
{
  library(ggfortify)
  library(forecast)
  library(tseries)
  library(car)
  library(zoo)
  library(readxl)
  library(rugarch)
}



#importing data

ts_raw<-read_excel("FEDFUNDS.xlsx")

ts_raw$observation_date <- substr(ts_raw$observation_date, 1, 7) #remove the day
nchar(ts$observation_date)[nchar(ts$observation_date) != 7] #all the data has 7 characters: aaaa-mm

#creating a time series object
ts <- ts(ts_raw$FEDFUNDS, start = c(as.yearmon("1954-07")), end = c(as.yearmon("2024-02")), frequency = 12)


#EDA
autoplot(ts)
acf(ts, lag.max = 100)
adf.test(ts) # p value>0.05, the time series is not stationary
autoplot(diff(ts))
autoplot(diff(log(ts)))
plot(stl(ts, s.window="periodic")) #no season, for economic series we don't expect any deterministic trend



#ARIMA (1,1,1) ON TS, forecast h=5
h<-5
train<-1:750
test<-751:836
fit.is<-Arima(ts[train], order=c(1,1,1), include.mean = F)
n<-length(test)-h
pred<-rep(NA, n)
for (i in 1:n){
  train_up<-c(train, test[1:i])
  fit_up<-Arima(ts[train_up], model=fit.is)
  pred[i]<-forecast(fit_up, h=h)$mean[h]
}
plot(ts[test], type="l")
lines(pred, col="red")
acf(residuals(fit.is))
checkresiduals(fit.is)


rmse <- sqrt(mean((ts[test] - mean(pred))^2))
rmse


#ARMA (1,1) ON log(ts), forecast h=5
ts_diff<-log(ts)
adf.test(ts_diff) #not stationary
h<-5
train<-1:750
test<-751:836
fit.is<-Arima(ts_diff[train], order=c(1,1,1), include.mean = F)
n<-length(test)-h
pred<-rep(NA, n)
for (i in 1:n){
  train_up<-c(train, test[1:i])
  fit_up<-Arima(ts_diff[train_up], model=fit.is)
  pred[i]<-forecast(fit_up, h=h)$mean[h]
}
plot(ts_diff[test], type="l")
lines(pred, col="red")
acf(residuals(fit.is))
checkresiduals(fit.is)


rmse <- sqrt(mean((ts[test] - mean(pred))^2))
rmse

#ARMA (1,1) ON ts_diff + GARCH(1,1), forecast h=5
train <- 1:750
test <- 751:836
ts_diff <- diff(ts)

ARIMA <- Arima(ts_diff, order = c(1, 0, 1))
acf(residuals(ARIMA)^2) #still some correlation, we wanna try to model the variance


spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1), include.mean = F))
fit <- ugarchfit(spec, data = ts_diff[train])


plot(fit, which = 1)
autoplot(residuals(fit))
acf(residuals(fit))

#forecast
h <- 1
n <- length(test) - h
pred <- rep(NA, n)

for (i in 1:n) {
  train_up <- c(train, test[1:i])
  fit_up <- ugarchfit(spec, data = ts_diff[train_up])
  forecast_up <- ugarchforecast(fit_up, n.ahead = h)
  pred[i] <- forecast_up@forecast$seriesFor[h]
  # Rimuovi la differenziazione
  pred[i] <- ts[train_up[length(train_up)]] + pred[i]
}
plot(ts[test], type = "l")
lines(pred, col = "red")

rmse <- sqrt(mean((ts[test] - mean(pred))^2))
rmse
