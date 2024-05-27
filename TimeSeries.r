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
  library(MASS)
  library(fGarch)
  library(fracdiff)
}



#importing data

ts_raw<-read_excel("FEDFUNDS.xlsx")

ts_raw$observation_date <- substr(ts_raw$observation_date, 1, 7) #remove the day
nchar(ts_raw$observation_date)[nchar(ts$observation_date) != 7] #all the data has 7 characters: aaaa-mm

#creating a time series object
ts <- ts(ts_raw$FEDFUNDS, start = c(as.yearmon("1954-07")), end = c(as.yearmon("2024-02")), frequency = 12)


#EDA
autoplot(ts)
acf(ts, lag.max = 100)
pacf(ts, lag.max = 100)
adf.test(ts) # p value>0.05, the time series is not stationary
autoplot(diff(ts))
autoplot(diff(log(ts)))
plot(stl(ts, s.window="periodic")) #no season, for economic series we don't expect any deterministic trend



#ARIMA (1,1,1) ON TS, forecast h=5
h<-5
train<-1:750
test<-751:836
fit.is<-Arima(ts[train], order=c(1,1,1), include.mean = F)
n<-length(test)
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
adf.test(ts_diff) # not stationary
h<-5
train<-1:750
test<-751:836
fit.is<-Arima(ts_diff[train], order=c(1,1,1), include.mean = F)
n<-length(test)
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

order<-fracdiff(ts, nar = 1, nma = 1)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1), include.mean = F,arfima=T), 
                  fixed.pars=list(arfima =order$d ), distribution.model = "std")
fit <- ugarchfit(spec, data = ts[train])


ut<-fit@fit$residuals/fit@fit$sigma #this are the standardized residuals, , in this series the mean is=0
#fit a distribution for ut
plot(density(ut))
lines(seq(-6,6,by=0.01),dstd(seq(-6,6,by=0.01),0,1,4),col=2)
#residual are t-student distributed with 3 degrees of freedom, we will use this to calculate the confidence interval

plot(fit, which = 1)
acf(ut)

#forecast
h<-5
n <- length(test)
pred <- rep(NA, n)
up_int<-rep(NA, n)
low_int<-rep(NA, n)


spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1,1), include.mean = F, arfima=T), 
                  fixed.pars=list(arfima =order$d ), distribution.model = "std")
fit <- ugarchfit(spec, data=ts[train])

for (i in 1:n) {
  train_up <- c(train, test[1:i])
  fit_coef<-getspec(fit)
  setfixed(fit_coef)<-as.list(coef(fit));
  forecast_up <- ugarchforecast(fit_coef, data=ts[train_up],n.ahead = h)
  pred[i] <- forecast_up@forecast$seriesFor[h]
  sigma<- forecast_up@forecast$sigmaFor[h]
  up_int[i] <- pred[i] + qstd(0.975, mean=0, sd=1,nu=4) * sigma
  low_int[i] <- pred[i] - qstd(0.975, mean=0, sd=1,nu=4) * sigma
}
plot(ts[test], type = "l")
lines(pred, col = "red")
lines(up_int, col = "blue", lty = 2)
lines(low_int, col = "blue", lty = 2)


rmse <- sqrt(mean((ts[test] - pred)^2))
rmse

#fit final model
fit_final <- ugarchfit(spec, data = ts)

#plot autocorrelation of (standardized) squared residuals to compare with N(0,1)
acf(abs(fit_final@fit$residuals / fit_final@fit$sigma))
plot(density(fit_final@fit$residuals / fit_final@fit$sigma))
lines(seq(-6,6,by=0.01),dstd(seq(-6,6,by=0.01),0,1,4),col=2)

#one year forecast
forecast_final <- ugarchforecast(fit_final, n.ahead = 12)
forecast_values <- forecast_final@forecast$seriesFor
forecast_se <- forecast_final@forecast$sigmaFor

#confidence interval 95%
alpha <- 0.05
z_score <- qstd(1 - alpha / 2, mean = 0, sd = 1,nu=4) 
forecast_values_upper <- forecast_values + z_score * forecast_se
forecast_values_lower <- forecast_values - z_score * forecast_se

#plot last 30 values of the time series and the forecast
plot_length <- 30
plot_start <- length(ts) - plot_length + 1
plot_end <- length(ts)+20

plot(ts[plot_start:plot_end], type = "l", col = "blue", ylab = "Value", xlab = "Time", ylim = c(0, 10))
lines(c(rep(NA, 30), forecast_values), col = "red")
+lines(c(rep(NA, 30), forecast_values_upper), col = "green", lty = 2)
lines(c(rep(NA, 30), forecast_values_lower), col = "green", lty = 2)
legend("topleft", legend = c("Time Series", "Forecast", "Confidence Interval"), col = c("blue", "red", "green"), lty = c(1, 1, 2))
