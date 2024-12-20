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

###########IMPORT DATA AND CREATE A TS OBJECT
{
ts_raw<-read_excel("FEDFUNDS.xlsx")
#we have montlhly data, so we remove the day (which is always 01). From aaaa-mm-dd to aaaa-mm.
ts_raw$observation_date <- substr(ts_raw$observation_date, 1, 7)
nchar(ts_raw$observation_date)[nchar(ts_raw$observation_date) != 7]
#creating a time series object
ts<-ts(ts_raw$FEDFUNDS, start = c(as.yearmon("1954-07")), end = c(as.yearmon("2024-02")), frequency = 12)
}

############EDA
{
autoplot(ts)
acf(diff(ts), lag.max = 100)
pacf(diff(ts), lag.max = 100)
adf.test(ts) # p value>0.05. We not rejct H0: the ts is not stationary.
#differenciating
autoplot(diff(ts))
autoplot(diff(log(ts)))
plot(stl(ts, s.window="periodic")) #no season, for economic series we don't expect any deterministic trend
}

#ARIMA (0,1,0)
fit.is<-Arima(ts, order=c(0,1,0), include.mean = F)
AIC(fit.is)
#ARIMA (1,1,0)
fit.is<-Arima(ts, order=c(1,1,0), include.mean = F)
AIC(fit.is)
#ARIMA (0,1,1)
fit.is<-Arima(ts, order=c(0,1,1), include.mean = F)
AIC(fit.is)
#ARIMA (1,1,1)
fit.is<-Arima(ts, order=c(1,1,1), include.mean = F)
AIC(fit.is)


#ARIMA (1,1,1) to forecast h=12
{
h<-12
train<-1:750 #train from 1954-07 to 2016-12
test<-751:836 #test from 2017-01 to 2024-02
#fit the Arima model on train
fit.is<-Arima(ts[train], order=c(1,1,1), include.mean = F)
n<-length(test)
pred<-rep(NA, n) 

for (i in 1:n){
  train_up<-c(train, test[1:i])
  fit_up<-Arima(ts[train_up], model=fit.is)
  pred[i]<-forecast(fit_up, h=h)$mean[1] #prediction from 763, so 86 prediction
}
#pred[1]: con 751 prediciamo 763, con 752 prediciamo 764, ecc
#pred[74]: con 824 prediciamo 836 (ultimo valore test)
plot(ts[test], type="l")
lines(pred, col="red")
legend("topleft", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = c(1, 1))

acf(residuals(fit.is))
checkresiduals(fit.is)

rmse <- sqrt(mean((ts[test[(h+1):n]] - pred[1:(n-h)]))^2)
rmse
}

#ARMA (1,1) on log(ts) forecast h=12
{
ts_log<-log(ts)
adf.test(ts_log) # p value>0.05. We not rejct H0: the ts is not stationary.
h<-12
train<-1:750
test<-751:836
fit.is<-Arima(ts_log[train], order=c(1,1,1), include.mean = F)
n<-length(test)
pred<-rep(NA, n)
for (i in 1:n){
  train_up<-c(train, test[1:i])
  fit_up<-Arima(ts_log[train_up], model=fit.is)
  pred[i]<-forecast(fit_up, h=h)$mean[h]
}
plot(ts_log[test], type="l")
lines(pred, col="red")
acf(residuals(fit.is))
checkresiduals(fit.is)

<<<<<<< HEAD
rmse <- sqrt(mean((ts[test[(h+1):n]] - (pred[1:(n-h)]))^2)
=======
rmse <- sqrt(mean((ts[test[(h+1):n]] - (pred[1:(n-h)]))^2))
>>>>>>> 82a8945f8a8bbb31b5d5627f9682ad700c633508
rmse
}

#ARFIMA+GARCH, forecast h=12 and prediction interval 95%
{
train <- 1:750
test <- 751:836

ARIMA <- Arima(ts, order = c(1, 1, 1))
acf(residuals(ARIMA)^2) #still some correlation, we want to try to model the variance usign GARCH

order<-fracdiff(ts, nar = 1, nma = 1)
order$d
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1), include.mean = F,arfima=T), 
                   fixed.pars=list(arfima=order$d), distribution.model = "std")
fit <- ugarchfit(spec, data = ts[train])

ut<-fit@fit$residuals/fit@fit$sigma #standardized residuals
#fit a distribution for ut:
plot(density(ut))
lines(seq(-6,6,by=0.01),dstd(seq(-6,6,by=0.01),0,1,4),col=2)
#residual are t-student distributed with 4 degrees of freedom, 
#we will use this to compute prediction interval
acf(abs(ut)) #there is no more autocorrelation

#FORECAST + PREDICTION INTERVAL 95%
h<-12	
n <- length(test)
pred <- rep(NA, n) #86 prediction, from 763
up_int<-rep(NA, n)
low_int<-rep(NA, n)

#we used fixed coefficients to ease the computation
fit_coef<-getspec(fit)
setfixed(fit_coef)<-as.list(coef(fit))
for (i in 1:n) {
  train_up <- c(train, test[1:i])
  forecast_up <- ugarchforecast(fit_coef, data=ts[train_up],n.ahead = h)
  pred[i] <- forecast_up@forecast$seriesFor[h]
  sigma<- forecast_up@forecast$sigmaFor[h]
  up_int[i] <- pred[i] + qstd(0.975, mean=0, sd=1,nu=4) * sigma 
  low_int[i] <- pred[i] - qstd(0.975, mean=0, sd=1,nu=4) * sigma
}
rmse <- sqrt(mean((ts[test[(h+1):n]] - pred[1:(n-h)]))^2)
rmse

#plot:
<<<<<<< HEAD
plot(ts[test], type = "l", ylim=c(-1,6), xaxt="n", xlab="Date", ylab="FED interest rate",
=======
plot(ts[test], type = "l", ylim=c(0,6), xaxt="n", xlab="Date", ylab="FED interest rate",
>>>>>>> 82a8945f8a8bbb31b5d5627f9682ad700c633508
     main="Actual vs Predicted Time Series")
polygon(c(seq_along(up_int), rev(seq_along(low_int))),
        c(up_int, rev(low_int)), col = "#C6E9F9", border = NA)
lines(ts[test], type = "l", col = "black", lwd=2.5)
lines(pred, col = "red", lwd=1.5)
lines(up_int, col = "blue", lty = 2)
lines(low_int, col = "blue", lty = 2)
dates <- seq(from=as.Date("1954-07-01"), to=as.Date("2025-12-01"), by = "month")
dates <- format(dates, "%Y-%m")
axis(1, at = seq(1, length(dates[test]), by=12), 
     labels = dates[test][seq(1, length(dates[test]), by =12)])
legend("topleft", legend = c("Actual", "Predicted", "Prediction Interval"), col = c("black", "red", "blue"), lty = c(1, 1, 2))
}

#FORECAST IN THE FUTURE WITH ARFIMA+GARCH
{
#fit final model on overall time series
fit_final <- ugarchfit(spec, data = ts)

#plot autocorrelation of abs standardized residual
acf(abs(fit_final@fit$residuals / fit_final@fit$sigma)) #No autocorrelation.
#compare distribution of standardized residual with a Standardized Student-t distribution with 4df.
plot(density(fit_final@fit$residuals / fit_final@fit$sigma))
lines(seq(-6,6,by=0.01),dstd(seq(-6,6,by=0.01),0,1,4),col=2)

#one year forecast in the future
forecast_final <- ugarchforecast(fit_final, n.ahead = 12)
forecast_values <- forecast_final@forecast$seriesFor
forecast_se <- forecast_final@forecast$sigmaFor

#prediction interval 95%
q <- qstd(0.975, mean = 0, sd = 1,nu=4) 
forecast_values_upper <- forecast_values + q * forecast_se
forecast_values_lower <- forecast_values - q * forecast_se

#plot last values of the time series, the future forecast and the prediction interval
plot_length <- 36
plot_start <- length(ts) - (plot_length-12)
plot_end <- length(ts) + 12 #848, so 12 months after the end of ts

plot(ts[plot_start:plot_end], type="l", ylim=c(0, 5.6), lwd=1.5, xaxt="n", xlab="Date", ylab="FED interest rate",
     main="Forecast and Prediction Interval in the future")
polygon(c((plot_length-12+1):plot_length, rev((plot_length-12+1):plot_length)),
        c(forecast_values_upper, rev(forecast_values_lower)),
        col = "#C6E9F9", border = NA)
lines(c(rep(NA, (plot_length-12)),forecast_values), col="red", lty = 1, lwd=1.5)
lines(c(rep(NA, (plot_length-12)),forecast_values_upper), col = "blue", lty = 2, lwd=1.5)
lines(c(rep(NA, (plot_length-12)),forecast_values_lower), col = "blue", lty = 2, lwd=1.5)
axis(1, at = seq(1, length(dates[plot_start:plot_end]), by=6), 
     labels = dates[plot_start:plot_end][seq(1, length(dates[plot_start:plot_end]), by =6)])
legend("bottomright", legend = c("TimeSeries", "Forecast", "Prediction Interval"), col = c("black", "red", "blue"), lty = c(1,2,2), lwd=c(2,2,2))
<<<<<<< HEAD
}
=======
}
>>>>>>> 82a8945f8a8bbb31b5d5627f9682ad700c633508
