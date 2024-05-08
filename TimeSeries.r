######TIME SERIES
{
  library(ggfortify)
  library(forecast)
  library(tseries)
  library(car)
}

#importing data
library(readxl)
ts<-read_excel("FEDFUNDS.xlsx")

ts$observation_date <- substr(ts$observation_date, 1, 7) #remove the day
nchar(ts$observation_date)[nchar(ts$observation_date) != 7] #all the data has 7 characters: aaaa-mm

#creating a time series object
library(zoo)
ts <- ts(ts$FEDFUNDS, start = c(as.yearmon("1954-07")), end = c(as.yearmon("2024-02")), frequency = 12)

# Visualize the time series
autoplot(ts)
# We notice that probably the time series is not stationary and seem to have a stochastic trend

# Apply the log to the time series: SERVE???
# ts_log <- log(ts)
# autoplot(ts_log)
# Now the time series looks more stationary

# Analysis of the stationarity: Augmented DF test
adf.test(ts)
# the test shows that the time series is NOT stationary and that there is a stochastic trend

# Decompose the time series in trend, seasonality and residuals
decomposed_ts <- decompose(ts)
# Plot the decomposed time series
autoplot(decomposed_ts)
# We can see that the trend is first increasing and then decreasing, the seasonality is not present
# Plot a seasonal plot
ggseasonplot(ts)
# Decompose the time series with the multiplicative decomposition
decomposed_ts_mult <- decompose(ts, type = "multiplicative")
autoplot(decomposed_ts_mult)

# Look at the ACF and PACF
ggAcf(ts)
ggPacf(ts)
# We see that there is persistence in the ACF and PACF, which suggests that the time series is not stationary



# Transform the TS in order to remove non-stationarity: first difference
ts_diff <- diff(ts)
autoplot(ts_diff)
adf.test(ts_diff)
# We see from the test that now the time series is stationary

# Look at the ACF and PACF of the first difference
ggAcf(ts_diff)
ggPacf(ts_diff)
# We see that the ACF has a significant peak at lag 1 and the PACF has a significant peak at lag 1
# This suggests that the time series can be modeled with an ARIMA(1,1,0) model
# We want to understand which parameters p and q of the ARIMA model are the best
auto.arima(ts_diff)
# The function suggests that the best model is an ARIMA(1,1,0) model because it has the lowest AIC

# Divide the time series in training and test set (80% training, 20% test)
train <- window(ts_diff, end = c(2019, 12))
test <- window(ts_diff, start = c(2020, 1))
# Fit an ARIMA(1,1,0) model
fit <- Arima(train, order = c(1,1,0))
summary(fit)

# Forecast the test set for all the months in 2024, so until December 2024
forecast <- forecast(fit, h = 24)
# Plot the forecast
autoplot(forecast)
# Plot the forecast with the test set
autoplot(forecast) + autolayer(test, series = "Test set")

# Show me the acf and pacf of the residuals with ARIMA(1,1,0)
ggAcf(residuals(fit))
ggPacf(residuals(fit))
# The ACF and PACF of the residuals show that there is no persistence in the residuals, so the model is good

# Predict the intervals of the forecast
forecast_intervals <- forecast(fit, h = 24, level = c(80, 95))
# Plot the forecast with the intervals
autoplot(forecast_intervals)
# Do a plot just from 2017 to see better the prediction
autoplot(forecast_intervals) + xlim(c(2017, 2024))


# First we do a Durbin-Watson test to see if there is autocorrelation in the residuals

# We do a Breusch-Godfrey test to see if the residuals are white noise

# We do a Ljung-Box test to see if the residuals are white noise

# Test if there are structural breaks: Chow test
chow.test(ts_diff, index = 200)
# The test shows that there is a structural break in the time series???
# We have to divide the time series in two parts and fit an ARIMA model to each part???

# Verify the accuracy of the forecast
accuracy(forecast, test)
# On test set MAE is 0.1, RMSE is 0.22: the forecast is ... good?