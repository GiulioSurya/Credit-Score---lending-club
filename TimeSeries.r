######TIME SERIES
{
  library(ggfortify)
  library(forecast)
  library(tseries)
}

#importing data
library(readxl)
ts<-read_excel("FEDFUNDS.xlsx")

ts$observation_date <- substr(ts$observation_date, 1, 7) #remove the day
nchar(ts$observation_date)[nchar(ts$observation_date) != 7] #all the data has 7 characters: aaaa-mm

#creating a time series object
library(zoo)
ts <- ts(ts$FEDFUNDS, start = c(as.yearmon("1954-07")), end = c(as.yearmon("2024-02")), frequency = 12)
autoplot(ts)