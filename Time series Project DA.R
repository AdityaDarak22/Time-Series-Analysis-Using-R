library(readxl)
library(fpp2)
library(tseries)
exchange_rate <- read_excel("D:/Folders after Upgrading Laptop/Users/Aditya/Desktop/ts datset/exchange rate dataset.xlsx")
View(exchange_rate)

# Converting Data frame to a Time Series
exchange_ts <- ts(exchange_rate, start = c(1999,1), frequency = 12)
View(exchange_ts)

#Time Series Plot
plot(exchange_ts, xlab = "Year", ylab = "Exchange Rates (NZD / INR)", main = "Exchange rate over the years")

#Seasonal Plot
ggseasonplot(exchange_ts, year.labels = TRUE, year.labels.left = TRUE) + ylab("Exchange Rates")+ ggtitle("Seasonal plot of NZD-INR")

#Log transformation
exhangelog <- log(exchange_ts)
plot(exhangelog)

#time series smoothing
plot(ma(exchange_ts,3))

#Decomposition of a time series
decom_rate <- decompose(exchange_ts, type = "multiplicative")
plot(decom_rate)


#ADF TEST
adf.test(exchange_ts)
ndiffs(exchange_ts)

nz_exchange <- diff(exchange_ts)
adf.test(nz_exchange)
autoplot(nz_exchange)


par(mfrow)
acf(nz_exchange)
pacf(nz_exchange)


fit <- arima(exchange_ts, order = c(0,1,1))  
fit
  
fit1 <- arima(exchange_ts, order = c(1,1,0))
fit1
  
fit2 <- arima(exchange_ts, order = c(0,1,0))
fit2
  
accuracy(fit)

qqnorm(fit1$residuals)
qqline(fit1$residuals)
Box.test(fit1$residuals, type = "Ljung-Box")

predict <- forecast(fit1,3)
plot(predict, xlab = "Years", ylab ="Exchange Rate ( NZ / INR)")  
predict
