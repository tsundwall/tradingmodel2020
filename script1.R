pacman::p_load(tidyverse, stringr, xts)
library(tseries)
library(lmtest)
library(forecast)

aapl <- read_csv("AAPL.csv")
aapl <- aapl %>% 
  filter(Date >= "2012-01-31") %>% 
  select(Close)

aapl <- ts(aapl, start = c(2012,02,01), frequency = 12)

components.aapl = decompose(aapl)
plot(components.aapl)

acf(aapl, lag.max = 24)

kpss.test(aapl)
stationary.aapl <- diff(aapl, differences = 1)
kpss.test(stationary.aapl)
plot(stationary.aapl)

adjusted.aapl <- aapl - components.aapl$seasonal
stationary2.aapl <- diff(adjusted.aapl, differences = 1)
kpss.test(stationary2.aapl)

plot(stationary2.aapl)
acf(stationary2.aapl)
pacf(stationary2.aapl)

(arima <- Arima(aapl, order = c(2,1,1), seasonal = list(order = c(1,0,0), period = 12),method = "ML", include.drift = FALSE))
coeftest(arima)

aapl.arima <- auto.arima(aapl, trace = TRUE)
beep(3)

aapl.forecast <- predict(arima, n.ahead = 12)

aapl.forecast <- forecast(aapl.arima)

forecast:::autoplot.forecast(aapl.forecast)
