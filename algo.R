#algo trading with ARIMA (S&P, select ETFs)
#4.20.2020 v1
#in conjunction with Shiny App interactive model at tsund/shinyapps.io/arima_project
#
#--------------------------------------

pacman::p_load(tidyverse, tseries, forecast, xts, tidyquant, e1071, broom, lmtest, tictoc)

tic("Total elapsed time")

sp_ref <- read_csv("C:\\Users\\tanne\\Desktop\\secref.csv")


all_securities <- as_vector(sp_ref)[1:5]
output_matrix_headers <- c("Ticker", "Quote", "Beta", "Sharpe", "MAPE", "Highest P-Value", "t + 1 return", "t + 2 return", "t + 3 return", "horizon (long)", "p order", "d order", "q order", "drift", "total terms") 
output_matrix <- matrix(nrow = length(all_securities), ncol = length(output_matrix_headers))
colnames(output_matrix) <- output_matrix_headers

sp500 <- tq_get("SPY", get = "stock.prices", from = "2019-01-01") %>% 
  mutate(return = close/lag(close) - 1) %>% 
  select(return) %>% 
  unlist()


for (val in seq(1, length(all_securities), 1)) {

tic(paste("Security #", val, sep =""))
  
curr_ticker <- all_securities[val]

ticker <- curr_ticker

quote <- getQuote(curr_ticker, src = "yahoo")[[2]]

sec <- tq_get(curr_ticker, get = "stock.prices", from = "2019-01-01") %>% 
  mutate(return = close/lag(close) - 1) %>% 
  select(return) %>% 
  unlist()
#capm <- lm(sec ~ sp500)
beta <- 0#tidy(capm)[[2,2]]

fred_3mo <- tq_get("DGS3MO", get = "economic.data")
rfr <- fred_3mo[nrow(fred_3mo),2]
mean <- round(mean(sec, na.rm = TRUE) * 252, digits = 2) * 100
sd <- round(sd(sec, na.rm = TRUE) * sqrt(252), digits = 2) * 100
sharpe <- round((mean - rfr) / sd, digits = 2)[[1]]

sec_orig <- tq_get(curr_ticker, get = "stock.prices", from = "2018-01-01") %>% 
  select(close)
sec_orig <- rbind(sec_orig, quote)
sec_orig <- ts(sec_orig, start = c(2018,01,01), frequency = 252)
arima <- auto.arima(sec_orig, max.p = 3, max.d = 1, max.q = 3, stepwise = TRUE, approximation = TRUE, trace = FALSE)
mape <- round((accuracy(arima)[5]) / 100, digits = 4)

pq_order <- arimaorder(arima)[1] + arimaorder(arima)[3] #if p, q, and drift terms arent chosen, time-series is a random walk (exclude)
if (pq_order > 0) { 
  highestpvalue <- round(max(coeftest(arima)[,4]), digits = 8)
  drift <- "drift" %in% labels(coeftest(arima))[[1]]
  }
else {highestpvalue <- "NA"
      drift <- FALSE}

forecast_table <- data.frame(forecast(arima, h = 5)) %>% 
  mutate(pct_chg = (Point.Forecast / quote - 1) * 100) 
tplusone <- forecast_table[1,6]
tplustwo <- forecast_table[2,6]
tplusthree <- forecast_table[3,6]

if (tplusone <= 0) {
  horizon <- 0
}

else if (tplustwo - tplusone <= 0) {
  horizon <- 1
}

else if (tplusthree - tplustwo <= 0){
  horizon <- 2
}

else {horizon <- 3}

p <- arimaorder(arima)[[1]]
d <- arimaorder(arima)[[2]]
q <- arimaorder(arima)[[3]]

terms <- p + d + q + drift

curr_item <- c(ticker, quote, beta, sharpe, mape, highestpvalue, tplusone, tplustwo, tplusthree, horizon, p, d, q, drift, terms)
output_matrix[val,] <- curr_item

toc(log = TRUE)
}

#malleable report parameters
# output_matrix <- tibble(output_matrix)
# 
# output_matrix$Quote <- as.numeric(output_matrix$Quote)
# output_matrix$Beta <- as.numeric(output_matrix$Beta)
# output_matrix$Sharpe <- as.numeric(output_matrix$Sharpe)
# output_matrix$MAPE <- as.numeric(output_matrix$MAPE)
# output_matrix$Highest.P.Value <- as.numeric(output_matrix$Highest.P.Value)
# output_matrix$t...1.return <- as.numeric(output_matrix$t...1.return)
# output_matrix$t...2.return <- as.numeric(output_matrix$t...2.return)
# output_matrix$t...3.return <- as.numeric(output_matrix$t...3.return)
# output_matrix$horizon..long. <- as.numeric(output_matrix$horizon..long.)
# output_matrix$total.terms <- as.numeric(output_matrix$total.terms)

output_matrix %>% 
  filter(highestpvalue < 0.05)

#write.csv(output_matrix, "C:/Users/tanne/Desktop/print.csv")

toc(LOG = TRUE)

beepr::beep(sound = 4)