pacman::p_load(shiny, tidyverse, tseries, forecast, xts, e1071, tidyquant, broom, lmtest)
options(scipen = 999)

ui <- fluidPage(

  titlePanel("Short Term Equities Model (Gamers Only)"),
  
  mainPanel(
  
  sliderInput(inputId = "year", value = 1, min = 2005, max = 2020, label = "Start Year", sep = ""),
  
  textInput(inputId = "ticker", label = "Security", value = "SPY", placeholder = "Enter ticker symbol for equity"),
  
  selectInput(inputId = "auto", label = "Stepwise Model Selection?", choices = c(TRUE, FALSE)),
  selectInput(inputId = "seasonality", label = "Seasonality Period", choices = c("Weekly", "Monthly", "None")),
  selectInput(inputId = "drift", label = "Drift/Constant Term?", choices = c(TRUE, FALSE)),
  selectInput(inputId = "orderp", label = "Order (p)", choices = c(0, 1, 2, 3, 4)),
  selectInput(inputId = "orderd", label = "Order (d)", choices = c(0, 1, 2)),           
  selectInput(inputId = "orderq", label = "Order (q)", choices = c(0, 1, 2, 3, 4)),
              
  selectInput(inputId = "forecastinterval", label = "Forecast Interval", choices = c("Daily", "Weekly")),
  selectInput("forecastduration", label = "Forecast Duration (Days)", choices = c(5, 10, 20, 30)),
  
  actionButton(inputId = "refresh", label = "Run Model"),
  
  tableOutput("stock"),
  plotOutput("ts"),
  tableOutput("table")
 
  ),
  
  sidebarPanel(
    
  tableOutput("stats"), 
  tableOutput("results"),
  tableOutput("market")
 
  )
  
)

server <- function(input,output) {

  refreshTicker <- eventReactive(input$refresh, {input$ticker})
  refreshYear <- eventReactive(input$refresh, {input$year})
  refreshAuto <- eventReactive(input$refresh, {input$auto})
  refreshSeasonality <- eventReactive(input$refresh, {input$seasonality})
  refreshDrift <- eventReactive(input$refresh, {input$drift})
  refreshP <- eventReactive(input$refresh, {input$orderP})
  refreshD <- eventReactive(input$refresh, {input$orderD})
  refreshQ <- eventReactive(input$refresh, {input$orderQ})
  refreshFinterval <- eventReactive(input$refresh, {input$forecastinterval})
  refreshFduration <- eventReactive(input$refresh, {input$forecastduration})
  
  
  output$ts <-  renderPlot({
   
  sec <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
      select(close)
  
  sec <- ts(sec, start = c(refreshYear,01,01), frequency = 252)
  
  #sp500_1 <- tq_get("SPY", get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
    #select(close)
  
 # sp500_1 <- ts(sp500_1, start = c(refreshYear,01,01), frequency = 252)

if(refreshSeasonality == "Weekly") {seasonality <- 5}
else if(refreshSeasonality == "Monthly") {seasonality <- 21}
else if(refreshSeasonality == "Yearly") {seasonality <- 252}
else {seasonality <- ""}
  

    
if(refreshAuto == TRUE) {
  
    arima <- auto.arima(sec, max.p = 2, max.d = 1, max.q = 2, stepwise = TRUE, approximation = TRUE)#, xreg = sp500_1)
  
}  
    
else {
  
    if(seasonality == ""){ 
      
    arima <- Arima(sec, order = c(refreshP, refreshD, refreshQ), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift)
      
    }
  
    else{ 
    
    arima <- Arima(sec, order = c(refreshP, refreshD, refreshQ), seasonal = list(order = c(1, 0, 1), period = seasonality), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift) 
    
    }
  
}

  sec.forecast <- forecast(arima, h = 30)

  

    plot(sec.forecast, main = paste(refreshTicker, " - ARIMA(", arimaorder(arima)[1], ",", arimaorder(arima)[2], ",", arimaorder(arima)[3], ")", " [Seasonality: ", refreshSeasonality, "]", sep = ""), ylab = "Price")
    lines((arima$residuals / (fitted(sec.forecast) + arima$residuals)) * 100 + mean(sec), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    abline(h = mean(sec) + 5, lty = "longdash")
    abline(h = mean(sec), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.65))
    abline(h = mean(sec) - 5, lty = "longdash")


  })

  output$table <- renderTable({
    sec_2 <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
      select(close)
    
    sec_2 <- ts(sec_2, start = c(refreshYear,01,01), frequency = 252)
  
    #sp500_2 <- tq_get("SPY", get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
      select(close)
    
    #sp500_2 <- ts(sp500_2, start = c(refreshYear,01,01), frequency = 252)
    
    if(refreshSeasonality == "Weekly") {seasonality_2 <- 5}
    else if(refreshSeasonality == "Monthly") {seasonality_2 <- 21}
    else if(refreshSeasonality == "Yearly") {seasonality_2 <- 252}
    else {seasonality_2 <- ""}
    
    
    
    if(refreshAuto == TRUE) {
      
      arima_2 <- auto.arima(sec_2, max.p = 2, max.d = 1, max.q = 2, stepwise = TRUE, approximation = TRUE)#, xreg = sp500_1)
      
    }  
    
    else {
      
      if(seasonality_2 == ""){ 
        
        arima_2 <- Arima(sec_2, order = c(refreshP, refreshD, refreshQ), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift)
        
      }
      
      else{ 
        
        arima_2 <- Arima(sec_2, order = c(refreshP, refreshD, refreshQ), seasonal = list(order = c(1, 0, 1), period = seasonality), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift) 
        
      }
      
    }
    
    arima_table <- data.frame(forecast(arima_2, h = as.numeric(refreshFduration)))
    
    arima_table <- arima_table %>% 
      mutate(PercentChange = (arima_table$Point.Forecast / sec_2[length(sec_2)] - 1) * 100 ) %>% 
      select(Point.Forecast, PercentChange)
    
interval <- if(refreshFinterval == "Daily") {1}
    else {5}

text <- if(refreshFinterval == "Daily") {"Day"}
    else {"Week"}
    
    arima_table <- arima_table[seq(interval,  as.numeric(refreshFduration), interval),]
    
    arima_table <- cbind(Forecast = c(paste(text,seq(1, as.numeric(refreshFduration) / interval, 1), sep = " ")), arima_table)
    
    (arima_table %>% 
      mutate(
    AnnualizedReturn = 252 / seq(interval,  as.numeric(refreshFduration), interval)  * PercentChange
      ))
  })
  
  output$stock <- renderTable({
    sec_3 <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
      select(close)
    
    sec_3 <- ts(sec_3, start = c(refreshYear,01,01), frequency = 252)
    sec_3 <- sec_3[length(sec_3)]
    
    #names(sec_3) <- c("Current Price")
    
    sec_3
    
}, "Forecasted Performance",
caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$stats <- renderTable({
      
      sec_4 <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
        select(close)
      
      sec_4 <- sec_4 %>% 
        mutate(return = (close / lag(close)) - 1) %>% 
        select(return) %>% 
        unlist()
      
      n <- length(sec_4)
      sd <- round(sd(sec_4, na.rm = TRUE) * sqrt(252), digits = 2)
      mean <- round(mean(sec_4, na.rm = TRUE) * 252, digits = 2)
      lower <- round(mean - (sd * 1.96), digits = 2)
      upper <- round(mean + (sd * 1.96), digits = 2)
      kurt <- round(kurtosis(sec_4, na.rm = TRUE), digits = 2)
      skew <- round(skewness(sec_4, na.rm = TRUE), digits = 2)
      tablebreak <- ""
      fred_3mo <- tq_get("DGS3MO", get = "economic.data")
      rfr <- fred_3mo[nrow(fred_3mo),2]
      sharpe <- round((mean - rfr) / sd, digits = 2)
      rfr <- paste(rfr, "%", sep = "")
      
      list <- as.matrix(list(n, sd, lower, mean, upper, kurt, skew, tablebreak, tablebreak, rfr, sharpe))
      
      list <- cbind(c("n","StDev","Lower CI","Mean", "Upper CI", "Kurtosis", "Skew", "", "", "Risk-Free Rate", "Sharpe Ratio"),list)
      
      colnames(list) <- c("Statistic", "Value")
      
      list
  }, caption = "Return Metrics (Annualized)",
  caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$results <- renderTable({
      sec_5 <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
        select(close)
      
      sec_5 <- ts(sec_5, start = c(refreshYear,01,01), frequency = 252)
      
      sp500_5 <- tq_get("SPY", get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
        select(close)
      
      sp500_5 <- ts(sp500_5, start = c(refreshYear,01,01), frequency = 252)
      
if(refreshSeasonality == "Weekly") {seasonality_5 <- 5}
else if(refreshSeasonality == "Monthly") {seasonality_5 <- 21}
else if(refreshSeasonality == "Yearly") {seasonality_5 <- 252}
else {seasonality_5 <- ""}



if(refreshAuto == TRUE) {
  
  arima_5 <- auto.arima(sec_5, max.p = 2, max.d = 1, max.q = 2, stepwise = TRUE, approximation = TRUE)#, xreg = sp500_1)
  
}  

else {
  
  if(seasonality_5 == ""){ 
    
    arima_5 <- Arima(sec_5, order = c(refreshP, refreshD, refreshQ), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift)
    
  }
  
  else{ 
    
    arima_5 <- Arima(sec_5, order = c(refreshP, refreshD, refreshQ), seasonal = list(order = c(1, 0, 1), period = seasonality), method = "ML", include.drift = refreshDrift, include.mean = refreshDrift) 
    
  }
  
}
      
      results_matrix <- cbind(tidy(coeftest(arima_5))[,1],round(tidy(coeftest(arima_5))[1:nrow(tidy(coeftest(arima_5))),2:5], digits = 3))
      
      MAPE <- list("MAPE", paste(round(accuracy(arima_5)[5], digits = 2), "%", sep = ""), "", "", "")
      
      lbtest <- list("Ljung-Box Test","","",round(tidy(Box.test(resid(arima_5), lag = 1, type = "Ljung-Box"))[1],digits = 3),round(tidy(Box.test(resid(arima_5), lag = 1, type = "Ljung-Box"))[2],digits = 3))
      
      results_matrix <- rbind(results_matrix, "", MAPE, lbtest)

      
    }, caption = "Model Estimates",
    caption.placement = getOption("xtable.caption.placement", "top"))
   
    output$market <- renderTable({
      sec_6 <- tq_get(refreshTicker, get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
        mutate(logreturn = close/lag(close) - 1) %>% 
      select(logreturn) %>% 
        unlist()
      
      sp500 <- tq_get("SPY", get = "stock.prices", from = paste(refreshYear, "-01-01", sep = "")) %>% 
        mutate(logreturn = close/lag(close) - 1) %>% 
      select(logreturn) %>% 
        unlist()
      
      
      regress <- cbind(tidy(lm(sec_6 ~ sp500))[,1], round(tidy(lm(sec_6 ~ sp500))[1:2,2:5], digits = 3))
      regress[1,1] <- "Alpha"
      regress[2,1] <- "Beta"
      
      r2 <- summary(lm(sec_6 ~ sp500))$r.squared
      
      r2 <- list("R^2", r2, "", "", "")
      
      regress <- rbind(regress, r2)

      
            
    }, caption = "CAPM",
    caption.placement = getOption("xtable.caption.placement", "top"))
     
}

shinyApp(ui = ui, server = server, options = "launch.browser")
