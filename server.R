
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(reshape2)
library(ggplot2)
library(demography)
library(sandwich)
library(jsonlite)
library(quantmod)
library(fOptions)
library(leaflet)
library(maps)
library(mapproj)

source("MortalityHelpers.R")
source("Options.R")
source("MapsHelper.R")

shinyServer(function(input, output) {
  
  calcOptionGreeks <- reactive({withProgress(message="Calculating option greeks",{
    value <- BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    delta <- BS_delta(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    gamma <- BS_gamma(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    theta <- BS_theta(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    vega <- BS_vega(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    rho <- BS_rho(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))
    df <- data.frame(t(c(value, delta, gamma, theta, vega, rho)), row.names = input$CallPut)
    colnames(df) <- c("Value", "Delta", "Gamma", "Theta", "Vega", "Rho")
    return(df)
  })
  })
  
  
  withProgress(message="Showing option values", {output$tbl.risk.options <- renderTable({
    calcOptionGreeks()
  })
  })
  
  
  withProgress(message="Plotting option delta", {output$plot.risk.delta <- renderPlot({
    eq <- function(x){BS_delta(x, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1) )}
    tmp <- data.frame(x=0:(2*floor(input$Price)), y=eq(0:(2*floor(input$Price))))
    p <- ggplot(tmp)
    p <- p + geom_line(aes(x=x,y=y))
    #p <- p + stat_function(fun=eq)
    p <- p + labs(y="Delta", x="Stock Price", title="Delta")
    p <- p+ geom_vline(xintercept=c(input$Strike*input$Price/100), linetype="dotted")
    p <- p+ geom_vline(xintercept=c(BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))), linetype="dashed")
    #p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(message="Plotting option gamma", {output$plot.risk.gamma <- renderPlot({
    eq <- function(x){BS_gamma(x, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1) )}
    tmp <- data.frame(x=0:(2*floor(input$Price)), y=eq(0:(2*floor(input$Price))))
    p <- ggplot(tmp)
    p <- p + geom_line(aes(x=x,y=y))
    #p <- p + stat_function(fun=eq)
    p <- p + labs(y="Gamma", x="Stock Price", title="Gamma")
    p <- p+ geom_vline(xintercept=c(input$Strike*input$Price/100), linetype="dotted")
    p <- p+ geom_vline(xintercept=c(BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))), linetype="dashed")
    #p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(message="Plotting option theta", {output$plot.risk.theta <- renderPlot({
    eq <- function(x){BS_theta(x, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1) )}
    tmp <- data.frame(x=0:(2*floor(input$Price)), y=eq(0:(2*floor(input$Price))))
    p <- ggplot(tmp)
    p <- p + geom_line(aes(x=x,y=y))
    #p <- p + stat_function(fun=eq)
    p <- p + labs(y="Theta", x="Stock Price", title="Theta")
    p <- p+ geom_vline(xintercept=c(input$Strike*input$Price/100), linetype="dotted")
    p <- p+ geom_vline(xintercept=c(BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))), linetype="dashed")
    #p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(message="Plotting option vega", {output$plot.risk.vega <- renderPlot({
    eq <- function(x){BS_vega(x, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1) )}
    tmp <- data.frame(x=0:(2*floor(input$Price)), y=eq(0:(2*floor(input$Price))))
    p <- ggplot(tmp)
    p <- p + geom_line(aes(x=x,y=y))
    #p <- p + stat_function(fun=eq)
    p <- p + labs(y="Vega", x="Stock Price", title="Vega")
    p <- p+ geom_vline(xintercept=c(input$Strike*input$Price/100), linetype="dotted")
    p <- p+ geom_vline(xintercept=c(BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))), linetype="dashed")
    #p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(message="Plotting option rho", {output$plot.risk.rho <- renderPlot({
    eq <- function(x){BS_rho(x, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1) )}
    tmp <- data.frame(x=0:(2*floor(input$Price)), y=eq(0:(2*floor(input$Price))))
    p <- ggplot(tmp)
    p <- p + geom_line(aes(x=x,y=y))
    #p <- p + stat_function(fun=eq)
    p <- p + labs(y="Rho", x="Stock Price", title="Rho")
    p <- p+ geom_vline(xintercept=c(input$Strike*input$Price/100), linetype="dotted")
    p <- p+ geom_vline(xintercept=c(BS(input$Price, input$Strike*input$Price/100, input$RiskFreeRate/100, input$Dividend/100, input$Volatility/100, input$Time,substr(input$CallPut,1,1))), linetype="dashed")
    #p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(message="Loading market (FRED) data", {
    source("script_LoadData.R")
  })
  
  withProgress(detail="Plotting spreads", {output$plot.spreads <- renderPlot({
    # graph the spreads with the specified range
    df <- data.frame(date=index(spreads), coredata(spreads))
    m <- melt(subset(df, date>=input$dtrng.rates[1] & date<= input$dtrng.rates[2]), id.vars="date", variable.name="Rating")
    m <- subset(m, Rating %in% input$chckBxGrpRating)
    bc.trim <- subset(bc, Peak>=min(m$date), Trough<=max(m$date))
    #p <- ggplot(m, aes(x=date,y=value, group=Rating))
    p <- ggplot(m)
    p <- p + geom_line(aes(x=date,y=value, group=Rating, color=Rating))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Spreads (%)", x="Time", title="BAML Option-Adjusted Spreads")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting swaps", {output$plot.swaps <- renderPlot({
    # graph the spreads with the specified range
    df <- data.frame(date=index(swaps), coredata(swaps))
    m <- melt(subset(df, date>=input$dtrng.rates[1] & date<= input$dtrng.rates[2]), id.vars="date", variable.name="Maturity")
    m <- subset(m, Maturity %in% input$chckBxGrpMaturity)
    bc.trim <- subset(bc, Peak>=min(m$date), Trough<=max(m$date))
    #p <- ggplot(m, aes(x=date,y=value, group=Maturity))
    p <- ggplot(m)
    p <- p + geom_line(aes(x=date,y=value, group=Maturity, color=Maturity))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Interest Rate (%)", x="Time", title="Swap Rates")
    p <- p + scale_color_hue(l=45)
    p
    
  })
  })
  
  withProgress(detail="Plotting defaults", {output$plot.moodysCorp <- renderPlot({
    # graph the spreads with the specified range
    df <- getDefaults(1920, 2013)
    df <- data.frame(date=index(df), coredata(df))
    m <- melt(subset(df, date>=input$dtrng.rates[1] & date<= input$dtrng.rates[2]), id.vars="date", variable.name="Rating")
    m <- subset(m, Rating %in% input$chckBxGrpRating)
    bc.trim <- subset(bc, Peak>=min(m$date), Trough<=max(m$date))
    #p <- ggplot(m, aes(x=date,y=value, group=Rating))
    p <- ggplot(m)
    p <- p + geom_line(aes(x=date,y=value, group=Rating,color=Rating))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Default Rate", x="Time", title="Moody's Annual Corporate Bond Defaults")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting Treasuries", {output$plot.treas <- renderPlot({
    # graph the spreads with the specified range
    df <- data.frame(date=index(treas), coredata(treas))
    m <- melt(subset(df, date>=input$dtrng.Trates[1] & date<= input$dtrng.Trates[2]), id.vars="date", variable.name="Maturity")
    m <- subset(m, Maturity %in% input$chckBxGrpTMaturity)
    bc.trim <- subset(bc, Peak>=min(m$date), Trough<=max(m$date))
    p <- ggplot(m)
    p <- p + geom_line(aes(x=date,y=value, group=Maturity, color=Maturity))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Interest Rate (%)", x="Time", title="Treasury Constant Maturity")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting Changes", {output$plot.risk.diff <- renderPlot({
    riskX <- risk
    riskX[is.na(riskX)] <-0 
    risk.monthly <- to.monthly(riskX, OHLC=FALSE)
    risk.monthly <- diff(risk.monthly)/lag(risk.monthly)
    df <- data.frame(date=as.Date(index(risk.monthly)), coredata(risk.monthly))
    day(df$date) <- days_in_month(df$date) 
    #df <- subset(df, date>=input$dtrng.Trates[1] & date<= input$dtrng.Trates[2])
    m <- melt(df, id.vars="date", variable.name="PctChange")
    
    bc.trim <- subset(bc, Peak>=min(m$date), Trough<=max(m$date))
    p <- ggplot(m)
    p <- p + geom_line(aes(x=date,y=value, group=PctChange, color=PctChange))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Percent Change in Risk Driver", x="Time", title="Percentage Change in Risk Drivers Across Time")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Calculating Correlation", {output$table.correl <- renderTable({
    riskX <- risk
    riskX[is.na(riskX)] <-0 
    risk.monthly <- to.monthly(riskX, OHLC=FALSE)
    risk.monthly <- diff(risk.monthly)/lag(risk.monthly)
    df <- data.frame(date=as.Date(index(risk.monthly)), coredata(risk.monthly))
    day(df$date) <- days_in_month(df$date)
    df <- na.omit(df)
    cor(df[-1])
  })
  })
  
  withProgress(detail="Calculating quantiles", {output$table.stdev <- renderTable({
    riskX <- risk
    riskX[is.na(riskX)] <-0 
    risk.monthly <- to.monthly(riskX, OHLC=FALSE)
    risk.monthly <- diff(risk.monthly)/lag(risk.monthly)
    df <- data.frame(date=as.Date(index(risk.monthly)), coredata(risk.monthly))
    day(df$date) <- days_in_month(df$date)
    df <- na.omit(df)
    value <- sapply(df[-1], function(x) round(quantile(x, c(.5, .75, .9, .995)), 4))
    value2 <- t(data.frame(sd(df[-1])))
    rownames(value2) <- c("St Dev")
    value <- rbind(value2, value)
    
  })
  })
  
  withProgress(detail="Plotting Fed Funds", {output$plot.ffr <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(DFF), coredata(DFF)))
    df <- subset(df, date>=input$dtrng.Trates[1] & date<= input$dtrng.Trates[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=DFF))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Interest Rates (%)", x="Time", title="Effective Federal Funds Rate")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting S&P 500", { output$plot.sp500 <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(SP500), coredata(SP500)))
    df <- subset(df, date>=input$dtrng.equity[1] & date<= input$dtrng.equity[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=SP500))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Index", x="Time", title="S&P 500©")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting Russell 1000", {output$plot.russell1000 <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(RU1000TR), coredata(RU1000TR)))
    df <- subset(df, date>=input$dtrng.equity[1] & date<= input$dtrng.equity[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=RU1000TR))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Index", x="Time", title="Russell 1000® Total Market Index")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting VIX", {output$plot.vix <- renderPlot({
    df <- na.omit(data.frame(date=index(VIXCLS), coredata(VIXCLS)))
    df <- subset(df, date>=input$dtrng.equity[1] & date<= input$dtrng.equity[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=VIXCLS))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="%", x="Time", title="CBOE Volatility Index: VIX©")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting GDP", {output$plot.gdp <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(GDPC1), coredata(GDPC1)))
    df <- subset(df, date>=input$dtrng.econ[1] & date<= input$dtrng.econ[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=GDPC1))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Billions of Chained 2009 $", x="Time", title="Real Gross Domestic Product")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting CPI", {output$plot.cpi <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(CPALTT01USQ661S), coredata(CPALTT01USQ661S)))
    df <- subset(df, date>=input$dtrng.econ[1] & date<= input$dtrng.econ[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=CPALTT01USQ661S))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Index", x="Time", title="Consumer Price Index: Total All Items for the United States©")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting unemployment", {output$plot.unrate <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(UNRATE), coredata(UNRATE)))
    df <- subset(df, date>=input$dtrng.econ[1] & date<= input$dtrng.econ[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=UNRATE))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Rate (%)", x="Time", title="Civilian Unemployment Rate")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting consumer sentiment", {output$plot.umichcs <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(UMCSENT), coredata(UMCSENT)))
    df <- subset(df, date>=input$dtrng.econ[1] & date<= input$dtrng.econ[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=UMCSENT))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Index", x="Time", title="University of Michigan: Consumer Sentiment©")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
  withProgress(detail="Plotting gold prices", {output$plot.gold <- renderPlot({
    # graph the spreads with the specified range
    df <- na.omit(data.frame(date=index(GOLDAMGBD228NLBM), coredata(GOLDAMGBD228NLBM)))
    df <- subset(df, date>=input$dtrng.econ[1] & date<= input$dtrng.econ[2])
    bc.trim <- subset(bc, Peak>=min(df$date), Trough<=max(df$date))
    p <- ggplot(df)
    p <- p + geom_line(aes(x=date,y=GOLDAMGBD228NLBM))
    if(nrow(bc.trim)>0){
      p <- p+ geom_rect(data=bc.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill="pink", alpha=.5)
    }
    p <- p + labs(y="Price ($)", x="Time", title="Gold Fixing Price in London Bullion Market, based in U.S. Dollars")
    p <- p + scale_color_hue(l=45)
    p
  })
  })
  
   output$table.bc <- renderDataTable({
     getBusinessCycles()
   })
  
   
   volControl <- eventReactive(input$VCIrunbutton, {
     vhp1<-20
     vhp2<-40
     lag <-2
     target_level <- input$VCItargetlevel/100
     max_leverage <- input$VCImaxleverage/100
     max_change <- input$VCImaxchange/100
     valdate <- input$VCIvaldate
     nsims <-  input$VCInumsim
     r <- as.numeric(subset(data.frame(date=index(DSWP1), coredata(DSWP1)), date==valdate)[2])/100
     S <- as.numeric(subset(data.frame(date=index(SP500), coredata(SP500)), date==valdate)[2])
     history_fit <- SP500[paste0(as.character(valdate-years(1)), "::", as.character(valdate))] #only want history through the valuation date
     SP500.sub <- SP500[paste0("::", as.character(valdate))] #only want history through the valuation date
     calendar<-timeSequence(from=valdate+1, by="day", length.out=366) #get a full calendar year
     calendar<-calendar[isBizday(calendar, holidayNYSE())] #remove weekends and NYSE holidays
     nTradingDays <- length(calendar) #count the number of trading days in the one-year horizon
     hnpr <- periodReturn(history_fit, period="daily", type="log", leading=FALSE)[-1]
     model = list(lambda = -1/2, omega = var(hnpr), alpha = var(hnpr)*0.1, beta = 0.1, gamma = 0, rf = log(1+r))
     hngf <- suppressWarnings(hngarchFit(x=hnpr, model = model))
     pricesHNG <- SimulateHNGPrices(hngf$model, nTradingDays, nsims, calendar, S,RN=TRUE)
     pricesHNG <- combine.past.proj(SP500.sub, pricesHNG, nsims)
     rcflHNG <- ApplyVolControl(valdate, pricesHNG, r, 100, nsims, nTradingDays, vhp1, vhp2, target_level, max_leverage, max_change, lag, calendar, FALSE)
     K <- 100
     payoffs <- as.vector(coredata(rcflHNG)[nrow(coredata(rcflHNG)),]) - K
     payoffs <- ifelse(payoffs<0, 0, payoffs)
     pv_payoffs <- payoffs*exp(-r)
     mean(pv_payoffs)
   }
   )
   
   withProgress(message="Showing option values", {output$tbl.volcontrol <- renderPrint({
     volControl()
   })
   })
   
  mortInput <- reactive({withProgress(message="Loading mortality data",{
    mort <- suppressWarnings(hmd.mx(country=input$slctCountry, user="jason.newkirk@aegon.com", password="Saturday7"))
  })
})
  
  mortForecast <- reactive({withProgress(message="Fitting and forecasting mortality",{
    level <- abs((input$sldrConfInt/100-.5)/.005)
    horizon <- input$nmrcHorizon
    mort.LC <- lca(mortInput(), series=names(mortInput()$rate[match(input$slctGender,names(mortInput()$rate))]), chooseperiod=input$chckbxChoosePeriod, breakmethod="bai", interpolate = TRUE)
    #mort.LC <- lca(mortInput(), series=names(mortInput()$rate[match(input$slctGender,names(mortInput()$rate))]), interpolate = TRUE)
    
    mxforecast <- forecast(mort.LC, h=horizon, level=level)
  })
})
  
  withProgress(message ="Plotting mortality", {output$plot.mx45 <- renderPlot({
    #mort <- hmd.mx(country=input$slctCountry, user="jason.newkirk@aegon.com", password="Saturday7")
    #mort.LC <- lca(mort, series=names(mort$rate[match(input$slctGender,names(mort$rate))]))
    #mxforecast <- forecast(mort.LC, h=horizon, level=level)
    plot.lc.qproj(mortForecast(), 45)
  })
  })
  
  withProgress(message="Plotting mortality", {output$plot.mx55 <- renderPlot({
    #mort <- hmd.mx(country=input$slctCountry, user="jason.newkirk@aegon.com", password="Saturday7")
    #mort.LC <- lca(mort, series=names(mort$rate[match(input$slctGender,names(mort$rate))]))
    #mxforecast <- forecast(mort.LC, h=horizon, level=level)
    plot.lc.qproj(mortForecast(), 55)
  })
  })
 
  withProgress(message ="Plotting mortality", { output$plot.mx65 <- renderPlot({
    #mort <- hmd.mx(country=input$slctCountry, user="jason.newkirk@aegon.com", password="Saturday7")
    #mort.LC <- lca(mort, series=names(mort$rate[match(input$slctGender,names(mort$rate))]))
    #mxforecast <- forecast(mort.LC, h=horizon, level=level)
    plot.lc.qproj(mortForecast(), 65)
  })
  })
  
 
  output$mymap <- renderPlot({
    #leaflet() %>% setView(-98.5795, 39.8282, zoom=4) %>% addTiles()
    dat <- read.csv("concDLPrem.csv", header = TRUE)
    percent_map(dat$pct, "darkgreen", "")
  })
  
  swtchDlSSD <- reactive({
    switch(input$slctDlSSD,
                           "Swaps" = swaps,
                           "BAML OAS Spreads" = spreads,
                           "Moody's Defaults" = defxts
    )
  })
 
  output$slctDlSSD <- renderUI({
    selectInput("slctDlSSD", "Choose file to download",
                choices = c("Swaps", "BAML OAS Spreads","Moody's Defaults" 
                            )                                                     
                )
  })
  
  output$dlSSD <- renderUI({  
    downloadButton('dlSSDrender', 'Download')
  })
  
  output$dlSSDrender <- downloadHandler(
    filename = function() {
      paste(input$slctDlSSD, ".csv", sep='')
    },
    content = function(file) {
      write.zoo(swtchDlSSD(), file, sep=",")
    },
    contentType = "text/csv"
  )
  
  swtchDlTreas <- reactive({
    switch(input$slctDlTreas,
           "Treasuries" = treas,
           "Fed Funds Rate" = DFF
    )
  })
  
#   output$dlTreas <- downloadHandler(
#     filename = function() {
#       paste(input$slctDlTreas, ".csv", sep='')
#     },
#     content = function(file) {
#       write.zoo(swtchDlTreas(), file, sep=",")
#     },
#     contentType = "text/csv"
#   )
  
  output$slctDlTreas <- renderUI({
    selectInput("slctDlTreas", "Choose file to download",
                choices = c("Treasuries", "Fed Funds Rate" 
                )                                                     
    )
  })
  
  output$dlTreas <- renderUI({  
    downloadButton('dlTreasrender', 'Download')
  })
  
  output$dlTreasrender <- downloadHandler(
    filename = function() {
      paste(input$slctDlTreas, ".csv", sep='')
    },
    content = function(file) {
      write.zoo(swtchDlTreas(), file, sep=",")
    },
    contentType = "text/csv"
  )
  
  swtchDlEquity <- reactive({
    switch(input$slctDlEquity,
           "S&P 500" = SP500,
           "Russell 1000" = RU1000TR,
           "VIX" = VIXCLS
    )
  })
  
  output$slctDlEquity <- renderUI({
    selectInput("slctDlEquity", "Choose file to download",
                choices = c("S&P 500", "Russell 1000","VIX" 
                )                                                     
    )
  })
  
  output$dlEquity <- renderUI({  
    downloadButton('dlEquityrender', 'Download')
  })
  
  output$dlEquityrender <- downloadHandler(
    filename = function() {
      paste(input$slctDlEquity, ".csv", sep='')
    },
    content = function(file) {
      write.zoo(swtchDlEquity(), file, sep=",")
    },
    contentType = "text/csv"
  )

  
  swtchDlEcon <- reactive({
    switch(input$slctDlEcon,
           "GDP" = GDPC1,
           "CPI" = CPIAUCSL,
           "Unemployment" = UNRATE,
           "UMICH Consumer Sentiment" = UMCSENT,
           "Gold Prices" = GOLDAMGBD228NLBM
    )
  })
  
  output$slctDlEcon <- renderUI({
    selectInput("slctDlEcon", "Choose file to download",
                choices = c("GDP", "CPI", "Unemployment","UMICH Consumer Sentiment", "Gold Prices" 
                )                                                     
    )
  })
  
  output$dlEcon <- renderUI({  
    downloadButton('dlEconrender', 'Download')
  })
  
  output$dlEconrender <- downloadHandler(
    filename = function() {
      paste(input$slctDlEcon, ".csv", sep='')
    },
    content = function(file) {
      write.zoo(swtchDlEcon(), file, sep=",")
    },
    contentType = "text/csv"
  )
  
  swtchDlMortality <- reactive({
    #test <- substring(input$slctDlMortality)
    test1 <- input$slctDlMortality
    test <- substring(test1, gregexpr(" ", test1)[[1]][1]+1 , gregexpr("yo", test1)[[1]][1]+1)
    switch(test,
           #paste("45yo", input$slctGender, sep="") = lca.forecast.x.df(mortForecast(), 45),
           "45yo" = lca.forecast.x.df(mortForecast(), 45),
           "55yo" = lca.forecast.x.df(mortForecast(), 55),
           "65yo" = lca.forecast.x.df(mortForecast(), 65),
           "75yo" = lca.forecast.x.df(mortForecast(), 75)
    )
  })
  
  output$slctDlMortality <- renderUI({
    selectInput("slctDlMortality", "Choose file to download",
                choices = c(paste(input$slctCountry, "45yo", input$slctGender, sep=" "), paste(input$slctCountry, "55yo", input$slctGender, sep=" "),
                            paste(input$slctCountry, "65yo", input$slctGender, sep=" "), paste(input$slctCountry, "75yo", input$slctGender, sep=" ")
                )                                                     
    )
  })
  
  output$dlMortality <- renderUI({  
    downloadButton('dlMortalityrender', 'Download')
  })
  
  output$dlMortalityrender <- downloadHandler(
    filename = function() {
      paste(input$slctDlMortality, ".csv", sep='')
    },
    content = function(file) {
      write.csv(swtchDlMortality(), file , row.names = FALSE)
    },
    contentType = "text/csv"
  )  
  
})
