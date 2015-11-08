
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(lubridate)
library(leaflet)

shinyUI(navbarPage("Actuarial Risk Dashboard",
  tabPanel("About",
    fluidRow(
      column(12,
        mainPanel(
          h1("Actuarial Risk Dashboard"),
          br(),
          h2("Introduction"),
          p("This app brings together economic information as well as mortality trends across the world.
            This app obtains important data, displays it visually, and develops correlations and 99.5th percentile shocks for purposes of analytical Value-at-Risk
            (VaR). Business cycle information from the National Bureau of Economic Research has been 
            added to help identify difficult scenarios. There is a goal 
            to clearly connect these date ranges with the time-series information in order to understand how  
            certain indicators were moving as a whole. Business cycles are marked on plots by the pink 
            shaded areas. Well-known models of mortality are applied to historical mortality rates and forecasts with confidence interverals are provided. 
            Concentration related risks are explored with visualizaton through mapping. Calculators are provided
            that allow for options to be priced and to explore the price sensitivity through the \"greeks\"."),
          h2("Important Note"),
          p("This app pulls data from many source when loading. An indicator in the top-right portion of the browser should identify this activity. This can take 10-15 seconds but once complete 
            the data is in memory allowing the app to provide adequate responsiveness to changes in user input."),
          h2("Documentation"),
          p("This app includes 5 main content tabs described below."),
          h3("1. Market Data"),
          p("   Market Data consists of tabs displaying historical time-series data. Controls to the left allow users to select 
            the historical time horizon that is desired and in some cases the number of series to display. Finally, there 
            are controls that allow users to download data supporting the plots. "),
          h3("2. Economic Data"),
          p("   Like the Market Data component, this consists of a tab displaying historical time-series data. Controls to the left allow users to select 
            the historical time horizon that is desired and allow users to download data supporting the plots. The second tab
            shows business-cycle data that is shown as an overlay to the market and economic time-series plots."),
          h3("3. Mortality"),
          p("   This component allows users to select from a list of countries in order to plot and forecast mortality rates. Controls to the left
            allows users to select a country, a gender, a projection horizon, whether or not to use all historical data or a subset that provides the best fit,
            and a confidence interval to include as part of the forecasts. Finally, there is a control that allows users to download data supporting the plots."),
          h3("4. Risk"),
          p("   This component summarizes some relationships between market data and their degree of variability and also includes a map that reads exposures 
            from a CSV file capturing the concentration of insurance premium originating from each US state (for a sample company). This component is informational and doesn't include any controls that allow the user to manipulate what is shown."),
          h3("5. Calculators"),
          p("   The calculators component of the app has a working version of a Black-Scholes calculator for European options and the foundation for a volatility control
            index calculator. Since the latter is still in development, it won't be discussed further. The options calculator allows users to select either a put or call option, a current underlying price, a relationship between the current underlying price and the strike
            price, a risk-free rate, a dividend rate, the volatility of the underlying price, and the time until maturity of the option. Once a user selects these inputs, a table is updated that includes the model price of the options and its \"greeks\". The results also 
            include a number of sensitivity plots that show how the \"greeks\" change with the underlying price. These are very useful visual representations of important relationships in options theory."),
          h2("Acknowledgements"),
          p("Developed by Jason Newkirk"),
          p("Economic data from Federal Reserve of Saint Louis"),
          p("Business cycle data from National Bureau of Economic Research"),
          p("Default data from Moody's"),
          p("Mortality data from the Human Mortality Database via 'demography'"),
          p("The app is hosted on http://www.shinyapps.io for free. Free means there are some limitations including
            a limit to the number of Active Hours per month. The current limit is 25 hours per month. Active Hours are the number of hours the app
            is up and serving requests. To help save the amount of Active Hours, the app goes idle 10 minutes after starting.
            Additional Active Hours can be purchased (see shinyapps.io for pricing) but I don't have plans to do that at the moment.")
        )
      )
    )
  ),
  navbarMenu("Market Data",
    tabPanel("Swaps, Spreads, Defaults",
      fluidRow(
        column(3,
           wellPanel(
             dateRangeInput("dtrng.rates",
                            "Date range:",
                            start= as.Date(ymd(Sys.Date()-years(10))),
                            end = Sys.Date(),
                            startview = "year",
                            separator = " - "),
             
             checkboxGroupInput("chckBxGrpMaturity", "Maturity:",
                                c("1" = "X1",
                                  "3" = "X3",
                                  "5" = "X5",
                                  "7" = "X7",
                                  "10" = "X10",
                                  #"20" = "X20",
                                  "30" = "X30"),
                                selected = c("X1", "X3", "X5", "X7", "X10","X20", "X30" ), inline=FALSE),
             
             checkboxGroupInput("chckBxGrpRating", "Rating:",
                                c("AAA" = "AAA",
                                  "AA" = "AA",
                                  "A" = "A",
                                  "BBB" = "BBB",
                                  "BB" = "BB",
                                  "B" = "B",
                                  "C" = "C"),
                                selected = c("AAA", "AA", "A", "BBB"), inline=FALSE),
             uiOutput("slctDlSSD"),
             uiOutput("dlSSD")
           )
        ),
        mainPanel(
          h2("Swaps"),
          plotOutput("plot.swaps"),
          h2("BofA Merrill Lynch Option-Adjusted Spread© (US)"),
          plotOutput("plot.spreads"),
          h2("Moody's Corporate Bond Defaults"),
          plotOutput("plot.moodysCorp")
        )
      )
    ),
    tabPanel("Treasuries, Fed Funds",
      fluidRow(
        column(3,
         wellPanel(
           dateRangeInput("dtrng.Trates",
                          "Date range:",
                          start= as.Date(ymd(Sys.Date()-years(10))),
                          end = Sys.Date(),
                          startview = "year",
                          separator = " - "),
           
           checkboxGroupInput("chckBxGrpTMaturity", "Maturity:",
                              c("1" = "DGS1",
                                "3" = "DGS3",
                                "5" = "DGS5",
                                "7" = "DGS7",
                                "10" = "DGS10",
                                "20" = "DGS20",
                                "30" = "DGS30"),
                              selected = c("DGS1", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30" ), inline=FALSE),
           uiOutput("slctDlTreas"),
           uiOutput("dlTreas")
          )
        ),
        mainPanel(
          h2("Treasuries"),
          plotOutput("plot.treas"),
          h2("Effective Federal Funds Rate"),
          plotOutput("plot.ffr")
        )
      )
    ),
    tabPanel("Equity",
        fluidRow(
          #sidebarLayout(
          column(3,
                 #sidebarPanel(
                 wellPanel(
                   dateRangeInput("dtrng.equity",
                                  "Date range:",
                                  start= as.Date(ymd(Sys.Date()-years(10))),
                                  end = Sys.Date(),
                                  startview = "year",
                                  separator = " - "),
                   
                   uiOutput("slctDlEquity"),
                   uiOutput("dlEquity")
                   
                 )
          ),
          mainPanel(
            h2("S&P 500©"),
            plotOutput("plot.sp500"),
            h2("Russell 1000® Total Market Index"),
            plotOutput("plot.russell1000"),
            h2("CBOE Volatility Index: VIX©"),
            plotOutput("plot.vix")
          )
        )
    )
  ),
  navbarMenu("Economy",
    tabPanel("Time-Series Data",
      fluidRow(
        column(3,
               wellPanel(
                 dateRangeInput("dtrng.econ",
                                "Date range:",
                                start= as.Date(ymd(Sys.Date()-years(10))),
                                end = Sys.Date(),
                                startview = "year",
                                separator = " - "),
                 
                 uiOutput("slctDlEcon"),
                 uiOutput("dlEcon")
                 
               )
        ),
        mainPanel(
          h2("Real Gross Domestic Product"),
          plotOutput("plot.gdp"),
          h2("Consumer Price Index: Total All Items for the United States©"),
          plotOutput("plot.cpi"),
          h2("Civilian Unemployment Rate"),
          plotOutput("plot.unrate"),
          h2("University of Michigan: Consumer Sentiment©"),
          plotOutput("plot.umichcs"),
          h2("Gold Fixing Price in London Bullion Market, based in U.S. Dollars"),
          plotOutput("plot.gold")
        )
      )
    ),
    tabPanel("Business Cycles",
      fluidRow(
        column(3,
          wellPanel(
            helpText(   a("National Bureau of Economic Research",     href="http://www.nber.org/cycles/US_Business_Cycle_Expansions_and_Contractions_20120423.pdf", target="_blank")
            )
          )
        ),
        mainPanel(
          h2("Business Cycle History"),
          h3("Since the start of 20th century"),
          dataTableOutput("table.bc")
        )
      )
    )
  ),
  tabPanel("Mortality",
    fluidRow(
      column(3,
        wellPanel(
          selectInput("slctCountry", "Country", c("AUS", "GBR_NP", "DEUTNP", "ESP", "ITA", "NLD", "USA", "JPN", "RUS", "SWE","TWN" ), selected = "USA", multiple = FALSE),
          selectInput("slctGender", "Gender", c("male", "female", "total" ), selected = "total", multiple = FALSE),
          numericInput("nmrcHorizon", "Projection Horizon", min=0, max=100, value=50, step=5),
          checkboxInput("chckbxChoosePeriod", "Best Fitting Period (versus all data)"),
          sliderInput("sldrConfInt", "Confidence Internal", min=0, max=100, value=99.5, step=.25, post="%"),
          
          uiOutput("slctDlMortality"),
          uiOutput("dlMortality")
        )
      ),
      mainPanel(
        h2("Lee-Carter Mortality Projections"),
        plotOutput("plot.mx45"),
        plotOutput("plot.mx55"),
        plotOutput("plot.mx65")
      )
    )
  ),
  navbarMenu("Risk",
    tabPanel("Correlation",
      fluidRow(
        column(3,
          wellPanel(
            h2("Financial Risk")
            #p("Add:"),
            #p("Selectable date range"),
            #p("Analytical GMDB calculator"),
            #p("Simulators for non-analytical VA??"),
            #p("Split financial risk and underwriting risk"),
            #p("Operational risk methods?? Implement a few basic models?")
          )
        ),
        mainPanel(
          h2("Daily Percentage Changes"),
          plotOutput("plot.risk.diff"),
          h3("Correlation Matrix"),
          tableOutput("table.correl"),
          h3("Standard Deviation and Percentiles (Monthly Changes)"),
          tableOutput("table.stdev")
        )
      )
    ),
    tabPanel("Concentration",
      fluidRow(
        column(3,
            wellPanel(
              h2("Concentration Risk")
            )
          ),
        mainPanel(
          h3("Premium and Annuity Considerations"),
          plotOutput("mymap")
        )
      )
    )
  ),
  navbarMenu("Calculators",
    tabPanel("Black-Scholes",
      fluidRow(
        column(3,
          wellPanel(
            selectInput("CallPut", "Option Type", c("Call", "Put"), selected=c("Call")),
            numericInput("Price", "Stock Price", min=0, value=1000, step=1),
            sliderInput("Strike", "Strike Divided By Price", min=0, max=200, value=100, step=1, post="%"),
            sliderInput("RiskFreeRate", "Annualized Risk-Free Rate", min=0, max=20, value=5, step=1, post="%"),
            sliderInput("Dividend", "Annualized Dividend Rate", min=0, max=20, value=0, step=1, post="%"),
            sliderInput("Volatility", "Annualized Price Volatility", min=0, max=100, value=20, step=1, post="%"),
            numericInput("Time", "Time to Maturity in Years", min=0, value=1, step=.25)
          )
        ),
        mainPanel(
          h2("Black-Scholes"),
          tableOutput("tbl.risk.options"),
          plotOutput("plot.risk.delta"),
          plotOutput("plot.risk.gamma"),
          plotOutput("plot.risk.theta"),
          plotOutput("plot.risk.vega"),
          plotOutput("plot.risk.rho")
        )
      )
    ),
    tabPanel("Volatility Control Index",
      fluidRow(
        column(3,
          wellPanel(
            selectInput("VCImodel", "Model Type", c("Heston-Nandi GARCH(1,1)"), selected=c("Heston-Nandi GARCH(1,1)")),
            numericInput("VCInumsim", "Number of Simulations", min=5000, value=5000, step=5000),
            dateInput("VCIvaldate", "Valuation Date", value= as.Date(ymd(Sys.Date())-days(7))),
            sliderInput("VCItargetlevel", "Target Volatility", min=0, max=100, value=5.5, step=.5, post="%"),
            sliderInput("VCImaxleverage", "Maximum Leverage", min=0, max=100, value=100, step=5, post="%"),
            sliderInput("VCImaxchange", "Maximum Change", min=0, max=100, value=10, step=5, post="%"),
            actionButton("VCIrunbutton", "Run Simulation")
          )
        ),
        mainPanel(
          h2("Volatility Control Index"),
          h3("Coming very soon!!!"),
          textOutput("tbl.volcontrol")
        )                       
      )
    )
  )
))
