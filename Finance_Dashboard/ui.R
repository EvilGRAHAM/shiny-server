# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)

# Stock List ----------
stock_list <- 
  rbind(
    tq_exchange("NASDAQ")
    ,tq_exchange("NYSE")
    ,tq_exchange("AMEX")
  )

# Dashboard Header ----------
db_header <- dashboardHeader(
  title = "Financial Dashboard"
)


# Dashboard Sidebar ----------
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    # Sidebar Menu List ----------
    menuItem("Stocks", tabName = "db_stock", icon = icon("line-chart"))
    ,menuItem("Portfolio Optimization", tabName = "db_port_opt", icon = icon("balance-scale"))
    ,menuItem("Energy", tabName = "db_energy", icon = icon("tint"))
    ,menuItem("Metals", tabName = "db_metal", icon = icon("bank"))
    # Date Range ----------
    ,dateRangeInput(
      inputId = "date_range"
      ,label = "Date Range:"
      ,max = today()
      ,start = today() - years(1)
      ,end = today()
    )
    # Bookmark Button ----------
    ,bookmarkButton()
  )
)


# Dashboard Body ----------
db_body <- dashboardBody(
  tabItems(
    
    # Stock Tab ----------
    tabItem(
      tabName = "db_stock"
      ,fluidRow(
        
        # Column 1 ----------
        column(
          6
          # Stock Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Stock Ticker ----------
            ,selectInput(
              inputId = "stock_ticker"
              ,label = "Enter a Stock Symbol:"
              ,choices = stock_list %>% 
                select(symbol) %>%
                arrange(symbol)
              ,multiple = TRUE
              ,selectize = TRUE
            )
            # Index Inputs ----------
            ,selectInput(
              inputId = "index_ticker"
              ,label = "Enter a Stock Index:"
              ,choices = c(
                "XLF"
                ,"SPY"
                ,"VXX"
                ,"EEM"
                ,"GDX"
                ,"QQQ"
                ,"USO"
                ,"IWM"
                ,"EWZ"
                ,"XOP"
              )
              ,multiple = TRUE
              ,selectize = TRUE
            )
            # Fix y-axis Scale Boolean ----------
            ,checkboxInput(
              inputId = "fix_y_axis_scale_ts"
              ,label = "Fix y-axis Scale?"
              ,value = FALSE
            )
          )
          # Price Table ----------
          ,box(
            width = NULL
            ,dataTableOutput("price_tbl")
          )
          # CAPM Regression Ouput ----------
          ,box(
            width = NULL
            ,verbatimTextOutput("capm_regression")
          )
          # Autocorrelation Plot ----------
          ,box(
            width = NULL
            ,plotOutput("acf_plot")
            # Number of days for ACF Lag ----------
            ,sliderInput(
              inputId = "n_acf_lag"
              ,label = "Number of days for Autocorrelation Lag:"
              ,value = c(0, 30)
              ,min = 0
              ,max = 100
              ,step = 1
            )
          )
        )
        
        # Column 2 ----------
        ,column(
          6
          # Price Time Series ----------
          ,box(
            width = NULL
            ,plotOutput(
              "price_ts"
              ,click = clickOpts(
                id = "price_ts_click"
              )
            )
            ,verbatimTextOutput(
              "price_ts_click_info"
            )
            # Number of days for Moving Average ----------
            ,sliderInput(
              inputId = "n_moving_average"
              ,label = "Number of days for Moving Average:"
              ,value = 20
              ,min = 0
              ,max = 365
              ,step = 1
            )
          )
          # Return Time Series ----------
          ,box(
            width = NULL
            ,plotOutput("return_ts")
            # Period for Returns ----------
            ,selectInput(
              inputId = "return_period"
              ,label = "Period for returns:"
              ,choices = c(
                Daily = "daily"
                ,Weekly = "weekly"
                ,Monthly = "monthly"
                ,Quarterly = "quarterly"
                ,Annual = "yearly"
              )
            )
          )
          # Stock vs. Index Returns ----------
          ,box(
            width = NULL
            ,plotOutput("stock_index_return")
          )
          # Market Cap ----------
          ,box(
            width = NULL
            ,plotOutput("market_cap_bc")
          )
          
        )
        
      )
    )
    
    # Portfolio Tab ----------
    ,tabItem(
      tabName = "db_port_opt"
      ,fluidRow(
        
        # Column 1 ----------
        column(
          6
          # Parameter Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Max Volatility ----------
            ,numericInput(
              inputId = "port_max_volatility"
              ,label = "Enter the maximum allowable volatility:"
              ,value = 0.3
              ,min = 0
              ,step = 0.1
            )
            # Number of Iterations
            ,sliderInput(
              inputId = "port_num_iterations"
              ,label = "Number of iterations:"
              ,min = 1
              ,max = 10000
              ,value = 1000
            )
          )
        )
        
        # Column 2 ----------
        ,column(
          6
          # Portfolio Proportions ----------
          ,box(
            width = NULL
            ,plotOutput("port_opt_prop_bc")
          )
        )
      )
    )
    
    # Metals Tab ----------
    ,tabItem(
      tabName = "db_metal"
      ,dataTableOutput("gold")
    )
    
    # Energy Tab ----------
    ,tabItem(
      tabName = "db_energy"
      ,fluidRow(
        
        # Column 1 ----------
        column(
          6
          # Stock Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Stock Ticker ----------
            ,selectInput(
              inputId = "energy_stock_ticker"
              ,label = "Enter a Stock Symbol:"
              ,choices = stock_list %>% 
                filter(sector == "Energy") %>% 
                select(symbol) %>%
                arrange(symbol)
              ,multiple = TRUE
              ,selectize = TRUE
            )
            # Energy Commodity Ticker ----------
            ,selectInput(
              inputId = "energy_commodity_ticker"
              ,label = "Enter a Commodity:"
              ,choices = 
                c(
                  WTI = "DCOILWTICO"
                  ,BRENT = "DCOILBRENTEU"
                  ,`Asian LNG` = "PNGASJPUSDM"
                  ,`Henry Hub Natural Gas Spot Price` = "MHHNGSP"
                  ,`Mount Belvieu Propane Prices` = "DPROPANEMBTX"
                  ,""
                  ,""
                  ,""
                  ,""
                  ,""
                )
              ,multiple = TRUE
              ,selectize = TRUE
            )
          )
        )
        
        # Column 2 ----------
        ,column(
          6
          # Energy Price Time Series ----------
          ,box(
            width = NULL
            ,plotOutput("energy_price_ts")
          )
        )
      )
    )
  )
)



# UI ----------
shinyUI(
  function(request) {
    dashboardPage(
      skin = "black"
      
      ,db_header
      
      ,db_sidebar
      
      ,db_body
      
    )
  }
)
