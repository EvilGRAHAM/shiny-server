# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(ggridges, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)

# Stock List ----------
stock_list <- 
  list(
    NASDAQ = tq_exchange("NASDAQ")
    ,NYSE = tq_exchange("NYSE")
    ,AMEX = tq_exchange("AMEX")
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
        # Stock Inputs ----------
        box(
          title = "Inputs"
          ,width = 4
          ,collapsible = TRUE
          # Stock Ticker ----------
          ,selectInput(
            inputId = "stock_ticker"
            ,label = "Enter a Stock Symbol:"
            ,choices = 
              # stock_list %>% 
              # select(symbol) %>%
              # arrange(symbol)
              stock_list %>% 
              map(~ select(., symbol)) %>% 
              map(~ arrange(., symbol)) %>% 
              map(~ .$symbol)
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
          title = "Data"
          ,width = 8
          ,collapsible = TRUE
          ,dataTableOutput("price_tbl")
        )
      )
      
      
      ,fluidRow(
        # Column 1 ----------
        column(
          width = 6
          # CAPM Regression Ouput ----------
          ,box(
            title = "CAPM Regression"
            ,width = NULL
            ,collapsible = TRUE
            ,verbatimTextOutput("capm_regression")
          )
          # Stock vs. Index Returns ----------
          ,box(
            title = "Stock vs. Index Returns"
            ,width = NULL
            ,plotOutput("stock_index_return")
          )
          # Autocorrelation Plot ----------
          ,box(
            title = "Autocorrelation Plot"
            ,width = NULL
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
          width = 6
          # Price Time Series ----------
          ,tabBox(
            title = "Time Series Plots"
            ,width = NULL
            ,tabPanel(
              title = "Price"
              ,plotOutput(
                "price_ts"
                ,click = clickOpts(id = "price_ts_click")
              )
              ,verbatimTextOutput("price_ts_click_info")
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
            ,tabPanel(
              title = "Volume"
              ,plotOutput("volume_ts")
            )
            ,tabPanel(
              title = "Returns"
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
          )
          # Return Histogram Plot ----------
          ,box(
            title = "Histogram of Returns"
            ,width = NULL
            ,plotOutput("return_hist_ridge")
          )
          # Market Cap ----------
          ,box(
            title = "Market Capitalization"
            ,width = NULL
            ,collapsible = TRUE
            ,collapsed = TRUE
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
          width = 6
          # Parameter Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Min Return ----------
            ,numericInput(
              inputId = "port_min_return"
              ,label = "Enter the Minimum Annual Return:"
              ,value = 0.3
              ,min = 0
              ,step = 0.1
            )
            # Allow Short Selling ----------
            ,checkboxInput(
              inputId = "short_sale"
              ,label = "Allow Short Selling?"
              ,value = FALSE
            )
          )
          # Summary Stats ----------
          ,box(
            title = "Summary Statistics"
            ,width = NULL
            # Summary Stats Table ----------
            ,dataTableOutput("port_opt_stats")
          )
          # Covariance Matrix ----------
          ,box(
            title = "Covariance Matrix"
            ,width = NULL
            # Covariance Matrix Table ----------
            ,dataTableOutput("port_cov_mat")
          )
        )
        
        # Column 2 ----------
        ,column(
          width = 6
          # Portfolio Proportions ----------
          ,box(
            width = NULL
            ,plotOutput("port_opt_weight_bc")
          )
          # Portfolio Returns Time Series ----------
          ,box(
            width = NULL
            ,plotOutput("port_return_ts")
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
          width = 6
          # Stock Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Stock Ticker ----------
            ,selectInput(
              inputId = "energy_stock_ticker"
              ,label = "Enter a Stock Symbol:"
              ,choices = 
                # stock_list %>% 
                # filter(sector == "Energy") %>% 
                # select(symbol) %>%
                # arrange(symbol)
                stock_list %>% 
                map(~ filter(., sector == "Energy")) %>% 
                map(~ select(., symbol)) %>% 
                map(~ arrange(., symbol)) %>% 
                map(~ .$symbol)
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
          width = 6
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
