#Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
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
    ,menuItem("Metals", tabName = "db_metal", icon = icon("bank"))
    ,menuItem("Energy", tabName = "db_energy", icon = icon("tint"))
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
    
    tabItem(
      tabName = "db_stock"
      ,fluidRow(
        
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
            # Period for Returns ----------
            ,selectInput(
              inputId = "return_period"
              ,label = "Period for returns:"
              ,choices = c(
                Daily = "daily"
                ,Monthly = "monthly"
                ,Quarterly = "quarterly"
                ,Annual = "yearly"
              )
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
        )
        
        ,column(
          6
          # Price Time Series ----------
          ,box(
            width = NULL
            ,plotOutput("price_ts")
          )
          # Return Time Series ----------
          ,box(
            width = NULL
            ,plotOutput("return_ts")
          )
          # Stock vs. Index Returns ----------
          ,box(
            width = NULL
            ,plotOutput("stock_index_return")
          )
        )
        
      )
    )
    
    ,tabItem(
      tabName = "db_metal"
      ,dataTableOutput("gold")
    )
    
    ,tabItem(
      tabName = "db_energy"
      ,fluidRow(
        
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
