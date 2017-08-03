#Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


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
    ,menuItem("Oil", tabName = "db_oil", icon = icon("tint"))
    # Date Range ----------
    ,dateRangeInput(
      inputId = "date_range"
      ,label = "Date Range:"
      ,max = today()
      ,start = today() - years(1)
      ,end = today()
    )
  )
)


# Dashboard Body ----------
db_body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "db_stock"
      ,fluidRow(
        
        column(6
          # Stock Inputs ----------
          ,box(
            title = "Inputs"
            ,width = NULL
            # Stock Ticker ----------
            ,selectInput(
              inputId = "stock_ticker"
              ,label = "Enter a Stock Symbol:"
              ,choices = 
                rbind(
                  tq_exchange("NASDAQ")
                  # ,tq_exchange("NYSE")
                  # ,tq_exchange("AMEX")
                ) %>%
                select(symbol) %>%
                arrange(symbol)
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
        )
        
        ,column(6
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
        )
      
      )
    )
    
    ,tabItem(tabName = "db_metal"
      ,dataTableOutput("gold")
    )
    
    ,tabItem(tabName = "db_oil")
  )
)



# UI ----------
shinyUI(
  dashboardPage(
    skin = "black"
    
    ,db_header
    
    ,db_sidebar
    
    ,db_body

  )
)
