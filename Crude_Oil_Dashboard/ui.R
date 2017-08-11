# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


# Variables ----------
trade_cycle_list <- tibble(`Trade Cycle` = as_date(NA))
for (i in -1:10){
  trade_cycle_list[i + 2, 1] <- floor_date(today(), unit = "months") - months(i)
}
# data_path <- "Data/crude_oil_dashboard_data.xlsx"
# priority_streams <- 
#   read_excel(data_path, "priority_streams")

get_priority_streams_data <- function(db_con){
  priority_streams <-
    pricing_con %>%
    tbl("PriorityList") %>% 
    select(Stream) %>% 
    as.data.frame()
  # read_excel(data_path, "priority_streams")
}
pricing_con <- DBI::dbConnect(odbc::odbc(),
                              Driver    = "ODBC Driver 13 for SQL Server", 
                              Server    = "avm006.database.windows.net",
                              Database  = "SQLDB4",
                              UID       = "pricingdb",
                              PWD       = "Trading1!",
                              Port      = 1433)
priority_streams <- get_priority_streams_data(pricing_con)


# Dashboard Header ----------
db_header <- dashboardHeader(
  title = "Pricing Dashboard"
)


# Dashboard Sidebar ----------
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    # Sidebar Menu List ----------
    menuItem("Pricing", tabName = "db_pricing", icon = icon("line-chart"))
    # Refresh Data ----------
    ,actionButton(
      inputId = "refresh_data"
      ,label = "Refresh Data"
    )
    # Trade Cycle ----------
    ,selectInput(
      inputId = "trade_cycle"
      ,label = "Enter the Trade Cycle:"
      ,choices = trade_cycle_list
      ,selectize = TRUE
    )
    ,selectInput(
      inputId = "base_stream"
      ,label = "Enter the Base Stream:"
      ,choices = priority_streams$Stream
      ,selectize = TRUE
    )
  )
)


# Dashboard Body ----------
db_body <- dashboardBody(
  tabItems(
    
    tabItem(
      tabName = "db_pricing"
      # Row 1 ----------
      ,fluidRow(
        # Price Time Series Charts ----------
        column(
          8
          ,box(
            plotOutput("price_ts_charts")
            ,selectInput(
              inputId = "chart_stream"
              ,label = "Enter the Base Stream:"
              ,choices = priority_streams$Stream
              ,selectize = TRUE
              ,multiple = TRUE
            )
            ,checkboxInput(
              inputId = "platform_split"
              ,label = "Split by Platform?"
              ,value = TRUE
            )
            ,title = "Time Series Charts"
            ,width = NULL
            ,collapsible = TRUE
          )
        )
        # Summary Charts ----------
        ,column(
          4
          ,tabBox(
            title = "Summary Charts"
            ,id = "summary_charts_tabset"
            ,tabPanel(
              "Blended Index Price Chart"
              ,plotOutput("blended_index_price_chart")
            )
            ,tabPanel(
              "Overall Volume Chart"
              ,plotOutput("overall_volume_chart")
            )
            ,tabPanel(
              "Volume Split Chart"
              ,plotOutput("volume_split_chart")
            )
            ,tabPanel(
              "Trade Split Chart"
              ,plotOutput("trade_split_chart")
            )
            ,width = NULL
            # ,collapsible = TRUE
          )
        )
      )
      
      # Row 2 ----------
      ,fluidRow(
        # Stream Information ----------
        column(
          6
          ,tabBox(
            title = "Stream Information"
            ,id = "stream_info"
            # Trade List ----------
            ,tabPanel(
              "Trades"
              ,dataTableOutput("base_trade_list")
            )
            # Monthly Index ----------
            ,tabPanel(
              "Monthly Indices"
              ,dataTableOutput("base_monthly_index")
            )
            ,width = NULL
          )
        )
        # Historical Charts ----------
        ,column(
          6
          ,tabBox(
            title = "Historical Price"
            ,id = "historical_chart"
            # Base Stream Historical Chart ----------
            ,tabPanel(
              "Base Stream Historical Price"
              ,plotOutput("historical_price_base_ts")
            )
            # WTI Historical Chart ----------
            ,tabPanel(
              "WTI Historical Price"
              ,plotOutput("historical_price_wti_ts")
            )
            ,width = NULL
          )
        )
      )
      # Row n ----------
      ,fluidRow(
        # Summary Table ----------
        column(
          12
          ,box(
            dataTableOutput("summary_tbl")
            ,title = "Summary Table"
            ,width = NULL
            ,collapsible = TRUE
          )
        )
      )
    )
  )
)


# UI ----------
shinyUI(
  dashboardPage(
    skin = "yellow"
    
    ,db_header
    
    ,db_sidebar
    
    ,db_body
    
  )
)