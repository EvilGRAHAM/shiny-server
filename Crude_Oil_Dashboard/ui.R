# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
# library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
# library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


# Dashboard Header ----------
db_header <- dashboardHeader(
  title = "Pricing Dashboard"
)


# Dashboard Sidebar ----------
trade_cycle_list <- tibble(`Trade Cycle` = as_date(NA))
for (i in -1:10){
  trade_cycle_list[i + 2, 1] <- floor_date(today(), unit = "months") - months(i)
}
data_path <- "Data/crude_oil_dashboard_data.xlsx"
priority_streams <- 
  read_excel(data_path, "priority_streams")

db_sidebar <- dashboardSidebar(
  sidebarMenu(
    # Sidebar Menu List ----------
    menuItem("Pricing", tabName = "db_pricing", icon = icon("line-chart"))
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
      # Summary Table ----------
      ,fluidRow(
        
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
      
      # Charts ----------
      ,fluidRow(
        # Price Time Series Charts ----------
        column(
          8
          ,box(
            selectInput(
              inputId = "chart_stream"
              ,label = "Enter the Base Stream:"
              ,choices = priority_streams$Stream
              ,selectize = TRUE
              ,multiple = TRUE
            )
            ,plotOutput("price_ts_charts")
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
              "Volume Split Chart"
              ,plotOutput("volume_split_chart")
            )
            ,tabPanel(
              "Trade Split Chart"
              ,plotOutput("trade_split_chart")
            )
            ,width = NULL
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