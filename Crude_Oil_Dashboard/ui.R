# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
# library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
# library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


# Dashboard Header ----------
db_header <- dashboardHeader(
  title = "Pricing Dashboard"
)

trade_cycle_list <- tibble(`Trade Cycle` = as.Date(NA))
for (i in -1:10){
  trade_cycle_list[i + 2, 1] <- floor_date(today(), unit = "months") - months(i)
}
# Dashboard Sidebar ----------
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
  )
)


# Dashboard Body ----------
db_body <- dashboardBody(
  tabItems(
    
    tabItem(
      tabName = "db_pricing"
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