# Libraries ----------
library(shiny)
library(shinythemes)
library(tidyverse)
library(Lahman)
library(lubridate)

# Baseball Statistics ----------
baseball_stats <- 
  Batting %>% 
  as.tibble() %>% 
  select(
    -c(
      playerID
      ,yearID
      ,stint
      ,teamID
      ,lgID
    )
  ) %>% 
  colnames()
# UI ----------
fluidPage(
  
  # Application title ----------
  theme = shinytheme("lumen")
  
  ,titlePanel("Baseball Individual Leaderboard")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      4
      ,offset = 0
      ,wellPanel(
        # Which statistic to show.
        selectInput(
          inputId = "baseball_stat"
          ,label = "Statistic:"
          ,baseball_stats
          ,selectize=TRUE
        )
        # Year that we provides an upper bound for the summing.
        ,sliderInput(
          inputId = "year_lkup"
          ,label = "Year:"
          ,min = 1871
          ,max = year(today())
          ,value = c(1871, year(today()))
          ,sep = ""
        )
        # Number of players to show.
        ,numericInput(
          inputId = "n"
          ,label = "Number of Players:"
          ,value = 10
        )
      )
    )
    
    # Main ----------
    # Show a plot of the generated distribution
    ,column(
      8
      ,offset = 0
      ,tabsetPanel(type = "tabs"
        ,tabPanel(
          "Leaderboard Plot"
          ,plotOutput("leaderboardPlot")
        )
        ,tabPanel(
          "Leaderboard Table"
          ,tableOutput("leaderboardTable")
        )
      )
    )
    
  )
  
)

