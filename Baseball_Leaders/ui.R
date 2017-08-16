# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(Lahman, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)


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
        selectInput(
          inputId = "data_input"
          ,label = "Data Set:"
          ,choices = c(
            "Batting"
            ,"Pitching"
          )
          ,selected = "Batting"
          ,selectize = TRUE
        )
        # Which statistic to show.
        ,selectInput(
          inputId = "baseball_stat"
          ,label = "Statistic:"
          ,choices = battingLabels %>%
            filter(
              variable != "playerID"
              ,variable != "yearID"
              ,variable != "stint"
              ,variable != "teamID"
              ,variable != "lgID"
            ) %>%
            select(variable)
          # ,choices = c(
          #   "label 1" = "option1"
          #   ,"label 2" = "option2"
          # )
          ,selected = "G"
          ,selectize = TRUE
        )
        # Year that we provides an upper bound for the summing.
        ,sliderInput(
          inputId = "year_lkup"
          ,label = "Year:"
          ,min = 1871
          ,max = year(today())
          ,value = c(1871 , year(today()))
          ,sep = ""
        )
        # Number of players to show.
        ,numericInput(
          inputId = "n"
          ,label = "Number of Players:"
          ,value = 10
          ,min = 1
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

