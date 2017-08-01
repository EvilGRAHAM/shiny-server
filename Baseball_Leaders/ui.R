library(shiny)
library(tidyverse)
library(Lahman)
library(lubridate)

shinyUI(
  fluidPage(
  
    # Application title
    titlePanel("Baseball Individual Leaderboard"),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
  
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  
  )
)
