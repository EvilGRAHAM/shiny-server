# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)


# UI ----------
fluidPage(
  
  # Application title ----------
  theme = shinytheme("lumen")
  ,titlePanel(title = "", windowTitle = "D&D Insult Generator")
  ,headerPanel(HTML('<p><img src="D&D_Transparent.png" height=60 width=120/></p> Insult Generator'))
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      4
      ,offset = 0
      ,wellPanel(
        actionButton(
          inputId = "generateInsults"
          ,label = "Generate Insults"
        )
      )
    )
    
    # Main ----------
    # Displays the generated insult
    ,column(
      8
      ,offset = 0
      ,verbatimTextOutput("insult_output")#, placeholder = TRUE)
      
    )
    
  )
  
)

