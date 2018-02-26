# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# UI ----------
fluidPage(
  
  # Application title ----------
  theme = shinytheme("lumen")
  ,titlePanel(title = "PANSS Instrument Results")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      3
      ,offset = 0
      ,wellPanel(
        
      )
    )
    
    # P ----------
    ,column(
      3
      ,offset = 0
      
    )
    
    # N ----------
    ,column(
      3
      ,offset = 0
      
    )
    
    # G ----------
    ,column(
      3
      ,offset = 0
      
    )
  )
  
)

