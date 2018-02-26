# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# UI ----------
fluidPage(
  
  # App Title ----------
  titlePanel("Results of PANSS Testing")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      width = 4
      ,offset = 0
      ,wellPanel(
        selectizeInput(
          inputId = "question_set"
          ,label = "Questions Displayed"
          ,choices = 
            c(
              "Positive" = "P"
              ,"Negative" = "N"
              ,"Generic" = "G"
            )
        )
        ,checkboxInput(
          inputId = "by_lang"
          ,"Facet by Language"
          ,value = FALSE
        )
        ,numericInput(
          inputId = "rater_num"
          ,label = "Enter your Rater ID"
          ,value = 1
          ,min = 1
          # ,max = 81
          ,step = 1
        )
      )
      ,verbatimTextOutput("logit")
    )

    # Plots ---------
    ,column(
      width = 8
      ,offset = 0
      ,dataTableOutput("results")
      ,plotOutput("hist")
      ,plotOutput("violin")
      ,plotOutput("prop")
    )
  )
)
