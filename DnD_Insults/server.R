# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)


# Data Import ----------
First_Phrase <-
  read_xlsx(
    path = "data/Insults.xlsx"
    ,sheet = "First_Phrase"
  )
Your_Second_Phrase <-
  read_xlsx(
    path = "data/Insults.xlsx"
    ,sheet = "Your_Second_Phrase"
  )
Youre_Second_Phrase <-
  read_xlsx(
    path = "data/Insults.xlsx"
    ,sheet = "Youre_Second_Phrase"
  )
Ive_Second_Phrase <-
  read_xlsx(
    path = "data/Insults.xlsx"
    ,sheet = "Ive_Second_Phrase"
  )


#Server ----------
shinyServer(
  function(input, output, clientData, session) {
    
    generated_insult <- 
      eventReactive(
        input$generateInsults
        ,{
          full_insult <- c()
          
          ## First Phrase ----------
          full_insult[1] <- 
            First_Phrase %>% 
            sample_n(size = 1) %>% 
            as.character()
          
          ## Second Phrase ----------
          for (i in 2:5){
            full_insult[i] <- 
              case_when(
                full_insult[1] == "Your" ~ 
                  Your_Second_Phrase[, i-1] %>% 
                  sample_n(size = 1) %>% 
                  as.character()
                ,full_insult[1] == "You're" ~ 
                  Youre_Second_Phrase[, i-1] %>% 
                  sample_n(size = 1) %>% 
                  as.character()
                ,full_insult[1] == "I've" ~ 
                  Ive_Second_Phrase[, i-1] %>% 
                  sample_n(size = 1) %>% 
                  as.character()
              )
          }
          
          paste0(full_insult, collapse = " ")
        }
      )
    
    output$insult_output <- renderPrint(generated_insult())
    
  })
