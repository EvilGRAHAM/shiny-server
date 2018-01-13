# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


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
          full_insult[1] <- as.character(First_Phrase[sample(x = as.numeric(count(First_Phrase)), size = 1), 1])
          
          ## Second Phrase ----------
          full_insult[2] <- 
            case_when(
              full_insult[1] == "Your" ~ as.character(Your_Second_Phrase[sample(x = as.numeric(count(Your_Second_Phrase[, 1])), size = 1), 1])
              ,full_insult[1] == "You're" ~ as.character(Youre_Second_Phrase[sample(x = as.numeric(count(Youre_Second_Phrase[, 1])), size = 1), 1])
              ,full_insult[1] == "I've" ~ as.character(Ive_Second_Phrase[sample(x = as.numeric(count(Ive_Second_Phrase[, 1])), size = 1), 1])
            )
          
          ## Third Phrase ----------
          full_insult[3] <- 
            case_when(
              full_insult[1] == "Your" ~ as.character(Your_Second_Phrase[sample(x = as.numeric(count(Your_Second_Phrase[, 2])), size = 1), 2])
              ,full_insult[1] == "You're" ~ as.character(Youre_Second_Phrase[sample(x = as.numeric(count(Youre_Second_Phrase[, 2])), size = 1), 2])
              ,full_insult[1] == "I've" ~ as.character(Ive_Second_Phrase[sample(x = as.numeric(count(Ive_Second_Phrase[, 2])), size = 1), 2])
            )
          
          ## Fourth Phrase ----------
          full_insult[4] <- 
            case_when(
              full_insult[1] == "Your" ~ as.character(Your_Second_Phrase[sample(x = as.numeric(count(Your_Second_Phrase[, 3])), size = 1), 3])
              ,full_insult[1] == "You're" ~ as.character(Youre_Second_Phrase[sample(x = as.numeric(count(Youre_Second_Phrase[, 3])), size = 1), 3])
              ,full_insult[1] == "I've" ~ as.character(Ive_Second_Phrase[sample(x = as.numeric(count(Ive_Second_Phrase[, 3])), size = 1), 3])
            )
          
          ## Fifth Phrase ----------
          full_insult[5] <- 
            case_when(
              full_insult[1] == "Your" ~ ""
              ,full_insult[1] == "You're" ~ ""
              ,full_insult[1] == "I've" ~ as.character(Ive_Second_Phrase[sample(x = as.numeric(count(Ive_Second_Phrase[, 4])), size = 1), 4])
            )
          paste0(full_insult, collapse = " ")
        }
      )
    
    output$insult_output <- renderPrint(generated_insult())
    
  })
