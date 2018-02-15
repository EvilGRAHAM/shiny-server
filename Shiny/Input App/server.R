# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# Data Import ----------
panss_old <- read_csv(file = "../data/Panssdata_test.csv")


#Server ----------
shinyServer(
  function(input, output, clientData, session) {

    panss_new <- reactive({
      panss_gen <- 
        tibble(
          RATER = max(as.integer(panss_old$RATER)) + 1
          ,LANG = input$language
          ,P1 = input$P1
          ,P2 = input$P2
          ,P3 = input$P3
          ,P4 = input$P4
          ,P5 = input$P5
          ,P6 = input$P6
          ,P7 = input$P7
          ,N1 = input$N1
          ,N2 = input$N2
          ,N3 = input$N3
          ,N4 = input$N4
          ,N5 = input$N5
          ,N6 = input$N6
          ,N7 = input$N7
          ,G1 = input$G1
          ,G2 = input$G2
          ,G3 = input$G3
          ,G4 = input$G4
          ,G5 = input$G5
          ,G6 = input$G6
          ,G7 = input$G7
          ,G8 = input$G8
          ,G9 = input$G9
          ,G10 = input$G10
          ,G11 = input$G11
          ,G12 = input$G12
          ,G13 = input$G13
          ,G14 = input$G14
          ,G15 = input$G15
          ,G16 = input$G16
        )
      rbind(panss_old, panss_gen)
    })
    
    output$new <- renderText(max(panss_old$RATER) + 1)
    
    observeEvent(
      input$submit
      ,{
        showModal(modalDialog(title = "Confirmation", "Are you sure you wish to submit?"))
        write_csv(x = panss_new(), path = "../data/Panssdata_test.csv")
        # panss_old_1() <- read_csv(file = "data/Panssdata_test.csv")
      }
    )

  }
)
