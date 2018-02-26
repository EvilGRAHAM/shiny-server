# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# UI ----------
fluidPage(
  
  # Application title ----------
  theme = shinytheme("lumen")
  ,titlePanel(title = "PANSS Instrument")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      3
      ,offset = 0
      ,wellPanel(
        selectizeInput(
          inputId = "language"
          ,label = "Language"
          ,choices = c(
            "English" = "E"
            ,"French" = "F"
            ,"Italian" = "I"
          )
        )
        ,verbatimTextOutput("new")
        ,actionButton(
          inputId = "submit"
          ,label = "Submit Response"
        )
      )
    )
    
    # P ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "P1"
        ,label = "P1: Delusions"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P2"
        ,label = "P2: Conceptual Disorganization"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P3"
        ,label = "P3: Hallucinatory behavior"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P4"
        ,label = "P4: Excitement"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P5"
        ,label = "P5: Grandiosity"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P6"
        ,label = "P6: Suspiciousness/Persecution"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "P7"
        ,label = "P7: Hostility"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
    
    # N ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "N1"
        ,label = "N1: Blunted affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N2"
        ,label = "N2: Emotional withdrawal"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N3"
        ,label = "N3: Poor rapport"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N4"
        ,label = "N4: Passive/Apathetic social withdrawal"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N5"
        ,label = "N5: Diffculty in abstract thinking"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N6"
        ,label = "N6: Lack of spontaneity"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N7"
        ,label = "N7: Stereotyped thinking"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
    
    # G ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "G1"
        ,label = "G1: Somatic Concern"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G2"
        ,label = "G2: Anxiety"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G3"
        ,label = "G3: Guilt feeling"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G4"
        ,label = "G4: Tension"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G5"
        ,label = "G5: Mannerisms and posturing"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G6"
        ,label = "G6: Depression"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G7"
        ,label = "G7: Motor retardation"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G8"
        ,label = "G8: Uncooperativeness"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G9"
        ,label = "G9: Unusual thought content"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G10"
        ,label = "G10: Disorientation"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G11"
        ,label = "G11: Poor attention"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G12"
        ,label = "G12: Lack of judgment and insight"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G13"
        ,label = "G13: Disturbance of volition"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G14"
        ,label = "G14: Poor impulse control"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G15"
        ,label = "G15: Preoccupation"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "G16"
        ,label = "G16: Active social avoidance"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
  )
  
)

