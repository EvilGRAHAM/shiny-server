# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinymaterial, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# UI ----------
function(request) {
  fluidPage(
    
    # Application title ----------
    titlePanel(title = "Tomb of Annihilation Daily Tracker")
    ,fluidRow(
      # Inputs ----------
      column(
        width = 3
        ,offset = 0
        ,wellPanel(
          # Region ----------
          actionButton(
            inputId = "new_day"
            ,label = "New Day"
          )
          ,bookmarkButton()
          ,textOutput("num_days")
          ,selectizeInput(
            inputId = "region"
            ,label = "Select Region:"
            ,choices = 
              c(
                "Beach"
                ,"Lake"
                ,"Jungle - No Undead"
                ,"Jungle - Lesser Undead"
                ,"Jungle - Greater Undead"
                ,"Mountains"
                ,"Rivers"
                ,"Ruins"
                ,"Swamp"
                ,"Wasteland"
              )
          )
          # Travel Speed ----------
          ,selectizeInput(
            inputId = "travel_speed"
            ,label = "Travel Speed:"
            ,choices = 
              c(
                "Normal" = "n"
                ,"Fast" = "f"
                ,"Slow" = "s"
              )
          )
          # Survival Roll Type ----------
          ,selectizeInput(
            inputId = "survival_roll_type"
            ,label = "Survival Roll Type:"
            ,choices = 
              c(
                "Normal"
                ,"Advantage"
                ,"Disadvantage"
              )
          )
          # Survival Modifier ----------
          ,numericInput(
            inputId = "survival_mod"
            ,label = "Survival Modifier:"
            ,value = 0
          )
        )
      )
      
      # Main ----------
      ,column(
        width = 9
        ,offset = 0
        ,tableOutput("regions_tbl")
        ,tableOutput("pace_tbl")
      )
      ,fluidRow(
        column(
          width = 4
          ,offset = 0
          ,verbatimTextOutput("navigation_check")
        )
        ,column(
          width = 4
          ,offset = 0
          ,tableOutput("random_encounter")
        )
        ,column(
          width = 4
          ,offset = 0
          ,tableOutput("weather")
        )
      )
      ,fluidRow(
        column(
          width = 3
          ,offset = 0
          ,tableOutput("cache_table")
        )
        ,column(
          width = 3 
          ,offset = 0
          ,tableOutput("treasure_table")
        )
        ,column(
          width = 3
          ,offset = 0
          ,tableOutput("dead_explorer_table")
        )
        ,column(
          width = 3
          ,offset = 0
          ,tableOutput("explorer_table")
        )
      )
      
      
    )
    
  )
}
