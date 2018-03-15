# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinymaterial, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# UI ----------
material_page(
  
  # Application title ----------
  title = "Tomb of Annihilation Daily Tracker"
  ,nav_bar_color = "blue-grey"
  ,material_row(
    # Inputs ----------
    material_column(
      width = 3
      ,offset = 0
      ,material_card(
        depth = 2
        # Region ----------
        ,material_button(
          input_id = "new_day"
          ,label = "New Day"
          ,color = "deep-purple"
        )
        ,material_dropdown(
          input_id = "region"
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
          ,color = "deep-purple"
        )
        # Travel Speed ----------
        ,material_dropdown(
          input_id = "travel_speed"
          ,label = "Travel Speed:"
          ,choices = 
            c(
              "Normal" = "n"
              ,"Fast" = "f"
              ,"Slow" = "s"
            )
          ,color = "deep-purple"
        )
        # Survival Roll Type ----------
        ,material_dropdown(
          input_id = "survival_roll_type"
          ,label = "Survival Roll Type:"
          ,choices = 
            c(
              "Normal"
              ,"Advantage"
              ,"Disadvantage"
            )
          ,color = "deep-purple"
        )
        # Survival Modifier ----------
        ,material_number_box(
          input_id = "survival_mod"
          ,label = "Survival Modifier:"
          ,min_value = -5
          ,max_value = 30
          ,initial_value = 0
          ,color = "deep-purple"
        )
      )
    )
    
    # Main ----------
    ,material_column(
      width = 9
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("regions_tbl")
      )
      ,material_card(
        depth = 2
        ,tableOutput("pace_tbl")
      )
    )
  )
  ,material_row(
    material_column(
      width = 4
      ,offset = 
        ,material_card(
          depth = 2          
          ,verbatimTextOutput("navigation_check")
        )
    )
    ,material_column(
      width = 4
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("random_encounter")
      )
    )
    ,material_column(
      width = 4
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("weather")
      )
    )
  )
  ,material_row(
    material_column(
      width = 6
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("cache_table")
      )
    )
    ,material_column(
      width = 6
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("treasure_table")
      )
    )
  )
  ,material_row(
    material_column(
      width = 6
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("dead_explorer_table")
      )
    )
    ,material_column(
      width = 6
      ,offset = 0
      ,material_card(
        depth = 2
        ,tableOutput("explorer_table")
      )
    )
  )
  
  
)
