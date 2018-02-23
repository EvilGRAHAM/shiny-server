# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# Variables ----------
regions <- 
  c(
    "Beach"
    ,"Jungle - No Undead"
    ,"Jungle - Lesser Undead"
    ,"Jungle - Greater Undead"
    ,"Mountains"
    ,"Rivers/Lake"
    ,"Ruins"
    ,"Swamp"
    ,"Wasteland"
  )

paces <- 
  c(
    "f" = "Fast"
    ,"n" = "Normal"
    ,"s" = "Slow"
  )

roll_types =
  c(
    "n" = "Normal"
    ,"a" = "Advantage"
    ,"d" = "Disadvantage"
  )

region_checks <-
  tibble(
    Region = regions
    ,`Navigation DC` = 
      c(
        rep(x = 10, times = 1, each = 1)
        ,rep(x = 15, times = 4, each = 1)
        ,rep(x = 10, times = 1, each = 1)
        ,rep(x = 15, times = 3, each = 1)
      )
    ,`Walking Hexes` = rep(x = 1, times = 9)
    ,`Canoeing Hexes` =
      c(
        rep(x = 1, times = 5, each = 1)
        ,rep(x = 2, times = 1, each = 1)
        ,rep(x = 1, times = 3, each = 1)
      )
    ,`Random Encounters` =
      c(
        rep(x = 16, times = 2, each = 1)
        ,rep(x = 14, times = 1, each = 1)
        ,rep(x = 12, times = 1, each = 1)
        ,rep(x = 16, times = 5, each = 1)
      )
    ,`Foraging DC` =
      c(
        rep(x = 10, times = 3, each = 1)
        ,rep(x = 15, times = 2, each = 1)
        ,rep(x = 10, times = 2, each = 1)
        ,rep(x = 15, times = 1, each = 1)
        ,rep(x = 20, times = 1, each = 1)
      )
  ) %>% 
  mutate_if(.predicate = is.numeric, .funs = as.integer)

pace_modifiers <-
  tibble(
    Pace = paces
    ,`Navigation Bonus` = 5 * (-1:1)
    ,`Hex Modifier (50% Chance)` = -1 * (-1:1)
    ,`Passive Perception` = 5 * c(-1, 0, 0)
    ,`Dehydration Bonus` = 5 * c(-1, 0, 0)
    ,`Sneak?` = c(F, F, T)
    ,`Forage?` = c(F, T, T)
  ) %>% 
  mutate_if(.predicate = is.numeric, .funs = as.integer)

weather_conditions <-
  tibble(
    d20 = 1:20
    ,Precipitation = 
      c(
        rep(x = "None", times = 1, each = 1)
        ,rep(x = "Light", times = 8, each = 1)
        ,rep(x = "Medium", times = 7, each = 1)
        ,rep(x = "Heavy", times = 3, each = 1)
        ,rep(x = "Tropical Storm", times = 1, each = 1)
      )
    ,Wind =
      c(
        rep(x = "None", times = 12, each = 1)
        ,rep(x = "Light", times = 5, each = 1)
        ,rep(x = "Heavy", times = 3, each = 1)
      )
  )

random_encounter_tbl <- 
  "data/random_encounters.csv" %>% 
  read_csv() %>% 
  gather(
    key = Region
    ,value = Roll
    ,-Encounter
  ) %>% 
  filter(!is.na(Roll)) %>% 
  mutate(
    Bound = str_extract(Region, ".B$")
    ,Region = 
      Region %>% 
      str_replace(pattern = ".B$", replacement = "") %>% 
      str_trim()
  ) %>% 
  spread(
    key = Bound
    ,value = Roll
  ) %>% 
  mutate(UB = if_else(is.na(UB), LB, UB))

# Server ----------
shinyServer(
  function(input, output) {
    # Regions Table ----------
    output$regions_tbl <- renderTable(region_checks)

    # Pace Table ----------
    output$pace_tbl <- renderTable(pace_modifiers)
    
    # Navigation Check ----------
    output$navigation_check <- renderPrint({
      nav_region <- input$region
      nav_travel_speed <- input$travel_speed
      nav_survival_mod <- input$survival_mod
      nav_roll_type <- if_else(weather_gen()[1, 1] == 20, "Disadvantage", input$survival_roll_type)
      
      nav_dc <- 
        region_checks %>% 
        filter(Region == nav_region) %>% 
        select(`Navigation DC`) %>% 
        as.integer()
      
      nav_pace_mod <- 
        pace_modifiers %>% 
        filter(Pace == paces[[nav_travel_speed]]) %>% 
        select(`Navigation Bonus`) %>% 
        as.integer()
      
      hex_mod <- 
        pace_modifiers %>% 
        filter(Pace == paces[[nav_travel_speed]]) %>% 
        select(`Hex Modifier (50% Chance)`) %>% 
        as.integer()
      
      nav_pace_sign <- if_else(nav_pace_mod < 0, "-", "+")
      
      nav_survival_sign <- if_else(nav_survival_mod < 0, "-", "+")
      
      nav_rolls <- 
        1:20 %>% 
        sample(
          size = if_else(nav_roll_type == roll_types[["n"]], 1, 2)
          ,replace = TRUE
        )
      
      nav_rolls <- 
        case_when(
          nav_roll_type == roll_types[["n"]] ~ nav_rolls
          ,nav_roll_type == roll_types[["a"]] ~ max(nav_rolls)
          ,nav_roll_type == roll_types[["d"]] ~ min(nav_rolls)
        )[1]
      
      nav_check <- nav_rolls + nav_survival_mod + nav_pace_mod
      
      inequality <-if_else(nav_check >= nav_dc, ">=", "<")
      
      hex_roll <- rbinom(n = 1, size = 1, prob = 0.5)
      nav_hex_mod <- 
        if_else(
          (hex_roll == 0 & nav_travel_speed == "s") | (hex_roll == 1 & nav_travel_speed == "f")
          ,hex_mod
          ,as.integer(0)
        )
      
      nav_result <- 
        cat(
          "Navigation Check Result:"
          ,nav_rolls
          ,nav_survival_sign
          ,abs(nav_survival_mod)
          ,nav_pace_sign
          ,abs(nav_pace_mod)
          ,"="
          ,nav_check
          ,inequality
          ,nav_dc
          ,"\n"
          ,"Getting Lost Random Direction:"
          ,sample(x = 1:6, size = 1 + nav_hex_mod, replace = TRUE)
          ,"\n"
          ,"Extra Hex Traveled:"
          ,nav_hex_mod
          ,"Hex"
        )
    })
    
    # Random Encounter ----------
    output$random_encounter <- renderTable({
      encounter_region <- input$region
      input$travel_speed
      input$survival_mod
      input$survival_roll_type
      
      encounter_dc <- 
        region_checks %>% 
        filter(Region == encounter_region) %>% 
        select(`Random Encounters`) %>% 
        as.integer()
      
      encounter_check <- sample(x = 1:20, size = 3, replace = TRUE)
      encounter_rolls <- sample(x = 1:100, size = 3, replace = TRUE)
      encounter_display <-
        tibble(
          `Time of Day` = 
            c(
              "Morning"
              ,"Afternoon"
              ,"Evening"
            )
          ,`Encounter Check` = encounter_check
        ) %>% 
        mutate(
          `Encounter ID` = 
            if_else(
              `Encounter Check` >= encounter_dc
              ,encounter_rolls
              ,as.integer(0)
            )
        )
      
      random_encounter_tbl %>% 
        right_join(
          encounter_display %>%
            select(
              `Time of Day`
              ,`Encounter ID`
            ) %>%
            spread(
              key = `Time of Day`
              ,value = `Encounter ID`
            ) %>% 
            mutate(Region = encounter_region) %>% 
            select(
              Region
              ,Morning
              ,Afternoon
              ,Evening
            )
          ,by = "Region"
        ) %>% 
        mutate(
          `Morning Check` = if_else(Morning >= LB & Morning <= UB, TRUE, FALSE)
          ,`Afternoon Check` = if_else(Afternoon >= LB & Afternoon <= UB, TRUE, FALSE)
          ,`Evening Check` = if_else(Evening >= LB & Evening <= UB, TRUE, FALSE)
        ) %>% 
        filter(
          `Morning Check` | `Afternoon Check` | `Evening Check`
        ) %>%
        select(
          LB
          ,UB
          ,Morning
          ,Afternoon
          ,Evening
          ,Encounter
        ) %>%
        gather(
          key = "Time of Day"
          ,value = "Encounter ID"
          ,-LB
          ,-UB
          ,-Encounter
        ) %>% 
        filter(
          `Encounter ID` >= LB
          ,`Encounter ID` <= UB
        ) %>% 
        right_join(
          encounter_display
          ,by = c("Time of Day", "Encounter ID")
        ) %>% 
        select(
          `Time of Day`
          ,`Encounter Check`
          ,`Encounter ID`
          ,Encounter
        )
    })
    
    # Weather Check ----------
    weather_gen <- reactive({
      input$region
      input$travel_speed
      input$survival_mod
      input$survival_roll_type
      
      weather_roll <- sample(1:20, size = 2, replace = TRUE)
      precip <- 
        weather_conditions %>% 
        filter(d20 == weather_roll[1]) %>% 
        select(Precipitation) %>% 
        as.character()
      
      wind <- 
        if_else(
          precip != "Tropical Storm"
          ,weather_conditions %>% 
            filter(d20 == weather_roll[2]) %>% 
            select(Wind) %>% 
            as.character()
          ,"Heavy"
        )
      
      tibble(
        `Precipitation Roll` = weather_roll[1]
        ,Precipitation = 
          weather_conditions %>% 
          filter(d20 == weather_roll[1]) %>% 
          select(Precipitation) %>% 
          as.character()
      ) %>% 
        mutate(
          `Wind Roll` = if_else(`Precipitation Roll` == 20, as.integer(20), weather_roll[2])
          ,Wind =
            weather_conditions %>% 
            filter(d20 == `Wind Roll`) %>% 
            select(Wind) %>% 
            as.character()
        )
    })
    
    output$weather <- renderTable({
      weather_gen()
    })
  }
)
