# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# Variables ----------
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

cache_table <- read_csv("data/cache_table.csv")
treasure_table <- read_csv("data/treasure_table.csv")
dead_explorer_table <- read_csv("data/dead_explorer_table.csv")
explorer_table <- read_csv("data/explorer_table.csv")

regions <- 
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
        rep(x = 10, times = 2, each = 1)
        ,rep(x = 15, times = 8, each = 1)
      )
    ,`Walking Hexes` = rep(x = 1, times = 10)
    ,`Canoeing Hexes` =
      c(
        rep(x = 1, times = 1, each = 1)
        ,rep(x = 2, times = 1, each = 1)
        ,rep(x = 1, times = 4, each = 1)
        ,rep(x = 2, times = 1, each = 1)
        ,rep(x = 1, times = 3, each = 1)
      )
    ,`Random Encounters` =
      c(
        rep(x = 16, times = 3, each = 1)
        ,rep(x = 15, times = 1, each = 1)
        ,rep(x = 14, times = 1, each = 1)
        ,rep(x = 16, times = 5, each = 1)
      )
    ,`Foraging DC` =
      c(
        rep(x = 10, times = 4, each = 1)
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


# Server ----------
shinyServer(
  function(input, output) {
    # Days Counter ----------
    num_days <- reactiveValues(count = 0) # Defining & initializing the reactiveValues object
    
    observeEvent(
      input$new_day
      ,num_days$count <- num_days$count + 1
    )
    
    output$num_days <- renderText(num_days$count)
    
    # Regions Table ----------
    output$regions_tbl <- renderTable(
      region_checks %>% 
        filter(Region == input$region)
    )

    # Pace Table ----------
    output$pace_tbl <- renderTable(
      pace_modifiers %>% 
        filter(Pace == paces[input$travel_speed])
    )
    
    # Navigation Check ----------
    output$navigation_check <- renderPrint({
      nav_region <- input$region
      nav_travel_speed <- input$travel_speed
      nav_survival_mod <- input$survival_mod
      nav_roll_type <- if_else(weather_gen()[1, 1] == 20, "Disadvantage", input$survival_roll_type)
      input$new_day
      
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
          "Day"
          ,max(input$day, num_days$count, na.rm = TRUE)
          ,"\nNav Check Result:"
          ,nav_rolls
          ,nav_survival_sign
          ,abs(nav_survival_mod)
          ,nav_pace_sign
          ,abs(nav_pace_mod)
          ,"="
          ,nav_check
          ,inequality
          ,nav_dc
          ,"\nGetting Lost Random Direction:"
          ,sample(x = 1:6, size = 1 + nav_hex_mod, replace = TRUE)
          ,"\nExtra Hex Traveled:"
          ,nav_hex_mod
          ,"Hex"
        )
    })
    
    # Random Encounter ----------
    random_encounter_gen <- reactive({
      encounter_region <- input$region
      input$travel_speed
      input$survival_mod
      input$survival_roll_type
      input$new_day
      
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
              ,as.integer(NA)
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
    
    # Cache Table ----------
    cache_gen <- reactive({
      cache_check <- 
        random_encounter_gen() %>%
        select(
          `Time of Day`
          ,Encounter
        ) %>% 
        mutate(
          roll_cache = 
            if_else(
              Encounter %in% 
                c(
                  "Cache"
                  ,"Girallons"
                  ,"Grungs"
                )
              ,TRUE
              ,FALSE
            ) 
        ) %>% 
        filter(roll_cache)
      
      cache_check %>% 
        mutate(
          `Cache ID` = 
            sample(
              x = 1:20
              ,size = 
                cache_check %>% 
                count() %>% 
                as.numeric()
              ,replace = TRUE
            )
        ) %>% 
        left_join(
          cache_table
          ,by = c("Cache ID" = "d20")
        ) %>% 
        select(
          `Time of Day`
          ,`Cache ID`
          ,Cache
        )
    })
    
    # Treasure Table ----------
    treasure_gen <- reactive({
      treasure_check <- 
        random_encounter_gen() %>%
        select(
          `Time of Day`
          ,Encounter
        ) %>% 
        mutate(
          roll_treasure = 
            if_else(
              Encounter %in% 
                c(
                  "Cyclops"
                  ,"Eblis"
                  ,"Explorer, dead"
                  ,"Girallons"
                  ,"Sea hags"
                  ,"Statue of Ubtao"
                  ,"Su-monsters"
                  ,"Wereboar"
                  ,"Zhentarim"
                )
              ,TRUE
              ,FALSE
            ) 
        ) %>% 
        filter(roll_treasure)
      
      treasure_check %>% 
        mutate(
          `Treasure ID` = 
            sample(
              x = 1:100
              ,size = 
                treasure_check %>% 
                count() %>% 
                as.numeric()
              ,replace = TRUE
            )
        ) %>% 
        left_join(
          treasure_table
          ,by = c("Treasure ID" = "d100")
        ) %>% 
        select(
          `Time of Day`
          ,`Treasure ID`
          ,Treasure
        )
    })
    
    # Dead Explorer Table ----------
    dead_explorer_gen <- reactive({
      dead_explorer_check <- 
        random_encounter_gen() %>%
        select(
          `Time of Day`
          ,Encounter
        ) %>% 
        mutate(roll_dead_explorer = if_else(Encounter == "Explorer, dead", TRUE, FALSE)) %>% 
        filter(roll_dead_explorer)
      
      dead_explorer_check %>% 
        mutate(
          `Remains ID` = 
            sample(
              x = 1:20
              ,size = 
                dead_explorer_check %>% 
                count() %>% 
                as.numeric()
              ,replace = TRUE
            )
        ) %>% 
        left_join(
          dead_explorer_table
          ,by = c("Remains ID" = "d20")
        ) %>% 
        select(
          `Time of Day`
          ,`Remains ID`
          ,Remains
        )
    })
    
    # Explorers Table ----------
    explorer_gen <- reactive({
      explorer_check <- 
        random_encounter_gen() %>%
        select(
          `Time of Day`
          ,Encounter
        ) %>% 
        mutate(roll_explorer = if_else(Encounter == "Explorers", TRUE, FALSE)) %>% 
        filter(roll_explorer)
      
      explorer_check %>% 
        mutate(
          `Explorers ID` = 
            sample(
              x = 1:6
              ,size = 
                explorer_check %>% 
                count() %>% 
                as.numeric()
              ,replace = TRUE
            )
        ) %>% 
        left_join(
          explorer_table
          ,by = c("Explorers ID" = "d6")
        ) %>% 
        select(
          `Time of Day`
          ,`Explorers ID`
          ,Explorers
        )
    })
    
    # Events Output ----------
    events_table_gen <- reactive({
      random_encounter_gen() %>% 
        left_join(
          cache_gen()
          ,by = "Time of Day"
        ) %>% 
        left_join(
          treasure_gen()
          ,by = "Time of Day"
        ) %>% 
        left_join(
          explorer_gen()
          ,by = "Time of Day"
        ) %>% 
        left_join(
          dead_explorer_gen()
          ,by = "Time of Day"
        ) %>% 
        select_if(.predicate = funs(prod(is.na(.)) == 0))
    })
    
    output$events_table <- renderTable(events_table_gen())
    
    # Weather Check ----------
    weather_gen <- reactive({
      input$region
      input$travel_speed
      input$survival_mod
      input$survival_roll_type
      input$new_day
      
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
    
    output$weather <- renderTable(weather_gen())
    
  }
)
