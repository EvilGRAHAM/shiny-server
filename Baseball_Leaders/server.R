# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(Lahman, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)

# ggplot Formatting ----------
# Updates theme_minimal so that there is borders around the graphs and the facet headings.
theme_minimal2 <- theme_minimal() %>%  theme_set()
theme_minimal2 <-
  theme_update(
    panel.border = element_rect(
      linetype = "solid"
      ,colour = "grey92"
      ,fill = NA
    )
    ,strip.background = element_rect(
      linetype = "solid"
      ,colour = "grey92"
      ,fill = NA
    )
  )

# Loads Batting dataset as tibble.
Batting_tib <- 
  Batting %>% 
  as.tibble()

#Server ----------
shinyServer(
  function(input, output) {
    
    # Dataset ----------
    batting_Career <- reactive({
      # Adds a progress bar to show the calculations are running.
      withProgress(
        message = "Calculating..."
        ,value = NULL
        ,Batting_tib %>% 
          filter(
            yearID >= input$year_lkup[[1]]
            ,yearID <= input$year_lkup[[2]]
          ) %>% 
          group_by(playerID) %>% 
          mutate(
            Career_Stat = cumsum(!!quo(eval(parse(text = input$baseball_stat))))
          ) %>% 
          arrange(desc(Career_Stat)) %>% 
          # Removes cases where a player appears multiple times in the list due to being a leader for multiple years.
          distinct(
            playerID
            ,.keep_all = TRUE
          )
      )
    })
    
    # Leaderboard Table ----------
    output$leaderboardTable <- renderTable({
      
      batting_Career() %>%
        # Grabs the top n values.
        head(input$n) %>% 
        mutate(
          # In case more than one name is returned, we choose the first one, as it should be the exact match.
          `Full Name` = paste(playerInfo(playerID)[1, ]$nameFirst, playerInfo(playerID)[1, ]$nameLast)
        ) %>% 
        ungroup() %>%
        select(
          -c(
            playerID
            ,stint
            ,teamID
            ,lgID
          )
        ) %>% 
        select(
          `Full Name`
          ,Career_Stat
          ,everything()
        ) %>% 
        rename(Year = yearID)
      
    })
    
    # Leaderboard Plot ----------
    output$leaderboardPlot <- renderPlot({
      
      ggplot(
        batting_Career() %>% 
          head(input$n) %>% 
          mutate(
            # In case more than one name is returned, we choose the first one, as it should be the exact match.
            `Full Name` = paste(playerInfo(playerID)[1, ]$nameFirst, playerInfo(playerID)[1, ]$nameLast)
          )
        ,aes(
          x = factor(`Full Name`, levels = `Full Name`[order(batting_Career()$Career_Stat %>% head(input$n) %>% desc())])
          ,y = Career_Stat
        )
      ) +
        geom_col(
          alpha = 0.75
        ) +
        labs(
          title = paste0("Top ", input$n, " Players by Career ", input$baseball_stat, "'s from ", input$year_lkup[[1]], "-", input$year_lkup[[2]])
          ,x = "Player"
          ,y = paste0("Career ", input$baseball_stat, "'s")
        ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1)
        )
      
    })
    
  })
