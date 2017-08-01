# Libraries ----------
library(shiny)
library(shinythemes)
library(tidyverse)
library(Lahman)
library(lubridate)

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
          filter(yearID <= input$year_lkup) %>% 
          group_by(playerID) %>% 
          mutate(
            Career_Stat = cumsum(!!quo(eval(parse(text = input$baseball_stat))))
          ) %>% 
          arrange(desc(Career_Stat)) %>% 
          # Removes cases where a player appears multiple times in the list due to being a leader for multiple years.
          distinct(
            playerID
            ,.keep_all = TRUE
          ) %>% 
          head(100) %>% 
          mutate(
            # Incase more than one name is returned, we choose the first one, as it should be the exact match.
            `Full Name` = paste0(playerInfo(playerID)[1, ]$nameLast, ", ", playerInfo(playerID)[1, ]$nameFirst)
          )
      )
    })
    
    # Leaderboard Table ----------
    output$leaderboardTable <- renderTable({
      
      batting_Career() %>%
        # Grabs the top n values.
        head(input$n) %>% 
        ungroup() %>% 
        select(
          -c(
            playerID
            ,stint
            ,teamID
            ,lgID
          )
        )
      
    })
    
    # Leaderboard Plot ----------
    output$leaderboardPlot <- renderPlot({
      
      ggplot(
        batting_Career() %>% 
          head(input$n)
        ,aes(
          x = factor(`Full Name`, levels = `Full Name`[order(batting_Career()$Career_Stat %>% head(input$n) %>% desc())])
          ,y = Career_Stat
        )
      ) +
        geom_col(
          alpha = 0.75
        ) +
        labs(
          title = paste0("Top ", input$n, " Players by Career ", input$baseball_stat, "'s in ", input$year_lkup)
          ,x = "Player"
          ,y = paste0("Career ", input$baseball_stat, "'s")
        )
      
    })
    
  })
