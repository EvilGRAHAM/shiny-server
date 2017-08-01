rm(list=ls())
library(tidyverse)
library(Lahman)
library(lubridate)

# Updates them_minimal so that there is borders around the graphs and the facet headings.
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

# Year to go up to
yr <- 2017
n <- 10
stat_input <- quo(G)

Batting_tib <- 
  Batting %>% 
  as.tibble()



# Provides cumulative stats for each player up to this year.
batting_cum <- 
  Batting_tib %>% 
  filter(yearID <= yr) %>% 
  group_by(playerID) %>% 
  mutate(Cum_HR = cumsum(!!stat_input)) %>% 
  arrange(desc(Cum_HR)) %>% 
  distinct(
    playerID
    ,.keep_all = TRUE
  ) %>%
  head(n) %>%
  mutate(
    `First Name` = playerInfo(playerID)[1, ]$nameFirst
    ,`Last Name` = playerInfo(playerID)[1, ]$nameLast
  ) %>%
  mutate(
    `Full Name` = paste0(`Last Name`, ", ", `First Name`)
  ) %>% 
  ungroup()


top_n_bar_chart <- 
  ggplot(
    batting_cum %>% 
      head(n)
    ,aes(
      x = factor(`Full Name`, levels = `Full Name`[order(batting_cum$Cum_HR %>% head(n) %>% desc())])
      ,y = Cum_HR
    )
  ) +
  geom_col(
    alpha = 0.75
  ) +
  labs(
    title = paste("Top", n, "Players by Cumulative HR's in", yr)
    ,y = "Cumulative HR's"
  )
top_n_bar_chart
  