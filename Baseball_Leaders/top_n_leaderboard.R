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
yr <- 1905
n <- 10
stat_input <- quo(HR)

Batting_tib <- 
  Batting %>% 
  as.tibble()



# Provides Career stats for each player up to this year.
batting_Career <- 
  Batting_tib %>% 
  filter(yearID <= yr) %>% 
  group_by(playerID) %>% 
  mutate(Career_HR = cumsum(!!stat_input)) %>% 
  arrange(desc(Career_HR)) %>% 
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
    batting_Career %>% 
      head(n)
    ,aes(
      x = factor(`Full Name`, levels = `Full Name`[order(batting_Career$Career_HR %>% head(n) %>% desc())])
      ,y = Career_HR
    )
  ) +
  geom_col(
    alpha = 0.75
  ) +
  labs(
    title = paste("Top", n, "Players by Career HR's in", yr)
    ,y = "Career HR's"
  )
top_n_bar_chart
  