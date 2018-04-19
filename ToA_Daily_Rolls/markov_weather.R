library(markovchain, warn.conflicts = FALSE, quietly = TRUE)
library(igraph, warn.conflicts = FALSE, quietly = FALSE)
library(ggraph, warn.conflicts = FALSE, quietly = FALSE)
library(tidyverse, warn.conflicts = FALSE, quietly = FALSE)

# set_graph_style()

weather_conditions <-
  tibble(
    Precipitation = 
      c(
        # rep(x = "None", times = 1, each = 1)
        # ,rep(x = "Light", times = 8, each = 1)
        # ,rep(x = "Medium", times = 7, each = 1)
        # ,rep(x = "Heavy", times = 3, each = 1)
        # ,rep(x = "Tropical Storm", times = 1, each = 1)
        "None"
        ,"Light"
        ,"Medium"
        ,"Heavy"
        ,"Tropical Storm"
      )
    ,Wind =
      c(
        # rep(x = "None", times = 12, each = 1)
        # ,rep(x = "Light", times = 5, each = 1)
        # ,rep(x = "Heavy", times = 3, each = 1)
        "None"
        ,"Light"
        ,"Medium"
        ,"Heavy"
        ,"Tropical Storm"
      )
  )

weather_combs <-
  weather_conditions %>% 
  expand.grid() %>% 
  unique() %>% 
  as_tibble() %>% 
  arrange(Precipitation, Wind) %>% 
  # mutate(States = paste(Wind, Precipitation, sep = ", ")) %>% 
  mutate(States = paste(str_sub(Precipitation, 1, 1), str_sub(Wind, 1, 1), sep = ", "))

weather_states <- weather_combs$States
num_states <- length(weather_combs$States)
weather_transition <-
  matrix(
    data =
      c(
      # NN    NL    NM    NH    NT    LN    LL    LM    LH    LT    MN    ML    MM    MH    MT
        0.25, 0.15, 0.10, 0.00, 0.00, 0.15, 0.15, 0.05, 0.00, 0.00, 0.10, 0.05, rep(0, 13),                        # NN
        0.15, 0.20, 0.15, 0.00, 0.00, 0.15, 0.15, 0.15, 0.00, 0.00, 0.00, 0.05, rep(0, 13),                        # NL
        0.00, 0.15, 0.20, 0.15, 0.00, 0.00, 0.15, 0.15, 0.15, 0.00, 0.00, 0.00, 0.05, rep(0, 12),                  # NM
        0.00, 0.00, 0.15, 0.20, 0.00, 0.05, 0.10, 0.15, 0.15, 0.00, 0.05, 0.10, 0.05, rep(0, 12),                  # NH
        rep(0, 25),                                                                                                # NT
        0.15, 0.15, 0.00, 0.00, 0.00, 0.20, 0.15, 0.05, 0.00, 0.00, 0.15, 0.15, 0.00, rep(0, 12),                  # LN
        0.05, 0.15, 0.05, 0.00, 0.00, 0.15, 0.20, 0.15, 0.00, 0.00, 0.05, 0.15, 0.05, rep(0, 12),                  # LL
        0.00, 0.05, 0.15, 0.05, 0.00, 0.00, 0.15, 0.20, 0.15, 0.00, 0.00, 0.05, 0.15, 0.05, rep(0, 11),            # LM
        0.00, 0.00, 0.15, 0.15, 0.00, 0.00, 0.05, 0.15, 0.20, 0.00, 0.00, 0.00, 0.15, 0.15, rep(0, 11),            # LH
        rep(0, 25),                                                                                                # LT
        rep(0, times = 25*15)
      )
    ,byrow = TRUE
    ,nrow = num_states
    ,ncol = num_states
    ,dimnames = list(Yesterday = weather_states, Today = weather_states)
  )
diag(weather_transition)[diag(weather_transition) == 0.00] <- 1
rowSums(weather_transition)[rowSums(weather_transition) < 1]
weather_transition

weather_mc <- 
  new(
    Class = "markovchain"
    ,states = weather_states
    ,byrow = TRUE
    ,transitionMatrix = weather_transition
  )
steadyStates(weather_mc)
plot(weather_mc)

weather_graph <- 
  weather_transition %>% 
  as.data.frame() %>% 
  rownames_to_column("From") %>% 
  as_tibble() %>% 
  gather(
    key = To
    ,value = Probability
    ,-From
  ) %>% 
  # filter(Probability != 0) %>% 
  graph_from_data_frame()

weather_graph %>% 
  ggraph(layout = "kk") + 
  geom_edge_link(aes(colour = Probability)) + 
  geom_edge_loop(aes(colour = Probability)) +
  geom_node_point() +
  scale_edge_color_distiller(
    type = "seq"
    ,palette = "OrRd"
    ,direction = 1
  )
