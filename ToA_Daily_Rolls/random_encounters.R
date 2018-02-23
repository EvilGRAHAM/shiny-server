random_encounter_tbl <- read_csv("data/random_encounters.csv")

random_encounter_tbl <-
  random_encounter_tbl %>% 
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

random_encounter_tbl %>% 
  right_join(
    tibble(Region = "Beach", x1 = 1, x2 = 19, x3 = 93)
    ,by = "Region"
  ) %>% 
  mutate(X1 = if_else(x1 >= LB & x1 <= UB, T, F))
    

random_encounter_tbl %>%
  mutate(x = between(c(1, 19, 24, 72), LB, UB))
  filter(
    Region == "Wasteland"
    ,between(c(1, 19, 24, 72), LB, UB)
    # ,LB %in% tibble(x = c(1, 19, 24, 72))$x
    # ,UB %in% tibble(x = c(1, 19, 24, 72))$x
  )
