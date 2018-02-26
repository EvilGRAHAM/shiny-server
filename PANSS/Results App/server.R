# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# Data Import ----------
panss_data <- read_csv(file = "../data/Panssdata_test.csv")


#Server ----------
shinyServer(
  function(input, output) {

  }
)
