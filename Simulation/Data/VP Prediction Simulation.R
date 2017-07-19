rm(list=ls())
try(dev.off(), silent = TRUE)
# Libraries -----------------------------
library(glmnet, warn.conflicts = FALSE)
library(boot, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(ggfortify, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

# Variables ----------------------------------
Mth <- c(
  `1` = "Jan"
  ,`2` = "Feb"
  ,`3` = "Mar"
  ,`4` = "Apr"
  ,`5` = "May"
  ,`6` = "Jun"
  ,`7` = "Jul"
  ,`8` = "Aug"
  ,`9` = "Sep"
  ,`10` = "Oct"
  ,`11` = "Nov"
  ,`12` = "Dec"
  ,`13` = "Ann"
)

Mth_long <- c(
  `1` = "January"
  ,`2` = "February"
  ,`3` = "March"
  ,`4` = "April"
  ,`5` = "May"
  ,`6` = "June"
  ,`7` = "July"
  ,`8` = "August"
  ,`9` = "September"
  ,`10` = "October"
  ,`11` = "November"
  ,`12` = "December"
  ,`13` = "Annual"
)

Mth_comb <- cbind(Mth, Mth_long)

Crude_Breakdown_Cleaned <- c(
  `Unknown` = "Unknown"
  ,`C5` = "C5+"
  ,`Light_LSB` = "Light LSB"
  ,`Light_SW` = "Light SW"
  ,`Medium_LSB` = "Medium LSB"
  ,`Medium_SW` = "Medium SW"
  ,`Midale` = "Midale"
  ,`Heavy_Sour` = "Heavy Sour"
  ,`Heavy_SW` = "Heavy SW"
)


excel_path <- "C:/Users/scott.graham/OneDrive - Tundra Energy Marketing Limited/Documents/GitHub/shiny-server/Simulation/Data/VP Simulation.xlsx"
excel_sheet_complete <- "VP Data Complete"
excel_sheet_reduced <- "VP Data"
excel_sheet_weather <- "Weather Data"
# Reads data from the testing file to input into the simulation.
excel_path_input <- "C:/Users/scott.graham/OneDrive - Tundra Energy Marketing Limited/Documents/Marketing/VP/Post Split/Simulation Testing.xlsx"
excel_sheet_input <- "LSB + BAKKEN"

# This is a list of all the variables we wish to include in the LASSO.
var.pred <- c(
  "Sulf"
  ,"Dens"
  # ,"mth"
  ,"Temp.Mean"
  ,"Temp.Roll"
  ,"Estevan"
  ,"Weyburn"
  ,"Oxbow"
  ,"Yellow Grass North"
  ,"Melita"
  ,"Kipling"
  # ,"VP_95_Spec"
  ,"C5"
  ,"Light_LSB"
  ,"Light_SW"
  ,"Medium_LSB"
  ,"Medium_SW"
  ,"Midale"
  ,"Heavy_Sour"
  ,"Heavy_SW"
  ,"Jan"
  ,"Feb"
  ,"Mar"
  ,"Apr"
  ,"May"
  ,"Jun"
  ,"Jul"
  ,"Aug"
  ,"Sep"
  ,"Oct"
  ,"Nov"
  ,"Dec"
  ,"Post_Aquisition"
  ,"Sulf.C5"
  ,"Sulf.L_LSB"
  ,"Sulf.L_SW"
  ,"Sulf.M_LSB"
  ,"Sulf.M_SW"
  ,"Sulf.Midale"
  ,"Sulf.H_Sour"
  ,"Sulf.H_SW"
  ,"Dens.C5"
  ,"Dens.L_LSB"
  ,"Dens.L_SW"
  ,"Dens.M_LSB"
  ,"Dens.M_SW"
  ,"Dens.Midale"
  ,"Dens.H_Sour"
  ,"Dens.H_SW"
  ,"Temp.Roll.C5"
  ,"Temp.Roll.L_LSB"
  ,"Temp.Roll.L_SW"
  ,"Temp.Roll.M_LSB"
  ,"Temp.Roll.M_SW"
  ,"Temp.Roll.Midale"
  ,"Temp.Roll.H_Sour"
  ,"Temp.Roll.H_SW"
  ,"Post_Aquisition.C5"
  ,"Post_Aquisition.L_LSB"
  ,"Post_Aquisition.L_SW"
  ,"Post_Aquisition.M_LSB"
  ,"Post_Aquisition.M_SW"
  ,"Post_Aquisition.Midale"
  ,"Post_Aquisition.H_Sour"
  ,"Post_Aquisition.H_SW"
)

lasso_var_names <- c(
  "Sulfur"
  ,"Density"
  # ,"Month"
  ,"Mean Temperature"
  ,"7 Day Rolling Temperature"
  ,"Estevan"
  ,"Weyburn"
  ,"Oxbow"
  ,"Yellow Grass"
  ,"Melita"
  ,"Kipling"
  # ,"VP 95 Spec"
  ,"C5+"
  ,"Light LSB"
  ,"Light SW"
  ,"Medium LSB"
  ,"Medium SW"
  ,"Midale"
  ,"Heavy Sour"
  ,"Heavy SW"
  ,"Jan"
  ,"Feb"
  ,"Mar"
  ,"Apr"
  ,"May"
  ,"Jun"
  ,"Jul"
  ,"Aug"
  ,"Sep"
  ,"Oct"
  ,"Nov"
  ,"Dec"
  ,"Post Aquisition"
  ,"Sulfur:C5+"
  ,"Sulfur:Light LSB"
  ,"Sulfur:Light SW"
  ,"Sulfur:Medium LSB"
  ,"Sulfur:Medium SW"
  ,"Sulfur:Midale"
  ,"Sulfur:Heavy Sour"
  ,"Sulfur:Heavy SW"
  ,"Density:C5+"
  ,"Density:Light LSB"
  ,"Density:Light SW"
  ,"Density:Medium LSB"
  ,"Density:Medium SW"
  ,"Density:Midale"
  ,"Density:Heavy Sour"
  ,"Density:Heavy SW"
  ,"Temp.Roll:C5+"
  ,"Temp.Roll:Light LSB"
  ,"Temp.Roll:Light SW"
  ,"Temp.Roll:Medium LSB"
  ,"Temp.Roll:Medium SW"
  ,"Temp.Roll:Midale"
  ,"Temp.Roll:Heavy Sour"
  ,"Temp.Roll:Heavy SW"
  ,"Post_Aquisition:C5+"
  ,"Post_Aquisition:Light LSB"
  ,"Post_Aquisition:Light SW"
  ,"Post_Aquisition:Medium LSB"
  ,"Post_Aquisition:Medium SW"
  ,"Post_Aquisition:Midale"
  ,"Post_Aquisition:Heavy Sour"
  ,"Post_Aquisition:Heavy SW"
)


# How many times we multiply the SD by to get our input rows.
multiplier <- 2


# Functions ----------------------------
data.conversions <- function(data_file) {
  
  # Converts SDate to date format, and Producer + Battery to factors
  # Grabs the year and the month for each entry.
  # Arranges the entries chronologically
  data_file <- 
    data_file %>% 
    mutate(
      SDate = as.Date(SDate, format = "%m/%d/%Y")
    ) %>% 
    mutate(
      Producer = as.factor(Producer)
      ,Battery = as.factor(Battery)
      ,Battery.1 = as.factor(Battery.1)
      ,CrudeType = as.factor(CrudeType)
      ,Crude_Num = as.factor(Crude_Num)
      ,Crude_Breakdown = as.factor(Crude_Breakdown)
      ,mth = 
        SDate %>% 
        month() %>% 
        as.integer()
      ,yr = 
        SDate %>% 
        year() %>% 
        as.integer()
      ,day =
        SDate %>% 
        day() %>% 
        as.integer()
    ) %>% 
    arrange(SDate) %>% 
    filter(
      !is.na(VP)
      ,!is.na(Sulf)
      ,!is.na(Dens)
    )
  
  # Re-orders the factors so unknown comes first.
  data_file$Crude_Breakdown <- factor(data_file$Crude_Breakdown, levels(data_file$Crude_Breakdown)[c(7, 1:6)])
  data_file
  
}


read_input <- function(var_name) {
  n <- readline(prompt = paste0("Enter a ", var_name, ": "))
}


# Grabs the users input for the simulation parameters.
get_input <- function() {
  Density_input <- NA
  Sulfur_input <- NA
  Temp.Roll_input <- NA
  mth_input <- NA
  while (is.na(Density_input)) {
    Density_input <- read_input("Density (kg/m3)") %>% as.numeric() %>% try(silent = TRUE)
  }
  while (is.na(Sulfur_input)) {
    Sulfur_input <- read_input("Sulfur (%wt)") %>% as.numeric() %>% try(silent = TRUE)
  }
  while (is.na(Temp.Roll_input)) {
    Temp.Roll_input <- read_input("7 Day Temperature Average for SE Sask (?C)") %>% as.numeric() %>% try(silent = TRUE)
  }
  while (is.na(mth_input)) {
    mth_input <- read_input("number of the month you wish to simulate. i.e. 1 = Jan, 2 = Feb, ...") %>% as.numeric() %>% try(silent = TRUE)
  }
  return(list(Density_input, Sulfur_input, Temp.Roll_input, mth_input))
}

crude_type_check_old <- function(density, sulfur){
  # Any input with a density <= 770 we call C5 regardless of sulfur
  # If Sulfur is <= 0.5 it is deemed Sweet vs. a Sour w/ sulfur > 0.5
  # Lights have a density <= 825, Mediums have a density (825, 900] for SW, (825, 870] for Sours
  # A Midale is a sour w/ density in (870, 900]
  # Heavy's have a density > 900
  if(density <= 770) {
    "C5"
  } else if(sulfur <= 0.5) {
    if(density <= 825) {
      "Light_SW"
    } else if(density > 825 & density <= 900) {
      "Medium_SW"
    } else {
      "Heavy_SW"
    }
  } else {
    if(density <= 825) {
      "Light_LSB"
    } else if(density > 825 & density <= 870) {
      "Medium_LSB"
    } else if(density > 870 & density <= 900) {
      "Midale"
    } else {
      "Heavy_Sour"
    }
  }
}

crude_type_check <- function(density, sulfur){
  # Any input with a density <= 770 we call C5 regardless of sulfur
  # If Sulfur is <= 0.5 it is deemed Sweet vs. a Sour w/ sulfur > 0.5
  # Lights have a density <= 825, Mediums have a density (825, 900]
  # A Midale is a sour w/ density in (865, 900] and sulfur > 2
  # Heavy's have a density > 900
  if(density <= 770) {
    "C5"
  } else if(sulfur <= 0.5) {
    if(density <= 825) {
      "Light_SW"
    } else if(density > 825 & density <= 900) {
      "Medium_SW"
    } else {
      "Heavy_SW"
    }
  } else {
    if(density <= 825) {
      "Light_LSB"
    } else if(density > 865 & density <= 900 & sulfur >= 2) {
      "Midale"
    } else if(density > 825 & density <= 900) {
      "Medium_LSB"
    } else {
      "Heavy_Sour"
    }
  }
}

# Data Import -----------------------------------

# Reads the data in from a xlsx, and converts it to a tibble.
data_complete <- 
  excel_path %>%
  read_excel(
    sheet = excel_sheet_complete
  )

data_reduced <- 
  excel_path %>%
  read_excel(
    sheet = excel_sheet_reduced
  )

data_weather <- 
  excel_path %>%
  read_excel(
    sheet = excel_sheet_weather
  )

data_test_input <- 
  excel_path_input %>%
  read_excel(sheet = excel_sheet_input)





# Setup -------------------------------------------------------------------

set.seed(8)

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

  
data_complete <- data_complete %>% data.conversions()
data_reduced <- data_reduced %>% data.conversions()

data_weather <- 
  data_weather %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Adding interaction effects to data_complete.
# As well it splits the mth variable into 12 dummy variables.
data_complete <- 
  data_complete %>% 
  mutate(
    Jan = if_else(mth == 1, 1, 0)
    ,Feb = if_else(mth == 2, 1, 0)
    ,Mar = if_else(mth == 3, 1, 0)
    ,Apr = if_else(mth == 4, 1, 0)
    ,May = if_else(mth == 5, 1, 0)
    ,Jun = if_else(mth == 6, 1, 0)
    ,Jul = if_else(mth == 7, 1, 0)
    ,Aug = if_else(mth == 8, 1, 0)
    ,Sep = if_else(mth == 9, 1, 0)
    ,Oct = if_else(mth == 10, 1, 0)
    ,Nov = if_else(mth == 11, 1, 0)
    ,Dec = if_else(mth == 12, 1, 0)
    ,Sulf.C5 = Sulf * C5
    ,Sulf.L_LSB = Sulf * Light_LSB
    ,Sulf.L_SW = Sulf * Light_SW
    ,Sulf.M_LSB = Sulf * Medium_LSB
    ,Sulf.M_SW = Sulf * Medium_SW
    ,Sulf.Midale = Sulf * Midale
    ,Sulf.H_Sour = Sulf * Heavy_Sour
    ,Sulf.H_SW = Sulf * Heavy_SW
    ,Dens.C5 = Dens * C5
    ,Dens.L_LSB = Dens * Light_LSB
    ,Dens.L_SW = Dens * Light_SW
    ,Dens.M_LSB = Dens * Medium_LSB
    ,Dens.M_SW = Dens * Medium_SW
    ,Dens.Midale = Dens * Midale
    ,Dens.H_Sour = Dens * Heavy_Sour
    ,Dens.H_SW = Dens * Heavy_SW
    ,Temp.Roll.C5 = Temp.Roll * C5
    ,Temp.Roll.L_LSB = Temp.Roll * Light_LSB
    ,Temp.Roll.L_SW = Temp.Roll * Light_SW
    ,Temp.Roll.M_LSB = Temp.Roll * Medium_LSB
    ,Temp.Roll.M_SW = Temp.Roll * Medium_SW
    ,Temp.Roll.Midale = Temp.Roll * Midale
    ,Temp.Roll.H_Sour = Temp.Roll * Heavy_Sour
    ,Temp.Roll.H_SW = Temp.Roll * Heavy_SW
    ,Post_Aquisition.C5 = Post_Aquisition * C5
    ,Post_Aquisition.L_LSB = Post_Aquisition * Light_LSB
    ,Post_Aquisition.L_SW = Post_Aquisition * Light_SW
    ,Post_Aquisition.M_LSB = Post_Aquisition * Medium_LSB
    ,Post_Aquisition.M_SW = Post_Aquisition * Medium_SW
    ,Post_Aquisition.Midale = Post_Aquisition * Midale
    ,Post_Aquisition.H_Sour = Post_Aquisition * Heavy_Sour
    ,Post_Aquisition.H_SW = Post_Aquisition * Heavy_SW
  )

# LASSO -------------------------------------------------------------------


 
# We generate a matrix from the complete data set using the list of variables defined above.
# Generates a Matrix containing the predictor variables.
lasso.pred <- 
  data_complete %>% 
  select(var.pred) %>% 
  as.matrix()
# Generates a matrix with the response variable only.
lasso.result <-
  data_complete %>% 
  select(VP) %>% 
  as.matrix()
# Names the columns in the two matrices so they read nicer.
colnames(lasso.pred) <- lasso_var_names
colnames(lasso.result) <- "VP"

# 0 => keep in model, 1 => perform normally
# Keeps all dummy variables
# We aren't using the Midale + Heavy vars as this way when you fit the regression model,
# the default (all dummy variables  = 0) is a midale. As well nothing that could be
# considered a heavy has entered the system.
cvfit_penalty_dummy <- 
  cv.glmnet(
    x = lasso.pred#[, 1:16]
    ,y = lasso.result
    ,alpha = 1
    # ,penalty.factor =
    #   c(
    #     rep(
    #       1
    #       ,times = 11 # VP_95_Spec
    #     )
    #     ,rep(
    #       0
    #       ,times = 7 # Heavy SW
    #     )
    #     ,rep(
    #       1
    #       ,times = 37 # Medium SW
    #     )
    #   )
  )
# LASSO + C5 + Split + Dummy + Keep - Midale + Spec
autoplot(cvfit_penalty_dummy) + labs(title = "LASSO + C5 + Split + Dummy + Keep - Midale + Spec")
coef(cvfit_penalty_dummy, s = "lambda.1se", exact = TRUE)

# Residual vs. Fitted -------------------------------------------------------------------
# This is just a copy of the matrix used to generate the LASSo.
pred_matrix <- lasso.pred
# Remove all the column names so we can make predictions against it.
colnames(pred_matrix) <- NULL
# Creates a tibble containing the Crude Type, Fitted VP, and Actual VP.
fitted_actual_complete <-
  tibble(
  Crude_Breakdown = data_complete$Crude_Breakdown
  ,Fitted = 
    predict(
      cvfit_penalty_dummy
      ,newx = pred_matrix
      ,s = "lambda.1se"
    ) %>% 
    as.vector()
  ,Actual = data_complete$VP
  )

# Adds the residuals coloumn to the tibble w/ all the predictions.
fitted_actual_complete <-
  fitted_actual_complete %>%
  mutate(Residuals = Actual - Fitted)

# Plots the residuals against the fitted values, coloured by crude type.
a_base <-
  ggplot(
    data = fitted_actual_complete
    ,aes(
      x = Fitted
      ,y = Residuals
    )
  ) +
  scale_colour_brewer(
    type = "qual"
    ,name = "Crude Type:"
    ,palette = "Set2"
    ,labels = Crude_Breakdown_Cleaned
  )


a_fit_resid <-
  a_base +
  geom_hline(
    aes(
      yintercept = 0
    )
  ) +
  geom_smooth(
    colour = "black"
  ) +
  geom_point(
    aes(
      colour = Crude_Breakdown
    )
  ) +
  labs(
    title = "Residuals vs. Fitted Plot"
    ,x = "Fitted VP (kPa)"
    ,y = "Residual VP (kPa)"
  )

a_fit_resid_marginal <- 
  a_fit_resid +
  facet_wrap(
    ~ Crude_Breakdown
    ,scales = "free"
    ,labeller = labeller(Crude_Breakdown = Crude_Breakdown_Cleaned)
  )
a_fit_resid
a_fit_resid_marginal

# # Input ---------------------------------------------

# user_input <- get_input()

# Simulation Setup ---------------------------------------------

# Figures out how many predictions to make.
num_tests <- 
  data_test_input %>% 
  count() %>% 
  as.numeric()

# Grabs each of the input variables, and saves them to their own variable.
Density_input <-
  data_test_input %>%
  select(Density)
Sulfur_input <-
  data_test_input %>%
  select(Sulfur)
Temp.Roll_input <-
  data_test_input %>%
  select(Temp.Roll)
mth_input <-
  data_test_input %>%
  select(Num_Month)
# Density_input <- user_input[[1]]
# Sulfur_input <- user_input[[2]]
# Temp.Roll_input <- user_input[[3]]
# mth_input <- user_input[[4]]

# Creates an empty tibble to be filled with the results of the predictions.
fitted_actual_input <- tibble(
  Num_Month = as.numeric(character())
  ,Month = as.factor(character())
  ,Crude_Breakdown = character()
  ,Fitted = as.numeric(character())
)


# Simulation Loop -----------------------------------

for (i in 1:num_tests) {
set.seed(4815609)
# Determines the crude type based on the inputted data.
Crude_Breakdown_input <- crude_type_check(Density_input[i, ], Sulfur_input[i, ])
# This finds the standard deviation for the crude type inputed.
crude_bd_stats <- 
  data_complete %>% 
  filter(Crude_Breakdown == Crude_Breakdown_input) %>% 
  summarize(
    Density_Mean = mean(Dens)
    ,Density_SD = sd(Dens)
    ,Sulfur_Mean = mean(Sulf)
    ,Sulfur_SD = sd(Sulf)
  )
crude_bd_stats

# As well as the mean and sd for Temp.Roll for the month inputted
weather_stats <- 
  data_weather %>% 
  filter(mth == mth_input[i, ] %>% as.numeric()) %>% 
  summarize(
    Temp.Roll_Mean = mean(Temp.Roll)
    ,Temp.Roll_SD = sd(Temp.Roll)
  )
# If we have an annual "month", we pull the total standard deviation.
if(i == 13) {
  weather_stats <- 
    data_weather %>% 
    summarize(
      Temp.Roll_Mean = mean(Temp.Roll)
      ,Temp.Roll_SD = sd(Temp.Roll)
    )
}
weather_stats

# Grabs all temperature entries whose entries are within +- multiplier*sd
data_weather_input <- 
  data_weather %>% 
  select(
    -c(
      Date
      ,SD
      ,day
      ,mth
      ,yr
    )
  ) %>% 
  filter(
    Temp.Roll >= Temp.Roll_input[i, ] %>% as.numeric() - multiplier * (
                                                        weather_stats %>% 
                                                        select(Temp.Roll_SD) %>% 
                                                        as.numeric()
                                                      ) &
    Temp.Roll <= Temp.Roll_input[i, ] %>% as.numeric() + multiplier * (
                                                        weather_stats %>% 
                                                        select(Temp.Roll_SD) %>% 
                                                        as.numeric()
                                                      ) %>% as.numeric()
  )

# Filters the data that will be inputed into the simulation s.t. the crude type matches,
# and the density and sulfur fall within a range of values.
# As well we remove any of the temperature data, as we will randomly insert them
# from a different weather table for the simulation.
data_input <- 
  data_complete %>% 
  select(
    -c(
        Temp.Mean
        ,Temp.Roll
        ,Estevan
        ,Weyburn
        ,Oxbow
        ,`Yellow Grass North`
        ,Melita
        ,Kipling
    )
  ) %>%
  filter(
    # You can uncomment the first line if you want to force the simulated data set to only
    # contain crude of the same type as the inputted.
    # As well if you uncomment the second line, only those entries of the same month as 
    # the inputted are used.
    # Crude_Breakdown == Crude_Breakdown_input &
    # mth == as.numeric(mth_input[i, ]) &
    Dens >= Density_input[i, ] %>% as.numeric() - multiplier * (
                                                crude_bd_stats %>% 
                                                select(Density_SD) %>% 
                                                as.numeric()
                                              ) &
    Dens <= Density_input[i, ] %>% as.numeric() + multiplier * (
                                                crude_bd_stats %>% 
                                                select(Density_SD) %>% 
                                                as.numeric()
                                              ) &
    Sulf >= Sulfur_input[i, ] %>% as.numeric() - multiplier * (
                                                crude_bd_stats %>% 
                                                select(Sulfur_SD) %>% 
                                                as.numeric()
                                              ) &
    Sulf <= Sulfur_input[i, ] %>% as.numeric() + multiplier * (
                                                crude_bd_stats %>% 
                                                select(Sulfur_SD) %>% 
                                                as.numeric()
                                              )
  )
  
# Instead of using the given temperature value from the data,
# we instead bootstrap in values from our filtered list of weather data. 
# There isn't that strong of a relationship between temperature and crude qualities,
# so we just randomly insert them into the prediction matrix
pred_matrix_input <-
  data_input %>%
  cbind(
    sample_n(
      data_weather_input
      ,size =
        data_input %>% 
        count() %>% 
        as.numeric()
      ,replace = TRUE
    )
  ) %>% 
  mutate(
    mth = as.numeric(mth_input[i, ])
    ,Jan = if_else(mth == 1, 1, 0)
    ,Feb = if_else(mth == 2, 1, 0)
    ,Mar = if_else(mth == 3, 1, 0)
    ,Apr = if_else(mth == 4, 1, 0)
    ,May = if_else(mth == 5, 1, 0)
    ,Jun = if_else(mth == 6, 1, 0)
    ,Jul = if_else(mth == 7, 1, 0)
    ,Aug = if_else(mth == 8, 1, 0)
    ,Sep = if_else(mth == 9, 1, 0)
    ,Oct = if_else(mth == 10, 1, 0)
    ,Nov = if_else(mth == 11, 1, 0)
    ,Dec = if_else(mth == 12, 1, 0)
    ,Sulf.C5 = Sulf * C5
    ,Sulf.L_LSB = Sulf * Light_LSB
    ,Sulf.L_SW = Sulf * Light_SW
    ,Sulf.M_LSB = Sulf * Medium_LSB
    ,Sulf.M_SW = Sulf * Medium_SW
    ,Sulf.Midale = Sulf * Midale
    ,Sulf.H_Sour = Sulf * Heavy_Sour
    ,Sulf.H_SW = Sulf * Heavy_SW
    ,Dens.C5 = Dens * C5
    ,Dens.L_LSB = Dens * Light_LSB
    ,Dens.L_SW = Dens * Light_SW
    ,Dens.M_LSB = Dens * Medium_LSB
    ,Dens.M_SW = Dens * Medium_SW
    ,Dens.Midale = Dens * Midale
    ,Dens.H_Sour = Dens * Heavy_Sour
    ,Dens.H_SW = Dens * Heavy_SW
    ,Temp.Roll.C5 = Temp.Roll * C5
    ,Temp.Roll.L_LSB = Temp.Roll * Light_LSB
    ,Temp.Roll.L_SW = Temp.Roll * Light_SW
    ,Temp.Roll.M_LSB = Temp.Roll * Medium_LSB
    ,Temp.Roll.M_SW = Temp.Roll * Medium_SW
    ,Temp.Roll.Midale = Temp.Roll * Midale
    ,Temp.Roll.H_Sour = Temp.Roll * Heavy_Sour
    ,Temp.Roll.H_SW = Temp.Roll * Heavy_SW
  ) %>% 
  select(var.pred) %>% 
  as.matrix()
colnames(pred_matrix_input) <- NULL

fitted_actual_input <- 
  fitted_actual_input %>% 
  rbind(
    tibble(
      Num_Month = i
      ,Month = Mth_long[i]
      ,Crude_Breakdown = data_input$Crude_Breakdown
      ,Fitted = 
        predict(
          cvfit_penalty_dummy
          ,newx = pred_matrix_input
          ,s = "lambda.1se"
        ) %>% 
        as.vector()
      ,Actual = data_input$VP
    )
  )
}


# Simulation Outputs ---------------------------------
# Reorders the months to be in calendar-esque order.
fitted_actual_input$Month <-
  fitted_actual_input$Month %>% 
  as.factor() %>% 
  factor(levels(.)[
    c(
      6
      ,5
      ,9
      ,2
      ,10
      ,8
      ,7
      ,3
      ,13
      ,12
      ,11
      ,4
      ,1
    )
  ]
)

fitted_actual_input_tidy <- 
  fitted_actual_input %>% 
  gather(
    Result
    ,VP
    ,-Num_Month
    ,-Month
    ,-Crude_Breakdown
  )

fitted_actual_input_summary <- 
  fitted_actual_input %>% 
  group_by(Month, Num_Month) %>% 
  summarize(
    `Mean Predicted VP` = mean(Fitted)
    ,`Median Predicted VP` = median(Fitted)
    ,`SD Predicted VP` = sd(Fitted)
    ,`Min Predicted VP` = min(Fitted)
    ,`Max Predicted VP` = max(Fitted)
    ,`Data Points` = length(Fitted)
  ) %>% 
  left_join(
    data_test_input %>% 
      select(
        Num_Month
        ,`WAVG VP`
      )
  ) %>% 
  ungroup() %>% 
  # mutate(
  #   Density = Density_input
  #   ,Sulfur = Sulfur_input
  #   ,`Crude Type` = Crude_Breakdown_Cleaned[Crude_Breakdown_input]
  #   ,`7 Day Temperature Average` = Temp.Roll_input
  # ) %>% 
  select(
    Month
    # ,Density
    # ,Sulfur
    # ,`Crude Type`
    # ,`7 Day Temperature Average`
    ,`WAVG VP`
    ,`Mean Predicted VP`
    ,`Median Predicted VP`
    ,`SD Predicted VP`
    ,`Min Predicted VP`
    ,`Max Predicted VP`
    ,`Data Points`
  )
fitted_actual_input_summary

fitted_actual_input_summary_tidy <- 
  fitted_actual_input_summary %>% 
  gather(
    Model
    ,VP
    ,-Month
    ,-`Data Points`
  )

# Simulation Graphs ------------------------------------------

b_base <-
  ggplot(
    data = fitted_actual_input_tidy
    ,aes(
      x = VP
    )
  ) +
  labs(
    title = "Distribution of VP in kPa"
    ,subtitle = 
      paste(
        "For"
        ,"SES"#Crude_Breakdown_Cleaned[Crude_Breakdown_input]
      )
    ,x = "VP (kPa)"
  ) + 
  facet_wrap(
    ~ Month
  )

b_kern <-
  b_base +
  geom_density() +
  labs(
    y = "Density"
  )

b_ecdf <-
  b_base +
  stat_ecdf(
    geom = "line"
  ) +
  labs(
    y = "P(< VP)"
  )

b_kern
b_ecdf

c_base <- 
  ggplot(
    fitted_actual_input_summary_tidy %>% 
      filter(
        Model != "SD Predicted VP"
        ,Model != "Median Predicted VP"
      )
    ,aes(
      x = Month
      ,y = VP
      ,colour = Model
    )
  ) +
  scale_x_discrete(
    labels = Mth_comb
  ) +
  scale_colour_brewer(
    type = "qual"
    ,name = "Model:"
    ,palette = "Dark2"
  ) +
  labs(
    title = "Actual vs. Fitted VP by Month"
    ,subtitle = 
      paste(
        "For"
        ,Crude_Breakdown_Cleaned[Crude_Breakdown_input]
      )
    ,x = ""
    ,y = "VP (kPa)"
  )
  

c_a_f <- 
  c_base +
  geom_violin(
    data = fitted_actual_input_tidy
    ,aes(
      y = VP
    )
    ,colour = "black"
  ) +
  stat_boxplot(
    data = fitted_actual_input_tidy
    ,aes(
      y = VP
    )
    ,geom = "errorbar"
    ,colour = "black"
    ,width = 2/3
  ) +
  geom_boxplot(
    data = fitted_actual_input_tidy
    ,aes(
      y = VP
    )
    ,colour = "black"
    ,fill = "white"
    ,width = 1/8
  ) +
  geom_line(
    aes(
      group = Model
    )
    ,size = 1
  )

c_a_f

