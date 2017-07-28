# Libraries -----------------------------
library(glmnet, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
library(ggfortify, warn.conflicts = FALSE, quietly = TRUE)
library(plotly, warn.conflicts = FALSE, quietly = TRUE)
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)
library(zeallot, warn.conflicts = FALSE, quietly = TRUE)

# Functions ----------------------------
data.conversions <- function(data_file) {
  
  # Converts SDate to date format, and Producer + Battery to factors
  # Grabs the year and the month for each entry.
  # Arranges the entries chronologically
  data_file %<>%
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
    Temp.Roll_input <- read_input("7 Day Temperature Average for SE Sask (°C)") %>% as.numeric() %>% try(silent = TRUE)
  }
  while (is.na(mth_input)) {
    mth_input <- read_input("Number of the month you wish to simulate. i.e. 1 = Jan, 2 = Feb, ...") %>% as.numeric() %>% try(silent = TRUE)
  }
  return(list(Density_input, Sulfur_input, Temp.Roll_input, mth_input))
}

# God I hate having to constantly use this set of pipes. 
# Count normally outputs a tibble, so this converts it to a number so we can do
# arithmetic and such with it.
count.num <- function(data_tibble) {
  n <- 
    data_tibble %>% 
    count %>% 
    as.numeric()
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
# Initialize ---------------------------------------------
# Initializes one of the output tables.
fitted_actual_input_summary_init <- tibble(
  Month = NA
  ,Density = as.numeric(NA)
  ,Sulfur = as.numeric(NA)
  ,`Crude Type` = NA
  ,`7 Day Temperature Average` = as.numeric(NA)
  ,`Mean Historical VP` = as.numeric(NA)
  ,`Mean Predicted VP` = as.numeric(NA)
  ,`Median Predicted VP` = as.numeric(NA)
  ,`SD Predicted VP` = as.numeric(NA)
  ,`Min Predicted VP` = as.numeric(NA)
  ,`Max Predicted VP` = as.numeric(NA)
  ,`Data Points` = as.numeric(NA)
)

# Page -----------------------------
function(input, output, session) {
  
  # Data Import -----------------------------------
  
  # Reads the data in from a xlsx, and converts it to a tibble.
  # Adding interaction effects to data_complete.
  # As well it splits the mth variable into 12 dummy variables.
  data_complete <-
    read_csv(
      "https://raw.githubusercontent.com/EvilGRAHAM/shiny-server/master/Simulation/Data/VP_Data_Complete.csv"
    ) %>% 
    data.conversions() %>% 
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
  
  data_weather <-
    read_csv(
      "https://raw.githubusercontent.com/EvilGRAHAM/shiny-server/master/Simulation/Data/Weather_Data.csv"
    ) %>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
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
  
  
  # This is a list of all the variables we wish to include in the LASSO.
  var.pred <- c(
    "Sulf"
    ,"Dens"
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
    # ,"Jan"
    # ,"Feb"
    # ,"Mar"
    # ,"Apr"
    # ,"May"
    # ,"Jun"
    # ,"Jul"
    # ,"Aug"
    # ,"Sep"
    # ,"Oct"
    # ,"Nov"
    # ,"Dec"
    # ,"Post_Aquisition"
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
    # ,"Post_Aquisition.C5"
    # ,"Post_Aquisition.L_LSB"
    # ,"Post_Aquisition.L_SW"
    # ,"Post_Aquisition.M_LSB"
    # ,"Post_Aquisition.M_SW"
    # ,"Post_Aquisition.Midale"
    # ,"Post_Aquisition.H_Sour"
    # ,"Post_Aquisition.H_SW"
  )
  
  lasso_var_names <- c(
    "Sulfur"
    ,"Density"
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
    # ,"Jan"
    # ,"Feb"
    # ,"Mar"
    # ,"Apr"
    # ,"May"
    # ,"Jun"
    # ,"Jul"
    # ,"Aug"
    # ,"Sep"
    # ,"Oct"
    # ,"Nov"
    # ,"Dec"
    # ,"Post Aquisition"
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
    # ,"Post Aquisition:C5+"
    # ,"Post Aquisition:Light LSB"
    # ,"Post Aquisition:Light SW"
    # ,"Post Aquisition:Medium LSB"
    # ,"Post Aquisition:Medium SW"
    # ,"Post Aquisition:Midale"
    # ,"Post Aquisition:Heavy Sour"
    # ,"Post Aquisition:Heavy SW"
  )
  
  # Main Function ------------------------------
  main <- function(){
    
    # Setup -------------------------------------------------------------------
    
    set.seed(19)
    
    # Looks at how the VP changes by crude type pre and post aquisition.
    post_aquisition_bd <- 
      data_complete %>% 
      group_by(
        Post_Aquisition
        ,Crude_Breakdown
      ) %>% 
      summarize(
        Mean = mean(VP)
      )
    post_aquisition_bd_spread <- 
      post_aquisition_bd %>% 
      spread(
        Post_Aquisition
        ,Mean
      ) %>% 
      mutate(
        Change_in_VP = `1` - `0`
      )
    colnames(post_aquisition_bd_spread) <- c("Crude_Breakdown", "Pre_Aquisition_VP", "Post_Aquisition_VP", "Change_in_VP")
    
    # Only use the adjusted values if the checkbox is set to true.
    if (input$post_aquisition_adjustment){
      data_complete %<>%
        left_join(
          post_aquisition_bd_spread
        ) %>% 
        mutate(
          VP = if_else(.$Post_Aquisition == 0
                       ,VP + Change_in_VP
                       ,VP
          )
        )
    }
    
    # How many times we multiply the SD by to get our input rows.
    crude_multiplier <- input$multiplier_crude_input
    weather_multiplier <- input$multiplier_weather_input
    
    
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
    set.seed(8)
    cvfit_penalty_dummy <-
      cv.glmnet(
        x = lasso.pred#[, 1:16]
        ,y = lasso.result
        ,alpha = input$alpha_input
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
    LASSO_coef <- coef(cvfit_penalty_dummy, s = "lambda.1se", exact = TRUE)
    LASSO_coef
    
    
    # Simulation Setup ---------------------------------------------
    
    
    Density_input <- input$dens_input
    Sulfur_input <- input$sulf_input
    Temp.Roll_input <- input$temp.roll_input
    mth_input <- input$mth_input
    
    # Creates an empty tibble to be filled with the results of the predictions.
    fitted_actual_input <- tibble(
      Num_Month = as.numeric(character())
      ,Month = as.factor(character())
      ,Crude_Breakdown = character()
      ,Fitted = as.numeric(character())
    )
    
    
    # Simulation Run -----------------------------------
    
    set.seed(4815609)
    
    # Determines the crude type based on the inputted data.
    Crude_Breakdown_input <- crude_type_check(Density_input, Sulfur_input)
    
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
      filter(mth == mth_input %>% as.numeric()) %>%
      summarize(
        Temp.Roll_Mean = mean(Temp.Roll)
        ,Temp.Roll_SD = sd(Temp.Roll)
      )
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
        Temp.Roll >= Temp.Roll_input %>% as.numeric() - weather_multiplier * (
          weather_stats %>%
            select(Temp.Roll_SD) %>%
            as.numeric()
        ) &
          Temp.Roll <= Temp.Roll_input %>% as.numeric() + weather_multiplier * (
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
      filter(
        # You can uncomment the first line if you want to force the simulated data set to only
        # contain crude of the same type as the inputted.
        # We filter out any crudes that don't exist within the inputed temperature range.
        # As well if you uncomment the second line, only those entries of the same month as 
        # the inputted are used.
        # Crude_Breakdown == Crude_Breakdown_input &
        # mth == as.numeric(mth_input) &
        Temp.Roll >= Temp.Roll_input %>% as.numeric() - crude_multiplier * (
          weather_stats %>%
            select(Temp.Roll_SD) %>%
            as.numeric()
        ) &
          Temp.Roll <= Temp.Roll_input %>% as.numeric() + crude_multiplier * (
            weather_stats %>%
              select(Temp.Roll_SD) %>%
              as.numeric()
          ) &
          Dens >= Density_input %>% as.numeric() - crude_multiplier * (
            crude_bd_stats %>%
              select(Density_SD) %>%
              as.numeric()
          ) &
          Dens <= Density_input %>% as.numeric() + crude_multiplier * (
            crude_bd_stats %>%
              select(Density_SD) %>%
              as.numeric()
          ) &
          Sulf >= Sulfur_input %>% as.numeric() - crude_multiplier * (
            crude_bd_stats %>%
              select(Sulfur_SD) %>%
              as.numeric()
          ) &
          Sulf <= Sulfur_input %>% as.numeric() + crude_multiplier * (
            crude_bd_stats %>%
              select(Sulfur_SD) %>%
              as.numeric()
          )
      ) %>% 
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
      )
    
    # Stops the main function if no data is returned.
    if(count(data_input) == 0){
      return(list(
        returned_data <- FALSE
        ,data.frame(x = numeric())
        ,ggplot(data = data.frame(x = numeric())) + geom_blank()
        ,ggplot(data = data.frame(x = numeric())) + geom_blank()
        ,LASSO_coef
        ,data.frame(x = numeric())
      )
      )
      stopifnot(TRUE)
    }
    
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
        mth = as.numeric(mth_input)
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
        ,Post_Aquisition.C5 = Post_Aquisition * C5
        ,Post_Aquisition.L_LSB = Post_Aquisition * Light_LSB
        ,Post_Aquisition.L_SW = Post_Aquisition * Light_SW
        ,Post_Aquisition.M_LSB = Post_Aquisition * Medium_LSB
        ,Post_Aquisition.M_SW = Post_Aquisition * Medium_SW
        ,Post_Aquisition.Midale = Post_Aquisition * Midale
        ,Post_Aquisition.H_Sour = Post_Aquisition * Heavy_Sour
        ,Post_Aquisition.H_SW = Post_Aquisition * Heavy_SW
      ) %>%
      select(var.pred) %>%
      as.matrix()
    colnames(pred_matrix_input) <- NULL
    
    fitted_actual_input <-
      fitted_actual_input %>%
      rbind(
        tibble(
          Num_Month = mth_input
          ,Month = Mth_long[mth_input]
          ,Crude_Breakdown = data_input$Crude_Breakdown
          ,Fitted =
            predict(
              cvfit_penalty_dummy
              ,newx = pred_matrix_input
              ,s = "lambda.1se"
            ) %>%
            as.vector()
          ,Historical = data_input$VP
        )
      )
    
    
    # Simulation Outputs --------------------------------------
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
        `Mean Historical VP` = round(mean(Historical), 4)
        ,`Mean Predicted VP` = round(mean(Fitted), 4)
        ,`Median Predicted VP` = round(median(Fitted), 4)
        ,`SD Predicted VP` = round(sd(Fitted), 4)
        ,`Min Predicted VP` = round(min(Fitted), 4)
        ,`Max Predicted VP` = round(max(Fitted), 4)
        ,`Data Points` = length(Fitted)
      ) %>%
      ungroup() %>%
      mutate(
        Density = Density_input
        ,Sulfur = Sulfur_input
        ,`Crude Type` = Crude_Breakdown_Cleaned[Crude_Breakdown_input]
        ,`7 Day Temperature Average` = Temp.Roll_input
      ) %>% 
      select(
        Month
        ,Density
        ,Sulfur
        ,`Crude Type`
        ,`7 Day Temperature Average`
        ,`Mean Historical VP`
        ,`Mean Predicted VP`
        ,`Median Predicted VP`
        ,`SD Predicted VP`
        ,`Min Predicted VP`
        ,`Max Predicted VP`
        ,`Data Points`
      )
    fitted_actual_input_summary
    
    
    # fitted_actual_input_summary_tidy <-
    #   fitted_actual_input_summary %>%
    #   gather(
    #     Model
    #     ,VP
    #     ,-Month
    #   )
    # Adds back the column names for the prediction matrix so it can be downloaded to a csv.
    colnames(pred_matrix_input) <- var.pred
    
    # Creates a list of the data used in the simulation to be downloaded by the user.
    data_output <- 
      pred_matrix_input %>% 
      as.data.frame() %>% 
      mutate(
        `Predicted VP` = fitted_actual_input$Fitted
        ,`Historical VP` = fitted_actual_input$Historical
      )
    
    # Simulation Graphs ------------------------------------------
    b_base <-
      ggplot(
        data = fitted_actual_input_tidy
        ,aes(
          x = VP
          ,colour = Result
        )
      ) +
      scale_colour_brewer(
        type = "qual"
        ,name = "Methodology"
        ,palette = "Dark2"
      ) +
      labs(
        title = "Distribution of VP in kPa"
        ,subtitle =
          paste(
            "For"
            ,Crude_Breakdown_Cleaned[Crude_Breakdown_input]
          )
        ,x = "VP (kPa)"
      ) +
      facet_wrap(
        ~ Month
      )
    
    b_kern <-
      b_base +
      # geom_histogram(
      #   aes(
      #     y = ..density..
      #   )
      #   ,alpha = 0.5
      #   ,binwidth = function(x) {2 * IQR(x) * length(x)^(-1/3)}
      # ) +
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
    
    # c_base <-
    #   ggplot(
    #     fitted_actual_input_summary_tidy %>%
    #       filter(
    #         Model != "SD Predicted VP"
    #         ,Model != "Median Predicted VP"
    #       )
    #     ,aes(
    #       x = Month
    #       ,y = VP
    #       ,colour = Model
    #     )
    #   ) +
    #   scale_x_discrete(
    #     labels = Mth_comb
    #   ) +
    #   scale_colour_brewer(
    #     type = "qual"
    #     ,name = "Model:"
    #     ,palette = "Dark2"
    #   ) +
    #   labs(
    #     title = "Actual vs. Fitted VP by Month"
    #     ,subtitle =
    #       paste(
    #         "For"
    #         ,Crude_Breakdown_Cleaned[Crude_Breakdown_input]
    #       )
    #     ,x = ""
    #     ,y = "VP (kPa)"
    #   )
    # 
    # 
    # c_a_f <-
    #   c_base +
    #   geom_violin(
    #     data = fitted_actual_input_tidy
    #     ,aes(
    #       y = VP
    #     )
    #     ,colour = "black"
    #   ) +
    #   stat_boxplot(
    #     data = fitted_actual_input_tidy
    #     ,aes(
    #       y = VP
    #     )
    #     ,geom = "errorbar"
    #     ,colour = "black"
    #     ,width = 2/3
    #   ) +
    #   geom_boxplot(
    #     data = fitted_actual_input_tidy
    #     ,aes(
    #       y = VP
    #     )
    #     ,colour = "black"
    #     ,fill = "white"
    #     ,width = 1/8
    #   ) +
    #   geom_line(
    #     aes(
    #       group = Model
    #     )
    #     ,size = 1
    #   )
    # 
    # c_a_f
    
    # Return ------------------------------
    return(
      list(
        returned_data <- TRUE
        ,fitted_actual_input_summary
        ,b_kern
        ,b_ecdf
        ,LASSO_coef
        ,data_output
      )
    )
  }
  
  # Output ------------------------------------
  
  fitted_actual_input_summary <- fitted_actual_input_summary_init
  # Creates an output when the action button is pressed.
  observeEvent(input$run_sim, {
    
    # Run Sim --------------------------
    # Adds a progress bar to show the simulation is running.
    withProgress(
      message = "Running Simulation"
      ,value = NULL
      ,c(
        returned_data_output
        ,fitted_actual_input_summary_output
        ,b_kern_output
        ,b_ecdf_output
        ,LASSO_coef_output
        ,data_output
      ) %<-% main()
    )
    
    # Data Cleaning ----------------------
    # Checks to see if any data, as if we don't these cause the application to crash.
    if(returned_data_output){
      fitted_actual_input_summary_new <- as.tibble(fitted_actual_input_summary_output)
      # To get each button push to add a new row, I had to initialize the tibble with NA's,
      # after we add a row, the NAs are removed.
      fitted_actual_input_summary <<-
        rbind(
          fitted_actual_input_summary
          ,fitted_actual_input_summary_new
        ) %>% 
        filter(!is.na(Month))
    } else{
      showNotification(
        "No data was found"
        ,type = "error"
      )
    }
    
    # Charts the results from each successive simulation.
    result_chart <-
      fitted_actual_input_summary %>% 
      mutate(Iteration = 1:count.num(fitted_actual_input_summary)) %>%
      select(
        -c(
          Density
          ,Sulfur
          ,`Crude Type`
          ,`7 Day Temperature Average`
          ,`SD Predicted VP`
          ,`Data Points`
        )
      ) %>% 
      gather(
        Result
        ,VP
        ,-Iteration
        ,-Month
      ) %>% 
      ggplot(
        aes(
          x = Iteration
          ,y = `VP`
          ,colour = Result
        )
      ) +
      scale_colour_brewer(
        type = "qual"
        ,name = "Result:"
        ,palette = "Set2"
      ) +
      labs(
        title = "Simulation Results"
        ,y = "VP (kPa)"
      ) +
      geom_line() +
      geom_point()
    
    # Grabs the variables and coefficients of the LASSO regression.
    LASSO_coef <-
      # LASSO_coef %>% 
      LASSO_coef_output %>%
      as.matrix() %>%
      round(4) %>% 
      as.data.frame() %>%
      rownames_to_column() %>%
      as.tibble() %>%
      filter(
        `1` != 0
      )
    colnames(LASSO_coef) <- c("Variable", "Coefficient")
    
    
    # Outputs -----------------------------------
    # Plots the kernel density estimator and the ECDF and the results
    output$b_kern <- renderPlot(b_kern_output)
    output$b_ecdf <- renderPlot(b_ecdf_output)
    output$result_chart <- renderPlot(result_chart)
    
    output$fitted_actual_input_summary <- renderDataTable(fitted_actual_input_summary)
    
    output$LASSO_coef <- renderDataTable(LASSO_coef)
    # output$LASSO_coef_1 <- renderTable(LASSO_coef_1)
    # output$LASSO_coef_2 <- renderTable(LASSO_coef_2)
    
    # Lets you download the data used in the model.
    output$downloadData <- 
      downloadHandler(
        filename = { paste("VP_Data_", Sys.Date(), ".csv", sep = "") }
        ,content = function(file) {
          write.csv(data_output, file)
        }
      )
    
    # Lets you download the simulation results
    output$downloadResult <- 
      downloadHandler(
        filename = { paste("VP_Simulation_Results_", Sys.Date(), ".csv", sep = "") }
        ,content = function(file) {
          write.csv(fitted_actual_input_summary %>% as.matrix, file)
        }
      )
    
    # Downloads the coefficients of the LASSO regression.
    output$downloadModel <- 
      downloadHandler(
        filename = { paste("VP_Model_", Sys.Date(), ".csv", sep = "") }
        ,content = function(file) {
          write.csv(
            LASSO_coef_output %>%
              as.matrix() %>%
              as.data.frame() %>%
              rownames_to_column() %>%
              as.tibble()
            , file
          )
        }
      )
    
    
  })
  
  
}

