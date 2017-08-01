rm(list=ls())
try(dev.off(), silent = TRUE)
# Libraries -----------------------------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinyjs, warn.conflicts = FALSE, quietly = TRUE)
library(plotly, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)

fluidPage(
  theme = shinytheme("lumen")
  
  ,titlePanel("VP Simulation")
  
  
  ,fluidRow(
    
    # Inputs -------------------------------
    column(
      4
      ,offset = 0
      ,wellPanel(
        checkboxInput(
          inputId = "post_aquisition_adjustment"
          ,label = "Use adjustment for changes post-aquisition:"
          ,value = TRUE
        )
        ,numericInput(
          inputId = "dens_input"
          ,label = "Density (kg/m3)"
          ,value = 825
          ,min = 775
        )
        ,numericInput(
          inputId = "sulf_input"
          ,label = "Sulfur (%wt)"
          ,value = 0.5
          ,min = 0.1
          ,step = 0.1
        )
        ,numericInput(
          inputId = "temp.roll_input"
          ,label = "7 Day Temperature Average for SE Sask (°C)"
          ,value = 0
          ,step = 0.5
        )
        ,sliderInput(
          inputId = "mth_input"
          ,label = "Number of the month you wish to simulate. i.e. 1 = Jan, 2 = Feb, ..."
          ,value = 1
          ,step = 1
          ,ticks = FALSE
          ,min = 1
          ,max = 12
        )
        ,numericInput(
          inputId = "multiplier_density_input"
          ,label = "SD Multiplier for Density"
          ,value = 1
          ,step = 0.25
        )
        ,numericInput(
          inputId = "multiplier_sulfur_input"
          ,label = "SD Multiplier for Sulfur"
          ,value = 1
          ,step = 0.25
        )
        ,numericInput(
          inputId = "multiplier_weather_input"
          ,label = "SD Multiplier for 7 Day Rolling Average Temperature"
          ,value = 1
          ,step = 0.25
        )
        ,numericInput(
          inputId = "alpha_input"
          ,label = "Alpha for Elastic net regularization (0 ⇒ More like Ridge, 1 ⇒ More like LASSO)"
          ,value = 1
          ,min = 0
          ,step = 0.1
          ,max = 1
        )
        ,actionButton(
          inputId = "run_sim"
          ,label = "Run Simulation"
        )
        ,downloadButton(
          outputId = "downloadData"
          ,label = "Download Data"
        )
        ,downloadButton(
          outputId = "downloadResult"
          ,label = "Download Results"
        )
        ,downloadButton(
          outputId = "downloadModel"
          ,label = "Download Model Coefficients"
        )
      )
    )
    
    # Charts ---------------------------
    ,column(
      8
      ,offset = 0
      ,tabsetPanel(type = "tabs"
        ,tabPanel("Distribution", plotOutput("b_kern"))
        ,tabPanel("ECDF", plotOutput("b_ecdf"))
        ,tabPanel("Results", plotOutput("result_chart"))
      )
    )
    
    # Results Table --------------------------
    ,column(
      6
      ,offset = 0
      ,dataTableOutput("fitted_actual_input_summary")
    )
  )
  

  ,fluidRow(
    
    # LASSO Coefficients --------------------------
    column(
      4
      ,offset = 0
      ,dataTableOutput("LASSO_coef")
    )
    
  )


)