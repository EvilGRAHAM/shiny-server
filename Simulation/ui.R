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
  
  ,headerPanel("VP Simulation")
  
  ,sidebarLayout(
    sidebarPanel(
      numericInput(
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
        inputId = "multiplier_crude_input"
        ,label = "SD Multiplier for Crude Quality"
        ,value = 2
        ,step = 0.5
      )
      ,numericInput(
        inputId = "multiplier_weather_input"
        ,label = "SD Multiplier for Temperature"
        ,value = 1
        ,step = 0.5
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
        "run_sim"
        ,"Run Simulation"
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
    
    ,mainPanel(
      tabsetPanel(type = "tabs"
        ,tabPanel("Distribution", plotOutput("b_kern"))
        ,tabPanel("ECDF", plotOutput("b_ecdf"))
        ,tabPanel("Results", plotOutput("result_chart"))
      )
    )

  )
  ,fluidRow(
    column(
      4
      ,offset = 0
      ,dataTableOutput("LASSO_coef")
    )
    # column(
    #   2
    #   ,offset = 0
    #   ,tableOutput("LASSO_coef_1")
    # )
    # ,column(
    #   2
    #   ,offset = 0
    #   ,tableOutput("LASSO_coef_2")
    # )
    ,column(
      8
      ,offset = 0
      ,dataTableOutput("fitted_actual_input_summary")
    )
  )
)