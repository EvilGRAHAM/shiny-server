rm(list=ls())
try(dev.off(), silent = TRUE)
# Libraries -----------------------------
library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)

fluidPage(
  theme = shinytheme("lumen")
  
  ,headerPanel('Simulation Inputs:')
  
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
        inputId = "multiplier_input"
        ,label = "SD Multiplier"
        ,value = 2
        ,step = 0.5
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
      # dataTableOutput("fitted_actual_input_summary")
      tabsetPanel(type = "tabs"
        ,tabPanel("Distribution", plotOutput("b_kern"))
        ,tabPanel("ECDF", plotOutput("b_ecdf"))
        ,tabPanel("Results", plotOutput("result_chart"))
      )
    )
  )
  ,fluidRow(
    column(
      2
      ,offset = 0
      ,tableOutput("LASSO_coef_1")
    )
    ,column(
      2
      ,offset = 0
      ,tableOutput("LASSO_coef_2")
    )
    ,column(
      8
      ,offset = 0
      ,dataTableOutput("fitted_actual_input_summary")
    )
  )
)