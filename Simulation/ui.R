rm(list=ls())
try(dev.off(), silent = TRUE)
# Libraries -----------------------------
library(glmnet, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(ggfortify, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)


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
      ,actionButton(
        "run_sim"
        ,"Run Simulation"
      )
      ,downloadButton(
        outputId = "download"
        ,label = "Download"
      )
    )
    ,mainPanel(
      tableOutput("fitted_actual_input_summary")
      ,tabsetPanel(type = "tabs"
        ,tabPanel("Distribution", plotOutput("b_kern"))
        ,tabPanel("ECDF", plotOutput("b_ecdf"))
      )
      ,tableOutput("LASSO_coef")
    )
  )
)

