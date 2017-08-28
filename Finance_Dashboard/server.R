# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


# ggplot Formatting ----------
# Updates theme_minimal so that there is borders around the graphs and the facet headings.
# theme_minimal2 <- theme_minimal() %>%  theme_set()
# theme_minimal2 <-
#   theme_update(
#     panel.border = element_rect(
#       linetype = "solid"
#       ,colour = "grey92"
#       ,fill = NA
#     )
#     ,strip.background = element_rect(
#       linetype = "solid"
#       ,colour = "grey92"
#       ,fill = NA
#     )
#   )
theme_tq() %>% theme_set()


# Functions ----------
# Retrieves stocks, and adds the returns column
stock_retrieve <- function(ticker, date_min, date_max, return_period, returns_col){
  tibble(symbol = ticker) %>% 
    tq_get(
      to = date_max
    ) %>%
    group_by(symbol) %>% 
    # filter(
    #   date <= date_max
    #   ,date >= date_min
    # ) %>% 
    tq_mutate(
      select = adjusted
      ,mutate_fun = periodReturn
      ,period = return_period
      ,col_rename = returns_col
    )
}


# Server ----------
shinyServer(
  function(input, output, session) {
    
    # Stock Data set ----------
    stock_price_data <- reactive({
      withProgress(
        message = "Retrieving Stock Data..."
        ,value = NULL
        ,stock_retrieve(
          input$stock_ticker
          ,input$date_range[[1]]
          ,input$date_range[[2]]
          ,input$return_period
          ,"R_a"
        )
      ) 
    })
    
    # Index Data set ----------
    index_price_data <- reactive({
      withProgress(
        message = "Retrieving Index Data..."
        ,value = NULL
        ,stock_retrieve(
          input$index_ticker
          ,input$date_range[[1]]
          ,input$date_range[[2]]
          ,input$return_period
          ,"R_b"
        )
      )
    })
    
    # Price Table ----------
    output$price_tbl <- renderDataTable({
      if(is.null(input$stock_ticker)){
        tibble(
          stock = as.numeric(NA)
          ,date = as.numeric(NA)
          ,open = as.numeric(NA)
          ,high = as.numeric(NA)
          ,low = as.numeric(NA)
          ,close = as.numeric(NA)
          ,volume = as.numeric(NA)
          ,adjusted = as.numeric(NA)
          ,R_a = as.numeric(NA)
        )
      } else{
        stock_price_data() %>% 
          mutate(
            open = round(open, 2)
            ,high = round(high, 2)
            ,low = round(low, 2)
            ,close = round(close, 2)
            ,adjusted = round(adjusted, 2)
            ,R_a = paste0(round(R_a, 4)*100,"%")
          ) %>% 
          # select(-R_a) %>% 
          arrange(
            desc(date)
          )
      }
      }
      ,selection = "none"
    )
    
    # Price Time Series ----------
    output$price_ts <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Date"
            ,y = "Price"
          )
      } else{
        stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]] - 2*input$n_moving_average
          ) %>%
          ggplot(
            aes(
              x = date
              # ,y = close
              ,y = adjusted
              ,open = open
              ,high = high
              ,low = low
              ,close = close
            )
          ) +
          geom_barchart() +
          # geom_line() +
          geom_smooth(
            se = FALSE
          ) +
          geom_bbands(
            ma_fun = SMA
            ,sd = 2
            ,n = input$n_moving_average
          ) +
          facet_wrap(
            ~ symbol
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          labs(
            x = "Date"
            ,y = "Price"
          ) +
          coord_x_date(
            xlim = c(
              input$date_range[[1]]
              ,input$date_range[[2]]
            )
          )
      }
    })
    
    output$price_ts_click_info <- renderPrint({
      if(is.null(input$price_ts_click)) {
        
      } else{
        nearPoints(
          stock_price_data() %>% 
            filter(
              date >= input$date_range[[1]] - 2*input$n_moving_average
            )
          ,input$price_ts_click
          ,addDist = FALSE
        )
        # paste0("x = ", as.Date(input$price_ts_click$x, origin = "1970-01-01"), "\ny = ", input$price_ts_click$y)
      }  
    })
    
    # Returns Time Series ----------
    output$return_ts <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Date"
            ,y = "Return"
          )
      } else{
        stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          ggplot(
            aes(
              x = date
              ,y = R_a
            )
          ) +
          geom_line() +
          facet_wrap(
            ~ symbol
            ,scales = "fixed"
          ) +
          labs(
            x = "Date"
            ,y = "Return"
          )
      }
    })
    
    # Stock vs. Index Returns Chart ----------
    output$stock_index_return <- renderPlot({
      if(is.null(input$stock_ticker) | is.null(input$index_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = expression("R"[Mkt])
            ,y = expression("R"[i])
          )
      } else{
        stock_price_data() %>%
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          left_join(
            index_price_data()
            ,by = "date"
          ) %>% 
          ggplot(
            aes(
              x = R_b
              ,y = R_a
            )
          ) +
          geom_point() +
          geom_smooth(method = "lm") +
          facet_wrap(
            symbol.y ~ symbol.x
            ,scales = "fixed"
          ) +
          labs(
            x = expression("R"[Mkt])
            ,y = expression("R"[i])
          )
      }
    })
    
    # Regression Model ----------
    output$capm_regression <- renderPrint({
      if(is.null(input$stock_ticker) | is.null(input$index_ticker)) {
        # Do Nothing...
      } else{
        stock_price_data() %>% 
          left_join(
            index_price_data()
            ,by = "date"
            ,suffix = c(".stock", ".index")
          ) %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          split(list(
            .$symbol.stock
            ,.$symbol.index
          )) %>% 
          map(~ lm(R_a ~ R_b, data = .x)) %>% 
          map(summary)
      }
    })
    
    # Gold Test ----------
    output$gold <- renderDataTable(
      tq_get(
        tibble(symbol = "gold")
        ,get = "metal.prices"
      )
      ,selection = "none"
    )
    
    # Energy Stock Data set ----------
    energy_stock_price_data <- reactive({
      withProgress(
        message = "Retrieving Stock Data..."
        ,value = NULL
        ,stock_retrieve(
          input$energy_stock_ticker
          ,input$date_range[[1]]
          ,input$date_range[[2]]
          ,input$return_period
          ,"R_a"
        )
      ) 
    })
    
    # Energy Commodity Data set ----------
    energy_commodity_price_data <- reactive({
      withProgress(
        message = "Retrieving Energy Data..."
        ,value = NULL
        ,tq_get(
          tibble(commodity = input$energy_commodity_ticker)
          ,get = "economic.data"
        ) %>%
          group_by(commodity) %>%
          filter(
            date <= input$date_range[[2]]
            ,date >= input$date_range[[1]]
          ) 
      )
    })
    
    # Energy Time Series ----------
    output$energy_price_ts <- renderPlot({
      # If nothing is inputted a blank plot is produced.
      if(is.null(input$energy_commodity_ticker) & is.null(input$energy_stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Date"
            ,y = "Price"
          )
        
      # Plots only the commodity if no stock in inputed.
      } else if(is.null(input$energy_stock_ticker)){
        energy_commodity_price_data() %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          ggplot(
            aes(
              x = date
              ,y = price
            )
          ) +
          geom_area(
            fill = "grey"
            ,alpha = 0.75
          ) +
          facet_wrap(
            ~ commodity
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          labs(
            x = "Date"
            ,y = "Price"
          )
        
      # Plots only the stock if no commodity in inputed.
      } else if(is.null(input$energy_commodity_ticker)){
        energy_stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          ggplot(
            aes(
              x = date
              ,y = close
            )
          ) +
          geom_line(
            aes(
              colour = symbol
            )
          ) +
          scale_color_tq(
            name = "Ticker:"
          ) +
          labs(
            x = "Date"
            ,y = "Price"
          )
      # Plots both if both are inputted.
      } else{
          energy_stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          ggplot(
            aes(
              x = date
              ,y = close
            )
          ) +
          geom_area(
            data = energy_commodity_price_data()
            ,aes(
              y = price
            )
            ,fill = "grey"
            ,alpha = 0.75
          ) +
          geom_line(
            aes(
              colour = symbol
            )
          ) +
          facet_wrap(
            ~ commodity
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          scale_color_tq(
            name = "Ticker:"
          ) +
          labs(
            x = "Date"
            ,y = "Price"
          )
      }
    })
  }
)
