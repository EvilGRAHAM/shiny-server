#Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)


# ggplot Formatting ----------
# Updates theme_minimal so that there is borders around the graphs and the facet headings.
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


# Server ----------
shinyServer(
  function(input, output) {
    
    # Data set ----------
    price_data <- reactive({
      tq_get(
        tibble(symbol = "AAPL")#input$stock_ticker)
      ) %>% 
        group_by(
          symbol
        )
    })
    
    # Price Table ----------
    output$price_tbl <- renderTable({
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
          ,Ra = as.numeric(NA)
        )
      } else{
        price_data() %>% 
          filter(
            date <= input$date_range[[2]]
            ,date >= input$date_range[[1]]
          ) %>% 
          arrange(
            desc(date)
          )
      }
    })
    
    # Price Time Series ----------
    output$price_ts <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + geom_blank()
      } else{
        price_data() %>% 
          filter(
            date <= input$date_range[[2]]
            ,date >= input$date_range[[1]]
          ) %>% 
          ggplot(
            aes(
              x = date
              ,y = close
              ,open = open
              ,high = high
              ,low = low
              ,close = close
            )
          ) +
          geom_barchart() +
          geom_bbands(
            ma_fun = SMA
            ,sd = 2
            ,n = 20
          ) +
          facet_wrap(
            ~ symbol
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          labs(
            x = "Date"
            ,y = "Price"
          )
      }
    })
    
    # Returns Time Series ----------
    output$return_ts <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + geom_blank()
      } else{
        price_data() %>% 
          filter(
            date <= input$date_range[[2]]
            ,date >= input$date_range[[1]]
          ) %>% 
          tq_transmute(
            select = adjusted
            ,mutate_fun = periodReturn
            ,period = input$return_period
            ,col_rename = "Ra"
          ) %>% 
          ggplot(
            aes(
              x = date
              ,y = Ra
            )
          ) +
          geom_line() +
          facet_wrap(
            ~ symbol
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          labs(
            x = "Date"
            ,y = "Return"
          )
      }
    })
    
    output$test <- renderTable(head(tq_get("AAPL")))#renderTable(head(tq_get("gold", get = "metal.prices")))
  }
)
