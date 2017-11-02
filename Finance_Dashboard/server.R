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
theme_tq() %>% theme_set()


# Functions ----------
# Retrieves stocks, and adds the returns column
stock_retrieve <- function(ticker, date_min, date_max, returns_col){
  tibble(symbol = ticker) %>%
    tq_get(
      to = date_max
    ) %>%
    group_by(symbol) %>%
    tq_mutate(
      select = adjusted
      ,mutate_fun = periodReturn
      ,period = "daily"
      ,col_rename = returns_col
    )
}

weight_determine <- function(stock_list, n){
  stock_weight <- rep(0, n)
  stock_order <- sample(n)
  for (i in 1:n-1){
    stock_weight[stock_order[i]] <- runif(n = 1, min = 0, max = 1 - sum(stock_weight)) %>% round(4)
  }
  stock_weight[stock_order[n]] <- 1 - sum(stock_weight)
  stock_weight
}

stock_var_fun <- function(stock_list, stock_cov, stock_weight, n){
  stock_var <- 0
  for (i in 1:n){
    for(j in 1:n){
      stock_var <- stock_var + stock_cov[i, j] * stock_weight[i] * stock_weight[j]
    }
  }
  stock_var
}

port_optim <- function(stock_ticker, stock_price_data, port_num_iterations, port_max_volatility){
  stock_num <- 
    stock_ticker %>% 
    length %>% 
    as.numeric
  
  stock_summary <-
    stock_price_data %>%
    tq_performance(
      Ra = R_a
      ,Rb = NULL
      , performance_fun = table.AnnualizedReturns
    )
  
  stock_cov <-
    stock_price_data %>%
    select(symbol, R_a, date) %>%
    spread(symbol, R_a) %>%
    select(-date) %>%
    cov() * 252
  
  portfolio_results <- matrix(nrow = port_num_iterations, ncol = 2 + stock_num)
  colnames(portfolio_results) <- c("Mean Return", "Volatility", stock_ticker %>% as.character())
  portfolio_results %<>%
    as.tibble %>%
    mutate_all(.funs = as.numeric)
  
  for(i in 1:port_num_iterations){
    portfolio_weight <- weight_determine(stock_list = stock_ticker, n = stock_num)
    portfolio_sd <- stock_var_fun(stock_list = stock_ticker, stock_cov = stock_cov, stock_weight = portfolio_weight, n = stock_num) %>% sqrt
    portfolio_return <- sum(stock_summary$AnnualizedReturn * portfolio_weight)
    portfolio_results[i, ] <- c(portfolio_return, portfolio_sd, portfolio_weight)
  }
  
  portfolio_optimal <-
    portfolio_results %>%
    filter(Volatility <= port_max_volatility) %>%
    arrange(desc(`Mean Return`)) %>%
    first
  
  portfolio_optimal %>%
    select(
      -c(
        `Mean Return`
        ,Volatility
      )
    ) %>%
    gather(
      key = symbol
      ,value = Weight
    ) %>%
    ggplot(
      aes(
        x = symbol
        ,y = Weight
      )
    ) +
    geom_col(alpha = 0.75) +
    labs(
      x = "Stock Ticker"
      ,y = "Weight (%)"
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
              ,y = adjusted
              ,open = open
              ,high = high
              ,low = low
              ,close = close
            )
          ) +
          geom_barchart() +
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
          tq_transmute(
            select = adjusted
            ,mutate_fun = periodReturn
            ,period = input$return_period
            ,col_rename = "R_a"
          ) %>% 
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
          geom_hline(
            aes(
              yintercept = 0
            )
            ,linetype = "dotted"
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
    
    # Autocorrelation Plot ----------
    output$acf_plot <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Lag"
            ,y = "Autocorrelation"
          )
      } else{
        stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          select(
            symbol
            ,R_a
          ) %>% 
          split(f = .$symbol) %>% 
          map(function(x) acf(x = x$R_a, plot = FALSE, lag.max = input$n_acf_lag[[2]])) %>% 
          map_dfr(~with(data = .,expr = data.frame(Lag = lag, Autocorrelation = acf))) %>% 
          mutate(symbol = rep(c(input$stock_ticker), each = input$n_acf_lag[[2]] + 1)) %>% 
          filter(
            Lag <= input$n_acf_lag[[2]]
            ,Lag >= input$n_acf_lag[[1]]
          ) %>% 
          ggplot(
            aes(
              x = Lag
              ,y = Autocorrelation
            )
          ) +
          geom_hline(
            aes(
              yintercept = 0
            )
            ,linetype = "dotted"
          ) +
          geom_segment(
            aes(
              xend = Lag
              ,yend = 0
            )
          ) +
          geom_smooth(
            linetype = 0
            ,method = "lm"
            ,formula = y ~ 1
          ) +
          facet_wrap(
            ~ symbol
          )
      }
    })
    
    # Market Cap Plot ----------
    output$market_cap_bc <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Stock Ticker"
            ,y = "Market Cap ($)"
          )
      } else{
        tibble(symbol = input$stock_ticker) %>% 
          tq_get(get = "key.stats") %>% 
          group_by(symbol) %>% 
          select(
            symbol
            ,Market.Capitalization
          ) %>% 
          arrange(symbol) %>% 
          ggplot(
            aes(
              x = symbol
              ,y = Market.Capitalization
            )
          ) + 
          geom_col(alpha = 0.75) +
          labs(
            x = "Stock Ticker"
            ,y = "Market Cap ($)"
          )
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
    
    # Portfolio Weights ----------
    output$port_opt_prop_bc <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() +
          geom_blank() +
          labs(
            x = "Stock Ticker"
            ,y = "Weight (%)"
          )
      } else{
        port_optim(
          stock_ticker = input$stock_ticker
          ,stock_price_data = stock_price_data()
          ,port_num_iterations = input$port_num_iterations
          ,port_max_volatility = input$port_max_volatility
        )
      }
    })
  }
)
