# Libraries ----------
library(tools, warn.conflicts = FALSE, quietly = TRUE)
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(ggridges, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)
library(CVXR, warn.conflicts = FALSE, quietly = TRUE)

# ggplot Formatting ----------
# Updates theme_minimal so that there is borders around the graphs and the facet headings.
theme_tq() %>% theme_set()


# Functions ----------
# Retrieves stocks, and adds the returns column
stock_retrieve <- function(ticker, date_min, date_max, returns_col){
  tibble(symbol = ticker) %>%
    tq_get(
      from = min(date_min, today() - years(1))
      ,to = date_max
    ) %>%
    group_by(symbol) %>%
    tq_mutate(
      select = adjusted
      ,mutate_fun = periodReturn
      ,period = "daily"
      ,col_rename = returns_col
    )
}

port_optim <- function(stock_ticker, stock_price_data, port_min_return, short_sale_bool){
  stock_num <- length(stock_ticker)
  
  stock_summary <-
    stock_price_data %>%
    tq_performance(
      Ra = R_a
      ,Rb = NULL
      , performance_fun = table.AnnualizedReturns
    )
  
  stock_returns <- 
    stock_summary %>% 
    ungroup() %>% 
    select(AnnualizedReturn) %>% 
    as.matrix() 
  rownames(stock_returns) <- stock_ticker
  
  stock_cov <-
    stock_price_data %>%
    select(symbol, R_a, date) %>%
    spread(symbol, R_a) %>%
    select(-date) %>%
    cov() * 252
  
  stock_weights <- Variable(rows = stock_num)
  objective_variance <- Minimize(quad_form(stock_weights, stock_cov))
  portfolio_constraints <- 
    if(short_sale_bool){
      list(
        t(stock_returns) %*% stock_weights >= port_min_return # Higher than specified return
        ,t(rep(x = 1, times = stock_num)) %*% stock_weights == 1 # Fully allocate budget
      )
    } else{
      list(
        t(stock_returns) %*% stock_weights >= port_min_return # Higher than specified return
        ,t(rep(x = 1, times = stock_num)) %*% stock_weights == 1 # Fully allocate budget
        ,stock_weights >= 0 # No Short Selling
      )
    }
  portfolio_problem <- Problem(objective = objective_variance, constraints = portfolio_constraints)
  cvx_portfolio <- solve(portfolio_problem)
  
  cvx_weight <- cvx_portfolio$getValue(stock_weights)
  rownames(cvx_weight) <- stock_ticker
  colnames(cvx_weight) <- "Weight"
  cvx_volatility <- sqrt(cvx_portfolio$value)
  cvx_returns <- as.numeric(t(stock_returns) %*% cvx_weight)
  cvx_sharpe <- cvx_returns/cvx_volatility
  
  list(
    `Portfolio Summary` =
      stock_summary %>% 
      bind_rows(
        tibble(
          symbol = "Portfolio"
          ,AnnualizedReturn = cvx_returns
          ,`AnnualizedSharpe(Rf=0%)` = cvx_sharpe
          ,AnnualizedStdDev = cvx_volatility
        )
      ) %>% 
      left_join(
        cvx_weight %>% 
          as.data.frame() %>% 
          rownames_to_column(var = "symbol") %>% 
          bind_rows(
            tibble(symbol = "Portfolio", Weight = sum(cvx_weight))
          )
        ,by = "symbol"
      )
    ,`Covariance Matrix` = stock_cov
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
          ticker = input$stock_ticker
          ,date_min = input$date_range[[1]]
          ,date_max = input$date_range[[2]]
          ,returns_col = "R_a"
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
          Symbol = as.numeric(NA)
          ,Date = as.numeric(NA)
          ,Open = as.numeric(NA)
          ,High = as.numeric(NA)
          ,Low = as.numeric(NA)
          ,Close = as.numeric(NA)
          ,Volume = as.numeric(NA)
          ,Adjusted = as.numeric(NA)
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
          ) %>%
          ungroup() %>%
          rename_all(.funs = toTitleCase)
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
          # geom_smooth(
          #   se = FALSE
          # ) +
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
    
    # Volume Time Series ----------
    output$volume_ts <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Date"
            ,y = "Volume"
          )
      } else{
        stock_price_data() %>% 
          filter(
            date >= input$date_range[[1]] - 2*input$n_moving_average
          ) %>%
          ggplot(
            aes(
              x = date
              ,y = volume
            )
          ) +
          geom_col() +
          facet_wrap(
            ~ symbol
            ,scales = if_else(input$fix_y_axis_scale_ts, "fixed", "free_y")
          ) +
          labs(
            x = "Date"
            ,y = "Volume"
          ) +
          coord_x_date(
            xlim = c(
              input$date_range[[1]]
              ,input$date_range[[2]]
            )
          )
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
          left_join(
            y = 
              stock_price_data() %>% 
              filter(
                date >= input$date_range[[1]]
                ,date <= input$date_range[[2]]
              ) %>%  
              group_by(symbol) %>% 
              tally()
            ,by = "symbol"
          ) %>%
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
            aes(yintercept = 0)
            ,linetype = "dotted"
          ) +
          geom_ribbon(
            aes(
              ymin = qnorm(0.025)/sqrt(n)
              ,ymax = qnorm(0.975)/sqrt(n)
            )
            ,alpha = 0.2
          ) +
          geom_segment(
            aes(
              xend = Lag
              ,yend = 0
            )
          ) +
          facet_wrap(~ symbol)
      }
    })
    
    # Market Cap Plot ----------
    output$market_cap_bc <- renderPlot({
      # if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Stock Ticker"
            ,y = "Market Cap ($)"
          )
      # } else{
      #   tibble(symbol = input$stock_ticker) %>% 
      #     tq_get(get = "key.stats") %>% 
      #     group_by(symbol) %>% 
      #     select(
      #       symbol
      #       ,Market.Capitalization
      #     ) %>% 
      #     arrange(symbol) %>% 
      #     ggplot(
      #       aes(
      #         x = symbol
      #         ,y = Market.Capitalization
      #       )
      #     ) + 
      #     geom_col(alpha = 0.75) +
      #     labs(
      #       x = "Stock Ticker"
      #       ,y = "Market Cap ($)"
      #     )
      # }
    })
    
    # Return Histogram Plot ----------
    output$return_hist_ridge <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() + 
          geom_blank() +
          labs(
            x = "Return"
            ,y = "Stock Ticker"
          )
      } else{
        stock_price_data() %>%
          filter(
            date >= input$date_range[[1]]
            ,date <= input$date_range[[2]]
          ) %>%
          ggplot(
            aes(
              x = R_a
              ,y = symbol
              ,height = ..density..
            )
          ) +
          geom_density_ridges(
            stat = "density"
            ,rel_min_height = 0.001
          ) +
          labs(
            x = "Return"
            ,y = "Stock Ticker"
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
    port_opt_tbl <- reactive(
      port_optim(
        stock_ticker = input$stock_ticker
        ,stock_price_data = stock_price_data()
        ,port_min_return = input$port_min_return
        ,short_sale_bool = input$short_sale
      )
    )
    
    output$port_opt_weight_bc <- renderPlot({
      if(is.null(input$stock_ticker)){
        ggplot() +
          geom_blank() +
          labs(
            x = "Stock Ticker"
            ,y = "Weight (%)"
          )
      } else{
        port_opt_tbl()$`Portfolio Summary` %>% 
          filter(symbol != "Portfolio") %>% 
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
    })
    
    # Optimal Portfolio Stats ----------
    output$port_opt_stats <- renderDataTable(
      if(is.null(input$stock_ticker)){
        tibble(
          Symbol = as.numeric(NA)
          ,Return = as.numeric(NA)
          ,`AnnualizedSharpe(Rf=0%)` = as.numeric(NA)
          ,Volatility = as.numeric(NA)
          ,Weight = as.numeric(NA)
        )
      } else{
        port_opt_tbl()$`Portfolio Summary` %>% 
          rename(
            Symbol = symbol
            ,Return = AnnualizedReturn
            # ,`Sharpe Ratio` = `AnnualizedSharpe(RF=0%)`
            ,Volatility = AnnualizedStdDev
          ) %>% 
          mutate_if(.predicate = is.double, .funs = round, 4) %>% 
          ungroup() %>% 
          rename_all(toTitleCase)
      }
      ,selection = "none"
    )
    
    # Covariance Matrix ----------
    output$port_cov_mat <- renderDataTable({
      if(is.null(input$stock_ticker)){
        matrix(dimnames = list("Stock A", "Stock A"))
      } else{
        round(port_opt_tbl()$`Covariance Matrix`, 4)
      }
      }
      ,selection = "none"
    )
    
    # Portfolio Returns Time Series ----------
    output$port_return_ts <- renderPlot({
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
          tq_portfolio(
            assets_col = symbol
            ,returns_col = R_a
            ,weights = 
              port_opt_tbl()$`Portfolio Summary` %>% 
              filter(symbol != "Portfolio") %>% 
              mutate_if(.predicate = is.double, .funs = round, 4) %>% 
              select(symbol, weight = Weight)
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
          labs(
            x = "Date"
            ,y = "Return"
          )
      }
    })
  
})
    