rm(list=ls())
# Libraries ----------
library(CVXR, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(plotly, warn.conflicts = FALSE, quietly = TRUE)


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


# Variables ----------
stock_list <- tibble(symbol = c("AAPL", "NFLX", "GOOGL", "FB") %>% sort)
stock_num <- stock_list %>% count %>% as.numeric()
time_period <- c(from = today(), to = today() - years(5))

# Functions ----------
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


# Stock Prices and Returns ----------
stock_returns <-
  stock_list %>%
  tq_get(
    from = today() - years(5)
    ,to = today()
  ) %>% 
  group_by(symbol) %>% 
  tq_mutate(
    select = adjusted
    ,mutate_fun = periodReturn
    ,period = "daily"
    ,col_rename = "R_a"
  )

stock_summary <- 
  stock_returns %>% 
  tq_performance(
    Ra = R_a
    ,Rb = NULL
    , performance_fun = table.AnnualizedReturns
  )
stock_summary

stock_return_mat <-
  stock_summary %>% 
  ungroup() %>% 
  select(AnnualizedReturn) %>% 
  as.matrix() 
rownames(stock_return_mat) <- stock_list$symbol
stock_return_mat

stock_wide <- 
  stock_returns %>%
  select(symbol, R_a, date) %>% 
  spread(symbol, R_a)

stock_cov <- 
  stock_wide %>% 
  select(-date) %>% 
  cov() * 252
stock_cov

# CVX ----------
stock_weights <- Variable(rows = stock_num)
objective_variance <- Minimize(quad_form(stock_weights, stock_cov))
portfolio_constraints <- 
  list(
    t(stock_return_mat) %*% stock_weights >= 0.30
    ,t(rep(x = 1, times = stock_num)) %*% stock_weights == 1
    ,stock_weights >= 0
  )
portfolio_problem <- Problem(objective = objective_variance, constraints = portfolio_constraints)
optim_port <- solve(portfolio_problem)

optim_stock_weights <- optim_port$getValue(stock_weights)
t(stock_return_mat) %*% optim_stock_weights
t(optim_stock_weights) %*% stock_cov %*% optim_stock_weights
optim_port$value

stock_returns %>%
  tq_portfolio(
    assets_col = symbol
    ,returns_col = R_a 
    ,weights = as.vector(round(optim_stock_weights, 4))
  )

stock_returns %>%
  tq_portfolio(
    assets_col = symbol
    ,returns_col = adjusted 
    ,weights = as.vector(round(optim_stock_weights, 4))
  )
