rm(list=ls())
# Libraries ----------
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lpSolve, warn.conflicts = FALSE, quietly = TRUE)


# ggplot Formatting ----------
# Updates theme_minimal so that there is borders around the graphs and the facet headings.
theme_tq() %>% theme_set()


# Variables ----------
stock_list <- tibble(symbol = c("AAPL", "NFLX", "GOOGL", "FB", "ENB", "SPY") %>% sort)
stock_num <- stock_list %>% count %>% as.numeric()
time_period <- c(from = today(), to = today() - years(5))
iterations <- 3000
max_volatility <- 0.3


# Functions ----------
weight_determine <- function(stock_list, n){
  stock_weight <- rep(0, n)
  stock_order <- sample(n)
  for (i in 1:n-1){
    stock_weight[stock_order[i]] <- runif(n = 1, min = -1, max = 1 - sum(stock_weight)) %>% round(4)
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
  stock_list$symbol %>%
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

stock_wide <- 
  stock_returns %>%
  select(symbol, R_a, date) %>% 
  spread(symbol, R_a)

stock_cov <- 
  stock_wide %>% 
  select(-date) %>% 
  cov() * 252
stock_cov

# opt_port <- 
#   lp(
#     direction = "max"
#     ,objective.in = stock_summary[, "AnnualizedReturn"]
#     ,const.mat = rbind(t(stock_summary[, "AnnualizedStdDev"]), rep(1, count(stock_list)))
#     ,const.dir = c("<=", "=")
#     ,const.rhs = c(0.3, 1)
#   )
# opt_port
# opt_port$solution


portfolio_results <- matrix(nrow = iterations, ncol = 2 + stock_num)
colnames(portfolio_results) <- c("Mean Return", "Volatility", stock_list$symbol %>% as.character())
portfolio_results %<>%
  as.tibble %>% 
  mutate_all(.funs = as.numeric)

for(i in 1:iterations){
  portfolio_weight <- weight_determine(stock_list = stock_list, n = stock_num)
  portfolio_sd <- stock_var_fun(stock_list = stock_list, stock_cov = stock_cov, stock_weight = portfolio_weight, n = stock_num) %>% sqrt
  portfolio_return <- sum(stock_summary$AnnualizedReturn * portfolio_weight)
  portfolio_results[i, ] <- c(portfolio_return, portfolio_sd, portfolio_weight)
}
portfolio_results

portfolio_results %>% 
  ggplot(
    aes(
      x = Volatility
      ,y = `Mean Return`
    ) 
  ) +
  geom_point(alpha = 0.5)

portfolio_optimal <- 
  portfolio_results %>% 
  filter(Volatility <= max_volatility) %>% 
  arrange(desc(`Mean Return`)) %>% 
  first
portfolio_optimal

# portfolio_return_ts <-

