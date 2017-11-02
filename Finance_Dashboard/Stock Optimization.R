rm(list=ls())
# Libraries ----------
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lpSolve, warn.conflicts = FALSE, quietly = TRUE)
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
iterations <- 3000
max_volatility <- 0.4084344


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

stock_wide <- 
  stock_returns %>%
  select(symbol, R_a, date) %>% 
  spread(symbol, R_a)

stock_cov <- 
  stock_wide %>% 
  select(-date) %>% 
  cov() * 252
stock_cov


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

# portfolio_results %>% 
#   plot_ly(
#     x = ~Volatility
#     ,y = ~`Mean Return`
#     ,text = ~ for (i in 3:8){paste0(colnames(portfolio_results)[i], ": ", portfolio_results[, i] %>% as.numeric)}
#     ,type = "scatter"
#     ,mode = "markers"
#   )

portfolio_results %>% 
  plot_ly(
    x = ~Volatility
    ,y = ~`Mean Return`
    ,text = ~ paste(
      "AAPL:", AAPL
      ,"<br>FB:", FB
      ,"<br>GOOGL:", GOOGL
      ,"<br>NFLX:", NFLX)
    ,type = "scatter"
    ,mode = "markers"
  )

portfolio_optimal <- 
  portfolio_results %>% 
  filter(Volatility <= max_volatility) %>% 
  arrange(desc(`Mean Return`)) %>% 
  first
portfolio_optimal

# portfolio_return_ts <-

library(quadprog)
risk_tolerance <- 0.5
port_optim <- function(risk_tolerance){
  bvec <- c(1, rep(0, stock_num))#, rep(-0.15, stock_num))
  Dmat <- 2*stock_cov
  dvec <- 
    stock_summary %>%
    ungroup() %>% 
    select(AnnualizedReturn) %>% 
    as.matrix*risk_tolerance
  Amat <- matrix(nrow = stock_num, ncol = stock_num + 1)
  # Weights sum to 1
  Amat[, 1] <- 1
  # Weights are non-negative
  Amat[, 2:(stock_num + 1)] <- diag(x = 1, nrow = stock_num)
  # Amat[, (stock_num + 2):(2*stock_num + 1)] <- diag(x = -1, nrow = stock_num)
  Amat
  ef_port <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  ef_port
  sum(ef_port$solution)
  port_return <- sum(ef_port$solution*dvec)/risk_tolerance
  port_sd <- stock_var_fun(stock_list = stock_list, stock_cov = stock_cov, stock_weight = ef_port$solution, n = stock_num) %>% sqrt
  port_summary <- tibble(port_return, port_sd)
  port_summary
}
port_optim(0.5)
results <- tibble(risk_tolerance = (1:100)/100)
results %>% 
  mutate(
    port_return = mean(risk_tolerance)#port_optim(risk_tolerance = risk_tolerance) %>% select(port_return) %>%  as.numeric()
  )
# ggplot(data = tibble(risk_tolerance = c(0.1,3))) +
#   stat_function(data = port_optim, aes(y = port_return), fun = port_optim)
test <- tibble(risk_tolerance = as.numeric(), port_return = as.numeric(), port_sd = as.numeric())
for (i in 1:50){
  test[i,] <- c(i/100, port_optim(i/100))
}
ggplot(test, aes(x = port_sd, y = port_return)) +
  geom_line()
