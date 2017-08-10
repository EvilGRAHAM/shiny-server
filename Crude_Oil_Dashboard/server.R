# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
# library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
# library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
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


# Functions ----------

summary_tab <- function(){}

blended_summary_fun <- function(summary_data){
  summary_data %>% 
    group_by(Stream) %>% 
    summarize(
      `Overall Volume` = sum(`Spot Volume`)
      ,`Overall Value` = sum(`Spot Volume` * `Spot Price`)
      ,`Index Price` = round(sum(`Spot Volume` * `Spot Price`) / sum(`Spot Volume`), 4)
      ,`Trades` = length(`Stream`)
    )
}

stream_chart_fun <- function(chart_data, platform_input, stream_input){
  chart_data %>% 
    filter(
      # Platform == platform_input
      Stream == stream_input
    ) %>%
    group_by(
      Stream
      ,`Trade Date`
      ,Platform
    ) %>% 
    select(
      `Trade Date`
      ,`Stream`
      ,`Index Price`
      ,`Spot Price`
      ,Platform
    ) %>% 
    gather(
      Method
      ,Price
      ,-`Trade Date`
      ,-`Stream`
      ,-Platform
    ) %>%
    ggplot(
      aes(
        x = `Trade Date`
        ,y = Price
        ,group = Method
        ,colour = Method
      )
    ) +
    geom_point() +
    geom_smooth(se = FALSE) +
    # facet_wrap(
    #   ~ Stream
    #   ,scales = "free"
    # ) +
    facet_grid(
      Stream ~ Platform
      ,scales = "free"
    ) +
    scale_colour_brewer(
      type = "qual"
      ,name = "Method:"
      ,palette = "Set2"
    )
}


# Data Import ----------
data_path <- "Data/crude_oil_dashboard_data.xlsx"

pricing_data <- 
  read_excel(data_path, "pricing_data")

nos_dates <- 
  read_excel(data_path, "nos_dates")

priority_streams <- 
  read_excel(data_path, "priority_streams")

pricing_data <- 
  pricing_data %>% 
  left_join(
    nos_dates
    ,by = "Trade Cycle"
  ) %>% 
  mutate(
    priority = if_else(Stream %in% priority_streams$Stream, 1, 0)
    ,in_trade_cycle = if_else(
      `Trade Date` %within% interval(`Trade Cycle` - months(1), `NOS Date`)
      ,1
      ,0
    )
  ) %>% 
  select(
    -Notes
  )

# Server ----------
shinyServer(
  function(input, output){
    
    # Trade Data ----------
    trade_data <- reactive({
      pricing_data %>% 
        filter(
          `Trade Cycle` == as.Date(input$trade_cycle)
          ,priority == 1
          ,in_trade_cycle == 1
        )
    })
    
    # Summary Table ----------
    output$summary_tbl <- renderDataTable({
      
      blended_summary <- blended_summary_fun(trade_data())
      
      summary_tbl <- 
        trade_data() %>% 
        group_by(
          Stream
          ,Platform
        ) %>%
        summarize(
          `Trade ID` = max(`Trade ID`)
          ,`Trades` = length(`Stream`)
        ) %>% 
        left_join(
          trade_data() %>% 
            ungroup() %>% 
            select(-c(Stream, Platform))
          ,by = "Trade ID"
        ) %>% 
        split(.$Platform)
      
      summary_tbl <-
        full_join(
          x = summary_tbl[[1]]
          ,y = summary_tbl[[2]]
          ,by = "Stream"
          ,suffix = c(".NE", ".NGX")
        ) %>% 
        left_join(
          y = blended_summary
          ,by = "Stream"
        ) %>%
        ungroup() %>% 
        select(
          `Stream`
          ,`Index Price`
          ,`Overall Volume`
          ,`Trades`
          # ,starts_with("Trade ID.")
          ,starts_with("Trade Date.")
          ,starts_with("Spot Price.")
          ,starts_with("Spot Volume.")
          ,starts_with("Index Price.")
          ,starts_with("Net Volume.")
          ,starts_with("Trades.")
        ) %>%
        select(
          `Stream`
          ,`Index Price`
          ,`Overall Volume`
          ,`Trades`
          ,ends_with(".NE")
          ,ends_with(".NGX")
        ) %>%
        mutate(
          `Trade Date.NE` = as.character(`Trade Date.NE`)
          ,`Trade Date.NGX` = as.character(`Trade Date.NGX`)
        ) %>% 
        arrange(
          Stream
        )
      
    }
    ,options = list(
      pageLength = priority_streams %>% count() %>% as.numeric()
    )
    
    )
    
    # Price Time Series Charts ----------
    output$price_ts_charts <- renderPlot(stream_chart_fun(trade_data(), "NE", input$chart_stream))
    
    # Blended Index Price Chart ----------
    output$blended_index_price_chart <-
      renderPlot(
        trade_data() %>% 
          blended_summary_fun() %>%
          ggplot(
            aes(
              x = `Stream`
              ,y = `Index Price`
            )
          ) +
          geom_col() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
          )
      )
    
    # Volume Split Chart ----------
    output$volume_split_chart <-
      renderPlot(
        trade_data() %>% 
          group_by(
            Stream
            ,Platform
          ) %>% 
          summarize(
            `Overall Volume` = sum(`Spot Volume`)
            ,`Overall Value` = sum(`Spot Volume` * `Spot Price`)
            ,`Index Price` = round(sum(`Spot Volume` * `Spot Price`) / sum(`Spot Volume`), 4)
            ,`Trades` = length(`Stream`)
          ) %>% 
          ggplot(
            aes(
              x = `Stream`
              ,y = `Overall Volume`
              ,colour = `Platform`
              ,fill = `Platform`
            )
          ) +
          geom_col(
            alpha = 0.75
            ,position = "fill"
          ) +
          geom_hline(
            aes(
              yintercept = 0.5
            )
            ,linetype = "dotted"
          ) +
          scale_colour_brewer(
            type = "qual"
            ,name = "Platform:"
            ,palette = "Set2"
          ) +
          scale_fill_brewer(
            type = "qual"
            ,name = "Platform:"
            ,palette = "Set2"
          ) +
          coord_flip()
      )
    
    # Trade Split Chart ----------
    output$trade_split_chart <-
      renderPlot(
        trade_data() %>% 
          group_by(
            Stream
            ,Platform
          ) %>% 
          summarize(
            `Overall Volume` = sum(`Spot Volume`)
            ,`Overall Value` = sum(`Spot Volume` * `Spot Price`)
            ,`Index Price` = round(sum(`Spot Volume` * `Spot Price`) / sum(`Spot Volume`), 4)
            ,`Trades` = length(`Stream`)
          ) %>% 
          ggplot(
            aes(
              x = `Stream`
              ,y = `Trades`
              ,colour = `Platform`
              ,fill = `Platform`
            )
          ) +
          geom_col(
            alpha = 0.75
            ,position = "fill"
          ) +
          geom_hline(
            aes(
              yintercept = 0.5
            )
            ,linetype = "dotted"
          ) +
          scale_colour_brewer(
            type = "qual"
            ,name = "Platform:"
            ,palette = "Set2"
          ) +
          scale_fill_brewer(
            type = "qual"
            ,name = "Platform:"
            ,palette = "Set2"
          ) +
          coord_flip()
      )
  }
)