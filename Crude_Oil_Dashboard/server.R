# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)
library(DBI, warn.conflicts = FALSE, quietly = TRUE)
library(odbc, warn.conflicts = FALSE, quietly = TRUE)


# Variables ----------
data_path <- "Data/crude_oil_dashboard_data.xlsx"
pricing_con <- DBI::dbConnect(odbc::odbc(),
                              Driver    = "ODBC Driver 13 for SQL Server", 
                              Server    = "avm006.database.windows.net",
                              Database  = "SQLDB4",
                              UID       = "pricingdb",
                              PWD       = "Trading1!",
                              Port      = 1433)


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
summary_tbl_fun <- function(trade_data){
  blended_summary <- blended_summary_fun(trade_data)
  
  summary_tbl <- 
    trade_data %>% 
    group_by(
      Stream
      ,Platform
    ) %>%
    summarize(
      `Trade ID` = max(`Trade ID`)
      ,`Trades` = length(`Stream`)
    ) %>% 
    left_join(
      trade_data %>% 
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

stream_chart_fun <- function(chart_data, stream_input){
  chart_data %>% 
    filter(
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
    geom_line() +
    # geom_smooth(se = FALSE) +
    scale_colour_brewer(
      type = "qual"
      ,name = "Method:"
      ,palette = "Set2"
    )
}

get_pricing_data <- function(db_con){
  rbind(
    pricing_con %>%
      tbl("NEIndexTable") %>% 
      as.tibble() %>% 
      mutate(Platform = "NE")
    ,pricing_con %>%
      tbl("NGXIndexTable") %>% 
      as.tibble() %>% 
      select(
        -`Trade ID`
      ) %>% 
      mutate(Platform = "NGX") %>% 
      rename(
        `Trade ID` = TradeIDEnd
        ,`Trade Date` = Trade_Date
      )
  )
    # read_excel(data_path, "pricing_data")
}

get_monthly_blended_index_data <- function(db_con){
  pricing_con %>%
    tbl("MonthlyBlendedIndex") %>% 
    as.tibble() %>% 
    select(
      -Notes
    ) %>% 
    rename(
      `Trade Cycle` = TradeCycle
      ,`Net Volume` = VolumeM3
      ,`Index Price` = Price
    )
  # read_excel(data_path, "monthly_blended_index")
}

get_nos_dates_data <- function(db_con){
  pricing_con %>%
    tbl("NOS_Dates") %>% 
    as.tibble() %>% 
    rename(
      `Trade Cycle` = Trade_Cycle
      ,`NOS Date` = NOS_Date
    )
    # read_excel(data_path, "nos_dates")
}

get_priority_streams_data <- function(db_con){
  pricing_con %>%
    tbl("PriorityList") %>% 
    select(Stream) %>% 
    as.data.frame()
    # read_excel(data_path, "priority_streams")
}

combine_pricing_data <- function(pricing_data, nos_dates, priority_streams){
  pricing_data %>% 
    left_join(
      nos_dates
      ,by = "Trade Cycle"
      ,copy = TRUE
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
}

nos_dates <- get_nos_dates_data(pricing_con)
priority_streams <- get_priority_streams_data(pricing_con)
monthly_blended_index <- get_monthly_blended_index_data(pricing_con)


# Server ----------
shinyServer(
  function(input, output){
    # Trade Data ----------
    trade_data_complete <- reactive({
      # Pulls new trades in whenever the Refresh Date button is pressed.
      input$refresh_data
      pricing_data <- 
        get_pricing_data(pricing_con) %>% 
        combine_pricing_data(nos_dates, priority_streams)
    })
    trade_data <- reactive({
      trade_data_complete() %>% 
        # Only uses trades in the selected trade cycle and with priority streams.
        filter(
          `Trade Cycle` == as.Date(input$trade_cycle)
          ,priority == 1
          ,in_trade_cycle == 1
        )
    })
    
    # NOS Date ----------
    output$nos_date <- renderPrint(
      (nos_dates %>% 
        filter(
          `Trade Cycle` == as.Date(input$trade_cycle)
        ) %>% 
        mutate(
          `NOS Date` = `NOS Date` - hours(9)
        )
      )$`NOS Date`
    )
    # Summary Table ----------
    output$summary_tbl <- renderDataTable(
      summary_tbl <- summary_tbl_fun(trade_data())
      ,options = list(
        pageLength = priority_streams %>% count() %>% as.numeric()
        ,target = "row"
      )
    )
    
    # Price Time Series Charts ----------
    output$price_ts_charts <- renderPlot({
      if(is.null(input$summary_tbl_rows_selected)){
        ggplot() +
          geom_blank() +
          labs(
            x = "Trade Date"
            ,y = "Price"
          )
      } else{
        platform_facet <- 
          if(input$platform_split){
            facet_grid(
              Stream ~ Platform
              ,scales = "free"
            )
          } else{
            facet_wrap(
              ~ Stream
              ,scales = "free"
            )
          }
        stream_chart_fun(
          trade_data()
          ,(summary_tbl_fun(trade_data())[input$summary_tbl_rows_selected, 1] %>% as.vector())$Stream %>% sort()#input$chart_stream %>% sort()
        ) +
          platform_facet
      }
    })
    
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
    
    # Overall Volume Chart ----------
    output$overall_volume_chart <-
      renderPlot(
        trade_data() %>% 
          blended_summary_fun() %>%
          ggplot(
            aes(
              x = `Stream`
              ,y = `Overall Volume`
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
    
    # Base Trade List ----------
    output$base_trade_list <- renderDataTable({
      if(is.null(input$summary_tbl_rows_selected)){
        tibble(
          `Stream` = as.character(NA)
          ,`Trade ID` = as.numeric(NA)
          ,`Trade Data` = as.Date(NA)
          ,`Index` = as.character(NA)
          ,`Spot Price` = as.numeric(NA)
          ,`Spot Volume` = as.numeric(NA)
          ,`Net Volume` = as.numeric(NA)
          ,`Index Price` = as.numeric(NA)
          ,`Platform` = as.character(NA)
        )
      } else{
        trade_data() %>%
          select(
            -c(
              `Trade Cycle`
              ,`Spot Value`
              ,`Trade Type`
              ,`NOS Date`
              ,`priority`
              ,`in_trade_cycle`
            )
          ) %>%
          filter(
            Stream == (summary_tbl_fun(trade_data())[input$summary_tbl_rows_selected, 1] %>% as.vector())$Stream#input$base_stream
          ) %>%
          mutate(
            `Trade Date` = as.character(`Trade Date`)
          )
      }
    })
    
    # Base Monthly Index ----------
    output$base_monthly_index <- renderDataTable({
      if(is.null(input$summary_tbl_rows_selected)){
        tibble(
          `Trade Cycle` = as.Date(NA)
          ,`Stream` = as.character(NA)
          ,`Net Volume` = as.numeric(NA)
          ,`Index Price` = as.numeric(NA)
        )
      } else{
        monthly_blended_index %>% 
          filter(
            Stream == (summary_tbl_fun(trade_data())[input$summary_tbl_rows_selected, 1] %>% as.vector())$Stream#input$base_stream
          ) %>% 
          arrange(
            desc(`Trade Cycle`)
          ) %>% 
          mutate(
            `Trade Cycle` = as.character(`Trade Cycle`)
            ,`Index Price` = round(`Index Price`, 4)
          )
      }
    })
    
    # Historical Price Time Series Chart for Base ----------
    output$historical_price_base_ts <- renderPlot({
      if(is.null(input$summary_tbl_rows_selected)){
        ggplot() +
          geom_blank() +
          labs(
            x = "Trade Cycle"
            ,y = "Index Price"
          )
      } else{
        monthly_blended_index %>% 
          filter(
            Stream == (summary_tbl_fun(trade_data())[input$summary_tbl_rows_selected, 1] %>% as.vector())$Stream#input$base_stream
          ) %>% 
          ggplot(
            aes(
              x = `Trade Cycle`
              ,y = `Index Price`
              ,colour = `Stream`
            ) 
          ) +
          geom_line() +
          geom_smooth(
            se = FALSE
            # ,colour = "black"
            ,linetype = "dashed"
          ) +
          scale_colour_brewer(
            type = "qual"
            ,name = "Stream:"
            ,palette = "Set2"
          )
      }
    })
    # Historical Price Time Series Chart for WTI ----------
    output$historical_price_wti_ts <- renderPlot({
      monthly_blended_index %>% 
        filter(
          Stream == "WTI"
        ) %>% 
        rename(
          `CMA` = `Index Price`
        ) %>% 
        ggplot(
          aes(
            x = `Trade Cycle`
            ,y = `CMA`
          ) 
        ) +
        geom_line() +
        geom_smooth(
          se = FALSE
          ,colour = "black"
          ,linetype = "dashed"
        )
    })
  }
)