# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Script: app.R
# Author: Rachna Lekh
# GitHub: https://github.com/RachnaLekh/TechnicalAnalysis
# Description: Interactive Portfolio Visualization Dashboard built with R Shiny,
#              ggplot2, and quantmod. Features dynamic date filtering, multi-asset
#              selection, candlestick/line/area charts, indicator overlays,
#              and Moving Average crossover trading signals with chart annotations.
# ==============================================================================

# Load required packages with automated checks
required_packages <- c("shiny", "ggplot2", "quantmod", "dplyr", "scales", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(quantmod)
  library(dplyr)
  library(scales)
  library(gridExtra)
})

# ------------------------------------------------------------------------------
# 1. Custom Technical Indicator Calculation Helpers
# ------------------------------------------------------------------------------

#' Calculate Simple Moving Average (SMA)
calculate_sma <- function(x, n) {
  len <- length(x)
  res <- rep(NA, len)
  if (len >= n) {
    for (i in n:len) {
      res[i] <- mean(x[(i - n + 1):i], na.rm = TRUE)
    }
  }
  return(res)
}

#' Calculate Exponential Moving Average (EMA)
calculate_ema <- function(x, n) {
  len <- length(x)
  if (len == 0) return(numeric(0))
  multiplier <- 2 / (n + 1)
  res <- numeric(len)
  res[1] <- x[1]
  for (i in 2:len) {
    res[i] <- (x[i] - res[i - 1]) * multiplier + res[i - 1]
  }
  return(res)
}

#' Calculate Relative Strength Index (RSI) using Wilder's smoothing
calculate_rsi <- function(prices, period = 14) {
  len <- length(prices)
  if (len <= period) return(rep(NA, len))
  
  diffs <- diff(prices)
  gains <- ifelse(diffs > 0, diffs, 0)
  losses <- ifelse(diffs < 0, abs(diffs), 0)
  
  avg_gain <- mean(gains[1:period], na.rm = TRUE)
  avg_loss <- mean(losses[1:period], na.rm = TRUE)
  
  rsi_values <- rep(NA, len)
  if (avg_loss == 0) {
    rsi_values[period + 1] <- 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }
  
  for (i in (period + 2):len) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  return(rsi_values)
}

#' Calculate MACD Line, Signal Line, and Histogram
calculate_macd <- function(prices, fast = 12, slow = 26, signal = 9) {
  fast_ema <- calculate_ema(prices, fast)
  slow_ema <- calculate_ema(prices, slow)
  macd_line <- fast_ema - slow_ema
  signal_line <- calculate_ema(macd_line, signal)
  histogram <- macd_line - signal_line
  return(data.frame(
    MACD = macd_line,
    Signal = signal_line,
    Hist = histogram
  ))
}

# ------------------------------------------------------------------------------
# 2. Data Acquisition Engine (Yahoo Finance with Local Fallback)
# ------------------------------------------------------------------------------
fetch_asset_data <- function(symbol, start_date, end_date) {
  cat(sprintf("[FETCH] Requesting data for %s from %s to %s\n", symbol, start_date, end_date))
  success <- FALSE
  df <- NULL
  
  # Step 1: Attempt live download via quantmod
  tryCatch({
    raw_xts <- quantmod::getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(
      Date = zoo::index(raw_xts),
      Open = as.numeric(quantmod::Op(raw_xts)),
      High = as.numeric(quantmod::Hi(raw_xts)),
      Low = as.numeric(quantmod::Lo(raw_xts)),
      Close = as.numeric(quantmod::Cl(raw_xts)),
      Volume = as.numeric(quantmod::Vo(raw_xts))
    )
    success <- TRUE
  }, error = function(e) {
    success <- FALSE
  })
  
  # Step 2: Fallback to local stored market datasets
  if (!success || is.null(df) || nrow(df) == 0) {
    candidate_paths <- c(
      file.path("..", "Assignment2", "output_data", paste0(symbol, "_daily.csv")),
      file.path("Assignment2", "output_data", paste0(symbol, "_daily.csv")),
      file.path("output_data", paste0(symbol, "_daily.csv"))
    )
    for (cp in candidate_paths) {
      if (file.exists(cp)) {
        df <- read.csv(cp)
        df$Date <- as.Date(df$Date)
        # Filter dates
        df <- df[df$Date >= as.Date(start_date) & df$Date <= as.Date(end_date), ]
        if (nrow(df) > 0) {
          success <- TRUE
          break
        }
      }
    }
  }
  
  return(df)
}

# ------------------------------------------------------------------------------
# 3. User Interface (UI) Component
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = NULL, # Standard clean Bootstrap
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Arial, sans-serif; background-color: #f7f9fc; }
      .well { background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.06); }
      .main-header { padding: 15px 20px; background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%); color: white; border-radius: 8px; margin-bottom: 20px; }
      .main-header h2 { margin: 0 0 5px 0; font-weight: 600; font-size: 24px; }
      .main-header p { margin: 0; font-size: 13px; opacity: 0.9; }
      .status-pill { display: inline-block; padding: 4px 10px; border-radius: 4px; font-weight: bold; font-size: 12px; }
      .pill-buy { background-color: #e6f7ed; color: #0d8a4e; border: 1px solid #a3e2bb; }
      .pill-sell { background-color: #fde8e8; color: #d02525; border: 1px solid #f8b4b4; }
      .pill-hold { background-color: #f0f3f6; color: #4a5568; border: 1px solid #cbd5e1; }
      .metric-box { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px; text-align: center; }
      .metric-title { font-size: 11px; text-transform: uppercase; color: #64748b; font-weight: bold; }
      .metric-value { font-size: 18px; font-weight: bold; color: #1e293b; margin-top: 4px; }
    "))
  ),
  
  div(class = "main-header",
    h2("BDA400: Technical Analysis & Portfolio Visualization Dashboard"),
    p("Student: Rachna Lekh | Assignment 6 (Visualization Phase) | CDI College Data Analytics")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Asset selector
      selectInput("stock_symbol", "Select Asset / Ticker:",
                  choices = c("AAPL - Apple Inc." = "AAPL",
                              "MSFT - Microsoft Corp." = "MSFT",
                              "GOOGL - Alphabet Inc." = "GOOGL",
                              "AMZN - Amazon.com Inc." = "AMZN",
                              "TSLA - Tesla Inc." = "TSLA"),
                  selected = "AAPL"),
      
      # Date range input as mandated in Step 2
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01",
                     end = "2023-12-31",
                     min = "2022-01-01",
                     max = "2024-12-31"),
      
      # Time Frame selector
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly"),
                  selected = "Daily"),
      
      # Chart Type selector
      selectInput("chart_type", "Select Chart Type:",
                  choices = c("Candlestick", "Line graph", "Area chart"),
                  selected = "Candlestick"),
      
      hr(),
      # Indicator toggles
      h4(strong("Technical Indicators")),
      checkboxGroupInput("technical_indicators", "Overlay Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages")),
      
      hr(),
      # Trading rules toggle
      h4(strong("Trading Strategy Engine")),
      checkboxInput("show_signals", "Display MA Crossover Signals", value = TRUE),
      p(style = "font-size: 11px; color: #64748b;",
        "Trading Rule: Buy when 20-day SMA crosses above 50-day SMA; Sell when 20-day SMA crosses below; otherwise Hold.")
    ),
    
    mainPanel(
      width = 9,
      # Top Performance Metrics
      fluidRow(
        column(3, div(class = "metric-box",
                      div(class = "metric-title", "Latest Close Price"),
                      div(class = "metric-value", textOutput("metric_price")))),
        column(3, div(class = "metric-box",
                      div(class = "metric-title", "Current 20-Day SMA"),
                      div(class = "metric-value", textOutput("metric_sma20")))),
        column(3, div(class = "metric-box",
                      div(class = "metric-title", "Current 50-Day SMA"),
                      div(class = "metric-value", textOutput("metric_sma50")))),
        column(3, div(class = "metric-box",
                      div(class = "metric-title", "Active Trading Signal"),
                      div(class = "metric-value", uiOutput("metric_signal"))))
      ),
      br(),
      # Main Stock Chart
      plotOutput("stock_chart", height = "440px"),
      br(),
      # Secondary Indicator Panel (renders conditionally)
      uiOutput("secondary_panels_ui"),
      br(),
      # Historical Trading Signal Log
      h4(strong("Recent Trading Signal Activity")),
      tableOutput("signal_table")
    )
  )
)

# ------------------------------------------------------------------------------
# 4. Reactive Server Component
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive data retrieval
  stock_data <- reactive({
    req(input$stock_symbol, input$date_range)
    df <- fetch_asset_data(input$stock_symbol, input$date_range[1], input$date_range[2])
    validate(
      need(!is.null(df) && nrow(df) > 0, "No market data available for the specified range. Please adjust dates.")
    )
    
    # Handle Time Frame aggregation
    if (input$time_frame == "Weekly") {
      df$Period <- format(df$Date, "%Y-W%W")
      df <- df %>%
        group_by(Period) %>%
        summarise(
          Date = max(Date),
          Open = first(Open),
          High = max(High),
          Low = min(Low),
          Close = last(Close),
          Volume = sum(Volume, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(Date)
    } else if (input$time_frame == "Monthly") {
      df$Period <- format(df$Date, "%Y-%m")
      df <- df %>%
        group_by(Period) %>%
        summarise(
          Date = max(Date),
          Open = first(Open),
          High = max(High),
          Low = min(Low),
          Close = last(Close),
          Volume = sum(Volume, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(Date)
    }
    
    # Calculate indicators
    df$SMA20 <- calculate_sma(df$Close, 20)
    df$SMA50 <- calculate_sma(df$Close, 50)
    df$RSI   <- calculate_rsi(df$Close, 14)
    macd_res <- calculate_macd(df$Close, 12, 26, 9)
    df$MACD_Line   <- macd_res$MACD
    df$Signal_Line <- macd_res$Signal
    df$Histogram   <- macd_res$Hist
    
    # Trading Rules: MA Crossover
    # Step 4: short_ma > long_ma -> Buy, short_ma < long_ma -> Sell, else Hold
    signals <- ifelse(is.na(df$SMA20) | is.na(df$SMA50), "Hold",
               ifelse(df$SMA20 > df$SMA50, "Buy",
               ifelse(df$SMA20 < df$SMA50, "Sell", "Hold")))
    df$Signal <- signals
    
    # Detect exact crossover transition points for annotations
    df$CrossoverEvent <- "None"
    if (nrow(df) >= 2) {
      for (k in 2:nrow(df)) {
        if (!is.na(df$SMA20[k]) && !is.na(df$SMA50[k]) && !is.na(df$SMA20[k-1]) && !is.na(df$SMA50[k-1])) {
          if (df$SMA20[k] > df$SMA50[k] && df$SMA20[k-1] <= df$SMA50[k-1]) {
            df$CrossoverEvent[k] <- "Golden Cross (Buy)"
          } else if (df$SMA20[k] < df$SMA50[k] && df$SMA20[k-1] >= df$SMA50[k-1]) {
            df$CrossoverEvent[k] <- "Death Cross (Sell)"
          }
        }
      }
    }
    
    return(df)
  })
  
  # Top Metric Renderers
  output$metric_price <- renderText({
    df <- stock_data()
    sprintf("$%.2f", tail(df$Close, 1))
  })
  output$metric_sma20 <- renderText({
    df <- stock_data()
    v <- tail(df$SMA20, 1)
    if (is.na(v)) "N/A" else sprintf("$%.2f", v)
  })
  output$metric_sma50 <- renderText({
    df <- stock_data()
    v <- tail(df$SMA50, 1)
    if (is.na(v)) "N/A" else sprintf("$%.2f", v)
  })
  output$metric_signal <- renderUI({
    df <- stock_data()
    sig <- tail(df$Signal, 1)
    pill_class <- switch(sig, "Buy" = "pill-buy", "Sell" = "pill-sell", "pill-hold")
    span(class = paste("status-pill", pill_class), sig)
  })
  
  # Main Stock Price Chart
  output$stock_chart <- renderPlot({
    df <- stock_data()
    
    # Base ggplot setup
    p <- ggplot(data = df, aes(x = Date))
    
    # Apply selected Chart Type
    if (input$chart_type == "Line graph") {
      p <- p + geom_line(aes(y = Close), color = "#2563eb", linewidth = 1.0)
    } else if (input$chart_type == "Area chart") {
      p <- p + geom_area(aes(y = Close), fill = "#3b82f6", alpha = 0.35, color = "#1d4ed8", linewidth = 0.8)
    } else if (input$chart_type == "Candlestick") {
      candle_width <- as.numeric(difftime(max(df$Date), min(df$Date), units = "days")) / (nrow(df) * 1.5)
      candle_width <- max(0.4, min(candle_width, 1.5))
      
      # High-Low wick line
      p <- p + geom_segment(aes(x = Date, xend = Date, y = Low, yend = High, color = Close >= Open), linewidth = 0.5)
      # Open-Close candle body
      p <- p + geom_rect(aes(xmin = Date - candle_width, xmax = Date + candle_width,
                             ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                             fill = Close >= Open, color = Close >= Open), linewidth = 0.3)
      p <- p + scale_fill_manual(values = c("TRUE" = "#16a34a", "FALSE" = "#dc2626"), guide = "none")
      p <- p + scale_color_manual(values = c("TRUE" = "#16a34a", "FALSE" = "#dc2626"), guide = "none")
    }
    
    # Step 3: Overlay Moving Average Technical Indicators if toggled
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = SMA20, color = "SMA 20"), linewidth = 0.9, na.rm = TRUE)
      p <- p + geom_line(aes(y = SMA50, color = "SMA 50"), linewidth = 0.9, linetype = "dashed", na.rm = TRUE)
    }
    
    # Step 4: Annotate bars on the chart with trading signals
    if (input$show_signals) {
      crossover_points <- df[df$CrossoverEvent != "None", ]
      if (nrow(crossover_points) > 0) {
        p <- p + geom_point(data = crossover_points,
                            aes(x = Date, y = Close, shape = CrossoverEvent),
                            size = 4, color = "#0f172a", fill = ifelse(crossover_points$CrossoverEvent == "Golden Cross (Buy)", "#22c55e", "#ef4444"))
        p <- p + geom_text(data = crossover_points,
                           aes(x = Date, y = Close, label = CrossoverEvent),
                           vjust = ifelse(crossover_points$CrossoverEvent == "Golden Cross (Buy)", -1.2, 1.8),
                           fontface = "bold", size = 3.6, color = "#1e293b")
        p <- p + scale_shape_manual(values = c("Golden Cross (Buy)" = 24, "Death Cross (Sell)" = 25), name = "Trading Signals")
      }
    }
    
    # Styling and Aesthetics
    p <- p + labs(
      title = paste(input$stock_symbol, "-", input$chart_type, "(", input$time_frame, "View )"),
      subtitle = "Price trend with technical overlays and automated crossover trade annotations",
      x = "Date",
      y = "Price (USD)"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", color = "#1e293b", size = 15),
      plot.subtitle = element_text(color = "#64748b", size = 11),
      axis.title = element_text(face = "bold", color = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.5),
      legend.position = "top"
    )
    
    print(p)
  })
  
  # Dynamic UI for Secondary Indicator Panels (RSI & MACD)
  output$secondary_panels_ui <- renderUI({
    panels <- list()
    if ("RSI" %in% input$technical_indicators) {
      panels[[length(panels) + 1]] <- h4(strong("Relative Strength Index (RSI - 14 Days)"))
      panels[[length(panels) + 1]] <- plotOutput("rsi_chart", height = "180px")
    }
    if ("MACD" %in% input$technical_indicators) {
      panels[[length(panels) + 1]] <- h4(strong("Moving Average Convergence Divergence (MACD 12, 26, 9)"))
      panels[[length(panels) + 1]] <- plotOutput("macd_chart", height = "200px")
    }
    tagList(panels)
  })
  
  # RSI Panel Plot
  output$rsi_chart <- renderPlot({
    df <- stock_data()
    p_rsi <- ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "#8b5cf6", linewidth = 0.9, na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "#ef4444", linewidth = 0.7) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "#10b981", linewidth = 0.7) +
      annotate("text", x = min(df$Date), y = 73, label = "Overbought (70)", color = "#ef4444", hjust = 0, size = 3.2) +
      annotate("text", x = min(df$Date), y = 33, label = "Oversold (30)", color = "#10b981", hjust = 0, size = 3.2) +
      ylim(0, 100) +
      labs(x = NULL, y = "RSI Index") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    print(p_rsi)
  })
  
  # MACD Panel Plot
  output$macd_chart <- renderPlot({
    df <- stock_data()
    p_macd <- ggplot(df, aes(x = Date)) +
      geom_col(aes(y = Histogram, fill = Histogram > 0), alpha = 0.7, show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "#22c55e", "FALSE" = "#ef4444")) +
      geom_line(aes(y = MACD_Line, color = "MACD Line"), linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = Signal_Line, color = "Signal Line"), linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
      scale_color_manual(values = c("MACD Line" = "#0284c7", "Signal Line" = "#f97316"), name = NULL) +
      labs(x = "Date", y = "MACD") +
      theme_minimal() +
      theme(legend.position = "top", panel.grid.minor = element_blank())
    print(p_macd)
  })
  
  # Signal Log Table
  output$signal_table <- renderTable({
    df <- stock_data()
    signals_subset <- df[df$CrossoverEvent != "None", c("Date", "Close", "SMA20", "SMA50", "CrossoverEvent")]
    if (nrow(signals_subset) == 0) {
      return(data.frame(Status = "No crossover events detected in the selected date range."))
    }
    signals_subset$Date <- format(signals_subset$Date, "%Y-%m-%d")
    signals_subset$Close <- sprintf("$%.2f", signals_subset$Close)
    signals_subset$SMA20 <- sprintf("$%.2f", signals_subset$SMA20)
    signals_subset$SMA50 <- sprintf("$%.2f", signals_subset$SMA50)
    colnames(signals_subset) <- c("Date", "Close Price", "20-Day SMA", "50-Day SMA", "Signal Triggered")
    return(signals_subset)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run Shiny Application
shinyApp(ui = ui, server = server)
