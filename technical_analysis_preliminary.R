# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# Author: Rachna Lekh
# Date: Session 3, September 2026
# Description: Modular utility functions to load stock data, compute technical
#              statistics (Mean, Mode, Median, Std Dev, SMA), and visualize trends.
# ==============================================================================

# Ensure required libraries are installed and loaded
required_packages <- c("quantmod", "TTR", "dplyr", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------------------------
# 1. Custom Statistical Mode Function
# ------------------------------------------------------------------------------
#' Compute Statistical Mode of Continuous Price Series
#'
#' Evaluates the mode by rounding continuous closing prices to the nearest integer
#' and identifying the most frequent price level.
compute_mode <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(NA)
  x_rounded <- round(x_clean, 0)
  freq_table <- table(x_rounded)
  mode_val <- as.numeric(names(sort(freq_table, decreasing = TRUE)[1]))
  return(mode_val)
}

# ------------------------------------------------------------------------------
# 2. Function to Load Stock Data from Portfolio File
# ------------------------------------------------------------------------------
#' Import and Load Stock Data
#'
#' Reads portfolio symbols from a text file and retrieves historical price
#' records using quantmod from Yahoo Finance, with resilient local fallback.
load_stock_data <- function(portfolio_file = "portfolio.txt", from = "2023-01-01", to = "2023-12-31") {
  if (!file.exists(portfolio_file)) {
    stop(paste("Error: Portfolio configuration file not found at:", portfolio_file))
  }
  
  symbols <- readLines(portfolio_file)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  
  cat(sprintf("[INFO] Reading portfolio: %d active ticker(s) found.\n", length(symbols)))
  
  stock_list <- list()
  
  for (sym in symbols) {
    cat(sprintf("[LOAD] Retrieving historical data for %s... ", sym))
    success <- FALSE
    
    # Primary: Attempt online fetch via quantmod
    tryCatch({
      raw_xts <- quantmod::getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)
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
    
    # Secondary: Fallback to local structured CSV if offline
    if (!success) {
      local_csv <- file.path("output_data", paste0(sym, "_daily.csv"))
      if (file.exists(local_csv)) {
        df <- read.csv(local_csv)
        df$Date <- as.Date(df$Date)
        success <- TRUE
      }
    }
    
    if (success) {
      # Compute rolling moving averages
      df$SMA20 <- as.numeric(TTR::SMA(df$Close, n = 20))
      df$SMA50 <- as.numeric(TTR::SMA(df$Close, n = 50))
      stock_list[[sym]] <- df
      cat(sprintf("Success (%d observations)\n", nrow(df)))
    } else {
      cat("Failed.\n")
    }
  }
  
  return(stock_list)
}

# ------------------------------------------------------------------------------
# 3. Function to Compute Technical & Descriptive Statistics
# ------------------------------------------------------------------------------
#' Calculate Descriptive and Moving Average Statistics
#'
#' Takes a stock data frame and computes Mean, Mode, Median, Standard Deviation,
#' and latest 20-day and 50-day Simple Moving Averages.
calculate_statistics <- function(stock_df, ticker = "STOCK") {
  close_prices <- stock_df$Close
  
  mean_val <- mean(close_prices, na.rm = TRUE)
  mode_val <- compute_mode(close_prices)
  median_val <- median(close_prices, na.rm = TRUE)
  sd_val <- sd(close_prices, na.rm = TRUE)
  
  latest_close <- tail(close_prices, 1)
  latest_sma20 <- tail(na.omit(stock_df$SMA20), 1)
  latest_sma50 <- tail(na.omit(stock_df$SMA50), 1)
  
  stats_row <- data.frame(
    Ticker = ticker,
    Mean = round(mean_val, 2),
    Mode = round(mode_val, 2),
    Median = round(median_val, 2),
    Std_Dev = round(sd_val, 2),
    Latest_Close = round(latest_close, 2),
    Latest_SMA20 = round(latest_sma20, 2),
    Latest_SMA50 = round(latest_sma50, 2),
    stringsAsFactors = FALSE
  )
  
  return(stats_row)
}

#' Batch Compute Statistics for All Stocks
calculate_all_statistics <- function(stock_list) {
  stats_rows <- list()
  for (sym in names(stock_list)) {
    stats_rows[[sym]] <- calculate_statistics(stock_list[[sym]], ticker = sym)
  }
  consolidated <- do.call(rbind, stats_rows)
  rownames(consolidated) <- NULL
  return(consolidated)
}

# ------------------------------------------------------------------------------
# 4. Utilities to Display Output
# ------------------------------------------------------------------------------
#' Print Formatted Statistics Summary Table
display_stock_summary <- function(stats_df) {
  cat("\n=========================================================================================\n")
  cat("                TECHNICAL ANALYSIS PRELIMINARY SUMMARY - BDA400                         \n")
  cat("=========================================================================================\n")
  cat(sprintf("%-8s | %-9s | %-8s | %-9s | %-9s | %-12s | %-9s | %-9s\n",
              "Ticker", "Mean ($)", "Mode ($)", "Median($)", "StdDev($)", "Latest Close", "SMA (20)", "SMA (50)"))
  cat("-----------------------------------------------------------------------------------------\n")
  for (i in 1:nrow(stats_df)) {
    cat(sprintf("%-8s | %9.2f | %8.2f | %9.2f | %9.2f | %12.2f | %9.2f | %9.2f\n",
                stats_df$Ticker[i],
                stats_df$Mean[i],
                stats_df$Mode[i],
                stats_df$Median[i],
                stats_df$Std_Dev[i],
                stats_df$Latest_Close[i],
                stats_df$Latest_SMA20[i],
                stats_df$Latest_SMA50[i]))
  }
  cat("=========================================================================================\n\n")
}

# ------------------------------------------------------------------------------
# 5. Visualization Utility
# ------------------------------------------------------------------------------
#' Plot Price and Moving Averages
plot_stock_technicals <- function(stock_df, ticker, output_dir = file.path("output_data", "plots")) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  p <- ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Close Price"), linewidth = 1.0) +
    geom_line(aes(y = SMA20, color = "20-Day SMA"), linetype = "dashed", linewidth = 0.9) +
    geom_line(aes(y = SMA50, color = "50-Day SMA"), linetype = "dotdash", linewidth = 0.9) +
    scale_color_manual(values = c("Close Price" = "#1f77b4", "20-Day SMA" = "#ff7f0e", "50-Day SMA" = "#2ca02c")) +
    labs(
      title = paste(ticker, "- Technical Analysis & Moving Averages"),
      subtitle = "Historical Price Series with 20-Day and 50-Day Simple Moving Averages",
      x = "Date",
      y = "Price (USD)",
      color = "Indicator"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "gray30", size = 9),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  save_path <- file.path(output_dir, paste0(ticker, "_technical_analysis.png"))
  ggsave(save_path, plot = p, width = 8, height = 4.5, dpi = 300)
  cat(sprintf("[PLOT] Saved chart: %s\n", save_path))
  return(p)
}

# ------------------------------------------------------------------------------
# 6. Main Pipeline Execution
# ------------------------------------------------------------------------------
main <- function() {
  cat("[START] Initializing Technical Analysis Preliminary Pipeline...\n")
  
  # Load data
  portfolio_path <- "portfolio.txt"
  stocks <- load_stock_data(portfolio_path, from = "2023-01-01", to = "2023-12-31")
  
  # Calculate statistics
  summary_stats <- calculate_all_statistics(stocks)
  
  # Display formatted output
  display_stock_summary(summary_stats)
  
  # Save CSV output
  out_csv <- file.path("output_data", "stock_statistics_summary.csv")
  if (!dir.exists("output_data")) dir.create("output_data", recursive = TRUE)
  write.csv(summary_stats, out_csv, row.names = FALSE)
  cat(sprintf("[EXPORT] Exported statistics summary to: %s\n", out_csv))
  
  # Generate charts for portfolio
  for (sym in names(stocks)) {
    plot_stock_technicals(stocks[[sym]], ticker = sym)
  }
  
  cat("[COMPLETE] Technical analysis preliminary stage successfully completed.\n")
}

# If executed directly as script, run main()
if (!interactive()) {
  main()
}
