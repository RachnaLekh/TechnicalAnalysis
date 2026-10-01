# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: macd.R
# Author: Rachna Lekh
# Description: Custom implementation of Moving Average Convergence Divergence (MACD)
#              without external financial indicator packages.
# ==============================================================================

# Ensure ema() is available, trying the script's own directory first.
# This approach is robust regardless of working directory.
.macd_script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)

if (!exists("ema")) {
  candidates <- c(
    file.path(.macd_script_dir, "ema.R"),
    "ema.R",
    file.path(getwd(), "Assignment5", "ema.R")
  )
  loaded <- FALSE
  for (f in candidates) {
    if (file.exists(f)) { source(f); loaded <- TRUE; break }
  }
  if (!loaded) stop("Could not find ema.R to source.")
}

#' Moving Average Convergence Divergence (MACD)
#'
#' Computes MACD line, signal line, and histogram using custom EMA functions.
#'
#' @param data A numeric vector representing closing prices.
#' @param short_period Fast EMA period (e.g. 12).
#' @param long_period Slow EMA period (e.g. 26).
#' @param signal_period Signal line EMA period (e.g. 9).
#' @return A named list containing macd_line, signal_line, and histogram.
macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line   = macd_line,
    signal_line = signal_line,
    histogram   = histogram
  )
  return(result)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Moving Average Convergence Divergence (MACD) Demo ---\n")
  data <- c(100, 105, 110, 115, 120, 125, 130)
  macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
  cat("MACD Line:   ", paste(round(macd_result$macd_line,   3), collapse = ", "), "\n")
  cat("Signal Line: ", paste(round(macd_result$signal_line, 3), collapse = ", "), "\n")
  cat("Histogram:   ", paste(round(macd_result$histogram,   3), collapse = ", "), "\n")
}
