# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: stoch_rsi.R
# Author: Rachna Lekh
# Description: Custom implementation of Stochastic RSI (StochRSI) from scratch
#              without external financial indicator packages.
# ==============================================================================

# Ensure dependencies are available, trying the script's own directory first.
# This approach is robust regardless of working directory.
.stoch_rsi_script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)

if (!exists("rsi")) {
  candidates <- c(
    file.path(.stoch_rsi_script_dir, "rsi.R"),
    "rsi.R",
    file.path(getwd(), "Assignment5", "rsi.R")
  )
  loaded <- FALSE
  for (f in candidates) {
    if (file.exists(f)) { source(f); loaded <- TRUE; break }
  }
  if (!loaded) stop("Could not find rsi.R to source.")
}

if (!exists("sma")) {
  candidates <- c(
    file.path(.stoch_rsi_script_dir, "sma.R"),
    "sma.R",
    file.path(getwd(), "Assignment5", "sma.R")
  )
  loaded <- FALSE
  for (f in candidates) {
    if (file.exists(f)) { source(f); loaded <- TRUE; break }
  }
  if (!loaded) stop("Could not find sma.R to source.")
}

#' Stochastic RSI (StochRSI)
#'
#' Computes the Stochastic Relative Strength Index (%K and %D lines) normalized between 0 and 1.
#'
#' Design note: This implementation uses global min/max normalization over all valid RSI values,
#' consistent with the assignment pseudocode. Standard (rolling-window) StochRSI implementations
#' use a rolling min/max over the last `period` RSI values instead; that variant gives different
#' numbers and is NOT what the pseudocode specifies.
#'
#' If data is shorter than or equal to period, all RSI values are NA and both k_line and d_line
#' are returned as all-NA vectors of length(data) without error.
#'
#' @param data A numeric vector representing historical prices.
#' @param period The period for the underlying RSI calculation (e.g. 14).
#' @param k_period The smoothing period for the %K line (e.g. 3).
#' @param d_period The smoothing period for the %D line (e.g. 3).
#' @return A named list containing k_line and d_line vectors, each of length(data).
stoch_rsi <- function(data, period, k_period, d_period) {
  n <- length(data)

  # Calculate the RSI (returns length-n vector with NAs for first `period` positions)
  rsi_values <- rsi(data, period)

  # If no valid RSI values exist (e.g. data shorter than or equal to period), return all NAs
  valid_indices <- which(!is.na(rsi_values))
  if (length(valid_indices) == 0) {
    return(list(k_line = rep(NA, n), d_line = rep(NA, n)))
  }

  # Global min/max normalization of RSI (pseudocode approach)
  min_rsi <- min(rsi_values[valid_indices])
  max_rsi <- max(rsi_values[valid_indices])

  # Build full-length normalized vector (NA where RSI is NA)
  normalized <- rep(NA, n)
  if (max_rsi == min_rsi) {
    normalized[valid_indices] <- 0.5
  } else {
    normalized[valid_indices] <- (rsi_values[valid_indices] - min_rsi) / (max_rsi - min_rsi)
  }

  # --- %K line: SMA(k_period) of the normalized values ---
  # sma() returns n_valid - k_period + 1 values (no NA padding).
  # We place them back into a full-length vector, leaving the first
  # k_period - 1 valid positions as NA.
  valid_norm_idx <- which(!is.na(normalized))
  valid_norm     <- normalized[valid_norm_idx]

  k_line <- rep(NA, n)
  if (length(valid_norm) >= k_period) {
    k_sma <- sma(valid_norm, k_period)
    # k_sma[1] corresponds to valid_norm_idx[k_period], k_sma[j] to valid_norm_idx[k_period + j - 1]
    for (j in seq_along(k_sma)) {
      k_line[valid_norm_idx[k_period + j - 1]] <- k_sma[j]
    }
  }

  # --- %D line: SMA(d_period) of the non-NA %K values ---
  valid_k_idx <- which(!is.na(k_line))
  valid_k     <- k_line[valid_k_idx]

  d_line <- rep(NA, n)
  if (length(valid_k) >= d_period) {
    d_sma <- sma(valid_k, d_period)
    for (j in seq_along(d_sma)) {
      d_line[valid_k_idx[d_period + j - 1]] <- d_sma[j]
    }
  }

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Stochastic RSI (StochRSI) Demo ---\n")
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 64, 68, 70, 72, 75, 73, 78, 80)
  stoch_rsi_result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
  cat("%K Line: ", paste(round(stoch_rsi_result$k_line, 3), collapse = ", "), "\n")
  cat("%D Line: ", paste(round(stoch_rsi_result$d_line, 3), collapse = ", "), "\n")

  cat("\nEdge case - data shorter than RSI period (should return all NA, no error):\n")
  short_result <- stoch_rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62),
                             period = 14, k_period = 3, d_period = 3)
  cat("%K Line: ", paste(short_result$k_line, collapse = ", "), "\n")
  cat("%D Line: ", paste(short_result$d_line, collapse = ", "), "\n")
}
