# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: test_all_indicators.R
# Author: Rachna Lekh
# Description: Comprehensive test suite for all 9 custom technical indicators.
#              Uses PASS/FAIL checks against hand-verified expected values,
#              covers textbook examples, NA-input edge cases, error cases, and
#              real market data if available.
#
# Run from repo root:      Rscript Assignment5/test_all_indicators.R
# Run from Assignment5/:   Rscript test_all_indicators.R
# ==============================================================================

cat("==============================================================================\n")
cat(" BDA400 Assignment 5: Technical Analysis Indicators Test Suite\n")
cat(" Student: Rachna Lekh (GitHub: RachnaLekh)\n")
cat("==============================================================================\n\n")

# ---------------------------------------------------------------------------
# Helper: determine script directory (works from repo root or Assignment5/)
# ---------------------------------------------------------------------------
script_dir <- local({
  base <- getwd()
  if (basename(base) == "Assignment5") "." else "Assignment5"
})

# ---------------------------------------------------------------------------
# Helper: PASS/FAIL checker
# ---------------------------------------------------------------------------
pass_count <- 0L
fail_count <- 0L
skip_count <- 0L

check <- function(label, actual, expected, tol = 1e-2) {
  # actual and expected can be numeric vectors (compared element-wise within tol),
  # or character vectors (compared exactly), or scalars.
  if (length(actual) != length(expected)) {
    cat(sprintf("  [FAIL] %s\n         Length mismatch: got %d, expected %d\n",
                label, length(actual), length(expected)))
    fail_count <<- fail_count + 1L
    return(invisible(FALSE))
  }
  # Element-wise comparison
  numeric_check <- is.numeric(actual) && is.numeric(expected)
  if (numeric_check) {
    na_match <- is.na(actual) == is.na(expected)
    val_match <- ifelse(is.na(actual) | is.na(expected), TRUE,
                        abs(actual - expected) <= tol)
    ok <- all(na_match & val_match)
  } else {
    ok <- all(actual == expected)
  }
  if (ok) {
    cat(sprintf("  [PASS] %s\n", label))
    pass_count <<- pass_count + 1L
  } else {
    cat(sprintf("  [FAIL] %s\n         Got:      %s\n         Expected: %s\n",
                label,
                paste(if (is.numeric(actual)) round(actual, 4) else actual, collapse = ", "),
                paste(if (is.numeric(expected)) round(expected, 4) else expected, collapse = ", ")))
    fail_count <<- fail_count + 1L
  }
  invisible(ok)
}

check_error <- function(label, expr) {
  # Verify that expr signals an error
  result <- tryCatch(expr, error = function(e) "ERROR")
  if (identical(result, "ERROR")) {
    cat(sprintf("  [PASS] %s (correctly raised an error)\n", label))
    pass_count <<- pass_count + 1L
  } else {
    cat(sprintf("  [FAIL] %s (expected an error but got: %s)\n", label, result))
    fail_count <<- fail_count + 1L
  }
}

# ---------------------------------------------------------------------------
# Step 1: Source all indicator scripts
# ---------------------------------------------------------------------------
cat("[STEP 1] Sourcing all 9 indicator scripts...\n")
indicator_files <- c(
  "sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
  "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R"
)

for (f in indicator_files) {
  full_path <- file.path(script_dir, f)
  if (file.exists(full_path)) {
    source(full_path)
    cat(sprintf("  Loaded: %s\n", f))
  } else {
    stop(paste("Failed to locate script:", full_path))
  }
}
cat("\n")

# ===========================================================================
# Step 2: Textbook benchmark tests (from PDF specification)
# Expected values independently verified in Python (see verify_indicators.py).
# ===========================================================================
cat("[STEP 2] Textbook Benchmark Tests (PDF Specifications)\n")
cat("------------------------------------------------------------------------------\n")

# --- (1) SMA ---
cat("(1) SMA: data=[10,12,15,20,18,22,25,24,21], period=3\n")
data_sma <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
res_sma  <- sma(data_sma, period = 3)
# sma() returns n-period+1 = 7 values with no NA padding
expected_sma <- c(12.333, 15.667, 17.667, 20.000, 21.667, 23.667, 23.333)
check("SMA length = 7 (n-period+1)", length(res_sma), 7L)
check("SMA values", res_sma, expected_sma, tol = 0.001)
# Padded form (for callers that need length-n)
padded_sma <- c(rep(NA, 3 - 1), res_sma)
check("SMA padded length = 9", length(padded_sma), 9L)
cat("\n")

# --- (2) EMA ---
cat("(2) EMA: data=[10,12,15,20,18,22,25,24,21], period=3\n")
data_ema <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
res_ema  <- ema(data_ema, period = 3)
# multiplier = 2/(3+1) = 0.5; seed = data[1] = 10
expected_ema <- c(10.0000, 11.0000, 13.0000, 16.5000, 17.2500, 19.6250, 22.3125, 23.1563, 22.0781)
check("EMA values", res_ema, expected_ema, tol = 0.001)
cat("\n")

# --- (3) MACD ---
cat("(3) MACD: data=[100,105,110,115,120,125,130], short=3, long=5, signal=2\n")
data_macd  <- c(100, 105, 110, 115, 120, 125, 130)
res_macd   <- macd(data_macd, short_period = 3, long_period = 5, signal_period = 2)
exp_macd   <- c(0.0000,  0.8333, 1.8056, 2.6620, 3.3372, 3.8394, 4.2002)
exp_signal <- c(0.0000,  0.5556, 1.3889, 2.2377, 2.9707, 3.5498, 3.9834)
exp_hist   <- c(0.0000,  0.2778, 0.4167, 0.4244, 0.3665, 0.2896, 0.2168)
check("MACD line",   res_macd$macd_line,   exp_macd,   tol = 0.001)
check("Signal line", res_macd$signal_line, exp_signal, tol = 0.001)
check("Histogram",   res_macd$histogram,   exp_hist,   tol = 0.001)
cat("\n")

# --- (4) StDev ---
cat("(4) StDev: data=[10,12,15,20,18,22,25,24,21]\n")
data_sd <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
res_sd  <- stdev(data_sd)
# mean=18.5556, sum_sq_dev=220.2222, pop_stdev=sqrt(220.2222/9)=4.9466
check("StDev value", res_sd, 4.9466, tol = 0.001)
cat("\n")

# --- (5) LinReg ---
cat("(5) LinReg: data=[100..135], regressionLength=6, regressionOffset=1\n")
data_lr    <- c(100, 105, 108, 112, 115, 120, 122, 128, 130, 135)
res_lr     <- linreg(regressionSource = data_lr, regressionLength = 6, regressionOffset = 1)
# R pseudocode: start_index = max(1, n - regressionLength + regressionOffset)
#             = max(1, 10 - 6 + 1) = 5 (1-based)
# end_index   = min(n, n - regressionOffset) = min(10, 10-1) = 9
# subset: data[5:9] (1-based) = [115, 120, 122, 128, 130] — 5 elements
# slope=3.8, intercept=111.6, predicted=[115.4, 119.2, 123.0, 126.8, 130.6]
check("LinReg slope",     res_lr$slope,     3.8,   tol = 0.001)
check("LinReg intercept", res_lr$intercept, 111.6, tol = 0.001)
check("LinReg predicted", res_lr$predicted_values, c(115.4, 119.2, 123.0, 126.8, 130.6), tol = 0.01)
cat("\n")

# --- (6) RSI ---
cat("(6) RSI: data=[45,50,48,55,52,49,58,60,65,62], period=5\n")
data_rsi <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
res_rsi  <- rsi(data_rsi, period = 5)
# Pseudocode: loop from i=period+1 using gains[i-1] (1-based); gives different values
# than textbook Wilder (which seeds first RSI without smoothing).
expected_rsi <- c(NA, NA, NA, NA, NA, 50.53, 68.93, 71.84, 78.21, 66.86)
check("RSI values", res_rsi, expected_rsi, tol = 0.01)
cat("\n")

# --- (7) StochRSI ---
cat("(7) StochRSI: data=[45..80, 18 elements], period=5, k=3, d=3\n")
data_stoch <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 64, 68, 70, 72, 75, 73, 78, 80)
res_stoch  <- stoch_rsi(data_stoch, period = 5, k_period = 3, d_period = 3)
# K line: NAs for indices 1-7, then 0.3781, 0.6416, 0.6220, 0.6086, 0.5944, 0.7142, 0.825, 0.909, 0.854, 0.8498, 0.8356
# D line: NAs for indices 1-9, then 0.5472, 0.6241, 0.6083, 0.6391, 0.7112, 0.8161, 0.8627, 0.871, 0.8465
expected_k <- c(NA,NA,NA,NA,NA,NA,NA, 0.3781,0.6416,0.6220,0.6086,0.5944,0.7142,0.825,0.909,0.854,0.8498,0.8356)
expected_d <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA, 0.5472,0.6241,0.6083,0.6391,0.7112,0.8161,0.8627,0.871,0.8465)
check("StochRSI k_line", res_stoch$k_line, expected_k, tol = 0.001)
check("StochRSI d_line", res_stoch$d_line, expected_d, tol = 0.001)
cat("\n")

# --- (8) Crossover ---
cat("(8) Crossover: arr1=[10,12,15,20,18,22,25,24,21], arr2=[18,20,22,18,15,12,10,11,13]\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
res_cross <- crossover(arr1, arr2)
expected_cross <- c("None","None","None","Up","None","None","None","None","None")
check("Crossover signals", res_cross, expected_cross)
cat("\n")

# --- (9) Crossunder ---
cat("(9) Crossunder: arr1=[10,12,15,20,18,22,25,24,21], arr2=[18,20,22,18,15,12,10,11,13]\n")
res_under <- crossunder(arr1, arr2)
expected_under <- c("None","False","False","False","False","False","False","False","False")
check("Crossunder signals", res_under, expected_under)
cat("\n")

# ===========================================================================
# Step 3: NA-input edge cases
# ===========================================================================
cat("[STEP 3] NA-Input Edge Cases\n")
cat("------------------------------------------------------------------------------\n")

# crossover with NA values (e.g. from padded SMA)
cat("(3a) Crossover with leading NAs (simulating sma(close,20) vs sma(close,50)):\n")
arr1_na <- c(NA, NA, NA, 15.5, 16.0, 17.0, 16.5)
arr2_na <- c(NA, NA, NA, NA,   NA,   16.5, 16.0)
res_co_na <- crossover(arr1_na, arr2_na)
# index 7 (0-based 6): arr1=16.5>16.0=arr2, arr1_prev=17.0>16.5=arr2_prev -> none (arr1 was already > arr2_prev... wait arr2_prev=NA -> None)
# all positions with any NA input must give "None"
expected_co_na <- c("None","None","None","None","None","None","None")
check("Crossover NA-safe: all None when NAs present", res_co_na, expected_co_na)
cat("\n")

cat("(3b) Crossunder with leading NAs:\n")
res_cu_na <- crossunder(arr1_na, arr2_na)
# positions 2..6 should be "False" (not "None"), position 1 is "None"
expected_cu_na <- c("None","False","False","False","False","False","False")
check("Crossunder NA-safe: False (not True) when NAs present", res_cu_na, expected_cu_na)
cat("\n")

cat("(3c) Crossover/Crossunder with no crash on mixed NA/real data:\n")
# This mimics crossover(sma(close,20), sma(close,50)) which previously crashed
close_test <- c(100,102,104,103,105,107,108,110,109,112,115,114,116,118,120,
                119,121,123,125,124,126,128,130,132,134)
sma20 <- c(rep(NA, 19), sma(close_test, 20))
sma50_raw <- close_test  # too short for sma50; we'll just test the NA case
sma50 <- rep(NA, length(close_test))
res_co_mixed <- crossover(sma20, sma50)
check("Crossover all-NA second array: returns all None", all(res_co_mixed == "None"), TRUE)
cat("\n")

# ===========================================================================
# Step 4: Error condition tests
# ===========================================================================
cat("[STEP 4] Error Condition Tests\n")
cat("------------------------------------------------------------------------------\n")

check_error("SMA period > length",
  sma(c(1,2,3), period = 5))

check_error("Crossover mismatched lengths",
  crossover(c(1,2,3), c(1,2)))

check_error("Crossunder mismatched lengths",
  crossunder(c(1,2,3), c(1,2)))

check_error("LinReg regressionLength > data length",
  linreg(c(1,2,3), regressionLength = 5, regressionOffset = 0))

check_error("LinReg regressionOffset >= regressionLength",
  linreg(c(1,2,3,4,5), regressionLength = 3, regressionOffset = 3))

cat("\n")

# ===========================================================================
# Step 5: StochRSI short-data edge case
# ===========================================================================
cat("[STEP 5] StochRSI short-data edge case\n")
cat("------------------------------------------------------------------------------\n")

cat("(5a) stoch_rsi on 10 elements with period=14 (data shorter than RSI period):\n")
short_data   <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
short_stoch  <- stoch_rsi(short_data, period = 14, k_period = 3, d_period = 3)
check("StochRSI short data: k_line all NA",
      all(is.na(short_stoch$k_line)), TRUE)
check("StochRSI short data: d_line all NA",
      all(is.na(short_stoch$d_line)), TRUE)
check("StochRSI short data: k_line length = 10",
      length(short_stoch$k_line), 10L)
cat("\n")

# ===========================================================================
# Step 6: Real market data test (AAPL)
# ===========================================================================
cat("[STEP 6] Real Market Data Test (Apple Inc.)\n")
cat("------------------------------------------------------------------------------\n")

# Look for AAPL raw price CSV inside the repo (not an external path)
aapl_candidates <- c(
  file.path(script_dir, "output_data", "AAPL_daily.csv"),
  file.path(script_dir, "data", "AAPL_daily.csv")
)
aapl_csv <- Filter(file.exists, aapl_candidates)

if (length(aapl_csv) == 0) {
  cat("  [WARNING] AAPL_daily.csv not found in Assignment5/output_data/ or Assignment5/data/\n")
  cat("            Skipping real-market section. Download genuine price data from\n")
  cat("            Yahoo Finance (finance.yahoo.com, ticker AAPL, 2023-01-01 to 2023-12-29)\n")
  cat("            and place the CSV as Assignment5/output_data/AAPL_daily.csv\n")
  cat("            Required columns: Date, Close\n")
  skip_count <- skip_count + 1L
} else {
  aapl_csv <- aapl_csv[1]
  aapl_data <- read.csv(aapl_csv, stringsAsFactors = FALSE)
  cat(sprintf("  Read %s (%d rows)\n", aapl_csv, nrow(aapl_data)))

  close_prices <- aapl_data$Close
  n_close      <- length(close_prices)

  # Compute all indicators
  aapl_sma20 <- c(rep(NA, 19), sma(close_prices, 20))
  aapl_sma50 <- c(rep(NA, 49), sma(close_prices, 50))
  aapl_ema12 <- ema(close_prices, 12)
  aapl_ema26 <- ema(close_prices, 26)
  aapl_macd  <- macd(close_prices, short_period = 12, long_period = 26, signal_period = 9)

  # Rolling 20-period StDev (population)
  aapl_sd20 <- rep(NA, n_close)
  for (idx in 20:n_close) {
    aapl_sd20[idx] <- stdev(close_prices[(idx - 19):idx])
  }

  aapl_rsi14  <- rsi(close_prices, period = 14)
  aapl_stoch  <- stoch_rsi(close_prices, period = 14, k_period = 3, d_period = 3)

  # Crossover / crossunder use padded SMA vectors (NA-safe)
  aapl_crossover  <- crossover(aapl_sma20, aapl_sma50)
  aapl_crossunder <- crossunder(aapl_sma20, aapl_sma50)

  # Count signals (golden cross = "Up", death cross = "Down")
  golden_cross_count <- sum(aapl_crossover  == "Up",   na.rm = TRUE)
  death_cross_count  <- sum(aapl_crossover  == "Down", na.rm = TRUE)
  crossunder_true_count <- sum(aapl_crossunder == "True", na.rm = TRUE)

  cat(sprintf("  Golden Crosses (SMA20 crosses above SMA50): %d\n", golden_cross_count))
  cat(sprintf("  Death Crosses  (SMA20 crosses below SMA50): %d\n", death_cross_count))
  cat(sprintf("  Crossunder events (crossunder == 'True'):   %d\n", crossunder_true_count))

  # Build output data frame
  df_out <- data.frame(
    Date        = aapl_data$Date,
    Close       = round(close_prices,        2),
    SMA20       = round(aapl_sma20,          2),
    SMA50       = round(aapl_sma50,          2),
    EMA12       = round(aapl_ema12,          2),
    EMA26       = round(aapl_ema26,          2),
    MACD_Line   = round(aapl_macd$macd_line,   3),
    Signal_Line = round(aapl_macd$signal_line, 3),
    Histogram   = round(aapl_macd$histogram,   3),
    StDev20     = round(aapl_sd20,            4),
    RSI14       = round(aapl_rsi14,           2),
    Stoch_K     = round(aapl_stoch$k_line,    3),
    Stoch_D     = round(aapl_stoch$d_line,    3),
    MA_Crossover  = aapl_crossover,
    MA_Crossunder = aapl_crossunder,
    stringsAsFactors = FALSE
  )

  out_csv <- file.path(script_dir, "output_data", "AAPL_technical_indicators.csv")
  write.csv(df_out, out_csv, row.names = FALSE)
  cat(sprintf("  Exported: %s\n", out_csv))

  cat("\n  Last 5 rows (key indicators):\n")
  tail5 <- tail(df_out[, c("Date","Close","SMA20","SMA50","RSI14","MA_Crossover")], 5)
  print(tail5, row.names = FALSE)
}

cat("\n")

# ===========================================================================
# Final summary
# ===========================================================================
cat("==============================================================================\n")
total <- pass_count + fail_count
cat(sprintf(" Results: %d / %d tests passed", pass_count, total))
if (skip_count > 0) cat(sprintf(", %d skipped (missing data)", skip_count))
cat("\n")
if (fail_count == 0 && skip_count == 0) {
  cat(" ALL TESTS PASSED\n")
} else if (fail_count == 0) {
  cat(" All executed tests passed (some skipped due to missing data)\n")
} else {
  cat(sprintf(" %d TEST(S) FAILED - review output above\n", fail_count))
}
cat("==============================================================================\n")
