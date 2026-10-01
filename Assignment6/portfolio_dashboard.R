# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Script: portfolio_dashboard.R
# Author: Rachna Lekh
# GitHub: https://github.com/RachnaLekh/TechnicalAnalysis
# Description: Modular launch script and core visual routines for the Shiny
#              Portfolio Dashboard.
# ==============================================================================

app_file <- if (file.exists("app.R")) "app.R" else file.path("Assignment6", "app.R")

if (file.exists(app_file)) {
  cat(sprintf("[INFO] Launching R Shiny application from: %s\n", app_file))
  cat("[INFO] Listening on http://127.0.0.1:4242 ... Press Ctrl+C or Esc to terminate.\n\n")
  shiny::runApp(app_file, port = 4242, launch.browser = TRUE)
} else {
  stop(sprintf("Could not locate %s. Please check active directory.", app_file))
}
