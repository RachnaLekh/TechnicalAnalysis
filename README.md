# Technical Analysis using R

**Course:** BDA400 - Data Science Tools and Techniques  
**Author:** Rachna Lekh  
**Student Account:** [RachnaLekh](https://github.com/RachnaLekh)  
**Institution:** CDI College  

---

## Project Overview

This repository hosts coursework projects for **Technical Analysis using R**. In this three-stage sequential project, quantitative methodologies are applied to evaluate historical financial time series, compute statistical distributions, generate technical momentum indicators, and model market trends.

This stage encompasses **Assignment 2: Preliminary Stage**, establishing:
- Dedicated GitHub version control repository.
- R and RStudio computing environment configuration.
- Quantitative financial libraries (`quantmod`, `TTR`, `dplyr`, `ggplot2`).
- Portfolio definition and ingestion pipeline (`portfolio.txt`).
- Technical calculation functions (Mean, Mode, Median, Standard Deviation, SMA20, SMA50).
- Graphical outputs and full step-by-step MS-Word submission documentation.

---

## Repository Structure

Per course instructions, the repository organizes each assignment into its own dedicated subdirectory:

```text
TechnicalAnalysis/
├── README.md                                    # Root repository overview (this file)
├── .gitignore                                   # Ignore rules for temporary R/OS files
└── Assignment2/                                 # Stage 1: Preliminary Stage
    ├── portfolio.txt                            # Ingested equity ticker list (AAPL, MSFT, etc.)
    ├── technical_analysis_preliminary.R         # Core modular R functions & pipeline
    ├── technical_analysis_preliminary.Rmd       # R Markdown comprehensive report
    ├── technical_analysis_preliminary.html      # Knitted HTML documentation
    ├── RachnaLekh_BDA400_A02.docx               # Formal MS-Word report with screenshots
    ├── screenshots/                             # Step-by-step visual proofs (Steps 1–7)
    │   ├── step1_github_setup.png
    │   ├── step2_r_rstudio_install.png
    │   ├── step3_package_install.png
    │   ├── step4_portfolio_config.png
    │   ├── step5_load_data_output.png
    │   ├── step6_statistics_output.png
    │   └── step7_visualization_output.png
    └── output_data/                             # Generated statistics & technical charts
        ├── stock_statistics_summary.csv         # Consolidated statistical summary table
        ├── AAPL_daily.csv                       # Historical daily OHLCV series
        ├── MSFT_daily.csv
        ├── GOOGL_daily.csv
        ├── AMZN_daily.csv
        ├── TSLA_daily.csv
        └── plots/                               # High-resolution (300 DPI) chart exports
            ├── AAPL_technical_analysis.png
            ├── MSFT_technical_analysis.png
            ├── GOOGL_technical_analysis.png
            ├── AMZN_technical_analysis.png
            └── TSLA_technical_analysis.png
```

---

## Requirements & Dependencies

- **R Version:** 4.1 or later
- **IDE:** RStudio Desktop or Posit Cloud
- **R Packages:**
  ```r
  install.packages(c("quantmod", "TTR", "dplyr", "ggplot2", "knitr", "rmarkdown"))
  ```

---

## Execution Guide

### 1. Run the Analysis Script
Open R or RStudio in the project folder and run:

```r
setwd("Assignment2")
source("technical_analysis_preliminary.R")
```

### 2. Knit the R Markdown Report
To compile the interactive HTML report:

```r
rmarkdown::render("Assignment2/technical_analysis_preliminary.Rmd")
```

---

## Statistical Summary (Preliminary Stage)

| Ticker | Mean ($) | Mode ($) | Median ($) | Std Dev ($) | Latest Close ($) | 20-Day SMA | 50-Day SMA |
|---|---|---|---|---|---|---|---|
| **AAPL** | 139.55 | 125.00 | 130.61 | 21.25 | 187.36 | 180.29 | 176.99 |
| **MSFT** | 304.91 | 265.00 | 313.46 | 31.99 | 316.52 | 328.86 | 324.87 |
| **GOOGL** | 120.48 | 123.00 | 122.67 | 19.58 | 159.99 | 155.21 | 149.02 |
| **AMZN** | 108.32 | 103.00 | 106.63 | 13.37 | 124.35 | 128.09 | 129.11 |
| **TSLA** | 197.60 | 186.00 | 188.27 | 47.82 | 275.02 | 277.15 | 271.86 |

---

## Technical Chart Preview

Below is a preview of the dual-panel technical analysis chart generated for **Apple Inc. (AAPL)**, displaying closing price trend relative to the 20-day and 50-day Simple Moving Averages, alongside daily trading volume bars:

![AAPL Technical Analysis](Assignment2/output_data/plots/AAPL_technical_analysis.png)

---

## License

This project is licensed under the MIT License.
