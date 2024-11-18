# ------------------------------------------------------------------------------
# ---- Setup: Workspace and Library Loading ----
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Clear existing workspace
rm(list = ls())

# Ensure pacman is installed
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load pacman and required packages
library(pacman)
p_load(
  tidyverse,      # Core packages for data manipulation and visualization
  readr,          # Read data from text files
  janitor,        # Data cleaning
  lubridate,      # Date handling
  panelView,      # Panel data visualization
  fixest,         # Econometric modeling for panel data
  countrycode,    # Country code conversion
  scpi,           # Synthetic control methods
  imputeTS,       # Time series imputation
  modelsummary,   # Model summaries and visualizations
  readxl,         # Excel file reading
  ggnewscale,     # Add multiple scales to ggplot2
  ggthemes,       # Additional ggplot2 themes
  pracma,         # Practical mathematical functions
  tsibble,        # Time series data frame structure
  feasts,         # Time series decomposition
  fabletools,     # Time series forecasting tools
  lpirfs,         # Local projection impulse response functions
  zoo,            # Time series objects
  binsreg,        # Binned scatterplots for regression analysis
  knitr,          # Dynamic report generation in R
  httr,           # Tools for working with HTTP, useful for API calls and web requests
  readxl          # Read Excel files into R
)

# Install and load GitHub packages
if (!requireNamespace("synthdid", quietly = TRUE)) {
  devtools::install_github("synth-inference/synthdid")
}
if (!requireNamespace("vdemdata", quietly = TRUE)) {
  devtools::install_github("vdeminstitute/vdemdata")
}

# Load GitHub packages explicitly
library(synthdid)
library(vdemdata)

# Set working directory to the appropriate project path
path <- getwd()

# ---- Plotting Theme

# Define and set a clean theme for all plots using ggthemes and additional customization
theme_clean <- 
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA), # Set plot background to white
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    legend.background = element_rect(fill = "white", color = NA), # Set legend background to white
  )
theme_set(theme_clean)

# Define a custom color for plotting (used later in your plots)
color4t <- '#a83232'

# ---- Important Dates and Periods

# Key political and event-related dates used for analysis
amlo_vic_date <- ymd("2018-07-01")      # AMLO victory date
amlo_vic_quarter <- 2018.3              # AMLO victory quarter

naim_ref_anouncement <- ymd("2018-08-21")  # Announcement date of NAIM referendum
naim_canc_date <- ymd("2018-10-29")        # NAIM cancelation date
naim_canc_quarter <- 2018.4                # NAIM cancelation quarter

amlo_gov_date <- ymd("2018-12-01")         # AMLO government start date
amlo_gov_quarter <- 2018.4                 # AMLO government start quarter

covid_date <- ymd("2020-02-01")            # Start of COVID-19 pandemic
covid_quarter <- 2020.1                    # COVID-19 quarter

# Set the system locale to English (US) to prevent issues with special characters
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# ---- Load External Functions

# Source custom functions for synthetic control analysis from a separate script
source("scripts/POS_functions.R")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ---- Data ----
# ------------------------------------------------------------------------------
# Check "scripts/POS_data.R" to see the construction of derived data from source
source("scripts/POS_data.R")

# ------------------------------------------------------------------------------
# ---- Exhibits ----
# ------------------------------------------------------------------------------
# Figure 1: Synthetic DID on exchange rate around NAIM’s cancellation ----
source("scripts/exhibits/POS_Fig1.R")
# ------------------------------------------------------------------------------
# Figure 2: NAIM cancellation as a macroeconomic natural experiment — SDID Falsification ----
source("scripts/exhibits/POS_Fig2.R")
# ------------------------------------------------------------------------------
# Figure 3: Descriptive Evidence — Monthly Economic Activity Indicators ----
source("scripts/exhibits/POS_Fig3.R")

# ------------------------------------------------------------------------------
# Figure 4: NAIM’s Referendum and Cancellation Economic Consequences — SCM Monthly ----
source("scripts/exhibits/POS_Fig4.R")
# ------------------------------------------------------------------------------
# Figure 5: NAIM’s Referendum and Cancellation Economic Consequences — Quarterly GDP SCM ----
source("scripts/exhibits/POS_Fig5.R")

# ------------------------------------------------------------------------------
# Figure 6: Economic Uncertainty Indexes ----
source("scripts/exhibits/POS_Fig6.R")
# ------------------------------------------------------------------------------
# Figure 7: Quarterly Investment — ATT ----
source("scripts/exhibits/POS_Fig7.R")
# ------------------------------------------------------------------------------
# Figure 8: Quarterly Construction Index by Region ----
source("scripts/exhibits/POS_Fig8.R")
# ------------------------------------------------------------------------------
# Figure 9: Medium-run Economic ATT on Quarterly GDP — SCM Analysis ----
source("scripts/exhibits/POS_Fig9.R")
# ------------------------------------------------------------------------------
# ---- Appendix ----
# ------------------------------------------------------------------------------
# Figure A.1: V-Dem Indexes — Democracy Trends in Mexico ----
source("scripts/exhibits/POS_FigA1.R")
# ------------------------------------------------------------------------------
# Figure B.1: Quarterly GDP SCM - controlling for imports and exports ----
source("scripts/exhibits/POS_FigB1.R")
# ------------------------------------------------------------------------------
# Figure B.2: Quarterly GDP SCM - controlling for government expenditure share ----
source("scripts/exhibits/POS_FigB2.R")
# ------------------------------------------------------------------------------
# Figure B.3: Robustness Check — Backdating Treatment 9 Months Prior to NAIM Cancellation ----
source("scripts/exhibits/POS_FigB3.R")
# ------------------------------------------------------------------------------
# Figure B.4: Leave-One-Out (LOO) Analysis and Plot for Monthly Economic Activity Index ----
source("scripts/exhibits/POS_FigB4.R")
# ------------------------------------------------------------------------------
# Figure B.5: LOO — Quarterly GDP SCM ----
source("scripts/exhibits/POS_FigB5.R")
# ------------------------------------------------------------------------------
# Figure B.6: Robustness Check — Alternative Donor Pool ----
source("scripts/exhibits/POS_FigB6.R")
# ------------------------------------------------------------------------------
# Figure B.7: Falsification (Placebo Test) in Other Countries ----
source("scripts/exhibits/POS_FigB7.R")
# ------------------------------------------------------------------------------
# Figure B.8: Sensitivity Analysis for Different Dates ----
source("scripts/exhibits/POS_FigB8.R")
# ------------------------------------------------------------------------------
# Figure B.9: Robustness: Local Projections IRF ----
source("scripts/exhibits/POS_FigB9.R")
# ------------------------------------------------------------------------------
# Figure B.10: Regions Map ----
source("scripts/exhibits/POS_FigB10.R")
# ------------------------------------------------------------------------------
# Figure B.11: Heterogeneity: Within-Mexico Regions ----
source("scripts/exhibits/POS_FigB11.R")
# ------------------------------------------------------------------------------
# Figure B.12: Robustness: Different weighting schemes ----
source("scripts/exhibits/POS_FigB12.R")
# ------------------------------------------------------------------------------
# Table B1: Balance on Pre-Treatment Observables: Components shares ----
source("scripts/exhibits/POS_TabB1.R")
# ------------------------------------------------------------------------------
# Table B.2: SCM Weights across specifications ----
source("scripts/exhibits/POS_TabB2.R")

# ------------------------------------------------------------------------------
# Figure C.1: Mexican Financial Uncertainty and Exchange Rate Volatility ----
source("scripts/exhibits/POS_FigC1.R")
# ------------------------------------------------------------------------------
# Figure C.2: GDP Components — ATT----
source("scripts/exhibits/POS_FigC2.R")
# ------------------------------------------------------------------------------
# Figure C.3: National Quarterly Activity Indexes by Sector ----
source("scripts/exhibits/POS_FigC3.R")
# ------------------------------------------------------------------------------
# Figure C.4: Yearly GDP per capita — SCM ----
source("scripts/exhibits/POS_FigC4.R")


