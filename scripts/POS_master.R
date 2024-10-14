# ------------------------------------------------------------------------------
# ---- Setup: Workspace and Library Loading ----
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Clear existing workspace
rm(list = ls())

# 'pacman' simplifies loading and managing R packages. The p_load function
# installs the packages if they are not already installed and then loads them.
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
  synthdid,       # Difference-in-differences with synthetic control
  modelsummary,   # Model summaries and visualizations
  readxl,         # Excel file reading
  ggnewscale,     # Add multiple scales to ggplot2
  ggthemes,       # Additional ggplot2 themes
  pracma,         # Practical mathematical functions
  tsibble,        # Time series data frame structure
  feasts,         # Time series decomposition
  fabletools,     # Time series forecasting tools
  lpirfs,         # Local projection impulse response functions
  vdemdata,       # Loads the V-Dem dataset, providing data on democracy indicators across the world
  zoo,            # Time series objects
  binsreg,        # Binned scatterplots for regression analysis
  knitr,          # Dynamic report generation in R
  httr,           # Tools for working with HTTP, useful for API calls and web requests
  readxl          # Read Excel files into R
)

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
