# ------------------------------------------------------------------------------
# Figure C.1: Mexican Financial Uncertainty and Exchange Rate Volatility ----
# ------------------------------------------------------------------------------
# Data Loading and Preparation
# ------------------------------------------------------------------------------

# Load exchange rate data
ex <- read_csv("data/source/BIS/exchange_rates.csv") |> 
  janitor::clean_names() |>  # Clean column names
  mutate(
    date = dmy(time_period),  # Convert date from string to Date format
    exr = mxn_mexican_peso     # Assign the Mexican Peso exchange rate to a new variable
  ) |> 
  select(date, exr) |>  # Keep only date and exchange rate columns
  glimpse()

# Load VIX index data
vix <- read_csv("data/source/spbmv_vix.csv") |> 
  mutate(
    date = dmy(date)  # Convert date from string to Date format
  ) |> 
  rename(vix = spbmv_vix)  # Rename the VIX column to a more descriptive name

# Merge exchange rate and VIX data
foo <- left_join(ex, vix, by = "date") |> 
  arrange(date) |>  # Sort data by date
  mutate(
    # Fill missing values by carrying the last observed value forward
    exr = zoo::na.locf(exr, na.rm = FALSE),
    vix = zoo::na.locf(vix, na.rm = FALSE),
    
    # Calculate daily log returns for the exchange rate
    log_return = c(NA, diff(log(exr))),
    
    # Calculate 7-day rolling volatility (standard deviation of log returns)
    exr_volatility = rollapply(log_return, width = 7, FUN = sd, fill = NA, align = "right")
  ) |>
  filter(!is.na(vix))  # Remove rows with missing VIX values

# ------------------------------------------------------------------------------
# Regression Analysis
# ------------------------------------------------------------------------------

# Estimate the relationship between exchange rate volatility and VIX (log-transformed)
model <- feols(
  exr_volatility ~ log(vix),  # Regress volatility on log of VIX
  data = foo, 
  vcov = vcov_NW(time = ~ date)  # Newey-West standard errors for time series data
)

# Display the model summary
summary(model)

# ------------------------------------------------------------------------------
# Binned Regression Plot
# ------------------------------------------------------------------------------

# Create a binned regression plot using binsreg package
p <- binsreg(
  foo$exr_volatility,       # Dependent variable: exchange rate volatility
  log(foo$vix),             # Independent variable: log of VIX index
  polyreg = 1               # Polynomial degree for the regression
)

# Customize the plot aesthetics
p$bins_plot +
  labs(
    x = "S&P/BMV IPC VIX index (log)", 
    y = "Exchange rate volatility",
  ) +
  theme_clean

# Save the plot as a PNG file
ggsave("results/FigC1/FigC1.png", dpi = 300, width = 7.4, height = 4.8)
