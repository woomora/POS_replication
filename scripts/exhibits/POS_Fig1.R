# ------------------------------------------------------------------------------
# Figure 1: Synthetic DID on exchange rate around NAIM’s cancellation
# ------------------------------------------------------------------------------
# This script performs a Synthetic Difference-in-Differences (SDID) analysis on the 
# exchange rate of the Mexican Peso (MXN) around the cancellation of the NAIM airport project.
# The aim is to estimate the effect of NAIM's cancellation on the MXN-USD exchange rate.

# Define the treatment date (NAIM cancellation)
time.tr <- "2018-10-29"
time.tr.date <- ymd(time.tr)

# Define the time window around the treatment (7 days before and after)
days <- 7

# ------------------------------------------------------------------------------
# Data Preparation ----
# Filter the data for the currencies and date range of interest
bis_xr_foo <- 
  bis_xr |> 
  # Remove 'eur_euro' and 'cad_canadian_dollar' from the analysis
  filter(!(currency %in% c("eur_euro", "cad_canadian_dollar"))) |> 
  # Filter the dataset to include only the 7 days before and after the NAIM cancellation
  filter(date >= time.tr.date - days  & date <= time.tr.date + days) |>
  group_by(currency) |> 
  # Mutate the data to add necessary columns for analysis
  mutate(
    # Take the natural logarithm of the exchange rate for easier interpretation of results
    lexr = log(exr),
    # Calculate the time relative to the treatment date (in days)
    time_to_treat = lubridate::interval(ymd(time.tr.date), date) %/% days(1),
    # Define a treatment indicator: 1 if MXN after NAIM cancellation, 0 otherwise
    treatment = if_else(currency == "mxn_mexican_peso" & date >= time.tr.date, 1, 0)
  ) |> 
  ungroup()

# ------------------------------------------------------------------------------
# Setup for Synthetic DID ----
# Create a panel matrix required for the SDID estimation
setup = 
  bis_xr_foo |> 
  # Select relevant columns (currency, time to treatment, log of exchange rate, and treatment)
  select(currency, time_to_treat, lexr, treatment) |> 
  data.frame() |> 
  # Create matrices for panel estimation
  panel.matrices()

# ------------------------------------------------------------------------------
# Synthetic DID Estimation ----
# Estimate the treatment effect using the SDID method
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)

# Set seed for reproducibility of standard errors
set.seed(123)

# Calculate the standard error using the placebo method
se = sqrt(vcov(tau.hat, method='placebo'))

# Extract the estimated treatment effect and standard error
ex_estimate <- tau.hat[1]
ex_se <- se[1,1]

# ------------------------------------------------------------------------------
# Plotting the Results ----
# Create a plot to visualize the Synthetic DID results
synthdid_plot(
  tau.hat, overlay = .999, trajectory.alpha = 1,  # Control the overlay and transparency of the trajectories
  control.name = 'Synthetic MXN',                # Name for the synthetic control group
  treated.name = 'MXN'                           # Name for the treated group (MXN)
) +
  # Label the axes
  labs(x = "Relative time to NAIM's cancellation (days)", 
       y = "MXN to USD (log)") +
  # Add an annotation for the treatment effect estimate and its standard error
  annotate(
    "text", x = -0.5, y = -Inf, 
    hjust = 0, vjust = -0.5, 
    color = color4t, size = 3,
    label = str_c("Estimate:\n", round(c(tau.hat), 3), " (", round(c(se), 3), ")") 
  ) +
  # Customize the x-axis breaks (show days -7 to 7)
  scale_x_continuous(breaks = seq(-7, 7)) +
  # Customize the color of the plot lines
  scale_color_manual(values = c("black", color4t)) +
  # Apply a clean theme
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",               # Move the legend to the bottom of the plot
    legend.text = element_text(size = 10),    # Adjust legend text size
    axis.title = element_text(size = 9),      # Adjust axis title text size
    plot.background = element_rect(fill = "white", color = "white"),  # Set plot background
    legend.background = element_rect(color = NA) # Set legend background
  )

# ------------------------------------------------------------------------------
# Saving the Plot (optional) ----
# Uncomment the following lines to save the plot as a PNG file
ggsave(
  "results/Fig1/Fig1.png", # File path to save the plot
  dpi = 300,                                     # Resolution (DPI)
  width = 7.4, height = 4.8                      # Plot dimensions
)
