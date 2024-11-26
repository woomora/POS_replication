# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB9"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.9: Robustness: Local Projections IRF
# ------------------------------------------------------------------------------
# This script estimates the impulse response functions (IRF) using local projections
# for both monthly and quarterly data, assessing the impact of the NAIM cancellation.
# The script generates two separate plots for monthly and quarterly IRFs.
# ------------------------------------------------------------------------------
# Monthly Data Preparation and Local Projections Estimation
# ------------------------------------------------------------------------------

# Data Preparation
foo <- 
  monindex |> 
  mutate(eaindex_trend = log(eaindex_trend)) |>  # Log-transform the economic activity index trend
  filter(date <= ymd("2020-01-01")) |>           # Filter data up to January 2020
  filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
  filter(!is.na(eaindex_trend))                  # Exclude rows with missing data

# Define pre- and post-treatment periods
period.labels <- as.character(levels(factor(foo$date)))
period <- yearmonth(levels(factor(foo$year_month)))
time.tr <- yearmonth("2018 Oct")                 # Set treatment date (NAIM cancellation)
period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
period.labels.post <- period.labels[c(match(time.tr, period):length(period))]

# Add time-to-treatment and treatment variables
foo <- 
  foo |> 
  group_by(countrycode) |> 
  mutate(
    time_to_treat = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1)),
    treat = if_else(countrycode == "MEX" & time_to_treat >= 0, 1, 0)  # Define treatment for Mexico post-NAIM
  ) |> 
  ungroup() |> 
  select(countrycode, date, eaindex_trend, treat, time_to_treat) |> 
  glimpse()

# Check the distribution of time to treatment
table(foo$time_to_treat)

# ------------------------------------------------------------------------------
# Estimate Local Projections for Monthly Data
# ------------------------------------------------------------------------------

# Estimate the panel model using local projections (lpirfs package)
results_panel_monthly <- lp_lin_panel(
  data_set     = foo, 
  endog_data   = "eaindex_trend",      # Endogenous variable (log of economic activity index)
  cumul_mult   = TRUE,                 # Cumulative multiplier
  shock        = "treat",              # Treatment shock variable
  diff_shock   = TRUE,                 # Use differences in shocks
  panel_model  = "within",             # Fixed effects model
  panel_effect = "individual",         # Individual-level fixed effects
  robust_cov   = "vcovSCC",            # Robust covariance matrix (Driscoll-Kraay)
  confint      = 1.96,                 # Confidence interval
  hor          = 14                    # Horizon of 14 months
)

# Plot IRFs for monthly data
plot_lin_panel <- plot_lin(results_panel_monthly)
plot(plot_lin_panel[[1]])  # Display the first plot

# Store monthly IRF estimates in a data frame
irf_monthly <- tibble(
  time_to_treat = seq(1, length(results_panel_monthly$irf_panel_mean[1,])),
  mean = results_panel_monthly$irf_panel_mean[1,],
  lci = results_panel_monthly$irf_panel_low[1,],
  uci = results_panel_monthly$irf_panel_up[1,]
) |> bind_rows(
  tibble(time_to_treat = 0, mean = 0, lci = 0, uci = 0)  # Add time zero
)

# Save IRF estimates to CSV
irf_monthly |> write_csv("results/FigB9/irf_monthly.csv")

# ------------------------------------------------------------------------------
# Quarterly Data Preparation and Local Projections Estimation
# ------------------------------------------------------------------------------

# Prepare quarterly data
foo <- 
  posq |> 
  filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
  filter(year >= 2013) |> 
  filter(year_quarter < yearquarter("2020 Q1")) |> 
  mutate(
    naim = if_else(countrycode == "MEX" & year_quarter >= yearquarter("2018 Q4"), 1, 0),  # Treatment for Mexico
    year_quarter = as.character(year_quarter),
    gdpi = log(gdpi),          # Log-transform GDP
    int_rate = log(int_rate),  # Log-transform interest rate
    ex_rate = log(ex_rate)     # Log-transform exchange rate
  ) |> 
  select(countrycode, year_quarter, gdpi, int_rate, ex_rate, naim) |> 
  distinct(countrycode, year_quarter, .keep_all = TRUE)  # Ensure distinct values

# Estimate local projections for quarterly data
results_panel_quarterly <- lp_lin_panel(
  data_set     = foo, 
  endog_data   = "gdpi",            # Endogenous variable (log of GDP)
  cumul_mult   = TRUE, 
  shock        = "naim",            # Treatment shock variable (NAIM cancellation)
  diff_shock   = TRUE, 
  panel_model  = "within", 
  panel_effect = "individual", 
  robust_cov   = "vcovNW",          # Robust covariance matrix (Newey-West)
  confint      = 1.96, 
  hor          = 4                  # Horizon of 4 quarters
)

# Plot IRFs for quarterly data
plot_lin_panel <- plot_lin(results_panel_quarterly)
plot(plot_lin_panel[[1]])

# Store quarterly IRF estimates in a data frame
irf_quarterly <- tibble(
  time_to_treat = seq(1, length(results_panel_quarterly$irf_panel_mean[1,])),
  mean = results_panel_quarterly$irf_panel_mean[1,],
  lci = results_panel_quarterly$irf_panel_low[1,],
  uci = results_panel_quarterly$irf_panel_up[1,]
) |> bind_rows(
  tibble(time_to_treat = 0, mean = 0, lci = 0, uci = 0)
)

# Save IRF estimates to CSV
irf_quarterly |> write_csv("results/FigB9/irf_quarterly.csv")

# ------------------------------------------------------------------------------
# Plotting Monthly and Quarterly IRFs
# ------------------------------------------------------------------------------

# Plot Monthly IRF
irf_monthly <- read_csv("results/FigB9/irf_monthly.csv")
irf_monthly |> 
  ggplot(aes(time_to_treat, mean)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-.07, .035, .01)) +
  scale_x_continuous(breaks = seq(0, 15, 3)) +
  labs(x = "Relative time to treatment (months)", y = "ATT", color = "", fill = "") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  geom_ribbon(alpha = .25, aes(ymin = lci, ymax = uci, fill = "95% CI")) +
  scale_fill_manual(values = c("grey", "grey70")) +
  geom_line(color = color4t) +
  theme(axis.text.y = element_text(size = 9), axis.title = element_text(size = 9), legend.text = element_text(size = 9)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))

# Save the plot as a PNG file
ggsave("results/FigB9/FigB9a.png", dpi = 300, width = 7.4, height = 4.8)

# Plot Quarterly IRF
irf_quarterly <- read_csv("results/FigB9/irf_quarterly.csv")
irf_quarterly |> 
  ggplot(aes(time_to_treat, mean)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-.07, .035, .01)) +
  scale_x_continuous(breaks = seq(0, 4, 1)) +
  labs(x = "Relative time to treatment (quarters)", y = "ATT", color = "", fill = "") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  geom_ribbon(alpha = .25, aes(ymin = lci, ymax = uci, fill = "95% CI")) +
  scale_fill_manual(values = c("grey", "grey70")) +
  geom_line(color = color4t) +
  theme(axis.text.y = element_text(size = 9), axis.title = element_text(size = 9), legend.text = element_text(size = 9)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))

# Save the plot as a PNG file
ggsave("results/FigB9/FigB9b.png", dpi = 300, width = 7.4, height = 4.8)
