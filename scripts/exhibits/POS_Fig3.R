# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 3: Descriptive Evidence — Monthly Economic Activity Indicators
# ------------------------------------------------------------------------------
# Load the Monthly Economic Activity Index dataset
monindex <- 
  read_csv("data/derived/monthly_indexes_final.csv") |> 
  mutate(
    year_month = yearmonth(date)  # Create a 'year_month' variable using the 'date'
  )

# Decompose the time series data to extract trend and seasonal components
monindex <- 
  monindex |> 
  left_join(
    monindex |> 
      filter(year_month <= yearmonth("2020 Jan")) |>  # Limit to data before January 2020
      as_tsibble(key = "countrycode", index = "year_month") |>  # Convert to tsibble for time-series operations
      model(classical_decomposition(eaindex ~ season(4), type = "additive")) |>  # Apply classical decomposition
      components() |> 
      select(countrycode, year_month, trend, seasonal) |> 
      rename(
        eaindex_trend = trend,       # Rename trend component
        eaindex_seasonal = seasonal  # Rename seasonal component
      )
  )

# Filter the dataset and create additional variables for analysis
monindex <- 
  monindex |> 
  filter(
    countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")  # Filter for specific countries
  ) |> 
  filter(date <= ymd("2020-02-01")) |>  # Limit to data before February 2020
  mutate(
    mex = if_else(countrycode == "MEX", 1, 0),  # Identify Mexico's data
    time_group = case_when(   # Categorize time periods based on political events
      date < amlo_vic_date ~ "Pre-election",
      date >= amlo_vic_date & date < naim_canc_date ~ "Post-election & Pre-cancellation",
      date >= naim_canc_date ~ "Post-cancellation"
    ),
    time_group = fct_relevel(   # Reorder factor levels for consistent plotting
      time_group, "Pre-election", "Post-election & Pre-cancellation", "Post-cancellation"
    )
  ) |> 
  group_by(countrycode) |> 
  mutate(
    growth = (eaindex_trend/lag(eaindex_trend) - 1) * 100,   # Calculate monthly growth rate
    growth_annual = ((eaindex_trend/lag(eaindex_trend))^12 - 1) * 100  # Annualized growth rate
  ) |> 
  ungroup() 

# ------------------------------------------------------------------------------
# Plot: Monthly Economic Activity Index over Time
# ------------------------------------------------------------------------------
# Create the plot with trend lines for other countries and highlight Mexico's data
p <- 
  monindex |> 
  filter(desestacion == 1) |>  # Filter for seasonally adjusted data
  filter(countrycode != "MEX") |>  # Exclude Mexico for the gray background lines
  ggplot(aes(date, eaindex_trend, group = country)) +
  geom_line(color = '#999999', alpha = .33) +  # Gray lines for other countries
  geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +  # Election date
  geom_vline(xintercept = naim_canc_date - 1) +  # NAIM cancellation date
  scale_x_date(
    date_breaks = "3 months",  # X-axis breaks every 3 months
    minor_breaks = "1 month",  # Minor breaks every month
    date_labels = "%b %y"      # Format for date labels
  ) +
  scale_y_continuous(
    breaks = seq(70, 110, 2.5)  # Y-axis breaks
  ) +
  labs(x = "", y = "Monthly Economic Activity Index\n(Index Base October 2018)", color = "") +
  geom_line(
    data = monindex |> filter(desestacion == 1) |> filter(countrycode == "MEX"),
    aes(date, eaindex_trend), color = color4t, lwd = .85  # Highlight Mexico in a different color
  ) +
  annotate("text", x = amlo_vic_date, y = 80, label = "Elections", angle = 90, size = 3, hjust = 0, vjust = -.5) +
  annotate("text", x = naim_canc_date, y = 80, label = "NAIM cancellation", angle = 90, size = 3, hjust = 0, vjust = -.5) +
  theme(
    axis.text.x = element_text(size = 6, angle = 90),  # X-axis text style
    axis.text.y = element_text(size = 8),              # Y-axis text style
    axis.title = element_text(size = 8)                # Axis title text style
  ) +
  coord_cartesian(ylim = c(80, 107.5))  # Y-axis limits

# ------------------------------------------------------------------------------
# Calculate Growth Rates for Each Time Period
# ------------------------------------------------------------------------------
growth_rates <- 
  monindex |>
  group_by(time_group, mex) |>  # Group by time period and Mexico indicator
  summarise(
    growth_mean = mean(growth_annual, na.rm = TRUE),  # Mean growth rate
    growth_sd = sd(growth_annual, na.rm = TRUE),      # Standard deviation
  ) |> 
  mutate(
    grlabel = round(growth_mean, 1)  # Round growth rates for labels
  )

# Add growth rate annotations to the plot
p +
  # Pre-election period
  annotate(
    "label", x = ymd("2017-11-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#555", 
    label = str_c(growth_rates$grlabel[1], "%")
  ) +
  annotate(
    "label", x = ymd("2017-11-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = color4t, 
    label = str_c(growth_rates$grlabel[2], "%")
  ) +
  # Post-election & Pre-cancellation period
  annotate(
    "label", x = ymd("2018-09-01"), y = Inf, size = 2.5, hjust = .5, color = "#555", 
    label = str_c(growth_rates$grlabel[3], "%")
  ) +
  annotate(
    "label", x = ymd("2018-09-01"), y = Inf, vjust = 2, size = 2.5, hjust = .5, color = color4t, 
    label = str_c(growth_rates$grlabel[4], "%")
  ) +
  # Post-cancellation period
  annotate(
    "label", x = ymd("2019-07-01"), y = Inf, size = 2.5, hjust = .5, color = "#555", 
    label = str_c(growth_rates$grlabel[5], "%")
  ) +
  annotate(
    "label", x = ymd("2019-07-01"), y = Inf, vjust = 2, size = 2.5, hjust = .5, color = color4t, 
    label = str_c(growth_rates$grlabel[6], "%")
  ) +
  coord_cartesian(clip = "off") +  # Ensure labels are fully visible
  theme(
    plot.margin = unit(c(1.5,1,1,1), "lines")  # Adjust plot margins
  )

# Save the plot as a PNG file
ggsave(
  "results/Fig3/Fig3.png",                       # File path to save the plot
  dpi = 300,                                     # Resolution (DPI)
  width = 7.4, height = 4.8                      # Plot dimensions
 )
