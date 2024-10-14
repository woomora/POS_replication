# ------------------------------------------------------------------------------
# Figure C.3: National Quarterly Activity Indexes by Sector ----
# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

# Load ITAEE data
itaee <- read_csv("data/derived/itaee.csv") |> 
  mutate(year_quarter = yearquarter(year_quarter))

# Filter and prepare data for the activity indexes of primary, secondary, and tertiary sectors
itaee_act <- itaee |> 
  filter(unit == "Total") |>  # Filter for total activity across sectors
  filter(act %in% c("66_primary", "69_secondary", "74_tertiary")) |>  # Filter specific activities
  filter(year_quarter >= yearquarter("2013 Q1")) |>  # Filter data from 2013 Q1 onwards
  mutate(
    # Group periods based on major events
    time_group = case_when(
      year_quarter < yearquarter("2018 Q4") ~ "Pre-NAIM cancellation",
      year_quarter >= yearquarter("2018 Q4") & year_quarter < yearquarter("2020 Q1") ~ "NAIM cancellation",
      TRUE ~ "Post-COVID-19"
    ),
    time_group = fct_relevel(time_group, "Pre-NAIM cancellation", "NAIM cancellation", "Post-COVID-19"),
    
    # Rename activity sectors
    act = case_when(
      act == "66_primary" ~ "Primary sector",
      act == "69_secondary" ~ "Secondary sector",
      act == "74_tertiary" ~ "Tertiary sector"
    ),
    
    # Convert year-quarter to date for plotting
    quarter_date = date(year_quarter)
  )

# Calculate trend based on the activity index at "2018 Q4" for normalization
itaee_act <- itaee_act |> 
  left_join(
    itaee_act |> 
      filter(year_quarter == yearquarter("2018 Q4")) |> 
      select(act, trend) |> 
      rename(trend_ref = trend)  # Reference trend for normalization
  ) |> 
  mutate(
    trend = (trend / trend_ref) * 100  # Normalize trends to the 2018 Q4 index
  ) |> 
  group_by(act) |> 
  arrange(year_quarter) |> 
  mutate(
    # Calculate quarterly and annualized growth rates
    growth = (trend / lag(trend) - 1) * 100,
    growth_annual = ((trend / lag(trend))^4 - 1) * 100
  ) |> 
  ungroup()

# Summarize growth rates by time group and sector
growth_rates <- itaee_act |> 
  group_by(time_group, act) |> 
  summarise(growth_mean = mean(growth_annual, na.rm = TRUE)) |> 
  mutate(grlabel = round(growth_mean, 1))

# ------------------------------------------------------------------------------
# Plotting
# ------------------------------------------------------------------------------

itaee_act |> 
  ggplot(aes(quarter_date, trend, color = act)) +
  
  # Vertical lines for key events
  geom_vline(xintercept = date(yearquarter("2018 Q4")) + 30) +
  annotate("text", x = date(yearquarter("2018 Q4")) + 30, y = -Inf, label = "NAIM cancellation", 
           angle = 90, size = 2, hjust = -.1, vjust = 1.5) +
  
  geom_vline(xintercept = date(yearquarter("2020 Q1"))) +
  annotate("text", x = date(yearquarter("2020 Q1")), y = -Inf, label = "COVID-19", 
           angle = 90, size = 2, hjust = -.1, vjust = 1.5) +
  
  # Plot the activity index trends
  geom_line() +
  
  # X-axis formatting for dates
  scale_x_date(breaks = "6 months", labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) +
  
  # Labels for the plot
  labs(x = "", y = "Quarterly Activity Index", color = "") +
  
  # Color scale using viridis
  viridis::scale_color_viridis(discrete = TRUE) +
  
  # Customize axis text
  theme(axis.text.x = element_text(size = 6, angle = 90)) +
  
  # Add annotations for growth rates across time periods
  # Pre-NAIM cancellation growth rates
  annotate("label", x = ymd("2018-03-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
           label = str_c(growth_rates$grlabel[1], "%")) +
  annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#21918c", 
           label = str_c(growth_rates$grlabel[2], "%")) +
  annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#fde725", 
           label = str_c(growth_rates$grlabel[3], "%")) +
  
  # NAIM cancellation growth rates
  annotate("label", x = ymd("2019-07-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
           label = str_c(growth_rates$grlabel[4], "%")) +
  annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#21918c", 
           label = str_c(growth_rates$grlabel[5], "%")) +
  annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#fde725", 
           label = str_c(growth_rates$grlabel[6], "%")) +
  
  # Post-COVID-19 growth rates
  annotate("label", x = ymd("2020-10-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
           label = str_c(growth_rates$grlabel[7], "%")) +
  annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#21918c", 
           label = str_c(growth_rates$grlabel[8], "%")) +
  annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#fde725", 
           label = str_c(growth_rates$grlabel[9], "%")) +
  
  # Adjust plot margins
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.5, 1, 1, 1), "lines"))

# ------------------------------------------------------------------------------
# Save the Plot
# ------------------------------------------------------------------------------

# Save the plot as a PNG file
ggsave("results/FigC3/FigC3.png", dpi = 300, width = 7.4, height = 4.8)
