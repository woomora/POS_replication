# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 2: NAIM cancellation as a macroeconomic natural experiment — SDID Falsification
# ------------------------------------------------------------------------------
# This code performs a falsification test for the Synthetic DID analysis around the NAIM cancellation.
# It checks for effects on alternative dates (three-day intervals) and plots the results.

# Run the falsification process only if the derived dataset does not already exist
if (!file.exists("results/Fig2/falsification_synthdid.csv")) {
  
  # ------------------------------------------------------------------------------
  # Falsification for each three days
  # ------------------------------------------------------------------------------
  
  # Generate a sequence of dates from Jan 8, 2018, to Apr 1, 2020 (in 1-day increments)
  dates <- 
    tibble(
      dates = seq(ymd("2018-01-08"), ymd("2020-04-01"), by = "1 day")
    ) |> 
    mutate(
      # Filter for Mondays, Wednesdays, and Fridays
      weekday = weekdays(dates)
    ) |> 
    filter(weekday %in% c("Monday", "Wednesday", "Friday"))
  
  # Initialize an empty tibble to store falsification results
  falsification_dates <- tibble()
  
  # Loop over each date in the filtered 'dates' tibble
  for (d in levels(factor(dates$dates))) {
    
    print(d)  # Print current date for tracking progress
    
    # Set up the treatment date for the current iteration
    time.tr <- d
    time.tr.date <- ymd(time.tr)
    
    days <- 7  # Define a 7-day window before and after the treatment
    
    # Filter the BIS exchange rate data for the 7-day window around the current date
    bis_xr_foo <- 
      bis_xr |> 
      filter(date >= time.tr.date - days  & date <= time.tr.date + days) |> 
      mutate(
        # Log of the exchange rate
        lexr = log(exr),
        # Time to treatment, measured in days from the treatment date
        time_to_treat = lubridate::interval(ymd(time.tr.date), date) %/% days(1),
        # Define treatment variable (1 if mxn_mexican_peso on/after treatment date, else 0)
        treatment = if_else(currency == "mxn_mexican_peso" & date >= time.tr.date, 1, 0)
      )
    
    # Prepare data for Synthetic DID analysis
    setup <- 
      bis_xr_foo |> 
      select(currency, time_to_treat, lexr, treatment) |> 
      data.frame() |> 
      panel.matrices()
    
    # Estimate treatment effect using Synthetic DID
    tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
    
    # Set seed for reproducibility of standard errors
    set.seed(123)
    
    # Calculate standard errors using the placebo method
    se <- sqrt(vcov(tau.hat, method = 'placebo'))
    
    # Append the results for the current date to the falsification_dates tibble
    falsification_dates <- 
      falsification_dates |> 
      bind_rows(
        tibble(
          date = ymd(d),
          estimate = tau.hat[,1],
          se = se[1,1],
          tvalue = estimate / se,
          falsf = 1
        )
      )
  }
  
  # ------------------------------------------------------------------------------
  # Finalize falsification results by calculating confidence intervals and p-values
  falsification_dates <- 
    falsification_dates |> 
    mutate(
      # 95% confidence intervals
      lci = estimate - 1.96 * se,
      uci = estimate + 1.96 * se,
      # t-value and p-value
      tvalue = estimate / se,
      pvalue = 2 * pt(q = abs(tvalue), df = 120, lower = F),
      # Classify the treatment window around the NAIM cancellation
      naim_canc_week = case_when(
        date %in% seq(ymd("2018-10-29") - 14, ymd("2018-10-29") + 14, by = "day") ~ "NAIM Cancelation week",
        TRUE ~ "Alternative date"
      ),
      # Define the type of effect based on p-value significance
      type = case_when(
        pvalue <= .05 & naim_canc_week == "Alternative date" ~ "Alternative date",
        pvalue <= .05 & naim_canc_week == "NAIM Cancelation week" ~ "NAIM Cancelation week",
        pvalue > .05 ~ "Non-significant effect"
      )
    )
  
  # Write the falsification results to a CSV file
  write_csv(falsification_dates, "Results/Fig2/falsification_synthdid.csv")
}

# ------------------------------------------------------------------------------
# Falsification Plot ----
# ------------------------------------------------------------------------------
# Read in the derived falsification data
falsification_dates <- 
  read_csv("results/Fig2/falsification_synthdid.csv") |> 
  # Filter to remove dates after February 2020
  filter(date <= ymd("2020-02-01")) |> 
  mutate(
    # Re-classify the 'type' variable for the plot
    type = case_when(
      pvalue <= 0.05 & naim_canc_week == "Alternative date" ~ "Alternative date",
      pvalue <= 0.05 & naim_canc_week == "NAIM Cancelation week" ~ "NAIM Cancelation week",
      pvalue > 0.05 ~ "Non-significant effect"
    )
  )

# ------------------------------------------------------------------------------
# Create the plot ----
# Set up the base plot
p <- 
  ggplot() +
  geom_hline(yintercept = 0) +  # Add a horizontal line at y = 0
  coord_cartesian(ylim = c(-.05, 0.05)) +  # Set y-axis limits
  scale_x_date(  # Customize the x-axis
    breaks =  seq(ymd("2018-01-01"), ymd("2020-03-01"), by = "1 day"),
    date_breaks = "1 months",
    date_labels = "%b %y"
  ) +
  labs(x = "", y = "Synth-DID Estimate (log-points)") +  # Label the axes
  
  # Add vertical lines and annotations for key events
  geom_vline(xintercept = ymd("2018-03-30"), linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = ymd("2018-03-30"), label = "Electoral process begins", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_vic_date, linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = amlo_vic_date, label = "Elections", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_ref_anouncement, linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = naim_ref_anouncement, label = "Referendum announcement", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_canc_date, linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = naim_canc_date, label = "NAIM Cancelation", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_gov_date, linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = amlo_gov_date, label = "President in office", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = ymd("2019-06-19"), linetype = "dashed", alpha = .5, lwd = .2) +
  annotate("text", x = ymd("2019-06-19"), label = "USMCA ratification", y = -0.05, angle = 90, size = 2, vjust = 0, hjust = -0.05)

# Add the data points and error bars for falsification dates
p +
  # Non-significant effects
  geom_point(data = falsification_dates |> filter(type == "Non-significant effect"), aes(x = date, y = estimate), alpha = .05, size = 1.5) +
  geom_linerange(data = falsification_dates |> filter(type == "Non-significant effect"), aes(x = date, ymin = lci, ymax = uci), alpha = .05, lwd = .2) +
  
  # Alternative dates with significant effects
  geom_point(data = falsification_dates |> filter(type == "Alternative date"), aes(x = date, y = estimate), alpha = .5, size = .9, color = "black") +
  geom_linerange(data = falsification_dates |> filter(type == "Alternative date"), aes(x = date, ymin = lci, ymax = uci), alpha = .5, lwd = .25, color = "black") +
  
  # NAIM cancellation week with significant effects
  geom_linerange(data = falsification_dates |> filter(type == "NAIM Cancelation week"), aes(x = date, ymin = lci, ymax = uci), lwd = .25, color = color4t) +
  geom_point(data = falsification_dates |> filter(type == "NAIM Cancelation week"), aes(x = date, y = estimate), alpha = 1, size = 1, color = color4t) +
  
  # Customize axis text
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 90)
  )

# ------------------------------------------------------------------------------
# Saving the Plot (optional) ----
# Uncomment the following lines to save the plot as a PNG file
ggsave(
  "results/Fig2/Fig2.png", # File path to save the plot
  dpi = 300,                                     # Resolution (DPI)
  width = 7.4, height = 4.8                      # Plot dimensions
)
