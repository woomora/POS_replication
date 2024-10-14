# ------------------------------------------------------------------------------
# Figure 6: Economic Uncertainty Indexes
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Part 1: Economic Policy Uncertainty (EPU) Index for Mexico
# ------------------------------------------------------------------------------
# Load and clean EPU data
gepu <- read_csv("data/source/Economic Policy Uncertainty/epu.csv") |>
  janitor::clean_names() |>
  mutate(
    month = if_else(str_count(month) == 1, str_c("0", month), as.character(month)),
    date = ymd(str_c(year, "-", month, "-01"))  # Convert to date format
  ) |>
  select(-gepu_current, -gepu_ppp, -year, -month) |>  # Drop unnecessary columns
  pivot_longer(
    !c(date), names_to = "country", values_to = "gepu"  # Pivot to long format
  ) |>
  filter(date >= ymd("2010-01-01")) |>  # Filter for dates starting from 2010
  filter(country %in% c("mexico")) |>  # Focus on Mexico
  select(-country)  # Remove country column

# Normalize the EPU index with September 2018 as the base
gepu <- gepu |>
  bind_cols(
    gepu |>
      filter(date == ymd("2018-09-01")) |>
      rename(gepu_base = gepu) |>
      select(-date)  # Get base value from September 2018
  ) |>
  mutate(gepu_index = (gepu / gepu_base))  # Normalize the index

# Plot EPU index for Mexico with relevant annotations
gepu |>
  filter(date >= ymd("2017-06-01")) |> 
  filter(date <= ymd("2020-04-01")) |> 
  ggplot(aes(date, gepu_index)) +
  geom_line(color = color4t) +  # Line plot
  labs(x = "", y = "EPU Index (Base September 2018)") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  
  # Annotate relevant events
  geom_vline(xintercept = ymd("2018-03-30"), alpha = .5, lwd = .2) +  # Electoral process starts
  annotate("text", x = ymd("2018-03-30"), label = "Electoral process begins", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_vic_date, alpha = .5, lwd = .2) +  # Elections
  annotate("text", x = amlo_vic_date, label = "Elections", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_ref_anouncement, alpha = .5, lwd = .2) +  # NAIM referendum announcement
  annotate("text", x = naim_ref_anouncement, label = "Referendum announcement", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_canc_date, alpha = .5, lwd = .2) +  # NAIM Cancellation
  annotate("text", x = naim_canc_date, label = "NAIM Cancellation", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_gov_date, alpha = .5, lwd = .2) +  # First day as president
  annotate("text", x = amlo_gov_date, label = "President in office", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = ymd("2019-06-19"), alpha = .5, lwd = .2) +  # USMCA ratification
  annotate("text", x = ymd("2019-06-19"), label = "USMCA ratification", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = covid_date, alpha = .5, lwd = .2) +  # COVID-19
  annotate("text", x = covid_date, label = "COVID-19", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  theme(
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6)
  )

# Save the plot as PNG
ggsave("results/Fig6/Fig6a.png", dpi = 300, width = 7.4, height = 4.8)

# ------------------------------------------------------------------------------
# Part 2: S&P/BMV IPC VIX Index
# ------------------------------------------------------------------------------
# Load and clean S&P/BMV IPC VIX data
spbmv_vix <- read_csv(str_c(path, "/data/source/spbmv_vix.csv")) |>
  janitor::clean_names() |>
  mutate(date = dmy(date))  # Convert date to correct format

# Normalize the VIX index with NAIM referendum announcement as the base
spbmv_vix <- spbmv_vix |>
  bind_cols(
    spbmv_vix |>
      filter(date == naim_ref_anouncement) |> 
      rename(spbmv_vix_ref = spbmv_vix) |> 
      select(-date)  # Base value from NAIM referendum announcement
  ) |>
  mutate(spbmv_vix_i = spbmv_vix / spbmv_vix_ref)  # Normalized VIX index

# Plot S&P/BMV IPC VIX Index with relevant annotations
spbmv_vix |>
  filter(date >= ymd("2017-08-01")) |> 
  filter(date <= ymd("2020-04-01")) |> 
  ggplot(aes(date, spbmv_vix_i)) +
  geom_line(color = color4t) +
  scale_x_date(breaks = seq(ymd("2018-01-01"), ymd("2020-03-01"), by = "1 day"),
               date_breaks = "2 months", date_labels = "%b %y") +
  labs(x = "", y = "S&P/BMV IPC VIX (Base 2018-08-21)") +
  
  # Annotate relevant events
  geom_vline(xintercept = ymd("2018-03-30"), alpha = .5, lwd = .2) +  # Electoral process starts
  annotate("text", x = ymd("2018-03-30"), label = "Electoral process begins", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_vic_date, alpha = .5, lwd = .2) +  # Elections
  annotate("text", x = amlo_vic_date, label = "Elections", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_ref_anouncement, alpha = .5, lwd = .2) +  # NAIM referendum announcement
  annotate("text", x = naim_ref_anouncement, label = "Referendum announcement", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = naim_canc_date, alpha = .5, lwd = .2) +  # NAIM Cancellation
  annotate("text", x = naim_canc_date, label = "NAIM Cancellation", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = amlo_gov_date, alpha = .5, lwd = .2) +  # First day as president
  annotate("text", x = amlo_gov_date, label = "President in office", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = ymd("2019-06-19"), alpha = .5, lwd = .2) +  # USMCA ratification
  annotate("text", x = ymd("2019-06-19"), label = "USMCA ratification", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  geom_vline(xintercept = covid_date, alpha = .5, lwd = .2) +  # COVID-19
  annotate("text", x = covid_date, label = "COVID-19", 
           y = -Inf, angle = 90, size = 2, vjust = 0, hjust = -0.05) +
  
  theme(
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6)
  )

# Save the VIX plot as PNG
ggsave("results/Fig6/Fig6b.png", dpi = 300, width = 7.4, height = 4.8)
