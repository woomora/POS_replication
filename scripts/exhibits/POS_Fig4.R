# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig4"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 4: NAIM’s Referendum and Cancellation Economic Consequences — SCM Monthly
# ------------------------------------------------------------------------------
# This section of code only runs if the result files do not exist in the specified directory.

# Define file paths for the output results
scpi_estimates_file <- "results/Fig4/scpi_estimates.csv"
scpi_inference_file <-"results/Fig4/scpi_inference.csv"
scpi_weights_file <- "results/Fig4/scpi_weights.csv"

# Check if result files already exist
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file) | !file.exists(scpi_weights_file)) {
  
  # ------------------------------------------------------------------------------
  # Prepare Data for SCM Analysis
  # ------------------------------------------------------------------------------
  
  foo <- 
    monindex |> 
    mutate(eaindex_trend = log(eaindex_trend)) |>  # Log transformation of the economic activity index
    filter(date <= ymd("2020-01-01")) |>           # Filter for the time period up to January 2020
    filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
    filter(!is.na(eaindex_trend))                  # Exclude rows with missing values
  
  # Define time periods
  period.labels <- as.character(levels(factor(foo$date)))
  period <- yearmonth(levels(factor(foo$year_month)))
  time.tr <- yearmonth("2018 Oct")                 # Treatment period (NAIM cancellation)
  period.pre <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]   # Post-treatment period
  period.labels.post <- period.labels[c(match(time.tr, period):length(period))]
  
  # Prepare the data for SCM
  foo <- 
    foo |> 
    group_by(countrycode) |> 
    mutate(
      time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1)),
      treat = if_else(time_to_treat_naim >= 0, 1, 0)  # Define treatment indicator
    ) |> 
    ungroup() |> 
    glimpse()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
  
  # ------------------------------------------------------------------------------
  # Set Options for Data Preparation
  # ------------------------------------------------------------------------------
  
  time.tr <- 0  # Time of treatment
  id.var      <- "countrycode"                      # ID variable
  time.var    <- "time_to_treat_naim"               # Time variable (relative to treatment)
  period.pre  <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]   # Post-treatment period
  unit.tr     <- "MEX"                              # Treated unit (Mexico)
  unit.co     <- unique(foo$countrycode)[-match("MEX", unique(foo$countrycode))]  # Donor pool
  outcome.var <- "eaindex_trend"                    # Outcome variable (economic activity index trend)
  cov.adj     <- NULL                               # No additional covariates for adjustment
  features    <- NULL                               # No extra features other than outcome
  constant    <- TRUE                               # Include a constant term in the model
  report.missing <- FALSE                           # Do not report missing values
  cointegrated.data <- TRUE                         # Assume data are cointegrated (for time series)
  
  # Data preparation for SCM
  df <- scdata(
    df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
    period.pre = period.pre, period.post = period.post,
    unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
    constant = constant, cointegrated.data = cointegrated.data
  )
  
  # ------------------------------------------------------------------------------
  # Set Options for SCM Estimation
  # ------------------------------------------------------------------------------
  
  # Synthetic Control Method options
  u.alpha  <- 0.05
  e.alpha  <- 0.1
  rho      <- NULL
  rho.max  <- 1
  V        <- NULL
  u.order  <- 1
  u.lags   <- 0
  u.sigma  <- "HC2"
  u.missp  <- TRUE
  e.lags   <- 1
  e.order  <- 0
  e.method <- "all"
  lgapp    <- "linear"
  cores    <- parallel::detectCores()
  sims     <- 1000
  w.constr <- list(name = "simplex")   # Simplex-type constraint for SCM weights
  
  # ------------------------------------------------------------------------------
  # Estimation with Synthetic Control Method
  # ------------------------------------------------------------------------------
  
  set.seed(1234)
  model <- scpi(
    w.constr = w.constr, data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
    u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags, e.method = e.method, 
    cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max
  )
  
  # Plot the SCM results
  scplot(model)
  
  # ------------------------------------------------------------------------------
  # Save SCM Results: Estimates, Inference, and Weights
  # ------------------------------------------------------------------------------
  
  # Extract estimates and save them
  scpi_estimates <- sc_est(model, "eaindex_trend", period, period.labels)
  write_csv(scpi_estimates, "results/Fig4/scpi_estimates.csv")
  
  # Extract inference results and save them
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
  write_csv(scpi_inference, "results/Fig4/scpi_inference.csv")
  
  # Extract SCM weights and save them
  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), "MEX.")
  )
  write_csv(weights, "results/Fig4/scpi_weights.csv")
  
}

# ------------------------------------------------------------------------------
# Read data for plots and calculations
# ------------------------------------------------------------------------------
# Estimates
scpi_estimates <- read_csv("results/Fig4/scpi_estimates.csv")

# Inference 
scpi_inference <- read_csv("results/Fig4/scpi_inference.csv")

# Calculate MSE (Mean Squared Error)
mse <- 
  scpi_estimates |> 
  filter(name == "diff") |> 
  mutate(
    mse = value^2
  )

mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse

# Calculate RMSE (Root Mean Squared Error)
rmse_pre <- sqrt(mse_pre / (nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post / (nrow(mse |> filter(time_to_treat >= 0)) + 1))

# Ratio of post-treatment to pre-treatment RMSE
ratio <- rmse_post / rmse_pre

# ------------------------------------------------------------------------------
# Create SCM Plots Based on Results
# ------------------------------------------------------------------------------
# Define min and max for the y-axis
min_y <- exp(min((scpi_estimates |> filter(name == "obs"))$value))
max_y <- exp(round(max((scpi_inference$uci))))

# Prepare data for the plots
foo <- scpi_estimates |> 
  filter(!(name %in% c("diff", "lci", "uci"))) |> 
  mutate(
    name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"),
    date = ymd(period)
  )

# Combine inference results
scpi_inference <- scpi_inference |> 
  mutate(date = ymd(period)) |> 
  bind_rows(
    foo |> filter(time_to_treat == -1) |> filter(name == "Synthetic Mexico") |> 
      select(-period) |> mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
  )

# Plot the results
ggplot() +
  scale_color_manual(values = c(color4t, "black")) +
  geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +
  geom_vline(xintercept = naim_canc_date - 29) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  scale_y_continuous(breaks = seq(50, 130, 2.5)) +
  labs(x = "", y = "Monthly Economic Activity Index\n(Index base October 2018)", color = "", fill = "") +
  annotate("text", x = amlo_vic_date, y = -Inf, label = "Elections", angle = 90, size = 2, vjust = -0.5, hjust = -0.1) +
  annotate("text", x = naim_canc_date - 29, y = -Inf, label = "NAIM cancellation", angle = 90, size = 2, vjust = 1.5, hjust = -0.1) +
  geom_ribbon(data = scpi_inference, alpha = .66, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI")) +
  geom_ribbon(data = scpi_inference, alpha = .5, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values = c("grey70", "grey")) +
  geom_line(data = foo, aes(date, exp(value), color = name)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) +
  annotate("label", x = amlo_vic_date - 30, y = Inf, vjust = 1, size = 2, hjust = 1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = naim_canc_date + 30, y = Inf, vjust = 1, size = 2, hjust = 0, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(size = 4, angle = 90),
    axis.text.y = element_text(size = 8, angle = 0),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 5.5),
    plot.margin = unit(c(1.5,1,1,1), "lines"),
  )

# Save the plot as a PNG file
ggsave("results/Fig4/Fig4a.png", dpi = 300, width = 7.4, height = 4.8)

# ------------------------------------------------------------------------------
# ATT (Average Treatment Effect) Plot
# ------------------------------------------------------------------------------
inference_2 <- scpi_inference |> 
  filter(time_to_treat != -1) |> 
  # select(-value) |> 
  left_join(
    scpi_estimates |> 
      filter(name == "obs") |> 
      select(time_to_treat, value) |> 
      filter(time_to_treat >= 0)
  ) |> 
  mutate(
    lci_ins = value - lci_ins,
    uci_ins = value - uci_ins,
    lci_ofs = value - lci_ofs,
    uci_ofs = value - uci_ofs
  ) |> 
  select(-value)

min_y <- min((scpi_estimates |> filter(name == "diff"))$value)

# ATT Plot
scpi_estimates |> 
  filter(name == "diff") |> 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(breaks = seq(-.07, .035, .01)) +
  scale_x_continuous(breaks = c(seq(-70, 0, 5), seq(0, 15, 3))) +
  labs(x = "Relative time to treatment", y = "ATT", color = "", fill = "") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  geom_ribbon(data = inference_2, alpha = .25, aes(x = time_to_treat, ymin = lci_ins, ymax = uci_ins, fill = "95% In-sample PI")) +
  geom_ribbon(data = inference_2, alpha = .5, aes(x = time_to_treat, ymin = lci_ofs, ymax = uci_ofs, fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values = c("grey", "grey70")) +
  geom_line(aes(time_to_treat, value), color = color4t) +
  annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = 1.1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = -0.1, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))

# Save the plot as a PNG file
ggsave("results/Fig4/Fig4b.png", dpi = 300, width = 7.4, height = 4.8)
