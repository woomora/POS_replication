# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.3: Robustness Check — Backdating Treatment 9 Months Prior to NAIM Cancellation
# ------------------------------------------------------------------------------
# This script performs a robustness check by backdating the treatment (NAIM cancellation)
# to 9 months before the actual event. It uses Synthetic Control Method (SCM) to evaluate
# the effect on the monthly economic activity index.

# Define file paths for the output results
scpi_estimates_file <- "results/FigB3/scpi_estimates_backdating9.csv"
scpi_inference_file <- "results/FigB3/scpi_inference_backdating9.csv"
scpi_weights_file <- "results/FigB3/scpi_weights_backdating9.csv"

# Check if result files already exist; if not, proceed with analysis
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file) | !file.exists(scpi_weights_file)) {
  
  # ------------------------------------------------------------------------------
  # Data Preparation
  # ------------------------------------------------------------------------------
  
  # Filter and preprocess the monthly economic activity index data
  foo <- 
    monindex |> 
    mutate(eaindex_trend = log(eaindex_trend)) |>  # Log-transform the trend variable
    filter(date <= ymd("2020-01-01")) |>  # Filter data up to January 2020
    filter(
      (countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN"))  # Select relevant countries
    ) |> 
    filter(!is.na(eaindex_trend))  # Remove rows with missing trend data
  
  # Define pre- and post-treatment periods
  period.labels <- as.character(levels(factor(foo$date)))
  period <- yearmonth(levels(factor(foo$year_month)))
  time.tr <- yearmonth("2018 Oct")  # Backdate 9 months before actual treatment
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  period.labels.post <- period.labels[c(match(time.tr, period):length(period))]
  
  # ------------------------------------------------------------------------------
  # Creating Time to Treatment and Treatment Indicators
  # ------------------------------------------------------------------------------
  
  foo <- 
    foo |> 
    group_by(countrycode) |> 
    mutate(
      time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1)),  # Define time relative to treatment
      treat = if_else(time_to_treat_naim >= 0, 1, 0)  # Create treatment indicator
    ) |> 
    ungroup() |> 
    glimpse()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
  
  # ------------------------------------------------------------------------------
  # SCM Estimation Options and Data Preparation
  # ------------------------------------------------------------------------------
  
  # Set options for inference
  u.alpha <- 0.05  # Confidence level for in-sample uncertainty
  e.alpha <- 0.1  # Confidence level for out-of-sample uncertainty
  rho <- NULL  # Regularization parameter
  rho.max <- 1  # Maximum value for rho
  V <- NULL  # Weighting matrix
  u.order <- 1  # Degree of polynomial for modeling u
  u.lags <- 0  # Lags of B to be used when modeling u
  u.sigma <- "HC2"  # Estimator for variance-covariance of u
  u.missp <- TRUE  # Treat the model as misspecified
  e.lags <- 1  # Degree of polynomial for modeling e
  e.order <- 0  # Lags of B to be used when modeling e
  e.method <- "all"  # Estimation method for out-of-sample uncertainty
  cores <- parallel::detectCores()  # Number of cores for parallelization
  sims <- 1000  # Number of simulations
  w.constr <- list(name = "simplex")  # Simplex-type constraint set
  
  # Set options for data preparation
  time.tr <- -9  # Backdate treatment by 9 months
  id.var <- "countrycode"  # ID variable
  time.var <- "time_to_treat_naim"  # Time variable
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  unit.tr <- "MEX"  # Treated unit
  unit.co <- unique(foo$countrycode)[-match("MEX", unique(foo$countrycode))]  # Donor pool
  outcome.var <- "eaindex_trend"  # Outcome variable
  constant <- TRUE  # Include constant term
  cointegrated.data <- TRUE  # Assume cointegrated data
  
  # Prepare data for SCM analysis
  df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
               period.pre = period.pre, period.post = period.post,
               unit.tr = unit.tr, unit.co = unit.co, constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # SCM Estimation
  # ------------------------------------------------------------------------------
  
  set.seed(1234)  # Set seed for reproducibility
  model <- scpi(
    w.constr = list(name = w.constr),  # Simplex-type constraints for SCM weights
    data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
    u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
    e.method = e.method, cores = cores, u.alpha = u.alpha,
    e.alpha = e.alpha, rho = rho, rho.max = rho.max
  )
  
  # Plot SCM results
  scplot(model)
  
  # ------------------------------------------------------------------------------
  # Save SCM Results: Estimates, Inference, and Weights
  # ------------------------------------------------------------------------------
  
  # Extract and save estimates
  scpi_estimates <- sc_est(model, outcome.var, period, period.labels)
  write_csv(scpi_estimates, scpi_estimates_file)
  
  # Extract and save inference results
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
  write_csv(scpi_inference, scpi_inference_file)
  
  # Extract and save SCM weights
  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), str_c(unit.tr,"."))
  )
  write_csv(weights, scpi_weights_file)
  
}

# ------------------------------------------------------------------------------
# Read Data for Plots
# ------------------------------------------------------------------------------

# Load results for plotting
scpi_estimates <- read_csv(scpi_estimates_file)
scpi_inference <- read_csv(scpi_inference_file)

# ------------------------------------------------------------------------------
# MSE and RMSE Calculation
# ------------------------------------------------------------------------------

# Calculate Mean Squared Error (MSE) for pre- and post-treatment periods
mse <- scpi_estimates |> filter(name == "diff") |> mutate(mse = value^2)
mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse
rmse_pre <- sqrt(mse_pre / (nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post / (nrow(mse |> filter(time_to_treat >= 0)) + 1))
ratio <- rmse_post / rmse_pre

# ------------------------------------------------------------------------------
# Plotting the Results with Detailed Customization
# ------------------------------------------------------------------------------

# Set backdate period for annotation and plot preparation
backdate <- 9

# Calculate minimum and maximum y-axis values for the plot
min_y <- exp(min((scpi_estimates |> filter(name == "obs"))$value))
max_y <- exp(round(max(scpi_inference$uci)))

# Prepare data frame for plotting observed vs synthetic control
foo <- scpi_estimates |> 
  filter(!(name %in% c("diff", "lci", "uci"))) |> 
  mutate(
    name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"),
    date = ymd(period)
  )

# Combine inference data for in-sample and out-of-sample prediction intervals
scpi_inference <- scpi_inference |> 
  mutate(date = ymd(period)) |> 
  bind_rows(
    foo |> 
      filter(time_to_treat == -backdate) |> 
      filter(name == "Synthetic Mexico") |> 
      select(-period) |> 
      mutate(
        lci_ins = value,
        uci_ins = value,
        lci_ofs = value,
        uci_ofs = value
      )
  )

# Plot observed vs synthetic control along with prediction intervals and event annotations
ggplot() +
  # Define colors
  scale_color_manual(values = c(color4t, "black")) +
  # Add vertical lines for key events
  geom_vline(xintercept = naim_canc_date - 30*backdate, linetype = "dotted") +
  geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +
  geom_vline(xintercept = naim_canc_date - 29) +
  # Define x-axis with custom date breaks
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  # Define y-axis with custom scale
  scale_y_continuous(breaks = seq(50, 130, 2.5)) +
  # Add labels and titles
  labs(x = "", y = "Monthly Economic Activity Index\n(Index base October 2018)", color = "", fill = "") +
  # Add text annotations for events
  annotate("text", x = naim_canc_date - 30*backdate, y = -Inf, label="Backdated treatment", angle = 90, size = 2, vjust=-0.5, hjust=-0.1) +
  annotate("text", x = amlo_vic_date, y = -Inf, label="Elections", angle = 90, size = 2, vjust=-0.5, hjust=-0.1) +
  annotate("text", x = naim_canc_date - 29, y = -Inf, label="NAIM cancellation", angle = 90, size = 2, vjust=1.5, hjust=-0.1) +
  # Add in-sample prediction interval
  geom_ribbon(data = scpi_inference, alpha = .66, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI")) +
  # Add out-of-sample prediction interval
  geom_ribbon(data = scpi_inference, alpha = .5, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values=c("grey70", "grey")) +
  # Add observed and synthetic control lines
  geom_line(data = foo, aes(date, exp(value), color = name)) +
  # Customize theme
  theme(
    axis.text.x = element_text(size = 4, angle = 90),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    plot.margin = unit(c(1.5,1,1,1), "lines")
  ) +
  guides(color = guide_legend(nrow=1, byrow=TRUE), fill = guide_legend(nrow=1, byrow=TRUE)) +
  # Annotate RMSPE values for pre- and post-treatment
  annotate("label", x = naim_canc_date - 30*backdate, y = Inf, vjust = 1, size = 2, hjust = 1, color="black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = naim_canc_date + 30, y = Inf, vjust = 1, size = 2, hjust = 0, color="black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  coord_cartesian(clip = "off")

# Save the plot
ggsave("results/FigB3/FigB3.png", dpi = 300, width = 7.4, height = 4.8)
