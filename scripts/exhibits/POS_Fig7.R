# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig7"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 7: Quarterly Investment — ATT
# ------------------------------------------------------------------------------
# This section of code runs the SCM analysis and only executes if the result files
# do not already exist.

# Define file paths for the output results
scpi_estimates_file <- "results/Fig7/investment_scpi_estimates.csv"
scpi_inference_file <- "results/Fig7/investment_scpi_inference.csv"

# Check if result files already exist
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file)) {
  
  # ------------------------------------------------------------------------------
  # Data Preparation for SCM Analysis
  # ------------------------------------------------------------------------------
  foo <- posq |> 
    filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
    filter(year >= 2013) |> 
    filter(year_quarter < yearquarter("2020 Q1")) |> 
    select(countrycode, year_quarter, gdpi, cnsmpti, govi, capfi, importsi, exportsi, 
           gdp, cnsmpt, gov, capf, imports, exports, unemp, int_rate, ex_rate, naim) |> 
    distinct() |> 
    mutate(
      capfi = log(capfi),  # Log-transform investment
      ex_rate = log(ex_rate),  # Log-transform exchange rate
      int_rate = log(int_rate)  # Log-transform interest rate
    )
  
  # ------------------------------------------------------------------------------
  # Estimation of Residualized Investment (controlling for interest and exchange rates)
  # ------------------------------------------------------------------------------
  capfi_res <- feols(capfi ~ ex_rate + int_rate, foo)  # Estimate residuals of log investment on exchange rate and interest rate
  
  foo <- foo |> mutate(capfi_res = residuals(capfi_res))  # Store the residualized investment variable
  
  # ------------------------------------------------------------------------------
  # Defining Pre- and Post-Treatment Periods for SCM
  # ------------------------------------------------------------------------------
  period.labels <- as.character(levels(factor(foo$year_quarter)))  # Period labels for x-axis
  period <- yearquarter(levels(factor(foo$year_quarter)))  # Time variable as year-quarter
  time.tr <- yearquarter("2018 Q4")  # Treatment period (NAIM cancellation)
  
  # Pre-treatment and post-treatment periods
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  
  # Add time-to-treatment variable
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(
      time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))  # Relative time to treatment
    ) |> 
    ungroup()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))  # Convert period to numeric
  
  # ------------------------------------------------------------------------------
  # Set Options for SCM Estimation
  # ------------------------------------------------------------------------------
  u.alpha <- 0.05  # Confidence level for in-sample uncertainty
  e.alpha <- 0.1  # Confidence level for out-of-sample uncertainty
  rho <- NULL  # Regularization parameter (NULL means it's estimated)
  rho.max <- 1  # Maximum value for rho
  u.order <- 1  # Degree of polynomial for modeling u
  u.lags <- 0  # Lags for modeling u
  u.sigma <- "HC1"  # Variance-covariance estimator (HC1 for heteroskedasticity)
  u.missp <- TRUE  # Treat the model as misspecified
  e.lags <- 1  # Lags for modeling e
  e.order <- 0  # Polynomial order for modeling e
  e.method <- "all"  # Estimation method for out-of-sample uncertainty
  lgapp <- "linear"  # Linear method for lag adjustments
  cores <- parallel::detectCores()  # Number of cores to use
  sims <- 1000  # Number of simulations for inference
  
  # ------------------------------------------------------------------------------
  # Data Preparation for SCM Analysis
  # ------------------------------------------------------------------------------
  time.tr <- 0
  id.var      <- "countrycode"                             # ID variable
  time.var    <- "time_to_treat_naim"                                # Time variable
  period.pre  <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]                          # Post-treatment period
  unit.tr     <- "MEX"                                      # Treated unit (in terms of id.var)
  unit.co     <- unique(foo$countrycode)[-match( "MEX", unique(foo$countrycode))]                    # Donors pool
  outcome.var <- "capfi_res"                                 # Outcome variable
  cov.adj     <- NULL #list(c("constant"))     # Covariates for adjustment
  features    <- NULL #,                                  # No features other than outcome
  constant    <- T                                # Constant term
  report.missing <- FALSE                              # To check where missing values are
  cointegrated.data <- TRUE                            # Belief that the data are cointegrated
  w.constr <- list(name = "lasso")     # Simplex-type constraint set
  
  # Prepare the data for SCM analysis
  df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
               period.pre = period.pre, period.post = period.post,
               unit.tr = unit.tr, unit.co = unit.co, constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # Estimation using SCM
  # ------------------------------------------------------------------------------
  set.seed(1234)
  model <- scpi(
    w.constr = list(name = "lasso"),  # Use lasso-type constraint for SCM weights
    data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
    u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
    e.method = e.method, cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max
  )
  
  # Plot SCM results
  scplot(model)
  
  # Save SCM estimates, inference, and weights to CSV
  scpi_estimates <- sc_est(model, outcome.var, period, period.labels)
  write_csv(scpi_estimates, scpi_estimates_file)
  
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
  write_csv(scpi_inference, scpi_inference_file)
  
  # Extract and save SCM weights
  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
  )
  write_csv(weights, "results/Fig7/investment_scpi_weights.csv")
  
}

# ------------------------------------------------------------------------------
# Read Data for Plotting and MSE Calculation (always runs)
# ------------------------------------------------------------------------------
# Load estimates and inference data
scpi_estimates <- read_csv(scpi_estimates_file)
scpi_inference <- read_csv(scpi_inference_file)

# Calculate MSE and RMSE
mse <- scpi_estimates |> 
  filter(name == "diff") |> 
  mutate(mse = value^2)

mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse

rmse_pre <- sqrt(mse_pre / (nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post / (nrow(mse |> filter(time_to_treat >= 0)) + 1))

# Ratio of post-treatment RMSE to pre-treatment RMSE
ratio <- rmse_post / rmse_pre

# ------------------------------------------------------------------------------
# Plot ATT (Average Treatment Effect) with Prediction Intervals
# ------------------------------------------------------------------------------
inference_2 <- scpi_inference |> 
  filter(time_to_treat != -1) |> 
  left_join(scpi_estimates |> filter(name == "obs") |> select(time_to_treat, value)) |> 
  mutate(
    lci_ins = value - lci_ins,
    uci_ins = value - uci_ins,
    lci_ofs = value - lci_ofs,
    uci_ofs = value - uci_ofs
  ) |> 
  select(-value)

# Plot ATT with prediction intervals
scpi_estimates |> 
  filter(name == "diff") |> 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(breaks = seq(-0.2, 0.035, 0.02)) +
  scale_x_continuous(breaks = c(seq(-70, 0, 2), seq(0, 4, 2))) +
  labs(x = "Relative time to treatment (quarters)", y = "ATT", color = "", fill = "") +
  theme(axis.text.x = element_text(size = 8)) +
  
  geom_ribbon(data = inference_2, alpha = 0.25, aes(x = time_to_treat, ymin = lci_ins, ymax = uci_ins, fill = "95% In-sample PI")) +
  geom_ribbon(data = inference_2, alpha = 0.5, aes(x = time_to_treat, ymin = lci_ofs, ymax = uci_ofs, fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values = c("grey", "grey70")) +
  
  geom_line(aes(time_to_treat, value), color = color4t) +
  
  annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = 1.1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = -0.1, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))

# Save the ATT plot as a PNG file
ggsave("results/Fig7/Fig7.png", dpi = 300, width = 7.4, height = 4.8)

