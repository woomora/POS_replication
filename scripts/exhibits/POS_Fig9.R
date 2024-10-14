# ------------------------------------------------------------------------------
# Figure 9: Medium-run Economic ATT on Quarterly GDP — SCM Analysis
# ------------------------------------------------------------------------------

# Define file paths for the output results
scpi_estimates_file <- "results/Fig9/scpi_estimates_longrung.csv"
scpi_inference_file <- "results/Fig9/scpi_inference_longrung.csv"
scpi_weights_file <- "results/Fig9/scpi_weights_longrung.csv"
fig9_plot_file <- "results/Fig9/Fig9.png"

# Check if result files already exist
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file) | !file.exists(scpi_weights_file) | !file.exists(fig9_plot_file)) {
  
  # ------------------------------------------------------------------------------
  # Data Preparation for SCM Analysis
  # ------------------------------------------------------------------------------
  
  foo <- posq |> 
    filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
    filter(year >= 2013) |> 
    filter(year_quarter <= yearquarter("2023 Q3")) |> 
    select(countrycode, year_quarter, gdpi, cnsmpti, govi, capfi, importsi, exportsi, 
           gdp, cnsmpt, gov, capf, imports, exports, unemp, int_rate, ex_rate, naim) |> 
    distinct() |> 
    mutate(
      gdpi = log(gdpi),                     # Log transformation for GDP index
      cnsmpts = cnsmpt/gdp,                 # Consumption as a ratio of GDP
      govs = gov/gdp,                       # Government spending as a ratio of GDP
      capfs = capf/gdp,                     # Investment as a ratio of GDP
      importss = imports/gdp,               # Imports as a ratio of GDP
      exportss = exports/gdp,               # Exports as a ratio of GDP
      net_ex = exportss - importss,         # Net exports
      ex_rate = log(ex_rate),               # Log transformation for exchange rate
      int_rate = log(int_rate)              # Log transformation for interest rate
    )
  
  # ------------------------------------------------------------------------------
  # Estimation of Residualized GDP (controlling for interest and exchange rates)
  # ------------------------------------------------------------------------------
  
  gdpi_res <- feols(gdpi ~ govs + net_ex + ex_rate + int_rate, foo)  # Regress GDP on exchange and interest rates
  foo <- foo |> mutate(gdpi_res = residuals(gdpi_res))  # Compute residualized GDP
  
  # ------------------------------------------------------------------------------
  # Defining Pre- and Post-Treatment Periods for SCM
  # ------------------------------------------------------------------------------
  
  period.labels <- as.character(levels(factor(foo$year_quarter)))  # Create labels for periods
  period <- yearquarter(levels(factor(foo$year_quarter)))          # Define periods
  time.tr <- yearquarter("2018 Q4")                                # Define treatment period (NAIM cancellation)
  
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(
      time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))
    ) |> 
    ungroup()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
  countries <- as.character(levels(factor(foo$countrycode)))
  
  # ------------------------------------------------------------------------------
  # Set Options for SCM Analysis
  # ------------------------------------------------------------------------------
  
  u.alpha  <- 0.05                        # In-sample confidence level
  e.alpha  <- 0.1                         # Out-of-sample confidence level
  rho      <- NULL                        # Regularization parameter
  rho.max  <- 1                           # Maximum rho value
  u.order  <- 1                           # Degree of polynomial in modeling u
  u.lags   <- 0                           # Lags of B to be used when modeling u
  u.sigma  <- "HC1"                       # Variance-covariance estimator for u
  u.missp  <- TRUE                        # Misspecification handling
  e.lags   <- 1                           # Lags in modeling e
  e.order  <- 0                           # Degree of polynomial in modeling e
  e.method <- "all"                       # Estimation method for out-of-sample uncertainty
  cores    <- parallel::detectCores()     # Number of cores for SCM
  sims     <- 1000                        # Number of simulations
  w.constr <- list(name = "simplex")      # Constraint for SCM weights
  
  time.tr <- 0
  id.var      <- "countrycode"                             # ID variable
  time.var    <- "time_to_treat_naim"                                # Time variable
  period.pre  <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]                          # Post-treatment period
  unit.tr     <- "MEX"                                      # Treated unit (in terms of id.var)
  unit.co     <- unique(foo$countrycode)[-match( "MEX", unique(foo$countrycode))]                    # Donors pool
  outcome.var <- "gdpi_res"                                 # Outcome variable
  cov.adj     <- NULL #list(c("constant"))     # Covariates for adjustment
  features    <- NULL #,                                  # No features other than outcome
  constant    <- T                                # Constant term
  report.missing <- FALSE                              # To check where missing values are
  cointegrated.data <- TRUE                            # Belief that the data are cointegrated
  
  
  
  # ------------------------------------------------------------------------------
  # Prepare the Data for SCM Analysis
  # ------------------------------------------------------------------------------
  
  df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
               period.pre = period.pre, period.post = period.post, unit.tr = unit.tr, 
               unit.co = unit.co, constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # Estimation using SCM
  # ------------------------------------------------------------------------------
  
  set.seed(1234)
  model <- scpi(
    w.constr = list(name = w.constr), data = df, u.order = u.order, u.lags = u.lags, 
    u.sigma = u.sigma, u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
    e.method = e.method, cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max
  )
  
  scplot(model)  # Plot SCM results
  
  # ------------------------------------------------------------------------------
  # Save SCM Results: Estimates, Inference, and Weights
  # ------------------------------------------------------------------------------
  
  scpi_estimates <- sc_est(model, outcome.var, period, period.labels)
  write_csv(scpi_estimates, scpi_estimates_file)
  
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
  write_csv(scpi_inference, scpi_inference_file)
  
  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), str_c(unit.tr,"."))
  )
  write_csv(weights, scpi_weights_file)
  
}

# ------------------------------------------------------------------------------
# Read Data for SCM Plots and MSE Calculation
# ------------------------------------------------------------------------------
scpi_estimates <- read_csv(scpi_estimates_file)
scpi_inference <- read_csv(scpi_inference_file)

# ------------------------------------------------------------------------------
# Calculate MSE and RMSE
# ------------------------------------------------------------------------------
mse <- scpi_estimates |> 
  filter(name == "diff") |> 
  mutate(mse = value^2)

mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse

rmse_pre <- sqrt(mse_pre / (nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post / (nrow(mse |> filter(time_to_treat >= 0)) + 1))
ratio <- rmse_post / rmse_pre

# ------------------------------------------------------------------------------
# Prepare Data for ATT Plot
# ------------------------------------------------------------------------------
inference_2 <- scpi_inference |> 
  filter(time_to_treat != -1) |> 
  left_join(
    scpi_estimates |> filter(name == "obs") |> select(time_to_treat, value)
  ) |> 
  mutate(
    lci_ins = value - lci_ins,
    uci_ins = value - uci_ins,
    lci_ofs = value - lci_ofs,
    uci_ofs = value - uci_ofs
  ) |> 
  select(-value)

# ------------------------------------------------------------------------------
# Plot ATT
# ------------------------------------------------------------------------------
scpi_estimates |> 
  filter(name == "diff") |> 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(breaks = seq(-.3, .035, .03)) +
  scale_x_continuous(breaks = c(seq(-70, 25, 5))) +
  labs(x = "Relative time to treatment (quarters)", y = "ATT", color = "", fill = "") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  # In-sample prediction intervals
  geom_ribbon(
    data = inference_2,
    alpha = .25,
    aes(x = time_to_treat, ymin = lci_ins, ymax = uci_ins, fill = "95% In-sample PI")
  ) +
  # Out-of-sample prediction intervals
  geom_ribbon(
    data = inference_2,
    alpha = .5,
    aes(x = time_to_treat, ymin = lci_ofs, ymax = uci_ofs, fill = "90% Out-of-sample PI")
  ) +
  scale_fill_manual(values=c("grey", "grey70")) +
  geom_line(aes(time_to_treat, value), color = color4t) +
  theme(
    axis.text.y = element_text(size = 9, angle = 0),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 9),
  ) +
  # Annotate RMSPE
  annotate(
    "label", x = 0, y = Inf, vjust = 0, size = 2, hjust = 1.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_pre, 3))
  ) +
  annotate(
    "label", x = 0, y = Inf, vjust = 0, size = 2, hjust = -0.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_post, 3))
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))

# Save the ATT plot as a PNG file
ggsave(fig9_plot_file, dpi = 300, width = 7.4, height = 4.8)
