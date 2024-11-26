# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigC4"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure C.4: Yearly GDP per capita — SCM ----
# ------------------------------------------------------------------------------
# This section of code runs the Synthetic Control Method (SCM) analysis and only executes 
# if the results files do not already exist.

# Define file paths for the output results
scpi_estimates_file <- "results/FigC4/scpi_estimates.csv"
scpi_inference_file <- "results/FigC4/scpi_inference.csv"

# Check if result files already exist
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file)) {
  
  # ------------------------------------------------------------------------------
  # Data Preparation for SCM Analysis
  # ------------------------------------------------------------------------------
  
  foo <- pos_year |> 
    filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |>
    filter(year >= 2009 & year <= 2022)  # Limit data to relevant years
  
  # Define pre- and post-treatment periods for SCM
  period.labels <- as.character(levels(factor(foo$year)))
  period <- as.numeric(levels(factor(foo$year)))
  
  time.tr <- 2018  # Define treatment period (AMLO takes office)
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))) |> 
    ungroup() |> 
    glimpse()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
  
  # ------------------------------------------------------------------------------
  # Set Options for Data Preparation for SCM
  # ------------------------------------------------------------------------------
  # Set options fo inference
  u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.1                         # Confidence level (out-of-sample uncertainty)
  rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                            # Maximum value attainable by rho
  V        <- NULL                         # Weighting matrix (if NULL it is the identity matrix)
  u.order  <- 0                            # Degree of polynomial in B and C when modelling u
  u.lags   <- 0                           # Lags of B to be used when modelling u
  u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u: HC2 (MacKinnon and White, 1985) adjusts for the leverage values hi where h is the diagonal of the projection matrix
  u.missp  <- T                            # If TRUE then the model is treated as misspecified
  e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
  e.order  <- 0                            # Lags of B to be used when modelling e
  e.method <- "all"                       # Estimation method for out-of-sample uncertainty
  lgapp    <- "linear" 
  cores    <- parallel::detectCores()      # Number of cores to be used by scpi
  sims     <- 1000                         # Number of simulations
  w.constr <- list(name = "simplex")     # Simplex-type constraint set
  
  # Set options for data preparation
  time.tr <- 0
  id.var      <- "countrycode"                                   # ID variable
  time.var    <- "time_to_treat_naim"                                # Time variable
  period.pre  <- period[-c(match(time.tr, period):length(period))]       # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]        # Post-treatment period
  unit.tr     <- "MEX"                                      # Treated unit (in terms of id.var)
  unit.co     <- unique(foo$countrycode)[-match( "MEX", unique(foo$countrycode))] 
  outcome.var <- "gdppc"                                  # Outcome variable
  cov.adj     <- list(c("constant"))     # Covariates for adjustment
  features    <- NULL                                  # No features other than outcome
  constant    <- F                                  # Constant term
  report.missing <- FALSE                              # To check where missing values are
  cointegrated.data <- TRUE                            # Belief that the data are cointegrated
  
  # Prepare the data for SCM analysis
  df  <-   scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
                  period.pre = period.pre, period.post = period.post,
                  unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
                  constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # Set SCM Estimation Options and Run SCM Model
  # ------------------------------------------------------------------------------
  
  # SCM Estimation parameters
  set.seed(1234)
  model <- 
    scpi(
      w.constr = list(name = constrain),
      data = df,u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
      u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
      e.method = e.method, cores = cores,  u.alpha = u.alpha,
      e.alpha = e.alpha, rho = rho, rho.max = rho.max
    )

  scplot(model)  # Plot SCM results
  
  # ------------------------------------------------------------------------------
  # Save SCM Results: Estimates and Inference
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
    country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
  )
  write_csv(weights, "results/FigC4/scpi_weights.csv")
  
}

# ------------------------------------------------------------------------------
# Read Data for SCM Plots and MSE Calculation
# ------------------------------------------------------------------------------
# Read estimates and inference data from CSV files
scpi_estimates <- read_csv("results/FigC4/scpi_estimates.csv")
scpi_inference <- read_csv("results/FigC4/scpi_inference.csv")

# ------------------------------------------------------------------------------
# Calculate MSE and RMSE
# ------------------------------------------------------------------------------
# Calculate the Mean Squared Error (MSE) for pre- and post-treatment periods
mse <- scpi_estimates |> 
  filter(name == "diff") |> 
  mutate(mse = value^2)

# Calculate RMSE (Root Mean Squared Error) for pre- and post-treatment periods
mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse

# Compute the RMSE by taking the square root of the MSE
rmse_pre <- sqrt(mse_pre / (nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post / (nrow(mse |> filter(time_to_treat >= 0)) + 1))

# Compute the ratio of post-treatment RMSE to pre-treatment RMSE
ratio <- rmse_post / rmse_pre

# ------------------------------------------------------------------------------
# Prepare Data for SCM Plot
# ------------------------------------------------------------------------------
# Create a data frame for plotting by transforming the scpi_estimates dataset
foo <- scpi_estimates |> 
  filter(!(name %in% c("diff", "lci", "uci"))) |> 
  mutate(name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"))

# Prepare labels for time_to_treat and period (x-axis)
labels <- scpi_estimates |> 
  filter(time_to_treat %% 2 != 1) |> 
  group_by(time_to_treat, period) |> 
  summarise() |> 
  ungroup()

# ------------------------------------------------------------------------------
# Plot SCM Results: Observed vs. Synthetic Mexico
# ------------------------------------------------------------------------------
foo |> 
  ggplot() +
  # Colors
  scale_color_manual(values = c(color4t, "black")) +
  # Vertical Lines
  geom_vline(xintercept = 2, linetype = "dotted") +
  geom_vline(xintercept = 0) +
  labs(x = "", y = outcome_name, color = "", fill = "") +
  scale_x_continuous(
    breaks = labels$time_to_treat,
    labels = labels$period
  ) +
  # In-sample
  geom_ribbon(
    data = scpi_inference,
    alpha = .66,
    aes(x = time_to_treat, ymin = (lci_ins), ymax = (uci_ins), fill = "95% In-sample PI"),
  ) +
  # Out-of-sample
  geom_ribbon(
    data = scpi_inference,
    alpha = .5,
    aes(x = time_to_treat, ymin = (lci_ofs), ymax = (uci_ofs), fill = "90% Out-of-sample PI"),
  ) +
  scale_fill_manual(values=c("grey", "grey70")) +
  # Observed vs Synthetic
  geom_line(aes(time_to_treat, (value), color = name)) +
  theme(
    axis.text.x = element_text(size = 6, angle = 0),
    axis.text.y = element_text(size = 8, angle = 0),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    plot.margin = unit(c(1.5,1,1,1), "lines"),
  ) +
  guides(
    color = guide_legend(nrow=1,byrow=TRUE),
    fill = guide_legend(nrow=1,byrow=TRUE)
  ) +
  # RMSPE
  annotate(
    "label", x = -1, y = Inf, vjust = 0,
    size = 2, hjust = 1.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_pre, 3))
  ) +
  annotate(
    "label", x = 0, y = Inf, vjust = 0,
    size = 2, hjust = -0.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_post, 3))
  ) +
  coord_cartesian(clip = "off") 

# Save the plot as a PNG file
ggsave("results/FigC4/FigC4a.png", dpi = 300, width = 7.4, height = 4.8)

# ------------------------------------------------------------------------------
# Plot ATT (Average Treatment Effect)
# ------------------------------------------------------------------------------
# Prepare data for ATT plot by calculating the differences
inference_2 <- 
  scpi_inference |> 
  left_join(
    scpi_estimates |> 
      filter(name == "obs") |> 
      select(time_to_treat, value)
  ) |> 
  mutate(
    lci_ins = value - lci_ins,
    uci_ins = value - uci_ins,
    lci_ofs = value - lci_ofs,
    uci_ofs = value - uci_ofs,
  ) |> 
  select(-value) 

# Plot ATT
scpi_estimates |> 
  filter(name == "diff") |> 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 2, linetype = "dotted") +
  
  scale_y_continuous(
    breaks = seq(-.2, .035, .02),
  ) +
  scale_x_continuous(
    breaks = seq(-10, 5, 1),
  ) +
  labs(x = "Relative time to treatment (years)", y = "ATT", color = "", fill = "")  +
  # In-sample
  geom_ribbon(
    data = inference_2,
    alpha = .25,
    aes(x = time_to_treat, ymin = (lci_ins), ymax = (uci_ins), fill = "95% In-sample PI"),
  ) +
  # Out-of-sample
  geom_ribbon(
    data = inference_2,
    alpha = .5,
    aes(x = time_to_treat, ymin = (lci_ofs), ymax = (uci_ofs), fill = "90% Out-of-sample PI"),
  ) +
  scale_fill_manual(values=c("grey", "grey70")) +
  geom_line(aes(time_to_treat, value), color = color4t)  +
  theme(
    axis.text.y = element_text(size = 9, angle = 0),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 9),
  )  +
  # RMSPE
  annotate(
    "label", x = 0, y = Inf, vjust = 0,
    size = 2, hjust = 1.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_pre, 3))
  ) +
  annotate(
    "label", x =0, y = Inf, vjust = 0,
    size = 2, hjust = -0.1, color="black", 
    label = str_c("RMSPE: ", round(rmse_post, 3))
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = unit(c(1.25,1,1,1), "lines")
  ) +
  annotate("text", x = 1.8, 
           y= -Inf, label="COVID-19", color="black", 
           angle = 90, size = 2.5, hjust = -0.2
  )

# Save the ATT plot as a PNG file
ggsave("results/FigC4/FigC4b.png", dpi = 300, width = 7.4, height = 4.8)
