# ------------------------------------------------------------------------------
# Figure B.6: Robustness Check — Alternative Donor Pool
# ------------------------------------------------------------------------------
# This script conducts a Synthetic Control Method (SCM) analysis using an alternative 
# donor pool of countries and runs the model to assess robustness by excluding 
# specific countries and variables.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------
foo <- posq |> 
  # Filter countries in upper-middle-income group, excluding specific countries
  filter(
    countrycode %in% upper_middle_income$countrycode & 
      !(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "USA", "CAN")) |
      countrycode == "MEX"
  ) |> 
  # Exclude other unwanted countries
  filter(!(countrycode %in% c("BGR", "MDA", "JAM"))) |> 
  filter(year >= 2013) |> 
  filter(year_quarter < yearquarter("2020 Q1")) |> 
  # Select relevant columns
  select(countrycode, year_quarter, gdpi, cnsmpti, govi, capfi, importsi, exportsi, 
         gdp, cnsmpt, gov, capf, imports, exports, unemp, int_rate, ex_rate, naim) |> 
  distinct() |> 
  # Transform variables for the model
  mutate(
    gdpi = log(gdpi),
    cnsmpts = cnsmpt / gdp,
    govs = gov / gdp,
    capfs = capf / gdp,
    importss = imports / gdp,
    exportss = exports / gdp,
    net_ex = exportss - importss,
    ex_rate = log(1 + ex_rate),
    int_rate = log(1 + int_rate)
  )

# Check filtered country codes
table(foo$countrycode)

# Visualize panel data for the variables
panelview(net_ex ~ naim, data = foo, index = c("countrycode", "year_quarter"))

# ------------------------------------------------------------------------------
# Residualization of GDP (controlling for trade balance, exchange, and interest rates)
# ------------------------------------------------------------------------------
gdpi_res <- feols(gdpi ~ govs + net_ex + ex_rate + int_rate, foo)

# Add residualized GDP (gdpi_res) to the dataset
foo <- foo |> mutate(gdpi_res = residuals(gdpi_res))

# Additional panel view of residualized variables
panelview(int_rate ~ naim, data = foo, index = c("countrycode", "year_quarter"))

# ------------------------------------------------------------------------------
# Define Pre- and Post-Treatment Periods for SCM
# ------------------------------------------------------------------------------
period.labels <- as.character(levels(factor(foo$year_quarter)))

period <- yearquarter(levels(factor(foo$year_quarter)))
length(period)
time.tr <- yearquarter("2018 Q4")
period.pre  <- period[-c(match(time.tr, period):length(period))]       # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]        # Post-treatment period

period.labels.post <- period.labels[c(match(time.tr, period):length(period))]   


foo <- 
  foo |> 
  group_by(countrycode) |> 
  mutate(
    time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1))
  ) |> 
  ungroup() |> 
  glimpse()

period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
countries <- as.character(levels(factor(foo$countrycode)))
# ------------------------------------------------------------------------------
# SCM Estimation: Setup and Execution
# ------------------------------------------------------------------------------
# Set estimation parameters
u.alpha <- 0.05        # Confidence level (in-sample uncertainty)
e.alpha <- 0.1         # Confidence level (out-of-sample uncertainty)
rho <- NULL            # Regularization parameter
rho.max <- 1           # Max value for rho
V <- NULL              # Weighting matrix
u.order <- 1           # Polynomial degree for modeling u
u.lags <- 0            # Number of lags for modeling u
u.sigma <- "HC1"       # Variance-covariance estimator for u
u.missp <- TRUE        # Misspecification model
e.lags <- 1            # Lags for modeling e
e.order <- 0           # Polynomial degree for modeling e
e.method <- "all"      # Method for out-of-sample uncertainty estimation
lgapp <- "linear" 
cores <- parallel::detectCores()  # Use all available cores
sims <- 1000                      # Number of simulations
w.constr <- list(name = "simplex") # Constraint for weight construction

# Set options for data preparation
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


# Prepare data for SCM analysis
df <- scdata(df = foo, id.var = "countrycode", time.var = "time_to_treat_naim", 
             outcome.var = "gdpi_res", period.pre = period.pre, period.post = period.post,
             unit.tr = "MEX", unit.co = unique(foo$countrycode)[-match("MEX", unique(foo$countrycode))], 
             constant = TRUE, cointegrated.data = TRUE)

# Run the SCM estimation model
set.seed(1234)
model <- scpi(w.constr = list(name = w.constr), data = df, u.order = u.order, u.lags = u.lags, 
              u.sigma = u.sigma, u.missp = u.missp, sims = sims, e.order = e.order, 
              e.lags = e.lags, e.method = e.method, cores = cores, u.alpha = u.alpha, 
              e.alpha = e.alpha, rho = rho, rho.max = rho.max)

# Plot results
scplot(model)

# ------------------------------------------------------------------------------
# Save SCM Results: Estimates, Inference, and Weights
# ------------------------------------------------------------------------------
scpi_estimates <- sc_est(model, "gdpi_res", period, period.labels)
write_csv(scpi_estimates, "results/FigB6/scpi_estimates_alt_donor_pool.csv")

scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
write_csv(scpi_inference, "results/FigB6/scpi_inference_alt_donor_pool.csv")

weights <- tibble(weights = round(model$est.results$w, 4),
                  country = str_remove(names(model$est.results$w), "MEX."))
write_csv(weights, "results/FigB6/scpi_weights_alt_donor_pool.csv")

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
  mutate(name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"),
         date = ymd(period))

# Prepare labels for time_to_treat and period (x-axis)
labels <- scpi_estimates |> 
  filter(time_to_treat %% 2 != 1) |> 
  group_by(time_to_treat, period) |> 
  summarise() |> 
  ungroup()

# Calculate minimum and maximum y-axis values for the plot
min_y <- min((scpi_estimates |> filter(name == "obs"))$value)
max_y <- round(max(scpi_inference$uci))

# Combine inference data for in-sample and out-of-sample prediction intervals
scpi_inference <- scpi_inference |> 
  mutate(date = ymd(period)) |> 
  bind_rows(
    foo |> filter(time_to_treat == -1) |> filter(name == "Synthetic Mexico") |> 
      select(-period) |> 
      mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
  )

# ------------------------------------------------------------------------------
# Plot SCM Results: Observed vs. Synthetic Mexico
# ------------------------------------------------------------------------------
# Levels
foo |> 
  ggplot() +
  # Colors
  scale_color_manual(values = c(color4t, "black")) +
  # Vertical Lines
  geom_vline(xintercept = -1, linetype = "dotted") +
  geom_vline(xintercept = 0) +
  labs(x = "", y = "Quarterly GDP Index (log, residualized)", color = "", fill = "") +
  scale_x_continuous(
    breaks = labels$time_to_treat,
    labels = labels$period
  ) +
  theme(
    axis.text.x = element_text(size = 6, angle = 0),
    axis.text.y = element_text(size = 12, angle = 0),
  ) + 
  annotate("text", x = -1, 
           y= -Inf, label="Elections",
           angle = 90, size = 2, 
           vjust=-0.5, hjust= -0.1
  ) +
  annotate("text", x = 0, 
           y = -Inf, label="NAIM cancelation",
           angle = 90, size = 2, 
           vjust=1.5, hjust= -0.1
  )  +
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
ggsave("results/FigB6/FigB6.png", dpi = 300, width = 7.4, height = 4.8)

