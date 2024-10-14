# ------------------------------------------------------------------------------
# Figure B.7: Falsification (Placebo Test) in Other Countries
# ------------------------------------------------------------------------------
# This script performs a falsification test by applying the SCM to other countries 
# (excluding Mexico) to see if similar effects are found when the treatment 
# (NAIM cancellation) is hypothetically assigned to other countries.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

# Filter and transform the data for the falsification test
foo <- 
  monindex |> 
  mutate(eaindex_trend = log(eaindex_trend)) |>  # Log-transform the economic activity index
  filter(date <= ymd("2020-01-01")) |>  # Include data up to January 2020
  filter(countrycode != "MEX") |>  # Exclude Mexico as the treatment country
  filter(!is.na(eaindex_trend))  # Remove missing data

# Define the treatment and pre/post-treatment periods
period.labels <- as.character(levels(factor(foo$date)))
period <- yearmonth(levels(factor(foo$year_month)))
time.tr <- yearmonth("2018 Oct")  # Define the treatment date (NAIM cancellation)
period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period

# Add a time-to-treatment variable to the dataset
foo <- 
  foo |> 
  group_by(countrycode) |> 
  mutate(
    time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1)),
    treat = if_else(time_to_treat_naim >= 0, 1, 0)  # Binary treatment indicator
  ) |> 
  ungroup()

period <- as.numeric(levels(factor(foo$time_to_treat_naim)))  # Convert period to numeric

# ------------------------------------------------------------------------------
# Set Inference Options for SCM
# ------------------------------------------------------------------------------

u.alpha  <- 0.05                         # Confidence level for in-sample uncertainty
e.alpha  <- 0.1                          # Confidence level for out-of-sample uncertainty
rho      <- NULL                         # Regularization parameter (auto-estimated if NULL)
rho.max  <- 1                            # Maximum value for regularization parameter
V        <- NULL                         # Weighting matrix (identity if NULL)
u.order  <- 1                            # Degree of polynomial for modeling u
u.lags   <- 0                            # Lags for u in the model
u.sigma  <- "HC2"                        # Estimator for variance-covariance of u
u.missp  <- TRUE                         # Treat the model as misspecified
e.lags   <- 1                            # Degree of polynomial for modeling e
e.order  <- 0                            # Lags for e in the model
e.method <- "all"                        # Method for estimating out-of-sample uncertainty
cores    <- parallel::detectCores()      # Number of cores for parallel processing
sims     <- 1000                         # Number of simulations
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# ------------------------------------------------------------------------------
# Data Preparation Options for SCM
# ------------------------------------------------------------------------------

time.tr <- 0  # Time of treatment (relative to time_to_treat_naim)
id.var      <- "countrycode"                   # ID variable (country code)
time.var    <- "time_to_treat_naim"            # Time variable (relative to treatment)
period.pre  <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]   # Post-treatment period
outcome.var <- "eaindex_trend"                 # Outcome variable (economic activity trend)
cov.adj     <- NULL                            # No additional covariates
features    <- NULL                            # No extra features other than the outcome
constant    <- TRUE                            # Include a constant term
report.missing <- FALSE                        # Do not report missing values
cointegrated.data <- TRUE                      # Assume cointegrated data for time series

# ------------------------------------------------------------------------------
# Perform Falsification Analysis for Top 2 Donor Countries
# ------------------------------------------------------------------------------

# Read weights and select the top 2 contributing countries
weights <- 
  read_csv("results/Fig4/scpi_weights.csv") |>
  arrange(-weights) |> 
  head(2)  # Select the top 2 donors based on SCM weights

# Loop through the top 2 countries for the falsification test
for (c in levels(factor(weights$country))) {
  
  if (c != "MEX") {  # Ensure Mexico is excluded as the treatment unit
    
    print(c)  # Print the country being analyzed
    
    # Set the treated unit to the current country and update the donor pool
    unit.tr <- c
    unit.co <- unique(foo$countrycode)[-match(unit.tr, unique(foo$countrycode))]
    
    # Prepare data for SCM analysis
    df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
                 period.pre = period.pre, period.post = period.post,
                 unit.tr = unit.tr, unit.co = unit.co, 
                 constant = constant, cointegrated.data = cointegrated.data)
    
    # Perform SCM estimation
    set.seed(1234)
    model <- scpi(
      w.constr = list(name = w.constr),
      data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
      u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
      e.method = e.method, cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, 
      rho = rho, rho.max = rho.max
    )
    
    scplot(model)  # Plot SCM results
    
    # Save the SCM estimates
    scpi_estimates <- sc_est(model, outcome.var, period, period.labels)
    write_csv(scpi_estimates, str_c("results/FigB7/", c, "_scpi_estimates.csv"))
    
    # Save the inference results
    scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
    write_csv(scpi_inference, str_c("results/FigB7/", c, "_scpi_inference.csv"))
    
    # Save SCM weights
    weights <- tibble(
      weights = round(model$est.results$w, 4),
      country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
    )
    write_csv(weights, str_c("results/FigB7/", c, "_scpi_weights.csv"))
    
  }
  
}

# ------------------------------------------------------------------------------
# Plotting Falsification Results for "COL"
# ------------------------------------------------------------------------------

# Set country code for the plot
c = "COL"

# Load estimates and inference data
scpi_estimates <- read_csv(str_c("results/FigB7/", c, "_scpi_estimates.csv"))
scpi_inference <- read_csv(str_c("results/FigB7/", c, "_scpi_inference.csv"))

# Calculate MSE and RMSE for pre- and post-treatment periods
mse <- scpi_estimates |>
  filter(name == "diff") |>
  mutate(mse = value^2)
mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse
rmse_pre <- sqrt(mse_pre/(nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post/(nrow(mse |> filter(time_to_treat >= 0)) + 1))

# Prepare data for plotting
min_y <- exp((min(((scpi_estimates |> filter(name == "obs"))$value))))
max_y <- exp(round(max((scpi_inference$uci))))
foo_plot <- scpi_estimates |>
  filter(!(name %in% c("diff", "lci", "uci"))) |>
  mutate(name = if_else(name == "obs", "Observed Colombia", "Synthetic Colombia"), date = ymd(period))
scpi_inference <- scpi_inference |>
  mutate(date = ymd(period)) |>
  bind_rows(
    foo_plot |> filter(time_to_treat == -1) |> filter(name == "Synthetic Colombia") |>
      select(-period) |>
      mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
  )

# Plot the falsification results for "COL"
ggplot() +
  # Colors
  scale_color_manual(values = c(color4t, "black")) +
  # Vertical Lines for key events
  geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +
  geom_vline(xintercept = naim_canc_date - 29) +
  # X axis scale and labels
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  # Y axis scale and labels
  scale_y_continuous(breaks = seq(50, 130, 2.5)) +
  # Annotations
  labs(x = "", y = "Monthly Economic Activity Index\n(Index base October 2018)", color = "", fill = "") +
  annotate("text", x = amlo_vic_date, y = -Inf, label = "Elections", angle = 90, size = 2, vjust = -0.5, hjust = -0.1) +
  annotate("text", x = naim_canc_date - 29, y = -Inf, label = "NAIM cancelation", angle = 90, size = 2, vjust = 1.5, hjust = -0.1) +
  # In-sample and out-of-sample prediction intervals
  geom_ribbon(data = scpi_inference, alpha = .66, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI")) +
  geom_ribbon(data = scpi_inference, alpha = .5, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values = c("grey70", "grey")) +
  # Observed vs Synthetic Lines
  geom_line(data = foo_plot, aes(date, exp(value), color = name)) +
  # Theme adjustments
  theme(axis.text.x = element_text(size = 4, angle = 90), axis.text.y = element_text(size = 8), axis.title = element_text(size = 8), legend.text = element_text(size = 6), plot.margin = unit(c(1.5, 1, 1, 1), "lines")) +
  # Guides for the legends
  guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) +
  # RMSPE annotations
  annotate("label", x = amlo_vic_date - 30, y = Inf, vjust = 1, size = 2, hjust = 1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = naim_canc_date + 30, y = Inf, vjust = 1, size = 2, hjust = 0, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  coord_cartesian(clip = "off")

# Save the plot for "COL"
ggsave(str_c("results/FigB7/FigB7_", c, ".png"), dpi = 300, width = 7.4, height = 4.8)

# ------------------------------------------------------------------------------
# Repeat the same process for "USA"
# ------------------------------------------------------------------------------
# Set country code for the plot
c = "USA"

# Load estimates and inference data
scpi_estimates <- read_csv(str_c("results/FigB7/", c, "_scpi_estimates.csv"))
scpi_inference <- read_csv(str_c("results/FigB7/", c, "_scpi_inference.csv"))

# Calculate MSE and RMSE for pre- and post-treatment periods
mse <- scpi_estimates |>
  filter(name == "diff") |>
  mutate(mse = value^2)
mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse
rmse_pre <- sqrt(mse_pre/(nrow(mse |> filter(time_to_treat < 0)) + 1))
rmse_post <- sqrt(mse_post/(nrow(mse |> filter(time_to_treat >= 0)) + 1))

# Prepare data for plotting
min_y <- exp((min(((scpi_estimates |> filter(name == "obs"))$value))))
max_y <- exp(round(max((scpi_inference$uci))))
foo_plot <- scpi_estimates |>
  filter(!(name %in% c("diff", "lci", "uci"))) |>
  mutate(name = if_else(name == "obs", "Observed USA", "Synthetic USA"), date = ymd(period))
scpi_inference <- scpi_inference |>
  mutate(date = ymd(period)) |>
  bind_rows(
    foo_plot |> filter(time_to_treat == -1) |> filter(name == "Synthetic USA") |>
      select(-period) |>
      mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
  )

# Plot the falsification results for "COL"
ggplot() +
  # Colors
  scale_color_manual(values = c(color4t, "black")) +
  # Vertical Lines for key events
  geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +
  geom_vline(xintercept = naim_canc_date - 29) +
  # X axis scale and labels
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  # Y axis scale and labels
  scale_y_continuous(breaks = seq(50, 130, 2.5)) +
  # Annotations
  labs(x = "", y = "Monthly Economic Activity Index\n(Index base October 2018)", color = "", fill = "") +
  annotate("text", x = amlo_vic_date, y = -Inf, label = "Elections", angle = 90, size = 2, vjust = -0.5, hjust = -0.1) +
  annotate("text", x = naim_canc_date - 29, y = -Inf, label = "NAIM cancelation", angle = 90, size = 2, vjust = 1.5, hjust = -0.1) +
  # In-sample and out-of-sample prediction intervals
  geom_ribbon(data = scpi_inference, alpha = .66, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI")) +
  geom_ribbon(data = scpi_inference, alpha = .5, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI")) +
  scale_fill_manual(values = c("grey70", "grey")) +
  # Observed vs Synthetic Lines
  geom_line(data = foo_plot, aes(date, exp(value), color = name)) +
  # Theme adjustments
  theme(axis.text.x = element_text(size = 4, angle = 90), axis.text.y = element_text(size = 8), axis.title = element_text(size = 8), legend.text = element_text(size = 6), plot.margin = unit(c(1.5, 1, 1, 1), "lines")) +
  # Guides for the legends
  guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) +
  # RMSPE annotations
  annotate("label", x = amlo_vic_date - 30, y = Inf, vjust = 1, size = 2, hjust = 1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
  annotate("label", x = naim_canc_date + 30, y = Inf, vjust = 1, size = 2, hjust = 0, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
  coord_cartesian(clip = "off")

# Save the plot for "COL"
ggsave(str_c("results/FigB7/FigB7_", c, ".png"), dpi = 300, width = 7.4, height = 4.8)
