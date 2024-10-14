# ------------------------------------------------------------------------------
# Figure B.4: Leave-One-Out (LOO) Analysis and Plot for Monthly Economic Activity Index
# ------------------------------------------------------------------------------
# This script performs a robustness check using SCM with Leave-One-Out (LOO) analysis
# for the two top donor countries. Econometric estimation is conditional on whether
# results files already exist.

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

foo <- 
  monindex |> 
  mutate(eaindex_trend = log(eaindex_trend)) |>  # Log-transform the trend variable
  filter(date <= ymd("2020-01-01")) |>  # Filter data up to January 2020
  filter(!is.na(eaindex_trend))  # Remove rows with missing trend data

# Define pre- and post-treatment periods
period.labels <- as.character(levels(factor(foo$date)))
period <- yearmonth(levels(factor(foo$year_month)))
time.tr <- yearmonth("2018 Oct")  # Set the treatment date (NAIM cancellation)
period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period

# Add time to treatment variable to the data
foo <- 
  foo |> 
  group_by(countrycode) |> 
  mutate(time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1))) |> 
  ungroup()

period <- as.numeric(levels(factor(foo$time_to_treat_naim)))  # Convert period to numeric

# ------------------------------------------------------------------------------
# Set SCM Inference Options
# ------------------------------------------------------------------------------

u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
e.alpha  <- 0.1                          # Confidence level (out-of-sample uncertainty)
rho      <- NULL                         # Regularization parameter
rho.max  <- 1                            # Maximum value for rho
V        <- NULL                         # Weighting matrix
u.order  <- 1                            # Degree of polynomial for modeling u
u.lags   <- 0                            # Lags of B to be used when modeling u
u.sigma  <- "HC1"                        # Variance-covariance estimator for u
u.missp  <- TRUE                         # Treat the model as misspecified
e.lags   <- 1                            # Degree of polynomial for modeling e
e.order  <- 0                            # Lags of B to be used when modeling e
e.method <- "all"                        # Method for estimating out-of-sample uncertainty
cores    <- parallel::detectCores()      # Number of cores for parallelization
sims     <- 1000                         # Number of simulations
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# ------------------------------------------------------------------------------
# Set Data Preparation Options
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

# ------------------------------------------------------------------------------
# Select the Top 2 Donor Countries Based on Weights
# ------------------------------------------------------------------------------

# Load original SCM weights and select the top 2 contributing countries
weights <- read_csv("results/Fig4/scpi_weights.csv") |>
  arrange(-weights) |> 
  head(2)  # Select top 2 donors based on weights

# ------------------------------------------------------------------------------
# Perform Leave-One-Out (LOO) Analysis if Files Don't Exist
# ------------------------------------------------------------------------------

for(c in levels(factor(weights$country))){
  
  if(c != "MEX"){  # Exclude Mexico from the donor countries
    
    print(c)  # Print the country being left out for tracking progress
    
    results_file <- str_c(path, "/results/FigB4/scpi_estimates_loo_", c, ".csv")
    
    if (!file.exists(results_file)) {  # Only run the estimation if the file doesn't exist
      
      foo1 <- foo |> filter(countrycode != c)  # Filter out the current donor country
      
      # Update donor pool by excluding the current country
      unit.co <- unique(foo1$countrycode)[-match(unit.tr, unique(foo1$countrycode))]
      
      # Prepare data for SCM analysis
      df <- scdata(df = foo1, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
                   period.pre = period.pre, period.post = period.post,
                   unit.tr = unit.tr, unit.co = unit.co, 
                   constant = constant, cointegrated.data = cointegrated.data)
      
      # Perform SCM estimation using the prepared data
      set.seed(1234)  # Set seed for reproducibility
      model <- scpi(
        w.constr = list(name = "simplex"),
        data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
        u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
        e.method = e.method, cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, 
        rho = rho, rho.max = rho.max
      )
      
      # Save SCM estimates
      scpi_estimates <- sc_est(model, outcome.var, period, period.labels)
      write_csv(scpi_estimates, results_file)
      
      # Save inference results
      scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
      write_csv(scpi_inference, str_c(path, "/results/FigB4/scpi_inference_loo_", c, ".csv"))
      
      # Save SCM weights
      weights <- tibble(
        weights = round(model$est.results$w, 4),
        country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
      )
      write_csv(weights, str_c(path, "/results/FigB4/scpi_weights_loo_", c, ".csv"))
      
    }
    
  }
  
}

# ------------------------------------------------------------------------------
# Generate Plots for Each Country
# ------------------------------------------------------------------------------

# Function to generate the plot for a given country
plot_synthetic_control <- function(country_code) {
  
  # Load estimates and inference data
  scpi_estimates <- read_csv(str_c(path, "/results/FigB4/scpi_estimates_loo_", country_code, ".csv"))
  scpi_inference <- read_csv(str_c(path, "/results/FigB4/scpi_inference_loo_", country_code, ".csv"))
  
  # Calculate RMSE
  mse <- scpi_estimates |> filter(name == "diff") |> mutate(mse = value^2)
  mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
  mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse
  rmse_pre <- sqrt(mse_pre/(nrow(mse |> filter(time_to_treat < 0)) + 1))
  rmse_post <- sqrt(mse_post/(nrow(mse |> filter(time_to_treat >= 0)) + 1))
  
  # Prepare data for plotting
  min_y <- exp((min(((scpi_estimates |> filter(name == "obs"))$value))))
  max_y <- exp(round(max((scpi_inference$uci))))
  
  foo_plot <- scpi_estimates |>
    filter(!(name %in% c("diff", "lci", "uci"))) |> 
    mutate(name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"), date = ymd(period))
  
  scpi_inference <- scpi_inference |> 
    mutate(date = ymd(period)) |> 
    bind_rows(
      foo_plot |> filter(time_to_treat == -1) |> filter(name == "Synthetic Mexico") |> 
        select(-period) |> mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
    )
  
  # Generate the plot
  ggplot() +
    # Colors
    scale_color_manual(values = c(color4t, "black")) +
    # Vertical Lines
    geom_vline(xintercept = amlo_vic_date, linetype = "dotted") +
    geom_vline(xintercept = naim_canc_date - 29) +
    # x axis scale dates
    scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
    # y axis scale
    scale_y_continuous(breaks = seq(50, 130, 2.5)) +
    # Labels
    labs(x = "", y = "Monthly Economic Activity Index\n(Index base October 2018)", color = "", fill = "") +
    annotate("text", x = amlo_vic_date, y = -Inf, label = "Elections", angle = 90, size = 2, vjust = -0.5, hjust = -0.1) +
    annotate("text", x = naim_canc_date - 29, y = -Inf, label = "NAIM cancelation", angle = 90, size = 2, vjust = 1.5, hjust = -0.1) +
    # In-sample
    geom_ribbon(data = scpi_inference, alpha = .66, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI")) +
    # Out-of-sample
    geom_ribbon(data = scpi_inference, alpha = .5, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI")) +
    scale_fill_manual(values = c("grey70", "grey")) +
    # Observed vs Synthetic
    geom_line(data = foo_plot, aes(date, exp(value), color = name)) +
    theme(axis.text.x = element_text(size = 4, angle = 90), axis.text.y = element_text(size = 8, angle = 0), axis.title = element_text(size = 8), legend.text = element_text(size = 6), plot.margin = unit(c(1.5, 1, 1, 1), "lines")) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) +
    # RMSPE
    annotate("label", x = amlo_vic_date - 30, y = Inf, vjust = 1, size = 2, hjust = 1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
    annotate("label", x = naim_canc_date + 30, y = Inf, vjust = 1, size = 2, hjust = 0, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
    coord_cartesian(clip = "off")
  
  # Save the plot
  ggsave(str_c("results/FigB4/FigB4_", country_code, ".png"), dpi = 300, width = 7.4, height = 4.8)
  
}

# ------------------------------------------------------------------------------
# Plot for "COL"
# ------------------------------------------------------------------------------
plot_synthetic_control("COL")

# ------------------------------------------------------------------------------
# Plot for "USA"
# ------------------------------------------------------------------------------
plot_synthetic_control("USA")
