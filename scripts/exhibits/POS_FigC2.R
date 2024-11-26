# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigC2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure C.2: GDP Components — ATT----
# ------------------------------------------------------------------------------
# Function to run SCM analysis for GDP components
# ------------------------------------------------------------------------------

run_scm_analysis <- function(component, log_transform = TRUE, output_name) {
  
  # ------------------------------------------------------------------------------
  # Data Loading and Preparation
  # ------------------------------------------------------------------------------
  foo <- posq |> 
    filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
    filter(year >= 2013) |> 
    filter(year_quarter < yearquarter("2020 Q1")) |> 
    select(countrycode, year_quarter, all_of(component), int_rate, ex_rate, naim) |>  # Selecting component dynamically
    distinct() |> 
    mutate(
      across(all_of(component), ~ log(.x)),  # Log-transform component
      ex_rate = log(ex_rate),   # Log-transform exchange rate
      int_rate = log(int_rate)  # Log-transform interest rate
    )
  
  # ------------------------------------------------------------------------------
  # Residualization of the component (controlling for interest and exchange rates)
  # ------------------------------------------------------------------------------
  component_res <- feols(as.formula(paste0(component, " ~ ex_rate + int_rate")), foo)  # Dynamically create formula
  foo <- foo |> mutate(!!paste0(component, "_res") := residuals(component_res))  # Store residualized component
  
  # ------------------------------------------------------------------------------
  # Defining Pre- and Post-Treatment Periods for SCM
  # ------------------------------------------------------------------------------
  period.labels <- as.character(levels(factor(foo$year_quarter)))  # Period labels for x-axis
  period <- yearquarter(levels(factor(foo$year_quarter)))          # Time variable as year-quarter
  time.tr <- yearquarter("2018 Q4")                                # Treatment period (NAIM cancellation)
  
  # Pre-treatment and post-treatment periods
  period.pre <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]   # Post-treatment period
  
  # Add time-to-treatment variable
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(
      time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))  # Relative time to treatment
    ) |> 
    ungroup()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))  # Convert period to numeric
  
  # ------------------------------------------------------------------------------
  # Data Preparation for SCM
  # ------------------------------------------------------------------------------
  # Set options for data preparation
  time.tr <- 0
  id.var      <- "countrycode"                             # ID variable
  time.var    <- "time_to_treat_naim"                                # Time variable
  period.pre  <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]                          # Post-treatment period
  unit.tr     <- "MEX"                                      # Treated unit (in terms of id.var)
  unit.co     <- unique(foo$countrycode)[-match( "MEX", unique(foo$countrycode))]                    # Donors pool
  outcome.var <- paste0(component, "_res")                                 # Outcome variable
  cov.adj     <- NULL #list(c("constant"))     # Covariates for adjustment
  features    <- NULL #,                                  # No features other than outcome
  constant    <- T                                # Constant term
  report.missing <- FALSE                              # To check where missing values are
  cointegrated.data <- TRUE                            # Belief that the data are cointegrated
  w.constr <- list(name = "lasso")     # Simplex-type constraint set
  
  df  <-   scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
                  period.pre = period.pre, period.post = period.post,
                  unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
                  constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # Estimation using SCM
  # ------------------------------------------------------------------------------
  set.seed(1234)
  model <- scpi(
    w.constr = list(name = "lasso"), data = df, u.order = 1, u.lags = 0, u.sigma = "HC1",
    u.missp = TRUE, sims = 1000, e.order = 0, e.lags = 1, e.method = "all", cores = parallel::detectCores(),
    u.alpha = 0.05, e.alpha = 0.1, rho = NULL, rho.max = 1
  )
  
  # Save SCM estimates and inference
  scpi_estimates <- sc_est(model, paste0(component, "_res"), period, period.labels)
  write_csv(scpi_estimates, paste0("results/FigC2/", output_name, "_scpi_estimates.csv"))
  
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)
  write_csv(scpi_inference, paste0("results/FigC2/", output_name, "_scpi_inference.csv"))
  
  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
  )
  write_csv(weights, paste0("results/FigC2/", output_name, "_scpi_weights.csv"))
  
  # ------------------------------------------------------------------------------
  # Plotting ATT (Average Treatment Effect) with Prediction Intervals
  # ------------------------------------------------------------------------------
  # MSE
  mse <- 
    scpi_estimates |> 
    filter(name == "diff") |> 
    mutate(
      mse = value^2,
    )
  
  mse_pre <- (mse |> filter(time_to_treat < 0) |> summarise(mse = sum(mse)))$mse
  mse_post <- (mse |> filter(time_to_treat >= 0) |> summarise(mse = sum(mse)))$mse
  
  rmse_pre <- sqrt(mse_pre/(nrow(mse |> filter(time_to_treat < 0)) + 1))
  rmse_post <- sqrt(mse_post/(nrow(mse |> filter(time_to_treat >= 0)) + 1))
  
  ratio <- rmse_post/rmse_pre
  
  inference_2 <- scpi_inference |> 
    filter(time_to_treat != -1) |> 
    left_join(scpi_estimates |> filter(name == "obs") |> select(time_to_treat, value)) |> 
    mutate(
      lci_ins = value - lci_ins, uci_ins = value - uci_ins,
      lci_ofs = value - lci_ofs, uci_ofs = value - uci_ofs
    ) |> 
    select(-value)
  
  scpi_estimates |> 
    filter(name == "diff") |> 
    ggplot() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(breaks = c(seq(-70, 0, 2), seq(0, 4, 2))) +
    labs(x = "Relative time to treatment (quarters)", y = "ATT", color = "", fill = "") +
    theme(axis.text.x = element_text(size = 8, angle = 0)) +
    geom_ribbon(data = inference_2, alpha = 0.25, aes(x = time_to_treat, ymin = lci_ins, ymax = uci_ins, fill = "95% In-sample PI")) +
    geom_ribbon(data = inference_2, alpha = 0.5, aes(x = time_to_treat, ymin = lci_ofs, ymax = uci_ofs, fill = "90% Out-of-sample PI")) +
    scale_fill_manual(values = c("grey", "grey70")) +
    geom_line(aes(time_to_treat, value), color = color4t) +
    annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = 1.1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
    annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = -0.1, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))
  
  # Save the plot as a PNG file
  ggsave(paste0("results/FigC2/FigC2_", output_name, ".png"), dpi = 300, width = 7.4, height = 4.8)
}

# ------------------------------------------------------------------------------
# Running the analysis for different components
# ------------------------------------------------------------------------------

# Run SCM for consumption
run_scm_analysis(component = "cnsmpti", output_name = "1consumption")

# Run SCM for government expenditure
run_scm_analysis(component = "govi", output_name = "2government_expenditure")

# Run SCM for exports
run_scm_analysis(component = "exportsi", output_name = "3exports")

# Run SCM for imports
run_scm_analysis(component = "importsi", output_name = "4imports")

