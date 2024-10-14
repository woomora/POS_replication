# ------------------------------------------------------------------------------
# Figure B.2: Quarterly GDP SCM - controlling for goverment expenditure share
# ------------------------------------------------------------------------------
# This section of code runs the Synthetic Control Method (SCM) analysis and only executes 
# if the results files do not already exist.

# Define file paths for the output results
scpi_estimates_file <- "results/FigB2/scpi_estimates_controls.csv"
scpi_inference_file <- "results/FigB2/scpi_inference_controls.csv"
scpi_weights_file <- "results/FigB2/scpi_weights_controls.csv"

# Check if result files already exist
if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file) | !file.exists(scpi_weights_file)) {
  
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
      gdpi = log(gdpi),
      cnsmpts = cnsmpt / gdp,
      govs = gov / gdp,
      capfs = capf / gdp,
      importss = imports / gdp,
      exportss = exports / gdp,
      net_ex = exportss - importss,
    )
  
  # ------------------------------------------------------------------------------
  # Estimation of Residualized GDP (controlling for interest and exchange rates)
  # ------------------------------------------------------------------------------
  
  gdpi_res <- feols(gdpi ~ govs + log(ex_rate) + log(int_rate), foo)  # Estimate residuals from log GDP on exchange rate and interest rate
  
  foo <- foo |> mutate(gdpi_res = residuals(gdpi_res))  # Create new residualized GDP column
  
  # ------------------------------------------------------------------------------
  # Defining Pre- and Post-Treatment Periods for SCM
  # ------------------------------------------------------------------------------
  
  period.labels <- as.character(levels(factor(foo$year_quarter)))
  period <- yearquarter(levels(factor(foo$year_quarter)))
  time.tr <- yearquarter("2018 Q4")  # Define treatment period (NAIM cancellation)
  
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))) |> 
    ungroup() |> 
    glimpse()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))
  countries <- as.character(levels(factor(foo$countrycode)))
  
  # ------------------------------------------------------------------------------
  # Set Options for Data Preparation for SCM
  # ------------------------------------------------------------------------------
  
  time.tr <- 0  # Time of treatment (relative to time_to_treat_naim)
  
  id.var <- "countrycode"  # ID variable (countries)
  time.var <- "time_to_treat_naim"  # Time variable
  unit.tr <- "MEX"  # Treated unit (Mexico)
  unit.co <- unique(foo$countrycode)[-match("MEX", unique(foo$countrycode))]  # Donor pool
  outcome.var <- "gdpi_res"  # Outcome variable (residualized GDP)
  period.pre <- period[-c(match(time.tr, period):length(period))]  # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]  # Post-treatment period
  constant <- TRUE  # Include constant term
  cointegrated.data <- TRUE  # Assume data are cointegrated
  
  # Prepare the data for SCM analysis
  df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
               period.pre = period.pre, period.post = period.post, unit.tr = unit.tr, 
               unit.co = unit.co, constant = constant, cointegrated.data = cointegrated.data)
  
  # ------------------------------------------------------------------------------
  # Set SCM Estimation Options and Run SCM Model
  # ------------------------------------------------------------------------------
  
  # SCM Estimation parameters
  set.seed(1234)
  model <- scpi(
    w.constr = list(name = "simplex"),  # Simplex-type constraints for SCM weights
    data = df, u.order = 1, u.lags = 0, u.sigma = "HC1", u.missp = TRUE, sims = 1000,
    e.order = 0, e.lags = 1, e.method = "all", cores = parallel::detectCores(),
    u.alpha = 0.05, e.alpha = 0.1, rho = NULL, rho.max = 1
  )
  
  scplot(model)  # Plot SCM results
  
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
    country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
  )
  write_csv(weights, scpi_weights_file)
}

# ------------------------------------------------------------------------------
# Read Data for SCM Plots and MSE Calculation
# ------------------------------------------------------------------------------
# Read estimates and inference data from CSV files
scpi_estimates <- read_csv("results/FigB2/scpi_estimates_controls.csv")
scpi_inference <- read_csv("results/FigB2/scpi_inference_controls.csv")

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
ggsave("results/FigB2/FigB2.png", dpi = 300, width = 7.4, height = 4.8)
