# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB12"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.12: Robustness: Different weighting schemes
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Define the list of constraints for SCM
constrains <- c("lasso", "ridge", "L1-L2", "ols")
# ------------------------------------------------------------------------------

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

gdpi_res <- feols(gdpi ~ log(ex_rate) + log(int_rate), foo)  # Estimate residuals from log GDP on exchange rate and interest rate

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


# Iterate over each constraint in the list
for (constraint in constrains) {
  
  # Define file paths for the output results for each constraint
  scpi_estimates_file <- paste0("results/FigB12/scpi_estimates_controls_", constraint, ".csv")
  scpi_inference_file <- paste0("results/FigB12/scpi_inference_controls_", constraint, ".csv")
  scpi_weights_file <- paste0("results/FigB12/scpi_weights_controls_", constraint, ".csv")
  
  # Check if result files already exist
  if (!file.exists(scpi_estimates_file) | !file.exists(scpi_inference_file) | !file.exists(scpi_weights_file)) {
    
    
    # ------------------------------------------------------------------------------
    # Set SCM Estimation Options and Run SCM Model
    # ------------------------------------------------------------------------------
    
    # SCM Estimation parameters
    set.seed(1234)
    model <- scpi(
      w.constr = list(name = constraint),  # Use the current constraint
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
    # Prepare Data for SCM Plot
    # ------------------------------------------------------------------------------
    foo <- scpi_estimates |> 
      filter(!(name %in% c("diff", "lci", "uci"))) |> 
      mutate(name = if_else(name == "obs", "Observed Mexico", "Synthetic Mexico"),
             date = ymd(period))
    
    labels <- scpi_estimates |> 
      filter(time_to_treat %% 2 != 1) |> 
      group_by(time_to_treat, period) |> 
      summarise() |> 
      ungroup()
    
    min_y <- min((scpi_estimates |> filter(name == "obs"))$value)
    max_y <- round(max(scpi_inference$uci))
    
    scpi_inference <- scpi_inference |> 
      mutate(date = ymd(period)) |> 
      bind_rows(
        foo |> filter(time_to_treat == -1) |> filter(name == "Synthetic Mexico") |> 
          select(-period) |> 
          mutate(lci_ins = value, uci_ins = value, lci_ofs = value, uci_ofs = value)
      )
    
    # ------------------------------------------------------------------------------
    # Plot SCM Results: Observed vs Synthetic Mexico
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
    
    ggsave(paste0("results/FigB12/FigB12_", constraint, "_observed_synthetic.png"), dpi = 300, width = 7.4, height = 4.8)
    
    # ------------------------------------------------------------------------------
    # Plot ATT (Average Treatment Effect)
    # ------------------------------------------------------------------------------
    # Prepare data for ATT plot by calculating the differences
    inference_2 <- scpi_inference |> 
      filter(time_to_treat != -1) |> 
      select(-value) |> 
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
    
    # Create the ATT plot
    scpi_estimates |> 
      filter(name == "diff") |> 
      ggplot() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      scale_y_continuous(breaks = seq(-0.15, 0.045, 0.01)) +
      scale_x_continuous(breaks = c(seq(-70, 0, 4), seq(0, 4, 2))) +
      labs(x = "Relative time to treatment (quarters)", y = "ATT", color = "", fill = "") +
      theme(axis.text.x = element_text(size = 8, angle = 0)) +
      
      # Add in-sample and out-of-sample prediction intervals (PI)
      geom_ribbon(data = inference_2, alpha = .25, aes(x = time_to_treat, ymin = lci_ins, ymax = uci_ins, fill = "95% In-sample PI")) +
      geom_ribbon(data = inference_2, alpha = .5, aes(x = time_to_treat, ymin = lci_ofs, ymax = uci_ofs, fill = "90% Out-of-sample PI")) +
      scale_fill_manual(values = c("grey", "grey70")) +
      # Plot the ATT (average treatment effect)
      geom_line(aes(time_to_treat, value), color = color4t) +
      
      # Add RMSPE labels
      annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = 1.1, color = "black", label = str_c("RMSPE: ", round(rmse_pre, 3))) +
      annotate("label", x = 0, y = Inf, vjust = 0, size = 2, hjust = -0.1, color = "black", label = str_c("RMSPE: ", round(rmse_post, 3))) +
      
      coord_cartesian(clip = "off") +
      theme(plot.margin = unit(c(1.25, 1, 1, 1), "lines"))
    
    ggsave(paste0("results/FigB12/FigB12_", constraint, "_att.png"), dpi = 300, width = 7.4, height = 4.8)
  }
}
