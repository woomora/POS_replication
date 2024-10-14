# Function to estimate synthetic control results ----
sc_est <- function(sc_model = model, outcome_var = outcome.var, period = period, period_labels = period.labels) {
  
  # Extract observed and synthetic control results for pre and post periods
  observed <- rbind(sc_model$data$Y.pre, sc_model$data$Y.post)
  synthetic <- rbind(sc_model$est.results$Y.pre.fit, sc_model$est.results$Y.post.fit)
  
  # Create tibble with observed, synthetic, and period information
  results <- tibble(
    obs = observed[, outcome_var],
    synth = synthetic[, 1],
    time_to_treat = period,
    period = period_labels
  ) %>% 
    # Calculate dynamic ATT (%) as the difference between observed and synthetic in exponential scale
    mutate(diff = (exp(obs - synth) - 1)) %>% 
    # Reshape data to long format
    pivot_longer(!c(period, time_to_treat))
  
  return(results)
}

# Function to calculate confidence intervals (in-sample and out-of-sample) -----
sc_inf <- function(sc_model = model, post_tr = period.post, period_labels = period.labels, alpha = 0, gaussian = FALSE) {
  
  # Create tibble with periods, in-sample, and out-of-sample results
  inference_data <- tibble(
    period = tail(period_labels, n = length(post_tr)),
    time_to_treat = seq(0, length(post_tr) - 1),
    lci_ins = sc_model$inference.results$CI.in.sample[, 1],
    uci_ins = sc_model$inference.results$CI.in.sample[, 2],
    lci_ofs_gau = sc_model$inference.results$CI.all.gaussian[, 1],
    uci_ofs_gau = sc_model$inference.results$CI.all.gaussian[, 2],
    lci_ofs_qreg = sc_model$inference.results$CI.all.qreg[, 1],
    uci_ofs_qreg = sc_model$inference.results$CI.all.qreg[, 2]
  )
  
  # Adjust confidence intervals based on Gaussian or quantile regression
  if (gaussian) {
    inference_data <- inference_data %>% 
      mutate(
        lci_ofs = lci_ofs_gau,
        uci_ofs = uci_ofs_gau, 
        lci = alpha * lci_ins + (1 - alpha) * lci_ofs,
        uci = alpha * uci_ins + (1 - alpha) * uci_ofs
      )
  } else {
    inference_data <- inference_data %>% 
      mutate(
        lci_ofs = lci_ofs_qreg, 
        uci_ofs = uci_ofs_qreg, 
        lci = alpha * lci_ins + (1 - alpha) * lci_ofs,
        uci = alpha * uci_ins + (1 - alpha) * uci_ofs
      )
  }
  
  return(inference_data)
}

# Function to conduct sensitivity analysis of synthetic control estimates -----
sc_sens <- function(sc_model = model, monthly = TRUE, sens = seq(0, 3, by = 0.5)) {
  
  e.alpha <- 0.05  # Default confidence level
  emean <- sc_model$inference.results$e.mean
  esig <- sqrt(sc_model$inference.results$e.var)
  
  # Choose time points based on monthly or quarterly data
  time_points <- if (monthly) c(14, length(sc_model$data$specs$period.post)) else c(4, length(sc_model$data$specs$period.post))
  
  sens_results <- tibble()
  
  # Iterate over time points and sensitivity values to calculate bounds
  for (p in time_points) {
    e.mean <- emean[p]
    sig <- esig[p]
    sig_seq <- sens * sig
    
    ssc_l <- sapply(sig_seq, function(sig_s) sc_model$inference.results$CI.in.sample[p, 1] + e.mean - sqrt(-log(e.alpha / 2) * 2 * (sig_s^2)))
    ssc_r <- sapply(sig_seq, function(sig_s) sc_model$inference.results$CI.in.sample[p, 2] + e.mean + sqrt(-log(e.alpha / 2) * 2 * (sig_s^2)))
    
    # Store results in a tibble
    sens_results <- sens_results %>%
      bind_rows(tibble(
        point_in_t = p,
        t = seq_along(sens),
        lb1 = exp(ssc_l),
        ub1 = exp(ssc_r),
        lb = rep(exp(sc_model$inference.results$CI.in.sample[p, 1]), length(sens)),
        ub = rep(exp(sc_model$inference.results$CI.in.sample[p, 2]), length(sens)),
        lab = factor(sens)
      ))
  }
  
  return(sens_results)
}

# Plotting functions ----
# Function to plot synthetic control estimates with confidence intervals and events marked ----
plot_sc <- function(estimates, inference = NULL, monthly = TRUE, covid = FALSE, unit_name = "", outcome_name = "") {
  
  # Prepare data for plotting
  min_y <- exp(min(estimates %>% filter(name == "obs")$value))
  foo <- estimates %>% 
    filter(!(name %in% c("diff", "lci", "uci"))) %>% 
    mutate(
      name = if_else(name == "obs", paste0("Observed ", unit_name), paste0("Synthetic ", unit_name)),
      date = ymd(period)
    )
  
  # Generate the plot with vertical lines for events
  p1 <- ggplot(foo) +
    geom_vline(xintercept = amlo_vic_date, alpha = 0.5) +
    geom_vline(xintercept = naim_canc_date, alpha = 0.5, size = 1.5) +
    scale_x_date(date_breaks = "4 month", date_labels = "%b %y") +
    scale_y_continuous(breaks = seq(50, 130, 5)) +
    labs(x = "", y = outcome_name, color = "", fill = "") +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 12))
  
  # Add inference ribbons if available
  if (!is.null(inference)) {
    p1 <- p1 + 
      geom_ribbon(data = inference, aes(x = date, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI"), alpha = 0.5) +
      geom_ribbon(data = inference, aes(x = date, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI"), alpha = 0.5) +
      scale_fill_manual(values = c("grey", "grey70")) +
      geom_line(aes(date, exp(value), color = name))
  } else {
    p1 <- p1 + geom_line(aes(date, exp(value), color = name))
  }
  
  # Annotate special events
  p1 <- p1 + 
    annotate("text", x = amlo_vic_date - 30, y = min_y, label = "Elections", angle = 90, size = 4, hjust = -0.5) +
    annotate("text", x = naim_canc_date - 30, y = min_y, label = "NAIM cancelation", angle = 90, size = 4, hjust = -0.25)
  
  # Add COVID event if applicable
  if (covid) {
    p1 <- p1 + 
      geom_vline(xintercept = covid_date - 15, linetype = "dotted") +
      annotate("text", x = covid_date - 30, y = min_y, label = "COVID-19", angle = 90, size = 4, hjust = -0.5)
  }
  
  return(p1)
}

# SC Constraints ----
# Define different types of constraints for synthetic control models
simplex <-  list(p = "L1", lb = 0, Q = 1, dir = "==")          # Simplex constraint: L1-norm, non-negative weights, sum to 1
lasso <-  list(p = "L1", lb = -Inf, Q = 1, dir = "<=")         # Lasso constraint: L1-norm, weights not constrained to be non-negative, sum less than or equal to 1
ridge <-  list(p = "L2", lb = -Inf, Q = 1, dir = "<=")         # Ridge constraint: L2-norm, weights not constrained to be non-negative, sum less than or equal to 1
l1l2 <- list(p = "L1-L2", lb = 0, Q = 1, Q2 = 1, dir = "==/<=")# L1-L2 hybrid constraint: combination of L1 and L2 norms

# Function to select the appropriate constraint based on the input
#' @param constrain A string specifying the type of constraint to apply. Options are "simplex", "lasso", "ridge", and "l1l2".
#' @return A list containing the parameters for the specified constraint.
sc_constrain <- function(constrain = "simplex") {
  
  # Define a named list of available constraints
  constraints <- list(
    simplex = simplex,
    lasso = lasso,
    ridge = ridge,
    l1l2 = l1l2
  )
  
  # Check if the provided constraint type is valid
  if (!constrain %in% names(constraints)) {
    stop("Invalid constraint type. Choose from: 'simplex', 'lasso', 'ridge', or 'l1l2'.")
  }
  
  # Return the selected constraint
  return(constraints[[constrain]])
}

# Example usage:
# constraint_params <- sc_constrain("ridge")
# Numbers to strings ----
number_to_string <- 
  function(x = x){
    
    require(english)
    require(numberize)
    
    x <- words(abs(x)*1000000)
    x <- (str_split(x, "billion", simplify = T)[,1])
    x <- str_c(x, "billion")
    x <- numberize(x)
    x <- formatC(x/(1000000*1000))
    
    if(str_count(x)==4){
      x <- str_c(str_sub(x, end = 1), ",", str_sub(x, start = 2), " billion")
    }else{
      x <- str_c(x, " billion")
    }
    
    return(x)
    
  }