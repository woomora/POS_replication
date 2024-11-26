# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB8"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.8: Sensitivity Analysis for Different Dates
# ------------------------------------------------------------------------------
# This script performs sensitivity analysis for the treatment effect by varying 
# the treatment start dates and evaluating the robustness of the SCM results.
# ------------------------------------------------------------------------------
# Data Preparation for SCM Analysis
# ------------------------------------------------------------------------------

foo <- 
  monindex |> 
  mutate(eaindex_trend = log(eaindex_trend)) |>  # Log transformation of the economic activity index
  filter(date <= ymd("2020-01-01")) |>           # Filter data up to January 2020
  filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
  filter(!is.na(eaindex_trend))                  # Exclude rows with missing values

# Define time periods
period.labels <- as.character(levels(factor(foo$date)))
period <- yearmonth(levels(factor(foo$year_month)))
time.tr <- yearmonth("2018 Oct")                 # Set the initial treatment period (NAIM cancellation)
period.pre <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
period.post <- period[c(match(time.tr, period):length(period))]   # Post-treatment period
period.labels.post <- period.labels[c(match(time.tr, period):length(period))]

# Prepare the data for SCM
foo <- 
  foo |> 
  group_by(countrycode) |> 
  mutate(
    time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) -1)),
    treat = if_else(time_to_treat_naim >= 0, 1, 0)  # Define treatment indicator
  ) |> 
  ungroup() |> 
  glimpse()

period <- as.numeric(levels(factor(foo$time_to_treat_naim)))

# ------------------------------------------------------------------------------
# Set Options for Data Preparation
# ------------------------------------------------------------------------------

time.tr <- 0  # Set the time of treatment
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

# Data preparation for SCM
df <- scdata(
  df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
  period.pre = period.pre, period.post = period.post,
  unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
  constant = constant, cointegrated.data = cointegrated.data
)

# ------------------------------------------------------------------------------
# Set Options for SCM Estimation
# ------------------------------------------------------------------------------

# Synthetic Control Method options
u.alpha  <- 0.05
e.alpha  <- 0.1
rho      <- NULL
rho.max  <- 1
V        <- NULL
u.order  <- 1
u.lags   <- 0
u.sigma  <- "HC2"
u.missp  <- TRUE
e.lags   <- 1
e.order  <- 0
e.method <- "all"
lgapp    <- "linear"
cores    <- parallel::detectCores()
sims     <- 1000
w.constr <- list(name = "simplex")   # Simplex-type constraint for SCM weights

# ------------------------------------------------------------------------------
# SCM Estimation
# ------------------------------------------------------------------------------

set.seed(1234)
model <- scpi(
  w.constr = w.constr, data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
  u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags, e.method = e.method, 
  cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max
)

# Plot the SCM results
scplot(model)

# ------------------------------------------------------------------------------
# Sensitivity Analysis for Alternative Treatment Dates
# ------------------------------------------------------------------------------
# February 2019 Sensitivity Analysis ----
# Repeat the SCM process for a different treatment date (Feb 2019) and evaluate sensitivity

sens = seq(0, 4, by = .5)  # Define sensitivity sequence
time.tr <- 0

e.alpha <- 0.05  # default level in scpi

emean <- model$inference.results$e.mean
esig <- sqrt(model$inference.results$e.var)
sc.l.0 <-  model$inference.results$CI.in.sample[,1,drop = F]
sc.r.0 <-  model$inference.results$CI.in.sample[,2,drop = F]
y <-  model$data$Y.post
y_hat <-  model$est.results$Y.post.fit
time <- 5

sen.dat <- tibble()

for (p in time) {
  
  ssc.l.1 <- ssc.r.1 <- c()
  e.mean <- emean[p-time.tr]
  sig <- esig[p-time.tr]
  sig.seq <- sens*sig
  
  for (s in 1:length(sig.seq)) {
    eps  <- sqrt(-log(e.alpha/2)*2*(sig.seq[s]^2))
    ssc.l.1[s] <- sc.l.0[p-time.tr] + e.mean - eps
    ssc.r.1[s] <- sc.r.0[p-time.tr] + e.mean + eps
  }
  
  foo <- 
    tibble(
      point_in_t = p,
      t=c(1:length(sens)), 
      lb1=ssc.l.1, 
      ub1=ssc.r.1,
      lb=rep(sc.l.0[p-time.tr], length(sens)),
      ub=rep(sc.r.0[p-time.tr], length(sens)),
      lab=as.factor(sens)
    ) |> 
    mutate_at(
      vars(lb1, ub1, lb, ub),
      funs(exp(.))
    ) |> 
    mutate(
      y_obs = y[p-time.tr],
      y_hat = y_hat[p-time.tr],
      diff_lb_y_obs = exp(y_obs- log(lb1)) -1,
    )
  
  sen.dat <- 
    sen.dat |> 
    bind_rows(
      foo
    )
  
}

median_x <- median(sen.dat$t)
y_obs <- median(sen.dat$y_obs)
y_hat <- median(sen.dat$y_hat)
diff <- exp(y_obs - y_hat) - 1
point_in_t <- as.numeric(levels(factor(sen.dat$point_in_t)))

plot <- 
  ggplot() + 
  # Synthetic Mexico
  geom_hline(yintercept = exp(y_hat), linetype=1, color = "black", size = 1) +
  annotate("text", x=median_x, y=exp(y_hat) + 0.5,
           label= str_c("Synthetic Mexico = ", round(exp(y_hat), 2)), size=3.5) +
  # Observed Mexico
  geom_hline(yintercept = exp(y_obs), linetype=1, color = color4t, size = 1) +
  annotate("text", x=median_x, y=exp(y_obs) - 0.5,
           label= str_c("Observed Mexico = ", round(exp(y_obs), 2)), size=3.5) +
  # Out-of-sample uncertainty
  geom_errorbar(data=sen.dat, aes(x=t, ymin=lb1, ymax=ub1), col="grey", width=0.2, linetype=5) +
  # In-sample uncertainty
  geom_errorbar(data=sen.dat, aes(x=t, ymin=lb, ymax=ub), col="black", width=0.2, linetype=1, alpha=.7) +
  # Text for percentage difference
  geom_text(data=sen.dat, aes(x=t, y = lb1*0.9975, label = round(diff_lb_y_obs, 3)), size = 3) +
  # Labels
  labs(
    x="sd. of e (out-of-sample uncertainty)", 
    y="Prediction Intervals",
    title = str_c("Estimate for date: ", period.labels.post[point_in_t]),
    subtitle = str_c("Difference between observed and synthetic: ", round(diff, 3))
  ) +
  scale_x_continuous(breaks = as.numeric(levels(factor(sen.dat$t))), labels = levels(factor(sen.dat$lab))) +
  theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 10))

# Display the plot
plot

# Save the plot as a PNG file
ggsave("results/FigB8/FigB8_Feb19.png", dpi = 300, width = 7.4, height = 4.8)

# ------------------------------------------------------------------------------
# December 2019 Sensitivity Analysis ----
# Repeat the SCM process for a different treatment date (Feb 2019) and evaluate sensitivity

sens = seq(0, 4, by = .5)  # Define sensitivity sequence
time.tr <- 0

e.alpha <- 0.05  # default level in scpi

emean <- model$inference.results$e.mean
esig <- sqrt(model$inference.results$e.var)
sc.l.0 <-  model$inference.results$CI.in.sample[,1,drop = F]
sc.r.0 <-  model$inference.results$CI.in.sample[,2,drop = F]
y <-  model$data$Y.post
y_hat <-  model$est.results$Y.post.fit
time <- length( model$data$specs$period.post)

sen.dat <- tibble()

for (p in time) {
  
  ssc.l.1 <- ssc.r.1 <- c()
  e.mean <- emean[p-time.tr]
  sig <- esig[p-time.tr]
  sig.seq <- sens*sig
  
  for (s in 1:length(sig.seq)) {
    eps  <- sqrt(-log(e.alpha/2)*2*(sig.seq[s]^2))
    ssc.l.1[s] <- sc.l.0[p-time.tr] + e.mean - eps
    ssc.r.1[s] <- sc.r.0[p-time.tr] + e.mean + eps
  }
  
  foo <- 
    tibble(
      point_in_t = p,
      t=c(1:length(sens)), 
      lb1=ssc.l.1, 
      ub1=ssc.r.1,
      lb=rep(sc.l.0[p-time.tr], length(sens)),
      ub=rep(sc.r.0[p-time.tr], length(sens)),
      lab=as.factor(sens)
    ) |> 
    mutate_at(
      vars(lb1, ub1, lb, ub),
      funs(exp(.))
    ) |> 
    mutate(
      y_obs = y[p-time.tr],
      y_hat = y_hat[p-time.tr],
      diff_lb_y_obs = exp(y_obs- log(lb1)) -1,
    )
  
  sen.dat <- 
    sen.dat |> 
    bind_rows(
      foo
    )
  
}

median_x <- median(sen.dat$t)
y_obs <- median(sen.dat$y_obs)
y_hat <- median(sen.dat$y_hat)
diff <- exp(y_obs - y_hat) - 1
point_in_t <- as.numeric(levels(factor(sen.dat$point_in_t)))

plot <- 
  ggplot() + 
  # Synthetic Mexico
  geom_hline(yintercept = exp(y_hat), linetype=1, color = "black", size = 1) +
  annotate("text", x=median_x, y=exp(y_hat) + 0.5,
           label= str_c("Synthetic Mexico = ", round(exp(y_hat), 2)), size=3.5) +
  # Observed Mexico
  geom_hline(yintercept = exp(y_obs), linetype=1, color = color4t, size = 1) +
  annotate("text", x=median_x, y=exp(y_obs) - 0.5,
           label= str_c("Observed Mexico = ", round(exp(y_obs), 2)), size=3.5) +
  # Out-of-sample uncertainty
  geom_errorbar(data=sen.dat, aes(x=t, ymin=lb1, ymax=ub1), col="grey", width=0.2, linetype=5) +
  # In-sample uncertainty
  geom_errorbar(data=sen.dat, aes(x=t, ymin=lb, ymax=ub), col="black", width=0.2, linetype=1, alpha=.7) +
  # Text for percentage difference
  geom_text(data=sen.dat, aes(x=t, y = lb1*0.9975, label = round(diff_lb_y_obs, 3)), size = 3) +
  # Labels
  labs(
    x="sd. of e (out-of-sample uncertainty)", 
    y="Prediction Intervals",
    title = str_c("Estimate for date: ", period.labels.post[point_in_t]),
    subtitle = str_c("Difference between observed and synthetic: ", round(diff, 3))
  ) +
  scale_x_continuous(breaks = as.numeric(levels(factor(sen.dat$t))), labels = levels(factor(sen.dat$lab))) +
  theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 10))

# Display the plot
plot

# Save the plot as a PNG file
ggsave("results/FigB8/FigB8_Dec19.png", dpi = 300, width = 7.4, height = 4.8)

#