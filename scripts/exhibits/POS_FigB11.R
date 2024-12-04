# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB11"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.11: Heterogeneity: Within-Mexico Regions ----
# ------------------------------------------------------------------------------
# Banxico Regionalization
# ------------------------------------------------------------------------------
rnorte <- c("Baja California", "Baja California Sur", "Coahuila de Zaragoza", 
            "Chihuahua", "Nuevo León", "Sinaloa", "Sonora", "Tamaulipas")

rcentro_norte <- c("Aguascalientes", "Colima", "Durango", "Guanajuato", 
                   "Jalisco", "Nayarit", "San Luis Potosí", "Zacatecas")

rcentro <- c("Ciudad de México", "México")

rcentro_sur <- c("Guerrero", "Hidalgo", "Michoacán de Ocampo", 
                 "Morelos", "Puebla", "Querétaro", "Tlaxcala")

rsur_sureste <- c("Campeche", "Chiapas", "Oaxaca", "Quintana Roo", 
                  "Tabasco", "Veracruz de Ignacio de la Llave", "Yucatán")

# ------------------------------------------------------------------------------
# Economic Activity by State
# ------------------------------------------------------------------------------
rqmex <- read_csv("data/source/INEGI/ITAEE/itaee_indice_desest.csv") |> 
  janitor::clean_names() |> 
  pivot_longer(!entidad_federativa) |> 
  mutate(
    year = as.numeric(str_sub(name, 2, 5)),
    quarter = str_sub(name, 8),
    year_quarter = yearquarter(str_c(year, " Q", quarter), fiscal_start = 1),
    date = case_when(
      quarter == "1" ~ ymd(str_c(year, "-", "04", "-01")),
      quarter == "2" ~ ymd(str_c(year, "-", "07", "-01")),
      quarter == "3" ~ ymd(str_c(year, "-", "10", "-01")),
      quarter == "4" ~ ymd(str_c(year + 1, "-", "01", "-01"))
    ),
    country =  "MEX",
    region = case_when(
      entidad_federativa %in% rnorte ~ "North",
      entidad_federativa %in% rcentro_norte ~ "Centre-North",
      entidad_federativa %in% rcentro ~ "Centre",
      entidad_federativa %in% rcentro_sur ~ "Centre-South",
      entidad_federativa %in% rsur_sureste ~ "South-Southeasth"
    ),
    amlo_vic = if_else(country == "MEX" & date >= ymd("2018-07-01"), 1, 0),
    naim = if_else(country == "MEX" & date >= ymd("2018-11-01"), 1, 0),
    covid = if_else(country == "MEX" & date >= ymd("2020-02-01"), 1, 0)
  ) |> 
  rename(state = entidad_federativa, eaindex = value) |> 
  select(-name)

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------
rqmex <- rqmex |> 
  filter(year >= 2010, year_quarter < yearquarter("2020 Q1")) |> 
  left_join(
    rqmex |> filter(year == 2018, quarter == 3) |> 
      select(state, eaindex) |> rename(eaindex_base = eaindex)
  ) |> 
  mutate(eaindexi = (eaindex / eaindex_base) * 100) |> 
  select(-eaindex_base)

rqmex_regions <- rqmex |> 
  group_by(region, year_quarter) |> 
  summarise(gdpi = mean(eaindexi)) |> 
  mutate(countrycode = region) |> 
  ungroup() |> 
  mutate(
    gdpi = log(gdpi)
  )

# ------------------------------------------------------------------------------
# Set SCM Inference Options
# ------------------------------------------------------------------------------
u.alpha  <- 0.05
e.alpha  <- 0.1
rho      <- NULL
rho.max  <- 1
V        <- NULL
u.order  <- 1
u.lags   <- 0
u.sigma  <- "HC1"
u.missp  <- TRUE
e.lags   <- 1
e.order  <- 0
e.method <- "all"
lgapp    <- "linear"
cores    <- parallel::detectCores()
sims     <- 1000
w.constr <- list(name = "simplex")

# ------------------------------------------------------------------------------
# Loop through each region and generate plots
# ------------------------------------------------------------------------------
regions <- levels(factor(rqmex_regions$countrycode))

for (r in regions) {
  region <- r
  
  # Prepare data for SCM
  foo <- rqmex_regions |> 
    filter(countrycode == r) |> 
    left_join(
      posq |> 
        filter(year_quarter < yearquarter("2020 Q1")) |> 
        filter(countrycode == "MEX") |> 
        select(year_quarter, ex_rate, int_rate)
    ) |> 
    bind_rows(
      posq |> 
        filter(year_quarter >= yearquarter("2010 Q1"), 
               year_quarter < yearquarter("2020 Q1"), 
               countrycode != "MEX") |> 
        filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "USA", "CAN")) |> 
        select(year_quarter, countrycode, gdpi, ex_rate, int_rate) |> 
        mutate(
          gdpi = log(gdpi),
          ex_rate = log(ex_rate),
          int_rate = log(int_rate)
        )
    ) 
  
  # ------------------------------------------------------------------------------
  # Estimation of Residualized GDP (controlling for interest and exchange rates)
  # ------------------------------------------------------------------------------
  
  gdpi_res <- feols(gdpi ~ (ex_rate) + (int_rate), foo)  # Estimate residuals from log GDP on exchange rate and interest rate
  
  foo <- foo |> mutate(gdpi_res = residuals(gdpi_res))  # Create new residualized GDP column
  
  # ------------------------------------------------------------------------------
  # Defining Pre- and Post-Treatment Periods for SCM
  # ------------------------------------------------------------------------------
  
  period <- yearquarter(levels(factor(foo$year_quarter)))
  time.tr <- yearquarter("2018 Q4")
  period.pre  <- period[-c(match(time.tr, period):length(period))]
  period.post <- period[c(match(time.tr, period):length(period))]
  
  foo <- foo |> 
    group_by(countrycode) |> 
    mutate(time_to_treat_naim = c(seq(-length(period.pre), -1), seq(0, length(period.post) - 1))) |> 
    ungroup()
  
  period <- as.numeric(levels(factor(foo$time_to_treat_naim)))

  # ------------------------------------------------------------------------------
  # Set Options for Data Preparation for SCM
  # ------------------------------------------------------------------------------
  
  time.tr <- 0
  id.var      <- "countrycode"                             # ID variable
  time.var    <- "time_to_treat_naim"                                # Time variable
  period.pre  <- period[-c(match(time.tr, period):length(period))]   # Pre-treatment period
  period.post <- period[c(match(time.tr, period):length(period))]                          # Post-treatment period
  outcome.var <- "gdpi_res"                                 # Outcome variable
  cov.adj     <- NULL #list(c("constant"))     # Covariates for adjustment
  features    <- NULL                                  # No features other than outcome
  constant    <- T                                 # Constant term
  report.missing <- FALSE                              # To check where missing values are
  cointegrated.data <- TRUE                            # Belief that the data are cointegrated
  
  unit.tr <- r                                      # Treated unit (in terms of id.var)
  unit.co <- unique(foo$countrycode)[-match(unit.tr, unique(foo$countrycode))]                    # Donors pool
  unit.co <- unit.co[!unit.co == region]
  
  # ------------------------------------------------------------------------------
  # Set SCM Estimation Options and Run SCM Model
  # ------------------------------------------------------------------------------
  
  df <- scdata(df = foo, id.var = id.var, time.var = time.var, outcome.var = outcome.var,
               period.pre = period.pre, period.post = period.post,
               unit.tr = unit.tr, unit.co = unit.co, constant = TRUE, cointegrated.data = TRUE)
  
  set.seed(1234)
  model <- scpi(w.constr = w.constr, data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags, e.method = e.method, 
                cores = cores, u.alpha = u.alpha, e.alpha = e.alpha, rho = rho, rho.max = rho.max)
  
  # ------------------------------------------------------------------------------
  # Save SCM Results: Estimates, Inference, and Weights
  # ------------------------------------------------------------------------------
  
  path <- "results/FigB11/"
  scpi_estimates <- sc_est(model, outcome.var, period, period)
  scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period)
  
  write_csv(scpi_estimates, str_c(path, "/", r, "_scpi_estimates.csv"))
  write_csv(scpi_inference, str_c(path, "/", r, "_scpi_inference.csv"))

  weights <- tibble(
    weights = round(model$est.results$w, 4),
    country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
  )
  write_csv(weights, str_c(path, "/", r, "_scpi_weights.csv"))
  
  # ------------------------------------------------------------------------------
  # Calculate MSE and RMSE
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
  
  # ------------------------------------------------------------------------------
  # Prepare Data for SCM Plot
  # ------------------------------------------------------------------------------
  unit_name = r
  outcome_name = "Quarterly GDP index (log, residualized)"
  
  foo <- 
    scpi_estimates |> 
    filter(!(name %in% c("diff", "lci", "uci"))) |> 
    mutate(name = if_else(name == "obs", str_c("Observed ", unit_name),  str_c("Synthetic ", unit_name)))
  
  labels <- 
    scpi_estimates |> 
    filter(time_to_treat %% 2 != 1) |> 
    group_by(time_to_treat, period) |> 
    summarise() |> 
    ungroup() 
  
  min_y <- exp((min(((scpi_estimates |> filter(name == "obs"))$value))))
  
  max_y <- exp(round(max((scpi_inference$uci))))
  
  # Data frame  
  foo <- 
    scpi_estimates |> 
    filter(!(name %in% c("diff", "lci", "uci"))) |> 
    mutate(
      name = if_else(name == "obs", str_c("Observed ", r, " Region"), str_c("Synthetic ", r, " Region")),
      date = yearquarter(period)
    ) 
  
  labels <- 
    scpi_estimates |> 
    filter(time_to_treat %% 2 != 1) |> 
    group_by(time_to_treat, period) |> 
    summarise() |> 
    ungroup() 
  
  scpi_inference <- 
    scpi_inference |> 
    mutate(
      date = yearquarter(period),
    ) |> 
    bind_rows(
      foo |> 
        filter(time_to_treat == -1) |> 
        filter(name == str_c("Synthetic ", r, " Region")) |> 
        select(-period) |> 
        mutate(
          lci_ins = value,
          uci_ins = value,
          lci_ofs = value,
          uci_ofs = value,
        )
    )
  
  # Levels
  foo |> 
    ggplot() +
    # Colors
    scale_color_manual(values = c(color4t, "black")) +
    # Vertical Lines
    geom_vline(xintercept = -1, linetype = "dotted") +
    geom_vline(xintercept = 0) +
    labs(x = "", y = outcome_name, color = "", fill = "") +
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
      aes(x = time_to_treat, ymin = exp(lci_ins), ymax = exp(uci_ins), fill = "95% In-sample PI"),
    ) +
    # Out-of-sample
    geom_ribbon(
      data = scpi_inference,
      alpha = .5,
      aes(x = time_to_treat, ymin = exp(lci_ofs), ymax = exp(uci_ofs), fill = "90% Out-of-sample PI"),
    ) +
    scale_fill_manual(values=c("grey", "grey70")) +
    # Observed vs Synthetic
    geom_line(aes(time_to_treat, exp(value), color = name)) +
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
  
  ggsave(
    str_c("results/FigB11/FigB11_", region, ".png"), 
    dpi = 300, width = 8.85, height = 4.85
  )
}

