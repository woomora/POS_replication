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
  )

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
outcome.var <- "gdpi"  # Outcome variable
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

# scplot(model)  # Plot SCM results

# ------------------------------------------------------------------------------
# Save SCM Results: Estimates, Inference, and Weights
# ------------------------------------------------------------------------------

# Extract and save estimates
scpi_estimates <- sc_est(model, outcome.var, period, period.labels)

# Extract and save inference results
scpi_inference <- sc_inf(model, post_tr = period.post, period_labels = period.labels)

# Extract and save SCM weights
weights <- tibble(
  weights = round(model$est.results$w, 4),
  country = str_remove(names(model$est.results$w), str_c(unit.tr, "."))
)

# ------------------------------------------------------------------------------
# Compute GDP Costs: Pre- and Post-Cancellation Effects on Mexico's GDP
# ------------------------------------------------------------------------------

# Data and Conversion Factors ----
# Units: millions of pesos unless otherwise stated

# Deflactor: Original in base 2020
# Source: https://fundar.org.mx/calculadora-deflactor/
deflactor <- read_csv("data/source/Fundar/Deflactor-actualizado-PCGPE25.csv") |> 
  mutate(
    year = as.double(year),
    year = if_else(is.na(year), 2024, year)
  )

# Convert base to 2018
deflactor <- 
deflactor |> 
  bind_cols(
    deflactor |> filter(year == 2018) |> select(-year) |> rename(deflactor_base = deflactor)
  ) |> 
  mutate(
    deflactor = (deflactor/deflactor_base)
  ) |> 
  select(-deflactor_base)


# Exchange rates (USD to MXN)
# Source: https://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?sector=6&accion=consultarCuadro&idCuadro=CF86&locale=es
fix <- read_csv("data/source/Banxico/FIX.csv") |> 
  mutate(
    date = dmy(date),
    year = year(date)
  )  |> 
  group_by(year) |> 
  summarise(
    fix = mean(fix, na.rm = T)
  ) |> 
  ungroup()

# ------------------------------------------------------------------------------
# ---- Load Observed Quarterly GDP (constant 2018 prices, millions of pesos) ----
# Source: https://www.inegi.org.mx/programas/pib/2013/#Tabulados 
# ------------------------------------------------------------------------------
# URL of the Excel file
url <- "https://www.inegi.org.mx/contenidos/programas/pib/2018/tabulados/des/pibt_cte_valor.xlsx"

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the Excel file
pibt <- read_xlsx(temp_file, n_max = 5)
pibt <- pibt[-c(1:2),-c(1:2)] |> t() |> data.frame() 
names(pibt) <- c("year", "quarter", "gdp")

pibt <-  
  tibble(pibt) |> 
  mutate(
    year = as.double(na.locf(year, na.rm = T)),
    quarter = case_when(
      quarter == "I" ~ "Q1",
      quarter == "II" ~ "Q2",
      quarter == "III" ~ "Q3",
      T ~ "Q4"
    ), 
    gdp = as.double(gdp),
    period = yearquarter(paste(year, quarter))
  ) |> 
  filter(year >= 2010) |> 
  select(period, gdp)

# ------------------------------------------------------------------------------
# Load Synthetic Control (SC) Estimation Data and Format
# ------------------------------------------------------------------------------
# Estimates are in log-points, cannot be converted to levels

qtr_est <- scpi_estimates |>
  mutate(
    # value = if_else(name != "diff", exp(value) - 1, value),
    period = yearquarter(period)
  ) |>
  pivot_wider(id_cols = c(time_to_treat, period)) |>
  mutate(
    obs_levs = exp(obs),              # Convert observed values to percentage
    synth_levs = exp(synth),          # Convert synthetic values to percentage
    diff = exp(diff) - 1,         # Difference in log-points between observed and synthetic in percentage
  )

# ------------------------------------------------------------------------------
# Load SC Prediction Intervals and Format
# ------------------------------------------------------------------------------
qtr_inf <- scpi_inference |>
  mutate(
    period = yearquarter(period),
    lci_levs = exp(lci),
    uci_levs = exp(uci),
  ) |>
  filter(time_to_treat >= 0) |> 
  select(period, time_to_treat, lci, uci, lci_levs, uci_levs)

# ------------------------------------------------------------------------------
# Merge SC Estimates and Prediction Intervals with Observed GDP Data
# ------------------------------------------------------------------------------
qgdpts <- qtr_est |>
  left_join(qtr_inf) |>  # Join with prediction intervals
  left_join(pibt) |>  # Join with GDP data
  mutate(
    year = year(period)
  ) |> 
  mutate(
    # Compute confidence intervals for GDP differences in log points
    diff_lci = exp(obs - lci) - 1,
    diff_uci = exp(obs - uci) - 1,

    # GDP estimates in 2018 MXN in millions
    gdp_synth = gdp * (1 - diff),
    gdp_synth_lci = gdp * (1 - diff_lci),
    gdp_synth_uci = gdp * (1 - diff_uci),
    
    gdp_diff = gdp - gdp_synth,
    gdp_diff_lci = gdp - gdp_synth_lci,
    gdp_diff_uci = gdp - gdp_synth_uci,
    
    # Convert GDP to 2018 USD for comparison
    gdp_obs_usd = (gdp) / (fix |> filter(year == 2018))$fix,
    gdp_synth_usd = (gdp_synth) / (fix |> filter(year == 2018))$fix,
    gdp_synth_lci_usd = (gdp_synth_lci) / (fix |> filter(year == 2018))$fix,
    gdp_synth_uci_usd = (gdp_synth_uci) / (fix |> filter(year == 2018))$fix,
    
    gdp_diff_usd = gdp_obs_usd - gdp_synth_usd,
    gdp_diff_lci_usd = gdp_obs_usd - gdp_synth_lci_usd,
    gdp_diff_uci_usd = gdp_obs_usd - gdp_synth_uci_usd
  ) |>
  glimpse()

# ------------------------------------------------------------------------------
# Compute GDP Costs (Pre-Covid) ----
# ------------------------------------------------------------------------------
# Focus on GDP difference at the end of 2019 Q4 (before Covid-19)
gdp_diff <- qgdpts |>
  filter(period == yearquarter("2019 Q4")) |>
  filter(time_to_treat == 4) |>
  select(time_to_treat, gdp_diff, gdp_diff_lci, gdp_diff_uci)

# Extract estimates and bounds for 2018 pesos
costo_naim_gdp18_lb <- gdp_diff$gdp_diff_lci[nrow(gdp_diff)]
costo_naim_gdp18 <- gdp_diff$gdp_diff[nrow(gdp_diff)]
costo_naim_gdp18_ub <- gdp_diff$gdp_diff_uci[nrow(gdp_diff)]

# Convert costs to 2024 MXN ----
costo_naim_gdp24_lb <- costo_naim_gdp18_lb * (deflactor |> filter(year==2024))$deflactor
costo_naim_gdp24 <- costo_naim_gdp18 * (deflactor |> filter(year==2024))$deflactor
costo_naim_gdp24_ub <- costo_naim_gdp18_ub * (deflactor |> filter(year==2024))$deflactor

# Convert costs to 2024 USD ----
costo_naim_gdp24_usd_lb <- costo_naim_gdp24_lb / (fix |> filter(year==2024))$fix
costo_naim_gdp24_usd <- costo_naim_gdp24 / (fix |> filter(year==2024))$fix
costo_naim_gdp24_usd_ub <- costo_naim_gdp24_ub / (fix |> filter(year==2024))$fix

# ------------------------------------------------------------------------------
# Comparison with ASF Cost Estimates (2013 pesos) ----
# Source: https://www.animalpolitico.com/2021/05/cancelar-naim-texcoco-costo-millones-pesos-auditoria-superior/
# ------------------------------------------------------------------------------
costo_naim_asf <- 113327.7 * 1/(deflactor |> filter(year==2019))$deflactor  # ASF cost estimate

# Relative cost to ASF estimate
rel_costo_naim_asf_lb <- -costo_naim_gdp18_lb / costo_naim_asf
rel_costo_naim_asf <- -costo_naim_gdp18 / costo_naim_asf
rel_costo_naim_asf_ub <- -costo_naim_gdp18_ub / costo_naim_asf

# ------------------------------------------------------------------------------
# Comparison with ASF Cost Estimates (2024 pesos) ----
# ------------------------------------------------------------------------------
costo_naim_asf24 <- costo_naim_asf * (deflactor |> filter(year==2024))$deflactor  # ASF cost estimate in 2024 pesos

# Relative cost to ASF estimate in 2024 pesos
rel_costo_naim_asf24_lb <- -costo_naim_gdp24_lb / costo_naim_asf24
rel_costo_naim_asf24 <- -costo_naim_gdp24 / costo_naim_asf24
rel_costo_naim_asf24_ub <- -costo_naim_gdp24_ub / costo_naim_asf24
