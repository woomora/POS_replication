# ------------------------------------------------------------------------------
# Table B1: Balance on Pre-Treatment Observables: Components shares
# ------------------------------------------------------------------------------

# Load IFS dataset
foo <- posq |> 
  filter(countrycode %in% c("ARG", "BRA", "CHL", "COL", "CRI", "HND", "ECU", "MEX", "USA", "CAN")) |> 
  filter(year >= 2013) |> 
  filter(year_quarter < yearquarter("2020 Q1")) |> 
  select(countrycode, year_quarter, gdpi, cnsmpti, govi, capfi, importsi, exportsi, 
         gdp, cnsmpt, gov, capf, imports, exports, unemp, int_rate, ex_rate, naim) |> 
  distinct() |> 
  mutate(
    time_to_treat_naim = as.numeric(year_quarter - yearquarter("2018 Q4")),
    gdpi = log(gdpi),
    cnsmpts = cnsmpt / gdp,
    govs = gov / gdp,
    capfs = capf / gdp,
    importss = imports / gdp,
    exportss = exports / gdp,
    net_ex = exportss - importss,
  )

# Note: GDP and components are in 2018 Mexican pesos

# Load SCM weights from the quarterly analysis
scpi_weights_quarterly <- read_csv("results/Fig5/scpi_weights_controls.csv")

# ------------------------------------------------------------------------------
# Calculate Synthesized Log-Levels (weighted means of donor countries) for GDP and components
# ------------------------------------------------------------------------------
synth_shares <- 
  foo |> 
  filter(countrycode != "MEX") |>                          # Exclude Mexico (treated unit)
  filter(time_to_treat_naim <= 0) |>                       # Pre-treatment period only
  select(countrycode, cnsmpts, govs, capfs, importss, exportss, int_rate, ex_rate) |>
  group_by(countrycode) |> 
  summarise_all(mean) |>                                   # Calculate mean values for each country
  ungroup() |> 
  left_join(
    tibble(
      weights = scpi_weights_quarterly$weights,            # Join with SCM weights
      countrycode = scpi_weights_quarterly$country
    )
  ) |> 
  summarise_at(
    vars(cnsmpts, govs, capfs, importss, exportss, int_rate, ex_rate),
    ~ sum(. * weights)                                     # Calculate weighted average using SCM weights
  )

# ------------------------------------------------------------------------------
# Observed Log-Levels for Mexico
# ------------------------------------------------------------------------------
obs_shares <- 
  foo |> 
  filter(countrycode == "MEX") |>                          # Focus on Mexico (treated unit)
  filter(time_to_treat_naim <= 0) |>                       # Pre-treatment period only
  select(cnsmpts, govs, capfs, importss, exportss, int_rate, ex_rate) |>
  summarise_all(mean)                                      # Calculate mean values for Mexico

# ------------------------------------------------------------------------------
# Mean Log-Levels for Donor Pool (average of all donor countries)
# ------------------------------------------------------------------------------
donor_pool_shares <- 
  foo |> 
  filter(countrycode != "MEX") |>                          # Exclude Mexico (treated unit)
  filter(time_to_treat_naim <= 0) |>                       # Pre-treatment period only
  select(cnsmpts, govs, capfs, importss, exportss, int_rate, ex_rate) |>
  summarise_all(mean)                                      # Calculate average values for donor pool

# ------------------------------------------------------------------------------
# Combine the Balance Results for SCM, Observed, and Donor Pool Mean
# ------------------------------------------------------------------------------
balance_obs <- 
  synth_shares |> 
  mutate(constrain = "SCM") |>                             # Add SCM log-levels
  bind_rows(
    obs_shares |> mutate(constrain = "Observed")        # Add observed (Mexico) log-levels
  ) |> 
  bind_rows(
    donor_pool_shares |> mutate(constrain = "Donor pool mean")  # Add donor pool mean log-levels
  ) |> 
  select(constrain, everything())                          # Reorder columns

# ------------------------------------------------------------------------------
# Output the Balance Table in LaTeX Format
# ------------------------------------------------------------------------------
balance_obs |> 
  mutate_if(
    is.double, ~ round(., 3)                              # Round all numeric values to 3 decimal places
  ) |> 
  kableExtra::kable(
    format = "latex",                                     # Output in LaTeX format
    digits = 3,                                           # Specify digits for numeric columns
    align = c("lccccccc"),                                # Align columns
    escape = T                                            # Escape special characters in LaTeX
  )

# ------------------------------------------------------------------------------
# Save the Balance Table to CSV
# ------------------------------------------------------------------------------
balance_obs |> 
  write_csv("results/TabB1/quarterly_pretreat_balance.csv")
