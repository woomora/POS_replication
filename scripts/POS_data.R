# ------------------------------------------------------------------------------
### Country codes ----
if (!file.exists("data/derived/countrycodes.csv")) {
  countrycodes <- countrycode::codelist |> 
    select(iso3c, imf, unicode.symbol, cldr.name.en, continent) |> 
    rename(countrycode = iso3c, countryflag = unicode.symbol, country = cldr.name.en) 
  
  # Save the processed countrycodes data
  write_csv(countrycodes, "data/derived/countrycodes.csv")
} else {
  countrycodes <- read_csv("data/derived/countrycodes.csv")
}

# ------------------------------------------------------------------------------
### WB Classifications ----
if (!file.exists("data/derived/wb_classifications.csv")) {
  wbc <- read_csv("data/source/WB classifications.csv") |> 
    janitor::clean_names() |> 
    rename(country = economy, countrycode = code)
  
  upper_middle_income <- wbc |> 
    filter(income_group == "Upper middle income") |> 
    left_join(countrycodes)
  
  lac <- wbc |> 
    filter(region == "Latin America & Caribbean") |> 
    left_join(countrycodes)
  
  # Save the processed data
  write_csv(upper_middle_income, "data/derived/upper_middle_income.csv")
  write_csv(lac, "data/derived/lac.csv")
} else {
  upper_middle_income <- read_csv("data/derived/upper_middle_income.csv")
  lac <- read_csv("data/derived/lac.csv")
}

# ------------------------------------------------------------------------------
### Monthly Economic Activity indexes ----
# Only process the data if the derived dataset doesn't already exist
if (!file.exists("data/derived/monthly_indexes_final.csv")) {
  
  # Step 1: Read and clean the raw data
  monindex <- read_csv("data/source/monthly_indexes.csv") |> 
    mutate(
      # Format month with leading zeros if necessary
      month = case_when(
        str_count(month) == 1 ~ str_c("0", month),
        TRUE ~ as.character(month)
      ),
      # Create a numeric year_month variable and parse date
      year_month = as.numeric(str_c(year, ".", month)),
      date = lubridate::dmy(str_c("01-", month, "-", year)),
      
      # Step 2: Create binary variables for important dates in Mexico
      amlo_vic = if_else(countrycode == "MEX" & date >= lubridate::dmy("01-07-2018"), 1, 0),
      naim = if_else(countrycode == "MEX" & date >= lubridate::dmy("01-11-2018"), 1, 0),
      amlo_gov = if_else(countrycode == "MEX" & date >= lubridate::dmy("01-12-2018"), 1, 0)
    ) |> 
    # Join with country codes and filter for years from 2013 onward
    left_join(countrycodes) |> 
    filter(year >= 2013)
  
  # Step 3: Deseasonalize data
  foo <- monindex |> filter(desestacion == 0)
  monindex <- monindex |> filter(desestacion == 1)
  
  # Step 4: Perform seasonal decomposition on non-deseasonalized data (desestacion == 0)
  for (i in levels(factor(foo$country))) {
    foo1 <- foo |> filter(country == i)
    ts.data1 <- ts(data = foo1$value, start = c(2013), end = c(2022), frequency = 12)
    ts.data1 <- na_locf(ts.data1)  # Fill missing values with Last Observation Carried Forward (LOCF)
    ts.vector <- stl(ts.data1, s.window = 12, na.action = na.omit)$time.series
    ts.vector <- ts.vector[-nrow(ts.vector), ]  # Remove last row (potentially incomplete)
    foo1 <- foo1 |> mutate(value = c(ts.vector[, 2]) + c(ts.vector[, 3]))  # Trend + remainder
    monindex <- monindex |> bind_rows(foo1)
  }
  
  # Step 6: Add base value for each country for October 2018
  monindex <- monindex |> 
    left_join(
      monindex |> 
        filter(year == 2018 & month == 10) |> 
        group_by(country) |> 
        summarise(base_value = mean(value, na.rm = TRUE))
    ) |> 
    mutate(eaindex = (value / base_value) * 100) |>  # Normalize to base value
    mutate(year_month = yearmonth(date))
    
  # Decompose the monthly economic activity index for trend and seasonal components
  monindex <- monindex |> 
    left_join(
      monindex |> 
        filter(year_month <= yearmonth("2020 Jan")) |> 
        as_tsibble(key = "countrycode", index = "year_month") |> 
        model(classical_decomposition(eaindex ~ season(3), type = "multiplicative")) |> 
        components() |> 
        select(countrycode, year_month, trend, seasonal) |> 
        rename(
          eaindex_trend = trend,
          eaindex_seasonal = seasonal
        )
    )
  
  # Step 5: Write the processed data to a CSV file
  write_csv(monindex, "data/derived/monthly_indexes_final.csv")
  
} else {
  # Step 7: If derived dataset exists, read the final dataset
  monindex <- read_csv("data/derived/monthly_indexes_final.csv")
}

# ------------------------------------------------------------------------------
### Quarterly data ----
# IMF Data Processing
# ------------------------------------------------------------------------------
if (!file.exists("data/derived/quarterly_data_final.csv")) {
  
  # Read and clean IMF data
  ifs <- read_csv(
    "data/source/IFS/IFS_06-16-2024 19-11-03-92_timeSeries.csv",
    col_types = paste0("cdccc", paste0(rep("d", 1230 - 5), collapse = ""))
  ) |> 
    janitor::clean_names() |> 
    filter(
      !(country_name %in% c(
        "West African Economic and Monetary Union (WAEMU)", "Sub-Saharan Africa", 
        "Middle East and Central Asia", "Euro Area", "Emerging and Developing Europe", 
        "Emerging and Developing Countries", "Emerging and Developing Asia", 
        "Eastern Caribbean Currency Union (ECCU)", "Central African Economic and Monetary Community", 
        "Advanced Economies"
      ))
    )
  
  # Merge with country codes
  ifs <- ifs |> 
    left_join(
      countrycodes |> select(countrycode, imf) |> 
        rename(country_code = imf, countrycode_iso = countrycode)
    ) |> 
    filter(
      countrycode_iso %in% c("USA", "CAN", levels(factor(lac$countrycode)), levels(factor(upper_middle_income$countrycode)))
    )
  
  # Define a function to process each indicator
  process_indicator <- function(indicator, value_name) {
    ifs |> 
      filter(indicator_code == indicator) |> 
      select(country_name, country_code, contains("q")) |> 
      pivot_longer(cols = !c(country_name, country_code), names_to = "quarter", values_to = value_name) |> 
      mutate(
        year = as.numeric(str_sub(quarter, 2, 5)),
        quarter = as.numeric(str_sub(quarter, -1)),
        year_quarter = yearquarter(str_c(year, " Q", quarter))
      ) |> 
      select(country_name, country_code, year, quarter, year_quarter, all_of(value_name))  |>
      filter(year >= 1980)
  }
  
  # Process all relevant indicators
  gdp <- process_indicator("NGDP_R_SA_XDC", "gdp")
  cnsmpt <- process_indicator("NCP_R_SA_XDC", "cnsmpt")
  hfce <- process_indicator("NCPHI_R_NSA_XDC", "hfce")
  gov <- process_indicator("NCGG_R_SA_XDC", "gov")
  capf <- process_indicator("NFI_R_SA_XDC", "capf")
  exports <- process_indicator("NX_R_SA_XDC", "exports")
  imports <- process_indicator("NM_R_SA_XDC", "imports")
  unemp <- process_indicator("LUR_PT", "unemp")
  prices <- process_indicator("PCPI_IX", "prices")
  ind_prod <- process_indicator("AIP_SA_IX", "ind_prod")
  int_rate <- process_indicator("FIMM_PA", "int_rate")
  ex_rate <- process_indicator("EDNA_USD_XDC_RATE", "ex_rate")
  
  # Join all the indicators together
  posq <- gdp |> 
    left_join(cnsmpt) |> 
    left_join(hfce) |> 
    left_join(gov) |> 
    left_join(capf) |> 
    left_join(exports) |> 
    left_join(imports) |> 
    left_join(int_rate) |> 
    left_join(ex_rate) |> 
    left_join(unemp) |> 
    left_join(prices) |> 
    left_join(ind_prod) |> 
    left_join(countrycodes |> rename(country_code = imf)) |> 
    filter(year >= 2010)
  
  # Mutate additional variables, including GDP shares and treatments
  posq <- posq |> 
    mutate(
      quarter_date = case_when(
        quarter == 1 ~ "31-03", quarter == 2 ~ "30-06", 
        quarter == 3 ~ "30-09", quarter == 4 ~ "31-12"
      ),
      quarter_date = str_c(quarter_date, "-", year),
      quarter_date = dmy(quarter_date),
      year_quarter = yearquarter(quarter_date, fiscal_start = 1),
      # GDP shares
      cnsmpt_s = cnsmpt / gdp,
      gov_s = gov / gdp,
      capf_s = capf / gdp,
      exports_s = exports / gdp,
      imports_s = imports / gdp,
      # Treatment flags
      naim = if_else(countrycode == "MEX" & quarter_date >= ymd("2018-12-30"), 1, 0),
      amlo_win = if_else(countrycode == "MEX" & quarter_date >= ymd("2018-09-30"), 1, 0)
    ) |> 
    distinct(countrycode, year_quarter, .keep_all = TRUE) |> 
    fill(cnsmpt:imports_s, .direction = "down")
  
  # Normalize data to a base period (e.g., 2018 Q3)
  posq <- posq |> 
    left_join(
      posq |> 
        filter(year_quarter == yearquarter("2018 Q3")) |> 
        select(countrycode, gdp, cnsmpt, hfce, gov, capf, imports, exports, int_rate, ex_rate, 
               cnsmpt_s, gov_s, capf_s, imports_s, exports_s, unemp, prices) |> 
        rename(
          # Levels
          base_gdp = gdp, base_cnsmpt = cnsmpt, base_hfce = hfce, base_gov = gov, base_capf = capf, 
          base_imports = imports, base_exports = exports,
          # Share of GDP
          base_cnsmpt_s = cnsmpt_s, base_gov_s = gov_s, base_capf_s = capf_s, base_imports_s = imports_s, base_exports_s = exports_s,
          # Other indicators
          base_int_rate = int_rate, base_ex_rate = ex_rate, base_unemp = unemp, base_prices = prices
        )
    ) |> 
    mutate(
      # Normalized indexes
      gdpi = (gdp / base_gdp) * 100,
      cnsmpti = (cnsmpt / base_cnsmpt) * 100,
      hfcei = (hfce / base_hfce) * 100,
      govi = (gov / base_gov) * 100,
      capfi = (capf / base_capf) * 100,
      importsi = (imports / base_imports) * 100,
      exportsi = (exports / base_exports) * 100,
      # Share of GDP
      cnsmpt_si = (cnsmpt_s / base_cnsmpt_s) * 100,
      gov_si = (gov_s / base_gov_s) * 100,
      capf_si = (capf_s / base_capf_s) * 100,
      imports_si = (imports_s / base_imports_s) * 100,
      exports_si = (exports_s / base_exports_s) * 100,
      # Other indexes
      int_rate_i = (int_rate / base_int_rate) * 100,
      ex_rate_i = (ex_rate / base_ex_rate) * 100,
      unemp_i = (unemp / base_unemp) * 100,
      prices_i = (prices / base_prices) * 100
    )
  
  # Write the final quarterly data to file
  write_csv(posq, "data/derived/quarterly_data_final.csv")
  
} else {
  # If the file already exists, simply load it
  posq <- read_csv("data/derived/quarterly_data_final.csv") |> 
    mutate(year_quarter = yearquarter(year_quarter))
}

# ------------------------------------------------------------------------------
### BIS data ----
# Conditional check for BIS data processing
if (!file.exists("data/derived/bis_data_final.csv")) {
  
  # Interest rates ----
  bis_ir <- read_csv("data/source/BIS/interest_rates.csv") |> 
    janitor::clean_names() |> 
    mutate(date = ymd(str_c(time_period, "-01"))) |> 
    select(-time_period) |> 
    pivot_longer(!date, names_to = "country", values_to = "int_rate") |> 
    mutate(
      countrycode = case_when(
        country == "ar_argentina" ~ "ARG",
        country == "br_brazil" ~ "BRA",
        country == "ca_canada" ~ "CAN",
        country == "cl_chile" ~ "CHL",
        country == "co_colombia" ~ "COL",
        country == "mx_mexico" ~ "MEX",
        country == "pe_peru" ~ "PER",
        country == "us_united_states" ~ "USA",
        country == "tr_turkiye" ~ "TUR",
        country == "ru_russia" ~ "RUS",
        country == "xm_euro_area" ~ "EUR",
        TRUE ~ NA_character_
      )
    ) |> 
    filter(!is.na(countrycode)) |> 
    arrange(country, date) |> 
    select(date, countrycode, int_rate)
  
  # Spreads
  bis_ir <- bis_ir |> 
    left_join(
      bis_ir |> filter(countrycode == "USA") |> 
        mutate(spread = int_rate) |> 
        select(date, spread),
      by = "date"
    ) |> 
    mutate(spread = int_rate - spread) |> 
    filter(countrycode != "USA")
  
  # Exchange rate ----
  bis_xr <- read_csv("data/source/BIS/exchange_rates.csv") |> 
    janitor::clean_names() |> 
    mutate(date = dmy(time_period)) |> 
    select(-time_period) |> 
    pivot_longer(!date, names_to = "currency", values_to = "exr") |> 
    arrange(currency, date) |> 
    mutate(
      countrycode = case_when(
        currency == "ars_argentine_peso" ~ "ARG",
        currency == "brl_brazilian_real" ~ "BRA",
        currency == "cad_canadian_dollar" ~ "CAN",
        currency == "clp_chilean_peso" ~ "CHL",
        currency == "cop_colombian_peso" ~ "COL",
        currency == "eur_euro" ~ "EUR",
        currency == "rub_russian_rouble" ~ "RUS",
        currency == "try_turkish_lira" ~ "TUR",
        currency == "uyu_uruguayan_peso" ~ "UYU",
        currency == "mxn_mexican_peso" ~ "MEX",
        TRUE ~ NA_character_
      ),
      exr = na_locf(exr)
    ) |> 
    filter(countrycode %in% c("ARG", "BRA", "CAN", "CHL", "COL", "EUR", "RUS", "TUR", "MEX"))
  
  # Left join Interest rates
  bis_xr <- bis_xr |> 
    left_join(bis_ir, by = c("date", "countrycode")) |> 
    group_by(currency, countrycode) |> 
    mutate(
      int_rate = na_locf(int_rate),
      spread = na_locf(spread)
    ) |> 
    ungroup()
  
  # Create XE Index
  bis_xr <- bis_xr |> 
    left_join(
      bis_xr |> filter(date == ymd("2018-07-01")) |> 
        select(currency, countrycode, exr) |> 
        rename(exr_election = exr),
      by = c("currency", "countrycode")
    ) |> 
    mutate(exri = exr / exr_election)
  
  # XE residuals on spread
  m1 <- feols(log(exr) ~ spread | currency + date, bis_xr, se = "het")
  bis_xr <- bis_xr |> mutate(lexr_res = residuals(m1))
  
  # XE volatility 
  bis_xr <- bis_xr |> 
    group_by(currency) |> 
    mutate(
      # Calculate daily changes as log returns
      log_return = c(NA, diff(log(exr))),
      # Calculate rolling standard deviation of log returns
      exr_volatility = rollapply(log_return, width = 7, FUN = sd, fill = NA, align = "right")
    ) |> 
    ungroup()
  
  # Write the processed BIS data to file
  write_csv(bis_xr, "data/derived/bis_data_final.csv")
  
} else {
  # If the file already exists, load it
  bis_xr <- read_csv("data/derived/bis_data_final.csv")
}

# ------------------------------------------------------------------------------
### ITAEE Data ----
# Regionalization based on Banxico
# https://www.banxico.org.mx/publicaciones-y-prensa/reportes-sobre-las-economias-regionales/%7B14E0CCD3-F41E-D1C0-C9B0-685788FA5A5B%7D.pdf

rnorte <- c("Baja California", "Baja California Sur", "Coahuila de Zaragoza", "Chihuahua", "Nuevo León", "Sinaloa", "Sonora", "Tamaulipas")
rcentro_norte <- c("Aguascalientes", "Colima", "Durango", "Guanajuato", "Jalisco", "Nayarit", "San Luis Potosí", "Zacatecas")
rcentro <- c("Ciudad de México", "México")
rcentro_sur <- c("Guerrero", "Hidalgo", "Michoacán de Ocampo", "Morelos", "Puebla", "Querétaro", "Tlaxcala")
rsur_sureste <- c("Campeche", "Chiapas", "Oaxaca", "Quintana Roo", "Tabasco", "Veracruz de Ignacio de la Llave", "Yucatán")


# Check if the ITAEE data has already been processed
if (!file.exists("data/derived/itaee_final.csv")) {
  
  ### Economic activity by state
  rqmex <- read_csv("data/source/INEGI/ITAEE/itaee_indice_desest.csv") |> 
    janitor::clean_names() |> 
    pivot_longer(!entidad_federativa) |> 
    mutate(
      year = as.numeric(str_sub(name, 2, 5)),
      quarter = str_sub(name, 8),
      year_quarter = yearquarter(str_c(year, " Q", quarter), fiscal_start = 1),
      date = case_when(
        quarter == "1" ~ ymd(str_c(year, "-04-01")),
        quarter == "2" ~ ymd(str_c(year, "-07-01")),
        quarter == "3" ~ ymd(str_c(year, "-10-01")),
        quarter == "4" ~ ymd(str_c(year + 1, "-01-01"))
      ),
      country = "MEX",
      region = case_when(
        entidad_federativa %in% rnorte ~ "Región Norte",
        entidad_federativa %in% rcentro_norte ~ "Región Centro-Norte",
        entidad_federativa %in% rcentro ~ "Región Centro",
        entidad_federativa %in% rcentro_sur ~ "Región Centro-Sur",
        entidad_federativa %in% rsur_sureste ~ "Región Sur-Sureste"
      ),
      amlo_vic = if_else(country == "MEX" & date >= ymd("2018-07-01"), 1, 0),
      naim = if_else(country == "MEX" & date >= ymd("2018-11-01"), 1, 0),
      covid = if_else(country == "MEX" & date >= ymd("2020-02-01"), 1, 0)
    ) |> 
    rename(state = entidad_federativa, eaindex = value) |> 
    select(-name)
  
  # After 2010 and before Covid-19
  rqmex <- rqmex |> 
    filter(year >= 2010) |> 
    filter(year_quarter < yearquarter("2020 Q1")) |> 
    left_join(
      rqmex |> filter(year == 2018 & quarter == 3) |> 
        select(state, eaindex) |> 
        rename(eaindex_base = eaindex)
    ) |> 
    mutate(eaindexi = (eaindex / eaindex_base) * 100) |> 
    select(-eaindex_base)
  
  # Aggregate by region
  rqmex_regions <- rqmex |> 
    group_by(region, year_quarter) |> 
    summarise(gdpi = mean(eaindexi, na.rm = TRUE)) |> 
    mutate(
      countrycode = region,
    ) |> 
    ungroup() |> 
    select(countrycode, year_quarter, gdpi)
  
  # Write the processed ITAEE data to file
  write_csv(rqmex, "data/derived/itaee_final.csv")
  
} else {
  # Load the processed ITAEE data if already available
  rqmex <- read_csv("data/derived/itaee_final.csv")
}

# ------------------------------------------------------------------------------
### Yearly data ----
# Process yearly data only if the file does not exist
if (!file.exists("data/derived/pos_year_final.csv")) {
  pos_year <- read_csv("data/source/WDI WB/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_598603.csv", skip = 3) |> 
    janitor::clean_names() |> 
    rename(countrycode = country_code) |> 
    select(-country_name, -indicator_name, -indicator_code) |> 
    pivot_longer(!countrycode, names_to = "year", values_to = "gdppc") |> 
    mutate(year = as.numeric(str_sub(year, 2, 5))) |> 
    filter(year >= 1997) |> 
    left_join(countrycodes) |> 
    filter(continent == "Americas") |> 
    mutate(
      naim = if_else(countrycode == "MEX" & year >= 2018, 1, 0),
      gdppc = log(gdppc)
    ) |> 
    filter(countrycode %in% c(levels(factor(monindex$countrycode)))) 
  
  # Save the processed yearly data
  write_csv(pos_year, "data/derived/pos_year_final.csv")
  
} else {
  # Load the processed yearly data
  pos_year <- read_csv("data/derived/pos_year_final.csv")
}

# ------------------------------------------------------------------------------