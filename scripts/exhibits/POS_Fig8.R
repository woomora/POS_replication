# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig8"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 8: Quarterly Construction Index by Region
# ------------------------------------------------------------------------------
# This section of the code generates the construction index plot by region and 
# only runs if the output plot file does not already exist.

# Define file path for the output plot
plot_file <- "results/Fig8/Fig8.png"

# Check if the plot file already exists
if (!file.exists(plot_file)) {
  
  # ------------------------------------------------------------------------------
  # Data Preparation for Plotting Construction Index by Region
  # ------------------------------------------------------------------------------
  
  itaee <- read_csv("data/derived/itaee.csv") |> 
    mutate(year_quarter = yearquarter(year_quarter))
  
  itaee_regions_const <- itaee |> 
    filter(unit != "Total") |>  # Exclude 'Total' unit
    filter(str_detect(act, "construccion")) |>  # Filter for construction activity
    filter(year_quarter >= yearquarter("2013 Q1")) |>  # Filter dates
    mutate(
      time_group = case_when(
        year_quarter < yearquarter("2018 Q4") ~ "Pre-NAIM cancellation",
        year_quarter >= yearquarter("2018 Q4") & year_quarter < yearquarter("2020 Q1") ~ "NAIM cancellation",
        TRUE ~ "Post-COVID-19"
      ),
      time_group = fct_relevel(time_group, "Pre-NAIM cancellation", "NAIM cancellation", "COVID-19", "Post-COVID-19"),
      unit = case_when(
        unit == "Región Centro" ~ "Centre",
        unit == "Región Centro-Norte" ~ "Centre-North",
        unit == "Región Centro-Sur" ~ "Centre-South",
        unit == "Región Norte" ~ "North",
        unit == "Región Sur-Sureste" ~ "South-Southeast"
      ),
      unit = fct_relevel(unit, "Centre", "Centre-North", "Centre-South", "North", "South-Southeast"),
      quarter_date = date(year_quarter)
    )
  
  itaee_regions_const <- itaee_regions_const |> 
    left_join(
      itaee_regions_const |> filter(year_quarter == yearquarter("2018 Q4")) |> 
        select(unit, trend) |> rename(trend_ref = trend)
    ) |> 
    mutate(trend = (trend / trend_ref) * 100) |> 
    group_by(unit) |> 
    mutate(
      growth = (trend / lag(trend) - 1) * 100,
      growth_annual = ((trend / lag(trend))^4 - 1) * 100
    ) |> 
    ungroup()
  
  growth_rates <- itaee_regions_const |>
    group_by(time_group, unit) |>
    summarise(growth_mean = mean(growth_annual, na.rm = TRUE)) |>
    mutate(grlabel = round(growth_mean, 1))
  
  # ------------------------------------------------------------------------------
  # Plot Construction Index by Region and Save the Plot
  # ------------------------------------------------------------------------------
  
  itaee_regions_const |> 
    ggplot(aes(quarter_date, trend, color = unit))  +
    geom_vline(xintercept = date(yearquarter("2018 Q4")) + 30) +
    annotate("text", x = date(yearquarter("2018 Q4")) + 30, 
             y=-Inf, label="NAIM cancellation", 
             angle = 90, size = 2, hjust = -.1, vjust = 1.5
    ) +
    geom_vline(xintercept = date(yearquarter("2020 Q1")) + 60) +
    annotate("text", x = date(yearquarter("2020 Q1")) + 60, 
             y=-Inf, label="COVID-19", 
             angle = 90, size = 2, hjust = -.1, vjust = 1.5
    ) +
    geom_line() +
    scale_x_date(breaks = "6 months", labels = function(x) zoo::format.yearqtr(x, "%YQ%q")) +
    labs(x = "", y = "Construction sector:\nQuarterly Activity Index", color = "") +
    viridis::scale_color_viridis(discrete = TRUE) +
    
    # Pre-NAIM cancellation
    annotate("label", x = ymd("2018-03-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
             label = str_c(growth_rates$grlabel[1], "%")) +
    annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#3b528b",
             label = str_c(growth_rates$grlabel[2], "%")) +
    annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#21918c",
             label = str_c(growth_rates$grlabel[3], "%")) +
    annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 5, size = 2.5, hjust = 0.5, color = "#5ec962",
             label = str_c(growth_rates$grlabel[4], "%")) +
    annotate("label", x = ymd("2018-03-01"), y = Inf, vjust = 6.5, size = 2.5, hjust = 0.5, color = "#fde725",
             label = str_c(growth_rates$grlabel[5], "%")) +
    
    # NAIM cancellation
    annotate("label", x = ymd("2019-07-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
             label = str_c(growth_rates$grlabel[6], "%")) +
    annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#3b528b",
             label = str_c(growth_rates$grlabel[7], "%")) +
    annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#21918c",
             label = str_c(growth_rates$grlabel[8], "%")) +
    annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 5, size = 2.5, hjust = 0.5, color = "#5ec962",
             label = str_c(growth_rates$grlabel[9], "%")) +
    annotate("label", x = ymd("2019-07-01"), y = Inf, vjust = 6.5, size = 2.5, hjust = 0.5, color = "#fde725",
             label = str_c(growth_rates$grlabel[10], "%")) +
    
    # Post-cancellation
    annotate("label", x = ymd("2020-10-01"), y = Inf, size = 2.5, hjust = 0.5, color = "#440154", 
             label = str_c(growth_rates$grlabel[11], "%")) +
    annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 2, size = 2.5, hjust = 0.5, color = "#3b528b",
             label = str_c(growth_rates$grlabel[12], "%")) +
    annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 3.5, size = 2.5, hjust = 0.5, color = "#21918c",
             label = str_c(growth_rates$grlabel[13], "%")) +
    annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 5, size = 2.5, hjust = 0.5, color = "#5ec962",
             label = str_c(growth_rates$grlabel[14], "%")) +
    annotate("label", x = ymd("2020-10-01"), y = Inf, vjust = 6.5, size = 2.5, hjust = 0.5, color = "#fde725",
             label = str_c(growth_rates$grlabel[15], "%")) +
    
    coord_cartesian(clip = "off") +
    theme(
      legend.text = element_text(size = 8),
      plot.margin = unit(c(1.5,1,1,1), "lines"),
      axis.text.x = element_text(size = 6, angle = 90)
    )
  
  # Save the plot as a PNG file
  ggsave(plot_file, dpi = 300, width = 7.4, height = 4.8)
}
