# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigA1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure A.1: V-Dem Indexes — Democracy Trends in Mexico
# ------------------------------------------------------------------------------
# This code generates a plot for V-Dem democracy indexes in Mexico from 1981 to 2020,
# comparing five democracy indices: Electoral, Liberal, Participatory, Deliberative, 
# and Egalitarian democracy indexes.

# Load V-Dem dataset and select relevant variables
vdem <- 
  tibble(vdemdata::vdem) |>   # Convert V-Dem dataset into a tibble for easier manipulation
  select(country_name, country_text_id, year, 
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem) |>  # Select relevant columns
  filter(year >= 1981)  # Keep data from 1981 onwards

# ------------------------------------------------------------------------------
# Data Transformation: Focus on Mexico and Reshape Data for Plotting
# ------------------------------------------------------------------------------
vdem |> 
  filter(country_name == "Mexico") |>  # Filter for Mexico only
  select(-country_name, -country_text_id) |>  # Drop redundant country columns
  pivot_longer(!year) |>  # Reshape data from wide to long format, stacking the indices
  mutate(
    # Rename the variables to more descriptive names for plot labels
    name = case_when(
      name == "v2x_polyarchy" ~ "Electoral democracy index",
      name == "v2x_libdem" ~ "Liberal democracy index",
      name == "v2x_partipdem" ~ "Participatory democracy index",
      name == "v2x_delibdem" ~ "Deliberative democracy index",
      name == "v2x_egaldem" ~ "Egalitarian democracy index",
    ),
    # Reorder the factor levels for better control of the plot legend order
    name = fct_relevel(
      name, "Electoral democracy index", "Liberal democracy index",
      "Participatory democracy index", "Deliberative democracy index",
      "Egalitarian democracy index"
    )
  ) |> 
  
  # ------------------------------------------------------------------------------
# Plot the Democracy Indices over Time
# ------------------------------------------------------------------------------
ggplot(aes(year, value, color = name)) +  # Set up plot with year on x-axis, value on y-axis, colored by index type
  geom_vline(xintercept = 2018, linetype = "dashed", color = "red") +  # Add vertical line at 2018 (significant event)
  geom_line(size = .5) +  # Plot the lines for each index
  viridis::scale_color_viridis(discrete = TRUE, direction = 1) +  # Use Viridis color scale for better contrast
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +  # Customize x-axis breaks (every 5 years)
  
  # Customize plot labels and appearance
  labs(
    x = "",  # No x-axis label
    y = "V-DEM Index",  # Label for y-axis
    color = ""  # No title for color legend
  ) +
  
  # Customize the theme of the plot
  theme(
    legend.position = "right",  # Position the legend on the right
    legend.text = element_text(size = 6),  # Set font size of the legend text
    axis.title.y = element_text(size = 8),  # Adjust y-axis title size
    axis.text.x = element_text(size = 7),  # Adjust x-axis text size
    axis.text.y = element_text(size = 7)  # Adjust y-axis text size
  )

# ------------------------------------------------------------------------------
# Save the Plot as a PNG Image
# ------------------------------------------------------------------------------
ggsave("results/FigA1/FigA1.png", dpi = 300, width = 8, height = 4.8)  # Save the plot with specified dimensions and resolution
