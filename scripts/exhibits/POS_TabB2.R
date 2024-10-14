# ------------------------------------------------------------------------------
# Table B.2: SCM Weights across specifications
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# List all directories in the "results" folder that contain the SCM results
# ------------------------------------------------------------------------------

directories <- list.dirs(path = "results", full.names = TRUE, recursive = FALSE)

# ------------------------------------------------------------------------------
# Function to read SCM weights from CSV files within each directory
# ------------------------------------------------------------------------------
# This function reads SCM weight files for each directory (figure or robustness exercise),
# adds a column for the figure name, and returns each dataset as a separate row.

read_weights <- function(directory) {
  
  # List all CSV files in the directory that match "weights" in the filename
  csv_files <- list.files(directory, pattern = "weights.*\\.csv$", full.names = TRUE)
  
  # Map through each CSV file, read it, and add figure and source file columns
  weights_data <- map(csv_files, ~ {
    
    # Read CSV file
    df <- read_csv(.x)
    
    # Add a column for the figure name or identifier based on the directory
    df$figure <- basename(directory)
    
    # Add a column to identify the source CSV file
    df$source_file <- basename(.x)
    
    # Return the dataframe
    df
  })
  
  # Combine all data frames into one for the current directory
  combined_data <- bind_rows(weights_data)
  
  # Return the combined data
  return(combined_data)
}

# ------------------------------------------------------------------------------
# Apply the function to all directories and combine all data frames into a single table
# ------------------------------------------------------------------------------

all_weights <- map_df(directories, read_weights)

# ------------------------------------------------------------------------------
# Format and pivot the final table
# ------------------------------------------------------------------------------
# The SCM weights are converted to percentage format and pivoted to show countries as columns
# and robustness exercises (figures) as rows.

final_table <- all_weights |>
  
  # Convert weights to percentage format as character with two decimals
  mutate(weights = sprintf("%.2f", weights * 100)) |>
  
  # Pivot countries as columns, with figure and source file as the row identifiers
  pivot_wider(
    id_cols = c(figure, source_file),  # Figure and source file as row identifiers
    names_from = country,              # Pivot countries to columns
    values_from = weights              # Assign formatted weights to country columns
  )

# ------------------------------------------------------------------------------
# Write the final table to a CSV file for further use or inspection
# ------------------------------------------------------------------------------

write_csv(final_table, "results/TabB2/TabB2.csv")

# ------------------------------------------------------------------------------
# Create LaTeX code for the table using kable and kableExtra packages
# ------------------------------------------------------------------------------
# The table is formatted as LaTeX with booktabs and aligned properly. It can be directly used
# in LaTeX documents or saved to a .tex file.

latex_table <- final_table |>
  
  # Create a LaTeX table using kable
  kable(
    format = "latex",                    # Output format is LaTeX
    booktabs = TRUE,                     # Use booktabs style for better formatting
    escape = FALSE,                      # Do not escape LaTeX-specific characters
    align = 'lcccc',                     # Align the columns: left-aligned for row names, centered for values
    caption = "SCM Weights across specifications"  # Add a caption for the table
  )

# ------------------------------------------------------------------------------
# Optionally, write the LaTeX code to a .tex file for later use
# ------------------------------------------------------------------------------
writeLines(latex_table, "results/TabB2/TabB2.tex")

# ------------------------------------------------------------------------------
# Print the LaTeX table code to the console for inspection or copying if needed
# ------------------------------------------------------------------------------
print(latex_table)
