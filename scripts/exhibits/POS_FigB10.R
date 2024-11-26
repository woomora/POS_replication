# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB10"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Figure B.10: Regions Map
# ------------------------------------------------------------------------------
# This script creates a map of Mexico showing regions based on the classification
# by Banxico (Bank of Mexico). The regions are visualized using shapefiles for
# Mexican states, and the economic activity index is calculated and visualized.
# Source: Banxico's Regional Economy Reports.
# URL: https://www.banxico.org.mx/publicaciones-y-prensa/reportes-sobre-las-economias-regionales/
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Regionalization Setup
# ------------------------------------------------------------------------------

# Define the regions based on Banxico's classification
rnorte <- c("Baja California", "Baja California Sur", "Coahuila de Zaragoza", 
            "Chihuahua", "Nuevo León", "Sinaloa", "Sonora", "Tamaulipas")

rcentro_norte <- c("Aguascalientes", "Colima", "Durango", "Guanajuato", 
                   "Jalisco", "Nayarit", "San Luis Potosí", "Zacatecas")

rcentro <- c("Ciudad de México", "México")

rcentro_sur <- c("Guerrero", "Hidalgo", "Michoacán de Ocampo", "Morelos", 
                 "Puebla", "Querétaro", "Tlaxcala")

rsur_sureste <- c("Campeche", "Chiapas", "Oaxaca", "Quintana Roo", 
                  "Tabasco", "Veracruz de Ignacio de la Llave", "Yucatán")

# ------------------------------------------------------------------------------
# Create and Save Regions Shapefile if it Doesn't Exist
# ------------------------------------------------------------------------------
library(sf)
sf::sf_use_s2(FALSE)  # Disable S2 geometry for shapefile manipulation

# Check if 'regions.shp' already exists
if (!file.exists("data/source/Mexican States shape/regions.shp")) {
  
  # Read shapefile for Mexican states
  edos_shp <- st_read("data/source/Mexican States shape/dest2019gw.shp") |> 
    janitor::clean_names() |> 
    mutate(
      estado = nom_ent,  # Rename state name variable
      region = case_when(
        estado %in% rnorte ~ "North",
        estado %in% rcentro_norte ~ "Center North",
        estado %in% rcentro ~ "Center",
        estado %in% rcentro_sur ~ "Center South",
        estado %in% rsur_sureste ~ "South-South East"
      )
    )
  
  # Summarize by region to create regional boundaries
  regions_shp <- edos_shp |> 
    group_by(region) |> 
    summarise(do_union = TRUE) |>  # Union geometries by region
    ungroup()
  
  # Save the regions shapefile for future use
  regions_shp |> st_write("data/source/Mexican States shape/regions.shp", append = TRUE)
  
} else {
  
  # Read shapefile for Mexican states
  edos_shp <- st_read("data/source/Mexican States shape/dest2019gw.shp") |> 
    janitor::clean_names() |> 
    mutate(
      estado = nom_ent,  # Rename state name variable
      region = case_when(
        estado %in% rnorte ~ "North",
        estado %in% rcentro_norte ~ "Center North",
        estado %in% rcentro ~ "Center",
        estado %in% rcentro_sur ~ "Center South",
        estado %in% rsur_sureste ~ "South-South East"
      )
    )
  
  # If 'regions.shp' exists, load it directly
  regions_shp <- st_read("data/source/Mexican States shape/regions.shp") |> 
    janitor::clean_names()
}

# ------------------------------------------------------------------------------
# Plot the Regions Map
# ------------------------------------------------------------------------------

# Plot map of Mexican regions using the shapefiles
ggplot() +
  geom_sf(data = regions_shp, aes(fill = region, color = region), alpha = 0.75) +  # Plot regions with colors
  geom_sf(data = edos_shp, alpha = 0.25, lwd = 0.5) +  # Plot state boundaries
  viridis::scale_fill_viridis(discrete = TRUE) +  # Viridis color scale
  viridis::scale_color_viridis(discrete = TRUE) +
  labs(fill = "Region", color = "Region") +
  theme_minimal()  # Minimal theme for cleaner look

# Save the plot as a PNG file
ggsave("results/FigB10/FigB10.png", dpi = 300)
