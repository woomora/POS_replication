# Replication of "Populism's Original Sin: Short-term Populist Penalties and Uncertainty Traps"

## Overview
This repository contains the replication materials for the paper _Populism's Original Sin: Short-Term Populist Penalties and Uncertainty Traps_. The paper investigates the short-term economic consequences of populist policies, focusing on the cancellation of Mexico City’s New International Airport (NAIM) in 2018 as a macroeconomic natural experiment. 

## Scripts

### 1. `POS_master.R`
This script contains the workspace and settings required to run the other scripts. The core analysis uses the synthetic control method to estimate the counterfactual economic outcomes had the NAIM project not been canceled. All results, including synthetic control estimates and cost calculations, are saved as output files for further review.

### 2. `POS_data.R`
This script handles all data-related operations, including importing and transforming raw economic data such as GDP differences.

**Data Sources:**
- Quarterly GDP data from [IMF's International Financial Statistics (IFS)](https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b).
- Monthly economic activity indicators for ten countries in the Americas.
- Exchange rate and interest rate data from [BIS](https://data.bis.org/topics/EER).
- Quarterly economic activity in Mexican regions from [INEGI](https://www.inegi.org.mx/temas/itaee/).

### 3. `POS_functions.R`
This script includes reusable functions used throughout the replication for estimation, sensitivity analysis, and visualization. These functions take as input an `scpi` object and generate the estimates and inference as a `tibble`.

- `sc_estimate`: Function for obtaining the synthetic control method estimates.
- `sc_inference`: Function for obtaining the synthetic control method inference.

### 4. `POS_monetary_costs.R`
This script focuses on estimating the monetary costs associated with the NAIM cancellation. It compares these costs with both baseline GDP estimates and independent estimates from the Superior Audit Office (ASF).

## Replication Instructions

1. Clone or download this repository.
2. Run the `POS_master.R` script to replicate the results of the paper.
   - This script will automatically call the necessary data and function scripts.
   - Outputs will be saved in the `results/` directory, including plots and synthetic control estimates.

### R Session Info

To replicate the analysis, the following packages are installed and loaded using the `pacman` package. `pacman::p_load()` installs from CRAN, while `pacman::p_load_gh()` installs packages from GitHub if they are not already installed.

```r
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20 (macOS Sonoma 14.2.1)

Attached packages:
- tidyverse 2.0.0
- readr 2.1.5
- janitor 2.2.0
- lubridate 1.9.3
- panelView 1.1.18
- fixest 0.12.1
- countrycode 1.6.0
- scpi 2.2.5
- imputeTS 3.3
- modelsummary 2.1.1
- readxl 1.4.3
- ggnewscale 0.4.10
- ggthemes 5.1.0
- pracma 2.4.4
- tsibble 1.1.4
- feasts 0.3.2
- fabletools 0.4.2
- lpirfs 0.2.3
- zoo 1.8-12
- binsreg 1.0
- knitr 1.48
- httr 1.4.7
```

## Citation
To cite the paper in your work, you can use the following BibTeX entry:

```bibtex
@article{Woo-Mora2024,
  title = {Populism's Original Sin: Short-term Populist Penalties and Uncertainty Traps},
  author = {L. Guillermo Woo-Mora},
  institution = {Paris School of Economics (PSE); Ecole des Hautes Etudes en Sciences Sociales (EHESS)},
  year = {2024},
  month = {October},
  url = {https://ssrn.com/abstract=4893646},
  note = {Available at SSRN: https://ssrn.com/abstract=4893646 or http://dx.doi.org/10.2139/ssrn.4893646}
}
```
