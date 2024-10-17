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

## Reproduction Instructions

1. Clone this repository.
2. Install the required R packages listed in `requirements.txt`.
3. Run the `POS_master.R` script to replicate the results of the paper.
   - This script will automatically call the necessary data and function scripts.
   - Outputs will be saved in the `results/` directory, including plots and synthetic control estimates.

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
