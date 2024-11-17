# Replication Package: "Populism's Original Sin: Short-term Populist Penalties and Uncertainty Traps"

## Overview

This repository contains the replication materials for the paper [_Populism's Original Sin: Short-Term Populist Penalties and Uncertainty Traps_](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4893646).

The scripts, data, and instructions provided here allow for the replication of all analyses and results. Follow the steps below to ensure smooth execution.

---

## Scripts

### 1. `POS_master.R`
This script sets up the workspace and runs all other scripts. It implements the synthetic control method to estimate counterfactual economic outcomes had the NAIM project not been canceled. Outputs, including synthetic control estimates, figures, and tables, are saved in the `results/` directory.

### 2. `POS_data.R`
This script handles data preparation, including importing and transforming economic data such as GDP differences.

**Key Data Sources:**
- Quarterly GDP data from [IMF's International Financial Statistics (IFS)](https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b).
- Monthly economic activity indicators for ten countries in the Americas.
- Exchange rate and interest rate data from [BIS](https://data.bis.org/topics/EER).
- Regional economic activity data from [INEGI](https://www.inegi.org.mx/temas/itaee/).

### 3. `POS_functions.R`
Reusable functions for estimation, sensitivity analysis, and visualization, including:
- `sc_estimate`: Obtains synthetic control estimates.
- `sc_inference`: Performs inference for synthetic control estimates.

### 4. `POS_monetary_costs.R`
This script estimates monetary costs related to the NAIM cancellation, comparing baseline GDP estimates with external benchmarks (e.g., Superior Audit Office).

---

## Replication Instructions

### 1. System Requirements

- **R version**: 4.4.1 or higher
- **Operating System**: macOS (Sonoma 14.2.1 or later), Linux, or Windows 10+
- **Additional Software**:
  - macOS: Xcode Command Line Tools
  - Windows: Rtools
  - Linux: Development libraries (`libssl-dev`, `libcurl4-openssl-dev`, etc.)

### 2. Setup

#### a. Clone the Repository
Download or clone the repository:
```bash
git clone https://github.com/billywoom/POS_replication.git
cd POS_replication
```

#### b. Install R Packages
This project uses the `pacman` package to simplify package management.

1. Install `pacman` if it's not already installed:
```r
install.packages("pacman")
```
    
2. Load and install all required packages:
```r
library(pacman)
p_load(tidyverse, readr, janitor, lubridate, panelView, fixest, countrycode, scpi, imputeTS,
      modelsummary, readxl, ggnewscale, ggthemes, pracma, tsibble, feasts, fabletools,
       lpirfs, zoo, binsreg, knitr, httr, synthdid, vdemdata)        
```

#### c. Install GitHub Packages
For packages not available on CRAN:
```r
devtools::install_github("synth-inference/synthdid")
devtools::install_github("vdeminstitute/vdemdata")
```

#### d. Address Known Installation Issues

- On macOS, install system dependencies if package installation fails:
```bash
xcode-select --install
brew install openssl libgit2
```
- On Windows, ensure Rtools is installed.

### 3. Run the Scripts

Set your working directory to the project folder and run the master script:
```r
setwd("/path/to/project")
source("POS_master.R")
```

### Session Information

To replicate the analysis, ensure your R session matches the following configuration:

```r
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20 (macOS Sonoma 14.2.1)

Attached packages:
- httr, knitr, binsreg, zoo, lpirfs, feasts, fabletools, tsibble, pracma, ggthemes,
  ggnewscale, readxl, modelsummary, imputeTS, scpi, countrycode, fixest, panelView,
  janitor, lubridate, forcats, stringr, dplyr, purrr, readr, tidyr, tibble, ggplot2,
  tidyverse, vdemdata, synthdid, pacman
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
