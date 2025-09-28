Project: A Nonparametric Analysis of Key Predictors and Diagnostic Models for Heart Disease

A statistical analysis of heart disease predictors using nonparametric methods, including:
- Simple linear regression
- Normality and goodness-of-fit tests
- Mann-Whitney tests
- Correlation analysis (Kendall, Spearman)
- Bootstrap confidence intervals
- Theil-Sen regression
- Kruskal-Wallis tests
- M-Estimator logistic model

## Setup

1. Install R (>= 4.0.0) from https://cran.r-project.org/

2. Clone this repository:
```bash
git clone https://github.com/vlakhanpal26/A-Nonparametric-Analysis-of-Heart-Disease.git
cd A-Nonparametric-Analysis-of-Heart-Disease
```

3. Install required R packages. In R:
```r
install.packages(c(
  "ggplot2",    # plotting
  "dplyr",      # data manipulation
  "caret",      # model training/testing
  "boot",       # bootstrapping
  "MASS",       # robust statistics
  "nortest",    # normality tests
  "fitdistrplus", # distribution fitting
  "corrplot"    # correlation plots
))
```

4. Place the heart disease dataset as `data/heart.csv`

5. Run the analysis (from project root):
```bash
Rscript scripts/run_all.R
```

Or in R:
```r
setwd("scripts")
source("run_all.R")
```

Results will be written to the `outputs/` directory.

## Project Structure

```
├── .gitignore
├── README.md
├── data/               # place heart.csv here
│   └── README.md
├── outputs/           # created by scripts
├── scripts/           # analysis code
│   ├── 00_setup.R
│   ├── 00_utils.R
│   ├── 01_simple_linear_models.R 
│   ├── 01b_gof_normality_plots.R
│   ├── 01c_ecdf_kde_splinecdf.R
│   ├── 02_mann_whitney.R
│   ├── 03_correlations.R
│   ├── 04_bootstrap_thalach_by_age.R
│   ├── 05_theilsen.R
│   ├── 06_kruskal_wallis.R
│   ├── 07_logistic_m_estimator.R
│   ├── 08_example_prediction.R
│   └── run_all.R
```
