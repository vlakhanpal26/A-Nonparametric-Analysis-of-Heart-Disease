# Nonparametric Analysis of Heart Disease Data

## Project - ISYE 6404: Nonparametric Data Analy

This repository contains my final project for ISYE 6404 Nonparametric Data Analysis. The project applies various nonparametric statistical methods to analyze heart disease data, exploring relationships between health indicators and heart disease presence.

## Project Overview

In this project, I applied the following nonparametric techniques learned throughout the course:
- Normality and goodness-of-fit tests
- Mann-Whitney tests for comparing distributions
- Correlation analysis (Kendall, Spearman)
- Bootstrap confidence intervals
- Theil-Sen regression for robust trend analysis
- Kruskal-Wallis tests for multiple group comparisons
- M-Estimator logistic model for robust classification

The project demonstrates the practical application of nonparametric methods in healthcare data analysis, showing how these techniques can provide robust insights without relying on distributional assumptions.

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

Analysis results and visualizations will be saved to the `outputs/` directory.

## Key Findings

Through this analysis, I discovered several important insights:

1. The distribution of various health indicators significantly differs between patients with and without heart disease (Mann-Whitney tests)
2. Age and maximum heart rate (thalach) show strong nonlinear relationships with heart disease risk (Bootstrap analysis)
3. Chest pain type and exercise-induced angina are strong predictors (Kruskal-Wallis tests)
4. The M-Estimator logistic model provided robust classification while being resistant to outliers

## Project Structure

```
├── .gitignore
├── README.md
├── data/               # Dataset directory
│   ├── heart.csv      # Heart disease dataset
│   └── README.md      # Data documentation
├── outputs/           # Analysis outputs
├── scripts/           # Analysis code
│   ├── 00_setup.R    # Environment setup
│   ├── 00_utils.R    # Utility functions
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
│   └── run_all.R     # Main execution script
```

## Dataset Description

The heart disease dataset (`data/heart.csv`) includes:
- Demographic information (age, sex)
- Clinical measurements (blood pressure, cholesterol)
- Diagnostic results (ECG, heart rate)
- Exercise test data
- Target variable: presence/absence of heart disease

-Vedika :)
