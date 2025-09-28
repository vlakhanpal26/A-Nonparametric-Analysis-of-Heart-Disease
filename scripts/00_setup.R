## 00_setup.R
## Setup workspace: load libraries and set paths
library(ggplot2)
library(dplyr)
library(caret)
library(boot)
library(MASS)
library(nortest)
library(fitdistrplus)
library(corrplot)

DATA_DIR <- file.path("..", "data")
OUT_DIR <- file.path("..", "outputs")
dir.create(OUT_DIR, showWarnings = FALSE)

data_path <- file.path(DATA_DIR, "heart.csv")
if (!file.exists(data_path)) stop("Place heart.csv in the data/ folder")
data <- read.csv(data_path)
