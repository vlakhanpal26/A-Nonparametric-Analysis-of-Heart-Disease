## 04_bootstrap_thalach_by_age.R
source("00_setup.R")
source("00_utils.R")
heart_data <- data
heart_data$age_bracket <- cut(heart_data$age, breaks = c(-Inf, 40, 50, 60, Inf), labels = c("<40","40-50","50-60","60+"))
bootstrap_stat <- function(x, n_boot=5000) replicate(n_boot, mean(sample(x, replace=TRUE)))
age_brackets <- levels(heart_data$age_bracket)
bootstrap_results <- list()
for (br in age_brackets){
  vals <- na.omit(heart_data$thalach[heart_data$age_bracket==br])
  if (length(vals)>0) bootstrap_results[[br]] <- bootstrap_stat(vals)
}
safe_write(bootstrap_results, file.path(OUT_DIR, "bootstrap_thalach_by_age.rds"))
