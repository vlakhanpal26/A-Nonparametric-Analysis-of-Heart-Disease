## 02_mann_whitney.R
source("00_setup.R")
data$target <- as.factor(data$target)
mw_results <- data.frame(Variable=character(), W_Statistic=numeric(), P_Value=numeric(), stringsAsFactors = FALSE)
for (col in names(data)){
  if (is.numeric(data[[col]]) && col != "target"){
    test <- wilcox.test(data[[col]] ~ data$target)
    mw_results <- rbind(mw_results, data.frame(Variable=col, W_Statistic=test$statistic, P_Value=test$p.value))
  }
}
print(mw_results)
safe_write(mw_results, file.path(OUT_DIR, "mann_whitney_results.rds"))
