## 01_simple_linear_models.R
source("00_setup.R")
source("00_utils.R")

vars <- setdiff(names(data), c("age", "target"))
results <- data.frame(variable=character(), R_Squared=numeric(), P_Value=numeric(), stringsAsFactors = FALSE)
for (col in vars) {
  if (is.numeric(data[[col]])) {
    f <- as.formula(paste("age ~", col))
    model <- lm(f, data = data)
    r2 <- summary(model)$r.squared
    p <- coef(summary(model))[2,4]
    results <- rbind(results, data.frame(variable=col, R_Squared=r2, P_Value=p))
  }
}
results <- results %>% arrange(desc(R_Squared))
print(results)
safe_write(results, file.path(OUT_DIR, "simple_linear_results.rds"))
