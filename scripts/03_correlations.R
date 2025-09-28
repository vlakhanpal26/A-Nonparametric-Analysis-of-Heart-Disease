## 03_correlations.R
source("00_setup.R")
numeric_vars <- data[, c("thalach", "trestbps", "chol", "age")]
kendall_matrix <- cor(numeric_vars, method = "kendall", use = "complete.obs")
print(kendall_matrix)
png(file.path(OUT_DIR, "kendall_corr.png"), width=800, height=600)
corrplot(kendall_matrix, method = "color", type = "upper", addCoef.col = "black", tl.srt = 45)
dev.off()
safe_write(kendall_matrix, file.path(OUT_DIR, "kendall_matrix.rds"))
