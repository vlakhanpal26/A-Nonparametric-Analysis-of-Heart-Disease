## 01b_gof_normality_plots.R
source("00_setup.R")
numeric_cols <- c("age", "trestbps", "chol", "thalach", "oldpeak")
gof_res <- list()
for (var in numeric_cols) {
  x <- na.omit(data[[var]])
  ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
  ad <- ad.test(x)
  cvm <- tryCatch({cvm.test(x)}, error = function(e) NULL)
  gof_res[[var]] <- list(ks=ks, ad=ad, cvm=cvm)
}
safe_write(gof_res, file.path(OUT_DIR, "gof_results.rds"))
