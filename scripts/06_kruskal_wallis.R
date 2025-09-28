## 06_kruskal_wallis.R
source("00_setup.R")
kw_results <- list()
kw_results$chol_by_sex <- kruskal.test(chol ~ sex, data = data)
kw_results$chol_by_cp <- kruskal.test(chol ~ cp, data = data)
kw_results$trestbps_by_cp <- kruskal.test(trestbps ~ cp, data = data)
kw_results$trestbps_by_sex <- kruskal.test(trestbps ~ sex, data = data)
print(kw_results)
safe_write(kw_results, file.path(OUT_DIR, "kruskal_wallis_results.rds"))
