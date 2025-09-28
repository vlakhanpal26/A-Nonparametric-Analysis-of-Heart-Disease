## 01c_ecdf_kde_splinecdf.R
source("00_setup.R")
numeric_cols <- c("age", "trestbps", "chol", "thalach", "oldpeak")
for (col in numeric_cols) {
  p <- ggplot(data, aes_string(x = col)) +
    stat_ecdf(geom = "step") + theme_minimal() + ggtitle(paste("ECDF:", col))
  ggsave(filename = file.path(OUT_DIR, paste0("ecdf_", col, ".png")), plot = p)

  p2 <- ggplot(data, aes_string(x = col)) + geom_histogram(aes(y = ..density..), bins=30, fill="skyblue", alpha=0.6) + geom_density() + theme_minimal() + ggtitle(paste("KDE:", col))
  ggsave(filename = file.path(OUT_DIR, paste0("kde_", col, ".png")), plot = p2)
}
