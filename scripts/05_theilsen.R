## 05_theilsen.R
source("00_setup.R")
theil_sen <- function(x,y){
  slopes <- as.numeric(outer(y,y, "-") / outer(x,x, "-"))
  slopes <- slopes[is.finite(slopes)]
  median(slopes, na.rm=TRUE)
}
heart_data <- data
heart_data$age_bracket <- cut(heart_data$age, breaks = c(-Inf,40,50,60,Inf), labels=c("<40","40-50","50-60","60+"))
theil_results <- data.frame(Age_Bracket=character(), Median_Slope=numeric(), stringsAsFactors=FALSE)
for (br in levels(heart_data$age_bracket)){
  df <- subset(heart_data, age_bracket==br)
  if (nrow(df)>1) theil_results <- rbind(theil_results, data.frame(Age_Bracket=br, Median_Slope=theil_sen(df$age, df$thalach)))
}
print(theil_results)
safe_write(theil_results, file.path(OUT_DIR, "theil_sen_results.rds"))
