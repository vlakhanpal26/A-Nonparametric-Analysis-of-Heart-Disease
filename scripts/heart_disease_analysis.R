#!/usr/bin/env Rscript
# Heart disease exploratory analysis and modeling inspired by annotated-NDA Final Project.pdf
# Run from project root; adjust DATA_PATH if your CSV lives elsewhere.

suppressPackageStartupMessages({
  required_pkgs <- c(
    "tidyverse", "broom", "nortest", "goftest", "corrplot",
    "cowplot", "scales"
  )
  optional_pkgs <- c("gridExtra")
  needed <- setdiff(required_pkgs, rownames(installed.packages()))
  if (length(needed) > 0) {
    message("Installing missing packages: ", paste(needed, collapse = ", "))
    install.packages(needed, repos = getOption("repos"))
  }
  lapply(required_pkgs, library, character.only = TRUE)
  invisible(lapply(intersect(optional_pkgs, rownames(installed.packages())), library, character.only = TRUE))
})

options(stringsAsFactors = FALSE)

DATA_PATH <- file.path("data", "heart.csv")
OUTPUT_DIR <- file.path("outputs", "figures")
RESULTS_DIR <- file.path("outputs", "tables")
LOG_DIR <- file.path("outputs", "logs")

for (dir in c(OUTPUT_DIR, RESULTS_DIR, LOG_DIR)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

stopifnot(file.exists(DATA_PATH))
heart <- read_csv(DATA_PATH, show_col_types = FALSE)

numeric_cols <- heart |> select(where(is.numeric)) |> names()
key_numeric <- intersect(c("age", "trestbps", "chol", "thalach", "oldpeak"), names(heart))

write_csv_safe <- function(df, path) {
  df |> mutate(across(where(is.numeric), ~ round(.x, 6))) |> write_csv(path)
}

# ---------------------------------------------------------------------------
# A. Simple Linear Regression (age as response)
# ---------------------------------------------------------------------------

simple_lm <- function(df, response = "age", exclude = c("target")) {
  predictors <- setdiff(names(df), c(response, exclude))
  map_dfr(predictors, function(var) {
    if (!is.numeric(df[[var]])) return(NULL)
    formula <- reformulate(var, response)
    model <- lm(formula, data = df)
    tidy_coef <- tidy(model)
    tibble(
      variable = var,
      r_squared = summary(model)$r.squared,
      p_value = tidy_coef$p.value[tidy_coef$term == var]
    )
  }) |> arrange(desc(r_squared))
}

lm_results <- simple_lm(heart)
print(lm_results)
write_csv_safe(lm_results, file.path(RESULTS_DIR, "simple_linear_regression.csv"))

# ---------------------------------------------------------------------------
# B. Anderson-Darling tests for normality
# ---------------------------------------------------------------------------

ad_results <- map_dfr(numeric_cols, function(var) {
  tst <- nortest::ad.test(na.omit(heart[[var]]))
  tibble(variable = var, ad_statistic = unname(tst$statistic), p_value = tst$p.value)
})
print(ad_results)
write_csv_safe(ad_results, file.path(RESULTS_DIR, "anderson_darling_tests.csv"))

# ---------------------------------------------------------------------------
# C-E. Kolmogorov-Smirnov & Cramer-von Mises tests with diagnostic plots
# ---------------------------------------------------------------------------

ks_cvm_results <- map_dfr(key_numeric, function(var) {
  vec <- na.omit(heart[[var]])
  ks <- ks.test(vec, "pnorm", mean(vec), sd(vec))
  cvm <- goftest::cvm.test(vec, null = "pnorm", mean = mean(vec), sd = sd(vec))
  tibble(
    variable = var,
    test = c("Kolmogorov-Smirnov", "Cramer-von Mises"),
    statistic = c(unname(ks$statistic), unname(cvm$statistic)),
    p_value = c(ks$p.value, cvm$p.value)
  )
})
print(ks_cvm_results)
write_csv_safe(ks_cvm_results, file.path(RESULTS_DIR, "goodness_of_fit_tests.csv"))

plot_histograms <- function(df, vars, out_dir) {
  walk(vars, function(var) {
    g <- ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(binwidth = diff(range(df[[var]], na.rm = TRUE))/30, fill = "skyblue", color = "grey30", alpha = 0.8) +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("hist_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_ecdf <- function(df, vars, out_dir) {
  walk(vars, function(var) {
    g <- ggplot(df, aes(x = .data[[var]])) +
      stat_ecdf(geom = "step", color = "royalblue", linewidth = 1) +
      labs(title = paste("Empirical CDF for", var), x = var, y = "ECDF") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("ecdf_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_kde_hist <- function(df, vars, out_dir) {
  walk(vars, function(var) {
    g <- ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "grey30", alpha = 0.6) +
      geom_density(fill = "orange", alpha = 0.2, adjust = 1) +
      labs(title = paste("Histogram & KDE for", var), x = var, y = "Density") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("kde_hist_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_kde_bandwidths <- function(df, vars, out_dir) {
  selectors <- c("nrd0", "nrd", "SJ", "ucv", "bcv")
  walk(vars, function(var) {
    vec <- na.omit(df[[var]])
    density_tbl <- map_dfr(selectors, function(sel) {
      dens <- density(vec, kernel = "gaussian", bw = sel)
      tibble(x = dens$x, y = dens$y, bandwidth = sel)
    })
    g <- ggplot(density_tbl, aes(x = x, y = y, color = bandwidth)) +
      geom_line(linewidth = 0.7) +
      labs(title = paste("KDE with bandwidth selectors for", var), x = var, y = "Density") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("kde_bandwidths_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_spline_cdf <- function(df, vars, out_dir) {
  selectors <- c("nrd0", "nrd", "SJ", "ucv", "bcv")
  walk(vars, function(var) {
    vec <- na.omit(df[[var]])
    density_tbl <- map_dfr(selectors, function(sel) {
      dens <- density(vec, kernel = "gaussian", bw = sel, from = min(vec), to = max(vec))
      cdf <- cumsum(dens$y) / sum(dens$y)
      tibble(x = dens$x, cdf = cdf, bandwidth = sel)
    })
    g <- ggplot(density_tbl, aes(x = x, y = cdf, color = bandwidth)) +
      geom_line(linewidth = 0.7) +
      labs(title = paste("Spline-based CDF for", var), x = var, y = "Cumulative probability") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("spline_cdf_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_qq_pp <- function(df, vars, out_dir) {
  walk(vars, function(var) {
    vec <- na.omit(df[[var]])
    df_pp <- tibble(sample = sort(vec), theoretical = qnorm(ppoints(length(vec)), mean(vec), sd(vec)))
    pp_plot <- ggplot(df_pp, aes(x = theoretical, y = sample)) +
      geom_point(color = "steelblue", alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
      labs(title = paste("P-P plot for", var), x = "Theoretical quantiles", y = "Empirical quantiles") +
      theme_minimal()
    qq_plot <- ggplot(data.frame(sample = vec), aes(sample = sample)) +
      stat_qq(color = "steelblue", alpha = 0.7) +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q plot for", var)) +
      theme_minimal()
    combined <- cowplot::plot_grid(pp_plot, qq_plot, ncol = 2)
    ggsave(file.path(out_dir, paste0("ppqq_", var, ".png")), combined, width = 10, height = 4.5, dpi = 200)
  })
}

plot_histograms(heart, key_numeric, OUTPUT_DIR)
plot_ecdf(heart, key_numeric, OUTPUT_DIR)
plot_kde_hist(heart, key_numeric, OUTPUT_DIR)
plot_kde_bandwidths(heart, key_numeric, OUTPUT_DIR)
plot_spline_cdf(heart, key_numeric, OUTPUT_DIR)
plot_qq_pp(heart, key_numeric, OUTPUT_DIR)

# ---------------------------------------------------------------------------
# F. Mann-Whitney (Wilcoxon rank-sum) tests by target
# ---------------------------------------------------------------------------

if (!"target" %in% names(heart)) stop("Expect target column for Mann-Whitney tests")
heart$target <- as.factor(heart$target)

mw_results <- map_dfr(setdiff(numeric_cols, "target"), function(var) {
  test <- wilcox.test(heart[[var]] ~ heart$target)
  tibble(variable = var, w_statistic = unname(test$statistic), p_value = test$p.value)
})
print(mw_results)
write_csv_safe(mw_results, file.path(RESULTS_DIR, "mann_whitney_tests.csv"))

# ---------------------------------------------------------------------------
# G. Kendall correlations
# ---------------------------------------------------------------------------

kendall_pairs <- list(
  c("age", "thalach"),
  c("trestbps", "thalach"),
  c("chol", "thalach"),
  c("thalach", "target")
)

kendall_results <- map_dfr(kendall_pairs, function(pair) {
  if (!all(pair %in% names(heart))) return(NULL)
  test <- cor.test(heart[[pair[1]]], heart[[pair[2]]], method = "kendall")
  tibble(
    var_x = pair[1],
    var_y = pair[2],
    tau = unname(test$estimate),
    p_value = test$p.value
  )
})
print(kendall_results)
write_csv_safe(kendall_results, file.path(RESULTS_DIR, "kendall_correlations.csv"))

if (all(c("thalach", "trestbps", "chol", "age", "target") %in% names(heart))) {
  corr_mat <- heart |> mutate(target_num = as.numeric(as.character(target))) |> select(thalach, trestbps, chol, age, target = target_num)
  kendall_matrix <- cor(corr_mat, method = "kendall", use = "pairwise.complete.obs")
  png(file.path(OUTPUT_DIR, "kendall_corrplot.png"), width = 800, height = 800, res = 150)
  corrplot::corrplot(kendall_matrix, method = "color", type = "upper", addCoef.col = "black")
  dev.off()
}

# ---------------------------------------------------------------------------
# H. Spearman & Pearson correlations vs thalach
# ---------------------------------------------------------------------------

cor_compare <- function(df, response = "thalach", predictors = c("age", "trestbps", "chol")) {
  map_dfr(predictors, function(var) {
    if (!all(c(response, var) %in% names(df))) return(NULL)
    pearson <- cor.test(df[[response]], df[[var]], method = "pearson")
    spearman <- cor.test(df[[response]], df[[var]], method = "spearman")
    tibble(
      predictor = var,
      pearson_r = unname(pearson$estimate),
      pearson_p = pearson$p.value,
      spearman_rho = unname(spearman$estimate),
      spearman_p = spearman$p.value
    )
  })
}

cor_results <- cor_compare(heart)
print(cor_results)
write_csv_safe(cor_results, file.path(RESULTS_DIR, "correlations_thalach.csv"))

cor_plot <- ggplot(cor_results, aes(x = predictor, y = pearson_r, fill = pearson_p < 0.05)) +
  geom_col(color = "grey20") +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "skyblue"), name = "p < 0.05") +
  labs(title = "Pearson correlation with thalach", y = "Correlation coefficient", x = "Predictor") +
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "pearson_thalach_barplot.png"), cor_plot, width = 6, height = 4, dpi = 200)

# ---------------------------------------------------------------------------
# Scatter plots with linear trend
# ---------------------------------------------------------------------------

plot_scatter <- function(df, response, predictors, out_dir) {
  walk(predictors, function(var) {
    if (!all(c(response, var) %in% names(df))) return(NULL)
    g <- ggplot(df, aes(x = .data[[var]], y = .data[[response]])) +
      geom_point(color = "grey40", alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Scatter:", response, "vs", var), x = var, y = response) +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("scatter_", response, "_", var, ".png")), g, width = 6, height = 4, dpi = 200)
  })
}

plot_scatter(heart, "thalach", c("age", "trestbps", "chol"), OUTPUT_DIR)

# ---------------------------------------------------------------------------
# I. Bootstrapping confidence intervals by age bracket
# ---------------------------------------------------------------------------

heart <- heart |> mutate(age_bracket = cut(age, breaks = c(-Inf, 40, 50, 60, Inf), labels = c("<40", "40-50", "50-60", "60+")))

bootstrap_mean <- function(x, reps = 5000L) {
  replicate(reps, mean(sample(x, replace = TRUE)))
}

bootstrap_results <- map(levels(heart$age_bracket), function(bracket) {
  vals <- heart$thalach[heart$age_bracket == bracket]
  boot <- bootstrap_mean(vals)
  tibble(age_bracket = bracket, boot_mean = boot)
}) |> bind_rows()

ci_results <- bootstrap_results |> group_by(age_bracket) |> summarise(
  lower_ci = quantile(boot_mean, 0.025),
  upper_ci = quantile(boot_mean, 0.975),
  .groups = "drop"
)

write_csv_safe(ci_results, file.path(RESULTS_DIR, "bootstrapped_thalach_ci.csv"))

boot_hist <- ggplot(bootstrap_results, aes(x = boot_mean, fill = age_bracket)) +
  geom_histogram(alpha = 0.6, bins = 40, position = "identity") +
  labs(title = "Bootstrapped mean thalach by age bracket", x = "Bootstrapped mean", y = "Frequency") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "bootstrap_histograms.png"), boot_hist, width = 7.5, height = 5, dpi = 200)

ci_plot <- ggplot(ci_results, aes(x = age_bracket, y = (lower_ci + upper_ci)/2)) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.35, color = "grey30") +
  labs(title = "95% bootstrap CI for thalach by age bracket", y = "Mean thalach", x = "Age bracket") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "bootstrap_ci_plot.png"), ci_plot, width = 6, height = 4, dpi = 200)

# ---------------------------------------------------------------------------
# J. Theil-Sen regression overall & by age bracket
# ---------------------------------------------------------------------------

sen_slope <- function(x, y) {
  combos <- combn(seq_along(x), 2)
  slopes <- (y[combos[2, ]] - y[combos[1, ]]) / (x[combos[2, ]] - x[combos[1, ]])
  slopes <- slopes[is.finite(slopes)]
  median(slopes)
}

bootstrap_sen_ci <- function(x, y, reps = 1000L) {
  slopes <- replicate(reps, {
    idx <- sample(seq_along(x), replace = TRUE)
    sen_slope(x[idx], y[idx])
  })
  quantile(slopes, c(0.025, 0.975))
}

sen_targets <- c("age", "trestbps", "chol")

sen_results <- map_dfr(sen_targets, function(var) {
  valid <- complete.cases(heart[[var]], heart$thalach)
  slope <- sen_slope(heart[[var]][valid], heart$thalach[valid])
  ci <- bootstrap_sen_ci(heart[[var]][valid], heart$thalach[valid])
  tibble(predictor = var, slope = slope, lower_ci = ci[1], upper_ci = ci[2])
})
write_csv_safe(sen_results, file.path(RESULTS_DIR, "theil_sen_overall.csv"))

sen_plot <- ggplot(sen_results, aes(x = predictor, y = slope)) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, color = "grey40") +
  labs(title = "Theil-Sen slope vs thalach", x = "Predictor", y = "Slope") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "theil_sen_overall.png"), sen_plot, width = 6, height = 4, dpi = 200)

sen_age_results <- map_dfr(levels(heart$age_bracket), function(bracket) {
  subset <- heart |> filter(age_bracket == bracket)
  if (nrow(subset) < 3) return(NULL)
  slope <- sen_slope(subset$age, subset$thalach)
  ci <- bootstrap_sen_ci(subset$age, subset$thalach)
  tibble(age_bracket = bracket, slope = slope, lower_ci = ci[1], upper_ci = ci[2])
})
write_csv_safe(sen_age_results, file.path(RESULTS_DIR, "theil_sen_by_age_bracket.csv"))

sen_age_plot <- ggplot(sen_age_results, aes(x = age_bracket, y = slope)) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, color = "grey40") +
  labs(title = "Theil-Sen slope (age vs thalach) by bracket", x = "Age bracket", y = "Slope") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "theil_sen_by_age_bracket.png"), sen_age_plot, width = 6, height = 4, dpi = 200)

# ---------------------------------------------------------------------------
# K. Kruskal-Wallis tests & boxplots
# ---------------------------------------------------------------------------

if (!all(c("sex", "cp") %in% names(heart))) stop("Expect sex and cp columns for Kruskal-Wallis analyses")

heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)

kw_spec <- tribble(
  ~response, ~group,
  "chol", "sex",
  "chol", "cp",
  "trestbps", "cp",
  "trestbps", "sex"
)

kw_results <- kw_spec |> mutate(
  test = pmap(list(response, group), function(resp, grp) {
    kruskal.test(heart[[resp]] ~ heart[[grp]])
  }),
  statistic = map_dbl(test, ~ unname(.x$statistic)),
  df = map_dbl(test, ~ .x$parameter),
  p_value = map_dbl(test, ~ .x$p.value)
) |> select(-test)

print(kw_results)
write_csv_safe(kw_results, file.path(RESULTS_DIR, "kruskal_wallis_tests.csv"))

plot_kw_box <- function(df, response, group, out_dir) {
  g <- ggplot(df, aes(x = .data[[group]], y = .data[[response]], fill = .data[[group]])) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste(response, "by", group), x = group, y = response) +
    theme_minimal() + theme(legend.position = "none")
  ggsave(file.path(out_dir, paste0("boxplot_", response, "_", group, ".png")), g, width = 6, height = 4, dpi = 200)
}

pwalk(kw_spec, ~ plot_kw_box(heart, ..1, ..2, OUTPUT_DIR))

# ---------------------------------------------------------------------------
# L. Logistic regression (M-estimator GLM) with evaluation
# ---------------------------------------------------------------------------

eq_cols <- setdiff(names(heart), c("target", "age_bracket"))
model_df <- heart |> select(all_of(eq_cols), target) |> mutate(target = as.numeric(as.character(target)))
if (any(is.na(model_df$target))) {
  model_df$target <- as.integer(as.factor(heart$target)) - 1L
}

set.seed(123)
train_idx <- sample(seq_len(nrow(model_df)), size = 0.7 * nrow(model_df))
train_data <- model_df[train_idx, ]
test_data <- model_df[-train_idx, ]

train_data$target <- factor(train_data$target)
test_data$target <- factor(test_data$target)

glm_model <- glm(target ~ ., data = train_data, family = binomial)
model_summary <- broom::tidy(glm_model)
write_csv_safe(model_summary, file.path(RESULTS_DIR, "logistic_model_coefficients.csv"))

likelihood_ratio <- anova(glm(target ~ 1, data = train_data, family = binomial), glm_model, test = "Chisq")
write_csv_safe(broom::tidy(likelihood_ratio), file.path(RESULTS_DIR, "logistic_likelihood_ratio.csv"))

predict_prob <- function(model, newdata) {
  probs <- predict(model, newdata = newdata, type = "response")
  tibble(probabilities = probs, predictions = as.integer(probs >= 0.5))
}

train_preds <- predict_prob(glm_model, train_data)
test_preds <- predict_prob(glm_model, test_data)

metrics <- function(preds, truth) {
  truth <- as.numeric(as.character(truth))
  pred_label <- preds$predictions
  tp <- sum(pred_label == 1 & truth == 1)
  tn <- sum(pred_label == 0 & truth == 0)
  fp <- sum(pred_label == 1 & truth == 0)
  fn <- sum(pred_label == 0 & truth == 1)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  f1 <- ifelse(is.na(precision) || is.na(recall) || (precision + recall) == 0, NA_real_, 2 * precision * recall / (precision + recall))
  tibble(tp = tp, tn = tn, fp = fp, fn = fn, accuracy = accuracy, precision = precision, recall = recall, f1 = f1)
}

train_metrics <- metrics(train_preds, train_data$target)
test_metrics <- metrics(test_preds, test_data$target)

write_csv_safe(train_metrics, file.path(RESULTS_DIR, "logistic_metrics_train.csv"))
write_csv_safe(test_metrics, file.path(RESULTS_DIR, "logistic_metrics_test.csv"))

confusion_matrix <- function(preds, truth) {
  truth <- factor(truth, levels = c(0, 1))
  pred <- factor(preds$predictions, levels = c(0, 1))
  tbl <- table(predicted = pred, actual = truth)
  as_tibble(tbl)
}

cm_test <- confusion_matrix(test_preds, test_data$target)
write_csv_safe(cm_test, file.path(RESULTS_DIR, "logistic_confusion_matrix.csv"))

example_patient <- tibble(
  age = 55,
  sex = 1,
  cp = 2,
  trestbps = 140,
  chol = 250,
  fbs = 1,
  restecg = 0,
  thalach = 150,
  exang = 0,
  oldpeak = 1.5,
  slope = 2,
  ca = 1,
  thal = 3
)

feature_cols <- setdiff(names(train_data), "target")
for (col in feature_cols) {
  if (!col %in% names(example_patient)) {
    if (is.factor(train_data[[col]])) {
      example_patient[[col]] <- factor(levels(train_data[[col]])[1], levels = levels(train_data[[col]]))
    } else {
      example_patient[[col]] <- 0
    }
  } else if (is.factor(train_data[[col]])) {
    example_patient[[col]] <- factor(as.character(example_patient[[col]]), levels = levels(train_data[[col]]))
  }
}
example_patient <- example_patient |> select(all_of(feature_cols))

example_prediction <- predict_prob(glm_model, example_patient)
write_csv_safe(example_prediction, file.path(RESULTS_DIR, "example_prediction.csv"))
cat("Example patient probability:", example_prediction$probabilities, "\n")

cat("Analysis complete. Outputs saved to:", OUTPUT_DIR, RESULTS_DIR, "\n")
