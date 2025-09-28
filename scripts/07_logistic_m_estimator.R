## 07_logistic_m_estimator.R
source("00_setup.R")
set.seed(42)
data$target <- as.factor(data$target)
train_index <- createDataPartition(data$target, p = 0.7, list = FALSE)
train_data <- data[train_index,]
test_data <- data[-train_index,]
model <- glm(target ~ ., data = train_data, family = binomial)
print(summary(model))
pred_probs <- predict(model, newdata = test_data, type = "response")
preds <- ifelse(pred_probs > 0.5, 1, 0)
conf <- table(Predicted = preds, Actual = as.numeric(as.character(test_data$target)))
print(conf)
safe_write(list(model=model, conf=conf), file.path(OUT_DIR, "logistic_model.rds"))
