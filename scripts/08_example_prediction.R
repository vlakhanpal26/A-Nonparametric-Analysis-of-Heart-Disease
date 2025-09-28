## 08_example_prediction.R
source("00_setup.R")
source("07_logistic_m_estimator.R")
new_patient <- data.frame(age=55, sex=1, cp=2, trestbps=140, chol=250, fbs=1, restecg=0, thalach=150, exang=0, oldpeak=1.5, slope=2, ca=3, thal=3)
prob <- predict(model, newdata = new_patient, type = "response")
cat("Probability of heart disease:", prob, "\n")
cat("Prediction (0=no,1=yes):", ifelse(prob>0.5,1,0), "\n")
