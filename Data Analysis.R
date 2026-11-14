# Install required packages
install.packages("readr")
install.packages("survival") 
install.packages("survminer")

library(readr)
library(survival)
library(survminer)

# 1. Load dataset & define binary censoring indicator
D2 <- read.csv("D:/PSHRD Course Material/4th Year/B-PHR-402/Practical/Course_Completion_Prediction.csv")
D2$Censord[D2$Turnover == 0] <- 0
D2$Censord[D2$Turnover == 1 | D2$Turnover == 2] <- 1

# ==============================================================================
# PART (i): Overall Kaplan-Meier Survival Probabilities (Payment_Amount)
# ==============================================================================
km_fit1 <- survfit(Surv(Payment_Amount, Censord) ~ 1, data = D2, type = "kaplan-meier")
print(km_fit1)
summary(km_fit1)
summary(km_fit1,times = c(100,200,300*(1:30)))
plot(km_fit1)
ggsurvplot(km_fit1, data = D2, conf.int = TRUE, risk.table = TRUE,
           xlab = "Payment Amount",
           ylab = "Survival Probability",
           title = "Overall Kaplan-Meier Survival Curve",
           ggtheme = theme_minimal())

# ==============================================================================
# PART (ii): Employment Status Effect (Stratified KM & Cox PH Model)
# ==============================================================================
km_fit2 <- survfit(Surv(Payment_Amount, Censord) ~ Employment_Status, data = D2, type = "kaplan-meier")
print(km_fit2)
summary(km_fit2)
plot(km_fit2)
ggsurvplot(km_fit2, data = D2, conf.int = TRUE, risk.table = TRUE, pval = TRUE,
           risk.table.height = 0.4,
           xlab = "Payment Amount",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Curve by Employment Status",
           ggtheme = theme_minimal())

cox_model2 <- coxph(Surv(Payment_Amount, Censord) ~ Employment_Status, data = D2)
summary(cox_model2)

# ==============================================================================
# PART (iii): Employment Status + Course Name Effect (Stratified KM & Cox PH Model)
# ==============================================================================
km_fit3 <- survfit(Surv(Payment_Amount, Censord) ~ Employment_Status + Course_Name, data = D2, type = "kaplan-meier")
print(km_fit3)
summary(km_fit3)
plot(km_fit3)
ggsurvplot(km_fit3, data = D2, conf.int = TRUE, risk.table = TRUE, pval = TRUE,
           risk.table.height = 0.75, risk.table.width = 2.5,
           xlab = "Payment Amount",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Curve by Employment Status and Course Name",
           ggtheme = theme_minimal())

cox_model3 <- coxph(Surv(Payment_Amount, Censord) ~ Employment_Status + Course_Name, data = D2)
summary(cox_model3)

# ==============================================================================
# Save all Kaplan-Meier & Cox PH outputs to CSV files
# ==============================================================================
write.csv(surv_summary(km_fit1), "km_fit1.csv", row.names = FALSE)
write.csv(surv_summary(km_fit2), "km_fit2.csv", row.names = FALSE)
write.csv(surv_summary(km_fit3), "km_fit3.csv", row.names = FALSE)

write.csv(as.data.frame(summary(km_fit1,times = c(100,200,300*(1:30)))$table), "km_fit1_summary.csv", row.names = FALSE)
write.csv(as.data.frame(summary(km_fit2)$table), "km_fit2_summary.csv")
write.csv(as.data.frame(summary(km_fit3)$table), "km_fit3_summary.csv")
write.csv(summary(cox_model2)$coefficients, "cox_model2.csv")
write.csv(summary(cox_model3)$coefficients, "cox_model3.csv")
