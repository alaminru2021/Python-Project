install.packages("readr")
library(readr)
D2<-read.csv("D:\\Practical\\402\\Course_Completion_Prediction.csv")
D2$censord[D2$Turnover==0]<-0
D2$censord[D2$Turnover==1 | D2$Turnover==2]<-1
library(survival)
km_fit1<-survfit(Surv(Payment_Amount,censord)~1,data=D2,type="kaplan-meier")
km_fit1
summary(km_fit1)
summary(km_fit1,times = c(100,200,300*(1:30)))
plot(km_fit1)
library(survminer)
ggsurvplot(km_fit1,data = D2,conf.int = T,risk.table = T,ggtheme = theme_minimal())
km_fit2<-survfit(Surv(Payment_Amount,censord)~Employment_Status,data=D2,type="kaplan-meier")
km_fit2
summary(km_fit2)
plot(km_fit2)
km_fit3<-survfit(Surv(Payment_Amount,censord)~Employment_Status+Course_Name,data=D2,type="kaplan-meier")
km_fit3
summary(km_fit3)
plot(km_fit3)
library(survminer)
ggsurvplot(km_fit3,data = D2,conf.int = T,risk.table = T,pval=T,Pval.method=T,ggtheme = theme_minimal())
coxph<-coxph(Surv(Payment_Amount,censord)~Employment_Status+Course_Name,data=D2)
summary(coxph)

