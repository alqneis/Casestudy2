---
title: "CaseStudy2"
author: "alqneis"
date: "2023-12-13"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r} 
library(tidyverse) 
library(dplyr) 
library(GGally) 
library(ggplot2) 
library(ggthemes) 
library(plotly) 
library(class) 
library(caret) 
library(e1071) 
library(readxl) 
library(reshape2)
library(rmarkdown)
``` 

```{r} 
CaseStudy2 <- read_excel("C:/Users/qneis/Downloads/CaseStudy2ex.xlsx") 
```

```{r} 
###EDA  
#Exploring the factor of Job satisfaction, Age, and monthly income on Attrition  
#Monthly Income seems to be a big indicator of attrition  
#Age might play a role as well 

ggplot(data = CaseStudy2) +  
  geom_point(mapping = aes(x = Age, y = MonthlyIncome, color = Attrition)) + 
  geom_smooth(mapping = aes(x = Age, y = MonthlyIncome, linetype = Attrition, color = Attrition)) + facet_grid(~JobSatisfaction) 

CaseStudy2 <- read_excel("C:/Users/qneis/Downloads/CaseStudy2ex.xlsx") 

CaseStudy2 %>% ggplot(mapping = aes(x = Department, y = DistanceFromHome, color= Attrition)) + geom_point(position = "jitter") 

p = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Department, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p1 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = MaritalStatus, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p2 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Age, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p3 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Gender, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p4 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = JobLevel, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p5 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = JobSatisfaction, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 
CaseStudy2NOA <- read_excel("C:/Users/qneis/Downloads/CaseStudy2NOA.xlsx") 
``` 

```{r} 
###Classifications of KNN DID NOT WORK 
#set.seed(6) 
#splitPerc = .75 
#CS2 = CaseStudy2 %>% filter(Attrition == "Yes" | Attrition == "No") 
#summary(CaseStudy2) 
#CaseStudy2[is.na(CaseStudy2)] <- mean(CaseStudy2, na.rm = TRUE) 
# Split the data into training and test sets 
#trainIndices <- sample(1:dim(CaseStudy2)[1], round(splitPerc * dim(CaseStudy2)[1])) 
#train <- CaseStudy2[trainIndices,] 
#test <- CaseStudy2[-trainIndices,] 
#classifications = knn(train[,c(17,19)],test[,c(17,19)],train$Attrition, prob = TRUE, k = 3) 
#table(classifications,test$Attrition) 
#confusionMatrix(table(classifications,test$Attrition)) 
```

```{r} 
###NAIVE BAyes  
model = naiveBayes(CaseStudy2[,c(2,18)],as.factor(CaseStudy2$Attrition),laplace = 1) 
table(predict(model,CaseStudy2[,c(2,18)]),as.factor(CaseStudy2$Attrition)) 
CM1 = confusionMatrix(table(predict(model,CaseStudy2[,c(2,20)]),as.factor(CaseStudy2$Attrition))) 

CM1 

###NaiveBayes 1 Job satisfaction and Monthly Income and Age 

model2 = naiveBayes(CaseStudy2[,c(2,18,20)],as.factor(CaseStudy2$Attrition),laplace = 1) 
table(predict(model2,CaseStudy2[,c(2,18,20)]),as.factor(CaseStudy2$Attrition)) 

CM2 = confusionMatrix(table(predict(model2,CaseStudy2[,c(2,18,20)]),as.factor(CaseStudy2$Attrition))) 

CM2 
###Model Number 2 Age and Monthly Income 
###NAIVE BAyes  
model = naiveBayes(CaseStudy2[,c(2,18)],as.factor(CaseStudy2$Attrition),laplace = 1) 
table(predict(model,CaseStudy2[,c(2,18)]),as.factor(CaseStudy2$Attrition)) 
CM1 = confusionMatrix(table(predict(model,CaseStudy2[,c(2,20)]),as.factor(CaseStudy2$Attrition))) 

CM1 

###NaiveBayes 1 Job satisfaction and Monthly Income and Age 

model2 = naiveBayes(CaseStudy2[,c(2,18,20)],as.factor(CaseStudy2$Attrition),laplace = 1) 
table(predict(model2,CaseStudy2[,c(2,18,20)]),as.factor(CaseStudy2$Attrition)) 

CM2 = confusionMatrix(table(predict(model2,CaseStudy2[,c(2,18,20)]),as.factor(CaseStudy2$Attrition))) 

CM2 

###Model Number 2 Age and Monthly Income

predictions = predict(model3, CaseStudy2[, c(17, 20)])

conf_matrix = table(predicted = predictions, actual = as.factor(CaseStudy2$Attrition))

CM3 = confusionMatrix(conf_matrix)
CM3
```

```{r} 
### Regression Models with difference factors 
CaseStudy2 %>% ggplot(aes(x = Age, y =MonthlyIncome, color = Attrition)) + geom_point(position = "jitter") + 
  ggtitle("Monthly Income vs Age") 

fit = lm(Age~MonthlyIncome, data = CaseStudy2) 
summary(fit) 
fit$coefficients 

fit$coefficients[1] 

fit$coefficients[2] 
CaseStudy2LR = CaseStudy2 %>% filter(MonthlyIncome < 20000) 

fit = lm(Age~MonthlyIncome, CaseStudy2LR) 
summary(fit) 

confint(fit) 
###Fit with Job satisfaction 

#Show lm() and estimation of toy example we have been using 

fit2 = lm(JobSatisfaction~MonthlyIncome, data = CaseStudy2) 

summary(fit2) 


fit$coefficients 
#beta_0_hat 
fit$coefficients[1] 
#beta_1_hat 
fit$coefficients[2] 
CaseStudy2LR = CaseStudy2 %>% filter(MonthlyIncome < 20000) 
fit2 = lm(JobSatisfaction~MonthlyIncome, CaseStudy2LR) 
summary(fit2) 
confint(fit2) 

CaseStudy2 %>%  
  ggplot(aes(x = Age, y = MonthlyIncome, color= Attrition)) + geom_point(position = "jitter") + ggtitle("Age vs Monthly Income Reg") + geom_smooth(method = "lm") + xlim(1000,20000) 

CaseStudy2 %>%  
  ggplot(aes(y = Age, x = MonthlyIncome, color= Attrition)) + geom_point(position = "jitter") + ggtitle("Age vs Monthly Income Reg") + geom_smooth(method = "lm") + xlim(1000,20000) 

CaseStudy2 %>%  
  ggplot(aes(x = JobSatisfaction, y = MonthlyIncome, color= Attrition)) + geom_point(position = "jitter") + ggtitle("JobSat vs Monthly Income Reg") + geom_smooth(method = "lm") + xlim(1000,20000) 

Predictions
CaseStudy2 %>% ggplot(aes(x = Age, y = MonthlyIncome)) + geom_point() 
CaseStudy2LR2nd = CaseStudy2 %>% mutate(Age2 = MonthlyIncome^2) 
fit = lm(Age~MonthlyIncome+MonthlyIncome^2, CaseStudy2LR2nd) 
summary(fit) 
preds = predict(fit) 

CaseStudy2 %>% ggplot(aes(x = Age, y = MonthlyIncome)) + geom_point() +geom_line(data = CaseStudy2, aes( x = JobSatisfaction, y = preds, col = "red")) 

### Monthly Income Attrition + Job Satisfaction Grid after plotting Regression 

groupedMI = CaseStudy2 %>% group_by(MonthlyIncome, Attrition) 
groupedMImean = groupedMI %>% summarise(mean(JobSatisfaction)) 
CaseStudy2 %>% ggplot(aes(JobSatisfaction, MonthlyIncome, color = Attrition)) + 
  geom_point(position = "jitter") + 
  facet_grid(Attrition~JobSatisfaction) + 
  ggtitle("Job Satisfaction vs MonthlyIncome by Attrition") 
```

```{r} 
###RMSE AND MSPE 
pred = predict(monthlyincomeFIT, CaseStudy2) 
pred2 = predict(monthlyincomeFIT2, CaseStudy2) 

rmse <- sqrt(mean((CaseStudy2$MonthlyIncome - pred)^2)) 

rmse2 <- sqrt(mean((CaseStudy2$MonthlyIncome - pred2)^2)) 

###MSPE 
monthlyincomeFIT = lm(formula = MonthlyIncome ~ Age, data = CaseStudy2) 

monthlyincomeFIT2 = lm(formula = MonthlyIncome ~ JobSatisfaction+Age+Gender, data = CaseStudy2) 

summary(monthlyincomeFIT) 

rmse <- sqrt(mean((CaseStudy2$MonthlyIncome - predicted)^2)) 

rmse <- sqrt(mean((CaseStudy2$MonthlyIncome - predicted)^2)) 

Model1_PredsMI = predict(monthlyincomeFIT, newdata = CaseStudy2NOA) 

as.data.frame(Model1_PredsMI) 

MSPE = data.frame(Observed = CaseStudy2NOA$MonthlyIncome, Predicted = Model1_PredsMI) 

MSPE$Resisdual = MSPE$Observed - MSPE$Predicted 

MSPE$SquaredResidual = MSPE$Resisdual^2 

MSPE 

mean(MSPE$SquaredResidual) 

###FIT 2 

Model1_PredsMI2 = predict(monthlyincomeFIT2, newdata = CaseStudy2NOA) 

as.data.frame(Model1_PredsMI2) 

MSPE2 = data.frame(Observed = CaseStudy2NOA$MonthlyIncome, Predicted = Model1_PredsMI2) 

MSPE2$Resisdual = MSPE2$Observed - MSPE2$Predicted 

MSPE2$SquaredResidual = MSPE2$Resisdual^2 

MSPE2 

mean(MSPE2$SquaredResidual) 
```

CaseStudy2NOA %>% classifications = knn(train[,c(1,2)],test[,c(1,2)],train$, prob = TRUE, k = 3)
table(classifications,test$Species)
confusionMatrix(table(classifications,test$Species))

