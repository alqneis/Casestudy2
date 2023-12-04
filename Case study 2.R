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
CaseStudy2 <- read_excel("C:/Users/qneis/Downloads/CaseStudy2ex.xlsx")

CaseStudy2 %>% ggplot(mapping = aes(x = Department, y = DistanceFromHome, color= Attrition)) + geom_point(position = "jitter")
CaseStudy2 %>% ggplot(mapping = aes(x = Department, y = MonthlyIncome, color= Attrition)) + geom_bar()
CaseStudy2 %>% ggplot(mapping = aes(x = Department, fill = Attrition)) + geom_bar(position = "fill")

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


###Classifications
set.seed(6)
splitPerc = .75
CS2 = CaseStudy2 %>% filter(Attrition == "Yes" | Attrition == "No")
summary(CaseStudy2)

trainIndices = sample(1:dim(CaseStudy2)[1],round(splitPerc * dim(CaseStudy2)[1]))
train = CaseStudy2[trainIndices,]
test = CaseStudy2[-trainIndices,]

CaseStudy2 %>% ggplot(aes(x = Age, MonthlyIncome,color = Attrition)) + geom_point()

CaseStudy2NOA %>% classifications = knn(train[,c(1,2)],test[,c(1,2)],train$, prob = TRUE, k = 3)
table(classifications,test$Species)
confusionMatrix(table(classifications,test$Species))

