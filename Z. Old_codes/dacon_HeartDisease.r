
#### library ####
library(dplyr)
library(forecast)
library(Metrics)
library(broom)
library(caret)
library(lme4)
library(sjPlot)
library(MASS)
library(data.table)
library(car)
library(psych) # describe
library(MatchIt)
library(tableone)
library(gbm)
#################

train <- read.csv("D:\\Kaggle\\HeartDisease_dataset\\train_preprocessed.csv")
test <- read.csv("D:\\Kaggle\\HeartDisease_dataset\\test_preprocessed.csv")

variables <- colnames(train)
variables <- variables[!variables %in% c("target", "id")]

#### Stepwise variable selection - for Logistic ####

target <- train$target
df2 <- train[variables]

full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

#########################