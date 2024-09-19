
#### Install ####
install.packages('survminer') 
#################

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
library(survival)
library(survminer)
library(ggplot2)
#################



#### read df

df <- read.csv("D:\\SNUlab\\ECG\\0. data\\ldlparticle_labdatajoined.csv", fileEncoding = 'euc-kr')
head(df)
str(df)
colSums(is.na(df))

################

#### COX ####

    #1. total LDL
    # fit
    fit <- coxph(Surv(TIME, death) ~ sex + age + ALT + AST + BUN + TG + albumin + glucose + uric_acid + SBP + DBP + LDL, data = df)
    fit <- coxph(Surv(TIME, death) ~ sex + age + LDL, data = df)
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint



    #2. LDL particle
    # fit
    fit <- coxph(Surv(TIME, death) ~ sex + age + LDL3_small, data = df)
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint
    
#####################