
# install.packages('dlnm')
# install.packages('splines')
# install.packages("httpgd")
# install.packages("knitr")
# install.packages("rmarkdown")
install.packages("ggplot2")

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
library(dlnm)
library(splines)
library(httpgd)
library(lubridate)
library(ape)
library(knitr)
library(rmarkdown)
library(survival)
library(ggplot2)
library(gmodels)
#################


basic0719 <- read.csv('D:\\국민건강영양조사\\basic0719.csv', encoding = 'euc-kr')
basic0720 <- read.csv('D:\\국민건강영양조사\\basic0720.csv', encoding = 'euc-kr')


# year 2007 -> 0, ..., 2019 -> 12로 수정


basic0719 <- basic0719 %>% mutate(year_new = 
    case_when(
    year == 2007 ~ 0, 
    year == 2008 ~ 1, 
    year == 2009 ~ 2, 
    year == 2010 ~ 3, 
    year == 2011 ~ 4, 
    year == 2012 ~ 5, 
    year == 2013 ~ 6, 
    year == 2014 ~ 7, 
    year == 2015 ~ 8, 
    year == 2016 ~ 9, 
    year == 2017 ~ 10,
    year == 2018 ~ 11,
    year == 2019 ~ 12, 
    TRUE ~ NA_real_)
)

# year 2007 -> 0, ..., 2020 -> 13으로 수정
basic0720 <- basic0720 %>%mutate(year_new = case_when(
    year == 2007 ~ 0,
    year == 2008 ~ 1, 
    year == 2009 ~ 2, 
    year == 2010 ~ 3, 
    year == 2011 ~ 4, 
    year == 2012 ~ 5, 
    year == 2013 ~ 6, 
    year == 2014 ~ 7, 
    year == 2015 ~ 8, 
    year == 2016 ~ 9, 
    year == 2017 ~ 10, 
    year == 2018 ~ 11, 
    year == 2019 ~ 12, 
    year == 2020 ~ 13, 
    TRUE ~ NA_real_
    )
)


# age, 조사년도, interaction check
model1 <- lm(ABSI ~ year_new * age, data=basic0719)
summary(model1)

model2 <- glm(ABSI ~ year_new * age, family=gaussian, data=basic0719)
summary(model2)

model3_1 <- lm(ABSI ~ year_new + age, data=basic0720)
summary(model3_1)

model3 <- lm(ABSI ~ year_new * age, data=basic0720)
summary(model3)

model4 <- glm(ABSI ~ year_new * age, family=gaussian, data=basic0720)
summary(model4)




# 표준화된 ABSI ~ ht, wt, wc, bmi

MME_A01_absi_standardized <- read.csv('MME_A01_absi_standardized.csv')

model5 <- lm(ABSI_Z ~ HEIGHT, data=MME_A01_absi_standardized)
summary(model5)

model5 <- lm(ABSI_Z ~ WEIGHT, data=MME_A01_absi_standardized)
summary(model5)

model5 <- lm(ABSI_Z ~ WAIST, data=MME_A01_absi_standardized)
summary(model5)

model5 <- lm(ABSI_Z ~ BMI, data=MME_A01_absi_standardized)
summary(model5)


# BMI ~ ht, wt, wc

model5 <- lm(BMI ~ HEIGHT, data=MME_A01_absi_standardized)
summary(model5)

model5 <- lm(BMI ~ WEIGHT, data=MME_A01_absi_standardized)
summary(model5)

model5 <- lm(BMI ~ WAIST, data=MME_A01_absi_standardized)
summary(model5)

cor(MME_A01_absi_standardized$BMI, MME_A01_absi_standardized$HEIGHT)
cor(MME_A01_absi_standardized$BMI, MME_A01_absi_standardized$WEIGHT)
cor(MME_A01_absi_standardized$BMI, MME_A01_absi_standardized$WAIST)


# BMI ~ ht, wt, wc UKB에서 check

ukb_1st_final <- read.csv('SAS\\ukb_1st_final.csv')

ukb_1st_final$BMI = ukb_1st_final$WGHT / ((ukb_1st_final$HGHT / 100)**2)

model5 <- lm(BMI ~ HGHT, data=ukb_1st_final)
summary(model5)

model5 <- lm(BMI ~ WGHT, data=ukb_1st_final)
summary(model5)

model5 <- lm(BMI ~ WSTC, data=ukb_1st_final)
summary(model5)

#### --------------------------------------------------------------------------------------------- ####


# ABSI 모델링

htn_df <- read.csv('0. data\\국건영_ABSI\\absi_htn.csv', encoding = 'euc-kr')
dm_df <- read.csv('0. data\\국건영_ABSI\\absi_dm.csv', encoding = 'euc-kr')
mi_df <- read.csv('0. data\\국건영_ABSI\\absi_mi.csv', encoding = 'euc-kr')
lip_df <- read.csv('0. data\\국건영_ABSI\\absi_lip.csv', encoding = 'euc-kr')
ceva_df <- read.csv('0. data\\국건영_ABSI\\absi_ceva.csv', encoding = 'euc-kr')
gout_df <- read.csv('0. data\\국건영_ABSI\\absi_gout.csv', encoding = 'euc-kr')
cancer_df <- read.csv('0. data\\국건영_ABSI\\absi_cancer.csv', encoding = 'euc-kr')

#### copy df to use

# HTN
df <- copy(htn_df)
target <- df$HTN_final
target_name <- "HTN_final"

# DM
df <- copy(dm_df)
target <- df$DM_final
target_name <- "DM_final"

# MI
df <- copy(mi_df)
target <- df$MI_final
target_name <- "MI_final"

# LIP
df <- copy(lip_df)
target <- df$LIP_final
target_name <- "LIP_final"

# CEVA
df <- copy(ceva_df)
target <- df$CEVA_final
target_name <- "CEVA_final"

# GOUT
df <- copy(gout_df)
target <- df$GOUT_final
target_name <- "GOUT_final"

# CANCER
df <- copy(cancer_df)
target <- df$Cancer_final
target_name <- "Cancer_final"

###################


#### 추가 전처리 및 factor

# 중간처리 변수들 빼기
df <- subset(df, select = -c(EDATE, maxdate, mindate, 평균, 표준편차))

df$SMOKE <- factor(df$SMOKE)
df$DRINK <- factor(df$DRINK)

###################

# ABSI, BMI 분포 확인

# hist(df$ABSI_Z)
# hist(df$ABSI)
# hist(df$BMI)

# summary(df$ABSI_Z)
# summary(df$ABSI)
# summary(df$BMI)

# nrow(df[df$ABSI >= quantile(df$ABSI, 0.75),])   # ABSI 3quartile 이상인 사람
# nrow(df[df$BMI >= quantile(df$BMI, 0.75),])  # BMI 3quartile 이상인 사람

# nrow(df[df$ABSI <= quantile(df$ABSI, 0.25),])   # ABSI 1quartile 이하인 사람
# nrow(df[df$BMI <= quantile(df$BMI, 0.25),])  # BMI 1quartile 이하인 사람

# ABSI, BMI categorize
df$absi_category <- 0
df$bmi_category <- 0

df$absi_category[df$ABSI_Z >= quantile(df$ABSI_Z, 0.75)] <- 1 # 3quartile 이상 (25%)
df$absi_category[df$ABSI_Z < quantile(df$ABSI_Z, 0.75)] <- 0 # 3quartile 미만 (75%)

df$bmi_category[df$BMI >= quantile(df$BMI, 0.75)] <- 1 # 3quartile 이상 (25%)
df$bmi_category[df$BMI < quantile(df$BMI, 0.75)] <- 0 # 3quartile 미만 (75%)

CrossTable(df$absi_category, df$bmi_category)
quantile(df$ABSI, 0.75)

# absi_bmi_category 생성

df <- df %>% mutate(absi_bmi_category = case_when(
  bmi_category == 0 & absi_category == 0 ~ 0,
  bmi_category == 1 & absi_category == 0 ~ 1,
  bmi_category == 0 & absi_category == 1 ~ 2,
  bmi_category == 1 & absi_category == 1 ~ 3
  ))

table(df$absi_bmi_category)
# factor
df$absi_bmi_category <- factor(df$absi_bmi_category)


# 카테고리별 outcome 수
print(target_name)
CrossTable(target, df$absi_bmi_category)



#### interaction PLOT ####
tmpdf <- copy(df)

# BMI mean
tmpdf$testBMI <- tmpdf$BMI - mean(tmpdf$BMI)
# t=table(df$absi_bmi_category,df$HTN_final)
# chisq.test(t)

# HTN
tmpdf$coef_category0 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.1044407, NaN)
tmpdf$coef_category1 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.1044407 + 2.2176163 + tmpdf$testBMI * (-0.0885468), NaN)
tmpdf$coef_category2 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.1044407 + 2.1984119 + tmpdf$testBMI * (-0.0941159), NaN) 
tmpdf$coef_category3 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.1044407 + 3.3382396 + tmpdf$testBMI * (-0.1289931), NaN) 

plot(tmpdf$BMI, tmpdf$coef_category0, col = "red", xlim = c(15, 40), ylim = c(-2, 3.3))
title(main='HTN')
lines(tmpdf$BMI, tmpdf$coef_category1, col = "pink")
lines(tmpdf$BMI, tmpdf$coef_category2, col = "green")
lines(tmpdf$BMI, tmpdf$coef_category3, col = "blue")
legend("bottomright",legend=c("coef_category0", "coef_category1","coef_category2", "coef_category3"),fill=c("red", "pink","green", "blue"),border="white",box.lty=0,cex=1.0)

# MI
tmpdf$coef_category0 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.2312, NaN)
tmpdf$coef_category1 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.2312 + 0.4574 + tmpdf$testBMI * (-0.04206), NaN)
tmpdf$coef_category2 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.2312 + -0.0571 + tmpdf$testBMI * (0.01931), NaN) 
tmpdf$coef_category3 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.2312 + -2.217 + tmpdf$testBMI * (0.07149), NaN) 

plot(tmpdf$BMI, tmpdf$coef_category0, col = "red", xlim = c(15, 40), ylim = c(-2, 3.3))
title(main='MI')
lines(tmpdf$BMI, tmpdf$coef_category1, col = "pink")
lines(tmpdf$BMI, tmpdf$coef_category2, col = "green")
lines(tmpdf$BMI, tmpdf$coef_category3, col = "blue")
legend("bottomright",legend=c("coef_category0", "coef_category1","coef_category2", "coef_category3"),fill=c("red", "pink","green", "blue"),border="white",box.lty=0,cex=1.0)


# GOUT
tmpdf$coef_category0 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.2527, NaN)
tmpdf$coef_category1 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.2527 + 12.68 + tmpdf$testBMI * (-0.4682), NaN)
tmpdf$coef_category2 <- ifelse(df$bmi_category == 0, tmpdf$testBMI * 0.2527 + 2.538 + tmpdf$testBMI * (-0.09254), NaN) 
tmpdf$coef_category3 <- ifelse(df$bmi_category == 1, tmpdf$testBMI * 0.2527 + 3.408 + tmpdf$testBMI * (-0.1282), NaN) 

plot(tmpdf$BMI, tmpdf$coef_category0, col = "red", xlim = c(15, 40), ylim = c(-2, 13))
title(main='GOUT')
lines(tmpdf$BMI, tmpdf$coef_category1, col = "pink")
lines(tmpdf$BMI, tmpdf$coef_category2, col = "green")
lines(tmpdf$BMI, tmpdf$coef_category3, col = "blue")
legend("bottomright",legend=c("coef_category0", "coef_category1","coef_category2", "coef_category3"),fill=c("red", "pink","green", "blue"),border="white",box.lty=0,cex=1.0)


#### Stepwise variable selection ####

# variables 리스트 생성
variables <- colnames(df)

# 선택되면 안되는 변수들 빼기
variables <- variables[!variables %in% c(target_name, "기수", "NIHID", "VISITALL", "TIME", "BMI", 
"ABSI", "ABSI_Z", "HTN_age", "absi_category", "bmi_category", "absi_bmi_category", "DM_age", 'MI_age', "LIP_age", "CEVA_age", "GOUT_age")]

df2 <- df[variables]

full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

step_variables

#########################


#### GLM ####

# create glm formula - ABSI only
variables <- c(step_variables, "ABSI_Z")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

glm_f <- as.formula(
  paste(target_name,
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

glm_f

# fit
fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
summary(fit)



# create glm formula - BMI only
variables <- c(step_variables, "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

glm_f <- as.formula(
  paste(target_name,
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

glm_f

# fit
fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
summary(fit)



# create glm formula - ABSI_Z & BMI
variables <- c(step_variables, "ABSI_Z", "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

glm_f <- as.formula(
  paste(target_name,
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

glm_f

# fit
fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
summary(fit)



#### COX ####
    
    # ABSI_Z only
    variables <- c(step_variables, "ABSI_Z")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"
    cox_f = as.formula(paste(y, "~", x))

    cox_f

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint




    # BMI only
    variables <- c(step_variables, "BMI")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"
    cox_f = as.formula(paste(y, "~", x))

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint




    # ABSI_Z + BMI
    variables <- c(step_variables, "ABSI_Z", "BMI")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"
    cox_f = as.formula(paste(y, "~", x))

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint


    # absi_bmi_category + BMI
    variables <- c(step_variables, "absi_bmi_category", "BMI")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"
    cox_f = as.formula(paste(y, "~", x))

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint


    # absi_bmi_category only
    variables <- c(step_variables, "absi_bmi_category")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"
    cox_f = as.formula(paste(y, "~", x))

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint
    

    # BMI x absi_bmi_category interaction
    variables <- c(step_variables, "BMI * absi_bmi_category")
    variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

    x = paste(variables, collapse = " + ")
    y = "Surv(TIME, target)"

    cox_f = as.formula(paste(y, "~", x))

    # fit
    fit <- coxph(cox_f, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint

#####################


### spline 넣어서 plot 그리기 ####
# absi_bmi_category + pspline(BMI)
mfit <- coxph(Surv(TIME, target) ~ absi_category + pspline(BMI), data=df)  # df의 의미는? 
termplot(mfit, term=2, se=TRUE, col.term=1, col.se=1)
title(main = target_name)



### BODYFAT & MUSCLE MASS ~ ABSI check ###
# 1. 안산안성 data
htn_df <- read.csv('0. data\\국건영_ABSI\\absi_htn.csv', encoding = 'euc-kr')
dm_df <- read.csv('0. data\\국건영_ABSI\\absi_dm.csv', encoding = 'euc-kr')
mi_df <- read.csv('0. data\\국건영_ABSI\\absi_mi.csv', encoding = 'euc-kr')
lip_df <- read.csv('0. data\\국건영_ABSI\\absi_lip.csv', encoding = 'euc-kr')
ceva_df <- read.csv('0. data\\국건영_ABSI\\absi_ceva.csv', encoding = 'euc-kr')
gout_df <- read.csv('0. data\\국건영_ABSI\\absi_gout.csv', encoding = 'euc-kr')
cancer_df <- read.csv('0. data\\국건영_ABSI\\absi_cancer.csv', encoding = 'euc-kr')

mm_df <- read.csv('0. data\\국건영_ABSI\\MME_MM.csv', encoding = 'euc-kr')

#### copy df to use

  # HTN
  df <- copy(htn_df)
  target <- df$HTN_final
  target_name <- "HTN_final"

  # DM
  df <- copy(dm_df)
  target <- df$DM_final
  target_name <- "DM_final"

  # MI
  df <- copy(mi_df)
  target <- df$MI_final
  target_name <- "MI_final"    # mi가 제일 n수 많음, outcome 상관없으니까 mi로 하자

  # LIP
  df <- copy(lip_df)
  target <- df$LIP_final
  target_name <- "LIP_final"

  # CEVA
  df <- copy(ceva_df)
  target <- df$CEVA_final
  target_name <- "CEVA_final"

  # GOUT
  df <- copy(gout_df)
  target <- df$GOUT_final
  target_name <- "GOUT_final"

  # CANCER
  df <- copy(cancer_df)
  target <- df$Cancer_final
  target_name <- "Cancer_final"

###################

### inner join with bfm_mm_df ###
df <- inner_join(df, mm_df, by = "NIHID")
###################


#### 추가 전처리 및 factor

# 중간처리 변수들 빼기
df <- subset(df, select = -c(EDATE, maxdate, mindate, 평균, 표준편차))

df$SMOKE <- factor(df$SMOKE)
df$DRINK <- factor(df$DRINK)

df$PHX_HTN <- factor(df$PHX_HTN)
df$PHX_DM <- factor(df$PHX_DM)
df$PHX_MI <- factor(df$PHX_MI)
df$PHX_LIP <- factor(df$PHX_LIP)
df$PHX_CEVA <- factor(df$PHX_CEVA)
df$PHX_GOUT <- factor(df$PHX_GOUT)
df$PHX_Cancer <- factor(df$PHX_Cancer)


###################

# ABSI, BMI categorize
df$absi_category <- 0
df$bmi_category <- 0

df$absi_category[df$ABSI_Z >= quantile(df$ABSI_Z, 0.75)] <- 1 # 3quartile 이상 (25%)
df$absi_category[df$ABSI_Z < quantile(df$ABSI_Z, 0.75)] <- 0 # 3quartile 미만 (75%)

df$bmi_category[df$BMI >= quantile(df$BMI, 0.75)] <- 1 # 3quartile 이상 (25%)
df$bmi_category[df$BMI < quantile(df$BMI, 0.75)] <- 0 # 3quartile 미만 (75%)

CrossTable(df$absi_category, df$bmi_category)
quantile(df$ABSI, 0.75)

# absi_bmi_category 생성

df <- df %>% mutate(absi_bmi_category = case_when(
  bmi_category == 0 & absi_category == 0 ~ 0,
  bmi_category == 1 & absi_category == 0 ~ 1,
  bmi_category == 0 & absi_category == 1 ~ 2,
  bmi_category == 1 & absi_category == 1 ~ 3
  ))

table(df$absi_bmi_category)
# factor
df$absi_bmi_category <- factor(df$absi_bmi_category)


# 카테고리별 outcome 수
print(target_name)
CrossTable(target, df$absi_bmi_category)


#### Stepwise variable selection - about BODYFAT ####

# variables 리스트 생성
variables <- colnames(df)

# 선택되면 안되는 변수들 빼기
variables <- variables[!variables %in% c(target_name, "기수", "NIHID", "VISITALL", "TIME", "BMI", 
"ABSI", "ABSI_Z", "HTN_age", "absi_category", "bmi_category", "absi_bmi_category", "DM_age", 'MI_age', "LIP_age", "CEVA_age", "GOUT_age", "IB1_3")]

df2 <- df[variables]

full <- lm(BODYFAT ~ ., data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

step_variables

#########################


#### LM ####

# create lm formula - BODYFAT ~ ABSI_Z
variables <- c(step_variables, "ABSI_Z")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)

# create lm formula - BODYFAT ~ BMI
variables <- c(step_variables, "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# create lm formula - BODYFAT ~ ABSI_Z + BMI
variables <- c(step_variables, "ABSI_Z", "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# create lm formula - BODYFAT ~ BMI + absi_bmi_category
variables <- c(step_variables, "BMI", "absi_bmi_category")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - BODYFAT ~ BMI only (unadjusted model)
variables <- c("BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - BODYFAT ~ BMI + ABSI_Z only (unadjusted model)
variables <- c("BMI", "ABSI_Z")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - BODYFAT ~ BMI + ABSI_Z + SEX + AGE only (unadjusted model)
variables <- c("BMI", "ABSI_Z", "SEX", "AGE")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("BODYFAT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)




#### Stepwise variable selection - about IB1_3 ####

# variables 리스트 생성
variables <- colnames(df)

# 선택되면 안되는 변수들 빼기
variables <- variables[!variables %in% c(target_name, "기수", "NIHID", "VISITALL", "TIME", "BMI", 
"ABSI", "ABSI_Z", "HTN_age", "absi_category", "bmi_category", "absi_bmi_category", "DM_age", 'MI_age', "LIP_age", "CEVA_age", "GOUT_age")]

df2 <- df[variables]

full <- lm(IB1_3 ~ ., data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

step_variables

#########################


#### LM ####

# create lm formula - IB1_3 ~ ABSI_Z
variables <- c(step_variables, "ABSI_Z")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)

# create lm formula - IB1_3 ~ BMI
variables <- c(step_variables, "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - IB1_3 ~ ABSI_Z + BMI
variables <- c(step_variables, "ABSI_Z", "BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - IB1_3 ~ BMI + absi_bmi_category
variables <- c(step_variables, "BMI", "absi_bmi_category")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - IB1_3 ~ BMI only (unadjusted model)
variables <- c("BMI")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - IB1_3 ~ BMI + ABSI_Z only (unadjusted model)
variables <- c("BMI", "ABSI_Z")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - IB1_3 ~ BMI + ABSI_Z + SEX + AGE only (unadjusted model)
variables <- c("BMI", "ABSI_Z", "SEX", "AGE")
variables <- variables[!variables %in% c('WAIST', 'HEIGHT', 'WEIGHT')]

lm_f <- as.formula(
  paste("IB1_3",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)




################################################

# corr check

var <- c("BODYFAT", "ABSI_Z")
var <- c("BODYFAT", "BMI")
var <- c("IB1_3", "ABSI_Z")
var <- c("IB1_3", "BMI")
var <- c("BODYFAT", "ABSI_Z", "BMI")
var <- c("IB1_3", "ABSI_Z", "BMI")

cordf <- df[var]

cor(cordf)


# descriptive
variables <- c(step_variables, "IB1_3", "ABSI_Z", "BMI", "absi_bmi_category")

df$bmi_category <- factor(df$bmi_category)
df$absi_category <- factor(df$absi_category)
summary(df)

df2 <- df[df$absi_bmi_category == 0,]
df2 <- df[df$absi_bmi_category == 1,]
df2 <- df[df$absi_bmi_category == 2,]
df2 <- df[df$absi_bmi_category == 3,]

summary(df2)

### 안산안성 end. ###



# 2. 국건영 data

basic_bone_0811 <- read.csv('D:\\국민건강영양조사\\basic_bone_0811_absiZ.csv', encoding = 'euc-kr')

df <- copy(basic_bone_0811)

# ABSI, BMI categorize
df$absi_category <- 0
df$bmi_category <- 0

df$absi_category[df$ABSI_Z >= quantile(df$ABSI_Z, 0.75)] <- 1 # 3quartile 이상 (25%)
df$absi_category[df$ABSI_Z < quantile(df$ABSI_Z, 0.75)] <- 0 # 3quartile 미만 (75%)

df$bmi_category[df$HE_BMI >= quantile(df$HE_BMI, 0.75)] <- 1 # 3quartile 이상 (25%)
df$bmi_category[df$HE_BMI < quantile(df$HE_BMI, 0.75)] <- 0 # 3quartile 미만 (75%)

CrossTable(df$absi_category, df$bmi_category)
quantile(df$ABSI, 0.75)

# absi_bmi_category 생성

df <- df %>% mutate(absi_bmi_category = case_when(
  bmi_category == 0 & absi_category == 0 ~ 0,
  bmi_category == 1 & absi_category == 0 ~ 1,
  bmi_category == 0 & absi_category == 1 ~ 2,
  bmi_category == 1 & absi_category == 1 ~ 3
  ))

table(df$absi_bmi_category)

# factor & 전처리
df$absi_bmi_category <- factor(df$absi_bmi_category)
df$SEX <- factor(df$SEX)
df$HE_obe <- factor(df$HE_obe)
df$HE_Upro <- factor(df$HE_Upro)
df$D_1_1 <- factor(df$D_1_1)
df$BS3_1 <- factor(df$BS3_1)
df$BP1 <- factor(df$BP1)
df$DI3_dg <- factor(df$DI3_dg)
df$DI4_dg <- factor(df$DI4_dg)
df$DI1_dg <- factor(df$DI1_dg)
df$DE1_dg <- factor(df$DE1_dg)
df$DI2_dg <- factor(df$DI2_dg)
df$DI3_pt <- factor(df$DI3_pt)
df$DE1_pt <- factor(df$DE1_pt)
df$DI2_pt <- factor(df$DI2_pt)
df$DI1_pt <- factor(df$DI1_pt)
df$DI4_pt <- factor(df$DI4_pt)
df$HE_hepaB <- factor(df$HE_hepaB)


df$BS3_2[(df$BS3_2 == 888) | (df$BS3_2 == 999)] <- 0
df$BS6_2_2[(df$BS6_2_2 == 88) | (df$BS6_2_2 == 99)] <- 0
df$BS6_3[(df$BS6_3 == 888) | (df$BS6_3 == 999)] <- 0
df$BS2_1[(df$BS2_1 == 88) | (df$BS2_1 == 99)] <- 0
df$BS2_1[(df$BS2_1 == 888) | (df$BS2_1 == 999)] <- 0

df$BD1_11[df$BD1_11 >= 1 & df$BD1_11 <= 4] <- 0
df$BD1_11[df$BD1_11 >= 5 & df$BD1_11 <= 6] <- 1
df$BD1_11[df$BD1_11 >= 8 & df$BD1_11 <= 9] <- 2

df$BD2_1[df$BD2_1 == 1] <- 0
df$BD2_1[df$BD2_1 >= 2 & df$BD2_1 <= 3] <- 1
df$BD2_1[df$BD2_1 >= 4 & df$BD2_1 <= 5] <- 2
df$BD2_1[df$BD2_1 >= 8 & df$BD2_1 <= 9] <- 3


output <- capture.output(summary(df), file=NULL,append=FALSE)
output_collapsed <- paste0(output, sep=" ", collapse="\n")
output_df <- as.data.frame(output_collapsed)
write.csv(output_df, "descriptive.csv", row.names = F, fileEncoding = 'euc-kr')

df2 <- df[df$absi_bmi_category == 0,]
df2 <- df[df$absi_bmi_category == 1,]
df2 <- df[df$absi_bmi_category == 2,]
df2 <- df[df$absi_bmi_category == 3,]

summary(df2)


# 중간처리 변수들 빼기
df <- subset(df, select = -c(평균, 표준편차, absi_category, bmi_category))


#### Stepwise variable selection - about BODYFAT ####

# variables 리스트 생성
variables <- colnames(df)

# 선택되면 안되는 변수들 빼기
variables <- variables[!variables %in% c("ID", "year", "psu", "HE_BMI", "ABSI", "ABSI_Z", "DW_SBT_LN", "absi_bmi_category")]

df2 <- df[variables]

full <- lm(DW_WBT_FT ~ ., data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

step_variables

#########################


#### LM ####

# create lm formula - DW_WBT_FT ~ ABSI_Z
variables <- c(step_variables, "ABSI_Z")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_WBT_FT ~ BMI
variables <- c(step_variables, "HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_WBT_FT ~ ABSI_Z + BMI
variables <- c(step_variables, "ABSI_Z", "HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_WBT_FT ~ BMI + absi_bmi_category
variables <- c(step_variables, "HE_BMI", "absi_bmi_category")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_WBT_FT ~ HE_BMI only (unadjusted)
variables <- c("HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_WBT_FT ~ HE_BMI, ABSI_Z only (unadjusted)
variables <- c("HE_BMI", "ABSI_Z")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_WBT_FT ~ HE_BMI, ABSI_Z, AGE, SEX only (unadjusted)
variables <- c("SEX", "AGE", "HE_BMI", "ABSI_Z")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_WBT_FT",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)




#### Stepwise variable selection - about 제지방량 ####

# variables 리스트 생성
variables <- colnames(df)

# 선택되면 안되는 변수들 빼기
variables <- variables[!variables %in% c("ID", "year", "psu", "HE_BMI", "ABSI", "ABSI_Z", "DW_WBT_FT", "absi_bmi_category")]

df2 <- df[variables]

full <- lm(DW_SBT_LN ~ ., data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

step_variables

#########################


#### LM ####

# create lm formula - DW_SBT_LN ~ ABSI_Z
variables <- c(step_variables, "ABSI_Z")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_SBT_LN ~ BMI
variables <- c(step_variables, "HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_SBT_LN ~ ABSI_Z + BMI
variables <- c(step_variables, "ABSI_Z", "HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)


# create lm formula - DW_SBT_LN ~ BMI + absi_bmi_category
variables <- c(step_variables, "HE_BMI", "absi_bmi_category")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_SBT_LN ~ BMI only (unadjusted model)
variables <- c("HE_BMI")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_SBT_LN ~ BMI + ABSI_Z only (unadjusted model)
variables <- c("HE_BMI", "ABSI_Z")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



# create lm formula - DW_SBT_LN ~ BMI + ABSI_Z + SEX + AGE only (unadjusted model)
variables <- c("HE_BMI", "ABSI_Z", "SEX", "AGE")
variables <- variables[!variables %in% c('HE_wc', 'HE_ht', 'HE_wt')]

lm_f <- as.formula(
  paste("DW_SBT_LN",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

lm_f

# fit
fit <- lm(lm_f, data = df)
summary(fit)



#########################


# corr check

var <- c("DW_WBT_FT", "ABSI_Z")
var <- c("DW_WBT_FT", "HE_BMI")
var <- c("DW_SBT_LN", "ABSI_Z")
var <- c("DW_SBT_LN", "HE_BMI")

var <- c("DW_WBT_FT", "ABSI_Z", "HE_BMI")
var <- c("DW_SBT_LN", "ABSI_Z", "HE_BMI")

cordf <- df[var]

cor(cordf)


# descriptive
variables <- c(step_variables, "DW_WBT_FT", "DW_SBT_LN", "ABSI_Z", "HE_BMI", "absi_bmi_category")
describe(df)

df2 <- df[df$absi_bmi_category == 0,]
df2 <- df[df$absi_bmi_category == 1,]
df2 <- df[df$absi_bmi_category == 2,]
df2 <- df[df$absi_bmi_category == 3,]

summary(df2[variables])

### 국건영 end. ###