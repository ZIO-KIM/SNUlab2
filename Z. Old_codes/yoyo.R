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
#################

#### read df

eGFR_change_df <- read.csv('eGFR_change_df_1th.csv', encoding = 'euc-kr') # baseline 1��
eGFR_change2_df <- read.csv('eGFR_change2_df_5th.csv', encoding = 'euc-kr') # baseline 5��
# CKD_df <- read.csv('CKD_df.csv', encoding = 'euc-kr')  # ����
FM_change1_df <- read.csv('FM_change1_df_1th.csv', encoding = 'euc-kr') # baseline 1��
FM_change2_df <- read.csv('FM_change2_df_1th.csv', encoding = 'euc-kr') # baseline 1��
FM_change3_df <- read.csv('FM_change3_df_1th.csv', encoding = 'euc-kr') # baseline 1��

CKD_df <- read.csv('CKD_df_5th.csv', encoding = 'euc-kr') # baseline 5��

###################


#### copy df to use

# eGFR_change (eGFR 5�� - eGFR 1��)
df <- copy(eGFR_change_df)
target <- df$eGFR_change
target_name <- "eGFR_change"
df_selection <- subset(df, select = -c(eGFR_change))

# eGFR_change2 (eGFR 7�� - eGFR 5��)
df <- copy(eGFR_change2_df)
target <- df$eGFR_change2
target_name <- "eGFR_change2"
df_selection <- subset(df, select = -c(eGFR_change2))

# FM_change1 (BFM 5�� - BFM 1��)
df <- copy(FM_change1_df)
target <- df$FM_change1
target_name <- "FM_change1"
df_selection <- subset(df, select = -c(FM_change1))

# FM_change2 ((TW - LBMhat)5�� - (TW - LBMhat)1��)
df <- copy(FM_change2_df)
target <- df$FM_change2
target_name <- "FM_change2"
df_selection <- subset(df, select = -c(FM_change2))

# FM_change3 (BFMhat 5�� - BFMhat 1��)
df <- copy(FM_change3_df)
target <- df$FM_change3
target_name <- "FM_change3"
df_selection <- subset(df, select = -c(FM_change3))

# CKD_df
df <- copy(CKD_df)
target <- df$CKD
target_name <- "CKD"
df_selection <- subset(df, select = -c(CKD))

#############


#### EDA

colSums(is.na(df))
describe(df)
summary(df)

#############


#### Preprocess

df <- subset(df, select=-c(���, EDATE, NIHID))
df$BODYFAT = df$BODYFAT/1000
df$SMOKE = factor(df$SMOKE)
df$DRK_NEW = factor(df$DRK_NEW)
df$PA_NEW = factor(df$PA_NEW)

# reassign yoyo values 3 to 2
df$yoyo_03[df$yoyo_03 == 3] <- 2
df$yoyo_05[df$yoyo_05 == 3] <- 2
df$yoyo_07[df$yoyo_07 == 3] <- 2
df$yoyo_10[df$yoyo_10 == 3] <- 2

# table(df$yoyo_07)

df_selection <- subset(df_selection, select=-c(���, EDATE, NIHID))
df_selection$BODYFAT = df_selection$BODYFAT/1000
df_selection$SMOKE = factor(df_selection$SMOKE)
df_selection$DRK_NEW = factor(df_selection$DRK_NEW)
df_selection$PA_NEW = factor(df_selection$PA_NEW)

tmp = copy(df)

# eGFR_change_df$yoyo_10=factor(eGFR_change_df$yoyo_10)
# CKD_df$yoyo_03_new=ifelse(CKD_df$yoyo_03==0,0,ifelse(CKD_df$yoyo_03==1,1,2))

############


#### Stepwise variable selection

full <- lm(target ~ ., data = df_selection)
step_lm <- step(full, direction = 'both')

summary(step_lm)

############


#### Unadjusted fit

  # yoyo_03
  fit <- lm(target ~ yoyo_03, data = df)
  summary(fit)
  
  # yoyo_05
  fit <- lm(target ~ yoyo_05, data = df)
  summary(fit)
  
  # yoyo_07
  fit <- lm(target ~ yoyo_07, data = df)
  summary(fit)
  
  # yoyo_10
  fit <- lm(target ~ yoyo_10, data = df)
  summary(fit)

##############
  
  
#### Adjusted Fit

  all_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise�� ���õ� �����鸸 ��������
  all_variables <- all_variables[!all_variables %in% c('yoyo_03', 'yoyo_05', 'yoyo_07', 'yoyo_10')] # yoyo ������ ���õǾ��� ��� ���ֱ�
  
  yoyo_added_variables <- c(all.vars(formula(step_lm)[[3]]))

  # selected variable
  fit <- lm(formula(step_lm), data = df)
  
  vif(fit)
  summary(fit)

  # selected variable + yoyo_03
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)

  if ('yoyo_03' %in% all_variables == FALSE) {  # yoyo_03 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_03')
  }

  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )

  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  # selected variable + yoyo_03 factor
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_03' %in% all_variables == FALSE) {  # yoyo_03 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_03')  
  }
  
  df$yoyo_03 = factor(df$yoyo_03)
  
  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)

  # selected variable + yoyo_05
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)

  if ('yoyo_05' %in% all_variables == FALSE) {  # yoyo_05 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_05')  
  }

  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )

  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  # selected variable + yoyo_05 factor
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_05' %in% all_variables == FALSE) {  # yoyo_05 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_05')  
  }
  
  df$yoyo_05 = factor(df$yoyo_05)
  
  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  # selected variable + yoyo_07
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_07' %in% all_variables == FALSE) {  # yoyo_07 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_07')  
  }

  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  cor(df$BMI, df$WAIST)
  
  # selected variable + yoyo_07 factor
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_07' %in% all_variables == FALSE) {  # yoyo_07 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_07')  
  }
  
  df$yoyo_07 = factor(df$yoyo_07)
  
  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  # selected variable + yoyo_10
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_10' %in% all_variables == FALSE) {  # yoyo_10 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_10')  
  }
  
  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)
  
  # selected variable + yoyo_10 factor
  
  df = copy(tmp)
  yoyo_added_variables = copy(all_variables)
  
  if ('yoyo_10' %in% all_variables == FALSE) {  # yoyo_10 ���� �ȵ��� ��쿡��
    yoyo_added_variables <- append(all_variables, 'yoyo_10')  
  }
  
  df$yoyo_10 = factor(df$yoyo_10)
  
  f <- as.formula(
    paste(target_name, 
          paste(yoyo_added_variables, collapse = " + "), 
          sep = " ~ ")
  )
  
  fit <- lm(f, data = df)
  
  vif(fit)
  summary(fit)

################
