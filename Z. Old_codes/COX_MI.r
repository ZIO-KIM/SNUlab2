
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

mi <- read.csv('data\\MI_psm.csv', encoding = 'euc-kr')

df = copy(mi)

#### MI
  # 기본변수 빼기
  df <- subset(df, select = -c(기수, EDATE, NIHID))

  # CKD도 빼기
  df <- subset(df, select = -c(final_CKD))

  # WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
  df <- subset(df, select = -c(WEIGHT, HEIGHT))

  # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select=-c(PHYACTL, PHYACTM, PHYACTH, TOTALC))
  
  # PA_NEW, DRK_NEW 전처리 (NEW 변수들 안 뺄 경우에만 실행)
  df <- df[df$PA_NEW != 0, ]
  df <- df[df$DRK_NEW != 0, ]
  
  # factor
  df$SMOKE = factor(df$SMOKE)
  df$SEX = factor(df$SEX)

  df$DRK_NEW = factor(df$DRK_NEW)
  df$PA_NEW = factor(df$PA_NEW)

#########################


#### Variables #### 

  # variables 리스트 생성
  variables <- colnames(df)

  # 신장변수 전체 (약물변수 포함)
  variables <- variables[!variables %in% c('final_MI', 'TIME')]

  variables

#############


#### COX ####

    # treatment가 안들어갔는데 이게 맞나? 
    # treatment를 주고, time 기간 동안의 treatment 1/0에 따른 생존률을 비교하는 게 아닌가? 
    # fit
    fit <- coxph(Surv(TIME, final_MI) ~ AGE + SEX + WAIST + GLU0_ORI + R_GTP_TR + AST_ORI + ALT_ORI + TCHL_ORI + HDL_ORI +  # nolint
    TRIGLY_ORI + HB_ORI + SMOKE + DRUGINS + DRUGHT + DRUGICD + DRUGLP +
    FMHTN + FMHEA + FMDM + PRT16_U + KID + MET_CAL + PA_NEW + SBP + DBP + eGFR + BMI + DRK_NEW, data = df) # nolint
    summary(fit)

    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint
    
#####################