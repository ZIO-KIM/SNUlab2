#### install ####
install.packages("sjPlot")
######################

#### library ####
library(dplyr)
library(forecast)
library(Metrics)
library(broom)
library(caret)
library(lme4)
library(sjPlot)
#################

# BFM.R에서 국건영 fit 모델 생성하는 부분까지 돌리고 넘어오기
# MME 데이터는 다 df2 변수에 담기

# 전체 변수 다 있는 df #
df2 <- read.csv('MME_preprocessed_eGFR_allcolumns.csv', encoding = 'euc-kr')

# 주요변수만 있는 df #
df2 <- read.csv('eGFR_final_dummies.csv', encoding = 'euc-kr')

##### 안산안성코호트 ######

##change selected column names##
colnames(df2)[colnames(df2) == 'BODYFAT'] <- '총지방량'
colnames(df2)[colnames(df2) == 'WEIGHT'] <- 'HE_wt'
colnames(df2)[colnames(df2) == 'HEIGHT'] <- 'HE_ht'
colnames(df2)[colnames(df2) == 'WAIST'] <- 'HE_wc'
colnames(df2)[colnames(df2) == 'TCHL_ORI'] <- 'HE_chol'
colnames(df2)[colnames(df2) == 'AGE'] <- 'age'
colnames(df2)[colnames(df2) == 'SMOKE_2.0'] <- 'BS3_1_2.0'
colnames(df2)[colnames(df2) == 'SMOKE_3.0'] <- 'BS3_1_1.0'
colnames(df2)[colnames(df2) == 'SEX_1'] <- 'sex_1.0'
colnames(df2)[colnames(df2) == 'SBP_AVG'] <- 'HE_sbp_tr'

## predict ##
#fit은 국건영 data로 만든 BFM 예측 모델
df2$predicted=predict(fit, newdata = df2,type="response")

# train test split
dt = sort(sample(nrow(df2), nrow(df2)*.7))
train<-df2[dt,]
test<-df2[-dt,]

# 1. 기존 체지방으로 모델 fit

  # 1-1. 나머지 변수는 eGFR 반응변수로 해서 stepwise 돌린 결과 선택된 변수들 (+ ID에 random effect 넣어줌)
  model1 <- lmer(eGFR ~ 총지방량 + BUN_ORI + EDATE + CREATININ_ORI + INCELL + age + SEX + BUN_TR + EXCELL + HE_chol + DRUGINS + RBC_U + 
                   DBP_R + SBP_R + HBA1C + BODYPRT + HE_ht + TREATD18C + KID + TCHL_TR + TREATD18H + TAKFQ + CRYSTAL4_U + (1 | NIHID), 
                 data = train)
  
  # 1-2. 나머지 변수를 weight cycle에 들어가는 주요변수들로 했을 때
  model1 <- lmer(eGFR ~ 총지방량 + age + HE_wt + HE_ht + HE_wc + HE_chol + BS3_1_2.0 + BS3_1_1.0 + sex_1.0 + HE_sbp_tr + (1 | NIHID), 
                 data = train)
  
  summary(model1)
  tab_model(model1)
#################################

# 2. 국건영 모델로 predict한 지방량으로 모델 fit
  
  # 2-1. 나머지 변수는 eGFR 반응변수로 해서 stepwise 돌린 결과 선택된 변수들 (+ ID에 random effect 넣어줌)
  model2 <- lmer(eGFR ~ predicted + BUN_ORI + EDATE + CREATININ_ORI + INCELL + age + SEX + BUN_TR + EXCELL + HE_chol + DRUGINS + RBC_U + 
                   DBP_R + SBP_R + HBA1C + BODYPRT + HE_ht + TREATD18C + KID + TCHL_TR + TREATD18H + TAKFQ + CRYSTAL4_U + (1 | NIHID), 
                 data = train)
  
  # 2-2. 나머지 변수를 weight cycle에 들어가는 주요변수들로 했을 때
  model2 <- lmer(eGFR ~ predicted + age + HE_wt + HE_ht + HE_wc + HE_chol + BS3_1_2.0 + BS3_1_1.0 + sex_1.0 + HE_sbp_tr + (1 | NIHID), 
                 data = train)
  
  summary(model2)
  tab_model(model2)
#################################

df2$predicted_1=predict(model1, newdata = df2,type="response", allow.new.levels = TRUE)
df2$predicted_2=predict(model2, newdata = df2,type="response", allow.new.levels = TRUE)

plot(df2$predicted_1, df2$predicted_2)
abline(0,1,col="red")
