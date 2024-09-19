#### library ####
library(dplyr)
library(forecast)
library(Metrics)
library(broom)
library(caret)
library(lme4)
library(sjPlot)
library(MASS)
#################

### CKD (categorical)는 여기서

## df
eGFR_change_df <- read.csv('eGFR_change_df.csv', encoding = 'euc-kr')
eGFR_change2_df <- read.csv('eGFR_change2_df.csv', encoding = 'euc-kr')
CKD_df <- read.csv('CKD_df.csv', encoding = 'euc-kr')
FM_change1_df <- read.csv('FM_change1_df.csv', encoding = 'euc-kr')
FM_change2_df <- read.csv('FM_change2_df.csv', encoding = 'euc-kr')
FM_change3_df <- read.csv('FM_change3_df.csv', encoding = 'euc-kr')

colSums(is.na(eGFR_change_df))
describe(eGFR_change_df)
summary(eGFR_change_df)

CKD_df <- subset(CKD_df, select=-c(기수, EDATE, NIHID))
CKD_df$BODYFAT=CKD_df$BODYFAT/1000
CKD_df$SMOKE=factor(CKD_df$SMOKE)
CKD_df$DRK_NEW=factor(CKD_df$DRK_NEW)
#eGFR_change_df$yoyo_10=factor(eGFR_change_df$yoyo_10)
CKD_df$yoyo_03_new=ifelse(CKD_df$yoyo_03==0,0,ifelse(CKD_df$yoyo_03==1,1,2))

table(df$DRUGHT)
table(df$DRUGINS)
table(df$DRUGICD)
table(df$DRUGLP)

############
fit <- glm(CKD ~ AGE + SEX + HEIGHT + WEIGHT + WAIST + 
            AST_ORI + ALT_ORI + TCHL_ORI + HDL_ORI + TRIGLY_ORI + DRUGINS + 
            DRUGHT + DRUGLP + FMDM + TREATD14 + BODYFAT + DRK_NEW,family=binomial(link="logit"), data = df)

fit <- glm(CKD ~ HEIGHT + AST_ORI + TREATD5 + PRT16_U + TREATD14 + 
      BODYFAT + eGFR, family = binomial(link = "logit"), data = df)
summary(fit)

fit <- glm(CKD ~ HEIGHT + AST_ORI + TREATD5 + PRT16_U + TREATD14 + 
             BODYFAT + eGFR + yoyo_03, family = binomial(link = "logit"), data = df)
summary(fit)

fit <- glm(CKD ~ HEIGHT + AST_ORI + TREATD5 + PRT16_U + TREATD14 + 
             BODYFAT + eGFR + yoyo_05, family = binomial(link = "logit"), data = df)
summary(fit)


fit <- glm(CKD ~ HEIGHT + AST_ORI + TREATD5 + PRT16_U + TREATD14 + 
             BODYFAT + eGFR + yoyo_07, family = binomial(link = "logit"), data = df)
summary(fit)

fit <- glm(CKD ~ HEIGHT + AST_ORI + TREATD5 + PRT16_U + TREATD14 + 
             BODYFAT + eGFR + yoyo_10, family = binomial(link = "logit"), data = df)
summary(fit)



### stepwise로 변수 select

full <- glm(CKD ~ .,family=binomial(link="logit"),data = df)

fit=step_lm_eGFR<-step(full, direction = 'both')
summary(fit)
#WEIGHT, HEIGHT, BMI, WAIST, BODYFAT
formula(step_lm_eGFR)
summary(step_lm_eGFR)

fit2=update(fit,.~.,+BODYFAT)

fit2=(glm(CKD ~ WEIGHT + AST_ORI + TREATD5 + DRUGLP + PRT16_U + 
            TREATD14 + TOTALC + BODYFAT + eGFR + BMI+yoyo_10, family = binomial(link = "logit"),data=CKD_df))


#library(car)
#vif(fit2)
summary(fit2)

