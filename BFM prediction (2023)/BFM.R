
install.packages("languageserver")

# 여러줄 주석처리 ctrl+ shift + c
#### install ####
install.packages("forecast")
install.packages("Metrics")
install.packages("writexl")
install.packages("kableExtra")
install.packages("caret")
install.packages('randomForest')
#############

#### library ####
library(dplyr)
library(forecast)
library(Metrics)
library(broom)
library(kableExtra)
library(caret)
library(randomForest)
#################

setwd('D:\\SNUlab\\')

# BFM df #
  df <- read.csv("testdf1001_final.csv", encoding = 'euc-kr')
  # 이상치 행 삭제 # 
df$ID=1:dim(df)[1]

df=df[-which(df$ID%in%c(4450, 16871, 5442, 3755, 11259, 12170, 521, 14506, 718, 3768, 3571, 2629, 14566, 3998, 3981, 16269, 5593, 4453, 
                        5473, 6912, 15580, 1320, 4463, 17111, 2017, 12621, 16731, 1998, 4457, 11400, 14921)),]

# df <- df[-c(16871, 5442, 12170, 718, 11259, 3755, 16269, 14506, 2629, 521, 3571, 4453, 14566, 5593, 13316, 3768, 3998, 
#               543, 6912, 11544, 4463, 16731, 5473, 12621, 15587, 16523),]

colnames(df)  
  
###############
  
# LBM+ASM df #
  df <- read.csv("testdf1007_LBM+ASM_final.csv", encoding = 'euc-kr')
  # 이상치 행 삭제 # 
  df <- df[-c(11421, 504, 13504, 694, 15180, 15034, 9602, 11421, 1560, 12423, 3435, 2552, 3597, 524, 14997, 3823, 15746, 4261, 
              13819, 11831, 4264),]
#################


# 승민샘 논문과 통일 작업 #
# 20 - 65세만 남기기 #
df <- df[(df$age >= 20) & (df$age <= 65), ]  # 15751 -> 12490


## wc 변수 제거할 경우 실행##
df <- subset(df, select=-c(HE_wc))
  
##change column names##
colnames(df)[colnames(df) == 'DW_WBT_FT'] <- '총지방량'
  
# train test split
set.seed(1512315)
dt = sort(sample(nrow(df), nrow(df)*.7))
train<-df[dt,]
test<-df[-dt,]


### 총지방량 주요변수만 사용 / 국건영 ####
# BS3_1_1.0 : 현재 흡연자
# BS3_1_2.0 : 과거 흡연, 현재 흡연하지 않음

# BFM fit, with HE_wc #
fit <- lm(총지방량 ~  HE_wt + HE_wc + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# plot(fit) + geom_text(aes(label = ID), size = 8)
# 
# train[train$ID == 4457, ]

# BFM fit, without HE_wc #
fit <- lm(총지방량 ~  HE_wt + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# LBM fit, with HE_wc #
fit <- lm(LBM ~  HE_wt + HE_wc + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)
#train$predicted=predict(fit,newdata=train,type="response")
#rmse(train$predicted,train$LBM)
#summary(lm(LBM~predicted,data=train))

# LBM fit, without HE_wc #
fit <- lm(LBM ~  HE_wt + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# ASM fit, with HE_wc #
fit <- lm(DW_WBT_BMC ~  HE_wt + HE_wc + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# ASM fit, without HE_wc #
fit <- lm(DW_WBT_BMC ~  HE_wt + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# HE_wt - LBM fit, with HE_wc #
fit <- lm(HE_wt - LBM ~  HE_wt + HE_wc + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + HE_chol + age + HE_sbp_tr, data = train)

# LBM fit, without HE_wc and chol#
fit <- lm(LBM ~  HE_wt + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + age + HE_sbp_tr, data = train)

# BFM fit, without HE_wc and chol#
fit <- lm(총지방량 ~  HE_wt + BS3_1_1.0 + BS3_1_2.0 + HE_ht + sex_1.0 + age + HE_sbp_tr, data = train)

################

formula(fit)
summary(fit)
aov_fit=summary(aov(fit))

sum(aov_fit[[1]][,3])
sum_fit=summary(fit)
sum_fit$r.squared
dim(train)[1]
SSE=sqrt(var(train$총지방량))*sqrt((1-summary(fit)$r.squared)*(dim(train)[1]-1)/(dim(train)[1]-2))


# 총지방량 fitted RMSE #
rmse(fit$fitted.values,train$'총지방량')
# LBM fitted RMSE #
rmse(fit$fitted.values,train$'LBM')

# ASM fitted RMSE #
rmse(fit$fitted.values,train$'DW_WBT_BMC')


# predict
test$predicted=predict(fit, newdata = test,type="response")


# 총지방량 RMSE # 
rmse(test$'총지방량', test$predicted) 

# LBM RMSE # 
rmse(test$'LBM', test$predicted) 

# ASM RMSE # 
rmse(test$'DW_WBT_BMC', test$predicted) 


# (WEIGHT - predicted LBM) & 총지방량 RMSE # 
rmse(test$'HE_wt' - test$predicted, test$'총지방량')


# visualization
plot(fit)
hist(scale(fit$residuals))


################################################################

### 총지방량 주요변수만 사용 / 안산안성코호트 ####

## BFM df ##
  df2 <- read.csv('MME_preprocessed_eGFR.csv', encoding = 'euc-kr')
  # 이상치 행 제거 #
  df2 <- df2[-c(13364),]
############

## BFM test - 10.19 ##    
  df2 <- read.csv('MME_.csv', encoding = 'euc-kr')

## LBM df ##
df2 <- read.csv('MME_preprocessed_eGFR.csv', encoding = 'euc-kr')

############

## 근육량 df ## test
df2 <- read.csv('MME_preprocessed_eGFR+근육량.csv', encoding = 'euc-kr')
############
  
##change column names##
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


colnames(df2)[colnames(df2) == 'SBP'] <- 'HE_sbp_tr'


# HE_wt 단위 변경 (g -> kg) # 
df2$HE_wt <- df2$HE_wt / 1000

## wc 변수 제거할 경우 실행##
df2 <- subset(df2, select=-c(HE_wc))

#predict
df2$predicted=predict(fit, newdata = df2, type="response")

sum(is.na(df2$predicted))

# write.csv(df2, file = "MME_seo_BFMpredicted.csv", row.names = FALSE)
# write.csv(df2, file = "MME_seo_LBMpredicted.csv", row.names = FALSE)

rmse(df2$'총지방량'[which(df2$기수 == 'A01')], df2$predicted[which(df2$기수 == 'A01')]) 

# BFM RMSE #
rmse(df2$'총지방량', df2$predicted) 
# BFM plot # 
plot(df2$'총지방량', df2$predicted)
# BFM summary # 
summary(lm(총지방량 ~ predicted, data = df2))


# LBM RMSE #
rmse(df2$'LBM', df2$predicted)
# LBM plot # 
plot(df2$'LBM', df2$predicted)
# LBM summary # 
summary(lm(LBM ~ predicted, data = df2))


# HE_wt - LBM RMSE #
rmse(df2$'HE_wt' - df2$'LBM', df2$predicted) 
# LBM plot # 
plot(df2$'HE_wt' - df2$'LBM', df2$predicted)
# LBM summary # 
summary(lm(LBM ~ predicted, data = df2))


# (WEIGHT - predicted LBM) & 총지방량 RMSE # 
rmse(df2$'HE_wt' - df2$predicted, df2$'총지방량')


# IB1_3 test
rmse(df2$'IB1_3', df2$predicted)
plot(df2$'IB1_3', df2$predicted)

#######################################################


### 총지방량 주요변수만 사용 / 검증센터자료 ####

# BFM df #
df3 <- read.csv('건증센터_preprocessed.csv', encoding = 'euc-kr')

# LBM df #
df3 <- read.csv('검증센터_preprocessed_LBM.csv', encoding = 'euc-kr')

##change column names##
colnames(df3)[colnames(df3) == '체지방'] <- '총지방량'
colnames(df3)[colnames(df3) == '체중'] <- 'HE_wt'
colnames(df3)[colnames(df3) == '신장'] <- 'HE_ht'
colnames(df3)[colnames(df3) == 'cholesterol'] <- 'HE_chol'
colnames(df3)[colnames(df3) == '수진시나이'] <- 'age'

colnames(df3)[colnames(df3) == '흡연여부_2.0'] <- 'BS3_1_2.0'
colnames(df3)[colnames(df3) == '흡연여부_3.0'] <- 'BS3_1_1.0'
colnames(df3)[colnames(df3) == '성별_M'] <- 'sex_1.0'
colnames(df3)[colnames(df3) == 'SBP'] <- 'HE_sbp_tr'


#predict
df3$predicted=predict(fit, newdata = df3,type="response")

# BFM RMSE #
rmse(df3$'총지방량', df3$predicted) 
# BFM plot #
plot(df3$'총지방량', df3$predicted)
abline(0,1,col="red")

# LBM RMSE #
rmse(df3$'LBM', df3$predicted) 
# LBM plot #
plot(df3$'LBM', df3$predicted)
abline(0,1,col="red")

# (WEIGHT - predicted LBM) & 총지방량 RMSE # 
rmse(df3$'HE_wt' - df3$predicted, df3$'총지방량')


#######################################################
##### end ######




### stepwise로 변수 select
dftmp <- read.csv('LBMstepwise_1013.csv', encoding = 'euc-kr')
full <- lm(LBM ~ ., data = dftmp)

step_lm_fat<-step(full, direction = 'both')

formula(step_lm_fat)
summary(step_lm_fat)





## 총골밀도
#lm.init <- lm(총골밀도 ~ age + HE_ht + HE_wt + HE_BMI + HE_sbp_tr + 
#                    HE_dbp_tr + HE_Upro + HE_glu + HE_chol + 
#                    HE_wc + HE_HDL_st2 + HE_crea , data = df_model_bone)

lm.full <- lm(총골밀도 ~ ., data = df_model_bone)

step_lm_bone<-step(lm.full, verbose=T)

summary(step_lm_bone)
