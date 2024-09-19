
#### Install ####
install.packages("MatchIt")
install.packages("tableone") # SMD
install.packages("httpgd")
install.packages("gbm")
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
library(gbm)
#################


#### read df

ht <- read.csv("D:\\BFM\\HT_psm.csv", encoding = "euc-kr")
ar <- read.csv("D:\\BFM\\AR_psm.csv", encoding = "euc-kr")
dm_all <- read.csv("DM_all_psm.csv", encoding = "euc-kr")

cancer <- read.csv("data\\Cancer\\Cancer_All.csv")
mi <- read.csv("data\\MI_final_Baseline1st+OccuredOnceOrMore.csv", encoding = "euc-kr")

table(cancer$Cancer) # 270명
table(mi$final_MI) # 66명
table(ht$final_CKD) # 648명 (ht, ar, dm_all 다 동일)

################

#### merge ####
#### CKD는 merge 필요 없음

# outcome: cancer
ht <- merge(ht, cancer, by = "NIHID")
ar <- merge(ar, cancer, by = "NIHID")
dm_all <- merge(dm_all, cancer, by = "NIHID")

# outcome: MI
ht <- merge(ht, mi, by = "NIHID")
ar <- merge(ar, mi, by = "NIHID")
dm_all <- merge(dm_all, mi, by = "NIHID")

################

#### copy df to use

# HT (고혈압)
df <- copy(ht)
target <- df$ht_drug_90
psm_col <- "ht_drug_90"

# AR (관절염)
df <- copy(ar)
target <- df$ar_drug_90
psm_col <- "ar_drug_90"

# DM (당뇨병)
df <- copy(dm_all)
target <- df$dm_all_drug_90
psm_col <- "dm_all_drug_90"

###################


#### EDA

colSums(is.na(df))
describe(df)
summary(df)

df_0 <- df[df[psm_col] == 0, ]
df_1 <- df[df[psm_col] == 1, ]

describe(df_0)
describe(df_1)

#############


#### Preprocess ####

#### HT
df <- subset(df, select = -c(기수, EDATE, NIHID, DRUGHT))

# WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
df <- subset(df, select = -c(WEIGHT, HEIGHT))

# KID = 2 (신장 치료 받고 있는 사람들) 빼기
df <- df[df$KID != 2, ]
df <- subset(df, select = -c(KID))

# 운동, 알콜 관련 categorical 변수들 뺄 때 실행
df <- subset(df, select = -c(PHYACTL, PHYACTM, PHYACTH, DRK_NEW, PA_NEW))

# factor
df$SMOKE <- factor(df$SMOKE)

# # PA_NEW, DRK_NEW 전처리 (NEW 변수들 안 뺄 경우에만 실행)
# df <- df[df$PA_NEW != 0, ]
# df <- df[df$DRK_NEW != 0, ]
#
# df$DRK_NEW = factor(df$DRK_NEW)
# df$PA_NEW = factor(df$PA_NEW)


#### AR
df <- subset(df, select = -c(기수, EDATE, NIHID))

# WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
df <- subset(df, select = -c(WEIGHT, HEIGHT))

# KID = 2 (신장 치료 받고 있는 사람들) 빼기
df <- df[df$KID != 2, ]
df <- subset(df, select = -c(KID))

# 운동, 알콜 관련 categorical 변수들 뺄 때 실행
df <- subset(df, select = -c(PHYACTL, PHYACTM, PHYACTH, DRK_NEW, PA_NEW))

# factor
df$SMOKE <- factor(df$SMOKE)

# # PA_NEW, DRK_NEW 전처리 (NEW 변수들 안 뺄 경우에만 실행)
# df <- df[df$PA_NEW != 0, ]
# df <- df[df$DRK_NEW != 0, ]
#
# df$DRK_NEW = factor(df$DRK_NEW)
# df$PA_NEW = factor(df$PA_NEW)


#### DM
df <- subset(df, select = -c(기수, EDATE, NIHID, DRUGINS))

# WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
df <- subset(df, select = -c(WEIGHT, HEIGHT))

# KID = 2 (신장 치료 받고 있는 사람들) 빼기
df <- df[df$KID != 2, ]
df <- subset(df, select = -c(KID))

# 운동, 알콜 관련 categorical 변수들 뺄 때 실행
df <- subset(df, select = -c(PHYACTL, PHYACTM, PHYACTH, DRK_NEW, PA_NEW))

# factor
df$SMOKE <- factor(df$SMOKE)

# # PA_NEW, DRK_NEW 전처리 (NEW 변수들 안 뺄 경우에만 실행)
# df <- df[df$PA_NEW != 0, ]
# df <- df[df$DRK_NEW != 0, ]
#
# df$DRK_NEW = factor(df$DRK_NEW)
# df$PA_NEW = factor(df$PA_NEW)

######################


#### 유의했던 변수들만 살릴 경우 ####

# DM
df <- subset(df, select = c(
  AGE, SEX, AST_ORI, TRIGLY_ORI, DRUGHT, TOTALC, MET_CAL,
  eGFR, BMI, GLU0_ORI, KID, WAIST, HB_ORI, FMDM, dm_all_drug_90, final_CKD
))

# HT
df <- subset(df, select = c(
  AGE, SEX, TRIGLY_ORI, FMHTN, eGFR, BMI, AST_ORI, MET_CAL, R_GTP_TR,
  HB_ORI, DRUGINS, FMHEA, ht_drug_90, final_CKD
))

# AR
df <- subset(df, select = -c(GLU0_ORI, TCHL_ORI, HDL_ORI, TRIGLY_ORI, SMOKE, DRUGICD, DRUGLP, SBP, DBP, BMI))


################################


#### Variables ####

# variables 리스트 생성
variables <- colnames(df)

# 신장변수 전체 (약물변수 포함)
variables <- variables[!variables %in% c(psm_col, "final_CKD", "Cancer", "final_MI", "TIME")] # for PSM

# # pvalue > 0.5 인 변수들 추가로 빼기 (HT, 1:1)
# variables <- variables[!variables %in% c('GLU0_ORI', 'R_GTP_TR', 'ALT_ORI', 'HDL_ORI', 'HB_ORI', 'SMOKE', 'DRUGICD', 'FMDM', 'SBP')]

variables

#############


#### Stepwise variable selection - for PSM ####
#### PSM 할때는 굳이 변수 선택 할 필요 없다! 안돌려도 됨 ####

target <- df[[psm_col]]
df2 <- df[variables]

full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

#########################


# 변수들에서 인슐린 빼기 (AR일 경우만, 인슐린 매칭 x)
# ALL
variables <- variables[!variables %in% c("DRUGINS")]
# STEP
step_variables <- step_variables[!step_variables %in% c("DRUGINS")]

# 변수들에서 뇌졸중약 빼기 (DM일 경우만, ICD 매칭 x)
# ALL
variables <- variables[!variables %in% c("DRUGICD")]
# STEP
step_variables <- step_variables[!step_variables %in% c("DRUGICD")]
###########################


#### PS Matching ####

#### create psm formula - with all variables
psm_f <- as.formula(
  paste(psm_col,
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

psm_f

#### create psm formula - with only selected variables
psm_f <- as.formula(
  paste(psm_col,
    paste(step_variables, collapse = " + "),
    sep = " ~ "
  )
)

psm_f

#### Matching

# 1:1
mod_match <- matchit(psm_f, method = "full", data = df)
mod_match$model$coefficient
# 1:2
mod_match <- matchit(psm_f, method = "nearest", ratio = 2, data = df)

# 1:3
mod_match <- matchit(psm_f, method = "nearest", ratio = 3, data = df)

# 1:3 - optimal
mod_match <- matchit(psm_f, method = "optimal", ratio = 3, data = df)

# 1:3 - nearest + randomforest
mod_match <- matchit(psm_f, method = "nearest", distance = "randomforest", ratio = 3, data = df)

# 1:4
mod_match <- matchit(psm_f, method = "nearest", ratio = 4, data = df)

# # 1:5
# mod_match <- matchit(psm_f, method = "nearest", ratio = 5, data = df)

mod_match

dta_m <- match.data(mod_match)

dta_m_all <- match.data(mod_match, drop.unmatched = FALSE)

dta_m_tmp_1 <- dta_m_tmp[dta_m_tmp$weights == 1, ]
dta_m_tmp_0 <- dta_m_tmp[dta_m_tmp$weights == 0, ]

matched <- ggplot(dta_m_tmp_1, aes(x = distance)) +
  geom_histogram(binwidth = .01)
unmatched <- ggplot(dta_m_tmp_0, aes(x = distance)) +
  geom_histogram(binwidth = .01)

# draw plot to check distribution (overlapped)
ggplot(dta_m_all, aes(distance, fill = factor(weights))) +
  geom_histogram(binwidth = .01)
ggplot(dta_m_all, aes(distance, fill = factor(ar_drug_90))) +
  geom_histogram(binwidth = .01)

# draw plot to check distribution (not overlapped)
ggarrange(matched, unmatched)


# # export dta_m
# write.csv(dta_m, file = "HT_psm_4.csv", row.names = TRUE)

# vars - with all variables
vars <- c(variables)

# vars - with only selected variables
vars <- c(step_variables)

#################


# #### try GBM - predicted values are used for propensity score ####

# gps <- gbm(glm_f,
#   distribution = "bernoulli", data = df, n.trees = 100,
#   interaction.depth = 4, train.fraction = 0.8, shrinkage = 0.0005
# )
# df$gpsvalue <- predict(gps, type = "response")
# summary(gps)

# #################


#### Adding weights calculated by propensity score ####

## for the treatment group : 1 / df$distance
## for the control group : 1 / (1 - df$distance)

# Attaching weight to the dataset
dta_m$weight.ATE <- ifelse(dta_m[[psm_col]] == 1, 1 / dta_m$distance, 1 / (1 - dta_m$distance))

# dataset all
dta_m_all$weight.ATE <- ifelse(dta_m_all[[psm_col]] == 1, 1 / dta_m_all$distance, 1 / (1 - dta_m_all$distance))

############################

ggplot(dta_m_all, aes(weight.ATE)) +
  geom_histogram(binwidth = 1)


sum(dta_m_all$weight.ATE)


#### SMD ####
tabmatched <- CreateTableOne(vars = vars, strata = psm_col, data = dta_m, test = FALSE)
print(tabmatched, smd = TRUE)
# summary(tabmatched)

# calculate CKD patients in each group
# 약물 복용 x
count(dta_m[dta_m[psm_col] == 0 & dta_m$final_CKD == 0, ])
count(dta_m[dta_m[psm_col] == 0 & dta_m$final_CKD == 1, ])

# 약물 복용 o
count(dta_m[dta_m[psm_col] == 1 & dta_m$final_CKD == 0, ])
count(dta_m[dta_m[psm_col] == 1 & dta_m$final_CKD == 1, ])
#######################################

# calculate Cancer patients in each group
# 약물 복용 x
count(dta_m[dta_m[psm_col] == 0 & dta_m$Cancer == 0, ])
count(dta_m[dta_m[psm_col] == 0 & dta_m$Cancer == 1, ])

# 약물 복용 o
count(dta_m[dta_m[psm_col] == 1 & dta_m$Cancer == 0, ])
count(dta_m[dta_m[psm_col] == 1 & dta_m$Cancer == 1, ])
#######################################

# calculate MI patients in each group
# 약물 복용 x
count(dta_m[dta_m[psm_col] == 0 & dta_m$final_MI == 0, ])
count(dta_m[dta_m[psm_col] == 0 & dta_m$final_MI == 1, ])

# 약물 복용 o
count(dta_m[dta_m[psm_col] == 1 & dta_m$final_MI == 0, ])
count(dta_m[dta_m[psm_col] == 1 & dta_m$final_MI == 1, ])
#######################################

##############

# # pvalue > 0.5 인 변수들 추가로 빼기 (AR, selected, 1:3)
# variables <- variables[!variables %in% c('WAIST', 'GLU0_ORI', 'AST_ORI', 'TCHL_ORI', 'HB_ORI', 'SMOKE', 'DRUGINS', 'DRUGICD', 'FMHEA', 'FMDM', 'PRT16_U', 'TOTALC')]

# # pvalue > 0.6 인 변수들 추가로 빼기 (DM, ALL, 1:4)
# variables <- variables[!variables %in% c('SMOKE', 'DRUGHT', 'DRUGLP', 'FMHEA', 'KID', 'MET_CAL')]

tmp <- variables


#### Stepwise variable selection - for Logistic ####

target <- dta_m$final_CKD
df2 <- dta_m[variables]

full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
step_lm <- step(full, direction = "both")

summary(step_lm)

step_variables <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

#########################



#### GLM ####

# create glm formula
variables <- c(step_variables, psm_col)

# # AR - 유의하지 않은 변수들 한개씩 빼가면서 테스트
# variables <- variables[!variables %in% c('TCHL_ORI', 'DRUGICD', 'TRIGLY_ORI', 'SBP', 'GLU0_ORI',
# 'BMI', 'DRUGLP', 'DBP', 'MET_CAL', 'SEX', 'SMOKE', 'DRUGHT', 'ALT_ORI',
# 'HB_ORI', 'FMHEA')]

# # DM일 경우 실행
# variables <- variables[!variables %in% c('DRUGICD')]

# # HT일 경우 실행
# variables <- variables[!variables %in% c('DRUGINS')]

# # AR일 경우 실행
# variables <- variables[!variables %in% c('DRUGINS')]

glm_f <- as.formula(
  paste("final_CKD",
    paste(variables, collapse = " + "),
    sep = " ~ "
  )
)

glm_f

# fit
fit <- glm(glm_f, family = binomial(link = "logit"), data = dta_m)
summary(fit)

# fit - with weights
fit <- glm(glm_f, family = binomial(link = "logit"), data = dta_m, weights = (weight.ATE))
summary(fit)

# fit - weight랑 treat만 넣어서 확인해보기
fit <- glm(Cancer ~ ar_drug_90, family = binomial(link = "logit"), data = dta_m, weights = (weight.ATE))
summary(fit)

####################

#### calculate weighted mean and var ####
# install.packages("Hmisc")
library(Hmisc)

v <- c("AGE")
v <- c("WAIST")
v <- c("TRIGLY_ORI")
v <- c("FMHEA")
v <- c("eGFR")

ggplot(dta_m_all, aes(WAIST)) + geom_histogram(binwidth = 1)

# mean
print(v)
wtd.mean(dta_m_all[[v]][which(dta_m_all[[psm_col]] == 0)], dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 0)], normwt = FALSE)
wtd.mean(dta_m_all[[v]][which(dta_m_all[[psm_col]] == 1)], dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 1)], normwt = FALSE)

# var
print(v)
wtd.var(dta_m_all[[v]][which(dta_m_all[[psm_col]] == 0)], dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 0)], normwt = FALSE)
wtd.var(dta_m_all[[v]][which(dta_m_all[[psm_col]] == 1)], dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 1)], normwt = FALSE)

describe(dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 1)])
describe(dta_m_all$weight.ATE[which(dta_m_all[[psm_col]] == 0)])

describe(dta_m_all$distance[which(dta_m_all[[psm_col]] == 1)])
describe(dta_m_all$distance[which(dta_m_all[[psm_col]] == 0)])

x = dta_m_all[[v]][which(dta_m_all[[psm_col]] == 0)]
y = dta_m_all[[v]][which(dta_m_all[[psm_col]] == 1)]

qqplot(x, y)

##############################################################

#### try GBM - predicted values are used for propensity score ####

gps <- gbm(glm_f,
  distribution = "bernoulli", data = df, n.trees = 100,
  interaction.depth = 4, train.fraction = 0.8, shrinkage = 0.0005
)

summary(gps)

#################

#### Odds Ratio ####
# odds ratio function
ORtable <- function(x, digits = 2) {
  suppressMessages(a <- confint(x))
  result <- data.frame(exp(coef(x)), exp(a))
  result <- round(result, digits)
  result <- cbind(result, round(summary(x)$coefficient[, 4], 3))
  colnames(result) <- c("OR", "2.5%", "97.5%", "p")
  result
}

ORtable(fit)
####################

table(df$SMOKE)
vif(fit)
################# end #################

t.test(Cancer ~ ar_drug_90, data = dta_m_all)

ggplot(dta_m, aes(weight.ATE)) + geom_histogram(binwidth = 5)
ggplot(dta_m, aes(distance)) + geom_histogram(binwidth = .01)

describe(dta_m$weight.ATE)
describe(dta_m$distance)