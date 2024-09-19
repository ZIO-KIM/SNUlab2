
#### Install ####
install.packages('depmixS4') 
install.packages('poLCA')

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
library(depmixS4)
library(poLCA)
library(gmodels)
library(ggplot2)
library(survival)
library(survminer)
#################



#### read df

mme <- read.csv('data\\MME_data\\MME_preprocessed.csv', encoding = 'euc-kr')
mme_f <- read.csv('data\\MME_data\\MME_F_preprocessed.csv', encoding = 'euc-kr')
mme_m <- read.csv('data\\MME_data\\MME_M_preprocessed.csv', encoding = 'euc-kr')


# simLCA <- function(alpha, ProbMat, sampleSize, nsim) {
#     ## Reformat the probability Matrix to a form required by poLCA.simdata

#     probs = vector("list", nrow(ProbMat))
    
#     for (i in 1:nrow(ProbMat)) {
#     probs[[i]] = cbind(ProbMat[i,], 1 - ProbMat[i,])
#     }
    
#     X = poLCA.simdata(N = sampleSize*nsim, probs = probs, P = alpha)$dat
#     split(X, rep(1:nsim, each=sampleSize))
# }

# set.seed(37421)
# X = simLCA(alpha, mme, sampleSize=50, nsim=1)[[1]]
# head(X)

colnames(mme)
sum(is.na(mme))
describe(mme)

ggplot(mme, aes(x=AGE)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=GLU0_ORI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=R_GTP_TR)) + geom_histogram(binwidth=.5) #
ggplot(mme, aes(x=AST_ORI)) + geom_histogram(binwidth=.5) #
ggplot(mme, aes(x=ALT_ORI)) + geom_histogram(binwidth=.5) # 
ggplot(mme, aes(x=TCHL_ORI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=HDL_ORI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=TRIGLY_ORI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=HB_ORI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=MET_CAL)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=SBP)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=DBP)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=eGFR)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=BMI)) + geom_histogram(binwidth=.5)
ggplot(mme, aes(x=PHYACTH)) + geom_histogram(binwidth=.5)


#### 2 CLASS, MME ALL ####
# R_GTP_ORI, AST_ORI, ALT_ORI 뺌 - 대다수가 0이라 에러 나는 것 같음
# TOTALC 마찬가지로 뺌
model_definition <- mix(list(AGE ~ 1, GLU0_ORI ~ 1, 
TCHL_ORI ~ 1, HDL_ORI ~ 1, TRIGLY_ORI ~ 1, HB_ORI ~ 1, 
PHYACTL ~ 1, PHYACTM ~ 1, PHYACTH ~ 1, SBP ~ 1, DBP ~ 1, eGFR ~ 1, BMI ~ 1),
 family = list(gaussian(), #For every corresponding 
 gaussian(),  #  indicator a family of distribution 
 gaussian(),  # should be indicated in the list.
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(), 
 gaussian(), gaussian()),
 data = mme,
 nstates = 2, #This is the number of classes
 nstart = c(0.5, 0.5), # Prior probabilities of classes
 initdata = mme # Our data
)

# Fit model
set.seed(21113)
fit.mod <- fit(model_definition)  # 오래걸림

fit.mod

summary(fit.mod)

# # entropy 계산 (안됨)
# entropy_R2 <- function(fit) {
#   entropy <- function(p) sum(-p * log(p))
#   error_prior <- entropy(fit$P) # Class proportions
#   error_post <- mean(apply(fit$posterior, 1, entropy))
#   R2_entropy <- (error_prior - error_post) / error_prior
#   R2_entropy
# }
# entropy_R2(fit.mod)

posterior.states <- posterior(fit.mod)
table(posterior.states$state)

mme_class <- cbind(mme, posterior.states)

table(mme_class$state)

#### 2 CLASS, MME M ####

sum(is.na(mme_m))
sum(is.infinite(mme_m))

# R_GTP_ORI, AST_ORI, ALT_ORI 뺌 - 대다수가 0이라 에러 나는 것 같음
# TOTALC 마찬가지로 뺌
model_definition <- mix(list(AGE ~ 1, GLU0_ORI ~ 1, 
TCHL_ORI ~ 1, HDL_ORI ~ 1, TRIGLY_ORI ~ 1, HB_ORI ~ 1, 
PHYACTL ~ 1, PHYACTM ~ 1, PHYACTH ~ 1, SBP ~ 1, DBP ~ 1, eGFR ~ 1, BMI ~ 1),
 family = list(gaussian(), # For every corresponding 
 gaussian(),  #  indicator a family of distribution 
 gaussian(),  # should be indicated in the list.
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(),
 gaussian(), gaussian()),
 data = mme_m,
 nstates = 2, # This is the number of classes
 nstart = c(0.5, 0.5), # Prior probabilities of classes
# initdata = mme, # Our data
 respstart=runif(52)
)
fit(model_definition)
summary(model_definition)

# Fit model
set.seed(20)
fit.mod <- fit(model_definition)


posterior.states_m <- posterior(fit.mod)
table(posterior.states_m$state)

mme_mclass <- cbind(mme_m, posterior.states_m)

write.csv(mme_mclass, file = 'mme_mclass.csv')

table(mme_mclass$state,mme_mclass$SMOKE)

colnames(mme_mclass)

tapply(mme_mclass$AGE, mme_mclass$state, summary)
tapply(mme_mclass$BMI, mme_mclass$state, summary)
tapply(mme_mclass$SBP, mme_mclass$state, summary)
tapply(mme_mclass$DBP, mme_mclass$state, summary)

tapply(mme_mclass$eGFR, mme_mclass$state, summary)

tapply(mme_mclass$GLU0_ORI, mme_mclass$state, summary)
tapply(mme_mclass$AST_ORI, mme_mclass$state, summary)
tapply(mme_mclass$TRIGLY_ORI, mme_mclass$state, summary)
tapply(mme_mclass$TCHL_ORI, mme_mclass$state, summary)
tapply(mme_mclass$MET_CAL, mme_mclass$state, summary)



summary(fit.mod)



#### 2 CLASS, MME F ####
# R_GTP_ORI, AST_ORI, ALT_ORI 뺌 - 대다수가 0이라 에러 나는 것 같음
# TOTALC 마찬가지로 뺌
model_definition <- mix(list(AGE ~ 1, GLU0_ORI ~ 1, 
TCHL_ORI ~ 1, HDL_ORI ~ 1, TRIGLY_ORI ~ 1, HB_ORI ~ 1, 
PHYACTL ~ 1, PHYACTM ~ 1, PHYACTH ~ 1, SBP ~ 1, DBP ~ 1, eGFR ~ 1, BMI ~ 1),
 family = list(gaussian(), #For every corresponding 
 gaussian(),  #  indicator a family of distribution 
 gaussian(),  # should be indicated in the list.
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(), gaussian(),
 gaussian(), gaussian(), 
 gaussian(), gaussian()),
 data = mme_f,
 nstates = 2, #This is the number of classes
 nstart = c(0.5, 0.5), # Prior probabilities of classes
 initdata = mme_f #Our data
)

# Fit model
set.seed(21113)
fit.mod <- fit(model_definition)  # 오래걸림
fit.mod

summary(fit.mod)

posterior.states_m <- posterior(fit.mod)
table(posterior.states_m$state)

mme_fclass <- cbind(mme_f, posterior.states_m)

# reassign female class values 1,2 to 3,4
mme_fclass$state[mme_fclass$state == 1] <- 4
mme_fclass$state[mme_fclass$state == 2] <- 3

# mme_fclass <- subset(mme_fclass, select = -c(X, X.1))
# mme_mclass <- subset(mme_mclass, select = -c(X))

write.csv(mme_fclass, file = 'mme_fclass.csv', row.names = FALSE)
# write.csv(mme_mclass, file = 'mme_mclass.csv', row.names = FALSE)


# create group2, group3 finaldf
mme_group2_mine <- rbind(mme_mclass, mme_fclass)

write.csv(mme_group2_mine, file = 'mme_class_mine.csv', row.names = FALSE)




#### class 생성 데이터로 fit

ckd = read.csv('CKD_final_Baseline1st+OccuredTwiceOrMoreInARow.csv', encoding = 'euc-kr')
mi = read.csv('data\\MI_final_Baseline1st+OccuredOnceOrMore.csv', encoding = 'euc-kr')
cancer = read.csv('data\\Cancer\\Cancer_All.csv')

# 서원진 샘이 class 나눠서 주신 데이터
mme_group2 <- read.csv('data\\mme_group2.csv', encoding = 'euc-kr')
mme_group3 <- read.csv('data\\mme_group3.csv', encoding = 'euc-kr')
mme_Mgroup2 <- read.csv('data\\MME_Mgroup2.csv', encoding = 'euc-kr')
mme_Mgroup3 <- read.csv('data\\MME_Mgroup3.csv', encoding = 'euc-kr')
mme_Fgroup2 <- read.csv('data\\MME_Fgroup2.csv', encoding = 'euc-kr')
mme_Fgroup3 <- read.csv('data\\MME_Fgroup3.csv', encoding = 'euc-kr')

# 내가 만든 데이터 
mme_mclass = read.csv('mme_mclass.csv', encoding = 'euc-kr')
mme_fclass = read.csv('mme_fclass.csv', encoding = 'euc-kr')
mme_group2_mine = read.csv('mme_class_mine.csv', encoding = 'euc-kr')

# merge - 내가 만든 데이터 
# ALL
df_ckd = merge(mme_group2_mine, ckd, by = 'NIHID')
df_mi = merge(mme_group2_mine, mi, by = 'NIHID')
df_cancer = merge(mme_group2_mine, cancer, by = 'NIHID')
# M
df_ckd = merge(mme_mclass, ckd, by = 'NIHID')
df_mi = merge(mme_mclass, mi, by = 'NIHID')
df_cancer = merge(mme_mclass, cancer, by = 'NIHID')
# F
df_ckd = merge(mme_fclass, ckd, by = 'NIHID')
df_mi = merge(mme_fclass, mi, by = 'NIHID')
df_cancer = merge(mme_fclass, cancer, by = 'NIHID')

# merge - 서원진 샘 데이터
# GROUP2
df_ckd = merge(mme_group2, ckd, by = 'NIHID')
df_mi = merge(mme_group2, mi, by = 'NIHID')
df_cancer = merge(mme_group2, cancer, by = 'NIHID')
# GROUP2 - M
df_ckd = merge(mme_Mgroup2, ckd, by = 'NIHID')
df_mi = merge(mme_Mgroup2, mi, by = 'NIHID')
df_cancer = merge(mme_Mgroup2, cancer, by = 'NIHID')
# GROUP2 - F
df_ckd = merge(mme_Fgroup2, ckd, by = 'NIHID')
df_mi = merge(mme_Fgroup2, mi, by = 'NIHID')
df_cancer = merge(mme_Fgroup2, cancer, by = 'NIHID')
# GROUP3
df_ckd = merge(mme_group3, ckd, by = 'NIHID')
df_mi = merge(mme_group3, mi, by = 'NIHID')
df_cancer = merge(mme_group3, cancer, by = 'NIHID')
# GROUP3 - M
df_ckd = merge(mme_Mgroup3, ckd, by = 'NIHID')
df_mi = merge(mme_Mgroup3, mi, by = 'NIHID')
df_cancer = merge(mme_Mgroup3, cancer, by = 'NIHID')
# GROUP3 - F
df_ckd = merge(mme_Fgroup3, ckd, by = 'NIHID')
df_mi = merge(mme_Fgroup3, mi, by = 'NIHID')
df_cancer = merge(mme_Fgroup3, cancer, by = 'NIHID')


# copy disease to use

df = copy(df_ckd)
target = 'final_CKD'

df = copy(df_mi)
target = 'final_MI'

df = copy(df_cancer)
target = 'Cancer'



#### 내가 만든 데이터로 fit

  # 기본변수 빼기
  df <- subset(df, select = -c(기수, EDATE, NIHID, S1, S2))
  # WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
  df <- subset(df, select = -c(WEIGHT, HEIGHT))
  # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select = -c(PHYACTL, PHYACTM, PHYACTH))
   # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select = -c(PA_NEW, DRK_NEW))
  # factor
  df$SMOKE = factor(df$SMOKE)
  # df$DRK_NEW = factor(df$DRK_NEW)
  # df$PA_NEW = factor(df$PA_NEW)
  df$state = factor(df$state)
  # df$state <- relevel(df$state, ref="2")   # reference 변경시 실행

  # variables 리스트 생성 - GLM
  variables <- colnames(df)
  variables <- variables[!variables %in% c(target, 'TIME', 'SEX', 'state')]

  # variables 리스트 생성 - COX
  variables <- colnames(df)
  variables <- variables[!variables %in% c(target, 'SEX', 'state')]

  # Backward elimination
  Y <- df[[target]]
  df2 <- df[variables]

  full <- glm(Y ~ ., family = binomial(link = "logit"), data = df2)
  step_glm <- step(full, direction = 'backward')

  summary(step_glm)

  back_variables <- c(all.vars(formula(step_glm)[[3]])) # backward로 선택된 변수들만 가져오기

  # create glm formula

    # all variables
    variables = c(variables, 'state')
    glm_f <- as.formula(
      paste(target,
            paste(variables, collapse = " + "),
            sep = " ~ ")
    )
    
    glm_f

    # backward eliminated variables

    # CLASS
    variables = c(back_variables, 'state')
    # SEX
    variables = c(back_variables, 'SEX')

    glm_f <- as.formula(
      paste(target,
            paste(variables, collapse = " + "),
            sep = " ~ ")
    )
    
    glm_f
    
    # glm fit
    fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
    summary(fit)


    # visualization

    par(mfrow=c(2, 2))

    AGE.mean <- t(tapply(df$AGE, df$state, mean))
    barplot(AGE.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "AGE")

    WAIST.mean <- t(tapply(df$WAIST, df$state, mean))
    barplot(WAIST.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "WAIST")

    R_GTP_TR.mean <- t(tapply(df$R_GTP_TR, df$state, mean))
    barplot(R_GTP_TR.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "R_GTP_TR")

    HDL_ORI.mean <- t(tapply(df$HDL_ORI, df$state, mean))
    barplot(HDL_ORI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "HDL_ORI")

    DRUGHT.mean <- t(tapply(df$DRUGHT, df$state, mean))
    barplot(DRUGHT.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "DRUGHT")

    FMDM.mean <- t(tapply(df$FMDM, df$state, mean))
    barplot(FMDM.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "FMDM")

    SBP.mean <- t(tapply(df$SBP, df$state, mean))
    barplot(SBP.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "SBP")

    DBP.mean <- t(tapply(df$DBP, df$state, mean))
    barplot(DBP.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "DBP")

    eGFR.mean <- t(tapply(df$eGFR, df$state, mean))
    barplot(eGFR.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "eGFR")

    BMI.mean <- t(tapply(df$BMI, df$state, mean))
    barplot(BMI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "BMI")


    back_variables
    # COX - CLASS + MI
    fit <- coxph(Surv(TIME, final_MI) ~ 
    WAIST + R_GTP_TR + AST_ORI + ALT_ORI + HDL_ORI + SMOKE + 
    KID + SBP + DBP + state, data = df) # nolint
    summary(fit)

    # COX - SEX + MI
    fit <- coxph(Surv(TIME, final_MI) ~ 
    WAIST + R_GTP_TR + AST_ORI + ALT_ORI + HDL_ORI + SMOKE + 
    KID + SBP + DBP + SEX, data = df) # nolint
    summary(fit)

    # COX - CLASS + CANCER
    fit <- coxph(Surv(TIME, Cancer) ~ 
    AGE + GLU0_ORI + HDL_ORI + TRIGLY_ORI + HB_ORI + DRUGHT +
    FMHTN + FMDM + SBP + eGFR + state, data = df) # nolint
    summary(fit)

    # COX - SEX + CANCER
    fit <- coxph(Surv(TIME, Cancer) ~ 
    AGE + GLU0_ORI + HDL_ORI + TRIGLY_ORI + HB_ORI + DRUGHT +
    FMHTN + FMDM + SBP + eGFR + SEX, data = df) # nolint
    summary(fit)



    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint     

################################


# copy disease to use

df = copy(df_ckd)
target = 'final_CKD'

df = copy(df_mi)
target = 'final_MI'

df = copy(df_cancer)
target = 'Cancer'

write.csv(df, file = '')

#### 서원진 샘 데이터로 fit


  # 기본변수 빼기
  df <- subset(df, select = -c(기수, EDATE, NIHID))
  # WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
  df <- subset(df, select = -c(WEIGHT, HEIGHT))
  # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select = -c(PHYACTL, PHYACTM, PHYACTH))
  # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select = -c(PA_NEW, DRK_NEW))
  # factor
  df$SMOKE = factor(df$SMOKE)
  # df$DRK_NEW = factor(df$DRK_NEW)
  # df$PA_NEW = factor(df$PA_NEW)
  df$CLASS = factor(df$CLASS)
  # df$CLASS <- relevel(df$state, ref="2")   # reference 변경시 실행 # group2

  # variables 리스트 생성 - GLM
  variables <- colnames(df)
  variables <- variables[!variables %in% c(target, 'TIME', 'SEX', 'state')]

  # variables 리스트 생성 - COX
  variables <- colnames(df)
  variables <- variables[!variables %in% c(target, 'SEX', 'state')]

  # Backward elimination
  Y <- df[[target]]
  df2 <- df[variables]

  full <- glm(Y ~ ., family = binomial(link = "logit"), data = df2)
  step_glm <- step(full, direction = 'backward')

  summary(step_glm)

  back_variables <- c(all.vars(formula(step_glm)[[3]])) # backward로 선택된 변수들만 가져오기

  # create glm formula

    # all variables
    variables = c(variables, 'CLASS')
    glm_f <- as.formula(
      paste(target, 
            paste(variables, collapse = " + "),
            sep = " ~ ")
    )
    
    glm_f

    # backward eliminated variables

    # CLASS
    variables = c(back_variables, 'CLASS')
    # SEX
    variables = c(back_variables, 'SEX')

    glm_f <- as.formula(
      paste(target,
            paste(variables, collapse = " + "),
            sep = " ~ ")
    )
    
    glm_f
    
    # fit
    fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
    summary(fit)

    # visualization

    par(mfrow=c(2, 2))

    AGE.mean <- t(tapply(df$AGE, df$CLASS, mean))
    barplot(AGE.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "AGE")

    GLU0_ORI.mean <- t(tapply(df$GLU0_ORI, df$CLASS, mean))
    barplot(GLU0_ORI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "GLU0_ORI")

    WAIST.mean <- t(tapply(df$WAIST, df$CLASS, mean))
    barplot(WAIST.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "WAIST")

    R_GTP_TR.mean <- t(tapply(df$R_GTP_TR, df$CLASS, mean))
    barplot(R_GTP_TR.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "R_GTP_TR")

    HDL_ORI.mean <- t(tapply(df$HDL_ORI, df$CLASS, mean))
    barplot(HDL_ORI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "HDL_ORI")

    DRUGHT.mean <- t(tapply(df$DRUGHT, df$CLASS, mean))
    barplot(DRUGHT.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "DRUGHT")

    FMDM.mean <- t(tapply(df$FMDM, df$CLASS, mean))
    barplot(FMDM.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "FMDM")

    FMHTN.mean <- t(tapply(df$FMHTN, df$CLASS, mean))
    barplot(FMHTN.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "FMHTN")

    SBP.mean <- t(tapply(df$SBP, df$CLASS, mean))
    barplot(SBP.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "SBP")

    DBP.mean <- t(tapply(df$DBP, df$CLASS, mean))
    barplot(DBP.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "DBP")

    eGFR.mean <- t(tapply(df$eGFR, df$CLASS, mean))
    barplot(eGFR.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "eGFR")

    BMI.mean <- t(tapply(df$BMI, df$CLASS, mean))
    barplot(BMI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "BMI")

    TRIGLY_ORI.mean <- t(tapply(df$TRIGLY_ORI, df$CLASS, mean))
    barplot(TRIGLY_ORI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "TRIGLY_ORI")

    HB_ORI.mean <- t(tapply(df$HB_ORI, df$CLASS, mean))
    barplot(HB_ORI.mean, col=c("#727272"), beside=TRUE, 
            xlab = "States",
            ylab = "HB_ORI")


    back_variables
    # COX - CLASS + MI
    fit <- coxph(Surv(TIME, final_MI) ~ 
    WAIST + R_GTP_TR + AST_ORI + ALT_ORI + HDL_ORI + KID + 
    SBP + DBP + CLASS, data = df) # nolint
    summary(fit)

    # COX - SEX + MI
    fit <- coxph(Surv(TIME, final_MI) ~ 
    WAIST + R_GTP_TR + AST_ORI + ALT_ORI + HDL_ORI + KID + 
    SBP + DBP + SEX, data = df) # nolint
    summary(fit)

    # COX - CLASS + Cancer
    fit <- coxph(Surv(TIME, Cancer) ~ 
    AGE + GLU0_ORI + HDL_ORI + TRIGLY_ORI + HB_ORI + 
    DRUGHT + FMHTN + FMDM + SBP + eGFR + CLASS, data = df) # nolint
    summary(fit)

    # COX - SEX + Cancer
    fit <- coxph(Surv(TIME, Cancer) ~ 
    AGE + GLU0_ORI + HDL_ORI + TRIGLY_ORI + HB_ORI + 
    DRUGHT + FMHTN + FMDM + SBP + eGFR + SEX, data = df) # nolint
    summary(fit)


    # visualization
    ggsurvplot(surv_fit(fit, data = df), data = df, risk.table = TRUE)

    # LML plot
    ggsurvplot(surv_fit(fit, data = df), data = df, fun = "cloglog") # nolint  

##########################

table(df$state, df$Cancer)

# crosstable
CrossTable(mme_group2$CLASS, mme_group2_mine$state)


# 
tapply(mme_group2_mine$AGE, mme_group2_mine$state, summary)
tapply(mme_group2_mine$BMI, mme_group2_mine$state, summary)
tapply(mme_group2_mine$SBP, mme_group2_mine$state, summary)
tapply(mme_group2_mine$DBP, mme_group2_mine$state, summary)

tapply(mme_group2_mine$eGFR, mme_group2_mine$state, summary)

tapply(mme_group2_mine$GLU0_ORI, mme_group2_mine$state, summary)
tapply(mme_group2_mine$AST_ORI, mme_group2_mine$state, summary)
tapply(mme_group2_mine$TRIGLY_ORI, mme_group2_mine$state, summary)
tapply(mme_group2_mine$TCHL_ORI, mme_group2_mine$state, summary)
tapply(mme_group2_mine$MET_CAL, mme_group2_mine$state, summary)


