
#### Install ####
install.packages("twang")
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
library(twang)
library(survey)
#################

#### twang test with lalonde ####
  data(lalonde)

  # fit GBM with ps
  ps.lalonde.gbm = ps(treat ~ age + educ + black + hispan + nodegree +
    married + re74 + re75,
    data = lalonde,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.01,
    estimand = "ATT",
    stop.method = c("es.mean","ks.max"),
    n.minobsinnode = 10,
    n.keep = 1,
    n.grid = 25,
    ks.exact = NULL,
    verbose = FALSE)

  plot(ps.lalonde.gbm)

  # assessing balance using balance tables
  lalonde.balance <- bal.table(ps.lalonde.gbm)

  # graphical assessments
  plot(ps.lalonde.gbm, plots = 2)
  plot(ps.lalonde.gbm, plots = 3)
  plot(ps.lalonde.gbm, plots = 4)
  plot(ps.lalonde.gbm, plots = 5)

  # Understanding the relationship between the covariates and the treatment assignment
  summary(ps.lalonde.gbm$gbm.obj,
  n.trees = ps.lalonde.gbm$desc$ks.max.ATT$n.trees,
  plot = TRUE)

  # get weights
  lalonde$w <- get.weights(ps.lalonde.gbm, stop.method="es.mean")
  
  design.ps <- svydesign(ids=~1, weights=~w, data=lalonde)

  # draw plot to check weight distribution
  ggplot(lalonde, aes(w)) + geom_histogram(binwidth = .5)


########## twang test end ##########

#### read df

cancer <- read.csv('data\\Cancer_ar_psm.csv', encoding = 'euc-kr')

##################

#### copy df to use

  # Cancer
  df <- copy(cancer)
  target <- df$Cancer
  psm_col <- "ar_drug_90"

#####################

#### Preprocess ####

#### Cancer
  # 기본변수 빼기
  df <- subset(df, select = -c(기수, EDATE, NIHID))

  # GLM 할때는 뺄것
  df <- subset(df, select = -c(TIME))

#########################

#### Variables #### 전체실행 하면됨

  # variables 리스트 생성
  variables <- colnames(df)

  # 신장변수 전체 (약물변수 포함)
  variables <- variables[!variables %in% c('Cancer', psm_col)]  # for PSM

  variables

#############

# #### Stepwise variable selection #### 이걸 해야할까? 

# target <- df$Cancer
# df2 <- df[variables]

# full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
# step_lm <- step(full, direction = 'both')

# summary(step_lm)

# step_variables2 <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

# #########################

#### create psm formula - with all variables
  psm_f <- as.formula(
    paste(psm_col, 
          paste(variables, collapse = " + "),
          sep = " ~ ")
  )
  
  psm_f

# fit GBM with ps - twang
  ps.cancer.gbm = ps(psm_f,
    data = cancer,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.01,
    estimand = "ATT",
    stop.method = c("es.mean","ks.max"),
    n.minobsinnode = 10,
    n.keep = 1,
    n.grid = 25,
    ks.exact = NULL,
    verbose = FALSE)

  plot(ps.cancer.gbm)

# assessing balance using balance tables
cancer.balance <- bal.table(ps.cancer.gbm)

# graphical assessments
plot(ps.cancer.gbm, plots = 2)
plot(ps.cancer.gbm, plots = 3)
plot(ps.cancer.gbm, plots = 4)
plot(ps.cancer.gbm, plots = 5)

# Understanding the relationship between the covariates and the treatment assignment
summary(ps.cancer.gbm$gbm.obj,
n.trees = ps.cancer.gbm$desc$ks.max.ATT$n.trees,
plot = TRUE)

# get weights
cancer$w <- get.weights(ps.cancer.gbm, stop.method = "es.mean")
  
design.ps <- svydesign(ids = ~1, weights = ~w, data = cancer)

# draw plot to check weight distribution
ggplot(cancer, aes(w)) + geom_histogram(binwidth = .01)



# 1:1
mod_match <- matchit(psm_f, method = "nearest", distance = 'gbm', data = df)
mod_match

dta_m <- match.data(mod_match)

vars <- c(variables)
#### SMD ####
tabmatched <- CreateTableOne(vars = vars, strata = psm_col, data = dta_m, test = FALSE)
print(tabmatched, smd = TRUE)
# summary(tabmatched)

# calculate CKD patients in each group
# 약물 복용 x
count(dta_m[dta_m[psm_col] == 0 & dta_m$Cancer == 0, ])
count(dta_m[dta_m[psm_col] == 0 & dta_m$Cancer == 1, ])
# 약물 복용 o
count(dta_m[dta_m[psm_col] == 1 & dta_m$Cancer == 0, ])
count(dta_m[dta_m[psm_col] == 1 & dta_m$Cancer == 1, ])

# Attaching weight to the dataset
dta_m$weight.ATE <- ifelse(dta_m[[psm_col]] == 1, 1 / dta_m$distance, 1 / (1 - dta_m$distance))

# dataset all
dta_m$weight.ATE <- ifelse(dta_m_all[[psm_col]] == 1, 1 / dta_m_all$distance, 1 / (1 - dta_m_all$distance))

############################

ggplot(dta_m, aes(weight.ATE)) + geom_histogram(binwidth = 1)


####
variables <- c(variables, psm_col)
glm_f <- as.formula(
  paste("Cancer",
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
