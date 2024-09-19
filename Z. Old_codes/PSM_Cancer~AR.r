
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
#################

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

#### Stepwise variable selection ####

target <- df$Cancer
df2 <- df[variables]

full <- glm(target ~ ., family = binomial(link = "logit"), data = df2)
step_lm <- step(full, direction = 'both')

summary(step_lm)

step_variables2 <- c(all.vars(formula(step_lm)[[3]])) # stepwise로 선택된 변수들만 가져오기

#########################


#### PS Matching ####

  #### create psm formula - with all variables
  psm_f <- as.formula(
    paste(psm_col, 
          paste(variables, collapse = " + "),
          sep = " ~ ")
  )
  
  psm_f

  #### Matching

    # 1:1
    mod_match <- matchit(psm_f, method = "nearest", data = df)
    
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

    mod_match

    dta_m <- match.data(mod_match)

    # vars - with all variables
    vars <- c(variables)

#################


#### SMD ####

tabmatched <- CreateTableOne(vars = vars, strata = psm_col, data = dta_m, test = FALSE)
print(tabmatched, smd = TRUE)

###################


#### GLM ####

  variables = c(variables, psm_col)

  # create glm formula

  glm_f <- as.formula(
    paste("Cancer",
          paste(variables, collapse = " + "),
          sep = " ~ ")
  )
  
  glm_f
  
  # fit
  fit <- glm(glm_f, family = binomial(link = "logit"), data = dta_m)
  summary(fit)

  # vif
  vif(fit)

####################


########### END ##############