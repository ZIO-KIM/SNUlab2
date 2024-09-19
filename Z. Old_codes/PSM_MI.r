
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

mi <- read.csv('data\\MI_psm.csv', encoding = 'euc-kr')

############

#### copy df to use

  # MI (심근경색)
  df <- copy(mi)
  target <- df$final_MI
  psm_col <- "final_MI"

#####################


#### Preprocess ####

#### MI
  # 기본변수 빼기
  df <- subset(df, select = -c(기수, EDATE, NIHID))

  # CKD도 빼기
  df <- subset(df, select = -c(final_CKD))

  # WEIGHT, HEIGHT 빼기 (BMI와 corr 높음)
  df <- subset(df, select = -c(WEIGHT, HEIGHT))

  # KID = 2 (신장 치료 받고 있는 사람들) 빼기
  df <- df[df$KID != 2, ]
  df <- subset(df, select = -c(KID))

  # 운동, 알콜 관련 categorical 변수들 뺄 때 실행
  df <- subset(df, select=-c(PHYACTL, PHYACTM, PHYACTH, DRK_NEW, PA_NEW))
  
  # factor
  df$SMOKE = factor(df$SMOKE)

  # GLM 할때는 뺄것
  df <- subset(df, select = -c(TIME))

#########################


#### Variables #### 전체실행 하면됨

  # variables 리스트 생성
  variables <- colnames(df)

  # 신장변수 전체 (약물변수 포함)
  variables <- variables[!variables %in% c(psm_col)]  # for PSM

  variables

#############


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

  # create glm formula

  glm_f <- as.formula(
    paste("final_MI", 
          paste(variables, collapse = " + "),
          sep = " ~ ")
  )
  
  glm_f
  
  # fit
  fit <- glm(glm_f, family = binomial(link = "logit"), data = df)
  summary(fit)

  # vif
  vif(fit)

####################


########### END ##############