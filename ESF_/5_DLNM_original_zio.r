
# install.packages('dlnm')
# install.packages('splines')
# install.packages("httpgd")
# install.packages("knitr")
# install.packages("rmarkdown")
# install.packages("pscl")
# install.packages('DescTools')
# install.packages("SciViews")
# install.packages("poolr")
# install.packages("ggpubr")

# cran mirror 설정
options(repos = c(CRAN = "http://cran.rstudio.com"))


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
library(dlnm)
library(splines)
library(httpgd)
library(lubridate)
library(ape)
library(knitr)
library(rmarkdown)
library(DescTools)
library(pscl)
library(SciViews)
library(poolr)
library(ggpubr)
#################


## 0. read outcome df ##
setwd('D:\\SNUlab\\ESF_\\')
# asthma = read.csv('outcome\\asthma_sudogwon_agg.csv')
# rhinitis = read.csv('outcome\\rhinitis_sudogwon_agg.csv')
# atopic = read.csv('outcome\\atopic_sudogwon_agg.csv')

# change to ses added df
  asthma = read.csv('outcome\\asthma_sudogwon_agg_2015SESadded.csv', fileEncoding = 'euc-kr')
  rhinitis = read.csv('outcome\\rhinitis_sudogwon_agg_2015SESadded.csv', fileEncoding = 'euc-kr')
  atopic = read.csv('outcome\\atopic_sudogwon_agg_2015SESadded.csv', fileEncoding = 'euc-kr')

  # non agg 데이터 불러오기
  asthma_non_agg = read.csv('outcome\\asthma_sudogwon_with_air_weather.csv', fileEncoding = 'euc-kr')
  rhinitis_non_agg = read.csv('outcome\\rhinitis_sudogwon_with_air_weather.csv', fileEncoding = 'euc-kr')
  atopic_non_agg = read.csv('outcome\\atopic_sudogwon_with_air_weather.csv', fileEncoding = 'euc-kr')

  # aggregated model을 위한 data (365.25*3 = 1096행)
  df = read.csv('outcome\\data_for_aggregated_model.csv', fileEncoding = 'euc-kr')
###############################################
dim(asthma)
dim(df)
head(df)

##### 0. If change needed, change in here #####
lag = 7
nk_to_use = 2

# define path to save plots
  # filter = "filterSFW"
  outcome = "ATOPIC_out_total"
  # lagNum = "lag7"
  # path = sprintf("D:\\SNUlab\\1. Results\\ESF_ (2022.08~)\\%s+%s+%s", outcome, lagNum, filter)
  path = sprintf("D:\\SNUlab\\1. Results\\ESF_ (2022.08~)\\%s", outcome)
  path
###############################################


##### 1. Select df #####
  df <- copy(asthma)

  df <- copy(rhinitis)

  df <- copy(atopic)
###################

##### 1. Select df for descriptive #####
  df <- copy(asthma_non_agg)

  df <- copy(rhinitis_non_agg)

  df <- copy(atopic_non_agg)
###################



# # atopic in+em 생성 #
# df$ATOPIC_out_total <- df$ATOPIC_out_total + df$ATOPIC_out_total

# ATOPIC out // 300 생성 # 

# df$ATOPIC_out_total <- round(df$ATOPIC_out_total/3)
# df$tmp<- round(df$ATOPIC_out_total/500)

# table(df$ATOPIC_out_total)
# prop.table(table(df$ATOPIC_out_total))                                                         
# prop.table(table(df$tmp))
# summary(df$tmp)
# hist(df$tmp)
# prop.table(table(df$ATOPIC_out_total))
# prop.table(table(df$ATOPIC_out_total))

# ATOPIC in+out+em 생성
df$ATOPIC_in_em_out_total <- df$ATOPIC_in_total + df$ATOPIC_em_total + df$ATOPIC_out_total

# ATOPIC in+em 생성
df$ATOPIC_out_total <- df$ATOPIC_out_total + df$ATOPIC_em_total

# ASTHMA in+em 생성
df$ATOPIC_out_total <- df$ASTHMA_in_total + df$ATOPIC_out_total

# RHINITIS in+em 생성
df$ATOPIC_out_total <- df$RHINITIS_in_total + df$RHINITIS_em_total




### EDA ###

  colnames(df)
  head(df)
  length(unique(df$air_out_idx))

  table(df$ATOPIC_out_total,df$air_out_idx)
  table(df$ATOPIC_out_total)
  test_date=df[which(df$dt=="2013-11-01"),]
  test_date$ATOPIC_out_total

### EDA end. ###


##### 1. Select only 2015 - 2017 (for PM2.5) ##### - 13-17 사용시 실행 x
dim(df) # 135864 69
df <- df[substr(df$dt, 1, 4) %in% c(2015, 2016, 2017),]
dim(df) # 78912 69
# colSums(is.na(df))
##################################################



##### 1. Select outcome variable #####
  # outcome 변수 바꿔가면서 실행 - 바뀜 방지용 한글표기
  # "ASTHMA_아웃_total"
  # "ASTHMA_인_total"
  # "ASTHMA_이엠_total"

  # "RHINITIS_아웃_total"
  # "RHINITIS_인_total"
  # "RHINITIS_이엠_total"

  # "ATOPIC_아웃_total"
  # "ATOPIC_인_total"
  # "ATOPIC_이엠_total"
#####################


##### 2. create crossbasis #####
  # NO2
  kno <- equalknots(df$NO2_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_no <- crossbasis(df$NO2_mean,argvar=list(knots=kno),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_no # nolint # nolint
  df$ns.basis_no <- crossbasis(df$NO2_mean,argvar=list(knots=kno),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #df$ns.basis_no

  # SO2
  kso <- equalknots(df$SO2_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_so <- crossbasis(df$SO2_mean,argvar=list(knots=kso),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_so
  df$ns.basis_so <- crossbasis(df$SO2_mean,argvar=list(knots=kso),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #df$ns.basis_so

  # CO
  kco <- equalknots(df$CO_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_co <- crossbasis(df$CO_mean,argvar=list(knots=kco),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_co
  df$ns.basis_co <- crossbasis(df$CO_mean,argvar=list(knots=kco),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #df$ns.basis_co

  # O3
  ko3 <- equalknots(df$O3_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_o3 <- crossbasis(df$O3_mean,argvar=list(knots=ko3),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_o3
  df$ns.basis_o3 <- crossbasis(df$O3_mean,argvar=list(knots=ko3),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #df$ns.basis_o3     

  # PM10
  kpm <- equalknots(df$PM10_mean[which(df$PM10_mean<153.9809)], nk = nk_to_use)

  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_pm <- crossbasis(df$PM10_mean,argvar=list(knots=kpm),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_pm
  df$ns.basis_pm <- crossbasis(df$PM10_mean,argvar=list(knots=kpm),group=df$air_out_idx, arglag=list(knots=klag), lag=lag); #df$ns.basis_pm

  # temp_tc
  ktemp_tc <- equalknots(df$temp_tc_total, nk = nk_to_use)
  klag <- logknots(1, nk = 1)
  ns.basis_temp_tc <- crossbasis(df$temp_tc_total,argvar=list(knots=ktemp_tc),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  df$ns.basis_temp_tc <- crossbasis(df$temp_tc_total,argvar=list(knots=ktemp_tc),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  ktemp_tc2 <- equalknots(df$temp_mean_total, nk = nk_to_use)
  ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  df$ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # tmp_mean_total
  # ktemp_tc2 <- equalknots(df$temp_mean_total[which(df$temp_mean_total>-20)],nk = nk_to_use)
  
  # ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  # df$ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # temp_min # 01.24 추가
  # ktemp_min <- equalknots(df$temp_min[which(df$temp_min>-30)],nk = nk_to_use)
  
  # ns.basis_temp_min <- crossbasis(df$temp_min,argvar=list(knots=ktemp_min),group=df$air_out_idx, arglag=list(knots=klag),lag=0); 
  # df$ns.basis_temp_min <- crossbasis(df$temp_min,argvar=list(knots=ktemp_min),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # temp_max # 01.24 추가
  # ktemp_max <- equalknots(df$temp_max[which(df$temp_max<50)],nk = nk_to_use)
  
  # ns.basis_temp_max <- crossbasis(df$temp_max,argvar=list(knots=ktemp_max),group=df$air_out_idx, arglag=list(knots=klag),lag=0); 
  # df$ns.basis_temp_max <- crossbasis(df$temp_max,argvar=list(knots=ktemp_max),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # humi
  khumi <- equalknots(df$humi_mean, nk = nk_to_use)
  klag <- logknots(1, nk = 1)
  ns.basis_humi <- crossbasis(df$humi_mean,argvar=list(knots=khumi),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_humi
  df$ns.basis_humi <- crossbasis(df$humi_mean,argvar=list(knots=khumi),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_humi

##### create crossbasis end. #####
##################################

##### 2. create crossbasis for aggregated model #####
  # NO2
  kno <- equalknots(df$no2_mean_by_idx, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_no_agg <- crossbasis(df$no2_mean_by_idx,argvar=list(knots=kno), arglag=list(knots=klag), lag=lag); #ns.basis_no # nolint # nolint
  df$ns.basis_no_agg <- crossbasis(df$no2_mean_by_idx,argvar=list(knots=kno), arglag=list(knots=klag), lag=lag); #df$ns.basis_no

  # SO2
  kso <- equalknots(df$so2_mean_by_idx, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_so_agg <- crossbasis(df$so2_mean_by_idx,argvar=list(knots=kso), arglag=list(knots=klag), lag=lag); #ns.basis_so
  df$ns.basis_so_agg <- crossbasis(df$so2_mean_by_idx,argvar=list(knots=kso), arglag=list(knots=klag), lag=lag); #df$ns.basis_so

  # CO
  kco <- equalknots(df$co_mean_by_idx, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_co_agg <- crossbasis(df$co_mean_by_idx,argvar=list(knots=kco), arglag=list(knots=klag), lag=lag); #ns.basis_co
  df$ns.basis_co_agg <- crossbasis(df$co_mean_by_idx,argvar=list(knots=kco), arglag=list(knots=klag), lag=lag); #df$ns.basis_co

  # PM10
  kpm <- equalknots(df$pm10_mean_by_idx[which(df$pm10_mean_by_idx<153.9809)], nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_pm_agg <- crossbasis(df$pm10_mean_by_idx,argvar=list(knots=kpm), arglag=list(knots=klag), lag=lag); #ns.basis_pm
  df$ns.basis_pm_agg <- crossbasis(df$pm10_mean_by_idx,argvar=list(knots=kpm), arglag=list(knots=klag), lag=lag); #df$ns.basis_pm

  # # temp_tc
  # ktemp_tc <- equalknots(df$temp_tc_total, nk = nk_to_use)
  # klag <- logknots(1, nk = 1)
  # ns.basis_temp_tc <- crossbasis(df$temp_tc_total,argvar=list(knots=ktemp_tc),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  # df$ns.basis_temp_tc <- crossbasis(df$temp_tc_total,argvar=list(knots=ktemp_tc),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # ktemp_tc2 <- equalknots(df$temp_mean_total, nk = nk_to_use)
  # ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  # df$ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # tmp_mean_total
  # ktemp_tc2 <- equalknots(df$temp_mean_total[which(df$temp_mean_total>-20)],nk = nk_to_use)
  
  # ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  # df$ns.basis_temp_mean <- crossbasis(df$temp_mean_total,argvar=list(knots=ktemp_tc2),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # temp_min # 01.24 추가
  # ktemp_min <- equalknots(df$temp_min[which(df$temp_min>-30)],nk = nk_to_use)
  
  # ns.basis_temp_min <- crossbasis(df$temp_min,argvar=list(knots=ktemp_min),group=df$air_out_idx, arglag=list(knots=klag),lag=0); 
  # df$ns.basis_temp_min <- crossbasis(df$temp_min,argvar=list(knots=ktemp_min),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # # temp_max # 01.24 추가
  # ktemp_max <- equalknots(df$temp_max[which(df$temp_max<50)],nk = nk_to_use)
  
  # ns.basis_temp_max <- crossbasis(df$temp_max,argvar=list(knots=ktemp_max),group=df$air_out_idx, arglag=list(knots=klag),lag=0); 
  # df$ns.basis_temp_max <- crossbasis(df$temp_max,argvar=list(knots=ktemp_max),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_temp_tc

  # humi
  khumi <- equalknots(df$humi_mean, nk = nk_to_use)
  klag <- logknots(1, nk = 1)
  ns.basis_humi <- crossbasis(df$humi_mean,argvar=list(knots=khumi),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_humi
  df$ns.basis_humi <- crossbasis(df$humi_mean,argvar=list(knots=khumi),group=df$air_out_idx, arglag=list(knots=klag),lag=0); #df$ns.basis_humi

##### create crossbasis end. #####
##################################


##### 3. create day #####

  df$day=wday(
    df$dt,
    label = FALSE,
    abbr = TRUE,
    week_start = getOption("lubridate.week.start", 7),
    locale = Sys.getlocale("LC_TIME")
  )

  df$day = factor(df$day)

  colnames(df)

  df <- df[,c("day","dt","air_out_idx",
                      "NO2_mean","ns.basis_no",
                      "SO2_mean","ns.basis_so",
                      "CO_mean","ns.basis_co",
                      "O3_mean","ns.basis_o3",
                      "PM10_mean","ns.basis_pm",
                      "temp_mean_total","ns.basis_temp_mean",
                      # "temp_min", "ns.basis_temp_min", # 01.24 temp_min 추가
                      # "temp_max", "ns.basis_temp_max", # 01.24 temp_max 추가
                      "humi_mean_total","ns.basis_humi",
                      "전체인구", "요양기관.수", "재정자립도.세입과목개편후.", "도시인구.용도지역기준.", "비도시인구.용도지역기준.", "도시인구.행정구역기준.", "농촌인구.행정구역기준.",
                      "도시지역인구비율.용도지역기준.", "도시지역인구비율.행정구역기준.", 
                      "ATOPIC_out_total")]
  summary(df)
  colSums(is.na(df))
  dim(df)
  df=df[complete.cases(df),] 
  dim(df)
  df$doy=yday(as.Date(df$dt,origin="1970-01-01"))
  df$year=substr(df$dt,1,4)

  # table(df$year, df$ATOPIC_out_total)
  dim(df)
  length(unique(df$air_out_idx))
  head(df)
  unique(df$air_out_idx)
##### create day end. #####
###########################


##### DESCRIPTIVE #####

  df$age_group<-ifelse(df$AGE %in% c(0,5),1,ifelse(df$AGE%in%c(10,15),2,ifelse(df$AGE>=65,3,4)))
  sprintf("TOTAL: %d (100%%)", sum(df$ATOPIC_out_total))
  sprintf("Male: %d (%.0f%%)", sum(df[df$SEX_TYPE == 1, 'ATOPIC_out_total']), sum(df[df$SEX_TYPE == 1, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
  sprintf("Female: %d (%.0f%%)", sum(df[df$SEX_TYPE == 2, 'ATOPIC_out_total']), sum(df[df$SEX_TYPE == 2, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
  sprintf("Age 0-9: %d (%.0f%%)", sum(df[df$age_group == 1, 'ATOPIC_out_total']), sum(df[df$age_group == 1, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
  sprintf("Age 10-19: %d (%.0f%%)", sum(df[df$age_group == 2, 'ATOPIC_out_total']), sum(df[df$age_group == 2, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
  sprintf("Age 20-64: %d (%.0f%%)", sum(df[df$age_group == 4, 'ATOPIC_out_total']), sum(df[df$age_group == 4, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
  sprintf("Age >= 65: %d (%.0f%%)", sum(df[df$age_group == 3, 'ATOPIC_out_total']), sum(df[df$age_group == 3, 'ATOPIC_out_total']) / sum(df$ATOPIC_out_total) * 100)
# air, weather는 다시 agg된 데이터로
    summary(df)
    sd(df$humi_mean)

    days <-seq(as.Date("2015-01-01"), as.Date('2017-12-31'), by="days")
    air_out_idx <- unique(df$air_out_idx)
    length(days)
    length(air_out_idx)
    head(days)
    idx <- merge(days, air_out_idx)
    head(idx)
    dim(idx)
    length(unique(df$dt))
    length(unique(df$air_out_idx))
    head(df)

    colSums(is.na(df))

    # daily average 구하기
    sum(df$ATOPIC_out_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))
    sum(df$ASTHMA_out_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))
    sum(df$ASTHMA_in_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))

    sum(df$RHINITIS_em_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))
    sum(df$ATOPIC_out_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))
    sum(df$ATOPIC_out_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))

    sum(df$ATOPIC_out_total) / (length(unique(df$dt))*length(unique(df$air_out_idx)))

    head(df)


# combine p-values of 4 pollutants by each model (w/o ESF, fixed effect, ESF)
  # for paper

  # asthma-em
    # 1. spatial model without ESF
    p <- c(6.175052e-07, 2.190039e-06, 4.216862e-07, 1.092698e-06)
    # 2. fixed effect model
    p <- c(2.027922e-08, 6.674379e-10, 8.429277e-10, 9.682194e-09)
    # 3. ESF model
    p <- c(0.7714571, 0.3498058, 0.6722087, 0.462323)
    fisher(p)
  # asthma-in
    # 1. spatial model without ESF
    p <- c(1.182165e-05, 6.341747e-05, 6.061118e-06, 2.156487e-05)
    # 2. fixed effect model
    p <- c(0.001039807, 0.0009718495, 0.001424104, 0.0008658081)
    # 3. ESF model
    p <- c(0.07530717, 0.1256748, 0.1342643, 0.1395208)
    1/sum(1/p)
    fisher(p)
  # asthma-out
    # 1. spatial model without ESF
    p <- c()
    # 2. fixed effect model
    p <- c()
    # 3. ESF model
    p <- c()
    fisher(p)
  # rhinitis-em
    # 1. spatial model without ESF
    p <- c()
    # 2. fixed effect model
    p <- c()
    # 3. ESF model
    p <- c()
    fisher(p)
  # rhinitis-in
    # 1. spatial model without ESF
    p <- c()
    # 2. fixed effect model
    p <- c()
    # 3. ESF model
    p <- c()
    fisher(p)
  # rhinitis-out
    # 1. spatial model without ESF
    p <- c()
    # 2. fixed effect model
    p <- c()
    # 3. ESF model
    p <- c()
    fisher(p)
  # atopic-em+in+out
    # 1. spatial model without ESF
    p <- c()
    # 2. fixed effect model
    p <- c()
    # 3. ESF model
    p <- c()
    fisher(p)

##### descriptive end. #####
############################



path4 = paste0(path, '\\1. general model (ESF X)')
# path4 = paste0(path, '\\1. general model (ESF X) (13-17)')
unlink(path4)
dir.create(path4)
##### 4. general model (ESF X) #####
    # set path to folder where plots to be saved
    setwd(path4)

    # 4-1. ns.basis_no
      # fit
      ns_lag_no <- glm(ATOPIC_out_total ~
                    ns.basis_no
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    #+ ns.basis_temp_min
                    #+ ns.basis_temp_max
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    + factor(day) 
                    # + factor(air_out_idx)
                    + ns(doy,4)
                    # + esf_em
                    ,	family = quasipoisson(), df); 

      # test <- glm(ATOPIC_out_total ~ 1, data=df, family=quasipoisson())
      # with(summary(test), null.deviance)
      # summary(test)

      logLik(ns_lag_no)
      logLik(ns_lag_no.init)
      summary(ns_lag_no)
      library(car)
      vif(ns_lag_no)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 31)/null.deviance), digits = 3)

      # with(summary(ns_lag_no), deviance)
      # with(summary(ns_lag_no), null.deviance)

      # anova(ns_lag_no)
      # 1-(54961943/100686999)

      # crosspred
      percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
      ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

      summary(ns.pred_no)

      low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
      mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
      high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      no2_general_RR <- RR
      no2_general_adjMFr2 <- adjMFr2

      # draw plot and save
      png(filename="NO2_mean.png")
      general_no2 <- plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
      dev.off()
      png(filename="NO2_mean_overall.png")
      general_no2_overall <- plot(ns.pred_no,"overall",xlab="NO2_mean")
      + legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      + legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

    ##### ns.basis_no end. #####

    # 4-2. ns.basis_so
      # fit
      ns_lag_so <- glm(ATOPIC_out_total ~ 
                    ns.basis_so
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_so)
      logLik(ns_lag_so)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred
      percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
      ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

      low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
      mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
      high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      so2_general_RR <- RR
      so2_general_adjMFr2 <- adjMFr2

      # draw plot and save
      png(filename="SO2_mean.png")
      plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
      dev.off()
      png(filename="SO2_mean_overall.png")
      plot(ns.pred_so,"overall",xlab="SO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

    ##### ns.basis_so end. #####

    # 4-3. ns.basis_co
      # fit
      ns_lag_co <- glm(ATOPIC_out_total ~ 
                    ns.basis_co
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) +ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_co)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred
      percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
      ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

      low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
      mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
      high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      co_general_RR <- RR
      co_general_adjMFr2 <- adjMFr2

      # draw plot and save
      png(filename="CO_mean.png")
      plot(ns.pred_co,zlab="RR",xlab="CO_mean")
      dev.off()
      png(filename="CO_mean_overall.png")
      plot(ns.pred_co,"overall",xlab="CO_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

    ##### ns.basis_co end. #####

    # 4-4. ns.basis_o3
      # fit
      ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                    ns.basis_o3
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) +ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_o3)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred
      percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
      ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

      low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
      mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
      high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save
      png(filename="O3_mean.png")
      plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
      dev.off()
      png(filename="O3_mean_overall.png")
      plot(ns.pred_o3,"overall",xlab="O3_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

    ##### ns.basis_o3 end. #####

    # 4-5. ns.basis_pm
      # fit
      ns_lag_pm <- glm(ATOPIC_out_total ~ 
                    ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_pm)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred
      percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
      ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

      low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
      mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
      high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save
      png(filename="PM10_mean.png")
      plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
      dev.off()
      png(filename="PM10_mean_overall.png")
      plot(ns.pred_pm,"overall",xlab="PM10_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_pm end. #####
##### general model (ESF X) end. #####
######################################






path9 = paste0(path, '\\9. general model - multiple exposure')
unlink(path9)
dir.create(path9)
##### 9. general model - multiple exposure ####
  setwd(path9)
  # 9-1. ns.basis_no + ns.basis_co
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_co
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - NO2
      percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_no
      ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - NO2
      png(filename="no2co_mean_NO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
      dev.off()
      png(filename="no2co_mean_NO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - CO
      percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_co
      ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - CO
      png(filename="no2co_mean_CO.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
      dev.off()
      png(filename="no2co_mean_CO_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="CO_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_no + ns.basis_co end. #####

    # 9-2. ns.basis_no + ns.basis_so
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_so
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - NO2
      percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_no
      ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - NO2
      png(filename="no2so2_mean_NO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
      dev.off()
      png(filename="no2so2_mean_NO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - SO2
      percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_so
      ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - SO2
      png(filename="no2so2_mean_SO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
      dev.off()
      png(filename="no2so2_mean_SO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_no + ns.basis_so end. #####

    # 9-3. ns.basis_no + ns.basis_pm
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - NO2
      percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_no
      ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - NO2
      png(filename="no2pm10_mean_NO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
      dev.off()
      png(filename="no2pm10_mean_NO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - PM10
      percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
      ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - PM10
      png(filename="no2pm10_mean_PM10.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
      dev.off()
      png(filename="no2pm10_mean_PM10_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_no + ns.basis_pm end. #####

    # 9-4. ns.basis_so + ns.basis_co
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_so
                    + ns.basis_co
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - SO2
      percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_so
      ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - SO2
      png(filename="so2co_mean_SO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
      dev.off()
      png(filename="so2co_mean_SO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - CO
      percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_co
      ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - CO
      png(filename="so2co_mean_CO.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
      dev.off()
      png(filename="so2co_mean_CO_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="CO_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_so + ns.basis_co end. #####

    # 9-5. ns.basis_so + ns.basis_pm
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_so
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - SO2
      percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_so
      ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - SO2
      png(filename="so2pm10_mean_SO2.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
      dev.off()
      png(filename="so2pm10_mean_SO2_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - PM10
      percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
      ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - PM10
      png(filename="so2pm10_mean_PM10.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
      dev.off()
      png(filename="so2pm10_mean_PM10_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_so + ns.basis_pm end. #####

    # 9-6. ns.basis_co + ns.basis_pm
      # fit
      ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_co
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 
      summary(ns_lag_multiexp)
      # anova(ns_lag_pm)

      #calculate McFadden's R-squared for model
      adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 31)/null.deviance), digits = 3)
      adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

      # crosspred - CO
      percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_co
      ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - CO
      png(filename="copm10_mean_CO.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
      dev.off()
      png(filename="copm10_mean_CO_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="CO_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()

      # crosspred - PM10
      percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
      ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

      low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2) 
      high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

      RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

      # draw plot and save - PM10
      png(filename="copm10_mean_PM10.png")
      plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
      dev.off()
      png(filename="copm10_mean_PM10_overall.png")
      plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
      legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
      legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
      dev.off()
      
    ##### ns.basis_co + ns.basis_pm end. #####

##### general model - multiple exposure end. #####
##################################################




path5 = paste0(path, '\\2. ESF model')
unlink(path5)
dir.create(path5)
##### 5. create df_esf data & fit model with ESF #####

  setwd(path5)
  # 5-1. ns.basis_no
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_no
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 32)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "NO2_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_no <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred2_no$allRRlow[length(ns.pred2_no$allRRfit)], digits = 2)
    mid = round(ns.pred2_no$allRRfit[length(ns.pred2_no$allRRfit)], digits = 2)
    high = round(ns.pred2_no$allRRhigh[length(ns.pred2_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    no2_esf_RR <- RR
    no2_esf_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="NO2_mean_esf.png")
    plot(ns.pred2_no,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="NO2_mean_esf_overall.png")
    plot(ns.pred2_no,"overall",xlab="NO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 5-2. ns.basis_so
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_so
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    # anova(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "SO2_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_so <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred2_so$allRRlow[length(ns.pred2_so$allRRfit)], digits = 2)
    mid = round(ns.pred2_so$allRRfit[length(ns.pred2_so$allRRfit)], digits = 2)
    high = round(ns.pred2_so$allRRhigh[length(ns.pred2_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    so2_esf_RR <- RR
    so2_esf_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="SO2_mean_esf.png")
    plot(ns.pred2_so,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="SO2_mean_esf_overall.png")
    plot(ns.pred2_so,"overall",xlab="SO2_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_so end. #####

  # 5-3. ns.basis_co
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_co
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "CO_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_co <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred2_co$allRRlow[length(ns.pred2_co$allRRfit)], digits = 2)
    mid = round(ns.pred2_co$allRRfit[length(ns.pred2_co$allRRfit)], digits = 2)
    high = round(ns.pred2_co$allRRhigh[length(ns.pred2_co$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    co_esf_RR <- RR
    co_esf_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="CO_mean_esf.png")
    plot(ns.pred2_co,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="CO_mean_esf_overall.png")
    plot(ns.pred2_co,"overall",xlab="CO_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_co end. #####

  # 5-4. ns.basis_o3
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_o3
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "O3_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_o3 <- crosspred(ns.basis_o3,ns_lag3_esf,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1]);
    
    low = round(ns.pred2_o3$allRRlow[length(ns.pred2_o3$allRRfit)], digits = 2)
    mid = round(ns.pred2_o3$allRRfit[length(ns.pred2_o3$allRRfit)], digits = 2)
    high = round(ns.pred2_o3$allRRhigh[length(ns.pred2_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean_esf.png")
    plot(ns.pred2_o3,zlab="RR",xlab="O3_mean_esf")
    dev.off()
    png(filename="O3_mean_esf_overall.png")
    plot(ns.pred2_o3,"overall",xlab="O3_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_o3 end. #####

  # 5-5. ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_pm
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "PM10_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_pm <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);
    
    low = round(ns.pred2_pm$allRRlow[length(ns.pred2_pm$allRRfit)], digits = 2)
    mid = round(ns.pred2_pm$allRRfit[length(ns.pred2_pm$allRRfit)], digits = 2)
    high = round(ns.pred2_pm$allRRhigh[length(ns.pred2_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean_esf.png")
    plot(ns.pred2_pm,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="PM10_mean_esf_overall.png")
    plot(ns.pred2_pm,"overall",xlab="PM10_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_pm end. #####

##### create df_esf data & fit model with ESF end. #####
#############################################################



path10 = paste0(path, '\\10. ESF model - multiple exposure')
unlink(path10)
dir.create(path10)
##### 10. ESF model - multiple exposure #####

  setwd(path10)

  # 10-1. ns.basis_no & ns.basis_co
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_co
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_no
                  + ns.basis_co
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "NO2CO_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - NO2
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2co_NO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="no2co_NO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - CO
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="no2co_CO_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="no2co_CO_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_co - multiple exposure end. #####

  # 10-2. ns.basis_no & ns.basis_so
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_so
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_no
                  + ns.basis_so
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "NO2SO2_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - NO2
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2so2_NO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="no2so2_NO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - SO2
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="no2so2_SO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="no2so2_SO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_so - multiple exposure end. #####

  # 10-3. ns.basis_no & ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_no
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_no
                  + ns.basis_pm
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "NO2PM10_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - NO2
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2pm10_NO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="no2pm10_NO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 0.0001),cen = percentiles_pm[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="no2pm10_PM10_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="no2pm10_PM10_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_pm - multiple exposure end. #####

  # 10-4. ns.basis_so & ns.basis_co
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_so
                    + ns.basis_co
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_so
                  + ns.basis_co
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "SO2CO_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - SO2
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="so2co_SO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="so2co_SO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - CO
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="so2co_CO_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="so2co_CO_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so & ns.basis_co - multiple exposure end. #####

  # 10-5. ns.basis_so & ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_so
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_so
                  + ns.basis_pm
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "SO2PM10_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - SO2
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="so2pm10_SO2_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="so2pm10_SO2_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="so2pm10_PM10_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="so2pm10_PM10_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so & ns.basis_pm - multiple exposure end. #####

  # 10-6. ns.basis_co & ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)

    ns_lag_multiexp <- glm(ATOPIC_out_total ~ 
                    ns.basis_co
                    + ns.basis_pm
                    # + ns.basis_temp_tc
                    # + ns.basis_temp_mean
                    # + ns.basis_humi 
                    
                    # + ns.basis_temp_min
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(air_out_idx)
                    + factor(day) + ns(doy,4)
                    # + esf_em
                    ,	family=quasipoisson(), df); 

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_co
                  + ns.basis_pm
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 44)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "COPM10_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred - CO
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="copm10_CO_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="copm10_CO_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);
    
    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="copm10_PM10_mean_esf.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="copm10_PM10_mean_esf_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean_esf")

    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co & ns.basis_pm - multiple exposure end. #####

##### ESF model - multiple exposure end. #####
##############################################



path6 = paste0(path, '\\3. fixed effect model')
# path6 = paste0(path, '\\3. fixed effect model (ESF X) (13-17)')
unlink(path6)
dir.create(path6)
##### 6. model with fixed effect (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path6)

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no3 <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no3$allRRlow[length(ns.pred_no3$allRRfit)], digits = 2)
    mid = round(ns.pred_no3$allRRfit[length(ns.pred_no3$allRRfit)], digits = 2)
    high = round(ns.pred_no3$allRRhigh[length(ns.pred_no3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    no2_fixed_RR <- RR
    no2_fixed_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no3,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no3,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "SO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so3 <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so3$allRRlow[length(ns.pred_so3$allRRfit)], digits = 2)
    mid = round(ns.pred_so3$allRRfit[length(ns.pred_so3$allRRfit)], digits = 2)
    high = round(ns.pred_so3$allRRhigh[length(ns.pred_so3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    so2_fixed_RR <- RR
    so2_fixed_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so3,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so3,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "CO_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co3 <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co3$allRRlow[length(ns.pred_co3$allRRfit)], digits = 2) 
    mid = round(ns.pred_co3$allRRfit[length(ns.pred_co3$allRRfit)], digits = 2) 
    high = round(ns.pred_co3$allRRhigh[length(ns.pred_co3$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    co_fixed_RR <- RR
    co_fixed_adjMFr2 <- adjMFr2

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co3,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co3,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)
    logLik(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "O3_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o33 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o33$allRRlow[length(ns.pred_o33$allRRfit)], digits = 2) 
    mid = round(ns.pred_o33$allRRfit[length(ns.pred_o33$allRRfit)], digits = 2) 
    high = round(ns.pred_o33$allRRhigh[length(ns.pred_o33$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o33,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o33,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "PM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm3 <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm3$allRRlow[length(ns.pred_pm3$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm3$allRRfit[length(ns.pred_pm3$allRRfit)], digits = 2) 
    high = round(ns.pred_pm3$allRRhigh[length(ns.pred_pm3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm3,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm3,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### model with fixed effect (ESF X) end. #####
################################################


path9 = paste0(path, '\\9. combined pdf images for paper')
unlink(path9)
dir.create(path9)
# set path to folder where plots to be saved
setwd(path9)

# supple 3. asthmaEM_CO_overall
pdf(file="asthmaEM_CO_overall.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_co,"overall", xlab="CO_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_general_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_general_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred_co3,"overall", xlab="CO_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_fixed_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_fixed_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred2_co,"overall", xlab="CO_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_esf_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_esf_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
dev.off()

# supple 3. asthmaEM_CO_3D
pdf(file="asthmaEM_CO_3D.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_co, zlab="RR", xlab="CO_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
plot(ns.pred_co3, zlab="RR", xlab="CO_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
plot(ns.pred2_co, zlab="RR", xlab="CO_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
dev.off()

# supple 4. rhinitisOUT_NO2_overall
pdf(file="rhinitisOUT_NO2_overall.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_no,"overall", xlab="NO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_general_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_general_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred_no3,"overall", xlab="NO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_fixed_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_fixed_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred2_no,"overall", xlab="NO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_esf_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_esf_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
dev.off()

# supple 4. rhinitisOUT_NO2_3D
pdf(file="rhinitisOUT_NO2_3D.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_no, zlab="RR", xlab="NO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
plot(ns.pred_no3, zlab="RR", xlab="NO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
plot(ns.pred2_no, zlab="RR", xlab="NO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
dev.off()

# supple 5. rhinitisOUT_CO_overall
pdf(file="rhinitisOUT_CO_overall.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_co,"overall", xlab="CO_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_general_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_general_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred_co3,"overall", xlab="CO_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_fixed_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_fixed_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred2_co,"overall", xlab="CO_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = co_esf_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = co_esf_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
dev.off()

# supple 5. rhinitisOUT_CO_3D
pdf(file="rhinitisOUT_CO_3D.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_co, zlab="RR", xlab="CO_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
plot(ns.pred_co3, zlab="RR", xlab="CO_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
plot(ns.pred2_co, zlab="RR", xlab="CO_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
dev.off()

# supple 6. atopicOUT_NO2_overall
pdf(file="atopicOUT_NO2_overall.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_no,"overall", xlab="NO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_general_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_general_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred_no3,"overall", xlab="NO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_fixed_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_fixed_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred2_no,"overall", xlab="NO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = no2_esf_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = no2_esf_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
dev.off()

# supple 6. atopicOUT_NO2_3D
pdf(file="atopicOUT_NO2_3D.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_no, zlab="RR", xlab="NO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
plot(ns.pred_no3, zlab="RR", xlab="NO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
plot(ns.pred2_no, zlab="RR", xlab="NO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
dev.off()

# supple 7. atopicOUT_SO2_overall
pdf(file="atopicOUT_SO2_overall.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_so,"overall", xlab="SO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = so2_general_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = so2_general_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred_so3,"overall", xlab="SO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = so2_fixed_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = so2_fixed_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
plot(ns.pred2_so,"overall", xlab="SO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
legend("topright", legend = so2_esf_RR, box.lty = 0, bg='transparent', cex = 1.1)
legend("topleft", legend = so2_esf_adjMFr2, box.lty = 0, bg='transparent', cex = 1.1)
dev.off()

# supple 7. atopicOUT_SO2_3D
pdf(file="atopicOUT_SO2_3D.pdf", width = 21, height = 7)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(ns.pred_so, zlab="RR", xlab="SO2_mean", main="A. General Model", cex.lab = 1.1, cex.main = 2)
plot(ns.pred_so3, zlab="RR", xlab="SO2_mean", main="B. Spatial model with fixed effect", cex.lab = 1.1, cex.main = 2)
plot(ns.pred2_so, zlab="RR", xlab="SO2_mean", main="C. Spatial model with ESF", cex.lab = 1.1, cex.main = 2)
dev.off()



path11 = paste0(path, '\\11. fixed effect model - multiple exposure')
unlink(path11)
dir.create(path11)
##### 11. Fixed effect model - multiple exposure #####

  setwd(path11)

  # 11-1. ns.basis_no & ns.basis_co - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  + ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2CO_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - NO2
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2co_NO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="no2co_NO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - CO
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="no2co_CO_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="no2co_CO_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_co - multiexposure end. #####

  # 11-2. ns.basis_no & ns.basis_so - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  + ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2SO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - NO2
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2so2_NO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="no2so2_NO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - SO2
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="no2so2_SO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="no2so2_SO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_so - multiexposure end. #####

  # 11-3. ns.basis_no & ns.basis_pm - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  + ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2PM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - NO2
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_multiexp <- crosspred(ns.basis_no,ns_lag_multiexp,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - NO2
    png(filename="no2pm10_NO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="no2pm10_NO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_pm
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="no2pm10_PM10_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="no2pm10_PM10_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no & ns.basis_pm - multiexposure end. #####

  # 11-4. ns.basis_so & ns.basis_co - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_so
                  + ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "SO2CO_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - SO2
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="so2co_SO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="so2co_SO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - CO
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="so2co_CO_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="so2co_CO_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so & ns.basis_co - multiexposure end. #####

  # 11-5. ns.basis_so & ns.basis_pm - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_so
                  + ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "SO2PM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - SO2
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_multiexp <- crosspred(ns.basis_so,ns_lag_multiexp,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - SO2
    png(filename="so2pm10_SO2_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="so2pm10_SO2_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_pm
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="so2pm10_PM10_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="so2pm10_PM10_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so & ns.basis_pm - multiexposure end. #####

  # 11-6. ns.basis_co & ns.basis_pm - multiexposure
    # fit
    ns_lag_multiexp <- glm(ATOPIC_out_total ~
                  ns.basis_co
                  + ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_multiexp)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_multiexp$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "COPM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_multiexp)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_multiexp), 1 - (deviance - 114)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred - CO
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_multiexp <- crosspred(ns.basis_co,ns_lag_multiexp,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - CO
    png(filename="copm10_CO_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="copm10_CO_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

    # crosspred - PM10
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_pm
    ns.pred_multiexp <- crosspred(ns.basis_pm,ns_lag_multiexp,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_multiexp$allRRlow[length(ns.pred_multiexp$allRRfit)], digits = 2)
    mid = round(ns.pred_multiexp$allRRfit[length(ns.pred_multiexp$allRRfit)], digits = 2)
    high = round(ns.pred_multiexp$allRRhigh[length(ns.pred_multiexp$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save - PM10
    png(filename="copm10_PM10_mean.png")
    plot(ns.pred_multiexp,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="copm10_PM10_mean_overall.png")
    plot(ns.pred_multiexp,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co & ns.basis_pm - multiexposure end. #####

##### Fixed effect model - multiple exposure end. #####
#######################################################



path8 = paste0(path, '\\8. random effect model (ESF X)')
unlink(path8)
dir.create(path8)
##### 8. model with random effect (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path8)
  library(lme4)
  library("MCMCglmm")
  library("broom.mixed")

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- MCMCglmm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  , random ~air_out_idx
                  , data = df
                  ,	family = poisson()
                  ,pr=TRUE,nitt=10000,DIC=TRUE, burnin =3000); 
    head(df)
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glmer(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + (1|dt)
                  ,	family=poisson(), df); # "quasi" families cannot be used in glmer
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "SO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glmer(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + (1|dt)
                  ,	family=poisson(), df); # "quasi" families cannot be used in glmer
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "CO_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glmer(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + (1|dt)
                  ,	family=poisson(), df); # "quasi" families cannot be used in glmer
    summary(ns_lag_o3)
    logLik(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "O3_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glmer(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + (1|dt)
                  ,	family=poisson(), df); # "quasi" families cannot be used in glmer
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "PM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### model with random effect (ESF X) end. #####
######################################



path13 = paste0(path, '\\0. aggregated model')
# path13 = paste0(path, '\\0. aggregated model')
unlink(path13)
dir.create(path13)
##### 0. aggregated model #####
  # set path to folder where plots to be saved
  setwd(path13)
  head(df)

  # 4-1. NO2
    # fit
    no_model <- glm(ATOPIC_out_total_agg ~
                  ns.basis_no_agg # 이거는 crossbasis 만들어서 걸어야 함
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_by_idx, df=4) + ns(humi_mean_by_idx, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 

    # no_model <- glm(ATOPIC_out_total_agg ~
    #               no2_mean_by_idx
    #               # + ns.basis_temp_tc
    #               # + ns.basis_temp_mean
    #               # + ns.basis_humi 
    #               #+ ns.basis_temp_min
    #               #+ ns.basis_temp_max
    #               + temp_mean_by_idx + humi_mean_by_idx
    #               + factor(day) 
    #               # + factor(air_out_idx)
    #               + doy
    #               # + esf_em
    #               ,	family = quasipoisson(), df);     

    # test <- glm(ATOPIC_out_total ~ 1, data=df, family=quasipoisson())
    # with(summary(test), null.deviance)
    # summary(test)

    table(df$ATOPIC_out_total_agg)

    logLik(no_model)
    logLik(no_model.init)
    summary(no_model)
    library(car)
    vif(no_model)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(no_model), 1 - (deviance - 31)/null.deviance), digits = 3)

    # with(summary(ns_lag_no), deviance)
    # with(summary(ns_lag_no), null.deviance)

    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    # crosspred
    percentiles_no <- round(quantile(df$no2_mean_by_idx,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no_agg,no_model,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    summary(ns.pred_no)

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    so_model <- glm(ATOPIC_out_total_agg ~
                  ns.basis_so_agg
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_by_idx, df=4) + ns(humi_mean_by_idx, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df);
    summary(so_model)
    logLik(so_model)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(so_model), 1 - (deviance - 31)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_so <- round(quantile(df$so2_mean_by_idx,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so_agg,so_model,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    co_model <- glm(ATOPIC_out_total_agg ~
                  ns.basis_co_agg
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_by_idx, df=4) + ns(humi_mean_by_idx, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df);
    summary(co_model)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(co_model), 1 - (deviance - 31)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_co <- round(quantile(df$co_mean_by_idx,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co_agg,co_model,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # # 4-4. ns.basis_o3
  #   # fit
  #   ns_lag_o3 <- glm(ATOPIC_out_total ~ 
  #                 ns.basis_o3
  #                 # + ns.basis_temp_tc
  #                 # + ns.basis_temp_mean
  #                 # + ns.basis_humi 
                  
  #                 # + ns.basis_temp_min
  #                 + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
  #                 # + factor(air_out_idx)
  #                 + factor(day) +ns(doy,4)
  #                 # + esf_em
  #                 ,	family=quasipoisson(), df); 
  #   summary(ns_lag_o3)

  #   #calculate McFadden's R-squared for model
  #   adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 31)/null.deviance), digits = 3)
  #   adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

  #   # crosspred
  #   percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
  #   ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

  #   low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
  #   mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
  #   high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

  #   RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

  #   # draw plot and save
  #   png(filename="O3_mean.png")
  #   plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
  #   dev.off()
  #   png(filename="O3_mean_overall.png")
  #   plot(ns.pred_o3,"overall",xlab="O3_mean")
  #   legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
  #   legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
  #   dev.off()

  # ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    pm_model <- glm(ATOPIC_out_total_agg ~
                  ns.basis_pm_agg
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_by_idx, df=4) + ns(humi_mean_by_idx, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df);
    summary(pm_model)
    # anova(ns_lag_pm)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(pm_model), 1 - (deviance - 31)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_pm <- round(quantile(df$pm10_mean_by_idx,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm_agg,pm_model,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### aggregated model end. #####
######################################









## ONLY RHINITIS IN  - SES models ##
## 12.08 '도시인구' 삭제

head(df$요양기관.수)
summary(df$요양기관.수)


path9 = paste0(path, '\\6. general model with SES (ESF X)')
# path9 = paste0(path, '\\6. general model with SES (ESF X) (13-17)')
unlink(path9)
dir.create(path9)
##### 6. general model with SES (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path9)

  # 4-1. ns.basis_no
    colnames(df)
    # fit
    ns_lag_no <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 
                  + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family = quasipoisson(), df); 

    logLik(ns_lag_no)
    logLik(ns_lag_no.init)
    summary(ns_lag_no)
    vif(ns_lag_no)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 34)/null.deviance), digits = 3)

    with(summary(ns_lag_no), deviance)
    with(summary(ns_lag_no), null.deviance)


    # r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)
    # PseudoR2(r.glm)
    # PseudoR2(r.glm, c("McFadden", "Nagel"))
    
    # pR2(ns_lag_no)['McFadden']

    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    dev.cur()
    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)
    
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 34)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 34)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 34)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 34)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### general model with SES (ESF X) end. #####
######################################

path11 = paste0(path, '\\8. fixed effect model with SES')
# path11 = paste0(path, '\\3. fixed effect model (ESF X) (13-17)')
unlink(path11)
dir.create(path11)
##### 8. fixed effect model with SES #####
  # set path to folder where plots to be saved
  setwd(path11)

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  # + ns(doy,4)
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)
    logLik(ns_lag_no)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "NO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "SO2_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) 
                  # +ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "CO_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) 
                  # +ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)
    logLik(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "O3_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  # + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    sink(file = "PM10_MoransI.txt")
    print("Fixed effect model")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 102)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### fixed effect model with SES end. #####
######################################

path10 = paste0(path, '\\7. ESF model with SES')
# path10 = paste0(path, '\\7. ESF model with SES (13-17)')
unlink(path10)
dir.create(path10)
##### 7. ESF model with SES #####

  setwd(path10)
  # 5-1. ns.basis_no
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_no
                 # + ns.basis_temp_mean
                 # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total,df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) 
                  # + ns(doy,4)
                  + esf_em
                  + 요양기관.수 + 재정자립도.세입과목개편후. 
                  # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 35)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$ATOPIC_out_total)
    # mean(df_esf$ATOPIC_out_total)
    # poi_test=rpois(n=sum(df_esf$ATOPIC_out_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "NO2_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)


    # crosspred
    ns.pred2_no <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred2_no$allRRlow[length(ns.pred2_no$allRRfit)], digits = 2)
    mid = round(ns.pred2_no$allRRfit[length(ns.pred2_no$allRRfit)], digits = 2)
    high = round(ns.pred2_no$allRRhigh[length(ns.pred2_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean_esf.png")
    plot(ns.pred2_no,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="NO2_mean_esf_overall.png")
    plot(ns.pred2_no,"overall",xlab="NO2_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 5-2. ns.basis_so
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_so
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) 
                  # + ns(doy,4)
                    + esf_em
                    + 요양기관.수 + 재정자립도.세입과목개편후. 
                    # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    # anova(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 35)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "SO2_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_so <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred2_so$allRRlow[length(ns.pred2_so$allRRfit)], digits = 2)
    mid = round(ns.pred2_so$allRRfit[length(ns.pred2_so$allRRfit)], digits = 2)
    high = round(ns.pred2_so$allRRhigh[length(ns.pred2_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean_esf.png")
    plot(ns.pred2_so,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="SO2_mean_esf_overall.png")
    plot(ns.pred2_so,"overall",xlab="SO2_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_so end. #####

  # 5-3. ns.basis_co
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_co
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) 
                  # + ns(doy,4)
                    + esf_em
                    + 요양기관.수 + 재정자립도.세입과목개편후. 
                    # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 35)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "CO_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_co <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred2_co$allRRlow[length(ns.pred2_co$allRRfit)], digits = 2)
    mid = round(ns.pred2_co$allRRfit[length(ns.pred2_co$allRRfit)], digits = 2)
    high = round(ns.pred2_co$allRRhigh[length(ns.pred2_co$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean_esf.png")
    plot(ns.pred2_co,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="CO_mean_esf_overall.png")
    plot(ns.pred2_co,"overall",xlab="CO_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_co end. #####

  # 5-4. ns.basis_o3
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_o3
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) 
                  # + ns(doy,4)
                    + esf_em
                    + 요양기관.수 + 재정자립도.세입과목개편후. 
                    # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 35)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "O3_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_o3 <- crosspred(ns.basis_o3,ns_lag3_esf,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1]);
    
    low = round(ns.pred2_o3$allRRlow[length(ns.pred2_o3$allRRfit)], digits = 2)
    mid = round(ns.pred2_o3$allRRfit[length(ns.pred2_o3$allRRfit)], digits = 2)
    high = round(ns.pred2_o3$allRRhigh[length(ns.pred2_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean_esf.png")
    plot(ns.pred2_o3,zlab="RR",xlab="O3_mean_esf")
    dev.off()
    png(filename="O3_mean_esf_overall.png")
    plot(ns.pred2_o3,"overall",xlab="O3_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_o3 end. #####

  # 5-5. ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(ATOPIC_out_total ~  
                  ns.basis_pm
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) 
                  # + ns(doy,4)
                    + esf_em
                    + 요양기관.수 + 재정자립도.세입과목개편후. 
                    # + 도시인구.용도지역기준.
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    head(df_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 35)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    sink(file = "PM10_MoransI.txt")
    print("General")
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    print("ESF")
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    sink(file = NULL)

    # crosspred
    ns.pred2_pm <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);
    
    low = round(ns.pred2_pm$allRRlow[length(ns.pred2_pm$allRRfit)], digits = 2)
    mid = round(ns.pred2_pm$allRRfit[length(ns.pred2_pm$allRRfit)], digits = 2)
    high = round(ns.pred2_pm$allRRhigh[length(ns.pred2_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean_esf.png")
    plot(ns.pred2_pm,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="PM10_mean_esf_overall.png")
    plot(ns.pred2_pm,"overall",xlab="PM10_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_pm end. #####

##### 7. ESF model with SES end. #####
#############################################################

#### ONLY RHINITIS IN end. ####




#### canceled models ####

path7 = paste0(path, '\\4. offset model (ESF X)')
unlink(path7)
dir.create(path7)
##### 7. model with offset (ESF X) #####

    setwd(path7)
    # lag 추가
    df = df%>%
        group_by(air_out_idx)%>%
        mutate(ATOPIC_out_total_lag7=lag(ATOPIC_out_total,7))%>%
        mutate(ATOPIC_out_total_lag14=lag(ATOPIC_out_total,14))
    dim(df)

    # 7일 제거
    df <- na.omit(df)
    table(df$ATOPIC_out_total_lag7)
    dim(df)
    # offset(log(df$ATOPIC_out_total_lag7))

    # outcome_lag 변수들에 + 1 (for log)
    df$ATOPIC_out_total_lag7 = df$ATOPIC_out_total_lag7 + 1
    df$ATOPIC_out_total_lag14 = df$ATOPIC_out_total_lag14 + 1

  # 6-1. ns.basis_no
    # fit
    ns_lag_no <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_out_total_lag7))
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 6-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_out_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 6-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_out_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 6-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_out_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 6-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_out_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

##### model with offset (ESF X) end. #####
######################################

path8 = paste0(path, '\\5. log(lag) model (ESF X)')
unlink(path8)
dir.create(path8)
##### 8. model with log(lag) (ESF X) #####

    setwd(path8)
    # lag 추가
    df = df%>%
        group_by(air_out_idx)%>%
        mutate(ATOPIC_out_total_lag7=lag(ATOPIC_out_total,7))%>%
        mutate(ATOPIC_out_total_lag14=lag(ATOPIC_out_total,14))
    dim(df)

    # 7일 제거
    df <- na.omit(df)
    table(df$ATOPIC_out_total_lag7)
    dim(df)
    # offset(log(df$ATOPIC_out_total_lag7))

    # outcome_lag 변수들에 + 1 (for log)
    df$ATOPIC_out_total_lag7 = df$ATOPIC_out_total_lag7 + 1
    df$ATOPIC_out_total_lag14 = df$ATOPIC_out_total_lag14 + 1

  # 6-1. ns.basis_no
    # fit
    ns_lag_no <- glm(ATOPIC_out_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  + log(ATOPIC_out_total_lag7)
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 37)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 6-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_out_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + log(ATOPIC_out_total_lag7)
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 37)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 6-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_out_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + log(ATOPIC_out_total_lag7)
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 37)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 6-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_out_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + log(ATOPIC_out_total_lag7)
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 37)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 6-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_out_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  # + ns.basis_temp_mean
                  # + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + log(ATOPIC_out_total_lag7)
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 37)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

##### model with log(lag) (ESF X) end. #####
######################################

#########################





#### package test ####
  # sppois
  # devtools::install_github("gregmacfarlane/sppois")
  library(sppois)
  df_testwithday <- df[df$dt %in% c('2016-01-07'), ]
  dim(df_testwithday)
  summary(sarpoisson(ATOPIC_out_total ~ ns.basis_no
                    # + ns.basis_temp_tc
                    + ns.basis_temp_mean
                    + ns.basis_humi 
                    #+ ns.basis_temp_min
                    #+ ns.basis_temp_max
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    # + factor(day) 
                    # + factor(air_out_idx)
                    # + ns(doy,4)
                    # + esf_em
                    , data = df_testwithday,
                    listw = col.listw_sudogwon, method = 'fiml'))

  head(df)
  dim(df)
  columbus_neighbors
  dim(columbus_crime)


  # errorsarlm
  # install.packages("spatialreg")
  library(spatialreg)
  library(spdep)

  data(oldcol)
  lw <- nb2listw(COL.nb, style="W")
  COL.errW.eig <- errorsarlm(CRIME ~ INC + HOVAL, data=COL.OLD,
  lw, method="eigen", quiet=FALSE)
  summary(COL.errW.eig, correlation=TRUE)


  COL.errW.eig <- errorsarlm(ATOPIC_out_total ~ ns.basis_no
                    # + ns.basis_temp_tc
                    + ns.basis_temp_mean
                    + ns.basis_humi 
                    #+ ns.basis_temp_min
                    #+ ns.basis_temp_max
                    + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                    + factor(day) 
                    # + factor(air_out_idx)
                    + ns(doy,4)
                    # + esf_em
                    , data = df,
                    col.listw_sudogwon, method="eigen", quiet=FALSE)
  summary(COL.errW.eig, correlation=TRUE)

#### package test end. ####



### calculator ###

x <- c(0.3392013, 0.3191927, 0.3432385, 0.3352751, 0.3305053)
mean(x)
sd(x)
