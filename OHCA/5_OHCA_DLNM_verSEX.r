
# install.packages('dlnm')
# install.packages('splines')
# install.packages("httpgd")
install.packages("knitr")
install.packages("rmarkdown")

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
#################


##### 0. ReadMe #####
  # 카운트 변수 바꿔가면서 사용할 것 (바뀜 방지용 한글표기)
  # 카운트 : 전체 aggregate
  # 카운트_disease : 질병성 심정지인 사람들만 aggregate
  # 카운트_disease_male : 질병성 심정지 중, 남자만 aggregate
  # 카운트_disease_female : 질병성 심정지 중, 여자만 aggregate
  # 카운트_disease_above70 : 질병성 심정지 중, 70세 이상인 사람만 aggregate
  # 카운트_disease_under70 : 질병성 심정지 중, 70세 미만인 사람만 aggregate
  # 카운트_disease_Mhistory_0 : 질병성 심정지 중, 과거력 (고혈압, 당뇨, 심장질환)이 없는 사람만 aggregate
  # 카운트_disease_Mhistory_1 : 질병성 심정지 중, 과거력 (고혈압, 당뇨, 심장질환)이 있는 사람만 aggregate
#####################


##### 0. If change needed, change in here #####
lag = 7
nk_to_use = 2

# define path to save plots
  filter = "filterSFW"
  outcome = "count_disease_gender"
  lagNum = "lag7"
  path = sprintf("D:\\SNUlab\\Results\\OHCA_ESF_자동화\\After_Outlier_Processed\\%s+%s+%s", outcome, lagNum, filter)
  path
###############################################


##### 1. read df #####
setwd('D:\\SNUlab\\')
# patient <- read.csv("data\\급성심장정지조사(08-19)SAS\\patient_final.csv", encoding = "euc-kr")  # 01.13 patient_final로 수정 # nolint
patient <- read.csv("data\\급성심장정지조사(08-19)SAS\\patient_final_0121_outlierprocessed.csv", encoding = "euc-kr")  # 01.24 patient_final_0121_outlierprocessed로 수정 # nolint
###################

colnames(patient)
cor(patient[,c(4,6,8,10,12,14,16,18,20,22)])
patient_male=patient[,c(1:67,69)]
patient_female=patient[,c(1:67,70)]
colnames(patient_male)[68]="count_disease_gender"
colnames(patient_female)[68]="count_disease_gender"

patient_male$sex="M"

patient_female$sex="F"

patient_gender=rbind(patient_male,patient_female)
patient_gender$sex=factor(patient_gender$sex)
patient = copy(patient_gender)

##### 2. create crossbasis #####
  # NO2
  kno <- equalknots(patient$NO2_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_no <- crossbasis(patient$NO2_mean,argvar=list(knots=kno),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_no # nolint # nolint
  patient$ns.basis_no <- crossbasis(patient$NO2_mean,argvar=list(knots=kno),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #patient$ns.basis_no

  # SO2
  kso <- equalknots(patient$SO2_mean, nk = nk_to_use)

  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_so <- crossbasis(patient$SO2_mean,argvar=list(knots=kso),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_so
  patient$ns.basis_so <- crossbasis(patient$SO2_mean,argvar=list(knots=kso),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #patient$ns.basis_so

  # CO
  kco <- equalknots(patient$CO_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_co <- crossbasis(patient$CO_mean,argvar=list(knots=kco),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_co
  patient$ns.basis_co <- crossbasis(patient$CO_mean,argvar=list(knots=kco),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #patient$ns.basis_co

  # O3
  ko3 <- equalknots(patient$O3_mean, nk = nk_to_use)
  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_o3 <- crossbasis(patient$O3_mean,argvar=list(knots=ko3),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_o3
  patient$ns.basis_o3 <- crossbasis(patient$O3_mean,argvar=list(knots=ko3),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #patient$ns.basis_o3     

  # PM10
  kpm <- equalknots(patient$PM10_mean[which(patient$PM10_mean<153.9809)], nk = nk_to_use)

  klag <- logknots(lag, nk = nk_to_use)
  ns.basis_pm <- crossbasis(patient$PM10_mean,argvar=list(knots=kpm),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #ns.basis_pm
  patient$ns.basis_pm <- crossbasis(patient$PM10_mean,argvar=list(knots=kpm),group=patient$air_out_idx, arglag=list(knots=klag), lag=lag); #patient$ns.basis_pm

  # temp_tc
  ktemp_tc <- equalknots(patient$temp_tc_total, nk = nk_to_use)
  klag <- logknots(1, nk = 1)
  ns.basis_temp_tc <- crossbasis(patient$temp_tc_total,argvar=list(knots=ktemp_tc),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  patient$ns.basis_temp_tc <- crossbasis(patient$temp_tc_total,argvar=list(knots=ktemp_tc),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_temp_tc

  ktemp_tc2 <- equalknots(patient$temp_mean_total, nk = nk_to_use)
  ns.basis_temp_mean <- crossbasis(patient$temp_mean_total,argvar=list(knots=ktemp_tc2),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  patient$ns.basis_temp_mean <- crossbasis(patient$temp_mean_total,argvar=list(knots=ktemp_tc2),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_temp_tc

  # tmp_mean_total
  ktemp_tc2 <- equalknots(patient$temp_mean_total[which(patient$temp_mean_total>-20)],nk = nk_to_use)
  
  ns.basis_temp_mean <- crossbasis(patient$temp_mean_total,argvar=list(knots=ktemp_tc2),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_temp_tc
  patient$ns.basis_temp_mean <- crossbasis(patient$temp_mean_total,argvar=list(knots=ktemp_tc2),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_temp_tc

  # temp_min # 01.24 추가
  ktemp_min <- equalknots(patient$temp_min[which(patient$temp_min>-30)],nk = nk_to_use)
  
  ns.basis_temp_min <- crossbasis(patient$temp_min,argvar=list(knots=ktemp_min),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); 
  patient$ns.basis_temp_min <- crossbasis(patient$temp_min,argvar=list(knots=ktemp_min),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_temp_tc

  # temp_max # 01.24 추가
  ktemp_max <- equalknots(patient$temp_max[which(patient$temp_max<50)],nk = nk_to_use)
  
  ns.basis_temp_max <- crossbasis(patient$temp_max,argvar=list(knots=ktemp_max),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); 
  patient$ns.basis_temp_max <- crossbasis(patient$temp_max,argvar=list(knots=ktemp_max),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_temp_tc

  # humi
  khumi <- equalknots(patient$humi_mean, nk = nk_to_use)
  klag <- logknots(1, nk = 1)
  ns.basis_humi <- crossbasis(patient$humi_mean,argvar=list(knots=khumi),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #ns.basis_humi
  patient$ns.basis_humi <- crossbasis(patient$humi_mean,argvar=list(knots=khumi),group=patient$air_out_idx, arglag=list(knots=klag),lag=0); #patient$ns.basis_humi

##### create crossbasis end. #####
##################################


##### 3. create day #####
  patient$day=wday(
    patient$PRE_ER_ARREST_DT,
    label = FALSE,
    abbr = TRUE,
    week_start = getOption("lubridate.week.start", 7),
    locale = Sys.getlocale("LC_TIME")
  )

  patient$day = factor(patient$day)

  patient <- patient[,c("day","PRE_ER_ARREST_DT","air_out_idx",
                      "NO2_mean","ns.basis_no",
                      "SO2_mean","ns.basis_so",
                      "CO_mean","ns.basis_co",
                      "O3_mean","ns.basis_o3",
                      "PM10_mean","ns.basis_pm",
                      "temp_mean_total","ns.basis_temp_mean",
                      "temp_min", "ns.basis_temp_min", # 01.24 temp_min 추가
                      "temp_max", "ns.basis_temp_max", # 01.24 temp_max 추가
                      "humi_mean_total","ns.basis_humi",
                      "count_disease_gender","sex")]

  patient=patient[complete.cases(patient),]
  patient$doy=yday(as.Date(patient$PRE_ER_ARREST_DT,origin="1970-01-01"))
  patient$year=substr(patient$PRE_ER_ARREST_DT,1,4)
  # table(patient$count_disease_gender,patient$year)

##### create day end. #####
###########################


##### 4. general model (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path)

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- glm(count_disease_gender ~ 
                  ns.basis_no
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) +ns(doy,4) + sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_no)
    
    anova(ns_lag_no)
    # crosspred
    percentiles_no <- round(quantile(patient$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])
    percentiles_temp <- round(quantile(patient$temp_mean_total,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_temp
    ns.pred_temp <- crosspred(ns.basis_temp_mean,ns_lag_no,at=seq(percentiles_temp[1],percentiles_temp[4], 0.1),cen = percentiles_temp[1])

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    dev.off()

    plot(ns.pred_temp)
  ##### ns.basis_no end. #####
  cor(patient$ns.basis_no,patient$ns.basis_pm)

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(count_disease_gender ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_so)

    # crosspred
    percentiles_so <- round(quantile(patient$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    dev.off()
  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(count_disease_gender ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) +ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_co)

    # crosspred
    percentiles_co <- round(quantile(patient$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    # draw plot and save
    png(filename="CO2_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO2_mean")
    dev.off()
    png(filename="CO2_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO2_mean")
    dev.off()
  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(count_disease_gender ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) +ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_o3)

    # crosspred
    percentiles_o3 <- round(quantile(patient$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    dev.off()
  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(count_disease_gender ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_pm)
    anova(ns_lag_pm)
    # crosspred
    percentiles_pm <- round(quantile(patient$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])
    percentiles_temp <- round(quantile(patient$temp_mean_total,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_temp
    ns.pred_temp <- crosspred(ns.basis_temp_mean,ns_lag_pm,at=seq(percentiles_temp[1],percentiles_temp[4], 0.1),cen = percentiles_temp[1])
ns.pred_pm$matRRhigh
    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    dev.off()
    plot(ns.pred_temp)
  ##### ns.basis_pm end. #####

  # 4-6. ns.basis_temp_mean
    # fit
    ns_lag_temp <- glm(count_disease_gender ~ 
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                   #+ ns.basis_temp_max
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_temp)

    # crosspred
    percentiles_temp <- round(quantile(patient$temp_mean_total,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_temp
    ns.pred_temp <- crosspred(ns.basis_temp_mean,ns_lag_temp,at=seq(percentiles_temp[1],percentiles_temp[4], 0.1),cen = percentiles_temp[1])

    # # crosspred
     percentiles_tempmax <- round(quantile(patient$temp_max,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_tempmax
     ns.pred_tempmax <- crosspred(ns.basis_temp_max,ns_lag_temp,at=seq(percentiles_tempmax[1],percentiles_tempmax[4], 0.1),cen = percentiles_tempmax[1])

    # # crosspred
    # percentiles_tempmin <- round(quantile(patient$temp_min,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_tempmin
    # ns.pred_tempmin <- crosspred(ns.basis_temp_min,ns_lag_temp,at=seq(percentiles_tempmin[1],percentiles_tempmin[4], 0.1),cen = percentiles_tempmin[1])

    # draw plot and save
    png(filename="Temp_mean_overall.png")
    plot(ns.pred_temp,"overall",xlab="Temp_mean")
    dev.off()

    # # draw plot and save
    # png(filename="Temp_max_overall.png")
    # plot(ns.pred_tempmax,"overall",xlab="Temp_max")
    # dev.off()

    #  # draw plot and save
    # png(filename="Temp_min_overall.png")
    # plot(ns.pred_tempmin,"overall",xlab="Temp_min")
    # dev.off()

  ##### ns.basis_temp_mean end. #####

  # 4-7. ns.basis_humi
    # fit
    ns_lag_humi <- glm(count_disease_gender ~ 
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                  # + esf_em
                  ,	family=quasipoisson(), patient); 
    summary(ns_lag_humi)

    # crosspred
    percentiles_humi <- round(quantile(patient$humi_mean_total,c(0.01,0.25,0.75,0.95)),digits=4) ; percentiles_humi
    ns.pred_humi <- crosspred(ns.basis_humi,ns_lag_humi,at=seq(percentiles_humi[1],percentiles_humi[4], 0.1),cen = percentiles_humi[1])

    # draw plot and save
    png(filename="Humidity_mean_overall.png")
    plot(ns.pred_humi,"overall",xlab="Humidity_mean")
    dev.off()
  ##### ns.basis_humi end. #####

##### general model (ESF X) end. #####
######################################


##### 5. create patient_esf data & fit model with ESF #####

  # 5-1. ns.basis_no
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_no$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  ns.basis_no
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)
table(patient_esf$count_disease_gender)
mean(patient_esf$count_disease_gender)
poi_test=rpois(n=sum(patient_esf$count_disease_gender),lambda=0.1531)
table(poi_test)   
    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    anova(ns_lag3_esf)
    # crosspred
    ns.pred2_no <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);

    # draw plot and save
    png(filename="NO2_mean_esf.png")
    plot(ns.pred2_no,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="NO2_mean_esf_overall.png")
    plot(ns.pred2_no,"overall",xlab="NO2_mean_esf")
    dev.off()
  ##### ns.basis_no end. #####

  # 5-2. ns.basis_so
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_so$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  ns.basis_so
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)
    colnames(patient_esf)
    mean(patient_esf$count_disease_gender)
    dim(patient_esf)
    table(patient_esf$count_disease_gender)
    poi_test=rpois(265120,0.3)
    table(poi_test)
    anova(ns_lag3_esf)
    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_so <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);

    # draw plot and save
    png(filename="SO2_mean_esf.png")
    plot(ns.pred2_so,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="SO2_mean_esf_overall.png")
    plot(ns.pred2_so,"overall",xlab="SO2_mean_esf")
    dev.off()
  ##### ns.basis_so end. #####

  # 5-3. ns.basis_co
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_co$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  ns.basis_co
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_co <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);

    # draw plot and save
    png(filename="CO2_mean_esf.png")
    plot(ns.pred2_co,zlab="RR",xlab="CO2_mean_esf")
    dev.off()
    png(filename="CO2_mean_esf_overall.png")
    plot(ns.pred2_co,"overall",xlab="CO2_mean_esf")
    dev.off()
  ##### ns.basis_co end. #####

  # 5-4. ns.basis_o3
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_o3$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  ns.basis_o3
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_o3 <- crosspred(ns.basis_o3,ns_lag3_esf,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1]);

    # draw plot and save
    png(filename="O3_mean_esf.png")
    plot(ns.pred2_o3,zlab="RR",xlab="O3_mean_esf")
    dev.off()
    png(filename="O3_mean_esf_overall.png")
    plot(ns.pred2_o3,"overall",xlab="O3_mean_esf")
    dev.off()
  ##### ns.basis_o3 end. #####

  # 5-5. ns.basis_pm
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_pm$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  ns.basis_pm
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_pm <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);

    # draw plot and save
    png(filename="PM10_mean_esf.png")
    plot(ns.pred2_pm,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="PM10_mean_esf_overall.png")
    plot(ns.pred2_pm,"overall",xlab="PM10_mean_esf")
    dev.off()
  ##### ns.basis_pm end. #####

  # 5-6. ns.basis_temp
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_temp$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_temp <- crosspred(ns.basis_temp_mean,ns_lag3_esf,at=seq(percentiles_temp[1],percentiles_temp[4], 0.001),cen = percentiles_temp[1]);

    # draw plot and save
    png(filename="Temp_mean_esf_overall.png")
    plot(ns.pred2_temp,"overall",xlab="Temp_mean_esf")
    dev.off()
  ##### ns.basis_temp end. #####

  # 5-7. ns.basis_humi
    # create patient_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_humi$residuals,by=list(patient$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[76]="rsum_ohca_em"
    lm.init <- lm(rsum_ohca_em ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_ohca_em ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    summary(esf.ccm)
    # dim(summary(esf.ccm)$coefficients)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$Index)
    colnames(esf)=c("esf_em", "air_out_idx")
    patient_esf <- merge(patient,esf,by=c("air_out_idx"))

    # fit
    ns_lag3_esf <- glm(count_disease_gender ~  
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  # + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)+sex
                    + esf_em
                  ,	family=quasipoisson(), patient_esf); 
    summary(ns_lag3_esf)

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(patient_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_humi <- crosspred(ns.basis_humi,ns_lag3_esf,at=seq(percentiles_humi[1],percentiles_humi[4], 0.001),cen = percentiles_humi[1]);

    # draw plot and save
    png(filename="Humidity_mean_esf_overall.png")
    plot(ns.pred2_humi,"overall",xlab="Humidity_mean_esf")
    dev.off()
  ##### ns.basis_humi end. #####
  
##### create patient_esf data & fit model with ESF end. #####
#############################################################

