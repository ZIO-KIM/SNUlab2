##### 라이브러리 불러오기 ##### 
library(haven)
library(ivtools)
library(splines)
library(lubridate)
require(devtools)
# install_version("instruments", version = "0.1", repos = "http://cran.us.r-project.org")
library(instruments)
library(dlnm)
library("naivereg")
library(OneSampleMR)

## rdata file load
# load(file="D:\\SNUlab\\thermal_inversion_0623_share\\220622_thermal_inversion.RData")

##### outcome 데이터 불러오기 #####
day_rhinitis_1317 <- read_sas("day_rhinitis_1317_cnt_3_total1.sas7bdat",NULL)
day_rhinitis_1317$age_group<-ifelse(day_rhinitis_1317$AGE %in% c(0,5),1,ifelse(day_rhinitis_1317$AGE%in%c(10,15),2,ifelse(day_rhinitis_1317$AGE>=65,3,4)))
day_rhinitis_1317_agg=aggregate(day_rhinitis_1317[,7:10], by=list(day_rhinitis_1317$age_group,
                                                              day_rhinitis_1317$SEX_TYPE,
                                                              day_rhinitis_1317$DT,
                                                              day_rhinitis_1317$"시군구"),sum)
# save.image("thermal_inversion_220419.RData")
colnames(day_rhinitis_1317_agg)[1:4]=c("age_group","sex","dt","측정소명")
day_rhinitis_1317_agg_seoul<-day_rhinitis_1317_agg[which(substr(day_rhinitis_1317_agg$"측정소명",1,2)=="서울"),]
day_rhinitis_1317_agg_seoul$"측정소명"<-gsub("서울특별시 ","",day_rhinitis_1317_agg_seoul$"측정소명")
day_rhinitis_1317_agg_seoul$date=gsub(" ","",paste(substr(day_rhinitis_1317_agg_seoul$dt,1,4),"-",substr(day_rhinitis_1317_agg_seoul$dt,5,6),"-",substr(day_rhinitis_1317_agg_seoul$dt,7,8)))


##### step2_final_back이랑 합치기 ##### 
# day_rhinitis_1317_agg_seoul_step <- merge(day_rhinitis_1317_agg_seoul,step2_final_back,by=c("date","측정소명"), all.x=TRUE)
day_rhinitis_1317_agg_seoul_step <- merge(day_rhinitis_1317_agg_seoul,step1_final_na,by=c("date","측정소명"), all.x=TRUE)
colnames(day_rhinitis_1317_agg_seoul_step)


# lead 붙인 데이터 불러오기 - 365 # - zio
rhinitis_lead_365 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\rhinitis_lead.csv")

# lead 붙인 데이터 불러오기 - 182 # - zio
day_rhinitis_1317_agg_seoul_step <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\day_rhinitis_1317_agg_seoul_step_lead_182.csv")

# lead 붙인 데이터 불러오기 - 92 # - zio
day_rhinitis_1317_agg_seoul_step <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\day_rhinitis_1317_agg_seoul_step_lead_92.csv")

# lead 붙인 데이터 불러오기 - 730 # - zio
day_rhinitis_1317_agg_seoul_step <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\day_rhinitis_1317_agg_seoul_step_lead_730.csv")

##### lag, crossbasis matrix #####
day_rhinitis_1317_agg_seoul_step$yday <- yday(day_rhinitis_1317_agg_seoul_step$date)
day_rhinitis_1317_agg_seoul_step$wday <- wday(day_rhinitis_1317_agg_seoul_step$date)
day_rhinitis_1317_agg_seoul_step$wday_mat<-model.matrix( ~ wday - 1, data=day_rhinitis_1317_agg_seoul_step)

lag=7
kno_temp <- equalknots(day_rhinitis_1317_agg_seoul_step$temp_mean_total,nk=2)
klag <- logknots(lag,nk=2)

# CROSSBASIS MATRIX
ns.basis_temp <- crossbasis(day_rhinitis_1317_agg_seoul_step$temp_mean_total,
                            argvar=list(knots=kno_temp),
                            group=day_rhinitis_1317_agg_seoul_step$air_out_idx,
                            arglag=list(knots=klag),
                            lag=lag)
day_rhinitis_1317_agg_seoul_step$ns.basis_temp <- crossbasis(day_rhinitis_1317_agg_seoul_step$temp_mean_total,
                                                           argvar=list(knots=kno_temp),
                                                           group=day_rhinitis_1317_agg_seoul_step$air_out_idx,
                                                           arglag=list(knots=klag),
                                                           lag=lag)


kno_yday <- equalknots(day_rhinitis_1317_agg_seoul_step$yday,nk=2)
ns.basis_yday <- crossbasis(day_rhinitis_1317_agg_seoul_step$yday,argvar=list(knots=kno_yday),
                            group=day_rhinitis_1317_agg_seoul_step$air_out_idx,
                            #arglag=list(knots=klag),
                            #lag=lag
)
day_rhinitis_1317_agg_seoul_step$ns.basis_yday <- crossbasis(day_rhinitis_1317_agg_seoul_step$yday,
                                                           argvar=list(knots=kno_yday),
                                                           group=day_rhinitis_1317_agg_seoul_step$air_out_idx,
                                                           #arglag=list(knots=klag),
                                                           #lag=lag
)

##### outcome 정의 #####
day_rhinitis_1317_agg_seoul_step$rhinitis_in_out=day_rhinitis_1317_agg_seoul_step$RHINITIS_in_total+day_rhinitis_1317_agg_seoul_step$RHINITIS_out_total
day_rhinitis_1317_agg_seoul_step$rhinitis_in_em=day_rhinitis_1317_agg_seoul_step$RHINITIS_in_total+day_rhinitis_1317_agg_seoul_step$RHINITIS_em_total
day_rhinitis_1317_agg_seoul_step$rhinitis_in_out_em=day_rhinitis_1317_agg_seoul_step$RHINITIS_in_total+day_rhinitis_1317_agg_seoul_step$RHINITIS_em_total+
  day_rhinitis_1317_agg_seoul_step$RHINITIS_out_total


##### age_group & sex 나누기 ##### - zio
# subgroup 별 outcome column 생성해서 한번에 merge

# age group 1
day_rhinitis_age1 <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==1), c("date", "air_out_idx", "RHINITIS_out_total")]
colnames(day_rhinitis_age1) <- c("date","air_out_idx", "RHINITIS_out_total_age1")
# 여기서 subgroup, air_idx, date 별로 한번 더 aggregate 해야함!
day_rhinitis_age1 <- aggregate(day_rhinitis_age1[,"RHINITIS_out_total_age1"],
                                            by=list(day_rhinitis_age1$date,
                                                    day_rhinitis_age1$air_out_idx),sum)
colnames(day_rhinitis_age1) <- c("date","air_out_idx", "RHINITIS_out_total_age1")

# age group 2
day_rhinitis_age2 <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==2), c("date", "air_out_idx", "RHINITIS_out_total")]                                                        
colnames(day_rhinitis_age2) <- c("date","air_out_idx", "RHINITIS_out_total_age2")
day_rhinitis_age2 <- aggregate(day_rhinitis_age2[,"RHINITIS_out_total_age2"],
                                            by=list(day_rhinitis_age2$date,
                                                    day_rhinitis_age2$air_out_idx),sum)    
colnames(day_rhinitis_age2) <- c("date","air_out_idx", "RHINITIS_out_total_age2")                                                    

# age group 3
day_rhinitis_age3 <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==3), c("date", "air_out_idx", "RHINITIS_out_total")]                                                 
colnames(day_rhinitis_age3) <- c("date","air_out_idx", "RHINITIS_out_total_age3")
day_rhinitis_age3 <- aggregate(day_rhinitis_age3[,"RHINITIS_out_total_age3"],
                                            by=list(day_rhinitis_age3$date,
                                                    day_rhinitis_age3$air_out_idx),sum)  
colnames(day_rhinitis_age3) <- c("date","air_out_idx", "RHINITIS_out_total_age3")

# age group 4
day_rhinitis_age4 <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==4), c("date", "air_out_idx", "RHINITIS_out_total")]
colnames(day_rhinitis_age4) <- c("date","air_out_idx", "RHINITIS_out_total_age4")
day_rhinitis_age4 <- aggregate(day_rhinitis_age4[,"RHINITIS_out_total_age4"],
                                            by=list(day_rhinitis_age4$date,
                                                    day_rhinitis_age4$air_out_idx),sum) 
colnames(day_rhinitis_age4) <- c("date","air_out_idx", "RHINITIS_out_total_age4")

# sex m
day_rhinitis_m <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "RHINITIS_out_total")]
colnames(day_rhinitis_m) <- c("date","air_out_idx", "RHINITIS_out_total_m")
day_rhinitis_m <- aggregate(day_rhinitis_m[,"RHINITIS_out_total_m"],
                                            by=list(day_rhinitis_m$date,
                                                    day_rhinitis_m$air_out_idx),sum)  
colnames(day_rhinitis_m) <- c("date","air_out_idx", "RHINITIS_out_total_m")

# sex f 
day_rhinitis_f <- day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "RHINITIS_out_total")]
colnames(day_rhinitis_f) <- c("date","air_out_idx", "RHINITIS_out_total_f")
day_rhinitis_f <- aggregate(day_rhinitis_f[,"RHINITIS_out_total_f"],
                                            by=list(day_rhinitis_f$date,
                                                    day_rhinitis_f$air_out_idx),sum)
colnames(day_rhinitis_f) <- c("date","air_out_idx", "RHINITIS_out_total_f")                                                    

# 0728 - 이하 sex, age 로 모두 aggregate 한 그룹은 취소
  # sex m age group 1 
  day_rhinitis_m_age1<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==1&day_rhinitis_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_m_age1) <- c("date","air_out_idx", "RHINITIS_out_total_m_age1")
  day_rhinitis_m_age1 <- aggregate(day_rhinitis_m_age1[,"RHINITIS_out_total_m_age1"],
                                              by=list(day_rhinitis_m_age1$date,
                                                      day_rhinitis_m_age1$air_out_idx),sum)
  colnames(day_rhinitis_m_age1) <- c("date","air_out_idx", "RHINITIS_out_total_m_age1")                                                     

  # sex m age group 2 
  day_rhinitis_m_age2<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==2&day_rhinitis_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_m_age2) <- c("date","air_out_idx", "RHINITIS_out_total_m_age2")
  day_rhinitis_m_age2 <- aggregate(day_rhinitis_m_age2[,"RHINITIS_out_total_m_age2"],
                                              by=list(day_rhinitis_m_age2$date,
                                                      day_rhinitis_m_age2$air_out_idx),sum) 
  colnames(day_rhinitis_m_age2) <- c("date","air_out_idx", "RHINITIS_out_total_m_age2")

  # sex m age group 3 
  day_rhinitis_m_age3<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==3&day_rhinitis_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_m_age3) <- c("date","air_out_idx", "RHINITIS_out_total_m_age3")
  day_rhinitis_m_age3 <- aggregate(day_rhinitis_m_age3[,"RHINITIS_out_total_m_age3"],
                                              by=list(day_rhinitis_m_age3$date,
                                                      day_rhinitis_m_age3$air_out_idx),sum)  
  colnames(day_rhinitis_m_age3) <- c("date","air_out_idx", "RHINITIS_out_total_m_age3")

  # sex m age group 4
  day_rhinitis_m_age4<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==4&day_rhinitis_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_m_age4) <- c("date","air_out_idx", "RHINITIS_out_total_m_age4")
  day_rhinitis_m_age4 <- aggregate(day_rhinitis_m_age4[,"RHINITIS_out_total_m_age4"],
                                              by=list(day_rhinitis_m_age4$date,
                                                      day_rhinitis_m_age4$air_out_idx),sum)  
  colnames(day_rhinitis_m_age4) <- c("date","air_out_idx", "RHINITIS_out_total_m_age4")

  # sex f age group 1
  day_rhinitis_f_age1<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==1&day_rhinitis_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_f_age1) <- c("date","air_out_idx", "RHINITIS_out_total_f_age1")
  day_rhinitis_f_age1 <- aggregate(day_rhinitis_f_age1[,"RHINITIS_out_total_f_age1"],
                                              by=list(day_rhinitis_f_age1$date,
                                                      day_rhinitis_f_age1$air_out_idx),sum) 
  colnames(day_rhinitis_f_age1) <- c("date","air_out_idx", "RHINITIS_out_total_f_age1")

  # sex f age group 2
  day_rhinitis_f_age2<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==2&day_rhinitis_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_f_age2) <- c("date","air_out_idx", "RHINITIS_out_total_f_age2")
  day_rhinitis_f_age2 <- aggregate(day_rhinitis_f_age2[,"RHINITIS_out_total_f_age2"],
                                              by=list(day_rhinitis_f_age2$date,
                                                      day_rhinitis_f_age2$air_out_idx),sum)
  colnames(day_rhinitis_f_age2) <- c("date","air_out_idx", "RHINITIS_out_total_f_age2")

  # sex f age group 3
  day_rhinitis_f_age3<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==3&day_rhinitis_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_f_age3) <- c("date","air_out_idx", "RHINITIS_out_total_f_age3")
  day_rhinitis_f_age3 <- aggregate(day_rhinitis_f_age3[,"RHINITIS_out_total_f_age3"],
                                              by=list(day_rhinitis_f_age3$date,
                                                      day_rhinitis_f_age3$air_out_idx),sum) 
  colnames(day_rhinitis_f_age3) <- c("date","air_out_idx", "RHINITIS_out_total_f_age3")

  # sex f age group 4
  day_rhinitis_f_age4<-day_rhinitis_1317_agg_seoul_step[which(day_rhinitis_1317_agg_seoul_step$age_group==4&day_rhinitis_1317_agg_seoul_step$sex==2),c("date", "air_out_idx", "RHINITIS_out_total")]
  colnames(day_rhinitis_f_age4) <- c("date","air_out_idx", "RHINITIS_out_total_f_age4")
  day_rhinitis_f_age4 <- aggregate(day_rhinitis_f_age4[,"RHINITIS_out_total_f_age4"],
                                              by=list(day_rhinitis_f_age4$date,
                                                      day_rhinitis_f_age4$air_out_idx),sum) 
  colnames(day_rhinitis_f_age4) <- c("date","air_out_idx", "RHINITIS_out_total_f_age4")


# step2_final$yday <- yday(step2_final$date)
# step2_final$wday <- wday(step2_final$date)
# step2_final_back$wday=factor(step2_final_back$wday)
# step2_final_back$wday_mat=model.matrix( ~ wday - 1, data=step2_final_back)  
# summary(lm(AQI ~ AQI_back+IV_300_BI,data=step2_final_back))
# model_formula = formula(rhinitis_em_total~rain_sum+temp_mean_total +ns.basis_yday+wday_mat+AQI)
# instrument_formula =formula(AQI ~ AQI_back+IV_300_BI+rain_sum+temp_mean_total +ns.basis_yday+wday_mat)


##### IV #####
find_instruments(model_formula,instrument_formula)

day_rhinitis_1317_agg_seoul_step2<-na.omit(day_rhinitis_1317_agg_seoul_step)
day_rhinitis_1317_agg_seoul_step2$wday <- as.factor(day_rhinitis_1317_agg_seoul_step2$wday)
day_rhinitis_1317_agg_seoul_step2$age_group = factor(day_rhinitis_1317_agg_seoul_step2$age_group)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+age_group+sex+AQI_lead365 ,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_rhinitis_1317_agg_seoul_step2,family =quasipoisson, link = 'log')
summary(glm_iv)

# glm_iv$instrumented
# glm_iv$instruments
# glm_iv$exclusion_restriction
# glm_iv$instrument_validity
# glm_iv$stage_one
# diagnose(glm_iv)

# h <- day_rhinitis_1317_agg_seoul_step2[,c("RHINITIS_out_total","rain_sum","ns.basis_temp","ns.basis_yday","wday","sex","age_group","AQI","AQI_back","IV_300_BI")]
# h$wday1 = ifelse(h$wday=="1", 1, 0)
# h$wday2 = ifelse(h$wday=="2", 1, 0)
# h$wday3 = ifelse(h$wday=="3", 1, 0)
# h$wday4 = ifelse(h$wday=="4", 1, 0)
# h$wday5 = ifelse(h$wday=="5", 1, 0)
# h$wday6 = ifelse(h$wday=="6", 1, 0)
# h$wday7 = ifelse(h$wday=="7", 1, 0)
# h$age_group1 = ifelse(h$age_group=="1", 1, 0)
# h$age_group2 = ifelse(h$age_group=="2", 1, 0)
# h$age_group3 = ifelse(h$age_group=="3", 1, 0)
# h$age_group4 = ifelse(h$age_group=="4", 1, 0)
# write.csv(h,"rhinitis_iv.csv")
# stata code ===
# ivpoisson gmm asthma_out_total rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 (aqi = aqi_back iv_300_bi)

# fit_1_rhinitis <- lm(AQI~rain_sum+ns.basis_temp +ns.basis_yday+wday+age_group+sex+AQI_back+IV_300_BI,data = day_rhinitis_1317_agg_seoul_step2)
# summary(fit_1_rhinitis)

##### 황사 추가해봤지만 안하는걸로 #####
# dust <- read_excel("02_22_dust.xlsx")
# colnames(dust)[1] <- "date_yj"
# dust$date = paste0(substr(dust$date_yj,1,4),"-",substr(dust$date_yj,5,6),"-",substr(dust$date_yj,7,8))
# dust_1417 <- dust[substr(dust$date,1,4) %in% c("2014","2015","2016","2017"),]
# day_rhinitis_1317_agg_seoul_step2_dust <- merge(day_rhinitis_1317_agg_seoul_step2,dust_1417,by="date",all.x = T)
# day_rhinitis_1317_agg_seoul_step2_dust$dust_yn <- as.factor(day_rhinitis_1317_agg_seoul_step2_dust$dust_yn)
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+dust_yn+AQI,
#                  instrument_formula = AQI ~ AQI_back+IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_dust,family =quasipoisson, link = 'log')
# summary(glm_iv)

##### 습도 추가 ##### 
# hum <- weather[,c("date","air_out_idx","humi_mean_total")]
day_rhinitis_1317_agg_seoul_step2_humi <- merge(day_rhinitis_1317_agg_seoul_step2, hum, by=c("air_out_idx","date"))
day_rhinitis_1317_agg_seoul_step2_humi <- day_rhinitis_1317_agg_seoul_step2_humi[day_rhinitis_1317_agg_seoul_step2_humi$humi_mean_total.x >= 0, ]

kno_hum <- equalknots(day_rhinitis_1317_agg_seoul_step2_humi$humi_mean_total.x,nk=2)

ns.basis_hum <- crossbasis(day_rhinitis_1317_agg_seoul_step2_humi$humi_mean_total.x,argvar=list(knots=kno_hum),
                           group=day_rhinitis_1317_agg_seoul_step2_humi$air_out_idx)

day_rhinitis_1317_agg_seoul_step2_humi$ns.basis_hum <- crossbasis(day_rhinitis_1317_agg_seoul_step2_humi$humi_mean_total.x,argvar=list(knots=kno_hum),
                                                                  group=day_rhinitis_1317_agg_seoul_step2_humi$air_out_idx)

glm_iv <- iv.glm(model_formula = RHINITIS_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+ns.basis_hum+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_rhinitis_1317_agg_seoul_step2_humi,family =quasipoisson, link = 'log')
summary(glm_iv)


##### 기압, 적설, 일사, 일조 추가 #####
#asthma 와 동일 
day_rhinitis_1317_agg_seoul_step2_humi_pressure <- merge(day_rhinitis_1317_agg_seoul_step2_humi,pressure_snow_sunshine_1417,by="date",all.x=T)

kno_pressure <- equalknots(day_rhinitis_1317_agg_seoul_step2_humi_pressure$pressure_mean,nk=2)
ns.basis_pressure <- crossbasis(day_rhinitis_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                group=day_rhinitis_1317_agg_seoul_step2_humi_pressure$air_out_idx)
day_rhinitis_1317_agg_seoul_step2_humi_pressure$ns.basis_pressure <- crossbasis(day_rhinitis_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                                                                group=day_rhinitis_1317_agg_seoul_step2_humi_pressure$air_out_idx)

glm_iv <- iv.glm(model_formula = RHINITIS_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure,family =quasipoisson, link = 'log')
summary(glm_iv)
# save.image("0502.RData")
# pss1417 <- write.csv(pressure_snow_sunshine_1417,"pressure_snow_sunshine_1417.csv")

##### age sex aggregate #####
rhinitis_agg <- aggregate(day_rhinitis_1317_agg_seoul_step2_humi_pressure[,"RHINITIS_out_total"],
                        by=list(day_rhinitis_1317_agg_seoul_step2_humi_pressure$date,
                                day_rhinitis_1317_agg_seoul_step2_humi_pressure$air_out_idx),sum)
colnames(rhinitis_agg) <- c("date","air_out_idx","RHINITIS_out_total_agg")
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg <- merge(rhinitis_agg,day_rhinitis_1317_agg_seoul_step2_humi_pressure,  by=c("date","air_out_idx"),all.x = T)

air_dups <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[c("air_out_idx", "date")]
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[!duplicated(air_dups),]

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# 0718 humidity < 0 삭제 후 다시 save
# save.image("220718.RData")



# 0719 melbourne 데이터 추가
mel_rhinitis <- merge(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg, mel_final, by=c('date'), all.x = T)
# mel IV 붙이지 말고 AQI만 쓰자
mel_rhinitis <- merge(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg, mel_air_not_na, by=c('date'), all.x = T)

dim(mel_rhinitis) # 33206
colnames(mel_rhinitis)
mel_rhinitis_not_na <- na.omit(mel_rhinitis)
dim(mel_rhinitis_not_na) # 32453
# mel_rhinitis_not_na$sin <- sin(2 * 3.14/365.25 * mel_rhinitis_not_na$yday)
# mel_rhinitis_not_na$cos <- cos(2 * 3.14/365.25 * mel_rhinitis_not_na$yday)

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_mel,
                 instrument_formula = AQI_mel ~ IV_300_mel,
                 data=mel_rhinitis_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum + ns.basis_pressure + sin+cos+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=mel_rhinitis_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum + ns.basis_pressure + sin+cos+AQI + AQI_mel,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=mel_rhinitis_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)

tail(mel_rhinitis_not_na)
cbind(lapply(lapply(mel_rhinitis_not_na, is.na), sum))
summary(mel_rhinitis_not_na$AQI)
summary(mel_rhinitis_not_na$AQI_mel)





# melbourne data lag 추가
create_mel_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_mel_lag1=lag(AQI_mel)) %>%
    mutate(AQI_mel_lag2=lag(AQI_mel,2)) %>%
    mutate(AQI_mel_lag3=lag(AQI_mel,3)) %>%
    mutate(AQI_mel_lag4=lag(AQI_mel,4)) %>%
    mutate(AQI_mel_lag5=lag(AQI_mel,5)) %>%
    mutate(AQI_mel_lag6=lag(AQI_mel,6)) %>%
    mutate(AQI_mel_lag7=lag(AQI_mel,7)) %>%
    mutate(AQI_mel_lag8=lag(AQI_mel,8)) %>%
    mutate(AQI_mel_lag9=lag(AQI_mel,9)) %>%
    mutate(AQI_mel_lag10=lag(AQI_mel,10)) %>%
    mutate(AQI_mel_lag11=lag(AQI_mel,11)) %>%
    mutate(AQI_mel_lag12=lag(AQI_mel,12)) %>%
    mutate(AQI_mel_lag13=lag(AQI_mel,13)) %>%
    mutate(AQI_mel_lag14=lag(AQI_mel,14)) %>%
    mutate(AQI_mel_lag15=lag(AQI_mel,15)) %>%
    mutate(AQI_mel_lag16=lag(AQI_mel,16)) %>%
    mutate(AQI_mel_lag17=lag(AQI_mel,17)) %>%
    mutate(AQI_mel_lag18=lag(AQI_mel,18)) %>%
    mutate(AQI_mel_lag19=lag(AQI_mel,19)) %>%
    mutate(AQI_mel_lag20=lag(AQI_mel,20)) %>%
    mutate(AQI_mel_lag21=lag(AQI_mel,21)) %>%
    mutate(AQI_mel_lag22=lag(AQI_mel,22)) %>%
    mutate(AQI_mel_lag23=lag(AQI_mel,23)) %>%
    mutate(AQI_mel_lag24=lag(AQI_mel,24)) %>%
    mutate(AQI_mel_lag25=lag(AQI_mel,25)) %>%
    mutate(AQI_mel_lag26=lag(AQI_mel,26)) %>%
    mutate(AQI_mel_lag27=lag(AQI_mel,27)) %>%
    mutate(AQI_mel_lag28=lag(AQI_mel,28)) %>%
    mutate(AQI_mel_lag29=lag(AQI_mel,29)) %>%
    mutate(AQI_mel_lag30=lag(AQI_mel,30)) %>%

    # mutate(IV_300_mel_lag1=lag(IV_300_mel)) %>%
    # mutate(IV_300_mel_lag2=lag(IV_300_mel,2)) %>%
    # mutate(IV_300_mel_lag3=lag(IV_300_mel,3)) %>%
    # mutate(IV_300_mel_lag4=lag(IV_300_mel,4)) %>%
    # mutate(IV_300_mel_lag5=lag(IV_300_mel,5)) %>%
    # mutate(IV_300_mel_lag6=lag(IV_300_mel,6)) %>%
    # mutate(IV_300_mel_lag7=lag(IV_300_mel,7)) %>%
    # mutate(IV_300_mel_lag8=lag(IV_300_mel,8)) %>%
    # mutate(IV_300_mel_lag9=lag(IV_300_mel,9)) %>%
    # mutate(IV_300_mel_lag10=lag(IV_300_mel,10)) %>%
    # mutate(IV_300_mel_lag11=lag(IV_300_mel,11)) %>%
    # mutate(IV_300_mel_lag12=lag(IV_300_mel,12)) %>%
    # mutate(IV_300_mel_lag13=lag(IV_300_mel,13)) %>%
    # mutate(IV_300_mel_lag14=lag(IV_300_mel,14)) %>%
    # mutate(IV_300_mel_lag15=lag(IV_300_mel,15)) %>%
    # mutate(IV_300_mel_lag16=lag(IV_300_mel,16)) %>%
    # mutate(IV_300_mel_lag17=lag(IV_300_mel,17)) %>%
    # mutate(IV_300_mel_lag18=lag(IV_300_mel,18)) %>%
    # mutate(IV_300_mel_lag19=lag(IV_300_mel,19)) %>%
    # mutate(IV_300_mel_lag20=lag(IV_300_mel,20)) %>%
    # mutate(IV_300_mel_lag21=lag(IV_300_mel,21)) %>%
    # mutate(IV_300_mel_lag22=lag(IV_300_mel,22)) %>%
    # mutate(IV_300_mel_lag23=lag(IV_300_mel,23)) %>%
    # mutate(IV_300_mel_lag24=lag(IV_300_mel,24)) %>%
    # mutate(IV_300_mel_lag25=lag(IV_300_mel,25)) %>%
    # mutate(IV_300_mel_lag26=lag(IV_300_mel,26)) %>%
    # mutate(IV_300_mel_lag27=lag(IV_300_mel,27)) %>%
    # mutate(IV_300_mel_lag28=lag(IV_300_mel,28)) %>%
    # mutate(IV_300_mel_lag29=lag(IV_300_mel,29)) %>%
    # mutate(IV_300_mel_lag30=lag(IV_300_mel,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
    mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
    mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
    mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
    mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
    mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
    mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
    mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
    mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
    mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%
    mutate(IV_300_BI_lag11=lag(IV_300_BI,11)) %>%
    mutate(IV_300_BI_lag12=lag(IV_300_BI,12)) %>%
    mutate(IV_300_BI_lag13=lag(IV_300_BI,13)) %>%
    mutate(IV_300_BI_lag14=lag(IV_300_BI,14)) %>%
    mutate(IV_300_BI_lag15=lag(IV_300_BI,15)) %>%
    mutate(IV_300_BI_lag16=lag(IV_300_BI,16)) %>%
    mutate(IV_300_BI_lag17=lag(IV_300_BI,17)) %>%
    mutate(IV_300_BI_lag18=lag(IV_300_BI,18)) %>%
    mutate(IV_300_BI_lag19=lag(IV_300_BI,19)) %>%
    mutate(IV_300_BI_lag20=lag(IV_300_BI,20)) %>%
    mutate(IV_300_BI_lag21=lag(IV_300_BI,21)) %>%
    mutate(IV_300_BI_lag22=lag(IV_300_BI,22)) %>%
    mutate(IV_300_BI_lag23=lag(IV_300_BI,23)) %>%
    mutate(IV_300_BI_lag24=lag(IV_300_BI,24)) %>%
    mutate(IV_300_BI_lag25=lag(IV_300_BI,25)) %>%
    mutate(IV_300_BI_lag26=lag(IV_300_BI,26)) %>%
    mutate(IV_300_BI_lag27=lag(IV_300_BI,27)) %>%
    mutate(IV_300_BI_lag28=lag(IV_300_BI,28)) %>%
    mutate(IV_300_BI_lag29=lag(IV_300_BI,29)) %>%
    mutate(IV_300_BI_lag30=lag(IV_300_BI,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", "AQI", "IV_300_BI", 
          "AQI_mel","AQI_mel_lag1","AQI_mel_lag2","AQI_mel_lag3","AQI_mel_lag4","AQI_mel_lag5","AQI_mel_lag6","AQI_mel_lag7",
          # "IV_300_mel","IV_300_mel_lag1","IV_300_mel_lag2","IV_300_mel_lag3","IV_300_mel_lag4","IV_300_mel_lag5","IV_300_mel_lag6","IV_300_mel_lag7", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}


# create lag
mel_lag <- create_mel_lag(mel_rhinitis_not_na)
mel_lag <- create_lag(mel_lag)

# lag 0
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel) == F,]
h <- h[is.na(h$AQI) == F,]
summary(h$AQI_mel)
summary(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_mel)
h <- na.omit(h)
dim(h) # 32278 40
# write.csv(h,"thermal_inversion_0623_share\\dump\\rhinitis_iv_neg_control_lag0_test.csv") # 0715 test 
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag0.csv")

# lag 1
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag1) == F,]
h <- h[is.na(h$AQI_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag1.csv")

# lag 2
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag2) == F,]
h <- h[is.na(h$AQI_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag2.csv")

# lag 3
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag3) == F,]
h <- h[is.na(h$AQI_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag3.csv")

# lag 4
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag4) == F,]
h <- h[is.na(h$AQI_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag4.csv")

# lag 5
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag5) == F,]
h <- h[is.na(h$AQI_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag5.csv")

# lag 6
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag6) == F,]
h <- h[is.na(h$AQI_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag6.csv")

# lag 7
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag7) == F,]
h <- h[is.na(h$AQI_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_melbourne_lag7.csv")






# 0714 365 lead 도 lag 0~7 추가 - zio

dim(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg) # 33206 55
dim(rhinitis_lead_365) # 34586 67

day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365 <- merge(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg, rhinitis_lead_365[, c('air_out_idx', 'date', 'AQI_lead365')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365)
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365 <- na.omit(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365)
dim(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365)

create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
    mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
    mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
    mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
    mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
    mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
    mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
    mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
    mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
    mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%
    mutate(IV_300_BI_lag11=lag(IV_300_BI,11)) %>%
    mutate(IV_300_BI_lag12=lag(IV_300_BI,12)) %>%
    mutate(IV_300_BI_lag13=lag(IV_300_BI,13)) %>%
    mutate(IV_300_BI_lag14=lag(IV_300_BI,14)) %>%
    mutate(IV_300_BI_lag15=lag(IV_300_BI,15)) %>%
    mutate(IV_300_BI_lag16=lag(IV_300_BI,16)) %>%
    mutate(IV_300_BI_lag17=lag(IV_300_BI,17)) %>%
    mutate(IV_300_BI_lag18=lag(IV_300_BI,18)) %>%
    mutate(IV_300_BI_lag19=lag(IV_300_BI,19)) %>%
    mutate(IV_300_BI_lag20=lag(IV_300_BI,20)) %>%
    mutate(IV_300_BI_lag21=lag(IV_300_BI,21)) %>%
    mutate(IV_300_BI_lag22=lag(IV_300_BI,22)) %>%
    mutate(IV_300_BI_lag23=lag(IV_300_BI,23)) %>%
    mutate(IV_300_BI_lag24=lag(IV_300_BI,24)) %>%
    mutate(IV_300_BI_lag25=lag(IV_300_BI,25)) %>%
    mutate(IV_300_BI_lag26=lag(IV_300_BI,26)) %>%
    mutate(IV_300_BI_lag27=lag(IV_300_BI,27)) %>%
    mutate(IV_300_BI_lag28=lag(IV_300_BI,28)) %>%
    mutate(IV_300_BI_lag29=lag(IV_300_BI,29)) %>%
    mutate(IV_300_BI_lag30=lag(IV_300_BI,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_lag_365 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lead365_lag1=lag(AQI_lead365)) %>%
    mutate(AQI_lead365_lag2=lag(AQI_lead365,2)) %>%
    mutate(AQI_lead365_lag3=lag(AQI_lead365,3)) %>%
    mutate(AQI_lead365_lag4=lag(AQI_lead365,4)) %>%
    mutate(AQI_lead365_lag5=lag(AQI_lead365,5)) %>%
    mutate(AQI_lead365_lag6=lag(AQI_lead365,6)) %>%
    mutate(AQI_lead365_lag7=lag(AQI_lead365,7)) %>%
    mutate(AQI_lead365_lag8=lag(AQI_lead365,8)) %>%
    mutate(AQI_lead365_lag9=lag(AQI_lead365,9)) %>%
    mutate(AQI_lead365_lag10=lag(AQI_lead365,10)) %>%
    mutate(AQI_lead365_lag11=lag(AQI_lead365,11)) %>%
    mutate(AQI_lead365_lag12=lag(AQI_lead365,12)) %>%
    mutate(AQI_lead365_lag13=lag(AQI_lead365,13)) %>%
    mutate(AQI_lead365_lag14=lag(AQI_lead365,14)) %>%
    mutate(AQI_lead365_lag15=lag(AQI_lead365,15)) %>%
    mutate(AQI_lead365_lag16=lag(AQI_lead365,16)) %>%
    mutate(AQI_lead365_lag17=lag(AQI_lead365,17)) %>%
    mutate(AQI_lead365_lag18=lag(AQI_lead365,18)) %>%
    mutate(AQI_lead365_lag19=lag(AQI_lead365,19)) %>%
    mutate(AQI_lead365_lag20=lag(AQI_lead365,20)) %>%
    mutate(AQI_lead365_lag21=lag(AQI_lead365,21)) %>%
    mutate(AQI_lead365_lag22=lag(AQI_lead365,22)) %>%
    mutate(AQI_lead365_lag23=lag(AQI_lead365,23)) %>%
    mutate(AQI_lead365_lag24=lag(AQI_lead365,24)) %>%
    mutate(AQI_lead365_lag25=lag(AQI_lead365,25)) %>%
    mutate(AQI_lead365_lag26=lag(AQI_lead365,26)) %>%
    mutate(AQI_lead365_lag27=lag(AQI_lead365,27)) %>%
    mutate(AQI_lead365_lag28=lag(AQI_lead365,28)) %>%
    mutate(AQI_lead365_lag29=lag(AQI_lead365,29)) %>%
    mutate(AQI_lead365_lag30=lag(AQI_lead365,30)) %>%

    # mutate(IV_300_BI_lead365_lag1=lag(IV_300_BI_lead365)) %>%
    # mutate(IV_300_BI_lead365_lag2=lag(IV_300_BI_lead365,2)) %>%
    # mutate(IV_300_BI_lead365_lag3=lag(IV_300_BI_lead365,3)) %>%
    # mutate(IV_300_BI_lead365_lag4=lag(IV_300_BI_lead365,4)) %>%
    # mutate(IV_300_BI_lead365_lag5=lag(IV_300_BI_lead365,5)) %>%
    # mutate(IV_300_BI_lead365_lag6=lag(IV_300_BI_lead365,6)) %>%
    # mutate(IV_300_BI_lead365_lag7=lag(IV_300_BI_lead365,7)) %>%
    # mutate(IV_300_BI_lead365_lag8=lag(IV_300_BI_lead365,8)) %>%
    # mutate(IV_300_BI_lead365_lag9=lag(IV_300_BI_lead365,9)) %>%
    # mutate(IV_300_BI_lead365_lag10=lag(IV_300_BI_lead365,10)) %>%
    # mutate(IV_300_BI_lead365_lag11=lag(IV_300_BI_lead365,11)) %>%
    # mutate(IV_300_BI_lead365_lag12=lag(IV_300_BI_lead365,12)) %>%
    # mutate(IV_300_BI_lead365_lag13=lag(IV_300_BI_lead365,13)) %>%
    # mutate(IV_300_BI_lead365_lag14=lag(IV_300_BI_lead365,14)) %>%
    # mutate(IV_300_BI_lead365_lag15=lag(IV_300_BI_lead365,15)) %>%
    # mutate(IV_300_BI_lead365_lag16=lag(IV_300_BI_lead365,16)) %>%
    # mutate(IV_300_BI_lead365_lag17=lag(IV_300_BI_lead365,17)) %>%
    # mutate(IV_300_BI_lead365_lag18=lag(IV_300_BI_lead365,18)) %>%
    # mutate(IV_300_BI_lead365_lag19=lag(IV_300_BI_lead365,19)) %>%
    # mutate(IV_300_BI_lead365_lag20=lag(IV_300_BI_lead365,20)) %>%
    # mutate(IV_300_BI_lead365_lag21=lag(IV_300_BI_lead365,21)) %>%
    # mutate(IV_300_BI_lead365_lag22=lag(IV_300_BI_lead365,22)) %>%
    # mutate(IV_300_BI_lead365_lag23=lag(IV_300_BI_lead365,23)) %>%
    # mutate(IV_300_BI_lead365_lag24=lag(IV_300_BI_lead365,24)) %>%
    # mutate(IV_300_BI_lead365_lag25=lag(IV_300_BI_lead365,25)) %>%
    # mutate(IV_300_BI_lead365_lag26=lag(IV_300_BI_lead365,26)) %>%
    # mutate(IV_300_BI_lead365_lag27=lag(IV_300_BI_lead365,27)) %>%
    # mutate(IV_300_BI_lead365_lag28=lag(IV_300_BI_lead365,28)) %>%
    # mutate(IV_300_BI_lead365_lag29=lag(IV_300_BI_lead365,29)) %>%
    # mutate(IV_300_BI_lead365_lag30=lag(IV_300_BI_lead365,30)) %>%

    ungroup()

  return (df_with_lag)
}

create_h_365 <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "AQI_lead365","AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7"
          # "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7"
          )]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

# create_h_with_index <- function(df) {
#   h <- df[,c("air_out_idx", "date", "rhinitis_out_total_agg", 
#           "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
#           "AQI_lead365","AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
#           "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")]
#   h$wday1 = ifelse(h$wday=="1", 1, 0)
#   h$wday2 = ifelse(h$wday=="2", 1, 0)
#   h$wday3 = ifelse(h$wday=="3", 1, 0)
#   h$wday4 = ifelse(h$wday=="4", 1, 0)
#   h$wday5 = ifelse(h$wday=="5", 1, 0)
#   h$wday6 = ifelse(h$wday=="6", 1, 0)
#   h$wday7 = ifelse(h$wday=="7", 1, 0)

#   return (h)
# }


# create lag
lead365_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_365)
lead365_lag <- create_lag_365(lead365_lag)

# lag 0
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365) == F,]
summary(h$AQI_lead365)
summary(h)
dim(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_lead365)
h <- na.omit(h)
dim(h) # 30679 38
# write.csv(h,"thermal_inversion_0623_share\\dump\\rhinitis_iv_neg_control_lag0_test.csv") # 0715 test 
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag0.csv")

# lag 1
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag1.csv")

# lag 2
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag2.csv")

# lag 3
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag3.csv")

# lag 4
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag4.csv")

# lag 5
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag5.csv")

# lag 6
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag6.csv")

# lag 7
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_365_neg_control_lag7.csv")




# 0714 182 lead 추가 - zio

create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lead182_lag1=lag(AQI_lead182)) %>%
    mutate(AQI_lead182_lag2=lag(AQI_lead182,2)) %>%
    mutate(AQI_lead182_lag3=lag(AQI_lead182,3)) %>%
    mutate(AQI_lead182_lag4=lag(AQI_lead182,4)) %>%
    mutate(AQI_lead182_lag5=lag(AQI_lead182,5)) %>%
    mutate(AQI_lead182_lag6=lag(AQI_lead182,6)) %>%
    mutate(AQI_lead182_lag7=lag(AQI_lead182,7)) %>%
    mutate(AQI_lead182_lag8=lag(AQI_lead182,8)) %>%
    mutate(AQI_lead182_lag9=lag(AQI_lead182,9)) %>%
    mutate(AQI_lead182_lag10=lag(AQI_lead182,10)) %>%
    mutate(AQI_lead182_lag11=lag(AQI_lead182,11)) %>%
    mutate(AQI_lead182_lag12=lag(AQI_lead182,12)) %>%
    mutate(AQI_lead182_lag13=lag(AQI_lead182,13)) %>%
    mutate(AQI_lead182_lag14=lag(AQI_lead182,14)) %>%
    mutate(AQI_lead182_lag15=lag(AQI_lead182,15)) %>%
    mutate(AQI_lead182_lag16=lag(AQI_lead182,16)) %>%
    mutate(AQI_lead182_lag17=lag(AQI_lead182,17)) %>%
    mutate(AQI_lead182_lag18=lag(AQI_lead182,18)) %>%
    mutate(AQI_lead182_lag19=lag(AQI_lead182,19)) %>%
    mutate(AQI_lead182_lag20=lag(AQI_lead182,20)) %>%
    mutate(AQI_lead182_lag21=lag(AQI_lead182,21)) %>%
    mutate(AQI_lead182_lag22=lag(AQI_lead182,22)) %>%
    mutate(AQI_lead182_lag23=lag(AQI_lead182,23)) %>%
    mutate(AQI_lead182_lag24=lag(AQI_lead182,24)) %>%
    mutate(AQI_lead182_lag25=lag(AQI_lead182,25)) %>%
    mutate(AQI_lead182_lag26=lag(AQI_lead182,26)) %>%
    mutate(AQI_lead182_lag27=lag(AQI_lead182,27)) %>%
    mutate(AQI_lead182_lag28=lag(AQI_lead182,28)) %>%
    mutate(AQI_lead182_lag29=lag(AQI_lead182,29)) %>%
    mutate(AQI_lead182_lag30=lag(AQI_lead182,30)) %>%

    mutate(IV_300_BI_lead182_lag1=lag(IV_300_BI_lead182)) %>%
    mutate(IV_300_BI_lead182_lag2=lag(IV_300_BI_lead182,2)) %>%
    mutate(IV_300_BI_lead182_lag3=lag(IV_300_BI_lead182,3)) %>%
    mutate(IV_300_BI_lead182_lag4=lag(IV_300_BI_lead182,4)) %>%
    mutate(IV_300_BI_lead182_lag5=lag(IV_300_BI_lead182,5)) %>%
    mutate(IV_300_BI_lead182_lag6=lag(IV_300_BI_lead182,6)) %>%
    mutate(IV_300_BI_lead182_lag7=lag(IV_300_BI_lead182,7)) %>%
    mutate(IV_300_BI_lead182_lag8=lag(IV_300_BI_lead182,8)) %>%
    mutate(IV_300_BI_lead182_lag9=lag(IV_300_BI_lead182,9)) %>%
    mutate(IV_300_BI_lead182_lag10=lag(IV_300_BI_lead182,10)) %>%
    mutate(IV_300_BI_lead182_lag11=lag(IV_300_BI_lead182,11)) %>%
    mutate(IV_300_BI_lead182_lag12=lag(IV_300_BI_lead182,12)) %>%
    mutate(IV_300_BI_lead182_lag13=lag(IV_300_BI_lead182,13)) %>%
    mutate(IV_300_BI_lead182_lag14=lag(IV_300_BI_lead182,14)) %>%
    mutate(IV_300_BI_lead182_lag15=lag(IV_300_BI_lead182,15)) %>%
    mutate(IV_300_BI_lead182_lag16=lag(IV_300_BI_lead182,16)) %>%
    mutate(IV_300_BI_lead182_lag17=lag(IV_300_BI_lead182,17)) %>%
    mutate(IV_300_BI_lead182_lag18=lag(IV_300_BI_lead182,18)) %>%
    mutate(IV_300_BI_lead182_lag19=lag(IV_300_BI_lead182,19)) %>%
    mutate(IV_300_BI_lead182_lag20=lag(IV_300_BI_lead182,20)) %>%
    mutate(IV_300_BI_lead182_lag21=lag(IV_300_BI_lead182,21)) %>%
    mutate(IV_300_BI_lead182_lag22=lag(IV_300_BI_lead182,22)) %>%
    mutate(IV_300_BI_lead182_lag23=lag(IV_300_BI_lead182,23)) %>%
    mutate(IV_300_BI_lead182_lag24=lag(IV_300_BI_lead182,24)) %>%
    mutate(IV_300_BI_lead182_lag25=lag(IV_300_BI_lead182,25)) %>%
    mutate(IV_300_BI_lead182_lag26=lag(IV_300_BI_lead182,26)) %>%
    mutate(IV_300_BI_lead182_lag27=lag(IV_300_BI_lead182,27)) %>%
    mutate(IV_300_BI_lead182_lag28=lag(IV_300_BI_lead182,28)) %>%
    mutate(IV_300_BI_lead182_lag29=lag(IV_300_BI_lead182,29)) %>%
    mutate(IV_300_BI_lead182_lag30=lag(IV_300_BI_lead182,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI_lead182","AQI_lead182_lag1","AQI_lead182_lag2","AQI_lead182_lag3","AQI_lead182_lag4","AQI_lead182_lag5","AQI_lead182_lag6","AQI_lead182_lag7",
          "IV_300_BI_lead182","IV_300_BI_lead182_lag1","IV_300_BI_lead182_lag2","IV_300_BI_lead182_lag3","IV_300_BI_lead182_lag4","IV_300_BI_lead182_lag5","IV_300_BI_lead182_lag6","IV_300_BI_lead182_lag7")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}


# create lag
lead182_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)

# lag 0
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182) == F,]
summary(h$AQI_lead182)
IQR(h$AQI_lead182)
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag0.csv")

# lag 1
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag1.csv")

# lag 2
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag2.csv")

# lag 3
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag3.csv")

# lag 4
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag4.csv")

# lag 5
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag5.csv")

# lag 6
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag6.csv")

# lag 7
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_182_lag7.csv")




# 0714 92 lead 추가 - zio

create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lead92_lag1=lag(AQI_lead92)) %>%
    mutate(AQI_lead92_lag2=lag(AQI_lead92,2)) %>%
    mutate(AQI_lead92_lag3=lag(AQI_lead92,3)) %>%
    mutate(AQI_lead92_lag4=lag(AQI_lead92,4)) %>%
    mutate(AQI_lead92_lag5=lag(AQI_lead92,5)) %>%
    mutate(AQI_lead92_lag6=lag(AQI_lead92,6)) %>%
    mutate(AQI_lead92_lag7=lag(AQI_lead92,7)) %>%
    mutate(AQI_lead92_lag8=lag(AQI_lead92,8)) %>%
    mutate(AQI_lead92_lag9=lag(AQI_lead92,9)) %>%
    mutate(AQI_lead92_lag10=lag(AQI_lead92,10)) %>%
    mutate(AQI_lead92_lag11=lag(AQI_lead92,11)) %>%
    mutate(AQI_lead92_lag12=lag(AQI_lead92,12)) %>%
    mutate(AQI_lead92_lag13=lag(AQI_lead92,13)) %>%
    mutate(AQI_lead92_lag14=lag(AQI_lead92,14)) %>%
    mutate(AQI_lead92_lag15=lag(AQI_lead92,15)) %>%
    mutate(AQI_lead92_lag16=lag(AQI_lead92,16)) %>%
    mutate(AQI_lead92_lag17=lag(AQI_lead92,17)) %>%
    mutate(AQI_lead92_lag18=lag(AQI_lead92,18)) %>%
    mutate(AQI_lead92_lag19=lag(AQI_lead92,19)) %>%
    mutate(AQI_lead92_lag20=lag(AQI_lead92,20)) %>%
    mutate(AQI_lead92_lag21=lag(AQI_lead92,21)) %>%
    mutate(AQI_lead92_lag22=lag(AQI_lead92,22)) %>%
    mutate(AQI_lead92_lag23=lag(AQI_lead92,23)) %>%
    mutate(AQI_lead92_lag24=lag(AQI_lead92,24)) %>%
    mutate(AQI_lead92_lag25=lag(AQI_lead92,25)) %>%
    mutate(AQI_lead92_lag26=lag(AQI_lead92,26)) %>%
    mutate(AQI_lead92_lag27=lag(AQI_lead92,27)) %>%
    mutate(AQI_lead92_lag28=lag(AQI_lead92,28)) %>%
    mutate(AQI_lead92_lag29=lag(AQI_lead92,29)) %>%
    mutate(AQI_lead92_lag30=lag(AQI_lead92,30)) %>%

    mutate(IV_300_BI_lead92_lag1=lag(IV_300_BI_lead92)) %>%
    mutate(IV_300_BI_lead92_lag2=lag(IV_300_BI_lead92,2)) %>%
    mutate(IV_300_BI_lead92_lag3=lag(IV_300_BI_lead92,3)) %>%
    mutate(IV_300_BI_lead92_lag4=lag(IV_300_BI_lead92,4)) %>%
    mutate(IV_300_BI_lead92_lag5=lag(IV_300_BI_lead92,5)) %>%
    mutate(IV_300_BI_lead92_lag6=lag(IV_300_BI_lead92,6)) %>%
    mutate(IV_300_BI_lead92_lag7=lag(IV_300_BI_lead92,7)) %>%
    mutate(IV_300_BI_lead92_lag8=lag(IV_300_BI_lead92,8)) %>%
    mutate(IV_300_BI_lead92_lag9=lag(IV_300_BI_lead92,9)) %>%
    mutate(IV_300_BI_lead92_lag10=lag(IV_300_BI_lead92,10)) %>%
    mutate(IV_300_BI_lead92_lag11=lag(IV_300_BI_lead92,11)) %>%
    mutate(IV_300_BI_lead92_lag12=lag(IV_300_BI_lead92,12)) %>%
    mutate(IV_300_BI_lead92_lag13=lag(IV_300_BI_lead92,13)) %>%
    mutate(IV_300_BI_lead92_lag14=lag(IV_300_BI_lead92,14)) %>%
    mutate(IV_300_BI_lead92_lag15=lag(IV_300_BI_lead92,15)) %>%
    mutate(IV_300_BI_lead92_lag16=lag(IV_300_BI_lead92,16)) %>%
    mutate(IV_300_BI_lead92_lag17=lag(IV_300_BI_lead92,17)) %>%
    mutate(IV_300_BI_lead92_lag18=lag(IV_300_BI_lead92,18)) %>%
    mutate(IV_300_BI_lead92_lag19=lag(IV_300_BI_lead92,19)) %>%
    mutate(IV_300_BI_lead92_lag20=lag(IV_300_BI_lead92,20)) %>%
    mutate(IV_300_BI_lead92_lag21=lag(IV_300_BI_lead92,21)) %>%
    mutate(IV_300_BI_lead92_lag22=lag(IV_300_BI_lead92,22)) %>%
    mutate(IV_300_BI_lead92_lag23=lag(IV_300_BI_lead92,23)) %>%
    mutate(IV_300_BI_lead92_lag24=lag(IV_300_BI_lead92,24)) %>%
    mutate(IV_300_BI_lead92_lag25=lag(IV_300_BI_lead92,25)) %>%
    mutate(IV_300_BI_lead92_lag26=lag(IV_300_BI_lead92,26)) %>%
    mutate(IV_300_BI_lead92_lag27=lag(IV_300_BI_lead92,27)) %>%
    mutate(IV_300_BI_lead92_lag28=lag(IV_300_BI_lead92,28)) %>%
    mutate(IV_300_BI_lead92_lag29=lag(IV_300_BI_lead92,29)) %>%
    mutate(IV_300_BI_lead92_lag30=lag(IV_300_BI_lead92,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI_lead92","AQI_lead92_lag1","AQI_lead92_lag2","AQI_lead92_lag3","AQI_lead92_lag4","AQI_lead92_lag5","AQI_lead92_lag6","AQI_lead92_lag7",
          "IV_300_BI_lead92","IV_300_BI_lead92_lag1","IV_300_BI_lead92_lag2","IV_300_BI_lead92_lag3","IV_300_BI_lead92_lag4","IV_300_BI_lead92_lag5","IV_300_BI_lead92_lag6","IV_300_BI_lead92_lag7")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}


# create lag
lead92_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)

# lag 0
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92) == F,]
summary(h$AQI_lead92)
IQR(h$AQI_lead92)
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag0.csv")

# lag 1
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag1.csv")

# lag 2
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag2.csv")

# lag 3
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag3.csv")

# lag 4
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag4.csv")

# lag 5
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag5.csv")

# lag 6
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag6.csv")

# lag 7
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_92_lag7.csv")





# 0714 730 lead 추가 - zio

create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lead730_lag1=lag(AQI_lead730)) %>%
    mutate(AQI_lead730_lag2=lag(AQI_lead730,2)) %>%
    mutate(AQI_lead730_lag3=lag(AQI_lead730,3)) %>%
    mutate(AQI_lead730_lag4=lag(AQI_lead730,4)) %>%
    mutate(AQI_lead730_lag5=lag(AQI_lead730,5)) %>%
    mutate(AQI_lead730_lag6=lag(AQI_lead730,6)) %>%
    mutate(AQI_lead730_lag7=lag(AQI_lead730,7)) %>%
    mutate(AQI_lead730_lag8=lag(AQI_lead730,8)) %>%
    mutate(AQI_lead730_lag9=lag(AQI_lead730,9)) %>%
    mutate(AQI_lead730_lag10=lag(AQI_lead730,10)) %>%
    mutate(AQI_lead730_lag11=lag(AQI_lead730,11)) %>%
    mutate(AQI_lead730_lag12=lag(AQI_lead730,12)) %>%
    mutate(AQI_lead730_lag13=lag(AQI_lead730,13)) %>%
    mutate(AQI_lead730_lag14=lag(AQI_lead730,14)) %>%
    mutate(AQI_lead730_lag15=lag(AQI_lead730,15)) %>%
    mutate(AQI_lead730_lag16=lag(AQI_lead730,16)) %>%
    mutate(AQI_lead730_lag17=lag(AQI_lead730,17)) %>%
    mutate(AQI_lead730_lag18=lag(AQI_lead730,18)) %>%
    mutate(AQI_lead730_lag19=lag(AQI_lead730,19)) %>%
    mutate(AQI_lead730_lag20=lag(AQI_lead730,20)) %>%
    mutate(AQI_lead730_lag21=lag(AQI_lead730,21)) %>%
    mutate(AQI_lead730_lag22=lag(AQI_lead730,22)) %>%
    mutate(AQI_lead730_lag23=lag(AQI_lead730,23)) %>%
    mutate(AQI_lead730_lag24=lag(AQI_lead730,24)) %>%
    mutate(AQI_lead730_lag25=lag(AQI_lead730,25)) %>%
    mutate(AQI_lead730_lag26=lag(AQI_lead730,26)) %>%
    mutate(AQI_lead730_lag27=lag(AQI_lead730,27)) %>%
    mutate(AQI_lead730_lag28=lag(AQI_lead730,28)) %>%
    mutate(AQI_lead730_lag29=lag(AQI_lead730,29)) %>%
    mutate(AQI_lead730_lag30=lag(AQI_lead730,30)) %>%

    mutate(IV_300_BI_lead730_lag1=lag(IV_300_BI_lead730)) %>%
    mutate(IV_300_BI_lead730_lag2=lag(IV_300_BI_lead730,2)) %>%
    mutate(IV_300_BI_lead730_lag3=lag(IV_300_BI_lead730,3)) %>%
    mutate(IV_300_BI_lead730_lag4=lag(IV_300_BI_lead730,4)) %>%
    mutate(IV_300_BI_lead730_lag5=lag(IV_300_BI_lead730,5)) %>%
    mutate(IV_300_BI_lead730_lag6=lag(IV_300_BI_lead730,6)) %>%
    mutate(IV_300_BI_lead730_lag7=lag(IV_300_BI_lead730,7)) %>%
    mutate(IV_300_BI_lead730_lag8=lag(IV_300_BI_lead730,8)) %>%
    mutate(IV_300_BI_lead730_lag9=lag(IV_300_BI_lead730,9)) %>%
    mutate(IV_300_BI_lead730_lag10=lag(IV_300_BI_lead730,10)) %>%
    mutate(IV_300_BI_lead730_lag11=lag(IV_300_BI_lead730,11)) %>%
    mutate(IV_300_BI_lead730_lag12=lag(IV_300_BI_lead730,12)) %>%
    mutate(IV_300_BI_lead730_lag13=lag(IV_300_BI_lead730,13)) %>%
    mutate(IV_300_BI_lead730_lag14=lag(IV_300_BI_lead730,14)) %>%
    mutate(IV_300_BI_lead730_lag15=lag(IV_300_BI_lead730,15)) %>%
    mutate(IV_300_BI_lead730_lag16=lag(IV_300_BI_lead730,16)) %>%
    mutate(IV_300_BI_lead730_lag17=lag(IV_300_BI_lead730,17)) %>%
    mutate(IV_300_BI_lead730_lag18=lag(IV_300_BI_lead730,18)) %>%
    mutate(IV_300_BI_lead730_lag19=lag(IV_300_BI_lead730,19)) %>%
    mutate(IV_300_BI_lead730_lag20=lag(IV_300_BI_lead730,20)) %>%
    mutate(IV_300_BI_lead730_lag21=lag(IV_300_BI_lead730,21)) %>%
    mutate(IV_300_BI_lead730_lag22=lag(IV_300_BI_lead730,22)) %>%
    mutate(IV_300_BI_lead730_lag23=lag(IV_300_BI_lead730,23)) %>%
    mutate(IV_300_BI_lead730_lag24=lag(IV_300_BI_lead730,24)) %>%
    mutate(IV_300_BI_lead730_lag25=lag(IV_300_BI_lead730,25)) %>%
    mutate(IV_300_BI_lead730_lag26=lag(IV_300_BI_lead730,26)) %>%
    mutate(IV_300_BI_lead730_lag27=lag(IV_300_BI_lead730,27)) %>%
    mutate(IV_300_BI_lead730_lag28=lag(IV_300_BI_lead730,28)) %>%
    mutate(IV_300_BI_lead730_lag29=lag(IV_300_BI_lead730,29)) %>%
    mutate(IV_300_BI_lead730_lag30=lag(IV_300_BI_lead730,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI_lead730","AQI_lead730_lag1","AQI_lead730_lag2","AQI_lead730_lag3","AQI_lead730_lag4","AQI_lead730_lag5","AQI_lead730_lag6","AQI_lead730_lag7",
          "IV_300_BI_lead730","IV_300_BI_lead730_lag1","IV_300_BI_lead730_lag2","IV_300_BI_lead730_lag3","IV_300_BI_lead730_lag4","IV_300_BI_lead730_lag5","IV_300_BI_lead730_lag6","IV_300_BI_lead730_lag7")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}


# create lag
lead730_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)

# lag 0
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730) == F,]
summary(h$AQI_lead730)
IQR(h$AQI_lead730)
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag0.csv")

# lag 1
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag1.csv")

# lag 2
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag2.csv")

# lag 3
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag3.csv")

# lag 4
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag4.csv")

# lag 5
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag5.csv")

# lag 6
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag6.csv")

# lag 7
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_iv_neg_control_730_lag7.csv")






# stata code ===
# ivpoisson gmm rhinitis_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = aqi_back iv_300_bi)

exp(.0027605
    *6.247)
exp(.0037737    *30)




# zio - check with each AQIs
# pm10_AQI, pm25_AQI, so2_AQI, co_AQI, o3_AQI, no2_AQI

# pm10_AQI (보정변수 so2_AQI, co_AQI, o3_AQI, no2_AQI 포함)
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ so2_AQI + co_AQI + o3_AQI + no2_AQI + pm10_AQI,
                 instrument_formula = pm10_AQI ~ IV_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# pm25_AQI (보정변수 so2_AQI, co_AQI, o3_AQI, no2_AQI, pm10_pm25_AQI 포함)
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$pm10_pm25 = day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$pm10_day_mean - day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$pm25_day_mean
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ so2_AQI + co_AQI + o3_AQI + no2_AQI + pm10_pm25 + pm25_AQI,
                 instrument_formula = pm25_AQI ~ IV_300_BI, 
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# so2_AQI (보정변수 pm10_AQI, co_AQI, o3_AQI, no2_AQI 포함)
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + co_AQI + o3_AQI + no2_AQI + so2_AQI,
                 instrument_formula = so2_AQI ~ IV_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# co_AQI (보정변수 pm10_AQI, so2_AQI, o3_AQI, no2_AQI 포함)
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + o3_AQI + no2_AQI + co_AQI,
                 instrument_formula = co_AQI ~ IV_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# o3_AQI (보정변수 pm10_AQI, so2_AQI, co_AQI, no2_AQI 포함)
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + co_AQI + no2_AQI + o3_AQI,
                 instrument_formula = o3_AQI ~ IV_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# no2_AQI (보정변수 pm10_AQI, so2_AQI, co_AQI, o3_AQI 포함)
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + co_AQI + o3_AQI + no2_AQI,
                 instrument_formula = no2_AQI ~ IV_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)


h <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[,c("RHINITIS_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", "pm10_pm25", "AQI", "pm10_AQI", "pm25_AQI", "so2_AQI", "co_AQI", "o3_AQI", "no2_AQI", "IV_300_BI")]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"D:\\SNUlab\\thermal_inversion_0623_share\\rhinitis_iv.csv")




# age, sex subgroup analysis - zio
# 0705 lag도 붙여서 내보내기 (for stata)

# day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sin <- sin(2 * 3.14/365.25 * day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$yday)
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$cos <- cos(2 * 3.14/365.25 * day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$yday)

## IV_500 - IV_300 만들기
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_500_300_BI = day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_500_BI - day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI
step1_final_13_na$IV_500_300_BI = step1_final_13_na$IV_500_BI - step1_final_13_na$IV_300_BI

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ AQI, 
                 instrument_formula = AQI ~ IV_500_300_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

table(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_500_300_BI, day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)

## IV_700 - IV_500 만들기
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_700_500_BI = day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_700_BI - day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$IV_500_BI
step1_final_13_na$IV_700_500_BI = step1_final_13_na$IV_700_BI - step1_final_13_na$IV_500_BI

glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ AQI, 
                 instrument_formula = AQI ~ IV_700_500_BI,
                 data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)


create_lag <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
    mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
    mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
    mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
    mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
    mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
    mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
    mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
    mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
    mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%
    mutate(IV_300_BI_lag11=lag(IV_300_BI,11)) %>%
    mutate(IV_300_BI_lag12=lag(IV_300_BI,12)) %>%
    mutate(IV_300_BI_lag13=lag(IV_300_BI,13)) %>%
    mutate(IV_300_BI_lag14=lag(IV_300_BI,14)) %>%
    mutate(IV_300_BI_lag15=lag(IV_300_BI,15)) %>%
    mutate(IV_300_BI_lag16=lag(IV_300_BI,16)) %>%
    mutate(IV_300_BI_lag17=lag(IV_300_BI,17)) %>%
    mutate(IV_300_BI_lag18=lag(IV_300_BI,18)) %>%
    mutate(IV_300_BI_lag19=lag(IV_300_BI,19)) %>%
    mutate(IV_300_BI_lag20=lag(IV_300_BI,20)) %>%
    mutate(IV_300_BI_lag21=lag(IV_300_BI,21)) %>%
    mutate(IV_300_BI_lag22=lag(IV_300_BI,22)) %>%
    mutate(IV_300_BI_lag23=lag(IV_300_BI,23)) %>%
    mutate(IV_300_BI_lag24=lag(IV_300_BI,24)) %>%
    mutate(IV_300_BI_lag25=lag(IV_300_BI,25)) %>%
    mutate(IV_300_BI_lag26=lag(IV_300_BI,26)) %>%
    mutate(IV_300_BI_lag27=lag(IV_300_BI,27)) %>%
    mutate(IV_300_BI_lag28=lag(IV_300_BI,28)) %>%
    mutate(IV_300_BI_lag29=lag(IV_300_BI,29)) %>%
    mutate(IV_300_BI_lag30=lag(IV_300_BI,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_lag_iv500 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_500_BI_lag1=lag(IV_500_BI)) %>%
    mutate(IV_500_BI_lag2=lag(IV_500_BI,2)) %>%
    mutate(IV_500_BI_lag3=lag(IV_500_BI,3)) %>%
    mutate(IV_500_BI_lag4=lag(IV_500_BI,4)) %>%
    mutate(IV_500_BI_lag5=lag(IV_500_BI,5)) %>%
    mutate(IV_500_BI_lag6=lag(IV_500_BI,6)) %>%
    mutate(IV_500_BI_lag7=lag(IV_500_BI,7)) %>%
    mutate(IV_500_BI_lag8=lag(IV_500_BI,8)) %>%
    mutate(IV_500_BI_lag9=lag(IV_500_BI,9)) %>%
    mutate(IV_500_BI_lag10=lag(IV_500_BI,10)) %>%
    mutate(IV_500_BI_lag11=lag(IV_500_BI,11)) %>%
    mutate(IV_500_BI_lag12=lag(IV_500_BI,12)) %>%
    mutate(IV_500_BI_lag13=lag(IV_500_BI,13)) %>%
    mutate(IV_500_BI_lag14=lag(IV_500_BI,14)) %>%
    mutate(IV_500_BI_lag15=lag(IV_500_BI,15)) %>%
    mutate(IV_500_BI_lag16=lag(IV_500_BI,16)) %>%
    mutate(IV_500_BI_lag17=lag(IV_500_BI,17)) %>%
    mutate(IV_500_BI_lag18=lag(IV_500_BI,18)) %>%
    mutate(IV_500_BI_lag19=lag(IV_500_BI,19)) %>%
    mutate(IV_500_BI_lag20=lag(IV_500_BI,20)) %>%
    mutate(IV_500_BI_lag21=lag(IV_500_BI,21)) %>%
    mutate(IV_500_BI_lag22=lag(IV_500_BI,22)) %>%
    mutate(IV_500_BI_lag23=lag(IV_500_BI,23)) %>%
    mutate(IV_500_BI_lag24=lag(IV_500_BI,24)) %>%
    mutate(IV_500_BI_lag25=lag(IV_500_BI,25)) %>%
    mutate(IV_500_BI_lag26=lag(IV_500_BI,26)) %>%
    mutate(IV_500_BI_lag27=lag(IV_500_BI,27)) %>%
    mutate(IV_500_BI_lag28=lag(IV_500_BI,28)) %>%
    mutate(IV_500_BI_lag29=lag(IV_500_BI,29)) %>%
    mutate(IV_500_BI_lag30=lag(IV_500_BI,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_lag_iv700 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_700_BI_lag1=lag(IV_700_BI)) %>%
    mutate(IV_700_BI_lag2=lag(IV_700_BI,2)) %>%
    mutate(IV_700_BI_lag3=lag(IV_700_BI,3)) %>%
    mutate(IV_700_BI_lag4=lag(IV_700_BI,4)) %>%
    mutate(IV_700_BI_lag5=lag(IV_700_BI,5)) %>%
    mutate(IV_700_BI_lag6=lag(IV_700_BI,6)) %>%
    mutate(IV_700_BI_lag7=lag(IV_700_BI,7)) %>%
    mutate(IV_700_BI_lag8=lag(IV_700_BI,8)) %>%
    mutate(IV_700_BI_lag9=lag(IV_700_BI,9)) %>%
    mutate(IV_700_BI_lag10=lag(IV_700_BI,10)) %>%
    mutate(IV_700_BI_lag11=lag(IV_700_BI,11)) %>%
    mutate(IV_700_BI_lag12=lag(IV_700_BI,12)) %>%
    mutate(IV_700_BI_lag13=lag(IV_700_BI,13)) %>%
    mutate(IV_700_BI_lag14=lag(IV_700_BI,14)) %>%
    mutate(IV_700_BI_lag15=lag(IV_700_BI,15)) %>%
    mutate(IV_700_BI_lag16=lag(IV_700_BI,16)) %>%
    mutate(IV_700_BI_lag17=lag(IV_700_BI,17)) %>%
    mutate(IV_700_BI_lag18=lag(IV_700_BI,18)) %>%
    mutate(IV_700_BI_lag19=lag(IV_700_BI,19)) %>%
    mutate(IV_700_BI_lag20=lag(IV_700_BI,20)) %>%
    mutate(IV_700_BI_lag21=lag(IV_700_BI,21)) %>%
    mutate(IV_700_BI_lag22=lag(IV_700_BI,22)) %>%
    mutate(IV_700_BI_lag23=lag(IV_700_BI,23)) %>%
    mutate(IV_700_BI_lag24=lag(IV_700_BI,24)) %>%
    mutate(IV_700_BI_lag25=lag(IV_700_BI,25)) %>%
    mutate(IV_700_BI_lag26=lag(IV_700_BI,26)) %>%
    mutate(IV_700_BI_lag27=lag(IV_700_BI,27)) %>%
    mutate(IV_700_BI_lag28=lag(IV_700_BI,28)) %>%
    mutate(IV_700_BI_lag29=lag(IV_700_BI,29)) %>%
    mutate(IV_700_BI_lag30=lag(IV_700_BI,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_lag_iv500_300 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_500_300_BI_lag1=lag(IV_500_300_BI)) %>%
    mutate(IV_500_300_BI_lag2=lag(IV_500_300_BI,2)) %>%
    mutate(IV_500_300_BI_lag3=lag(IV_500_300_BI,3)) %>%
    mutate(IV_500_300_BI_lag4=lag(IV_500_300_BI,4)) %>%
    mutate(IV_500_300_BI_lag5=lag(IV_500_300_BI,5)) %>%
    mutate(IV_500_300_BI_lag6=lag(IV_500_300_BI,6)) %>%
    mutate(IV_500_300_BI_lag7=lag(IV_500_300_BI,7)) %>%
    mutate(IV_500_300_BI_lag8=lag(IV_500_300_BI,8)) %>%
    mutate(IV_500_300_BI_lag9=lag(IV_500_300_BI,9)) %>%
    mutate(IV_500_300_BI_lag10=lag(IV_500_300_BI,10)) %>%

    mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
    mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
    mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
    mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
    mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
    mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
    mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
    mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
    mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
    mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_iv700_500 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lag1=lag(AQI)) %>%
    mutate(AQI_lag2=lag(AQI,2)) %>%
    mutate(AQI_lag3=lag(AQI,3)) %>%
    mutate(AQI_lag4=lag(AQI,4)) %>%
    mutate(AQI_lag5=lag(AQI,5)) %>%
    mutate(AQI_lag6=lag(AQI,6)) %>%
    mutate(AQI_lag7=lag(AQI,7)) %>%
    mutate(AQI_lag8=lag(AQI,8)) %>%
    mutate(AQI_lag9=lag(AQI,9)) %>%
    mutate(AQI_lag10=lag(AQI,10)) %>%
    mutate(AQI_lag11=lag(AQI,11)) %>%
    mutate(AQI_lag12=lag(AQI,12)) %>%
    mutate(AQI_lag13=lag(AQI,13)) %>%
    mutate(AQI_lag14=lag(AQI,14)) %>%
    mutate(AQI_lag15=lag(AQI,15)) %>%
    mutate(AQI_lag16=lag(AQI,16)) %>%
    mutate(AQI_lag17=lag(AQI,17)) %>%
    mutate(AQI_lag18=lag(AQI,18)) %>%
    mutate(AQI_lag19=lag(AQI,19)) %>%
    mutate(AQI_lag20=lag(AQI,20)) %>%
    mutate(AQI_lag21=lag(AQI,21)) %>%
    mutate(AQI_lag22=lag(AQI,22)) %>%
    mutate(AQI_lag23=lag(AQI,23)) %>%
    mutate(AQI_lag24=lag(AQI,24)) %>%
    mutate(AQI_lag25=lag(AQI,25)) %>%
    mutate(AQI_lag26=lag(AQI,26)) %>%
    mutate(AQI_lag27=lag(AQI,27)) %>%
    mutate(AQI_lag28=lag(AQI,28)) %>%
    mutate(AQI_lag29=lag(AQI,29)) %>%
    mutate(AQI_lag30=lag(AQI,30)) %>%

    mutate(IV_700_500_BI_lag1=lag(IV_700_500_BI)) %>%
    mutate(IV_700_500_BI_lag2=lag(IV_700_500_BI,2)) %>%
    mutate(IV_700_500_BI_lag3=lag(IV_700_500_BI,3)) %>%
    mutate(IV_700_500_BI_lag4=lag(IV_700_500_BI,4)) %>%
    mutate(IV_700_500_BI_lag5=lag(IV_700_500_BI,5)) %>%
    mutate(IV_700_500_BI_lag6=lag(IV_700_500_BI,6)) %>%
    mutate(IV_700_500_BI_lag7=lag(IV_700_500_BI,7)) %>%
    mutate(IV_700_500_BI_lag8=lag(IV_700_500_BI,8)) %>%
    mutate(IV_700_500_BI_lag9=lag(IV_700_500_BI,9)) %>%
    mutate(IV_700_500_BI_lag10=lag(IV_700_500_BI,10)) %>%

    mutate(IV_500_300_BI_lag1=lag(IV_500_300_BI)) %>%
    mutate(IV_500_300_BI_lag2=lag(IV_500_300_BI,2)) %>%
    mutate(IV_500_300_BI_lag3=lag(IV_500_300_BI,3)) %>%
    mutate(IV_500_300_BI_lag4=lag(IV_500_300_BI,4)) %>%
    mutate(IV_500_300_BI_lag5=lag(IV_500_300_BI,5)) %>%
    mutate(IV_500_300_BI_lag6=lag(IV_500_300_BI,6)) %>%
    mutate(IV_500_300_BI_lag7=lag(IV_500_300_BI,7)) %>%
    mutate(IV_500_300_BI_lag8=lag(IV_500_300_BI,8)) %>%
    mutate(IV_500_300_BI_lag9=lag(IV_500_300_BI,9)) %>%
    mutate(IV_500_300_BI_lag10=lag(IV_500_300_BI,10)) %>%

    mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
    mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
    mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
    mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
    mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
    mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
    mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
    mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
    mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
    mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%

    ungroup()

  return (df_with_lag)
}



create_h <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg",
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_iv500 <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_500_BI","IV_500_BI_lag1","IV_500_BI_lag2","IV_500_BI_lag3","IV_500_BI_lag4","IV_500_BI_lag5","IV_500_BI_lag6","IV_500_BI_lag7","IV_500_BI_lag8","IV_500_BI_lag9","IV_500_BI_lag10", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_iv500_300 <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_500_300_BI","IV_500_300_BI_lag1","IV_500_300_BI_lag2","IV_500_300_BI_lag3","IV_500_300_BI_lag4","IV_500_300_BI_lag5","IV_500_300_BI_lag6","IV_500_300_BI_lag7","IV_500_300_BI_lag8","IV_500_300_BI_lag9","IV_500_300_BI_lag10", 
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_iv700 <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_700_BI","IV_700_BI_lag1","IV_700_BI_lag2","IV_700_BI_lag3","IV_700_BI_lag4","IV_700_BI_lag5","IV_700_BI_lag6","IV_700_BI_lag7","IV_700_BI_lag8","IV_700_BI_lag9","IV_700_BI_lag10", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_iv700_500 <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_700_500_BI","IV_700_500_BI_lag1","IV_700_500_BI_lag2","IV_700_500_BI_lag3","IV_700_500_BI_lag4","IV_700_500_BI_lag5","IV_700_500_BI_lag6","IV_700_500_BI_lag7","IV_700_500_BI_lag8","IV_700_500_BI_lag9","IV_700_500_BI_lag10", 
          "IV_500_300_BI","IV_500_300_BI_lag1","IV_500_300_BI_lag2","IV_500_300_BI_lag3","IV_500_300_BI_lag4","IV_500_300_BI_lag5","IV_500_300_BI_lag6","IV_500_300_BI_lag7","IV_500_300_BI_lag8","IV_500_300_BI_lag9","IV_500_300_BI_lag10",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_with_subgroup <- function(df) {
  h <- df[,c("RHINITIS_out_total_agg", "RHINITIS_out_total_age1", "RHINITIS_out_total_age2", "RHINITIS_out_total_age3", 
          "RHINITIS_out_total_age4", "RHINITIS_out_total_m", "RHINITIS_out_total_f", 
          "RHINITIS_out_total_m_age3", "RHINITIS_out_total_f_age3",
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}

create_h_with_index <- function(df) {
  h <- df[,c("air_out_idx", "date", "RHINITIS_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI")]
  h$wday1 = ifelse(h$wday=="1", 1, 0)
  h$wday2 = ifelse(h$wday=="2", 1, 0)
  h$wday3 = ifelse(h$wday=="3", 1, 0)
  h$wday4 = ifelse(h$wday=="4", 1, 0)
  h$wday5 = ifelse(h$wday=="5", 1, 0)
  h$wday6 = ifelse(h$wday=="6", 1, 0)
  h$wday7 = ifelse(h$wday=="7", 1, 0)

  return (h)
}


getwd()
colnames(day_rhinitis_1317_agg_seoul_step2_humi_pressure)
# day_rhinitis_1317_agg_seoul_step2_humi_pressure 를 다시 age, sex 로 aggregate
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg 는 date, air_out_idx 로만 aggregate 되어있음

## 0706 박사님 피드백 후 수정 - day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg 에 subgroup 별 column 을 merge
## 이제 dataset 별 lag 별이 아니라, lag 별로만 na 제거하고 내보내면 됨 !! ~~

# SUBGROUP 있을때 실행
final_df_for_subgroup_analysis <- merge(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg, day_rhinitis_age1,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_age2,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_age3,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_age4,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_m,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_f,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_m_age1,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_m_age2,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_m_age3,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_m_age4,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_f_age1,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_f_age2,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_f_age3,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_rhinitis_f_age4,  by=c("date","air_out_idx"),all.x = T)

colnames(final_df_for_subgroup_analysis)


# SUBGROUP 없을때 실행
final_df_for_subgroup_analysis <- copy(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg)


# 2013-12 붙이기 - 여기부터 다시 

# stage 1) lag 용 df를 만들어서 최종 df에 붙이자 
  # select IV
  iv_name = "IV_300_BI"
  iv_name = "IV_500_BI"
  iv_name = "IV_700_BI"
  iv_name  = "IV_500_300_BI"
  iv_name  = "IV_700_500_BI"

  # create lag df 
    # IV_300, IV_500, IV_700 사용할 경우
    tmp_df_for_lag <- subset(final_df_for_subgroup_analysis, select=c("date", "air_out_idx", "AQI", iv_name))
    tmp_df_for_lag_13 <- subset(step1_final_13_na, select = c("date", "air_out_idx", "AQI", iv_name))
    tmp_df_lag_final <- rbind(tmp_df_for_lag_13, tmp_df_for_lag)

    # IV_500_300, IV_700_500 사용할 경우
    # 1. IV_500_300
      tmp_df_for_lag <- subset(final_df_for_subgroup_analysis, select=c("date", "air_out_idx", "AQI", "IV_500_300_BI", "IV_300_BI"))
      tmp_df_for_lag_13 <- subset(step1_final_13_na, select = c("date", "air_out_idx", "AQI", "IV_500_300_BI", "IV_300_BI"))
      tmp_df_lag_final <- rbind(tmp_df_for_lag_13, tmp_df_for_lag)
    # 2. IV_700_500
      tmp_df_for_lag <- subset(final_df_for_subgroup_analysis, select=c("date", "air_out_idx", "AQI", "IV_700_500_BI", "IV_500_300_BI", "IV_300_BI"))
      tmp_df_for_lag_13 <- subset(step1_final_13_na, select = c("date", "air_out_idx", "AQI", "IV_700_500_BI", "IV_500_300_BI", "IV_300_BI"))
      tmp_df_lag_final <- rbind(tmp_df_for_lag_13, tmp_df_for_lag)

  # select iv function
  tmp_df_lag_final <- create_lag(tmp_df_lag_final) # iv_300
  tmp_df_lag_final <- create_lag_iv500(tmp_df_lag_final) # iv_500
  tmp_df_lag_final <- create_lag_iv700(tmp_df_lag_final) # iv_700
  tmp_df_lag_final <- create_lag_iv500_300(tmp_df_lag_final) # iv_500_300
  tmp_df_lag_final <- create_lag_iv700_500(tmp_df_lag_final) # iv_700_500



# stage 2) lag 붙였으니 다시 2014년부터 뽑자
tmp_df_lag_final <- tmp_df_lag_final[substr(tmp_df_lag_final$date, 1, 4) %in% c(2014:2017),]
colnames(tmp_df_lag_final)
min(tmp_df_lag_final$date)

# stage 3) final df + lag df merge 
# 기본
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 5:64)], by=c("date", "air_out_idx"), all.x = T)
# IV_500_300
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 6:55)], by=c("date", "air_out_idx"), all.x = T)
# IV_700_500
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 7:66)], by=c("date", "air_out_idx"), all.x = T)

summary(final_df_for_subgroup_analysis_lag)
colnames(final_df_for_subgroup_analysis_lag)


# stage 4) lag 별 na 제거하고 df 생성
### 0715 stage 4 는 밑으로 ###

#### IV_300_BI ####
  # lag 0
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag0.csv")

  # lag 1
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag1.csv")

  # lag 2
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag2.csv")

  # lag 3
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag3.csv")

  # lag 4
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag4.csv")

  # lag 5
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag5.csv")

  # lag 6
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag6.csv")

  # lag 7
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag7.csv")
#### IV_300_BI end. ####


#### IV_500_BI ####
  # lag 0
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag0_iv500.csv")

  # lag 1
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag1_iv500.csv")

  # lag 2
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag2_iv500.csv")

  # lag 3
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag3_iv500.csv")

  # lag 4
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag4_iv500.csv")

  # lag 5
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag5_iv500.csv")

  # lag 6
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag6_iv500.csv")

  # lag 7
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag7_iv500.csv")
#### IV_500_BI end. ####


#### IV_700_BI ####
  # lag 0
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag0_iv700.csv")

  # lag 1
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag1_iv700.csv")

  # lag 2
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag2_iv700.csv")

  # lag 3
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag3_iv700.csv")

  # lag 4
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag4_iv700.csv")

  # lag 5
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag5_iv700.csv")

  # lag 6
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag6_iv700.csv")

  # lag 7
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag7_iv700.csv")
#### IV_700_BI end. ####



#### IV_500_300_BI ####
  # lag 0
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h) # 20199 36
  h <- na.omit(h)
  dim(h) # 20081 36
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag0_iv500_300.csv")

  # lag 1
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag1_iv500_300.csv")

  # lag 2
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag2_iv500_300.csv")

  # lag 3
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag3_iv500_300.csv")

  # lag 4
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag4_iv500_300.csv")

  # lag 5
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag5_iv500_300.csv")

  # lag 6
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag6_iv500_300.csv")

  # lag 7
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag7_iv500_300.csv")
#### IV_500_300_BI end. ####


#### IV_700_500_BI ####
  # lag 0
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h) # 18040 36
  h <- na.omit(h)
  dim(h) # 17901 36
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag0_iv700_500.csv")

  # lag 1
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag1_iv700_500.csv")

  # lag 2
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag2_iv700_500.csv")

  # lag 3
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag3_iv700_500.csv")

  # lag 4
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag4_iv700_500.csv")

  # lag 5
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag5_iv700_500.csv")

  # lag 6
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag6_iv700_500.csv")

  # lag 7
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\rhinitis_lag7_iv700_500.csv")
#### IV_700_500_BI end. ####




## 이하 다 취소 ------------------------------------------------------------------
# # agegroup1
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==1,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup1 - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_age1_lag.csv")


# # agegroup2
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==2,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup2 - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_age2_lag.csv")


# # agegroup3
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==3,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup3 - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_age3_lag.csv")


# # agegroup4
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==4,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup4 - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_age4_lag.csv")


# # sex M
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==1,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # sex M - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex1_lag.csv")


# # sex F
# day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==2,]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # sex F - lag
#   day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2_lag <- create_lag(day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2)
#   h <- create_h(day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_sex2_lag.csv")


# ##### age_group & sex 나누기 #####
# # AGE1_M
# day_rhinitis_m_age1 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==1&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_m_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE1_M - lag
#   day_rhinitis_m_age1_lag <- create_lag(day_rhinitis_m_age1)
#   h <- create_h(day_rhinitis_m_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_m_age1_lag.csv")


# # AGE1_F
# day_rhinitis_f_age1<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==1&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_f_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE1_F - lag
#   day_rhinitis_f_age1_lag <- create_lag(day_rhinitis_f_age1)
#   h <- create_h(day_rhinitis_f_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_f_age1_lag.csv")


# # AGE2_M
# day_rhinitis_m_age2<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==2&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_m_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE2_M - lag
#   day_rhinitis_m_age2_lag <- create_lag(day_rhinitis_m_age2)
#   h <- create_h(day_rhinitis_m_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_m_age2_lag.csv")


# # AGE2_F
# day_rhinitis_f_age2<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==2&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_f_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE2_F - lag
#   day_rhinitis_f_age2_lag <- create_lag(day_rhinitis_f_age2)
#   h <- create_h(day_rhinitis_f_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_f_age2_lag.csv")


# # AGE3_M
# day_rhinitis_m_age3<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==3&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_m_age3,family = quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE3_M - lag
#   day_rhinitis_m_age3_lag <- create_lag(day_rhinitis_m_age3)
#   h <- create_h(day_rhinitis_m_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_m_age3_lag.csv")


# # AGE3_F
# day_rhinitis_f_age3 <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==3&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_f_age3,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE3_F - lag
#   day_rhinitis_f_age3_lag <- create_lag(day_rhinitis_f_age3)
#   h <- create_h(day_rhinitis_f_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_f_age3_lag.csv")


# # AGE4_M
# day_rhinitis_m_age4<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==4&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_m_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE4_M - lag
#   day_rhinitis_m_age4_lag <- create_lag(day_rhinitis_m_age4)
#   h <- create_h(day_rhinitis_m_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_m_age4_lag.csv")


# # AGE4_F
# day_rhinitis_f_age4<-day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg[which(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$age_group==4&day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = RHINITIS_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_rhinitis_f_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE4_F - lag
#   day_rhinitis_f_age4_lag <- create_lag(day_rhinitis_f_age4)
#   h <- create_h(day_rhinitis_f_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_rhinitis_f_age4_lag.csv")





##### 220617 lag 보기 #####
library(dplyr)
day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag=
  day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg%>%
  group_by(air_out_idx)%>%
  mutate(AQI_lag1=lag(AQI)) %>%
  mutate(AQI_lag2=lag(AQI,2)) %>%
  mutate(AQI_lag3=lag(AQI,3)) %>%
  mutate(AQI_lag4=lag(AQI,4)) %>%
  mutate(AQI_lag5=lag(AQI,5)) %>%
  mutate(AQI_lag6=lag(AQI,6)) %>%
  mutate(AQI_lag7=lag(AQI,7)) %>%
  mutate(AQI_lag8=lag(AQI,8)) %>%
  mutate(AQI_lag9=lag(AQI,9)) %>%
  mutate(AQI_lag10=lag(AQI,10)) %>%
  mutate(AQI_lag11=lag(AQI,11)) %>%
  mutate(AQI_lag12=lag(AQI,12)) %>%
  mutate(AQI_lag13=lag(AQI,13)) %>%
  mutate(AQI_lag14=lag(AQI,14)) %>%
  mutate(AQI_lag15=lag(AQI,15)) %>%
  mutate(AQI_lag16=lag(AQI,16)) %>%
  mutate(AQI_lag17=lag(AQI,17)) %>%
  mutate(AQI_lag18=lag(AQI,18)) %>%
  mutate(AQI_lag19=lag(AQI,19)) %>%
  mutate(AQI_lag20=lag(AQI,20)) %>%
  mutate(AQI_lag21=lag(AQI,21)) %>%
  mutate(AQI_lag22=lag(AQI,22)) %>%
  mutate(AQI_lag23=lag(AQI,23)) %>%
  mutate(AQI_lag24=lag(AQI,24)) %>%
  mutate(AQI_lag25=lag(AQI,25)) %>%
  mutate(AQI_lag26=lag(AQI,26)) %>%
  mutate(AQI_lag27=lag(AQI,27)) %>%
  mutate(AQI_lag28=lag(AQI,28)) %>%
  mutate(AQI_lag29=lag(AQI,29)) %>%
  mutate(AQI_lag30=lag(AQI,30)) %>%
  
  mutate(IV_300_BI_lag1=lag(IV_300_BI)) %>%
  mutate(IV_300_BI_lag2=lag(IV_300_BI,2)) %>%
  mutate(IV_300_BI_lag3=lag(IV_300_BI,3)) %>%
  mutate(IV_300_BI_lag4=lag(IV_300_BI,4)) %>%
  mutate(IV_300_BI_lag5=lag(IV_300_BI,5)) %>%
  mutate(IV_300_BI_lag6=lag(IV_300_BI,6)) %>%
  mutate(IV_300_BI_lag7=lag(IV_300_BI,7)) %>%
  mutate(IV_300_BI_lag8=lag(IV_300_BI,8)) %>%
  mutate(IV_300_BI_lag9=lag(IV_300_BI,9)) %>%
  mutate(IV_300_BI_lag10=lag(IV_300_BI,10)) %>%
  mutate(IV_300_BI_lag11=lag(IV_300_BI,11)) %>%
  mutate(IV_300_BI_lag12=lag(IV_300_BI,12)) %>%
  mutate(IV_300_BI_lag13=lag(IV_300_BI,13)) %>%
  mutate(IV_300_BI_lag14=lag(IV_300_BI,14)) %>%
  mutate(IV_300_BI_lag15=lag(IV_300_BI,15)) %>%
  mutate(IV_300_BI_lag16=lag(IV_300_BI,16)) %>%
  mutate(IV_300_BI_lag17=lag(IV_300_BI,17)) %>%
  mutate(IV_300_BI_lag18=lag(IV_300_BI,18)) %>%
  mutate(IV_300_BI_lag19=lag(IV_300_BI,19)) %>%
  mutate(IV_300_BI_lag20=lag(IV_300_BI,20)) %>%
  mutate(IV_300_BI_lag21=lag(IV_300_BI,21)) %>%
  mutate(IV_300_BI_lag22=lag(IV_300_BI,22)) %>%
  mutate(IV_300_BI_lag23=lag(IV_300_BI,23)) %>%
  mutate(IV_300_BI_lag24=lag(IV_300_BI,24)) %>%
  mutate(IV_300_BI_lag25=lag(IV_300_BI,25)) %>%
  mutate(IV_300_BI_lag26=lag(IV_300_BI,26)) %>%
  mutate(IV_300_BI_lag27=lag(IV_300_BI,27)) %>%
  mutate(IV_300_BI_lag28=lag(IV_300_BI,28)) %>%
  mutate(IV_300_BI_lag29=lag(IV_300_BI,29)) %>%
  mutate(IV_300_BI_lag30=lag(IV_300_BI,30)) %>%
  ungroup()
h <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag[,c("RHINITIS_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
                                                              "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
                                                              "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10")]

h <- day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag[is.na(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag$AQI_lag7)==F,]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag.csv")
# stata code ===
# ivpoisson gmm ASTHMA_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)

