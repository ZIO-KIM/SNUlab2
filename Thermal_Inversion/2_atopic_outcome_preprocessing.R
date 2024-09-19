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

##### outcome 데이터 불러오기 #####
day_atopic_1317 <- read_sas("day_atopic_1317_cnt_3_total1.sas7bdat",NULL)
day_atopic_1317$age_group<-ifelse(day_atopic_1317$AGE %in% c(0,5),1,ifelse(day_atopic_1317$AGE%in%c(10,15),2,ifelse(day_atopic_1317$AGE>=65,3,4)))
day_atopic_1317_agg=aggregate(day_atopic_1317[,7:10], by=list(day_atopic_1317$age_group,
                                                              day_atopic_1317$SEX_TYPE,
                                                              day_atopic_1317$DT,
                                                              day_atopic_1317$"시군구"),sum)
# save.image("thermal_inversion_220419.RData")
colnames(day_atopic_1317_agg)[1:4]=c("age_group","sex","dt","측정소명")
day_atopic_1317_agg_seoul<-day_atopic_1317_agg[which(substr(day_atopic_1317_agg$"측정소명",1,2)=="서울"),]
day_atopic_1317_agg_seoul$"측정소명"<-gsub("서울특별시 ","",day_atopic_1317_agg_seoul$"측정소명")
day_atopic_1317_agg_seoul$date=gsub(" ","",paste(substr(day_atopic_1317_agg_seoul$dt,1,4),"-",substr(day_atopic_1317_agg_seoul$dt,5,6),"-",substr(day_atopic_1317_agg_seoul$dt,7,8)))


##### step2_final_back이랑 합치기 ##### 
day_atopic_1317_agg_seoul_step<-merge(day_atopic_1317_agg_seoul,step2_final_back,by=c("date","측정소명"), all.x=TRUE)

##### lag, crossbasis matrix #####
day_atopic_1317_agg_seoul_step$yday <- yday(day_atopic_1317_agg_seoul_step$date)
day_atopic_1317_agg_seoul_step$wday <- wday(day_atopic_1317_agg_seoul_step$date)
day_atopic_1317_agg_seoul_step$wday_mat<-model.matrix( ~ wday - 1, data=day_atopic_1317_agg_seoul_step)

lag=7
kno_temp <- equalknots(day_atopic_1317_agg_seoul_step$temp_mean_total,nk=2)
klag <- logknots(lag,nk=2)

# CROSSBASIS MATRIX
ns.basis_temp <- crossbasis(day_atopic_1317_agg_seoul_step$temp_mean_total,
                            argvar=list(knots=kno_temp),
                            group=day_atopic_1317_agg_seoul_step$air_out_idx,
                            arglag=list(knots=klag),
                            lag=lag)
day_atopic_1317_agg_seoul_step$ns.basis_temp <- crossbasis(day_atopic_1317_agg_seoul_step$temp_mean_total,
                                                           argvar=list(knots=kno_temp),
                                                           group=day_atopic_1317_agg_seoul_step$air_out_idx,
                                                           arglag=list(knots=klag),
                                                           lag=lag)

kno_yday <- equalknots(day_atopic_1317_agg_seoul_step$yday,nk=2)
ns.basis_yday <- crossbasis(day_atopic_1317_agg_seoul_step$yday,argvar=list(knots=kno_yday),
                            group=day_atopic_1317_agg_seoul_step$air_out_idx,
                            #arglag=list(knots=klag),
                            #lag=lag
)
day_atopic_1317_agg_seoul_step$ns.basis_yday <- crossbasis(day_atopic_1317_agg_seoul_step$yday,
                                                           argvar=list(knots=kno_yday),
                                                           group=day_atopic_1317_agg_seoul_step$air_out_idx,
                                                           #arglag=list(knots=klag),
                                                           #lag=lag
)


##### outcome 정의 #####
day_atopic_1317_agg_seoul_step$atopic_in_out=day_atopic_1317_agg_seoul_step$ATOPIC_in_total+day_atopic_1317_agg_seoul_step$ATOPIC_out_total
day_atopic_1317_agg_seoul_step$atopic_in_em=day_atopic_1317_agg_seoul_step$ATOPIC_in_total+day_atopic_1317_agg_seoul_step$ATOPIC_em_total
day_atopic_1317_agg_seoul_step$atopic_in_out_em=day_atopic_1317_agg_seoul_step$ATOPIC_in_total+day_atopic_1317_agg_seoul_step$ATOPIC_out_total+
  day_atopic_1317_agg_seoul_step$ATOPIC_em_total


##### age_group & sex 나누기 #####
day_atopic_m_age1<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==1&day_atopic_1317_agg_seoul_step$sex==1),]
day_atopic_m_age2<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==2&day_atopic_1317_agg_seoul_step$sex==1),]
day_atopic_m_age3<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==3&day_atopic_1317_agg_seoul_step$sex==1),]
day_atopic_m_age4<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==4&day_atopic_1317_agg_seoul_step$sex==1),]

day_atopic_f_age1<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==1&day_atopic_1317_agg_seoul_step$sex==2),]
day_atopic_f_age2<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==2&day_atopic_1317_agg_seoul_step$sex==2),]
day_atopic_f_age3<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==3&day_atopic_1317_agg_seoul_step$sex==2),]
day_atopic_f_age4<-day_atopic_1317_agg_seoul_step[which(day_atopic_1317_agg_seoul_step$age_group==4&day_atopic_1317_agg_seoul_step$sex==2),]

# step2_final$yday <- yday(step2_final$date)
# step2_final$wday <- wday(step2_final$date)
# step2_final_back$wday=factor(step2_final_back$wday)
# step2_final_back$wday_mat=model.matrix( ~ wday - 1, data=step2_final_back)  
# summary(lm(AQI ~ AQI_back+IV_300_BI,data=step2_final_back))
# model_formula = formula(atopic_em_total~rain_sum+temp_mean_total +ns.basis_yday+wday_mat+AQI)
# instrument_formula =formula(AQI ~ AQI_back+IV_300_BI+rain_sum+temp_mean_total +ns.basis_yday+wday_mat)


##### IV #####
find_instruments(model_formula,instrument_formula)

day_atopic_1317_agg_seoul_step2<-na.omit(day_atopic_1317_agg_seoul_step)
day_atopic_1317_agg_seoul_step2$wday <- as.factor(day_atopic_1317_agg_seoul_step2$wday)
day_atopic_1317_agg_seoul_step2$age_group = factor(day_atopic_1317_agg_seoul_step2$age_group)
glm_iv <- iv.glm(model_formula = ATOPIC_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+age_group+sex+AQI,
                 instrument_formula = AQI ~ AQI_back+IV_300_BI,
                 data=day_atopic_1317_agg_seoul_step2,family =quasipoisson, link = 'log')
summary(glm_iv)

# glm_iv$instrumented
# glm_iv$instruments
# glm_iv$exclusion_restriction
# glm_iv$instrument_validity
# glm_iv$stage_one
# diagnose(glm_iv)
# 
# h <- day_atopic_1317_agg_seoul_step2[,c("ATOPIC_out_total","rain_sum","ns.basis_temp","ns.basis_yday","wday","sex","age_group","AQI","AQI_back","IV_300_BI")]
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
# write.csv(h,"atopic_iv.csv")
# stata code ===
# ivpoisson gmm asthma_out_total rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 (aqi = aqi_back iv_300_bi)

##### 황사 추가해봤지만 안하는걸로 #####
# dust <- read_excel("02_22_dust.xlsx")
# colnames(dust)[1] <- "date_yj"
# dust$date = paste0(substr(dust$date_yj,1,4),"-",substr(dust$date_yj,5,6),"-",substr(dust$date_yj,7,8))
# dust_1417 <- dust[substr(dust$date,1,4) %in% c("2014","2015","2016","2017"),]
# day_atopic_1317_agg_seoul_step2_dust <- merge(day_atopic_1317_agg_seoul_step2,dust_1417,by="date",all.x = T)
# day_atopic_1317_agg_seoul_step2_dust$dust_yn <- as.factor(day_atopic_1317_agg_seoul_step2_dust$dust_yn)
# glm_iv <- iv.glm(model_formula = ATOPIC_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+dust_yn+AQI,
#                  instrument_formula = AQI ~ AQI_back+IV_300_BI,
#                  data=day_atopic_1317_agg_seoul_step2_dust,family =quasipoisson, link = 'log')
# summary(glm_iv)


##### 습도 추가 ##### 
# hum <- weather[,c("date","air_out_idx","humi_mean_total")]
day_atopic_1317_agg_seoul_step2_humi <- merge(day_atopic_1317_agg_seoul_step2, hum, by=c("air_out_idx","date"))

kno_hum <- equalknots(day_atopic_1317_agg_seoul_step2_humi$humi_mean_total,nk=2)

ns.basis_hum <- crossbasis(day_atopic_1317_agg_seoul_step2_humi$humi_mean_total,argvar=list(knots=kno_hum),
                           group=day_atopic_1317_agg_seoul_step2_humi$air_out_idx)

day_atopic_1317_agg_seoul_step2_humi$ns.basis_hum <- crossbasis(day_atopic_1317_agg_seoul_step2_humi$humi_mean_total,argvar=list(knots=kno_hum),
                                                                group=day_atopic_1317_agg_seoul_step2_humi$air_out_idx)

glm_iv <- iv.glm(model_formula = ATOPIC_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+ns.basis_hum+AQI,
                 instrument_formula = AQI ~ AQI_back+IV_300_BI,
                 data=day_atopic_1317_agg_seoul_step2_humi,family =quasipoisson, link = 'log')
summary(glm_iv)

##### 기압, 적설, 일사, 일조 추가 #####
#asthma 와 동일 
day_atopic_1317_agg_seoul_step2_humi_pressure <- merge(day_atopic_1317_agg_seoul_step2_humi,pressure_snow_sunshine_1417,by="date",all.x=T)

kno_pressure <- equalknots(day_atopic_1317_agg_seoul_step2_humi_pressure$pressure_mean,nk=2)
ns.basis_pressure <- crossbasis(day_atopic_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                group=day_atopic_1317_agg_seoul_step2_humi_pressure$air_out_idx)
day_atopic_1317_agg_seoul_step2_humi_pressure$ns.basis_pressure <- crossbasis(day_atopic_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                                                              group=day_atopic_1317_agg_seoul_step2_humi_pressure$air_out_idx)

glm_iv <- iv.glm(model_formula = ATOPIC_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ AQI_back+IV_300_BI,
                 data=day_atopic_1317_agg_seoul_step2_humi_pressure,family =quasipoisson, link = 'log')
summary(glm_iv)

##### age sex aggreagate #####
atopic_agg <- aggregate(day_atopic_1317_agg_seoul_step2_humi_pressure[,"ATOPIC_out_total"],
                        by=list(day_atopic_1317_agg_seoul_step2_humi_pressure$date,
                                day_atopic_1317_agg_seoul_step2_humi_pressure$air_out_idx),sum)
colnames(atopic_agg) <- c("date","air_out_idx","ATOPIC_out_total_agg")
day_atopic_1317_agg_seoul_step2_humi_pressure_agg <- merge(atopic_agg,day_atopic_1317_agg_seoul_step2_humi_pressure[,c(1,2,14:82)],  by=c("date","air_out_idx"),all.x = T)

air_dups <- day_atopic_1317_agg_seoul_step2_humi_pressure_agg[c("air_out_idx", "date")]
day_atopic_1317_agg_seoul_step2_humi_pressure_agg <- day_atopic_1317_agg_seoul_step2_humi_pressure_agg[!duplicated(air_dups),]

glm_iv <- iv.glm(model_formula = ATOPIC_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=day_atopic_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)


h <- day_atopic_1317_agg_seoul_step2_humi_pressure_agg[,c("ATOPIC_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday","AQI","visibility_back","IV_300_BI")]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"atopic_iv.csv")
# stata code ===
# ivpoisson gmm atopic_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = aqi_back iv_300_bi)

exp(0.0012422  *30)
exp(.0022121*30)


##### 220617 lag 보기 #####
library(dplyr)
day_atopic_1317_agg_seoul_step2_humi_pressure_agg_lag=
  day_atopic_1317_agg_seoul_step2_humi_pressure_agg%>%
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
h <- day_atopic_1317_agg_seoul_step2_humi_pressure_agg_lag[,c("ATOPIC_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
                                                                "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
                                                                "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10")]

h <- day_atopic_1317_agg_seoul_step2_humi_pressure_agg_lag[is.na(day_atopic_1317_agg_seoul_step2_humi_pressure_agg_lag$AQI_lag7)==F,]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"day_atopic_1317_agg_seoul_step2_humi_pressure_agg_lag.csv")
# stata code ===
# ivpoisson gmm ASTHMA_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)

