install.packages("OneSampleMR")
##### library ##### 
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
library(readxl)
library(dplyr)
####################

##### outcome 데이터 불러오기 ##### - zio
## rdata file load
# load(file="D:\\SNUlab\\thermal_inversion_0623_share\\220622_thermal_inversion.RData") - 성표샘 원본
# load(file="D:\\SNUlab\\thermal_inversion_0623_share\\220718.RData") # - humidity < 0 삭제 후 
step1_final_13_na <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_13_na.csv')


### 2018 데이터 처리 - python 에서 ###
# 성표샘이 만드신 최종 데이터 - 
  # 1. day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag
  # 2. day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag
  # 3. day_whole_1317_agg_seoul_step2_humi_pressure_agg_lag
  write.csv(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag, "D:\\SNUlab\\thermal_inversion_0623_share\\day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag.csv")
  write.csv(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag, "D:\\SNUlab\\thermal_inversion_0623_share\\day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg_lag.csv")
  write.csv(day_whole_1317_agg_seoul_step3_humi_pressure_agg_lag, "D:\\SNUlab\\thermal_inversion_0623_share\\day_whole_1317_agg_seoul_step3_humi_pressure_agg_lag.csv")

  # crossbasis까지 포함하여 내보낸 데이터에서 na.omit 을 하면 발생하는 에러 - 이 데이터로 다시 해보자
  # day_asthma_1317_agg_seoul_step
  write.csv(day_asthma_1317_agg_seoul_step, "D:\\SNUlab\\thermal_inversion_0623_share\\day_asthma_1317_agg_seoul_step.csv")
  write.csv(day_rhinitis_1317_agg_seoul_step, "D:\\SNUlab\\thermal_inversion_0623_share\\day_rhinitis_1317_agg_seoul_step.csv")
  # write.csv(day_whole_1317_agg_seoul_step2, "D:\\SNUlab\\thermal_inversion_0623_share\\day_whole_1317_agg_seoul_step2.csv")

# 2018 데이터 최종 - step1_final_na_back ("D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_na_back_2018.csv")
#####################################

day_asthma_1317 <- read_sas("day_asthma_1317_cnt_3_total1.sas7bdat",NULL)
colnames(day_asthma_1317)
summary(day_asthma_1317)
day_asthma_1317$age_group<-ifelse(day_asthma_1317$AGE %in% c(0,5),1,ifelse(day_asthma_1317$AGE%in%c(10,15),2,ifelse(day_asthma_1317$AGE>=65,3,4)))
day_asthma_1317_agg=aggregate(day_asthma_1317[,7:10], by=list(day_asthma_1317$age_group,
                                                              day_asthma_1317$SEX_TYPE,
                                                              day_asthma_1317$DT,
                                                              day_asthma_1317$"시군구"),sum)
colnames(day_asthma_1317_agg)[1:4]=c("age_group","sex","dt","측정소명")
day_asthma_1317_agg_seoul<-day_asthma_1317_agg[which(substr(day_asthma_1317_agg$"측정소명",1,2)=="서울"),]
day_asthma_1317_agg_seoul$"측정소명"<-gsub("서울특별시 ","",day_asthma_1317_agg_seoul$"측정소명")
day_asthma_1317_agg_seoul$date=gsub(" ","",paste(substr(day_asthma_1317_agg_seoul$dt,1,4),"-",substr(day_asthma_1317_agg_seoul$dt,5,6),"-",substr(day_asthma_1317_agg_seoul$dt,7,8)))

summary(day_asthma_1317)
colnames(day_asthma_1317_agg_seoul)
colnames(step2_final_back_zio)
##### step2_final_back이랑 합치기 ##### 
# day_asthma_1317_agg_seoul_step <- merge(day_asthma_1317_agg_seoul,step2_final_back,by=c("date","측정소명"), all.x=TRUE)
day_asthma_1317_agg_seoul_step <- merge(day_asthma_1317_agg_seoul,step1_final_na,by=c("date","측정소명"), all.x=TRUE)
colnames(day_asthma_1317_agg_seoul_step)
dim(day_asthma_1317_agg_seoul_step)

# lead 붙인 데이터 불러오기 - 365 # - zio
asthma_lead_365 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead.csv")
### 이 밑으로 다 돌리지 말고, AQI_lead365만 떼서 final data 에 붙이기 !! ###
dim(asthma_lead_365)
cbind(lapply(lapply(asthma_lead_365, is.na), sum))

# lead 붙인 데이터 불러오기 - 182 # - zio
asthma_lead_182 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead_182.csv")
dim(asthma_lead_182)
colSums(is.na(asthma_lead_182))

# lead 붙인 데이터 불러오기 - 92 # - zio
asthma_lead_92 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead_92.csv")
dim(asthma_lead_92)
colSums(is.na(asthma_lead_92))

# lead 붙인 데이터 불러오기 - 730 # - zio
asthma_lead_730 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead_730.csv")
dim(asthma_lead_730)
colSums(is.na(asthma_lead_730))

# lead 붙인 데이터 불러오기 - 730 # - zio
asthma_lead_1 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead_1.csv")
dim(asthma_lead_1)
colSums(is.na(asthma_lead_1))

##### lag, crossbasis matrix #####
# day_asthma_1317_agg_seoul_step <- subset(day_asthma_1317_agg_seoul_step, select = -c(yday, wday, wday_mat))
day_asthma_1317_agg_seoul_step$yday <- yday(day_asthma_1317_agg_seoul_step$date)
day_asthma_1317_agg_seoul_step$wday <- wday(day_asthma_1317_agg_seoul_step$date)
day_asthma_1317_agg_seoul_step$wday_mat<-model.matrix( ~ wday - 1, data=day_asthma_1317_agg_seoul_step)

lag=7
kno_temp <- equalknots(day_asthma_1317_agg_seoul_step$temp_mean_total,nk=2)
klag <- logknots(lag,nk=2)

# CROSSBASIS MATRIX
ns.basis_temp <- crossbasis(day_asthma_1317_agg_seoul_step$temp_mean_total,
                            argvar=list(knots=kno_temp),
                            group=day_asthma_1317_agg_seoul_step$air_out_idx,
                           arglag=list(knots=klag),
                           lag=lag)
day_asthma_1317_agg_seoul_step$ns.basis_temp <- crossbasis(day_asthma_1317_agg_seoul_step$temp_mean_total,
                                       argvar=list(knots=kno_temp),
                                       group=day_asthma_1317_agg_seoul_step$air_out_idx,
                                       arglag=list(knots=klag),
                                       lag=lag)


kno_yday <- equalknots(day_asthma_1317_agg_seoul_step$yday,nk=2)

ns.basis_yday <- crossbasis(day_asthma_1317_agg_seoul_step$yday,argvar=list(knots=kno_yday),
                            group=day_asthma_1317_agg_seoul_step$air_out_idx,
                            #arglag=list(knots=klag),
                            #lag=lag
                            )
day_asthma_1317_agg_seoul_step$ns.basis_yday <- crossbasis(day_asthma_1317_agg_seoul_step$yday,
                                                           argvar=list(knots=kno_yday),
                                                           group=day_asthma_1317_agg_seoul_step$air_out_idx,
                                                           #arglag=list(knots=klag),
                                                           #lag=lag
                                                           )

##### outcome 정의 #####
day_asthma_1317_agg_seoul_step$ASTHMA_in_out=day_asthma_1317_agg_seoul_step$ASTHMA_in_total+day_asthma_1317_agg_seoul_step$ASTHMA_out_total
day_asthma_1317_agg_seoul_step$ASTHMA_in_em=day_asthma_1317_agg_seoul_step$ASTHMA_in_total+day_asthma_1317_agg_seoul_step$ASTHMA_em_total
day_asthma_1317_agg_seoul_step$ASTHMA_in_out_em=day_asthma_1317_agg_seoul_step$ASTHMA_in_total+day_asthma_1317_agg_seoul_step$ASTHMA_em_total+
                                                day_asthma_1317_agg_seoul_step$ASTHMA_out_total
summary(day_asthma_1317_agg_seoul_step)
min(day_asthma_1317_agg_seoul_step$date)
# write.csv(head(day_asthma_1317_agg_seoul_step,1000),"D:\\check.csv")



##### age_group & sex 나누기 ##### - zio
# subgroup 별 outcome column 생성해서 한번에 merge

# age group 1
day_asthma_age1 <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==1), c("date", "air_out_idx", "ASTHMA_out_total")]
colnames(day_asthma_age1) <- c("date","air_out_idx", "ASTHMA_out_total_age1")
# 여기서 subgroup, air_idx, date 별로 한번 더 aggregate 해야함!
day_asthma_age1 <- aggregate(day_asthma_age1[,"ASTHMA_out_total_age1"],
                                            by=list(day_asthma_age1$date,
                                                    day_asthma_age1$air_out_idx),sum)
colnames(day_asthma_age1) <- c("date","air_out_idx", "ASTHMA_out_total_age1")
summary(day_asthma_age1)

# age group 2
day_asthma_age2 <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==2), c("date", "air_out_idx", "ASTHMA_out_total")]                                                        
colnames(day_asthma_age2) <- c("date","air_out_idx", "ASTHMA_out_total_age2")
day_asthma_age2 <- aggregate(day_asthma_age2[,"ASTHMA_out_total_age2"],
                                            by=list(day_asthma_age2$date,
                                                    day_asthma_age2$air_out_idx),sum)    
colnames(day_asthma_age2) <- c("date","air_out_idx", "ASTHMA_out_total_age2")                                                    

# age group 3
day_asthma_age3 <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==3), c("date", "air_out_idx", "ASTHMA_out_total")]                                                 
colnames(day_asthma_age3) <- c("date","air_out_idx", "ASTHMA_out_total_age3")
day_asthma_age3 <- aggregate(day_asthma_age3[,"ASTHMA_out_total_age3"],
                                            by=list(day_asthma_age3$date,
                                                    day_asthma_age3$air_out_idx),sum)  
colnames(day_asthma_age3) <- c("date","air_out_idx", "ASTHMA_out_total_age3")

# age group 4
day_asthma_age4 <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==4), c("date", "air_out_idx", "ASTHMA_out_total")]
colnames(day_asthma_age4) <- c("date","air_out_idx", "ASTHMA_out_total_age4")
day_asthma_age4 <- aggregate(day_asthma_age4[,"ASTHMA_out_total_age4"],
                                            by=list(day_asthma_age4$date,
                                                    day_asthma_age4$air_out_idx),sum) 
colnames(day_asthma_age4) <- c("date","air_out_idx", "ASTHMA_out_total_age4")

# sex m
day_asthma_m <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "ASTHMA_out_total")]
colnames(day_asthma_m) <- c("date","air_out_idx", "ASTHMA_out_total_m")
day_asthma_m <- aggregate(day_asthma_m[,"ASTHMA_out_total_m"],
                                            by=list(day_asthma_m$date,
                                                    day_asthma_m$air_out_idx),sum)  
colnames(day_asthma_m) <- c("date","air_out_idx", "ASTHMA_out_total_m")

# sex f 
day_asthma_f <- day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "ASTHMA_out_total")]
colnames(day_asthma_f) <- c("date","air_out_idx", "ASTHMA_out_total_f")
day_asthma_f <- aggregate(day_asthma_f[,"ASTHMA_out_total_f"],
                                            by=list(day_asthma_f$date,
                                                    day_asthma_f$air_out_idx),sum)
colnames(day_asthma_f) <- c("date","air_out_idx", "ASTHMA_out_total_f")                                                    


# 0728 - 이하 sex, age 로 aggregate 한 그룹은 취소
  # # sex m age group 1 
  # day_asthma_m_age1<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==1&day_asthma_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_m_age1) <- c("date","air_out_idx", "ASTHMA_out_total_m_age1")
  # day_asthma_m_age1 <- aggregate(day_asthma_m_age1[,"ASTHMA_out_total_m_age1"],
  #                                             by=list(day_asthma_m_age1$date,
  #                                                     day_asthma_m_age1$air_out_idx),sum)
  # colnames(day_asthma_m_age1) <- c("date","air_out_idx", "ASTHMA_out_total_m_age1")                                                     

  # # sex m age group 2 
  # day_asthma_m_age2<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==2&day_asthma_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_m_age2) <- c("date","air_out_idx", "ASTHMA_out_total_m_age2")
  # day_asthma_m_age2 <- aggregate(day_asthma_m_age2[,"ASTHMA_out_total_m_age2"],
  #                                             by=list(day_asthma_m_age2$date,
  #                                                     day_asthma_m_age2$air_out_idx),sum) 
  # colnames(day_asthma_m_age2) <- c("date","air_out_idx", "ASTHMA_out_total_m_age2")

  # # sex m age group 3 
  # day_asthma_m_age3<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==3&day_asthma_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_m_age3) <- c("date","air_out_idx", "ASTHMA_out_total_m_age3")
  # day_asthma_m_age3 <- aggregate(day_asthma_m_age3[,"ASTHMA_out_total_m_age3"],
  #                                             by=list(day_asthma_m_age3$date,
  #                                                     day_asthma_m_age3$air_out_idx),sum)  
  # colnames(day_asthma_m_age3) <- c("date","air_out_idx", "ASTHMA_out_total_m_age3")

  # # sex m age group 4
  # day_asthma_m_age4<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==4&day_asthma_1317_agg_seoul_step$sex==1), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_m_age4) <- c("date","air_out_idx", "ASTHMA_out_total_m_age4")
  # day_asthma_m_age4 <- aggregate(day_asthma_m_age4[,"ASTHMA_out_total_m_age4"],
  #                                             by=list(day_asthma_m_age4$date,
  #                                                     day_asthma_m_age4$air_out_idx),sum)  
  # colnames(day_asthma_m_age4) <- c("date","air_out_idx", "ASTHMA_out_total_m_age4")

  # # sex f age group 1
  # day_asthma_f_age1<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==1&day_asthma_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_f_age1) <- c("date","air_out_idx", "ASTHMA_out_total_f_age1")
  # day_asthma_f_age1 <- aggregate(day_asthma_f_age1[,"ASTHMA_out_total_f_age1"],
  #                                             by=list(day_asthma_f_age1$date,
  #                                                     day_asthma_f_age1$air_out_idx),sum) 
  # colnames(day_asthma_f_age1) <- c("date","air_out_idx", "ASTHMA_out_total_f_age1")

  # # sex f age group 2
  # day_asthma_f_age2<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==2&day_asthma_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_f_age2) <- c("date","air_out_idx", "ASTHMA_out_total_f_age2")
  # day_asthma_f_age2 <- aggregate(day_asthma_f_age2[,"ASTHMA_out_total_f_age2"],
  #                                             by=list(day_asthma_f_age2$date,
  #                                                     day_asthma_f_age2$air_out_idx),sum)
  # colnames(day_asthma_f_age2) <- c("date","air_out_idx", "ASTHMA_out_total_f_age2")

  # # sex f age group 3
  # day_asthma_f_age3<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==3&day_asthma_1317_agg_seoul_step$sex==2), c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_f_age3) <- c("date","air_out_idx", "ASTHMA_out_total_f_age3")
  # day_asthma_f_age3 <- aggregate(day_asthma_f_age3[,"ASTHMA_out_total_f_age3"],
  #                                             by=list(day_asthma_f_age3$date,
  #                                                     day_asthma_f_age3$air_out_idx),sum) 
  # colnames(day_asthma_f_age3) <- c("date","air_out_idx", "ASTHMA_out_total_f_age3")

  # # sex f age group 4
  # day_asthma_f_age4<-day_asthma_1317_agg_seoul_step[which(day_asthma_1317_agg_seoul_step$age_group==4&day_asthma_1317_agg_seoul_step$sex==2),c("date", "air_out_idx", "ASTHMA_out_total")]
  # colnames(day_asthma_f_age4) <- c("date","air_out_idx", "ASTHMA_out_total_f_age4")
  # day_asthma_f_age4 <- aggregate(day_asthma_f_age4[,"ASTHMA_out_total_f_age4"],
  #                                             by=list(day_asthma_f_age4$date,
  #                                                     day_asthma_f_age4$air_out_idx),sum) 
  # colnames(day_asthma_f_age4) <- c("date","air_out_idx", "ASTHMA_out_total_f_age4")



# step2_final$yday <- yday(step2_final$date)
# step2_final$wday <- wday(step2_final$date)
# step2_final_back$wday=factor(step2_final_back$wday)
# step2_final_back$wday_mat=model.matrix( ~ wday - 1, data=step2_final_back)
# summary(lm(AQI ~ AQI_back+IV_300_BI,data=step2_final_back))
# model_formula = formula(ASTHMA_em_total~rain_sum+temp_mean_total +ns.basis_yday+wday_mat+AQI)
# instrument_formula =formula(AQI ~ AQI_back+IV_300_BI+rain_sum+temp_mean_total +ns.basis_yday+wday_mat)



##### IV ##### - zio
library(data.table)

find_instruments(model_formula,instrument_formula)

colnames(day_asthma_1317_agg_seoul_step)
dim(day_asthma_1317_agg_seoul_step)

day_asthma_1317_agg_seoul_step2<-na.omit(day_asthma_1317_agg_seoul_step)
# day_asthma_1317_agg_seoul_step2 <- copy(day_asthma_1317_agg_seoul_step)
dim(day_asthma_1317_agg_seoul_step2)
colnames(day_asthma_1317_agg_seoul_step)


## summary for paper
colnames(day_asthma_1317)

# 전체인원
sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total)
sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total)

dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
summary(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
str(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)

sd(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$NO2_mean)

age1_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$age_group==1),]
sd(age1_summary$ASTHMA_out_total)
sum(age1_summary$ASTHMA_out_total)
age2_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$age_group==2),]
sum(age2_summary$ASTHMA_out_total)
age3_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$age_group==3),]
sum(age3_summary$ASTHMA_out_total)
age4_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$age_group==4),]
sum(age4_summary$ASTHMA_out_total)
sexm_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$sex==1),]
sd(sexm_summary$ASTHMA_out_total)
sum(sexm_summary$ASTHMA_out_total)
sexf_summary <- day_asthma_1317_agg_seoul_step2[which(day_asthma_1317_agg_seoul_step2$sex==2),]
sd(sexf_summary$ASTHMA_out_total)
sum(sexf_summary$ASTHMA_out_total)

sum(sexm_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100
sum(sexf_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100

sum(age1_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100
sum(age2_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100
sum(age3_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100
sum(age4_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2$ASTHMA_out_total) * 100


age1_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$age_group==1),]
sum(age1_summary$RHINITIS_out_total)
age2_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$age_group==2),]
sum(age2_summary$RHINITIS_out_total)
age3_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$age_group==3),]
sum(age3_summary$RHINITIS_out_total)
age4_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$age_group==4),]
sum(age4_summary$RHINITIS_out_total)
sexm_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$sex==1),]
sum(sexm_summary$RHINITIS_out_total)
sexf_summary <- day_rhinitis_1317_agg_seoul_step2[which(day_rhinitis_1317_agg_seoul_step2$sex==2),]
sum(sexf_summary$RHINITIS_out_total)

sum(sexm_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100
sum(sexf_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100

sum(age1_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100
sum(age2_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100
sum(age3_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100
sum(age4_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2$RHINITIS_out_total) * 100
head(age4_summary)



# 2016-Nov only
# 이건 na_omit 하기 전인 day_asthma_1317_agg_seoul_step 로
day_asthma_1317_agg_seoul_step_2016_Nov <- day_asthma_1317_agg_seoul_step[substr(day_asthma_1317_agg_seoul_step$date, 1, 7) %in% c("2016-11"),]
# day_rhinitis_1317_agg_seoul_step_2016_Nov <- day_rhinitis_1317_agg_seoul_step[substr(day_rhinitis_1317_agg_seoul_step$date, 1, 7) %in% c("2016-11"),]

dim(day_asthma_1317_agg_seoul_step_2016_Nov)
dim(day_rhinitis_1317_agg_seoul_step_2016_Nov)

head(day_asthma_1317_agg_seoul_step_2016_Nov)
head(day_rhinitis_1317_agg_seoul_step_2016_Nov)

sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total)
sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total)

## pollutant descriptive는 1_data_preprocessing으로

# summary(day_asthma_1317_agg_seoul_step_2016_Nov_humi_pressure_agg)
# sd(day_asthma_1317_agg_seoul_step_2016_Nov_humi_pressure_agg$pressure_mean)

age1_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$age_group==1),]
sum(age1_summary$ASTHMA_out_total)
age2_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$age_group==2),]
sum(age2_summary$ASTHMA_out_total)
age3_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$age_group==3),]
sum(age3_summary$ASTHMA_out_total)
age4_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$age_group==4),]
sum(age4_summary$ASTHMA_out_total)
sexm_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$sex==1),]
sum(sexm_summary$ASTHMA_out_total)
sexf_summary <- day_asthma_1317_agg_seoul_step_2016_Nov[which(day_asthma_1317_agg_seoul_step_2016_Nov$sex==2),]
sum(sexf_summary$ASTHMA_out_total)

sum(sexm_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100
sum(sexf_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100

sum(age1_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100
sum(age2_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100
sum(age3_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100
sum(age4_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step_2016_Nov$ASTHMA_out_total) * 100


age1_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$age_group==1),]
sum(age1_summary$RHINITIS_out_total)
age2_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$age_group==2),]
sum(age2_summary$RHINITIS_out_total)
age3_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$age_group==3),]
sum(age3_summary$RHINITIS_out_total)
age4_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$age_group==4),]
sum(age4_summary$RHINITIS_out_total)
sexm_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$sex==1),]
sum(sexm_summary$RHINITIS_out_total)
sexf_summary <- day_rhinitis_1317_agg_seoul_step_2016_Nov[which(day_rhinitis_1317_agg_seoul_step_2016_Nov$sex==2),]
sum(sexf_summary$RHINITIS_out_total)

sum(sexm_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100
sum(sexf_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100

sum(age1_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100
sum(age2_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100
sum(age3_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100
sum(age4_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step_2016_Nov$RHINITIS_out_total) * 100
head(age4_summary)




# 14_15_17-Nov only

day_asthma_1317_agg_seoul_step2_14_15_17_Nov <- day_asthma_1317_agg_seoul_step2[substr(day_asthma_1317_agg_seoul_step2$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]
day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov <- day_rhinitis_1317_agg_seoul_step2[substr(day_rhinitis_1317_agg_seoul_step2$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]

dim(day_asthma_1317_agg_seoul_step2_14_15_17_Nov)
dim(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov)

sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total)
sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total)

summary(day_asthma_1317_agg_seoul_step2_14_15_17_Nov_humi_pressure_agg)
sd(day_asthma_1317_agg_seoul_step2_14_15_17_Nov_humi_pressure_agg$pressure_mean)

age1_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$age_group==1),]
sum(age1_summary$ASTHMA_out_total)
age2_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$age_group==2),]
sum(age2_summary$ASTHMA_out_total)
age3_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$age_group==3),]
sum(age3_summary$ASTHMA_out_total)
age4_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$age_group==4),]
sum(age4_summary$ASTHMA_out_total)
sexm_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$sex==1),]
sum(sexm_summary$ASTHMA_out_total)
sexf_summary <- day_asthma_1317_agg_seoul_step2_14_15_17_Nov[which(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$sex==2),]
sum(sexf_summary$ASTHMA_out_total)

sum(sexm_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100
sum(sexf_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100

sum(age1_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100
sum(age2_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100
sum(age3_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100
sum(age4_summary$ASTHMA_out_total) / sum(day_asthma_1317_agg_seoul_step2_14_15_17_Nov$ASTHMA_out_total) * 100


age1_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$age_group==1),]
sum(age1_summary$RHINITIS_out_total)
age2_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$age_group==2),]
sum(age2_summary$RHINITIS_out_total)
age3_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$age_group==3),]
sum(age3_summary$RHINITIS_out_total)
age4_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$age_group==4),]
sum(age4_summary$RHINITIS_out_total)
sexm_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$sex==1),]
sum(sexm_summary$RHINITIS_out_total)
sexf_summary <- day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov[which(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$sex==2),]
sum(sexf_summary$RHINITIS_out_total)

sum(sexm_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100
sum(sexf_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100

sum(age1_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100
sum(age2_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100
sum(age3_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100
sum(age4_summary$RHINITIS_out_total) / sum(day_rhinitis_1317_agg_seoul_step2_14_15_17_Nov$RHINITIS_out_total) * 100
head(age4_summary)




# iv table
day_asthma_1317_agg_seoul_step2_humi_pressure_agg$month = substr(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$DT, 5, 6)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg$year = substr(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$DT, 1, 4)

head(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)


table(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$month, day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)

table(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$year, day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)

head(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)


############### end. ###############



day_asthma_1317_agg_seoul_step2$wday <- as.factor(day_asthma_1317_agg_seoul_step2$wday)
day_asthma_1317_agg_seoul_step2$age_group = factor(day_asthma_1317_agg_seoul_step2$age_group)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_asthma_1317_agg_seoul_step2,family =quasipoisson, link = 'log')
summary(glm_iv)
# glm_iv$instrumented
# glm_iv$instruments
# glm_iv$exclusion_restriction
# glm_iv$instrument_validity
# glm_iv$stage_one
# diagnose(glm_iv)

# h <- day_asthma_1317_agg_seoul_step2[,c("ASTHMA_out_total","rain_sum","ns.basis_temp","ns.basis_yday","wday","sex","age_group","AQI","AQI_back","IV_300_BI")]
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
# write.csv(h,"asthma_iv.csv")
# stata code ===
# ivpoisson gmm asthma_out_total rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 (aqi = aqi_back iv_300_bi)

# 
# 
# 
fit_1_asthma <- lm(AQI~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+AQI_back+IV_300_BI,data = day_asthma_1317_agg_seoul_step2_humi)
summary(fit_1_asthma)
# 
fit_1_asthma2 <- lm(AQI~AQI_back+IV_300_BI,data = day_asthma_1317_agg_seoul_step2_dust)
summary(fit_1_asthma2)
fit_1_asthma2 <- lm(AQI~AQI_back+IV_300_BI+dust_yn,data = day_asthma_1317_agg_seoul_step2_dust)
summary(fit_1_asthma2)


##### 황사 추가 -> 안하는걸로 #####
# dust <- read_excel("02_22_dust.xlsx")
# colnames(dust)[1] <- "date_yj"
# dust$date = paste0(substr(dust$date_yj,1,4),"-",substr(dust$date_yj,5,6),"-",substr(dust$date_yj,7,8))
# dust_1417 <- dust[substr(dust$date,1,4) %in% c("2014","2015","2016","2017"),]
# day_asthma_1317_agg_seoul_step2_dust <- merge(day_asthma_1317_agg_seoul_step2,dust_1417,by="date",all.x = T)
# day_asthma_1317_agg_seoul_step2_dust$dust_yn <- as.numeric(day_asthma_1317_agg_seoul_step2_dust$dust_yn)
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+AQI,
#                  instrument_formula = AQI ~ AQI_back+IV_300_BI+dust_yn,
#                  data=day_asthma_1317_agg_seoul_step2_dust,family =quasipoisson, link = 'log')
# summary(glm_iv)


##### 습도 추가 #####  - zio
# hum <- weather[,c("date","air_out_idx","humi_mean_total")]
summary(hum)
dim(hum)
dim(hum[hum$humi_mean_total < 10,])
summary(hum[hum$humi_mean_total >= 0,])
day_asthma_1317_agg_seoul_step2_humi <- merge(day_asthma_1317_agg_seoul_step2, hum, by=c("air_out_idx","date"))
summary(day_asthma_1317_agg_seoul_step2_humi)
day_asthma_1317_agg_seoul_step2_humi <- day_asthma_1317_agg_seoul_step2_humi[day_asthma_1317_agg_seoul_step2_humi$humi_mean_total.x >= 0, ]
colnames(day_asthma_1317_agg_seoul_step2_humi)
colSums(is.na(day_asthma_1317_agg_seoul_step2_humi))
summary(day_asthma_1317_agg_seoul_step2_humi$humi_mean_total.x)
kno_hum <- equalknots(day_asthma_1317_agg_seoul_step2_humi$humi_mean_total.x,nk=2)
ns.basis_hum <- crossbasis(day_asthma_1317_agg_seoul_step2_humi$humi_mean_total.x,argvar=list(knots=kno_hum),
                            group=day_asthma_1317_agg_seoul_step2_humi$air_out_idx)
day_asthma_1317_agg_seoul_step2_humi$ns.basis_hum <- crossbasis(day_asthma_1317_agg_seoul_step2_humi$humi_mean_total.x,argvar=list(knots=kno_hum),
                                                           group=day_asthma_1317_agg_seoul_step2_humi$air_out_idx)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ns.basis_temp +ns.basis_yday+wday+sex+age_group+ns.basis_hum+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_asthma_1317_agg_seoul_step2_humi,family =quasipoisson, link = 'log')
summary(glm_iv)

summary(day_asthma_1317_agg_seoul_step2_humi)
##### 기압, 적설, 일사, 일조 추가 #####
# 2014
# pressure_snow_sunshine_2014 <- read.csv("pressure_snow_sunshine_2014.csv")
# pressure_snow_sunshine_2014$지점 <- NULL
# pressure_snow_sunshine_2014$지점명 <- NULL
# colnames(pressure_snow_sunshine_2014)[2:5]<- c("pressure","sunshine","insolation","snow")
# pressure_snow_sunshine_2014$sunshine[is.na(pressure_snow_sunshine_2014$sunshine)] <- 0 
# pressure_snow_sunshine_2014$insolation[is.na(pressure_snow_sunshine_2014$insolation)] <- 0 
# pressure_snow_sunshine_2014$snow[is.na(pressure_snow_sunshine_2014$snow)] <- 0 
# pressure_snow_sunshine_2014$date = substr(pressure_snow_sunshine_2014$일시,1,10)
# pressure_snow_sunshine_2014_agg_1=aggregate(pressure_snow_sunshine_2014[,2],
#                                             by=list(pressure_snow_sunshine_2014$date),mean,na.rm=T)
# colnames(pressure_snow_sunshine_2014_agg_1) <- c("date","pressure_mean")
# pressure_snow_sunshine_2014_agg_2=aggregate(pressure_snow_sunshine_2014[,3:4],
#                                           by=list(pressure_snow_sunshine_2014$date),sum)
# colnames(pressure_snow_sunshine_2014_agg_2) <- c("date","sunshine_sum","insolation_sum")
# pressure_snow_sunshine_2014_agg_3=aggregate(pressure_snow_sunshine_2014[,5],
#                                             by=list(pressure_snow_sunshine_2014$date),max)
# colnames(pressure_snow_sunshine_2014_agg_3) <- c("date","snow_max")
# pressure_snow_sunshine_2014_agg <- merge(pressure_snow_sunshine_2014_agg_1,pressure_snow_sunshine_2014_agg_2, by="date", all.x=T)
# pressure_snow_sunshine_2014_agg <- merge(pressure_snow_sunshine_2014_agg,pressure_snow_sunshine_2014_agg_3, by="date", all.x=T)
# 
# # 2015
# pressure_snow_sunshine_2015 <- read.csv("pressure_snow_sunshine_2015.csv")
# pressure_snow_sunshine_2015$지점 <- NULL
# pressure_snow_sunshine_2015$지점명 <- NULL
# colnames(pressure_snow_sunshine_2015)[2:5]<- c("pressure","sunshine","insolation","snow")
# pressure_snow_sunshine_2015$sunshine[is.na(pressure_snow_sunshine_2015$sunshine)] <- 0 
# pressure_snow_sunshine_2015$insolation[is.na(pressure_snow_sunshine_2015$insolation)] <- 0 
# pressure_snow_sunshine_2015$snow[is.na(pressure_snow_sunshine_2015$snow)] <- 0 
# pressure_snow_sunshine_2015$date = substr(pressure_snow_sunshine_2015$일시,1,10)
# pressure_snow_sunshine_2015_agg_1=aggregate(pressure_snow_sunshine_2015[,2],
#                                             by=list(pressure_snow_sunshine_2015$date),mean,na.rm=T)
# colnames(pressure_snow_sunshine_2015_agg_1) <- c("date","pressure_mean")
# pressure_snow_sunshine_2015_agg_2=aggregate(pressure_snow_sunshine_2015[,3:4],
#                                             by=list(pressure_snow_sunshine_2015$date),sum)
# colnames(pressure_snow_sunshine_2015_agg_2) <- c("date","sunshine_sum","insolation_sum")
# pressure_snow_sunshine_2015_agg_3=aggregate(pressure_snow_sunshine_2015[,5],
#                                             by=list(pressure_snow_sunshine_2015$date),max)
# colnames(pressure_snow_sunshine_2015_agg_3) <- c("date","snow_max")
# pressure_snow_sunshine_2015_agg <- merge(pressure_snow_sunshine_2015_agg_1,pressure_snow_sunshine_2015_agg_2, by="date", all.x=T)
# pressure_snow_sunshine_2015_agg <- merge(pressure_snow_sunshine_2015_agg,pressure_snow_sunshine_2015_agg_3, by="date", all.x=T)
# 
# 
# # 2016
# pressure_snow_sunshine_2016 <- read.csv("pressure_snow_sunshine_2016.csv")
# pressure_snow_sunshine_2016$지점 <- NULL
# pressure_snow_sunshine_2016$지점명 <- NULL
# colnames(pressure_snow_sunshine_2016)[2:5]<- c("pressure","sunshine","insolation","snow")
# pressure_snow_sunshine_2016$sunshine[is.na(pressure_snow_sunshine_2016$sunshine)] <- 0 
# pressure_snow_sunshine_2016$insolation[is.na(pressure_snow_sunshine_2016$insolation)] <- 0 
# pressure_snow_sunshine_2016$snow[is.na(pressure_snow_sunshine_2016$snow)] <- 0 
# pressure_snow_sunshine_2016$date = substr(pressure_snow_sunshine_2016$일시,1,10)
# pressure_snow_sunshine_2016_agg_1=aggregate(pressure_snow_sunshine_2016[,2],
#                                             by=list(pressure_snow_sunshine_2016$date),mean,na.rm=T)
# colnames(pressure_snow_sunshine_2016_agg_1) <- c("date","pressure_mean")
# pressure_snow_sunshine_2016_agg_2=aggregate(pressure_snow_sunshine_2016[,3:4],
#                                             by=list(pressure_snow_sunshine_2016$date),sum)
# colnames(pressure_snow_sunshine_2016_agg_2) <- c("date","sunshine_sum","insolation_sum")
# pressure_snow_sunshine_2016_agg_3=aggregate(pressure_snow_sunshine_2016[,5],
#                                             by=list(pressure_snow_sunshine_2016$date),max)
# colnames(pressure_snow_sunshine_2016_agg_3) <- c("date","snow_max")
# pressure_snow_sunshine_2016_agg <- merge(pressure_snow_sunshine_2016_agg_1,pressure_snow_sunshine_2016_agg_2, by="date", all.x=T)
# pressure_snow_sunshine_2016_agg <- merge(pressure_snow_sunshine_2016_agg,pressure_snow_sunshine_2016_agg_3, by="date", all.x=T)
# 
# # 2017
# pressure_snow_sunshine_2017 <- read.csv("pressure_snow_sunshine_2017.csv")
# pressure_snow_sunshine_2017$지점 <- NULL
# pressure_snow_sunshine_2017$지점명 <- NULL
# colnames(pressure_snow_sunshine_2017)[2:5]<- c("pressure","sunshine","insolation","snow")
# pressure_snow_sunshine_2017$sunshine[is.na(pressure_snow_sunshine_2017$sunshine)] <- 0 
# pressure_snow_sunshine_2017$insolation[is.na(pressure_snow_sunshine_2017$insolation)] <- 0 
# pressure_snow_sunshine_2017$snow[is.na(pressure_snow_sunshine_2017$snow)] <- 0 
# pressure_snow_sunshine_2017$date = substr(pressure_snow_sunshine_2017$일시,1,10)
# pressure_snow_sunshine_2017_agg_1=aggregate(pressure_snow_sunshine_2017[,2],
#                                             by=list(pressure_snow_sunshine_2017$date),mean,na.rm=T)
# colnames(pressure_snow_sunshine_2017_agg_1) <- c("date","pressure_mean")
# pressure_snow_sunshine_2017_agg_2=aggregate(pressure_snow_sunshine_2017[,3:4],
#                                             by=list(pressure_snow_sunshine_2017$date),sum)
# colnames(pressure_snow_sunshine_2017_agg_2) <- c("date","sunshine_sum","insolation_sum")
# pressure_snow_sunshine_2017_agg_3=aggregate(pressure_snow_sunshine_2017[,5],
#                                             by=list(pressure_snow_sunshine_2017$date),max)
# colnames(pressure_snow_sunshine_2017_agg_3) <- c("date","snow_max")
# pressure_snow_sunshine_2017_agg <- merge(pressure_snow_sunshine_2017_agg_1,pressure_snow_sunshine_2017_agg_2, by="date", all.x=T)
# pressure_snow_sunshine_2017_agg <- merge(pressure_snow_sunshine_2017_agg,pressure_snow_sunshine_2017_agg_3, by="date", all.x=T)
# pressure_snow_sunshine_1417 <- rbind(pressure_snow_sunshine_2014_agg,pressure_snow_sunshine_2015_agg,pressure_snow_sunshine_2016_agg,pressure_snow_sunshine_2017_agg)
day_asthma_1317_agg_seoul_step2_humi_pressure <- merge(day_asthma_1317_agg_seoul_step2_humi,pressure_snow_sunshine_1417,by="date",all.x=T)
head(pressure_snow_sunshine_1417)
colSums(is.na(pressure_snow_sunshine_1417))
summary(pressure_snow_sunshine_1417$pressure_mean)
summary(day_asthma_1317_agg_seoul_step2_humi_pressure)

pressure_snow_sunshine_16_Nov <- pressure_snow_sunshine_1417[substr(pressure_snow_sunshine_1417$date, 1, 7) %in% c("2016-11"),]
dim(pressure_snow_sunshine_16_Nov)
summary(pressure_snow_sunshine_16_Nov$pressure_mean)
sd(pressure_snow_sunshine_16_Nov$pressure_mean)

pressure_snow_sunshine_14_15_17_Nov <- pressure_snow_sunshine_1417[substr(pressure_snow_sunshine_1417$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]
dim(pressure_snow_sunshine_14_15_17_Nov)
colSums(is.na(pressure_snow_sunshine_14_15_17_Nov))
summary(pressure_snow_sunshine_14_15_17_Nov$pressure_mean)
sd(pressure_snow_sunshine_14_15_17_Nov$pressure_mean)


kno_pressure <- equalknots(day_asthma_1317_agg_seoul_step2_humi_pressure$pressure_mean,nk=2)
ns.basis_pressure <- crossbasis(day_asthma_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                           group=day_asthma_1317_agg_seoul_step2_humi_pressure$air_out_idx)
day_asthma_1317_agg_seoul_step2_humi_pressure$ns.basis_pressure <- crossbasis(day_asthma_1317_agg_seoul_step2_humi_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                                                group=day_asthma_1317_agg_seoul_step2_humi_pressure$air_out_idx)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ ns.basis_temp+ns.basis_yday+wday+sex+age_group
                 +ns.basis_hum+ns.basis_pressure+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure,family =quasipoisson, link = 'log')
summary(glm_iv)
table(day_asthma_1317_agg_seoul_step2_humi_pressure$IV_300_BI)
summary(day_asthma_1317_agg_seoul_step2_humi_pressure)
count(day_asthma_1317_agg_seoul_step2_humi_pressure[day_asthma_1317_agg_seoul_step2_humi_pressure$humi_mean_total < 0, ])



#####age sex agg #####
asthma_agg <- aggregate(day_asthma_1317_agg_seoul_step2_humi_pressure[,"ASTHMA_out_total"],
                                                          by=list(day_asthma_1317_agg_seoul_step2_humi_pressure$date,
                                                                  day_asthma_1317_agg_seoul_step2_humi_pressure$air_out_idx),sum)
colnames(asthma_agg) <- c("date","air_out_idx","ASTHMA_out_total_agg")

# 0808 asthma_em 추가
asthma_em_agg <- aggregate(day_asthma_1317_agg_seoul_step2_humi_pressure[,"ASTHMA_em_total"],
                                                          by=list(day_asthma_1317_agg_seoul_step2_humi_pressure$date,
                                                                  day_asthma_1317_agg_seoul_step2_humi_pressure$air_out_idx),sum)
colnames(asthma_em_agg) <- c("date","air_out_idx","ASTHMA_em_total_agg")

# day_asthma_1317_agg_seoul_step2_humi_pressure_agg <- merge(asthma_agg,day_asthma_1317_agg_seoul_step2_humi_pressure[,c(1,2,14:82)],  by=c("date","air_out_idx"),all.x = T)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg <- merge(asthma_agg,day_asthma_1317_agg_seoul_step2_humi_pressure,  by=c("date","air_out_idx"),all.x = T)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg <- merge(asthma_em_agg,day_asthma_1317_agg_seoul_step2_humi_pressure_agg,  by=c("date","air_out_idx"),all.x = T)

air_dups <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[c("air_out_idx", "date")]
day_asthma_1317_agg_seoul_step2_humi_pressure_agg <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[!duplicated(air_dups),]
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_lead365,
                 instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

count(day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$humi_mean_total <0, ])
summary(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$ASTHMA_out_total_agg)

# # 0718 humidity < 0 삭제 후 다시 save
# save.image("220718.RData")


# 0719 melbourne 데이터 추가
mel_asthma <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, mel_final, by=c('date'), all.x = T)
# mel IV 붙이지 말고 AQI만 쓰자
mel_asthma <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, mel_air_not_na, by=c('date'), all.x = T)

dim(mel_asthma) # 33206 68

# wind 추가
mel_asthma <- merge(mel_asthma, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(mel_asthma)

mel_asthma_not_na <- na.omit(mel_asthma)
dim(mel_asthma_not_na) # 32453 96
# mel_asthma_not_na$sin <- sin(2 * 3.14/365.25 * mel_asthma_not_na$yday)
# mel_asthma_not_na$cos <- cos(2 * 3.14/365.25 * mel_asthma_not_na$yday)

# plot(mel_asthma_not_na$yday,mel_asthma_not_na$sin)
# plot(mel_asthma_not_na$yday,mel_asthma_not_na$cos)

# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+sin+cos+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=mel_asthma_not_na,family =quasipoisson, link = 'log')
# summary(glm_iv)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_mel,
                 instrument_formula = AQI_mel ~ IV_300_mel,
                 data=mel_asthma_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)
colnames(mel_asthma_not_na)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=mel_asthma_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure + AQI + AQI_mel,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=mel_asthma_not_na,family =quasipoisson, link = 'log')
summary(glm_iv)

tail(mel_asthma_not_na)
cbind(lapply(lapply(mel_asthma_not_na, is.na), sum))
summary(mel_asthma_not_na$AQI)
summary(mel_asthma_not_na$AQI_mel)
dim(mel_asthma_not_na)
colnames(mel_asthma_not_na)

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
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", "AQI", "IV_300_BI", 
          "AQI_mel","AQI_mel_lag1","AQI_mel_lag2","AQI_mel_lag3","AQI_mel_lag4","AQI_mel_lag5","AQI_mel_lag6","AQI_mel_lag7",
          # "IV_300_mel","IV_300_mel_lag1","IV_300_mel_lag2","IV_300_mel_lag3","IV_300_mel_lag4","IV_300_mel_lag5","IV_300_mel_lag6","IV_300_mel_lag7", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am")]
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
mel_lag <- create_mel_lag(mel_asthma_not_na)
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
# write.csv(h,"thermal_inversion_0623_share\\dump\\asthma_iv_neg_control_lag0_test.csv") # 0715 test 
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag0.csv")

# lag 1
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag1) == F,]
h <- h[is.na(h$AQI_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag1.csv")

# lag 2
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag2) == F,]
h <- h[is.na(h$AQI_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag2.csv")

# lag 3
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag3) == F,]
h <- h[is.na(h$AQI_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag3.csv")

# lag 4
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag4) == F,]
h <- h[is.na(h$AQI_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag4.csv")

# lag 5
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag5) == F,]
h <- h[is.na(h$AQI_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag5.csv")

# lag 6
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag6) == F,]
h <- h[is.na(h$AQI_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag6.csv")

# lag 7
h <- create_h(mel_lag)
h <- h[is.na(h$AQI_mel_lag7) == F,]
h <- h[is.na(h$AQI_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_melbourne_lag7.csv")

dim(h)







colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)

cor(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$insolation_sum, day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)
test <- glm(IV_300_BI ~ insolation_sum, data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg, family = "binomial")
summary(test)

cor(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sunshine_sum, day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)
test <- glm(IV_300_BI ~ sunshine_sum, data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg, family = "binomial")
summary(test)

summary(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sunshine_sum)




# 0714 365 lead 도 lag 0~7 추가 - zio
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
dim(asthma_lead_365)
colnames(asthma_lead_365)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, asthma_lead_365[, c('air_out_idx', 'date', 'AQI_lead365')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365) # 33206 56

# wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365 <- na.omit(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365) # 30847 56


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
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "AQI_lead365","AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7",
          # "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am"
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
#   h <- df[,c("air_out_idx", "date", "ASTHMA_out_total_agg", 
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
lead365_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_365)
lead365_lag <- create_lag_365(lead365_lag)


setwd("D:\\SNUlab\\")
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
# write.csv(h,"thermal_inversion_0623_share\\dump\\asthma_iv_neg_control_lag0_test.csv") # 0715 test 
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag0.csv")

# lag 1
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag1.csv")

# lag 2
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag2.csv")

# lag 3
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag3.csv")

# lag 4
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag4.csv")

# lag 5
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag5.csv")

# lag 6
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag6.csv")

# lag 7
h <- create_h_365(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_365_neg_control_lag7.csv")




# 0715 create df with index
# lag 0
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365) == F,]
summary(h$AQI_lead365)
summary(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_lead365)
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag0.csv")

# lag 1
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag1.csv")

# lag 2
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag2.csv")

# lag 3
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag3.csv")

# lag 4
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag4.csv")

# lag 5
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag5.csv")

# lag 6
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag6.csv")

# lag 7
h <- create_h_with_index(lead365_lag)
h <- h[is.na(h$AQI_lead365_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag7.csv")







# 0714 182 lead 추가 - zio

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, asthma_lead_182[, c('air_out_idx', 'date', 'AQI_lead182')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182) # 33206 56

# wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182 <- na.omit(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182) # 30760 56

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

create_lag_182 <- function(df) {
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

    # mutate(IV_300_BI_lead182_lag1=lag(IV_300_BI_lead182)) %>%
    # mutate(IV_300_BI_lead182_lag2=lag(IV_300_BI_lead182,2)) %>%
    # mutate(IV_300_BI_lead182_lag3=lag(IV_300_BI_lead182,3)) %>%
    # mutate(IV_300_BI_lead182_lag4=lag(IV_300_BI_lead182,4)) %>%
    # mutate(IV_300_BI_lead182_lag5=lag(IV_300_BI_lead182,5)) %>%
    # mutate(IV_300_BI_lead182_lag6=lag(IV_300_BI_lead182,6)) %>%
    # mutate(IV_300_BI_lead182_lag7=lag(IV_300_BI_lead182,7)) %>%
    # mutate(IV_300_BI_lead182_lag8=lag(IV_300_BI_lead182,8)) %>%
    # mutate(IV_300_BI_lead182_lag9=lag(IV_300_BI_lead182,9)) %>%
    # mutate(IV_300_BI_lead182_lag10=lag(IV_300_BI_lead182,10)) %>%
    # mutate(IV_300_BI_lead182_lag11=lag(IV_300_BI_lead182,11)) %>%
    # mutate(IV_300_BI_lead182_lag12=lag(IV_300_BI_lead182,12)) %>%
    # mutate(IV_300_BI_lead182_lag13=lag(IV_300_BI_lead182,13)) %>%
    # mutate(IV_300_BI_lead182_lag14=lag(IV_300_BI_lead182,14)) %>%
    # mutate(IV_300_BI_lead182_lag15=lag(IV_300_BI_lead182,15)) %>%
    # mutate(IV_300_BI_lead182_lag16=lag(IV_300_BI_lead182,16)) %>%
    # mutate(IV_300_BI_lead182_lag17=lag(IV_300_BI_lead182,17)) %>%
    # mutate(IV_300_BI_lead182_lag18=lag(IV_300_BI_lead182,18)) %>%
    # mutate(IV_300_BI_lead182_lag19=lag(IV_300_BI_lead182,19)) %>%
    # mutate(IV_300_BI_lead182_lag20=lag(IV_300_BI_lead182,20)) %>%
    # mutate(IV_300_BI_lead182_lag21=lag(IV_300_BI_lead182,21)) %>%
    # mutate(IV_300_BI_lead182_lag22=lag(IV_300_BI_lead182,22)) %>%
    # mutate(IV_300_BI_lead182_lag23=lag(IV_300_BI_lead182,23)) %>%
    # mutate(IV_300_BI_lead182_lag24=lag(IV_300_BI_lead182,24)) %>%
    # mutate(IV_300_BI_lead182_lag25=lag(IV_300_BI_lead182,25)) %>%
    # mutate(IV_300_BI_lead182_lag26=lag(IV_300_BI_lead182,26)) %>%
    # mutate(IV_300_BI_lead182_lag27=lag(IV_300_BI_lead182,27)) %>%
    # mutate(IV_300_BI_lead182_lag28=lag(IV_300_BI_lead182,28)) %>%
    # mutate(IV_300_BI_lead182_lag29=lag(IV_300_BI_lead182,29)) %>%
    # mutate(IV_300_BI_lead182_lag30=lag(IV_300_BI_lead182,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7",
          "AQI_lead182","AQI_lead182_lag1","AQI_lead182_lag2","AQI_lead182_lag3","AQI_lead182_lag4","AQI_lead182_lag5","AQI_lead182_lag6","AQI_lead182_lag7",
          # "IV_300_BI_lead182","IV_300_BI_lead182_lag1","IV_300_BI_lead182_lag2","IV_300_BI_lead182_lag3","IV_300_BI_lead182_lag4","IV_300_BI_lead182_lag5","IV_300_BI_lead182_lag6","IV_300_BI_lead182_lag7", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am")]
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
lead182_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_182)
lead182_lag <- create_lag_182(lead182_lag)
colnames(lead182_lag)

# lag 0
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182) == F,]
summary(h$AQI_lead182)
summary(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_lead182)
h <- na.omit(h)
dim(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag0.csv")

# lag 1
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag1.csv")

# lag 2
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag2.csv")

# lag 3
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag3.csv")

# lag 4
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag4.csv")

# lag 5
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag5.csv")

# lag 6
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag6.csv")

# lag 7
h <- create_h(lead182_lag)
h <- h[is.na(h$AQI_lead182_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_182_lag7.csv")





# 0714 92 lead 추가 - zio

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, asthma_lead_92[, c('air_out_idx', 'date', 'AQI_lead92')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92) # 33206 56

# wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92 <- na.omit(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92) # 30949 56

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

create_lag_92 <- function(df) {
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

    # mutate(IV_300_BI_lead92_lag1=lag(IV_300_BI_lead92)) %>%
    # mutate(IV_300_BI_lead92_lag2=lag(IV_300_BI_lead92,2)) %>%
    # mutate(IV_300_BI_lead92_lag3=lag(IV_300_BI_lead92,3)) %>%
    # mutate(IV_300_BI_lead92_lag4=lag(IV_300_BI_lead92,4)) %>%
    # mutate(IV_300_BI_lead92_lag5=lag(IV_300_BI_lead92,5)) %>%
    # mutate(IV_300_BI_lead92_lag6=lag(IV_300_BI_lead92,6)) %>%
    # mutate(IV_300_BI_lead92_lag7=lag(IV_300_BI_lead92,7)) %>%
    # mutate(IV_300_BI_lead92_lag8=lag(IV_300_BI_lead92,8)) %>%
    # mutate(IV_300_BI_lead92_lag9=lag(IV_300_BI_lead92,9)) %>%
    # mutate(IV_300_BI_lead92_lag10=lag(IV_300_BI_lead92,10)) %>%
    # mutate(IV_300_BI_lead92_lag11=lag(IV_300_BI_lead92,11)) %>%
    # mutate(IV_300_BI_lead92_lag12=lag(IV_300_BI_lead92,12)) %>%
    # mutate(IV_300_BI_lead92_lag13=lag(IV_300_BI_lead92,13)) %>%
    # mutate(IV_300_BI_lead92_lag14=lag(IV_300_BI_lead92,14)) %>%
    # mutate(IV_300_BI_lead92_lag15=lag(IV_300_BI_lead92,15)) %>%
    # mutate(IV_300_BI_lead92_lag16=lag(IV_300_BI_lead92,16)) %>%
    # mutate(IV_300_BI_lead92_lag17=lag(IV_300_BI_lead92,17)) %>%
    # mutate(IV_300_BI_lead92_lag18=lag(IV_300_BI_lead92,18)) %>%
    # mutate(IV_300_BI_lead92_lag19=lag(IV_300_BI_lead92,19)) %>%
    # mutate(IV_300_BI_lead92_lag20=lag(IV_300_BI_lead92,20)) %>%
    # mutate(IV_300_BI_lead92_lag21=lag(IV_300_BI_lead92,21)) %>%
    # mutate(IV_300_BI_lead92_lag22=lag(IV_300_BI_lead92,22)) %>%
    # mutate(IV_300_BI_lead92_lag23=lag(IV_300_BI_lead92,23)) %>%
    # mutate(IV_300_BI_lead92_lag24=lag(IV_300_BI_lead92,24)) %>%
    # mutate(IV_300_BI_lead92_lag25=lag(IV_300_BI_lead92,25)) %>%
    # mutate(IV_300_BI_lead92_lag26=lag(IV_300_BI_lead92,26)) %>%
    # mutate(IV_300_BI_lead92_lag27=lag(IV_300_BI_lead92,27)) %>%
    # mutate(IV_300_BI_lead92_lag28=lag(IV_300_BI_lead92,28)) %>%
    # mutate(IV_300_BI_lead92_lag29=lag(IV_300_BI_lead92,29)) %>%
    # mutate(IV_300_BI_lead92_lag30=lag(IV_300_BI_lead92,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI_lead92","AQI_lead92_lag1","AQI_lead92_lag2","AQI_lead92_lag3","AQI_lead92_lag4","AQI_lead92_lag5","AQI_lead92_lag6","AQI_lead92_lag7",
          # "IV_300_BI_lead92","IV_300_BI_lead92_lag1","IV_300_BI_lead92_lag2","IV_300_BI_lead92_lag3","IV_300_BI_lead92_lag4","IV_300_BI_lead92_lag5","IV_300_BI_lead92_lag6","IV_300_BI_lead92_lag7"
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7",
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am")]
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
lead92_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_92)
lead92_lag <- create_lag_92(lead92_lag)

# lag 0
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92) == F,]
summary(h$AQI_lead92)
summary(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_lead92)
h <- na.omit(h)
dim(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag0.csv")

# lag 1
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag1.csv")

# lag 2
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag2.csv")

# lag 3
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag3.csv")

# lag 4
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag4.csv")

# lag 5
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag5.csv")

# lag 6
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag6.csv")

# lag 7
h <- create_h(lead92_lag)
h <- h[is.na(h$AQI_lead92_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_92_lag7.csv")





# 0714 730 lead 추가 - zio

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, asthma_lead_730[, c('air_out_idx', 'date', 'AQI_lead730')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730) # 33206 56

# wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730 <- na.omit(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730) # 22818 56

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

create_lag_730 <- function(df) {
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

    # mutate(IV_300_BI_lead730_lag1=lag(IV_300_BI_lead730)) %>%
    # mutate(IV_300_BI_lead730_lag2=lag(IV_300_BI_lead730,2)) %>%
    # mutate(IV_300_BI_lead730_lag3=lag(IV_300_BI_lead730,3)) %>%
    # mutate(IV_300_BI_lead730_lag4=lag(IV_300_BI_lead730,4)) %>%
    # mutate(IV_300_BI_lead730_lag5=lag(IV_300_BI_lead730,5)) %>%
    # mutate(IV_300_BI_lead730_lag6=lag(IV_300_BI_lead730,6)) %>%
    # mutate(IV_300_BI_lead730_lag7=lag(IV_300_BI_lead730,7)) %>%
    # mutate(IV_300_BI_lead730_lag8=lag(IV_300_BI_lead730,8)) %>%
    # mutate(IV_300_BI_lead730_lag9=lag(IV_300_BI_lead730,9)) %>%
    # mutate(IV_300_BI_lead730_lag10=lag(IV_300_BI_lead730,10)) %>%
    # mutate(IV_300_BI_lead730_lag11=lag(IV_300_BI_lead730,11)) %>%
    # mutate(IV_300_BI_lead730_lag12=lag(IV_300_BI_lead730,12)) %>%
    # mutate(IV_300_BI_lead730_lag13=lag(IV_300_BI_lead730,13)) %>%
    # mutate(IV_300_BI_lead730_lag14=lag(IV_300_BI_lead730,14)) %>%
    # mutate(IV_300_BI_lead730_lag15=lag(IV_300_BI_lead730,15)) %>%
    # mutate(IV_300_BI_lead730_lag16=lag(IV_300_BI_lead730,16)) %>%
    # mutate(IV_300_BI_lead730_lag17=lag(IV_300_BI_lead730,17)) %>%
    # mutate(IV_300_BI_lead730_lag18=lag(IV_300_BI_lead730,18)) %>%
    # mutate(IV_300_BI_lead730_lag19=lag(IV_300_BI_lead730,19)) %>%
    # mutate(IV_300_BI_lead730_lag20=lag(IV_300_BI_lead730,20)) %>%
    # mutate(IV_300_BI_lead730_lag21=lag(IV_300_BI_lead730,21)) %>%
    # mutate(IV_300_BI_lead730_lag22=lag(IV_300_BI_lead730,22)) %>%
    # mutate(IV_300_BI_lead730_lag23=lag(IV_300_BI_lead730,23)) %>%
    # mutate(IV_300_BI_lead730_lag24=lag(IV_300_BI_lead730,24)) %>%
    # mutate(IV_300_BI_lead730_lag25=lag(IV_300_BI_lead730,25)) %>%
    # mutate(IV_300_BI_lead730_lag26=lag(IV_300_BI_lead730,26)) %>%
    # mutate(IV_300_BI_lead730_lag27=lag(IV_300_BI_lead730,27)) %>%
    # mutate(IV_300_BI_lead730_lag28=lag(IV_300_BI_lead730,28)) %>%
    # mutate(IV_300_BI_lead730_lag29=lag(IV_300_BI_lead730,29)) %>%
    # mutate(IV_300_BI_lead730_lag30=lag(IV_300_BI_lead730,30)) %>%
    ungroup()

  return (df_with_lag)
}

create_h <- function(df) {
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI_lead730","AQI_lead730_lag1","AQI_lead730_lag2","AQI_lead730_lag3","AQI_lead730_lag4","AQI_lead730_lag5","AQI_lead730_lag6","AQI_lead730_lag7",
          # "IV_300_BI_lead730","IV_300_BI_lead730_lag1","IV_300_BI_lead730_lag2","IV_300_BI_lead730_lag3","IV_300_BI_lead730_lag4","IV_300_BI_lead730_lag5","IV_300_BI_lead730_lag6","IV_300_BI_lead730_lag7"
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7",
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am")]
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
lead730_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_730)
lead730_lag <- create_lag_730(lead730_lag)

# lag 0
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730) == F,]
summary(h$AQI_lead730)
summary(h)
cbind(lapply(lapply(h, is.na), sum))
IQR(h$AQI_lead730)
h <- na.omit(h)
dim(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag0.csv")

# lag 1
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag1) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag1.csv")

# lag 2
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag2) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag2.csv")

# lag 3
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag3) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag3.csv")

# lag 4
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag4) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag4.csv")

# lag 5
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag5) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag5.csv")

# lag 6
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag6) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag6.csv")

# lag 7
h <- create_h(lead730_lag)
h <- h[is.na(h$AQI_lead730_lag7) == F,]
h <- na.omit(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_iv_neg_control_730_lag7.csv")



# 1118 1 lead lag 추가
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
dim(asthma_lead_1)
colnames(asthma_lead_1)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, asthma_lead_1[, c('air_out_idx', 'date', 'AQI_lead1')], by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1) # 33206 57

# wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1 <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1)

day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1 <- na.omit(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1) # 32425 66


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


create_lag_1 <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(AQI_lead1_lag1=lag(AQI_lead1)) %>%
    mutate(AQI_lead1_lag2=lag(AQI_lead1,2)) %>%
    mutate(AQI_lead1_lag3=lag(AQI_lead1,3)) %>%
    mutate(AQI_lead1_lag4=lag(AQI_lead1,4)) %>%
    mutate(AQI_lead1_lag5=lag(AQI_lead1,5)) %>%
    mutate(AQI_lead1_lag6=lag(AQI_lead1,6)) %>%
    mutate(AQI_lead1_lag7=lag(AQI_lead1,7)) %>%
    mutate(AQI_lead1_lag8=lag(AQI_lead1,8)) %>%
    mutate(AQI_lead1_lag9=lag(AQI_lead1,9)) %>%
    mutate(AQI_lead1_lag10=lag(AQI_lead1,10)) %>%
    mutate(AQI_lead1_lag11=lag(AQI_lead1,11)) %>%
    mutate(AQI_lead1_lag12=lag(AQI_lead1,12)) %>%
    mutate(AQI_lead1_lag13=lag(AQI_lead1,13)) %>%
    mutate(AQI_lead1_lag14=lag(AQI_lead1,14)) %>%
    mutate(AQI_lead1_lag15=lag(AQI_lead1,15)) %>%
    mutate(AQI_lead1_lag16=lag(AQI_lead1,16)) %>%
    mutate(AQI_lead1_lag17=lag(AQI_lead1,17)) %>%
    mutate(AQI_lead1_lag18=lag(AQI_lead1,18)) %>%
    mutate(AQI_lead1_lag19=lag(AQI_lead1,19)) %>%
    mutate(AQI_lead1_lag20=lag(AQI_lead1,20)) %>%
    mutate(AQI_lead1_lag21=lag(AQI_lead1,21)) %>%
    mutate(AQI_lead1_lag22=lag(AQI_lead1,22)) %>%
    mutate(AQI_lead1_lag23=lag(AQI_lead1,23)) %>%
    mutate(AQI_lead1_lag24=lag(AQI_lead1,24)) %>%
    mutate(AQI_lead1_lag25=lag(AQI_lead1,25)) %>%
    mutate(AQI_lead1_lag26=lag(AQI_lead1,26)) %>%
    mutate(AQI_lead1_lag27=lag(AQI_lead1,27)) %>%
    mutate(AQI_lead1_lag28=lag(AQI_lead1,28)) %>%
    mutate(AQI_lead1_lag29=lag(AQI_lead1,29)) %>%
    mutate(AQI_lead1_lag30=lag(AQI_lead1,30)) %>%

    ungroup()

  return (df_with_lag)
}

create_h_1 <- function(df) {
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7",
          "AQI_lead1","AQI_lead1_lag1","AQI_lead1_lag2","AQI_lead1_lag3","AQI_lead1_lag4","AQI_lead1_lag5","AQI_lead1_lag6","AQI_lead1_lag7",
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7",
          # "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am"
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
#   h <- df[,c("air_out_idx", "date", "ASTHMA_out_total_agg", 
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
lead1_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_1)
lead1_lag <- create_lag_1(lead1_lag)


setwd("D:\\SNUlab\\")

# lag 7
h <- create_h_1(lead1_lag)
h <- h[is.na(h$AQI_lead1_lag7) == F,]
h <- na.omit(h)
dim(h)
colnames(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_1_neg_control_lag7.csv")







# h <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[,c("ASTHMA_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday","AQI","visibility_back","IV_300_BI")]
# h$wday1 = ifelse(h$wday=="1", 1, 0)
# h$wday2 = ifelse(h$wday=="2", 1, 0)
# h$wday3 = ifelse(h$wday=="3", 1, 0)
# h$wday4 = ifelse(h$wday=="4", 1, 0)
# h$wday5 = ifelse(h$wday=="5", 1, 0)
# h$wday6 = ifelse(h$wday=="6", 1, 0)
# h$wday7 = ifelse(h$wday=="7", 1, 0)
# write.csv(h,"asthma_iv.csv")
# stata code ===
# ivpoisson gmm asthma_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = aqi_back iv_300_bi)


# zio - check with each AQIs
# pm10_AQI, pm25_AQI, so2_AQI, co_AQI, o3_AQI, no2_AQI

# pm10_AQI (보정변수 so2_AQI, co_AQI, o3_AQI, no2_AQI 포함)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ so2_AQI + co_AQI + o3_AQI + no2_AQI + pm10_AQI,
                 instrument_formula = pm10_AQI ~ IV_300_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# pm25_AQI (보정변수 so2_AQI, co_AQI, o3_AQI, no2_AQI, pm10_pm25_AQI 포함)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg$pm10_pm25 = day_asthma_1317_agg_seoul_step2_humi_pressure_agg$pm10_day_mean - day_asthma_1317_agg_seoul_step2_humi_pressure_agg$pm25_day_mean
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ so2_AQI + co_AQI + o3_AQI + no2_AQI + pm10_pm25 + pm25_AQI,
                 instrument_formula = pm25_AQI ~ IV_300_BI, 
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# test0720 <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
# h <- create_h_with_index(test0720)
# write.csv(h, 'D:\\SNUlab\\thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_agg.csv')


# so2_AQI (보정변수 pm10_AQI, co_AQI, o3_AQI, no2_AQI 포함)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + co_AQI + o3_AQI + no2_AQI + so2_AQI,
                 instrument_formula = so2_AQI ~ IV_300_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# co_AQI (보정변수 pm10_AQI, so2_AQI, o3_AQI, no2_AQI 포함)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + o3_AQI + no2_AQI + co_AQI,
                 instrument_formula = co_AQI ~ IV_300_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# o3_AQI (보정변수 pm10_AQI, so2_AQI, co_AQI, no2_AQI 포함)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + co_AQI + no2_AQI + o3_AQI,
                 instrument_formula = o3_AQI ~ IV_300_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

# no2_AQI (보정변수 pm10_AQI, so2_AQI, co_AQI, o3_AQI 포함)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ pm10_AQI + so2_AQI + co_AQI + o3_AQI + no2_AQI,
                 instrument_formula = no2_AQI ~ IV_300_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)


h <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[,c("ASTHMA_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", "pm10_pm25", "AQI", "pm10_AQI", "pm25_AQI", "so2_AQI", "co_AQI", "o3_AQI", "no2_AQI", "IV_300_BI")]
h <- h[is.na(h$pm10_AQI)==F,]
summary(h$pm10_AQI)
summary(h$pm25_AQI)
summary(h$so2_AQI)
summary(h$co_AQI)
summary(h$o3_AQI)
summary(h$no2_AQI)
boxplot(h$pm10_AQI)
quantile(h$pm10_AQI, c(0.99, 0.999, 0.9999, 0.9995))

# try IQR outlier process
# 3 quatile + IQR 보다 큰 값 자르기 -> 0.1% 잘라서 다시
h <- h[-which(h$pm10_AQI>summary(h$pm10_AQI)[5] + 1.5*IQR(h$pm10_AQI)),]

# try minmax scaling pm10_AQI
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
h$pm10_AQI <- normalize(h$pm10_AQI)
h$so2_AQI <- normalize(h$so2_AQI)
h$o3_AQI <- normalize(h$o3_AQI)
h$no2_AQI <- normalize(h$no2_AQI)
h$pm25_AQI <- normalize(h$pm25_AQI)
h$co_AQI <- normalize(h$co_AQI)

h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"D:\\SNUlab\\thermal_inversion_0623_share\\asthma_iv.csv")

summary(h$ASTHMA_out_total_agg)
summary(h$pm10_AQI)
summary(h$AQI)
cor(h$AQI,h$ASTHMA_out_total_agg)
summary(glm_iv$stage_one)





# age, sex subgroup analysis - zio
# 0705 lag도 붙여서 내보내기 (for stata)

write.csv(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, 'D:\\SNUlab\\thermal_inversion_0623_share\\day_asthma_1317_agg_seoul_step2_humi_pressure_agg.csv')
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sin <- sin(2 * 3.14/365.25 * day_asthma_1317_agg_seoul_step2_humi_pressure_agg$yday)
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg$cos <- cos(2 * 3.14/365.25 * day_asthma_1317_agg_seoul_step2_humi_pressure_agg$yday)

# 0812 wind 추가
day_asthma_1317_agg_seoul_step2_humi_pressure_agg <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, wind_1317_final2, by=c('air_out_idx', 'date'), all.x = T)


## IV_500 - IV_300 만들기
day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_500_300_BI = day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_500_BI - day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI
step1_final_13_na$IV_500_300_BI = step1_final_13_na$IV_500_BI - step1_final_13_na$IV_300_BI

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ AQI, 
                 instrument_formula = AQI_lag2 ~ IV_500_300_BI_lag2,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
summary(glm_iv)

table(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_500_300_BI, day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_300_BI)

## IV_700 - IV_500 만들기
day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_700_500_BI = day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_700_BI - day_asthma_1317_agg_seoul_step2_humi_pressure_agg$IV_500_BI
step1_final_13_na$IV_700_500_BI = step1_final_13_na$IV_700_BI - step1_final_13_na$IV_500_BI

glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+ AQI, 
                 instrument_formula = AQI ~ IV_700_500_BI,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg,family =quasipoisson, link = 'log')
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

create_lag_pm10AQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(pm10_AQI_lag1=lag(pm10_AQI)) %>%
    mutate(pm10_AQI_lag2=lag(pm10_AQI,2)) %>%
    mutate(pm10_AQI_lag3=lag(pm10_AQI,3)) %>%
    mutate(pm10_AQI_lag4=lag(pm10_AQI,4)) %>%
    mutate(pm10_AQI_lag5=lag(pm10_AQI,5)) %>%
    mutate(pm10_AQI_lag6=lag(pm10_AQI,6)) %>%
    mutate(pm10_AQI_lag7=lag(pm10_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_so2AQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(so2_AQI_lag1=lag(so2_AQI)) %>%
    mutate(so2_AQI_lag2=lag(so2_AQI,2)) %>%
    mutate(so2_AQI_lag3=lag(so2_AQI,3)) %>%
    mutate(so2_AQI_lag4=lag(so2_AQI,4)) %>%
    mutate(so2_AQI_lag5=lag(so2_AQI,5)) %>%
    mutate(so2_AQI_lag6=lag(so2_AQI,6)) %>%
    mutate(so2_AQI_lag7=lag(so2_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_no2AQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(no2_AQI_lag1=lag(no2_AQI)) %>%
    mutate(no2_AQI_lag2=lag(no2_AQI,2)) %>%
    mutate(no2_AQI_lag3=lag(no2_AQI,3)) %>%
    mutate(no2_AQI_lag4=lag(no2_AQI,4)) %>%
    mutate(no2_AQI_lag5=lag(no2_AQI,5)) %>%
    mutate(no2_AQI_lag6=lag(no2_AQI,6)) %>%
    mutate(no2_AQI_lag7=lag(no2_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_coAQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(co_AQI_lag1=lag(co_AQI)) %>%
    mutate(co_AQI_lag2=lag(co_AQI,2)) %>%
    mutate(co_AQI_lag3=lag(co_AQI,3)) %>%
    mutate(co_AQI_lag4=lag(co_AQI,4)) %>%
    mutate(co_AQI_lag5=lag(co_AQI,5)) %>%
    mutate(co_AQI_lag6=lag(co_AQI,6)) %>%
    mutate(co_AQI_lag7=lag(co_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_pm25AQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(pm25_AQI_lag1=lag(pm25_AQI)) %>%
    mutate(pm25_AQI_lag2=lag(pm25_AQI,2)) %>%
    mutate(pm25_AQI_lag3=lag(pm25_AQI,3)) %>%
    mutate(pm25_AQI_lag4=lag(pm25_AQI,4)) %>%
    mutate(pm25_AQI_lag5=lag(pm25_AQI,5)) %>%
    mutate(pm25_AQI_lag6=lag(pm25_AQI,6)) %>%
    mutate(pm25_AQI_lag7=lag(pm25_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}

create_lag_o3AQI <- function(df) {
  df_with_lag = df%>%
    group_by(air_out_idx)%>%
    mutate(o3_AQI_lag1=lag(o3_AQI)) %>%
    mutate(o3_AQI_lag2=lag(o3_AQI,2)) %>%
    mutate(o3_AQI_lag3=lag(o3_AQI,3)) %>%
    mutate(o3_AQI_lag4=lag(o3_AQI,4)) %>%
    mutate(o3_AQI_lag5=lag(o3_AQI,5)) %>%
    mutate(o3_AQI_lag6=lag(o3_AQI,6)) %>%
    mutate(o3_AQI_lag7=lag(o3_AQI,7)) %>%

    ungroup()

  return (df_with_lag)
}




create_h <- function(df) { # wind added
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10", "AQI_lag11", "AQI_lag12", "AQI_lag13", "AQI_lag14", 
          "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10", "IV_300_BI_lag11", "IV_300_BI_lag12", "IV_300_BI_lag13", "IV_300_BI_lag14",
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am",
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

create_h_mv <- function(df) {
  h <- df[,c("ASTHMA_out_total_agg", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am", 
          "AQI", "AQI_lag0_lag1","AQI_lag0_lag2","AQI_lag0_lag3","AQI_lag0_lag4","AQI_lag0_lag5","AQI_lag0_lag6","AQI_lag0_lag7", "AQI_lag0_lag8", "AQI_lag0_lag9", "AQI_lag0_lag10", "AQI_lag0_lag11", "AQI_lag0_lag12", "AQI_lag0_lag13", "AQI_lag0_lag14", 
          "IV_300_BI","IV_300_BI_lag0_lag1","IV_300_BI_lag0_lag2","IV_300_BI_lag0_lag3","IV_300_BI_lag0_lag4","IV_300_BI_lag0_lag5","IV_300_BI_lag0_lag6","IV_300_BI_lag0_lag7", "IV_300_BI_lag0_lag8", "IV_300_BI_lag0_lag9", "IV_300_BI_lag0_lag10", "IV_300_BI_lag0_lag11", "IV_300_BI_lag0_lag12", "IV_300_BI_lag0_lag13", "IV_300_BI_lag0_lag14", 
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
  h <- df[,c("ASTHMA_out_total_agg", 
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
  h <- df[,c("ASTHMA_out_total_agg", 
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
  h <- df[,c("ASTHMA_out_total_agg", 
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
  h <- df[,c("ASTHMA_out_total_agg", 
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

create_h_with_subgroup <- function(df) { # wind added
  h <- df[,c("ASTHMA_out_total_agg", "ASTHMA_out_total_age1", "ASTHMA_out_total_age2", "ASTHMA_out_total_age3", 
          "ASTHMA_out_total_age4", "ASTHMA_out_total_m", "ASTHMA_out_total_f", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am", 
          "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI", 
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
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

create_h_with_index <- function(df) {
  h <- df[,c("air_out_idx", "date", "ASTHMA_out_total_agg", "ASTHMA_out_total_age1", "ASTHMA_out_total_age2", "ASTHMA_out_total_age3", 
          "ASTHMA_out_total_age4", "ASTHMA_out_total_m", "ASTHMA_out_total_f", 
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", 
          "wind_speed_day_agg", "sin_direction_day_agg", "cos_direction_day_agg", "wind_speed_day_9am", "sin_direction_day_9am", "cos_direction_day_9am", 
          "so2_AQI", "so2_AQI_lag1","so2_AQI_lag2","so2_AQI_lag3","so2_AQI_lag4","so2_AQI_lag5","so2_AQI_lag6","so2_AQI_lag7",
          "no2_AQI", "no2_AQI_lag1","no2_AQI_lag2","no2_AQI_lag3","no2_AQI_lag4","no2_AQI_lag5","no2_AQI_lag6","no2_AQI_lag7",
          "co_AQI", "co_AQI_lag1","co_AQI_lag2","co_AQI_lag3","co_AQI_lag4","co_AQI_lag5","co_AQI_lag6","co_AQI_lag7",
          "pm10_AQI", "pm10_AQI_lag1","pm10_AQI_lag2","pm10_AQI_lag3","pm10_AQI_lag4","pm10_AQI_lag5","pm10_AQI_lag6","pm10_AQI_lag7",
          "pm25_AQI", "pm25_AQI_lag1","pm25_AQI_lag2","pm25_AQI_lag3","pm25_AQI_lag4","pm25_AQI_lag5","pm25_AQI_lag6","pm25_AQI_lag7",
          "o3_AQI", "o3_AQI_lag1","o3_AQI_lag2","o3_AQI_lag3","o3_AQI_lag4","o3_AQI_lag5","o3_AQI_lag6","o3_AQI_lag7",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
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


getwd()
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure)
dim(day_asthma_1317_agg_seoul_step2_humi_pressure)
# day_asthma_1317_agg_seoul_step2_humi_pressure 를 다시 age, sex 로 aggregate
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg 는 date, air_out_idx 로만 aggregate 되어있음

## 0706 박사님 피드백 후 수정 - day_asthma_1317_agg_seoul_step2_humi_pressure_agg 에 subgroup 별 column 을 merge
## 이제 dataset 별 lag 별이 아니라, lag 별로만 na 제거하고 내보내면 됨 !! ~~

# SUBGROUP 있을때 실행
final_df_for_subgroup_analysis <- merge(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, day_asthma_age1,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_age2,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_age3,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_age4,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_m,  by=c("date","air_out_idx"),all.x = T)
final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_f,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_m_age1,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_m_age2,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_m_age3,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_m_age4,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_f_age1,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_f_age2,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_f_age3,  by=c("date","air_out_idx"),all.x = T)
# final_df_for_subgroup_analysis <- merge(final_df_for_subgroup_analysis, day_asthma_f_age4,  by=c("date","air_out_idx"),all.x = T)

colnames(final_df_for_subgroup_analysis)


# SUBGROUP 없을때 실행
final_df_for_subgroup_analysis <- copy(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)

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
    tmp_df_for_lag <- subset(final_df_for_subgroup_analysis, select=c("date", "air_out_idx", "AQI", "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI", iv_name))
    tmp_df_for_lag_13 <- subset(step1_final_13_na, select = c("date", "air_out_idx", "AQI", "so2_AQI", "no2_AQI", "co_AQI", "pm10_AQI", "pm25_AQI", "o3_AQI", iv_name))
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
    # iv_300
    tmp_df_lag_final <- create_lag(tmp_df_lag_final) # iv_300
    tmp_df_lag_final <- create_lag_no2AQI(tmp_df_lag_final)
    tmp_df_lag_final <- create_lag_so2AQI(tmp_df_lag_final)
    tmp_df_lag_final <- create_lag_coAQI(tmp_df_lag_final)
    tmp_df_lag_final <- create_lag_o3AQI(tmp_df_lag_final)
    tmp_df_lag_final <- create_lag_pm10AQI(tmp_df_lag_final)
    tmp_df_lag_final <- create_lag_pm25AQI(tmp_df_lag_final)

  tmp_df_lag_final <- create_lag_iv500(tmp_df_lag_final) # iv_500
  tmp_df_lag_final <- create_lag_iv700(tmp_df_lag_final) # iv_700
  tmp_df_lag_final <- create_lag_iv500_300(tmp_df_lag_final) # iv_500_300
  tmp_df_lag_final <- create_lag_iv700_500(tmp_df_lag_final) # iv_700_500
  


# stage 2) lag 붙였으니 다시 2014년부터 뽑자
tmp_df_lag_final <- tmp_df_lag_final[substr(tmp_df_lag_final$date, 1, 4) %in% c(2014:2017),]
colnames(tmp_df_lag_final)
min(tmp_df_lag_final$date)

# stage 3) final df + lag df merge 
colnames(tmp_df_lag_final)
colnames(final_df_for_subgroup_analysis)
# 기본
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 11:112)], by=c("date", "air_out_idx"), all.x = T)
# IV_500_300
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 6:55)], by=c("date", "air_out_idx"), all.x = T)
# IV_700_500
final_df_for_subgroup_analysis_lag <- merge(final_df_for_subgroup_analysis, tmp_df_lag_final[,c(1, 2, 7:66)], by=c("date", "air_out_idx"), all.x = T)

summary(final_df_for_subgroup_analysis_lag)
colnames(final_df_for_subgroup_analysis_lag)
dim(final_df_for_subgroup_analysis_lag)



# stage 4) lag 별 na 제거하고 df 생성
### 0715 stage 4 는 밑으로 ###

#### IV_300_BI #### - subgroup 없을때
  # lag 0
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0.csv")

  # lag 1
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  colnames(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1.csv")

  # lag 2
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2.csv")

  # lag 3
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3.csv")

  # lag 4
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4.csv")

  # lag 5
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5.csv")

  # lag 6
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6.csv")

  # lag 7
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7.csv")

  # lag 8
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag8) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag8.csv")

  # lag 9
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag9) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag9.csv")

  # lag 10
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag10) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag10.csv")

  # lag 11
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag11) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag11.csv")

  # lag 12
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag12) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag12.csv")

  # lag 13
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag13) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag13.csv")

  # lag 14
  h <- create_h(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag14) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag14.csv")

  summary(final_df_for_subgroup_analysis_lag$ASTHMA_em_total_agg)
#### IV_300_BI end. ####



#### IV_300_BI #### - subgroup 있을때
  setwd("D:\\SNUlab\\")
  # lag 0
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0.csv")

  # lag 1
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1.csv")

  # lag 2
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2.csv")

  # lag 3
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3.csv")

  # lag 4
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4.csv")

  # lag 5
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5.csv")

  # lag 6
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6.csv")

  # lag 7
  h <- create_h_with_subgroup(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  h <- na.omit(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7.csv")

#### IV_300_BI end. ####



#### IV_300_BI - subgroup 없을때 - moving average added ####
  ## moving average 변수 생성 ##
  colnames(final_df_for_subgroup_analysis_lag)
  # AQI
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag1 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag2 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag3 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag4 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag5 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag6 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag7 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag8 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag9 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag10 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag11 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag12 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag13 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12', 'AQI_lag13')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$AQI_lag0_lag14 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12', 'AQI_lag13', 'AQI_lag14')], na.rm = TRUE)
  # thermal inversion
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag1 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag2 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag3 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag4 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag5 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag6 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag7 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag8 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag9 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag10 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag11 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag12 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag13 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12', 'IV_300_BI_lag13')], na.rm = TRUE)
  final_df_for_subgroup_analysis_lag$IV_300_BI_lag0_lag14 <- rowMeans(final_df_for_subgroup_analysis_lag[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12', 'IV_300_BI_lag13', 'IV_300_BI_lag14')], na.rm = TRUE)
  colSums(is.na(final_df_for_subgroup_analysis_lag))  

  setwd("D:\\SNUlab\\")
  # lag 0 - lag 1
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag1) == F,]
  colnames(h)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag1.csv")

  # lag 0 - lag 2
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag2.csv")

  # lag 0 - lag 3
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag3.csv")

  # lag 0 - lag 4
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag4.csv")

  # lag 0 - lag 5
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag5.csv")

  # lag 0 - lag 6
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag6.csv")

  # lag 0 - lag 7
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag7.csv")

  # lag 0 - lag 8
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag8) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag8.csv")

  # lag 0 - lag 9
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag9) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag9.csv")

  # lag 0 - lag 10
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag10) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag10.csv")

  # lag 0 - lag 11
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag11) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag11.csv")

  # lag 0 - lag 12
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag12) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag12.csv")

  # lag 0 - lag 13
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag13) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag13.csv")

  # lag 0 - lag 14
  h <- create_h_mv(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag0_lag14) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_lag14.csv")

#### IV_300_BI moving average end. ####




#### IV_500_BI ####
  # lag 0
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_iv500.csv")

  # lag 1
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1_iv500.csv")

  # lag 2
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2_iv500.csv")

  # lag 3
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3_iv500.csv")

  # lag 4
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4_iv500.csv")

  # lag 5
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5_iv500.csv")

  # lag 6
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6_iv500.csv")

  # lag 7
  h <- create_h_iv500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7_iv500.csv")
#### IV_500_BI end. ####


#### IV_700_BI ####
  # lag 0
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_iv700.csv")

  # lag 1
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1_iv700.csv")

  # lag 2
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2_iv700.csv")

  # lag 3
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3_iv700.csv")

  # lag 4
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4_iv700.csv")

  # lag 5
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5_iv700.csv")

  # lag 6
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6_iv700.csv")

  # lag 7
  h <- create_h_iv700(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7_iv700.csv")
#### IV_700_BI end. ####



#### IV_500_300_BI ####
  # lag 0
  colnames(final_df_for_subgroup_analysis_lag)
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h) # 20199 36
  h <- na.omit(h)
  dim(h) # 20081 36
  table(h$IV_500_300_BI)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_iv500_300.csv")

  # lag 1
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1_iv500_300.csv")

  # lag 2
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2_iv500_300.csv")

  # lag 3
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3_iv500_300.csv")

  # lag 4
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4_iv500_300.csv")

  # lag 5
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5_iv500_300.csv")

  # lag 6
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6_iv500_300.csv")

  # lag 7
  h <- create_h_iv500_300(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7_iv500_300.csv")
#### IV_500_300_BI end. ####


#### IV_700_500_BI ####
  # lag 0
  colnames(final_df_for_subgroup_analysis_lag)
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI) == F,]
  summary(h$AQI)
  IQR(h$AQI)
  dim(h) # 18040 36
  h <- na.omit(h)
  dim(h) # 17901 36
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag0_iv700_500.csv")

  # lag 1
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag1) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag1_iv700_500.csv")

  # lag 2
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag2) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag2_iv700_500.csv")

  # lag 3
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag3) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag3_iv700_500.csv")

  # lag 4
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag4) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag4_iv700_500.csv")

  # lag 5
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag5) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag5_iv700_500.csv")

  # lag 6
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag6) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag6_iv700_500.csv")

  # lag 7
  h <- create_h_iv700_500(final_df_for_subgroup_analysis_lag)
  h <- h[is.na(h$AQI_lag7) == F,]
  h <- na.omit(h)
  write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\asthma_lag7_iv700_500.csv")
#### IV_700_500_BI end. ####






# 0715 stage 4) index 포함한 df
# lag 0
colnames(final_df_for_subgroup_analysis_lag)
h <- create_h_with_index(final_df_for_subgroup_analysis_lag)
h <- h[is.na(h$AQI) == F,]
summary(h$AQI)
IQR(h$AQI)
dim(h)
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_data.csv")

# lag 1
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag1) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag1.csv")

# lag 2
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag2) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag2.csv")

# lag 3
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag3) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag3.csv")

# lag 4
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag4) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag4.csv")

# lag 5
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag5) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag5.csv")

# lag 6
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag6) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag6.csv")

# lag 7
h <- create_h_with_index(tmp_df_lag_final)
h <- h[is.na(h$AQI_lag7) == F,]
write.csv(h,"thermal_inversion_0623_share\\lag datas with index\\asthma_lag7.csv")




### -------- 이하 다 취소
# # agegroup1
# day_asthma_1317_agg_seoul_step2_humi_pressure_age1 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==1,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup1 - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_age1_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_age1)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_age1_lag.csv")


# # agegroup2
# day_asthma_1317_agg_seoul_step2_humi_pressure_age2 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==2,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup2 - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_age2_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_age2)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_age2_lag.csv")


# # agegroup3
# day_asthma_1317_agg_seoul_step2_humi_pressure_age3 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==3,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_age3,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup3 - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_age3_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_age3)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_age3_lag.csv")


# # agegroup4
# day_asthma_1317_agg_seoul_step2_humi_pressure_age4 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==4,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   #agegroup4 - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_age4_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_age4)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_age4_lag.csv")


# # sex M
# day_asthma_1317_agg_seoul_step2_humi_pressure_sex1 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==1,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_sex1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # sex M - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_sex1_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_sex1)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_sex1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_sex1_lag.csv")


# # sex F
# day_asthma_1317_agg_seoul_step2_humi_pressure_sex2 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==2,]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_sex2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # sex F - lag
#   day_asthma_1317_agg_seoul_step2_humi_pressure_sex2_lag <- create_lag(day_asthma_1317_agg_seoul_step2_humi_pressure_sex2)
#   h <- create_h(day_asthma_1317_agg_seoul_step2_humi_pressure_sex2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_1317_agg_seoul_step2_humi_pressure_sex2_lag.csv")


# ##### age_group & sex 나누기 #####
# # AGE1_M
# day_asthma_m_age1 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==1&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_m_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE1_M - lag
#   day_asthma_m_age1_lag <- create_lag(day_asthma_m_age1)
#   h <- create_h(day_asthma_m_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_m_age1_lag.csv")


# # AGE1_F
# day_asthma_f_age1<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==1&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_f_age1,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE1_F - lag
#   day_asthma_f_age1_lag <- create_lag(day_asthma_f_age1)
#   h <- create_h(day_asthma_f_age1_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_f_age1_lag.csv")


# # AGE2_M
# day_asthma_m_age2<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==2&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_m_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE2_M - lag
#   day_asthma_m_age2_lag <- create_lag(day_asthma_m_age2)
#   h <- create_h(day_asthma_m_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_m_age2_lag.csv")


# # AGE2_F
# day_asthma_f_age2<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==2&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_f_age2,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE2_F - lag
#   day_asthma_f_age2_lag <- create_lag(day_asthma_f_age2)
#   h <- create_h(day_asthma_f_age2_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_f_age2_lag.csv")


# # AGE3_M
# day_asthma_m_age3<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==3&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_m_age3,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE3_M - lag
#   day_asthma_m_age3_lag <- create_lag(day_asthma_m_age3)
#   h <- create_h(day_asthma_m_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_m_age3_lag.csv")


# # AGE3_F
# day_asthma_f_age3 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==3&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_f_age3,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE3_F - lag
#   day_asthma_f_age3_lag <- create_lag(day_asthma_f_age3)
#   h <- create_h(day_asthma_f_age3_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_f_age3_lag.csv")


# # AGE4_M
# day_asthma_m_age4<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==4&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==1),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_m_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE4_M - lag
#   day_asthma_m_age4_lag <- create_lag(day_asthma_m_age4)
#   h <- create_h(day_asthma_m_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_m_age4_lag.csv")


# # AGE4_F
# day_asthma_f_age4<-day_asthma_1317_agg_seoul_step2_humi_pressure_agg[which(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$age_group==4&day_asthma_1317_agg_seoul_step2_humi_pressure_agg$sex==2),]
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_f_age4,family =quasipoisson, link = 'log')
# summary(glm_iv)
#   # AGE4_F - lag
#   day_asthma_f_age4_lag <- create_lag(day_asthma_f_age4)
#   h <- create_h(day_asthma_f_age4_lag)
#   h <- h[is.na(h$AQI_lag7) == F,]
#   write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\day_asthma_f_age4_lag.csv")





exp(.003325 *6.247)
fit_1_asthma <- lm(AQI~rain_sum+ns.basis_temp +ns.basis_yday+ns.basis_hum+ns.basis_pressure+wday+IV_300_BI,data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
summary(fit_1_asthma)
fit_1_asthma <- lm(AQI~IV_300_BI,data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
summary(fit_1_asthma)
write.csv(day_asthma_1317_agg_seoul_step2_humi_pressure_agg,"day_asthma_1317_agg_seoul_step2_humi_pressure_agg.csv")
# save(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, file = "day_asthma_1317_agg_seoul_step2_humi_pressure_agg.RData")




#####기름값, 이동량 #####
# petrol_mobility = read.csv("petrol_mobility.csv")
# petrol_mobility$X = NULL
# petrol_mobility = petrol_mobility[,c("air_out_idx","date","petrol_price","mobility_count")]
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price <- left_join(day_asthma_1317_agg_seoul_step2_humi_pressure_agg,petrol_mobility, by=c("air_out_idx","date"))
# 
# fit_1_asthma <- lm(AQI~petrol_price,data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price)
# summary(fit_1_asthma)
# fit_1_asthma <- lm(no2_AQI~mobility_count2,data = day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price)
# summary(fit_1_asthma)
# colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price)
# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg~rain_sum+ ns.basis_temp+ns.basis_yday+wday+
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price,family =quasipoisson, link = 'log')
# summary(glm_iv)
# summary(glm_iv$stage_one)
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$mobility_count2 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$mobility_count/10000
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$PM10_25 <- 
#   day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm10_day_mean -
#   day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm25_day_mean
# cor(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$PM10_25,day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm25_day_mean)
# cor(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$PM10_25,day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm25_day_mean)
# cor(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm10_day_mean,day_asthma_1317_agg_seoul_step2_humi_pressure_agg_price$pm25_day_mean)
# 
# 
# 


#age_group1
day_asthma_1317_agg_seoul_step2_humi_pressure_age1 <- day_asthma_1317_agg_seoul_step2_humi_pressure[day_asthma_1317_agg_seoul_step2_humi_pressure$age_group==1,]
glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ ns.basis_temp+ns.basis_yday+wday+sex
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI+AQI_back,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_age1,family =quasipoisson, link = 'log')
summary(glm_iv)

#age_group2
day_asthma_1317_agg_seoul_step2_humi_pressure_age2 <- day_asthma_1317_agg_seoul_step2_humi_pressure[day_asthma_1317_agg_seoul_step2_humi_pressure$age_group==2,]
glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ ns.basis_temp+ns.basis_yday+wday+sex
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI+AQI_back,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_age2,family =quasipoisson, link = 'log')
summary(glm_iv)

#age_group3
day_asthma_1317_agg_seoul_step2_humi_pressure_age3 <- day_asthma_1317_agg_seoul_step2_humi_pressure[day_asthma_1317_agg_seoul_step2_humi_pressure$age_group==3,]
glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ ns.basis_temp+ns.basis_yday+wday+sex
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI+AQI_back,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_age3,family =quasipoisson, link = 'log')
summary(glm_iv)

#age_group4
day_asthma_1317_agg_seoul_step2_humi_pressure_age4 <- day_asthma_1317_agg_seoul_step2_humi_pressure[day_asthma_1317_agg_seoul_step2_humi_pressure$age_group==4,]
glm_iv <- iv.glm(model_formula = ASTHMA_out_total~rain_sum+ ns.basis_temp+ns.basis_yday+wday+sex
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI+AQI_back,
                 data=day_asthma_1317_agg_seoul_step2_humi_pressure_age4,family =quasipoisson, link = 'log')
summary(glm_iv)
# save.image("220428.RData")

##### 유진쌤에게 받은 negative control #####
#count_non_disease_to_seongpyo.csv
# stata code ===
# ivpoisson gmm count_non_disease rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)
# ivpoisson gmm count_non_disease_traffic rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)
# ivpoisson gmm count_non_disease_total rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)



##### 내가 심정지 체크했나? #####
outcome_check = read.csv("group_patient_disease_final_idx.csv",header = T)
colnames(outcome_check)
head(outcome_check,1)
colnames(outcome_check)[c(1,19)] <- c("date","air_out_idx")


final_data_check = left_join(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, outcome_check, by=c("date","air_out_idx"))
nrow(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)-nrow(final_data_check)
summary(final_data_check)
colnames(final_data_check)
for (i in c(75:87)){
  final_data_check[,i] = as.numeric(final_data_check[,i])
}
glm_iv <- iv.glm(model_formula = count_non_disease_traffic ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=final_data_check,family =quasipoisson, link = 'log')
summary(glm_iv)


#0509 교수님 피드백
# 당일효과뿐 아니라, 다음날, 다다음날의 효과도 보는 것이 타당하기 때문에, 앞쪽으로 lag를 보자. 7일.
##### AQI랑 IV에 LAG를 1-7까지 줘서 보기 ######

##### negative control 보기 #####
# 기존 데이터 day_asthma_1317_agg_seoul_step2_humi_pressure_agg
# negative control 
negative = read.csv("group_patient_final_idx_ALL.csv")
colnames(negative)[1] = "date"
colnames(negative)[33] = "air_out_idx"
asthma_with_negative = left_join(day_asthma_1317_agg_seoul_step2_humi_pressure_agg, negative, by=c("air_out_idx","date"))
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
dim(asthma_with_negative)

#  < 질병 외 > 
# count_non_disease_1 : 질병 외 심정지 중, 1. 운수사고 case 만 aggregate
glm_iv <- iv.glm(model_formula = count_non_disease_8 ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=asthma_with_negative,family =quasipoisson, link = 'log')
summary(glm_iv)
#AQI                     1.897e-02  8.087e-03   2.345   0.0190 *

# count_non_disease_2 : 질병 외 심정지 중, 2. 추락 case 만 aggregate
#AQI                     3.301e-03  8.116e-03   0.407  0.68423

# count_non_disease_3 : 질병 외 심정지 중, 3. 부딪힘 case 만 aggregate
#AQI                     1.396e-02  2.391e-02   0.584  0.55934

# count_non_disease_4 : 질병 외 심정지 중, 4. 자상, 찔림, 절단 case 만 aggregate
#AQI                     -0.02244    0.04818  -0.466    0.641

# count_non_disease_5 : 질병 외 심정지 중, 5. 총상 case 만 aggregate
# 케이스 너무 적음.

# count_non_disease_6 : 질병 외 심정지 중, 6. 불, 화염, 고온체 case 만 aggregate
#AQI                       0.03246    0.04254   0.763   0.4454

# count_non_disease_7 : 질병 외 심정지 중, 7. 질식 case 만 aggregate
#AQI                     0.010324   0.010801   0.956   0.3391

# count_non_disease_8 : 질병 외 심정지 중, 8. 익수 case 만 aggregate
#AQI                     2.596e-02  1.684e-02   1.542  0.12306

# count_non_disease_9 : 질병 외 심정지 중, 9. 의수 case 만 aggregate
# AQI                     0.015289   0.007561   2.022  0.04317 *

# count_non_disease_10 : 질병 외 심정지 중, 10. 중독 case 만 aggregate
#AQI                     -0.031100   0.018633  -1.669   0.0951 .

# count_non_disease_88 : 질병 외 심정지 중, 88. 기타 case 만 aggregate
#AQI                    -2.324e-02  4.583e-02  -0.507    0.612

# count_non_disease_99 : 질병 외 심정지 중, 99. 미상 case 만 aggregate
# AQI                     3.504e-02  3.556e-02   0.985   0.3245
sum(asthma_with_negative$count_non_disease_99)

asthma_with_negative$negative_control =   asthma_with_negative$count_non_disease_2+  asthma_with_negative$count_non_disease_3+
                                          asthma_with_negative$count_non_disease_4+  asthma_with_negative$count_non_disease_5+
                                          asthma_with_negative$count_non_disease_6+  asthma_with_negative$count_non_disease_7+
                                          asthma_with_negative$count_non_disease_8+  asthma_with_negative$count_non_disease_10+
                                          asthma_with_negative$count_non_disease_88+ asthma_with_negative$count_non_disease_99
glm_iv <- iv.glm(model_formula = negative_control ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=asthma_with_negative,family =quasipoisson, link = 'log')
summary(glm_iv)
#AQI                     0.0042205  0.0053839   0.784    0.433

colnames(asthma_with_negative)
# save.image("220520.RData")
# h <- asthma_with_negative[,c("negative_control","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday","AQI","IV_300_BI")]
# h$wday1 = ifelse(h$wday=="1", 1, 0)
# h$wday2 = ifelse(h$wday=="2", 1, 0)
# h$wday3 = ifelse(h$wday=="3", 1, 0)
# h$wday4 = ifelse(h$wday=="4", 1, 0)
# h$wday5 = ifelse(h$wday=="5", 1, 0)
# h$wday6 = ifelse(h$wday=="6", 1, 0)
# h$wday7 = ifelse(h$wday=="7", 1, 0)
# write.csv(h,"negative_control_iv.csv")
# stata code ===
# ivpoisson gmm negative_control rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)


# 0718 asthma_with_negative 에 lag 생성 - zio

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
  h <- df[,c("negative_control",
          "rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
          "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
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


asthma_with_negative_lag <- create_lag(asthma_with_negative)
colnames(asthma_with_negative_lag)

summary(asthma_with_negative_lag$negative_control)
hist(asthma_with_negative_lag$negative_control)
sum(asthma_with_negative_lag$negative_control)
length(asthma_with_negative_lag$negative_control)
test=rpois(31287,0.05542)
mean(test)
summary(test)
table(test)
table(asthma_with_negative_lag$negative_control)

# lag 별 na 제거하고 df 생성
# lag 0
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI) == F,]
summary(h$AQI)
IQR(h$AQI)
dim(h)
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag0.csv")

# lag 1
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag1) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag1.csv")

# lag 2
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag2) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag2.csv")

# lag 3
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag3) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag3.csv")

# lag 4
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag4) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag4.csv")

# lag 5
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag5) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag5.csv")

# lag 6
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag6) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag6.csv")

# lag 7
h <- create_h(asthma_with_negative_lag)
h <- h[is.na(h$AQI_lag7) == F,]
write.csv(h,"thermal_inversion_0623_share\\age sex subgrouped data\\ohca_negative_control_lag7.csv")




sum(table(asthma_with_negative$negative_control))
sum(day_asthma_1317_agg_seoul_step2_humi_pressure_agg$ASTHMA_out_total_agg)
sum(table(day_rhinitis_1317_agg_seoul_step2_humi_pressure_agg$RHINITIS_out_total_agg))


colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag
         )


##### 220617 lag 보기 ##### 
library(dplyr)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag=
  day_asthma_1317_agg_seoul_step2_humi_pressure_agg%>%
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



h <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag[,c("ASTHMA_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday",
                                                              "AQI","AQI_lag1","AQI_lag2","AQI_lag3","AQI_lag4","AQI_lag5","AQI_lag6","AQI_lag7","AQI_lag8","AQI_lag9","AQI_lag10",
                                                              "IV_300_BI","IV_300_BI_lag1","IV_300_BI_lag2","IV_300_BI_lag3","IV_300_BI_lag4","IV_300_BI_lag5","IV_300_BI_lag6","IV_300_BI_lag7","IV_300_BI_lag8","IV_300_BI_lag9","IV_300_BI_lag10")]

h <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag[is.na(day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag$AQI_lag7)==F,]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag.csv")
# stata code ===
# ivpoisson gmm ASTHMA_out_total_agg rain_sum nsbasis_tempv1l2 nsbasis_tempv1l3 nsbasis_tempv1l4 nsbasis_tempv2l1 nsbasis_tempv2l2 nsbasis_tempv2l3 nsbasis_tempv2l4 nsbasis_tempv3l1 nsbasis_tempv3l2 nsbasis_tempv3l3 nsbasis_tempv3l4 nsbasis_ydayv1l1 nsbasis_ydayv2l1 nsbasis_ydayv3l1 wday1 wday2 wday3 wday4 wday5 wday6 wday7 nsbasis_humv1l1 nsbasis_humv2l1 nsbasis_humv3l1 nsbasis_pressurev1l1 nsbasis_pressurev2l1 nsbasis_pressurev3l1  (aqi = iv_300_bi)





# 2018 데이터 추가해서 365 lead 확인 # - zio
asthma_lead <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\asthma_lead.csv")
rhinitis_lead <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\rhinitis_lead.csv")
whole_lead <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\whole_lead.csv")

colnames(asthma_lead)
cbind(lapply(lapply(asthma_lead, is.na), sum))
dim(asthma_lead)
# asthma_lead_na <- na.omit(asthma_lead)

# glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg ~ rain_sum+ ns.basis_temp + ns.basis_yday + wday
#                  +ns.basis_hum+ns.basis_pressure + AQI_lead365,
#                  instrument_formula = AQI_lead365 ~ IV_300_BI_lead365,
#                  data=asthma_lead,family =quasipoisson, link = 'log')
# summary(glm_iv)

cbind(
  lapply(
    lapply(asthma_lead, is.na)
  , sum)
)

cbind(
  lapply(
    lapply(asthma_lead_na, is.nan)
  , sum)
)

cbind(
  lapply(
    lapply(asthma_lead_na, is.infinite)
  , sum)
)

h <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[,c("ASTHMA_out_total_agg","rain_sum","ns.basis_temp","ns.basis_hum","ns.basis_pressure","ns.basis_yday","wday", "AQI", "pm10_AQI", "pm25_AQI", "so2_AQI", "co_AQI", "o3_AQI", "no2_AQI", "IV_300_BI")]
h$wday1 = ifelse(h$wday=="1", 1, 0)
h$wday2 = ifelse(h$wday=="2", 1, 0)
h$wday3 = ifelse(h$wday=="3", 1, 0)
h$wday4 = ifelse(h$wday=="4", 1, 0)
h$wday5 = ifelse(h$wday=="5", 1, 0)
h$wday6 = ifelse(h$wday=="6", 1, 0)
h$wday7 = ifelse(h$wday=="7", 1, 0)
write.csv(h,"D:\\SNUlab\\thermal_inversion_0623_share\\asthma_iv.csv")

# asthma_lead_na_row = apply(asthma_lead_na, 1, mean)
# asthma_lead_na <- asthma_lead_na[asthma_lead_na_row != 0, ]

describe(asthma_lead)

# ##### lag, crossbasis matrix #####
# asthma_lead$yday <- yday(asthma_lead$date)
# asthma_lead$wday <- wday(asthma_lead$date)
# asthma_lead$wday_mat<-model.matrix( ~ wday - 1, data=asthma_lead)

# lag=7
# kno_temp <- equalknots(asthma_lead$temp_mean_total,nk=2)
# klag <- logknots(lag,nk=2)

# # CROSSBASIS MATRIX
# ns.basis_temp <- crossbasis(asthma_lead$temp_mean_total,
#                             argvar=list(knots=kno),
#                             group=asthma_lead$air_out_idx,
#                            arglag=list(knots=klag),
#                            lag=lag)

# asthma_lead$ns.basis_temp <- crossbasis(asthma_lead$temp_mean_total,
#                                        argvar=list(knots=kno_temp),
#                                        group=asthma_lead$air_out_idx,
#                                        arglag=list(knots=klag),
#                                        lag=lag)


# kno_yday <- equalknots(day_asthma_1317_agg_seoul_step$yday,nk=2)

# ns.basis_yday <- crossbasis(day_asthma_1317_agg_seoul_step$yday,argvar=list(knots=kno_yday),
#                             group=day_asthma_1317_agg_seoul_step$air_out_idx,
#                             #arglag=list(knots=klag),
#                             #lag=lag
#                             )
# day_asthma_1317_agg_seoul_step$ns.basis_yday <- crossbasis(day_asthma_1317_agg_seoul_step$yday,
#                                                            argvar=list(knots=kno_yday),
#                                                            group=day_asthma_1317_agg_seoul_step$air_out_idx,
#                                                            #arglag=list(knots=klag),
#                                                            #lag=lag
#                                                            )

