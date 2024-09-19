
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
#################

setwd('D:\\SNUlab\\data\\')

# patient = read.csv('급성심장정지조사(08-19)SAS\\patient_final.csv', encoding = 'euc-kr')  # 01.13 patient_final로 수정
patient = read.csv('급성심장정지조사(08-19)SAS\\patient_final_0121_outlierprocessed.csv', encoding = 'euc-kr')  # 01.24 patient_final_0121_outlierprocessed로 수정

#### step 7. 이 모델에 연결된 공간 패턴을 생성 ##########################################
#사계절로 나눈 후 mean aggregation하고 미세먼지가 가장 심한 spring과 fall만 넣어보자. 
#2014.12 데이터도 들어가 있음~
patient_final_value <- patient[,c("air_out_idx","PRE_ER_ARREST_DT","SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total")]

patient_final_value$Month=substr(patient_final_value$PRE_ER_ARREST_DT,6,7)
dim(patient_final_value)
# patient_final_value[which(patient_final_value$air_out_idx==247),]
spring=patient_final_value[patient_final_value$Month%in%c("03","04","05"),c("SO2_mean","NO2_mean", "CO_mean","O3_mean","PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]

# spring2=spring[!complete.cases(spring),] 

spring_list=unique(spring$air_out_idx)
total_list=unique(patient_final_value$air_out_idx)
total_list[34]
spring_list
which(!(total_list %in% spring_list))


patient_final_value[which(patient_final_value$air_out_idx==34),]
# which(patient_final_value$air_out_idx==114)
unique(patient_final_value$air_out_idx)

summer=patient_final_value[patient_final_value$Month%in%c("06","07","08"),c("SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
#summer=summer[complete.cases(summer),]

col.poly_sudogwon2 <- col.poly_sudogwon[,c(1,6)] # 밑에서 정의되어 있어서 추가 (확인 필요)
######## 실행 X - plot 오래걸림 #########
  # plot.map(as.numeric(summer$SO2_mean),col.poly_sudogwon2,main=paste("SO2_mean"))
  # plot.map(as.numeric(summer$PM10_mean),col.poly_sudogwon2,main=paste("PM10_mean"))
########################################



fall=patient_final_value[patient_final_value$Month%in%c("09","10","11"),c("SO2_mean","NO2_mean","CO_mean", "O3_mean","PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
#fall=fall[complete.cases(fall),]
winter=patient_final_value[patient_final_value$Month%in%c("12","01","02"),c("SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
#winter=winter[complete.cases(winter),]

spring_mean=spring %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(spring=mean),na.rm=TRUE)
summer_mean=summer %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(summer=mean),na.rm=TRUE)
fall_mean=fall %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(fall=mean),na.rm=TRUE)
winter_mean=winter %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(winter=mean),na.rm=TRUE)

moran.test(as.numeric(spring_mean$PM10_mean_spring),listw=col.listw_sudogwon)
######## 실행 X - plot 오래걸림 #########
# plot.map(as.numeric(spring_mean$PM10_mean_spring),col.poly_sudogwon2,main=paste("PM10_mean"))
########################################

as.numeric(spring_mean$PM10_mean_spring)
colnames(spring_mean)
sudogwon_1317_spring_fall_mean <- cbind(spring_mean[,2:8],fall_mean[,2:8])

# 기존
# X <- as.matrix(cbind(1,spring_mean[,c(2:8)],fall_mean[,c(2:8)]))
# 최적 성능을 내는 변수 개수가 arbitrary 함
#X <- as.matrix(cbind(1,spring_mean[,c(2:8)],fall_mean[,c(2:8)]))
X <- as.matrix(cbind(1,spring_mean[,c(2:4)],fall_mean[,c(2:4)], winter_mean[,c(2:4)]))

# ,winter_mean[,c(2:8)]

crossprod(X)

M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
eig_sudogwon <- eigen(MCM_sudogwon)
E_sudogwon <- eigen(MCM_sudogwon)$vectors
eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))
##########################################################################################
eigenvalues_sudogwon



#### step 8. 이 모델에 연결된 공간 패턴에 대한 모란의 I 값을 계산하고 그래프 그리기 ######
# ones_sudogwon <- rep(1,n_sudogwon)
# mi_sudogwon <- eigenvalues_sudogwon*n_sudogwon/crossprod(ones_sudogwon,C_sudogwon%*%ones_sudogwon)
# plot(mi_sudogwon,ylim=c(-1,1),pch=20,xlab="Eigenvector Spatial Pattern",ylab="Moran's I")
#  abline(0,0,lty=3)
##########################################################################################





#### step 9 . 이 모델에 연결된 공간 패턴 시각화 ##########################################
# col.poly_sudogwon2 <- col.poly_sudogwon[,c(1,6)]
# colnames(col.poly_sudogwon2)
# for(i in 1:1){
#   plot.map(as.numeric(E_sudogwon[,i]),col.poly_sudogwon2,main=paste("EV",i,": Moran's I = ",round(mi_sudogwon[i],3)))
# }
##########################################################################################



#### 여기까지만 돌리면 됨 ####




# #### step 10. outcome 정의 : ATOPIC,ASTHMA,RHINITIS 2015년 01월 데이터 가져오기 ##########################
# outcome_idx <- read.csv("outcome_idx.csv")
# outcome_idx$X = NULL
# outcome_idx2 = merge(outcome_idx,map_sudogwon_idx,by="air_out_idx")
# length(unique(outcome_idx$air_out_idx))

# length(unique(outcome_idx2$air_out_idx))

# #74개가 나오는데, 그 이유는 부천시가 outcome에서 3개로 나눠져서 map_idx에 비해 2개가 더 많음. 1개만 쓸까?
# #그러면, 부천시를 첫번째 것만 남기자.

# outcome_sudogwon_idx <- outcome_idx2[-c(30,31),]#72개


# # ~~~_total1.sas7bdat = 2013 - 2017 데이터임ㅎㅎ
# # atopic_1317 = read_sas("day_atopic_1317_cnt_3_total1.sas7bdat")
#  asthma_1317 = read_sas("day_asthma_1317_cnt_3_total1.sas7bdat")
# # rhinitis_1317 = read_sas("day_rhinitis_1317_cnt_3_total1.sas7bdat")


# ##################################
# ############ 1. asthma############ 
# ##################################

# ## 1-1. asthma_sum
# asthma_1317$ASTHMA_SUM=
#   asthma_1317$ASTHMA_out_total+
#   asthma_1317$ASTHMA_in_2_total

# ## 1-2. asthma_em
# asthma_1317$ASTHMA_em_total


# asthma_1317_1=asthma_1317[,c("DT","시군구","AGE","SEX_TYPE","ASTHMA_SUM","ASTHMA_out_total","ASTHMA_in_total","ASTHMA_in_2_total","ASTHMA_em_total")]


# ## 1-3. outcome별로 dataset 새로 만들기
# # 전체= asthma_1317_sudogwon
# asthma_1317_2=aggregate(asthma_1317_1[,-c(1:4)],by=list(asthma_1317_1$DT,asthma_1317_1$시군구),FUN=sum)
# colnames(asthma_1317_2)[c(1,2)]=c("DT","시군구")
# asthma_1317_sudogwon = merge(asthma_1317_2,outcome_sudogwon_idx, by="시군구") #72*365 = 26280
# dim(asthma_1317_sudogwon)
# asthma_1317_sudogwon = asthma_1317_sudogwon %>% arrange(air_out_idx, DT)
# weather_air_final
# weather_air_final$DT=as.numeric( gsub("-","",patient_final_value$dt))

# # 전체= asthma_1317_sudogwon
# hist(asthma_1317_sudogwon$ASTHMA_em_total)


# colnames(weather_air_final)
# outcome_asthma <- left_join(weather_air_final, asthma_1317_sudogwon, by=c("DT","air_out_idx")) 
# # outcome_asthma <-outcome_asthma[,order(names(outcome_asthma))] ; sort(colnames(outcome_asthma))
# colnames(outcome_asthma)
# #outcome_asthma1=outcome_asthma[,c(1:2,8,14,20,26,32,48:514,538,540:914,917:920)] # complete case를 위해 우리가 만든, 필요한 변수만 넣기
# outcome_asthma1=outcome_asthma[,c(1,2,8,14,20,26,32,48,49,73,75,76,77,83)] # complete case를 위해 우리가 만든, 필요한 변수만 넣기
# outcome_asthma1=outcome_asthma1[order(outcome_asthma1$air_out_idx,outcome_asthma1$DT),]

# table(unique(outcome_asthma1$air_out_idx)) ; length(unique(outcome_asthma1$air_out_idx)) # 72 check

# ###########################################################################################

# #asthma 

# outcome_asthma2=outcome_asthma1[complete.cases(outcome_asthma1),]
# asthma_1317_DAY=asthma_1317[,c(1,2)]
# asthma_1317_DAY=asthma_1317_DAY[-which(duplicated(asthma_1317_DAY$DT)),]  

# outcome_asthma2=merge(outcome_asthma1,asthma_1317_DAY,by="DT",all.x=TRUE)
# outcome_asthma2$DAY=factor(outcome_asthma2$DAY)
# #outcome_asthma3=outcome_asthma2[complete.cases(outcome_asthma2),]
# outcome_asthma3=outcome_asthma2 %>% arrange(air_out_idx,DT)


# # 72 check