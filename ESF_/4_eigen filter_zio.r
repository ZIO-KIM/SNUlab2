
# install.packages(c("httpgd"))

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
library(data.table)
library(foreign)
#################

setwd('D:\\SNUlab\\ESF_\\')

## 0. read outcome df ##
asthma = read.csv('outcome\\asthma_sudogwon_agg_2015SESadded.csv', fileEncoding='euc-kr')
rhinitis = read.csv('outcome\\rhinitis_sudogwon_agg_2015SESadded.csv', fileEncoding='euc-kr')
atopic = read.csv('outcome\\atopic_sudogwon_agg_2015SESadded.csv', fileEncoding='euc-kr')
###############################################



##### 1. If change needed, change in here #####
outcome = "atopic"
df <- copy(atopic)

length(unique(df$air_out_idx))
###############################################

dim(df)
365.25*5*72 + 61*72


##### 1. Select only 2015 - 2017 (for PM2.5) #####
dim(df) # 135864 69
df <- df[substr(df$dt, 1, 4) %in% c(2015, 2016, 2017),]
dim(df) # 78912 69
# colSums(is.na(df))



#### step 7. 이 모델에 연결된 공간 패턴을 생성 - 기본 ESF ##########################################

  final_value <- df[,c("air_out_idx","dt","SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total")]
  final_value$Month=substr(final_value$dt,6,7)
  head(final_value)
  dim(final_value)

  spring = final_value[final_value$Month%in%c("03","04","05"),c("SO2_mean","NO2_mean", "CO_mean","O3_mean","PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
  length(unique(spring$air_out_idx))
  # spring2=spring[!complete.cases(spring),] 

  spring_list=unique(spring$air_out_idx)
  total_list=unique(final_value$air_out_idx)
  # total_list[34]
  spring_list
  which(!(total_list %in% spring_list))
  dim(spring)

  # final_value[which(final_value$air_out_idx==34),]
  # which(patient_final_value$air_out_idx==114)
  # unique(final_value$air_out_idx)

  summer=final_value[final_value$Month%in%c("06","07","08"),c("SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
  #summer=summer[complete.cases(summer),]
  length(unique(summer$air_out_idx))

  col.poly_sudogwon2 <- col.poly_sudogwon[,c(1,6)] # 밑에서 정의되어 있어서 추가 (확인 필요)
  ######## 실행 X - plot 오래걸림 #########
    # plot.map(as.numeric(summer$SO2_mean),col.poly_sudogwon2,main=paste("SO2_mean"))
    # plot.map(as.numeric(summer$PM10_mean),col.poly_sudogwon2,main=paste("PM10_mean"))
  ########################################

  col.listw_sudogwon

  fall=final_value[final_value$Month%in%c("09","10","11"),c("SO2_mean","NO2_mean","CO_mean", "O3_mean","PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
  #fall=fall[complete.cases(fall),]
  winter=final_value[final_value$Month%in%c("12","01","02"),c("SO2_mean","NO2_mean","CO_mean","O3_mean", "PM10_mean","temp_tc_total","humi_mean_total","air_out_idx")]
  #winter=winter[complete.cases(winter),]

  spring_mean=spring %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(spring=mean),na.rm=TRUE)
  summer_mean=summer %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(summer=mean),na.rm=TRUE)
  fall_mean=fall %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(fall=mean),na.rm=TRUE)
  winter_mean=winter %>% group_by(air_out_idx) %>% summarise_at(vars(c(SO2_mean,NO2_mean,CO_mean,O3_mean,PM10_mean,temp_tc_total,humi_mean_total)), list(winter=mean),na.rm=TRUE)

  head(spring_mean)
  # summary(spring_mean)
  # summary(summer_mean)
  # summary(fall_mean)
  # summary(winter_mean)

  dim(spring_mean)
  moran.test(as.numeric(spring_mean$PM10_mean_spring),listw=col.listw_sudogwon)
  length(col.listw_sudogwon)
  ######## 실행 X - plot 오래걸림 #########
  # plot.map(as.numeric(spring_mean$PM10_mean_spring),col.poly_sudogwon2,main=paste("PM10_mean"))
  ########################################

  as.numeric(spring_mean$PM10_mean_spring)
  colnames(spring_mean)
  dim(spring_mean)
  colnames(fall_mean)
  sudogwon_1317_spring_fall_mean <- cbind(spring_mean[,2:8],fall_mean[,2:8])

  # 기존
  # X <- as.matrix(cbind(1,spring_mean[,c(2:8)],fall_mean[,c(2:8)]))
  # 최적 성능을 내는 변수 개수가 arbitrary 함
  #X <- as.matrix(cbind(1,spring_mean[,c(2:8)],fall_mean[,c(2:8)]))
  # X <- as.matrix(cbind(1,spring_mean[,c(2:4)],fall_mean[,c(2:4)], winter_mean[,c(2:4)]))


  # X <- as.matrix(cbind(1,spring_mean[,c(2:7)],winter_mean[,c(2:6)],fall_mean[,c(2:8)]))  # 이 순서 지켜야함

  # X <- as.matrix(cbind(1, winter_mean[,c(2:6)], spring_mean[,c(2:6)])) 
  # X <- as.matrix(cbind(1, winter_mean[,c(2:6)], spring_mean[,c(2:8)], fall_mean[,c(2:8)]))  
  # X <- as.matrix(cbind(1, spring_mean[,c(2:8)], fall_mean[,c(2:5)], winter_mean[,c(2:8)]))
  # ,winter_mean[,c(2:8)]
  # X <- as.matrix(cbind(1, winter_mean[,c(2:6)], spring_mean[,c(2:6)])) 

  # X <- as.matrix(cbind(1, spring_mean[,c(2:8)], fall_mean[,c(2:5)], winter_mean[,c(2:8)]))


##########################################################################################

## change filters ##
  X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))

  # param_index list #
    # ASTHMA em NO2
    param_index <- c(4,15,9,17,10,2,29,30,19,16)
    X <- X_all[,param_index]
    # ASTHMA in NO2
    # param_index <- c(8,22,28,17,21,29,16,19,30,6,18)
    param_index <- c(24,26,13,2,22,8,23,3,29,5,12,19,14) # PM10
    X <- X_all[,param_index]
    # ASTHMA out NO2
    param_index <- c(15,17,22,26,29,5,14)
    X <- X_all[,param_index]
    # RHINITIS em NO2
    param_index <- c(26,5,19,10,20,1,22,21,28,9,25,6,7,30,24,16,15)
    X <- X_all[,param_index]
    # RHINITIS in NO2
    param_index <- c(9,1,5,18,4,2,14,26,23,24,17)
    param_index <- c(21,23,1,20,25,7,18,10,22) # so2 기준
    param_index <- c(2,14,5,10,24,16,30,22,26,21,8,12,13,19,23) # ses 넣은 모델 기준
    param_index <- c(28,18,7,4,2,13,1,8,12) # ses without 도시인구, no2 기준 0.07, but so2 mi가 이상 (general보다 ESF가 높음)
    X <- X_all[,param_index]
    # RHINITIS out NO2
    param_index <- c(8,7,5,16,20,9) # no2기준 0.06
    X <- X_all[,param_index]
    # ATOPIC em NO2
    # param_index <- c(10,24,12,15,26,8,14,21,29)
    param_index <- c(23,1,2,27,9,13,7)
    X <- X_all[,param_index]
    # ATOPIC in NO2
    param_index <- c(19,14,12,4,30,10,11,26,9,16,1,7,13,29,28)
    X <- X_all[,param_index]
    # ATOPIC out NO2
    param_index <- c(21,7,13,16,3,15,2,1,24,10,30,22,12,26)
    X <- X_all[,param_index]
    # ATOPIC in+out+em NO2
    param_index <- c(29,26,10,22,30,15,28,25,27,5,17,23)
    X <- X_all[,param_index]

  # X <- as.matrix(cbind(1, spring_mean[,c(2:4)], winter_mean[,c(2:6)], fall_mean[c(2:6)]))
  crossprod(X)

  M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
  MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
  eig_sudogwon <- eigen(MCM_sudogwon)
  E_sudogwon <- eigen(MCM_sudogwon)$vectors
  eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
  E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))



  # plot.map(as.numeric(E_re_sudogwon[,1]),col.poly_sudogwon2,main=paste("EV",1,": Moran's I = ",round(mi_sudogwon[1],3)))
  # plot.map(as.numeric(E_re_sudogwon[,72]),col.poly_sudogwon2,main=paste("EV",72,": Moran's I = ",round(mi_sudogwon[72],3)))

  # plot.map(as.numeric(atopic_out_test[,2]),col.poly_sudogwon2,main=c("Atopic Outpatient"))
  # plot.map(as.numeric(X[,1]),col.poly_sudogwon2,main=c("PM10_mean_fall"))
  # plot.map(as.numeric(test[,2]),col.poly_sudogwon2,main=c("Residuals"))
  # colnames(X)test
  # atopic_out_test=aggregate(df$ATOPIC_out_total,list(df$air_out_idx),FUN=mean)

######################




#### step 8. 이 모델에 연결된 공간 패턴에 대한 모란의 I 값을 계산하고 그래프 그리기 ######
ones_sudogwon <- rep(1,n_sudogwon)
mi_sudogwon <- eigenvalues_sudogwon*n_sudogwon/crossprod(ones_sudogwon,C_sudogwon%*%ones_sudogwon)
plot(mi_sudogwon,ylim=c(-1,1),pch=20,xlab="Eigenvector Spatial Pattern",ylab="Moran's I")
 abline(0,0,lty=3)
##########################################################################################





#### step 9 . 이 모델에 연결된 공간 패턴 시각화 ##########################################
col.poly_sudogwon2 <- col.poly_sudogwon[,c(1,6)]
colnames(col.poly_sudogwon2)
for(i in 1:1){
  plot.map(as.numeric(E_sudogwon[,i]),col.poly_sudogwon2,main=paste("EV",i,": Moran's I = ",round(mi_sudogwon[i],3)))
}
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