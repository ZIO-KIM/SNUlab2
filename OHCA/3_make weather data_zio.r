
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

setwd('D:\\SNUlab\\0. data\\')

# weather_data_idx_final = read.csv('dump\\weather_data_idx_final.csv', encoding = 'euc-kr')
weather_data_idx_final = read.csv('급성심장정지조사(08-19)SAS\\patient_final_0121_outlierprocessed (이상치_수기제거후_makeweatherdata돌리기전_복사본).csv', encoding = 'euc-kr')

#3. index , date 별 temp/humi mean 구하기 
weather_data_idx_final = weather_data_idx_final[is.na(weather_data_idx_final$humi_mean)==FALSE,]
weather_data_idx_final_arrange <- weather_data_idx_final %>% arrange(air_out_idx, PRE_ER_ARREST_DT)
weather_1317_5 <- weather_data_idx_final_arrange %>%
  mutate(temp_tc=temp_max-temp_min) %>% 
  
  group_by(air_out_idx,PRE_ER_ARREST_DT) %>%
  mutate(temp_mean_total=mean(temp_mean, na.rm=TRUE),
         temp_min_total=mean(temp_min, na.rm=TRUE),
         temp_tc_total=mean(temp_tc, na.rm=TRUE),
         humi_mean_total=mean(humi_mean, na.rm=TRUE)
  )

length(unique(weather_1317_5$air_out_idx)) # 75개 check


#4. 기상 데이터 lag / MA 만들기 

#한 air_out_idx, dt 별로 한 줄씩 temp_mean_total, humi_mean_total만 나오게 하자 ∵일별 중복이 많음
weather_1317_5_nodup <- weather_1317_5 %>% 
  group_by(air_out_idx) %>% filter(! duplicated(PRE_ER_ARREST_DT))
#504416->301396
library(TTR)
library(pracma)

weather_1317_5_sudogwon <- weather_1317_5_nodup
weather_1317_5_sudogwon_2 <- weather_1317_5_sudogwon[substr(weather_1317_5_sudogwon$PRE_ER_ARREST_DT,1,4) %in% c("2009","2010","2011","2012","2013","2014","2015","2016","2017", "2018","2019"),]

## temp, humi 결측치 확인  
sum(is.na(weather_1317_5_sudogwon_2$temp_mean_total)) # 116 NA  
sum(is.na(weather_1317_5_sudogwon_2$temp_tc)) # 172 NA  
sum(is.na(weather_1317_5_sudogwon_2$humi_mean_total)) # 0
#3개 NA; 서초(113)- 2015.8.15-16 / 군포(40)- 2015.10.13  


# write.csv(weather_1317_5_sudogwon, file='dump\\weather_1317_5_sudogwon.csv', row.names = FALSE)
write.csv(weather_1317_5_sudogwon, file='급성심장정지조사(08-19)SAS\\patient_final_0121_outlierprocessed.csv', row.names = FALSE)

###끝
