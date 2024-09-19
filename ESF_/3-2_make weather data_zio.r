
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

setwd('D:\\SNUlab\\ESF_')

weather_data_idx_final = read.csv('weather\\weather_data_idx_final.csv', encoding = 'euc-kr')
# weather_data_idx_final = read.csv('급성심장정지조사(08-19)SAS\\patient_final_0121_outlierprocessed (이상치_수기제거후_makeweatherdata돌리기전_복사본).csv', encoding = 'euc-kr')

#3. index , date 별 temp/humi mean 구하기 
weather_data_idx_final = weather_data_idx_final[is.na(weather_data_idx_final$humi_mean)==FALSE,]
weather_data_idx_final_arrange <- weather_data_idx_final %>% arrange(air_out_idx, dt)
weather_1317_5 <- weather_data_idx_final_arrange %>%
  mutate(temp_tc=temp_max-temp_min) %>% 
  
  group_by(air_out_idx,dt) %>%
  mutate(temp_mean_total=mean(temp_mean, na.rm=TRUE),
         temp_min_total=mean(temp_min, na.rm=TRUE),
         temp_tc_total=mean(temp_tc, na.rm=TRUE),
         humi_mean_total=mean(humi_mean, na.rm=TRUE)
  )

length(unique(weather_1317_5$air_out_idx)) # 75개 check
dim(weather_1317_5)
head(weather_1317_5)

#4. 기상 데이터 lag / MA 만들기 

#한 air_out_idx, dt 별로 한 줄씩 temp_mean_total, humi_mean_total만 나오게 하자 ∵일별 중복이 많음
weather_1317_5_nodup <- weather_1317_5 %>% 
  group_by(air_out_idx) %>% filter(! duplicated(dt))

dim(weather_1317_5_nodup)
# 213815 -> 138077

library(TTR)
library(pracma)

# weather_1317_5_sudogwon <- weather_1317_5_nodup
# weather_1317_5_sudogwon_2 <- weather_1317_5_sudogwon[substr(weather_1317_5_sudogwon$dt,1,4) %in% c("2009","2010","2011","2012","2013","2014","2015","2016","2017", "2018","2019"),]

## temp, humi 결측치 확인  
sum(is.na(weather_1317_5_nodup$temp_mean_total)) # 3 NA  
sum(is.na(weather_1317_5_nodup$temp_tc)) # 30 NA  
sum(is.na(weather_1317_5_nodup$humi_mean_total)) # 0
#3개 NA; 서초(113)- 2015.8.15-16 / 군포(40)- 2015.10.13  


# write.csv(weather_1317_5_sudogwon, file='dump\\weather_1317_5_sudogwon.csv', row.names = FALSE)
write.csv(weather_1317_5_nodup, file='weather\\weather_1217_afterR.csv', row.names = FALSE)

# 다시 3-1_make air+weather data.ipynb 로 넘어가기



# # outcome data csv 변환
# asthma = read_sas('outcome\\day_asthma_1317_cnt_3_total1.sas7bdat')
# write.csv(asthma, 'outcome\\day_asthma_1317_cnt_3_total1.csv', fileEncoding = 'euc-kr', row.names = FALSE)

# rhinitis = read_sas('outcome\\day_rhinitis_1317_cnt_3_total1.sas7bdat')
# atopic = read_sas('outcome\\day_atopic_1317_cnt_3_total1.sas7bdat')
# write.csv(rhinitis, 'outcome\\day_rhinitis_1317_cnt_3_total1.csv', fileEncoding = 'euc-kr', row.names = FALSE)
# write.csv(atopic, 'outcome\\day_atopic_1317_cnt_3_total1.csv', fileEncoding = 'euc-kr', row.names = FALSE)

###끝
