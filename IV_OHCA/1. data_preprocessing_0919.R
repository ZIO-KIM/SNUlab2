##### 라이브러리 불러오기 ##### 
library(haven)
library(ivtools)
library(splines)
library(lubridate)
require(devtools)
library(instruments)
library(dlnm)
library("naivereg")
library(OneSampleMR)
library(readxl)
library(dplyr)
library(tidyr)
library(con2aqi)

setwd("D://SNUlab//IV_OHCA")

##### [1] AQI - 09-13년 #####
##### ((1-2) AQI - 09-13년 - SO2, NO2, CO, O3 (09시) #####
#2002년-2006년 pm25 결측치 많기 때문에 2007년부터 시작.
#2009년
air_four_2009_1Q <- read_excel("2009년01분기.xlsx")
air_four_2009_1Q <- air_four_2009_1Q[air_four_2009_1Q$지역=="서울" & substr(air_four_2009_1Q$측정일시,9,10)=="09",]
air_four_2009_2Q <- read_excel("2009년02분기.xlsx")
air_four_2009_2Q <- air_four_2009_2Q[air_four_2009_2Q$지역=="서울" & substr(air_four_2009_2Q$측정일시,9,10)=="09",]
air_four_2009_3Q <- read_excel("2009년03분기.xlsx")
air_four_2009_3Q <- air_four_2009_3Q[air_four_2009_3Q$지역=="서울" & substr(air_four_2009_3Q$측정일시,9,10)=="09",]
air_four_2009_4Q <- read_excel("2009년04분기.xlsx")
air_four_2009_4Q <- air_four_2009_4Q[air_four_2009_4Q$지역=="서울" & substr(air_four_2009_4Q$측정일시,9,10)=="09",]
air_four_2009 <- rbind(air_four_2009_1Q,air_four_2009_2Q,air_four_2009_3Q,air_four_2009_4Q)
#2010년
air_four_2010_1Q <- read_excel("2010년01분기.xlsx")
air_four_2010_1Q <- air_four_2010_1Q[air_four_2010_1Q$지역=="서울" & substr(air_four_2010_1Q$측정일시,9,10)=="09",]
air_four_2010_2Q <- read_excel("2010년02분기.xlsx")
air_four_2010_2Q <- air_four_2010_2Q[air_four_2010_2Q$지역=="서울" & substr(air_four_2010_2Q$측정일시,9,10)=="09",]
air_four_2010_3Q <- read_excel("2010년03분기.xlsx")
air_four_2010_3Q <- air_four_2010_3Q[air_four_2010_3Q$지역=="서울" & substr(air_four_2010_3Q$측정일시,9,10)=="09",]
air_four_2010_4Q <- read_excel("2010년04분기.xlsx")
air_four_2010_4Q <- air_four_2010_4Q[air_four_2010_4Q$지역=="서울" & substr(air_four_2010_4Q$측정일시,9,10)=="09",]
air_four_2010 <- rbind(air_four_2010_1Q,air_four_2010_2Q,air_four_2010_3Q,air_four_2010_4Q)
#2011년
air_four_2011_1Q <- read_excel("2011년01분기.xlsx")
air_four_2011_1Q <- air_four_2011_1Q[air_four_2011_1Q$지역=="서울" & substr(air_four_2011_1Q$측정일시,9,10)=="09",]
air_four_2011_2Q <- read_excel("2011년02분기.xlsx")
air_four_2011_2Q <- air_four_2011_2Q[air_four_2011_2Q$지역=="서울" & substr(air_four_2011_2Q$측정일시,9,10)=="09",]
air_four_2011_3Q <- read_excel("2011년03분기.xlsx")
air_four_2011_3Q <- air_four_2011_3Q[air_four_2011_3Q$지역=="서울" & substr(air_four_2011_3Q$측정일시,9,10)=="09",]
air_four_2011_4Q <- read_excel("2011년04분기.xlsx")
air_four_2011_4Q <- air_four_2011_4Q[air_four_2011_4Q$지역=="서울" & substr(air_four_2011_4Q$측정일시,9,10)=="09",]
air_four_2011 <- rbind(air_four_2011_1Q,air_four_2011_2Q,air_four_2011_3Q,air_four_2011_4Q)
#2012년
air_four_2012_1Q <- read_excel("2012년01분기.xlsx")
air_four_2012_1Q <- air_four_2012_1Q[air_four_2012_1Q$지역=="서울" & substr(air_four_2012_1Q$측정일시,9,10)=="09",]
air_four_2012_2Q <- read_excel("2012년02분기.xlsx")
air_four_2012_2Q <- air_four_2012_2Q[air_four_2012_2Q$지역=="서울" & substr(air_four_2012_2Q$측정일시,9,10)=="09",]
air_four_2012_3Q <- read_excel("2012년03분기.xlsx")
air_four_2012_3Q <- air_four_2012_3Q[air_four_2012_3Q$지역=="서울" & substr(air_four_2012_3Q$측정일시,9,10)=="09",]
air_four_2012_4Q <- read_excel("2012년04분기.xlsx")
air_four_2012_4Q <- air_four_2012_4Q[air_four_2012_4Q$지역=="서울" & substr(air_four_2012_4Q$측정일시,9,10)=="09",]
air_four_2012 <- rbind(air_four_2012_1Q,air_four_2012_2Q,air_four_2012_3Q,air_four_2012_4Q)
#2013년
air_four_2013_1Q <- read_excel("2013년01분기.xlsx")
air_four_2013_1Q <- air_four_2013_1Q[air_four_2013_1Q$지역=="서울" & substr(air_four_2013_1Q$측정일시,9,10)=="09",]
air_four_2013_2Q <- read_excel("2013년02분기.xlsx")
air_four_2013_2Q <- air_four_2013_2Q[air_four_2013_2Q$지역=="서울" & substr(air_four_2013_2Q$측정일시,9,10)=="09",]
air_four_2013_3Q <- read_excel("2013년03분기.xlsx")
air_four_2013_3Q <- air_four_2013_3Q[air_four_2013_3Q$지역=="서울" & substr(air_four_2013_3Q$측정일시,9,10)=="09",]
air_four_2013_4Q <- read_excel("2013년04분기.xlsx")
air_four_2013_4Q <- air_four_2013_4Q[air_four_2013_4Q$지역=="서울" & substr(air_four_2013_4Q$측정일시,9,10)=="09",]
air_four_2013 <- rbind(air_four_2013_1Q,air_four_2013_2Q,air_four_2013_3Q,air_four_2013_4Q)


air_four_0913 <- rbind(air_four_2009,air_four_2010,air_four_2011,air_four_2012,air_four_2013)
air_1317_sudogwon_3 <- read.csv("air_1317_sudogwon_3.csv")
air_dups <- air_1317_sudogwon_3[c("air_out_idx", "측정소코드")]
air_idx <- air_dups[!duplicated(air_dups),]
air_four_0913_idx <- merge(air_four_0913, air_idx, by="측정소코드",all.x=T)
air_four_0913_idx$air_out_idx[is.na(air_four_0913_idx$air_out_idx)] <- 99999
check_four_0913 <- air_four_0913_idx[air_four_0913_idx$air_out_idx!=99999,]
colnames(check_four_0913)
for (i in c(5:9)){
  check_four_0913[,i] <- as.numeric(check_four_0913[,i])
}
check_four_0913$SO2[check_four_0913$SO2<0] <- NA
check_four_0913$CO[check_four_0913$CO<0] <- NA
check_four_0913$O3[check_four_0913$O3<0] <- NA
check_four_0913$NO2[check_four_0913$NO2<0] <- NA
air_four_0913_na <- na.omit(check_four_0913) 
(nrow(check_four_0913)-nrow(air_four_0913_na))/25/5 #연 평균 14.68일 정도는 na로 삭제
air_four_0913_na2 <- air_four_0913_na[,c("측정소코드","지역","측정소명","측정일시","SO2","CO","O3","NO2","air_out_idx")]
air_four_0913_na2$date = paste0(substr(air_four_0913_na2$측정일시,1,4),"-",
                                     substr(air_four_0913_na2$측정일시,5,6),"-",
                                     substr(air_four_0913_na2$측정일시,7,8))

##### (1-2) AQI - 09-13년 - PM10, PM25(일평균) #####
# PM10 
#2009년
air_2009_1Q <- read_excel("2009년01분기.xlsx")
air_2009_1Q <- air_2009_1Q[air_2009_1Q$지역=="서울",]
air_2009_2Q <- read_excel("2009년02분기.xlsx")
air_2009_2Q <- air_2009_2Q[air_2009_2Q$지역=="서울",]
air_2009_3Q <- read_excel("2009년03분기.xlsx")
air_2009_3Q <- air_2009_3Q[air_2009_3Q$지역=="서울",]
air_2009_4Q <- read_excel("2009년04분기.xlsx")
air_2009_4Q <- air_2009_4Q[air_2009_4Q$지역=="서울",]
air_2009 <- rbind(air_2009_1Q,air_2009_2Q,air_2009_3Q,air_2009_4Q)
#2010년
air_2010_1Q <- read_excel("2010년01분기.xlsx")
air_2010_1Q <- air_2010_1Q[air_2010_1Q$지역=="서울",]
air_2010_2Q <- read_excel("2010년02분기.xlsx")
air_2010_2Q <- air_2010_2Q[air_2010_2Q$지역=="서울",]
air_2010_3Q <- read_excel("2010년03분기.xlsx")
air_2010_3Q <- air_2010_3Q[air_2010_3Q$지역=="서울",]
air_2010_4Q <- read_excel("2010년04분기.xlsx")
air_2010_4Q <- air_2010_4Q[air_2010_4Q$지역=="서울",]
air_2010 <- rbind(air_2010_1Q,air_2010_2Q,air_2010_3Q,air_2010_4Q)
#2011년
air_2011_1Q <- read_excel("2011년01분기.xlsx")
air_2011_1Q <- air_2011_1Q[air_2011_1Q$지역=="서울",]
air_2011_2Q <- read_excel("2011년02분기.xlsx")
air_2011_2Q <- air_2011_2Q[air_2011_2Q$지역=="서울",]
air_2011_3Q <- read_excel("2011년03분기.xlsx")
air_2011_3Q <- air_2011_3Q[air_2011_3Q$지역=="서울",]
air_2011_4Q <- read_excel("2011년04분기.xlsx")
air_2011_4Q <- air_2011_4Q[air_2011_4Q$지역=="서울",]
air_2011 <- rbind(air_2011_1Q,air_2011_2Q,air_2011_3Q,air_2011_4Q)
#2012년
air_2012_1Q <- read_excel("2012년01분기.xlsx")
air_2012_1Q <- air_2012_1Q[air_2012_1Q$지역=="서울",]
air_2012_2Q <- read_excel("2012년02분기.xlsx")
air_2012_2Q <- air_2012_2Q[air_2012_2Q$지역=="서울",]
air_2012_3Q <- read_excel("2012년03분기.xlsx")
air_2012_3Q <- air_2012_3Q[air_2012_3Q$지역=="서울",]
air_2012_4Q <- read_excel("2012년04분기.xlsx")
air_2012_4Q <- air_2012_4Q[air_2012_4Q$지역=="서울",]
air_2012 <- rbind(air_2012_1Q,air_2012_2Q,air_2012_3Q,air_2012_4Q)
#2013년
air_2013_1Q <- read_excel("2013년01분기.xlsx")
air_2013_1Q <- air_2013_1Q[air_2013_1Q$지역=="서울",]
air_2013_2Q <- read_excel("2013년02분기.xlsx")
air_2013_2Q <- air_2013_2Q[air_2013_2Q$지역=="서울",]
air_2013_3Q <- read_excel("2013년03분기.xlsx")
air_2013_3Q <- air_2013_3Q[air_2013_3Q$지역=="서울",]
air_2013_4Q <- read_excel("2013년04분기.xlsx")
air_2013_4Q <- air_2013_4Q[air_2013_4Q$지역=="서울",]
air_2013 <- rbind(air_2013_1Q,air_2013_2Q,air_2013_3Q,air_2013_4Q)

air_0913 <- rbind(air_2009,air_2010,air_2011,air_2012,air_2013)
air_0913_idx <- merge(air_0913, air_idx, by="측정소코드",all.x=T)
air_0913_idx$air_out_idx[is.na(air_0913_idx$air_out_idx)] <- 99999
check_0913 <- air_0913_idx[air_0913_idx$air_out_idx!=99999,]
colnames(check_0913)
for (i in c(5:9)){
  check_0913[,i] <- as.numeric(check_0913[,i])
}
check_0913$NO2[check_0913$PM10<0] <- NA
air_0913_na <- na.omit(check_0913) 
(nrow(check_0913)-nrow(air_0913_na))/25/5/24 #연 평균 0일 na로 삭제
air_0913_na_only = air_0913_na[,c("측정소코드","지역","측정소명","측정일시","PM10","주소","air_out_idx")]
air_0913_na_only$date = 
  paste0(substr(air_0913_na_only$측정일시,1,4),"-",
         substr(air_0913_na_only$측정일시,5,6),"-",
         substr(air_0913_na_only$측정일시,7,8))
air_0913_na_only_agg = aggregate(air_0913_na_only$PM10, by=list(air_0913_na_only$측정소명,
                                                                air_0913_na_only$date,
                                                                air_0913_na_only$측정소코드,
                                                                air_0913_na_only$air_out_idx),mean)
colnames(air_0913_na_only_agg) <- c("측정소명","date","측정소코드","air_out_idx","pm10_day_mean")
summary(air_0913_na_only_agg)
#PM25 #
#2009
PM25_2009 <- read.csv("PM25_2009.csv")
PM25_2009 <- PM25_2009[1:(nrow(PM25_2009)-4),] #평균 통계값 들어가 있는 마지막 4개 행 삭제. 
PM25_2009 <- PM25_2009[PM25_2009$년 %in% c("2009"),]
PM25_2009 <- PM25_2009 %>%
  mutate ( month = ifelse(PM25_2009$월 %in% c(1:9),paste0(0,PM25_2009$월),PM25_2009$월))
PM25_2009 <- PM25_2009 %>%
  mutate ( day = ifelse(PM25_2009$일%in% c(1:9),paste0(0,PM25_2009$일),PM25_2009$일))
PM25_2009 <- PM25_2009 %>%
  mutate( date = paste0(PM25_2009$년,"-",PM25_2009$month,"-",PM25_2009$day))
colnames(PM25_2009)
PM25_2009_2 <- PM25_2009[,c(33,5:29)]
for (i in 2:26){
  PM25_2009_2[,i] <-as.numeric(PM25_2009_2[,i])
}

PM25_2009_3 <- PM25_2009_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) %>%
  select(date, pm25_day_mean, 측정소명)
colnames(PM25_2009_3)[2] <- "pm25_day_mean"
summary(PM25_2009_3)
nrow(PM25_2009_3)/25

#2010
PM25_2010 <- read.csv("PM25_2010.csv")
PM25_2010 <- PM25_2010[1:(nrow(PM25_2010)-4),] #평균 통계값 들어가 있는 마지막 4개 행 삭제. 
PM25_2010 <- PM25_2010[PM25_2010$년 %in% c("2010"),]
PM25_2010 <- PM25_2010 %>%
  mutate ( month = ifelse(PM25_2010$월 %in% c(1:9),paste0(0,PM25_2010$월),PM25_2010$월))
PM25_2010 <- PM25_2010 %>%
  mutate ( day = ifelse(PM25_2010$일%in% c(1:9),paste0(0,PM25_2010$일),PM25_2010$일))
PM25_2010 <- PM25_2010 %>%
  mutate( date = paste0(PM25_2010$년,"-",PM25_2010$month,"-",PM25_2010$day))
colnames(PM25_2010)

PM25_2010_2 <- PM25_2010[,c(33,5:29)]
for (i in 2:26){
  PM25_2010_2[,i] <-as.numeric(PM25_2010_2[,i])
}
PM25_2010_3 <- PM25_2010_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) %>%
  select(date, pm25_day_mean, 측정소명)
colnames(PM25_2010_3)[2] <- "pm25_day_mean"

#2011
PM25_2011 <- read.csv("PM25_2011.csv")
PM25_2011 <- PM25_2011[1:(nrow(PM25_2011)-4),] #평균 통계값 들어가 있는 마지막 4개 행 삭제.
PM25_2011 <- PM25_2011[PM25_2011$년 %in% c("2011"),]
PM25_2011 <- PM25_2011 %>%
  mutate ( month = ifelse(PM25_2011$월 %in% c(1:9),paste0(0,PM25_2011$월),PM25_2011$월))
PM25_2011 <- PM25_2011 %>%
  mutate ( day = ifelse(PM25_2011$일%in% c(1:9),paste0(0,PM25_2011$일),PM25_2011$일))
PM25_2011 <- PM25_2011 %>%
  mutate( date = paste0(PM25_2011$년,"-",PM25_2011$month,"-",PM25_2011$day))
colnames(PM25_2011)
 
PM25_2011_2 <- PM25_2011[,c(33,5:29)]
for (i in 2:26){
  PM25_2011_2[,i] <-as.numeric(PM25_2011_2[,i])
}
PM25_2011_3 <- PM25_2011_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) %>%
  select(date, pm25_day_mean, 측정소명)
colnames(PM25_2011_3)[2] <- "pm25_day_mean"
summary(PM25_2011_3)
nrow(PM25_2011_3)/25

#2012
PM25_2012 <- read.csv("PM25_2012.csv")
PM25_2012 <- PM25_2012[1:(nrow(PM25_2012)-4),] #평균 통계값 들어가 있는 마지막 4개 행 삭제. 
PM25_2012 <- PM25_2012[PM25_2012$년 %in% c("2012"),]
PM25_2012 <- PM25_2012 %>%
  mutate ( month = ifelse(PM25_2012$월 %in% c(1:9),paste0(0,PM25_2012$월),PM25_2012$월))
PM25_2012 <- PM25_2012 %>%
  mutate ( day = ifelse(PM25_2012$일%in% c(1:9),paste0(0,PM25_2012$일),PM25_2012$일))
PM25_2012 <- PM25_2012 %>%
  mutate( date = paste0(PM25_2012$년,"-",PM25_2012$month,"-",PM25_2012$day))
colnames(PM25_2012)

PM25_2012_2 <- PM25_2012[,c(33,5:29)]
for (i in 2:26){
  PM25_2012_2[,i] <-as.numeric(PM25_2012_2[,i])
}
PM25_2012_3 <- PM25_2012_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) %>%
  select(date, pm25_day_mean, 측정소명)
colnames(PM25_2012_3)[2] <- "pm25_day_mean"
summary(PM25_2012_3)
nrow(PM25_2012_3)/25

#2013
PM25_2013 <- read.csv("PM25_2013.csv")
PM25_2013 <- PM25_2013[PM25_2013$년 %in% c("2013"),]
PM25_2013 <- PM25_2013 %>%
  mutate ( month = ifelse(PM25_2013$월 %in% c(1:9),paste0(0,PM25_2013$월),PM25_2013$월))
PM25_2013 <- PM25_2013 %>%
  mutate ( day = ifelse(PM25_2013$일%in% c(1:9),paste0(0,PM25_2013$일),PM25_2013$일))
PM25_2013 <- PM25_2013 %>%
  mutate( date = paste0(PM25_2013$년,"-",PM25_2013$month,"-",PM25_2013$day))
colnames(PM25_2013)
 
PM25_2013_2 <- PM25_2013[,c(32,5:29)]
for (i in 2:26){
  PM25_2013_2[,i] <-as.numeric(PM25_2013_2[,i])
}
PM25_2013_3 <- PM25_2013_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) %>%
  select(date, pm25_day_mean, 측정소명)
colnames(PM25_2013_3)[2] <- "pm25_day_mean"
summary(PM25_2013_3)
nrow(PM25_2013_3)/25
PM25_0913 <- rbind(PM25_2009_3,PM25_2010_3,PM25_2011_3,PM25_2012_3,PM25_2013_3)
summary(PM25_0913)
# PM10이랑 PM25 합치고 일평균 계산
air_0913_na_pm = left_join(air_0913_na_only_agg,PM25_0913,by=c("측정소명","date"))
summary(air_0913_na_pm)


##### (1-3) AQI 전체 #####
#so2, no2, co, o3 는 09시 기준
#데이터명 = air_four_0913_na2
#pm10, pm2.5 는 일 평균
#데이터명 = air_0913_na_pm
air_0913_total= left_join(air_four_0913_na2,air_0913_na_pm,by=c("date","air_out_idx"))

#na 값 제거
air_0913_not_na <- na.omit(air_0913_total) #43250-41483=1767행 제거.
air_0913_not_na$pm10_day_mean[air_0913_not_na$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_0913_not_na$so2_AQI = con2aqi("so2",air_0913_not_na$SO2*1000)
air_0913_not_na$co_AQI = con2aqi("co",air_0913_not_na$CO*10)
air_0913_not_na$o3_AQI = con2aqi("o3",air_0913_not_na$O3,"1h")
air_0913_not_na$no2_AQI = con2aqi("no2",air_0913_not_na$NO2*1000)
air_0913_not_na$pm10_AQI = con2aqi("pm10",air_0913_not_na$pm10_day_mean)
air_0913_not_na$pm25_AQI = con2aqi("pm25",air_0913_not_na$pm25_day_mean)
air_0913_not_na$AQI = (air_0913_not_na$so2_AQI+air_0913_not_na$co_AQI+air_0913_not_na$o3_AQI+air_0913_not_na$no2_AQI+air_0913_not_na$pm10_AQI+air_0913_not_na$pm25_AQI)/6
summary(air_0913_not_na$AQI)
colSums(is.na(air_0913_not_na))


##### (1-4) weather와 합치기 (temp, rain, humi 가져오기) #####
#air_out_idx 가져와서 weather_air_idx 만들기 
weather_raw <- read.csv("weather_air_final.csv")
weather_dups <- weather_raw[c("air_out_idx", "지점명.x")]
weather_idx <- weather_dups[!duplicated(weather_dups),]
weather_air_idx <- left_join(air_idx,weather_idx,by="air_out_idx")
colnames(weather_air_idx)[3] = "지점명"
#2009년
weather_2009 <- read_excel("weather_day_year2009.xlsx")
colSums(is.na(weather_2009))
weather_2009_2 <- weather_2009[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2009_3 <- left_join(weather_2009_2,weather_air_idx, by="지점명")
weather_2009_3$air_out_idx[is.na(weather_2009_3$air_out_idx)]<-99999 
weather_2009_4 = weather_2009_3[weather_2009_3$air_out_idx!=99999,]
#2010년
weather_2010 <- read_excel("weather_day_year2010.xlsx")
weather_2010_2 <- weather_2010[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2010_3 <- left_join(weather_2010_2,weather_air_idx, by="지점명")
weather_2010_3$air_out_idx[is.na(weather_2010_3$air_out_idx)]<-99999 
weather_2010_4 = weather_2010_3[weather_2010_3$air_out_idx!=99999,]
#2011년
weather_2011 <- read_excel("weather_day_year2011.xlsx")
weather_2011_2 <- weather_2011[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2011_3 <- left_join(weather_2011_2,weather_air_idx, by="지점명")
weather_2011_3$air_out_idx[is.na(weather_2011_3$air_out_idx)]<-99999 
weather_2011_4 = weather_2011_3[weather_2011_3$air_out_idx!=99999,]
#2012년
weather_2012 <- read_excel("weather_day_year2012.xlsx")
weather_2012_2 <- weather_2012[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2012_3 <- left_join(weather_2012_2,weather_air_idx, by="지점명")
weather_2012_3$air_out_idx[is.na(weather_2012_3$air_out_idx)]<-99999 
weather_2012_4 = weather_2012_3[weather_2012_3$air_out_idx!=99999,]
#2013년
weather_2013 <- read_excel("weather_day_year2013.xlsx")
weather_2013_2 <- weather_2013[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2013_3 <- left_join(weather_2013_2,weather_air_idx, by="지점명")
weather_2013_3$air_out_idx[is.na(weather_2013_3$air_out_idx)]<-99999 
weather_2013_4 = weather_2013_3[weather_2013_3$air_out_idx!=99999,]

weather_0913 <- rbind(weather_2009_4,weather_2010_4,weather_2011_4,weather_2012_4,weather_2013_4)
colnames(weather_0913)[3] <- "date"
weather_0913_2 <- aggregate(weather_0913[,c(4:6)], by=list(weather_0913$air_out_idx,
                                                           weather_0913$date),mean)
colnames(weather_0913_2)[1:2] = c("air_out_idx","date")

weather_0913_2$date <- as.character(weather_0913_2$date)
dim(weather_0913_2)
colnames(weather_0913_2)

weather_0913_3 = weather_0913_2[weather_0913_2$air_out_idx %in% unique(air_0913_not_na$air_out_idx),]

air_weather_0913 <- left_join(air_0913_not_na, weather_0913_2, by=c("date","air_out_idx"))


##### [2] 18-19년 #####
##### ((2-1) AQI - SO2, NO2, CO, O3 (09시) #####
#2018년
air_four_2018_1Q <- read_excel("2018년 1분기.xlsx")
air_four_2018_1Q <- air_four_2018_1Q[substr(air_four_2018_1Q$지역,1,2)=="서울" & substr(air_four_2018_1Q$측정일시,9,10)=="09",]
air_four_2018_2Q <- read_excel("2018년 2분기.xlsx")
air_four_2018_2Q <- air_four_2018_2Q[substr(air_four_2018_2Q$지역,1,2)=="서울" & substr(air_four_2018_2Q$측정일시,9,10)=="09",]
air_four_2018_3Q <- read_excel("2018년 3분기.xlsx")
air_four_2018_3Q <- air_four_2018_3Q[substr(air_four_2018_3Q$지역,1,2)=="서울" & substr(air_four_2018_3Q$측정일시,9,10)=="09",]
air_four_2018_3Q$망 <- NULL
air_four_2018_4Q <- read_excel("2018년 4분기.xlsx")
air_four_2018_4Q <- air_four_2018_4Q[substr(air_four_2018_4Q$지역,1,2)=="서울" & substr(air_four_2018_4Q$측정일시,9,10)=="09",]
air_four_2018_4Q$망 <- NULL
air_four_2018 <- rbind(air_four_2018_1Q,air_four_2018_2Q,air_four_2018_3Q,air_four_2018_4Q)
#2019년
air_four_2019_1m <- read_excel("2019년 1월.xlsx")
air_four_2019_1m <- air_four_2019_1m[substr(air_four_2019_1m$지역,1,2)=="서울" & substr(air_four_2019_1m$측정일시,9,10)=="09",]
air_four_2019_2m <- read_excel("2019년 2월.xlsx")
air_four_2019_2m <- air_four_2019_2m[substr(air_four_2019_2m$지역,1,2)=="서울" & substr(air_four_2019_2m$측정일시,9,10)=="09",]
air_four_2019_3m <- read_excel("2019년 3월.xlsx")
air_four_2019_3m <- air_four_2019_3m[substr(air_four_2019_3m$지역,1,2)=="서울" & substr(air_four_2019_3m$측정일시,9,10)=="09",]
air_four_2019_4m <- read_excel("2019년 4월.xlsx")
air_four_2019_4m <- air_four_2019_4m[substr(air_four_2019_4m$지역,1,2)=="서울" & substr(air_four_2019_4m$측정일시,9,10)=="09",]
air_four_2019_5m <- read_excel("2019년 5월.xlsx")
air_four_2019_5m <- air_four_2019_5m[substr(air_four_2019_5m$지역,1,2)=="서울" & substr(air_four_2019_5m$측정일시,9,10)=="09",]
air_four_2019_6m <- read_excel("2019년 6월.xlsx")
air_four_2019_6m <- air_four_2019_6m[substr(air_four_2019_6m$지역,1,2)=="서울" & substr(air_four_2019_6m$측정일시,9,10)=="09",]
air_four_2019_7m <- read_excel("2019년 7월.xlsx")
air_four_2019_7m <- air_four_2019_7m[substr(air_four_2019_7m$지역,1,2)=="서울" & substr(air_four_2019_7m$측정일시,9,10)=="09",]
air_four_2019_8m <- read_excel("2019년 8월.xlsx")
air_four_2019_8m <- air_four_2019_8m[substr(air_four_2019_8m$지역,1,2)=="서울" & substr(air_four_2019_8m$측정일시,9,10)=="09",]
air_four_2019_9m <- read_excel("2019년 9월.xlsx")
air_four_2019_9m <- air_four_2019_9m[substr(air_four_2019_9m$지역,1,2)=="서울" & substr(air_four_2019_9m$측정일시,9,10)=="09",]
air_four_2019_10m <- read_excel("2019년 10월.xlsx")
air_four_2019_10m <- air_four_2019_10m[substr(air_four_2019_10m$지역,1,2)=="서울" & substr(air_four_2019_10m$측정일시,9,10)=="09",]
air_four_2019_11m <- read_excel("2019년 11월.xlsx")
air_four_2019_11m <- air_four_2019_11m[substr(air_four_2019_11m$지역,1,2)=="서울" & substr(air_four_2019_11m$측정일시,9,10)=="09",]
air_four_2019_12m <- read_excel("2019년 12월.xlsx")
air_four_2019_12m <- air_four_2019_12m[substr(air_four_2019_12m$지역,1,2)=="서울" & substr(air_four_2019_12m$측정일시,9,10)=="09",]
air_four_2019 <- rbind(air_four_2019_1m,air_four_2019_2m,air_four_2019_3m,air_four_2019_4m,air_four_2019_5m,air_four_2019_6m
                       ,air_four_2019_7m,air_four_2019_8m,air_four_2019_9m,air_four_2019_10m,air_four_2019_11m,air_four_2019_12m)
air_four_2019$망 <- NULL


air_four_1819 <- rbind(air_four_2018,air_four_2019)
colSums(is.na(air_four_1819))
air_four_1819_idx <- merge(air_four_1819, air_idx, by="측정소코드",all.x=T)
air_four_1819_idx$air_out_idx[is.na(air_four_1819_idx$air_out_idx)] <- 99999
check_four_1819 <- air_four_1819_idx[air_four_1819_idx$air_out_idx!=99999,]
for (i in c(5:9)){
  check_four_1819[,i] <- as.numeric(check_four_1819[,i])
}
check_four_1819$SO2[check_four_1819$SO2<0] <- NA
check_four_1819$CO[check_four_1819$CO<0] <- NA
check_four_1819$O3[check_four_1819$O3<0] <- NA
check_four_1819$NO2[check_four_1819$NO2<0] <- NA
air_four_1819_na <- na.omit(check_four_1819) 
(nrow(check_four_1819)-nrow(air_four_1819_na))/25/2 #연 평균 28일 정도는 na로 삭제

air_four_1819_na2 <- air_four_1819_na[,c("측정소코드","지역","측정소명","측정일시","SO2","CO","O3","NO2","air_out_idx")]
air_four_1819_na2$date = paste0(substr(air_four_1819_na2$측정일시,1,4),"-",
                                     substr(air_four_1819_na2$측정일시,5,6),"-",
                                     substr(air_four_1819_na2$측정일시,7,8)) 

##### (2-2) AQI - PM10, PM25(일평균) #####
#2018년
air_2018_1Q <- read_excel("2018년 1분기.xlsx")
air_2018_1Q <- air_2018_1Q[substr(air_2018_1Q$지역,1,2)=="서울",]
air_2018_2Q <- read_excel("2018년 2분기.xlsx")
air_2018_2Q <- air_2018_2Q[substr(air_2018_2Q$지역,1,2)=="서울",]
air_2018_3Q <- read_excel("2018년 3분기.xlsx")
air_2018_3Q <- air_2018_3Q[substr(air_2018_3Q$지역,1,2)=="서울",]
air_2018_3Q$망 <- NULL
air_2018_4Q <- read_excel("2018년 4분기.xlsx")
air_2018_4Q <- air_2018_4Q[substr(air_2018_4Q$지역,1,2)=="서울",]
air_2018_4Q$망 <- NULL
air_2018 <- rbind(air_2018_1Q,air_2018_2Q,air_2018_3Q,air_2018_4Q)
#2019년
air_2019_1m <- read_excel("2019년 1월.xlsx")
air_2019_1m <- air_2019_1m[substr(air_2019_1m$지역,1,2)=="서울",]
air_2019_2m <- read_excel("2019년 2월.xlsx")
air_2019_2m <- air_2019_2m[substr(air_2019_2m$지역,1,2)=="서울",]
air_2019_3m <- read_excel("2019년 3월.xlsx")
air_2019_3m <- air_2019_3m[substr(air_2019_3m$지역,1,2)=="서울",]
air_2019_4m <- read_excel("2019년 4월.xlsx")
air_2019_4m <- air_2019_4m[substr(air_2019_4m$지역,1,2)=="서울",]
air_2019_5m <- read_excel("2019년 5월.xlsx")
air_2019_5m <- air_2019_5m[substr(air_2019_5m$지역,1,2)=="서울",]
air_2019_6m <- read_excel("2019년 6월.xlsx")
air_2019_6m <- air_2019_6m[substr(air_2019_6m$지역,1,2)=="서울",]
air_2019_7m <- read_excel("2019년 7월.xlsx")
air_2019_7m <- air_2019_7m[substr(air_2019_7m$지역,1,2)=="서울",]
air_2019_8m <- read_excel("2019년 8월.xlsx")
air_2019_8m <- air_2019_8m[substr(air_2019_8m$지역,1,2)=="서울",]
air_2019_9m <- read_excel("2019년 9월.xlsx")
air_2019_9m <- air_2019_9m[substr(air_2019_9m$지역,1,2)=="서울",]
air_2019_10m <- read_excel("2019년 10월.xlsx")
air_2019_10m <- air_2019_10m[substr(air_2019_10m$지역,1,2)=="서울",]
air_2019_11m <- read_excel("2019년 11월.xlsx")
air_2019_11m <- air_2019_11m[substr(air_2019_11m$지역,1,2)=="서울",]
air_2019_12m <- read_excel("2019년 12월.xlsx")
air_2019_12m <- air_2019_12m[substr(air_2019_12m$지역,1,2)=="서울",]
air_2019 <- rbind(air_2019_1m,air_2019_2m,air_2019_3m,air_2019_4m,air_2019_5m,air_2019_6m
                  ,air_2019_7m,air_2019_8m,air_2019_9m,air_2019_10m,air_2019_11m,air_2019_12m)
air_2019$망 <- NULL

air_1819 <- rbind(air_2018,air_2019)
air_1819_idx <- merge(air_1819, air_idx, by="측정소코드",all.x=T)
colSums(is.na(air_1819_idx))
air_1819_idx
air_1819_idx$air_out_idx[is.na(air_1819_idx$air_out_idx)] <- 99999
check_1819 <- air_1819_idx[air_1819_idx$air_out_idx!=99999,]
colSums(is.na(check_1819))
length(unique(check_1819$air_out_idx))
for (i in c(5:9)){
  check_1819[,i] <- as.numeric(check_1819[,i])
}
check_1819$PM10[check_1819$PM10<0] <- NA
check_1819$PM25[check_1819$PM25<0] <- NA
dim(check_1819)
air_1819_na <- na.omit(check_1819) 
dim(air_1819_na)
(nrow(check_1819)-nrow(air_1819_na))/25/2/24 #연 평균 30일 정도는 na로 삭제
colnames(air_1819_na)
air_1819_na_only = air_1819_na[,c("측정소코드","지역","측정소명","측정일시","PM10","PM25","주소","air_out_idx")]
air_1819_na_only$date = 
  paste0(substr(air_1819_na_only$측정일시,1,4),"-",
         substr(air_1819_na_only$측정일시,5,6),"-",
         substr(air_1819_na_only$측정일시,7,8))
colnames(air_1819_na_only)
air_1819_na_only_agg = aggregate(air_1819_na_only[,c(5:6)], by=list(air_1819_na_only$측정소명,
                                                                    air_1819_na_only$date,
                                                                    air_1819_na_only$측정소코드,
                                                                    air_1819_na_only$air_out_idx),mean)
colnames(air_1819_na_only_agg) <- c("측정소명","date","측정소코드","air_out_idx","pm10_day_mean","pm25_day_mean")
air_1819_total= left_join(air_four_1819_na2,air_1819_na_only_agg,by=c("date","air_out_idx"))
colSums(is.na(air_1819_total))
dim(air_1819_total)
length(unique(air_1819_total$air_out_idx))

##### (2-3) AQI 전체 #####
air_1819_total$pm10_day_mean[air_1819_total$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_1819_total$so2_AQI = con2aqi("so2",air_1819_total$SO2*1000)
air_1819_total$co_AQI = con2aqi("co",air_1819_total$CO*10)
air_1819_total$o3_AQI = con2aqi("o3",air_1819_total$O3,"1h")
air_1819_total$no2_AQI = con2aqi("no2",air_1819_total$NO2*1000)
air_1819_total$pm10_AQI = con2aqi("pm10",air_1819_total$pm10_day_mean)
air_1819_total$pm25_AQI = con2aqi("pm25",air_1819_total$pm25_day_mean)
air_1819_total$AQI = (air_1819_total$so2_AQI+air_1819_total$co_AQI+air_1819_total$o3_AQI+air_1819_total$no2_AQI+air_1819_total$pm10_AQI+air_1819_total$pm25_AQI)/6
summary(air_1819_total$AQI)

##### (2-4) weather와 합치기 (temp, rain, humi 가져오기) #####
#2018년
weather_2018 <- read_excel("weather_day_year2018.xlsx")
weather_2018_2 <- weather_2018[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2018_3 <- left_join(weather_2018_2,weather_air_idx, by="지점명")
weather_2018_3$air_out_idx[is.na(weather_2018_3$air_out_idx)]<-99999 
weather_2018_4 = weather_2018_3[weather_2018_3$air_out_idx!=99999,]
#2019년
weather_2019 <- read_excel("weather_day_year2019.xlsx")
weather_2019_2 <- weather_2019[,c("지점","지점명","dt","temp_mean","rain_sum","humi_mean")]
weather_2019_3 <- left_join(weather_2019_2,weather_air_idx, by="지점명")
weather_2019_3$air_out_idx[is.na(weather_2019_3$air_out_idx)]<-99999 
weather_2019_4 = weather_2019_3[weather_2019_3$air_out_idx!=99999,]
weather_1819 <- rbind(weather_2018_4,weather_2019_4)
colSums(is.na(weather_1819))
length(unique(weather_1819$air_out_idx))
colnames(weather_1819)[3] <- "date"
weather_1819_2 <- aggregate(weather_1819[,c(4:6)], by=list(weather_1819$air_out_idx,
                                                           weather_1819$date),mean)
colnames(weather_1819_2)[1:2] = c("air_out_idx","date")
weather_1819_2$date <- as.character(weather_1819_2$date)

weather_1819_3 = weather_1819_2[weather_1819_2$air_out_idx %in% unique(air_1819_total$air_out_idx),]

air_weather_1819 <- left_join(air_1819_total, weather_1819_2, by=c("date","air_out_idx"))
summary(air_weather_1819)


##### [3] 09-13이랑 18-19 합치기
colnames(air_weather_0913) == colnames(air_weather_1819)
air_weather_0919_without_1417 = rbind(air_weather_0913,air_weather_1819)
nrow(air_weather_0919_without_1417)/25/7
dim(air_weather_0919_without_1417)
colSums(is.na(air_weather_0919_without_1417))

##### [4] pressure 합치기 (pressure 가져오기) #####
# 얘는 생각해보니 구별이 아니라 서울 전체라서 이거 써도 되는지 모르겠다... ㅠ #
# 2009
pressure_snow_sunshine_2009 <- read.csv("pressure_snow_sunshine_2009.csv")
pressure_snow_sunshine_2009$지점 <- NULL
pressure_snow_sunshine_2009$지점명 <- NULL
colnames(pressure_snow_sunshine_2009)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2009$sunshine[is.na(pressure_snow_sunshine_2009$sunshine)] <- 0
pressure_snow_sunshine_2009$insolation[is.na(pressure_snow_sunshine_2009$insolation)] <- 0
pressure_snow_sunshine_2009$snow[is.na(pressure_snow_sunshine_2009$snow)] <- 0
pressure_snow_sunshine_2009$date = substr(pressure_snow_sunshine_2009$일시,1,10)
pressure_snow_sunshine_2009_agg_1=aggregate(pressure_snow_sunshine_2009[,2],
                                            by=list(pressure_snow_sunshine_2009$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2009_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2009_agg_2=aggregate(pressure_snow_sunshine_2009[,3:4],
                                            by=list(pressure_snow_sunshine_2009$date),sum)
colnames(pressure_snow_sunshine_2009_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2009_agg_3=aggregate(pressure_snow_sunshine_2009[,5],
                                            by=list(pressure_snow_sunshine_2009$date),max)
colnames(pressure_snow_sunshine_2009_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2009_agg <- merge(pressure_snow_sunshine_2009_agg_1,pressure_snow_sunshine_2009_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2009_agg <- merge(pressure_snow_sunshine_2009_agg,pressure_snow_sunshine_2009_agg_3, by="date", all.x=T)
# 2010
pressure_snow_sunshine_2010 <- read.csv("pressure_snow_sunshine_2010.csv")
pressure_snow_sunshine_2010$지점 <- NULL
pressure_snow_sunshine_2010$지점명 <- NULL
colnames(pressure_snow_sunshine_2010)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2010$sunshine[is.na(pressure_snow_sunshine_2010$sunshine)] <- 0
pressure_snow_sunshine_2010$insolation[is.na(pressure_snow_sunshine_2010$insolation)] <- 0
pressure_snow_sunshine_2010$snow[is.na(pressure_snow_sunshine_2010$snow)] <- 0
pressure_snow_sunshine_2010$date = substr(pressure_snow_sunshine_2010$일시,1,10)
pressure_snow_sunshine_2010_agg_1=aggregate(pressure_snow_sunshine_2010[,2],
                                            by=list(pressure_snow_sunshine_2010$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2010_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2010_agg_2=aggregate(pressure_snow_sunshine_2010[,3:4],
                                            by=list(pressure_snow_sunshine_2010$date),sum)
colnames(pressure_snow_sunshine_2010_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2010_agg_3=aggregate(pressure_snow_sunshine_2010[,5],
                                            by=list(pressure_snow_sunshine_2010$date),max)
colnames(pressure_snow_sunshine_2010_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2010_agg <- merge(pressure_snow_sunshine_2010_agg_1,pressure_snow_sunshine_2010_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2010_agg <- merge(pressure_snow_sunshine_2010_agg,pressure_snow_sunshine_2010_agg_3, by="date", all.x=T)

# 2010
pressure_snow_sunshine_2010 <- read.csv("pressure_snow_sunshine_2010.csv")
pressure_snow_sunshine_2010$지점 <- NULL
pressure_snow_sunshine_2010$지점명 <- NULL
colnames(pressure_snow_sunshine_2010)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2010$sunshine[is.na(pressure_snow_sunshine_2010$sunshine)] <- 0
pressure_snow_sunshine_2010$insolation[is.na(pressure_snow_sunshine_2010$insolation)] <- 0
pressure_snow_sunshine_2010$snow[is.na(pressure_snow_sunshine_2010$snow)] <- 0
pressure_snow_sunshine_2010$date = substr(pressure_snow_sunshine_2010$일시,1,10)
pressure_snow_sunshine_2010_agg_1=aggregate(pressure_snow_sunshine_2010[,2],
                                            by=list(pressure_snow_sunshine_2010$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2010_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2010_agg_2=aggregate(pressure_snow_sunshine_2010[,3:4],
                                            by=list(pressure_snow_sunshine_2010$date),sum)
colnames(pressure_snow_sunshine_2010_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2010_agg_3=aggregate(pressure_snow_sunshine_2010[,5],
                                            by=list(pressure_snow_sunshine_2010$date),max)
colnames(pressure_snow_sunshine_2010_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2010_agg <- merge(pressure_snow_sunshine_2010_agg_1,pressure_snow_sunshine_2010_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2010_agg <- merge(pressure_snow_sunshine_2010_agg,pressure_snow_sunshine_2010_agg_3, by="date", all.x=T)

# 2011
pressure_snow_sunshine_2011 <- read.csv("pressure_snow_sunshine_2011.csv")
pressure_snow_sunshine_2011$지점 <- NULL
pressure_snow_sunshine_2011$지점명 <- NULL
colnames(pressure_snow_sunshine_2011)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2011$sunshine[is.na(pressure_snow_sunshine_2011$sunshine)] <- 0
pressure_snow_sunshine_2011$insolation[is.na(pressure_snow_sunshine_2011$insolation)] <- 0
pressure_snow_sunshine_2011$snow[is.na(pressure_snow_sunshine_2011$snow)] <- 0
pressure_snow_sunshine_2011$date = substr(pressure_snow_sunshine_2011$일시,1,10)
pressure_snow_sunshine_2011_agg_1=aggregate(pressure_snow_sunshine_2011[,2],
                                            by=list(pressure_snow_sunshine_2011$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2011_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2011_agg_2=aggregate(pressure_snow_sunshine_2011[,3:4],
                                            by=list(pressure_snow_sunshine_2011$date),sum)
colnames(pressure_snow_sunshine_2011_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2011_agg_3=aggregate(pressure_snow_sunshine_2011[,5],
                                            by=list(pressure_snow_sunshine_2011$date),max)
colnames(pressure_snow_sunshine_2011_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2011_agg <- merge(pressure_snow_sunshine_2011_agg_1,pressure_snow_sunshine_2011_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2011_agg <- merge(pressure_snow_sunshine_2011_agg,pressure_snow_sunshine_2011_agg_3, by="date", all.x=T)

# 2012
pressure_snow_sunshine_2012 <- read.csv("pressure_snow_sunshine_2012.csv")
pressure_snow_sunshine_2012$지점 <- NULL
pressure_snow_sunshine_2012$지점명 <- NULL
colnames(pressure_snow_sunshine_2012)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2012$sunshine[is.na(pressure_snow_sunshine_2012$sunshine)] <- 0
pressure_snow_sunshine_2012$insolation[is.na(pressure_snow_sunshine_2012$insolation)] <- 0
pressure_snow_sunshine_2012$snow[is.na(pressure_snow_sunshine_2012$snow)] <- 0
pressure_snow_sunshine_2012$date = substr(pressure_snow_sunshine_2012$일시,1,10)
pressure_snow_sunshine_2012_agg_1=aggregate(pressure_snow_sunshine_2012[,2],
                                            by=list(pressure_snow_sunshine_2012$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2012_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2012_agg_2=aggregate(pressure_snow_sunshine_2012[,3:4],
                                            by=list(pressure_snow_sunshine_2012$date),sum)
colnames(pressure_snow_sunshine_2012_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2012_agg_3=aggregate(pressure_snow_sunshine_2012[,5],
                                            by=list(pressure_snow_sunshine_2012$date),max)
colnames(pressure_snow_sunshine_2012_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2012_agg <- merge(pressure_snow_sunshine_2012_agg_1,pressure_snow_sunshine_2012_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2012_agg <- merge(pressure_snow_sunshine_2012_agg,pressure_snow_sunshine_2012_agg_3, by="date", all.x=T)

# 2013
pressure_snow_sunshine_2013 <- read.csv("pressure_snow_sunshine_2013.csv")
pressure_snow_sunshine_2013$지점 <- NULL
pressure_snow_sunshine_2013$지점명 <- NULL
colnames(pressure_snow_sunshine_2013)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2013$sunshine[is.na(pressure_snow_sunshine_2013$sunshine)] <- 0
pressure_snow_sunshine_2013$insolation[is.na(pressure_snow_sunshine_2013$insolation)] <- 0
pressure_snow_sunshine_2013$snow[is.na(pressure_snow_sunshine_2013$snow)] <- 0
pressure_snow_sunshine_2013$date = substr(pressure_snow_sunshine_2013$일시,1,10)
pressure_snow_sunshine_2013_agg_1=aggregate(pressure_snow_sunshine_2013[,2],
                                            by=list(pressure_snow_sunshine_2013$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2013_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2013_agg_2=aggregate(pressure_snow_sunshine_2013[,3:4],
                                            by=list(pressure_snow_sunshine_2013$date),sum)
colnames(pressure_snow_sunshine_2013_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2013_agg_3=aggregate(pressure_snow_sunshine_2013[,5],
                                            by=list(pressure_snow_sunshine_2013$date),max)
colnames(pressure_snow_sunshine_2013_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2013_agg <- merge(pressure_snow_sunshine_2013_agg_1,pressure_snow_sunshine_2013_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2013_agg <- merge(pressure_snow_sunshine_2013_agg,pressure_snow_sunshine_2013_agg_3, by="date", all.x=T)

# 2014
pressure_snow_sunshine_2014 <- read.csv("D://SNUlab//0. data//thermal inversion//pressure_snow_sunshine//pressure_snow_sunshine_2014.csv")
pressure_snow_sunshine_2014$지점 <- NULL
pressure_snow_sunshine_2014$지점명 <- NULL
colnames(pressure_snow_sunshine_2014)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2014$sunshine[is.na(pressure_snow_sunshine_2014$sunshine)] <- 0
pressure_snow_sunshine_2014$insolation[is.na(pressure_snow_sunshine_2014$insolation)] <- 0
pressure_snow_sunshine_2014$snow[is.na(pressure_snow_sunshine_2014$snow)] <- 0
pressure_snow_sunshine_2014$date = substr(pressure_snow_sunshine_2014$일시,1,10)
pressure_snow_sunshine_2014_agg_1=aggregate(pressure_snow_sunshine_2014[,2],
                                            by=list(pressure_snow_sunshine_2014$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2014_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2014_agg_2=aggregate(pressure_snow_sunshine_2014[,3:4],
                                            by=list(pressure_snow_sunshine_2014$date),sum)
colnames(pressure_snow_sunshine_2014_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2014_agg_3=aggregate(pressure_snow_sunshine_2014[,5],
                                            by=list(pressure_snow_sunshine_2014$date),max)
colnames(pressure_snow_sunshine_2014_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2014_agg <- merge(pressure_snow_sunshine_2014_agg_1,pressure_snow_sunshine_2014_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2014_agg <- merge(pressure_snow_sunshine_2014_agg,pressure_snow_sunshine_2014_agg_3, by="date", all.x=T)
dim(pressure_snow_sunshine_2014_agg)
colSums(is.na(pressure_snow_sunshine_2014_agg))

# 2015
pressure_snow_sunshine_2015 <- read.csv("D://SNUlab//0. data//thermal inversion//pressure_snow_sunshine//pressure_snow_sunshine_2015.csv")
pressure_snow_sunshine_2015$지점 <- NULL
pressure_snow_sunshine_2015$지점명 <- NULL
colnames(pressure_snow_sunshine_2015)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2015$sunshine[is.na(pressure_snow_sunshine_2015$sunshine)] <- 0
pressure_snow_sunshine_2015$insolation[is.na(pressure_snow_sunshine_2015$insolation)] <- 0
pressure_snow_sunshine_2015$snow[is.na(pressure_snow_sunshine_2015$snow)] <- 0
pressure_snow_sunshine_2015$date = substr(pressure_snow_sunshine_2015$일시,1,10)
pressure_snow_sunshine_2015_agg_1=aggregate(pressure_snow_sunshine_2015[,2],
                                            by=list(pressure_snow_sunshine_2015$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2015_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2015_agg_2=aggregate(pressure_snow_sunshine_2015[,3:4],
                                            by=list(pressure_snow_sunshine_2015$date),sum)
colnames(pressure_snow_sunshine_2015_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2015_agg_3=aggregate(pressure_snow_sunshine_2015[,5],
                                            by=list(pressure_snow_sunshine_2015$date),max)
colnames(pressure_snow_sunshine_2015_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2015_agg <- merge(pressure_snow_sunshine_2015_agg_1,pressure_snow_sunshine_2015_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2015_agg <- merge(pressure_snow_sunshine_2015_agg,pressure_snow_sunshine_2015_agg_3, by="date", all.x=T)
dim(pressure_snow_sunshine_2015_agg)
colSums(is.na(pressure_snow_sunshine_2015_agg))

# 2016
pressure_snow_sunshine_2016 <- read.csv("D://SNUlab//0. data//thermal inversion//pressure_snow_sunshine//pressure_snow_sunshine_2016.csv")
pressure_snow_sunshine_2016$지점 <- NULL
pressure_snow_sunshine_2016$지점명 <- NULL
colnames(pressure_snow_sunshine_2016)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2016$sunshine[is.na(pressure_snow_sunshine_2016$sunshine)] <- 0
pressure_snow_sunshine_2016$insolation[is.na(pressure_snow_sunshine_2016$insolation)] <- 0
pressure_snow_sunshine_2016$snow[is.na(pressure_snow_sunshine_2016$snow)] <- 0
pressure_snow_sunshine_2016$date = substr(pressure_snow_sunshine_2016$일시,1,10)
pressure_snow_sunshine_2016_agg_1=aggregate(pressure_snow_sunshine_2016[,2],
                                            by=list(pressure_snow_sunshine_2016$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2016_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2016_agg_2=aggregate(pressure_snow_sunshine_2016[,3:4],
                                            by=list(pressure_snow_sunshine_2016$date),sum)
colnames(pressure_snow_sunshine_2016_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2016_agg_3=aggregate(pressure_snow_sunshine_2016[,5],
                                            by=list(pressure_snow_sunshine_2016$date),max)
colnames(pressure_snow_sunshine_2016_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2016_agg <- merge(pressure_snow_sunshine_2016_agg_1,pressure_snow_sunshine_2016_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2016_agg <- merge(pressure_snow_sunshine_2016_agg,pressure_snow_sunshine_2016_agg_3, by="date", all.x=T)
dim(pressure_snow_sunshine_2016_agg)
colSums(is.na(pressure_snow_sunshine_2016_agg))

# 2017
pressure_snow_sunshine_2017 <- read.csv("D://SNUlab//0. data//thermal inversion//pressure_snow_sunshine//pressure_snow_sunshine_2017.csv")
pressure_snow_sunshine_2017$지점 <- NULL
pressure_snow_sunshine_2017$지점명 <- NULL
colnames(pressure_snow_sunshine_2017)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2017$sunshine[is.na(pressure_snow_sunshine_2017$sunshine)] <- 0
pressure_snow_sunshine_2017$insolation[is.na(pressure_snow_sunshine_2017$insolation)] <- 0
pressure_snow_sunshine_2017$snow[is.na(pressure_snow_sunshine_2017$snow)] <- 0
pressure_snow_sunshine_2017$date = substr(pressure_snow_sunshine_2017$일시,1,10)
pressure_snow_sunshine_2017_agg_1=aggregate(pressure_snow_sunshine_2017[,2],
                                            by=list(pressure_snow_sunshine_2017$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2017_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2017_agg_2=aggregate(pressure_snow_sunshine_2017[,3:4],
                                            by=list(pressure_snow_sunshine_2017$date),sum)
colnames(pressure_snow_sunshine_2017_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2017_agg_3=aggregate(pressure_snow_sunshine_2017[,5],
                                            by=list(pressure_snow_sunshine_2017$date),max)
colnames(pressure_snow_sunshine_2017_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2017_agg <- merge(pressure_snow_sunshine_2017_agg_1,pressure_snow_sunshine_2017_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2017_agg <- merge(pressure_snow_sunshine_2017_agg,pressure_snow_sunshine_2017_agg_3, by="date", all.x=T)
dim(pressure_snow_sunshine_2017_agg)
colSums(is.na(pressure_snow_sunshine_2017_agg))

# 2018
pressure_snow_sunshine_2018 <- read.csv("pressure_snow_sunshine_2018.csv")
pressure_snow_sunshine_2018$지점 <- NULL
pressure_snow_sunshine_2018$지점명 <- NULL
colnames(pressure_snow_sunshine_2018)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2018$sunshine[is.na(pressure_snow_sunshine_2018$sunshine)] <- 0
pressure_snow_sunshine_2018$insolation[is.na(pressure_snow_sunshine_2018$insolation)] <- 0
pressure_snow_sunshine_2018$snow[is.na(pressure_snow_sunshine_2018$snow)] <- 0
pressure_snow_sunshine_2018$date = substr(pressure_snow_sunshine_2018$일시,1,10)
pressure_snow_sunshine_2018_agg_1=aggregate(pressure_snow_sunshine_2018[,2],
                                            by=list(pressure_snow_sunshine_2018$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2018_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2018_agg_2=aggregate(pressure_snow_sunshine_2018[,3:4],
                                            by=list(pressure_snow_sunshine_2018$date),sum)
colnames(pressure_snow_sunshine_2018_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2018_agg_3=aggregate(pressure_snow_sunshine_2018[,5],
                                            by=list(pressure_snow_sunshine_2018$date),max)
colnames(pressure_snow_sunshine_2018_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2018_agg <- merge(pressure_snow_sunshine_2018_agg_1,pressure_snow_sunshine_2018_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2018_agg <- merge(pressure_snow_sunshine_2018_agg,pressure_snow_sunshine_2018_agg_3, by="date", all.x=T)

# 2019
pressure_snow_sunshine_2019 <- read.csv("pressure_snow_sunshine_2019.csv")
pressure_snow_sunshine_2019$지점 <- NULL
pressure_snow_sunshine_2019$지점명 <- NULL
colnames(pressure_snow_sunshine_2019)[2:5]<- c("pressure","sunshine","insolation","snow")
pressure_snow_sunshine_2019$sunshine[is.na(pressure_snow_sunshine_2019$sunshine)] <- 0
pressure_snow_sunshine_2019$insolation[is.na(pressure_snow_sunshine_2019$insolation)] <- 0
pressure_snow_sunshine_2019$snow[is.na(pressure_snow_sunshine_2019$snow)] <- 0
pressure_snow_sunshine_2019$date = substr(pressure_snow_sunshine_2019$일시,1,10)
pressure_snow_sunshine_2019_agg_1=aggregate(pressure_snow_sunshine_2019[,2],
                                            by=list(pressure_snow_sunshine_2019$date),mean,na.rm=T)
colnames(pressure_snow_sunshine_2019_agg_1) <- c("date","pressure_mean")
pressure_snow_sunshine_2019_agg_2=aggregate(pressure_snow_sunshine_2019[,3:4],
                                            by=list(pressure_snow_sunshine_2019$date),sum)
colnames(pressure_snow_sunshine_2019_agg_2) <- c("date","sunshine_sum","insolation_sum")
pressure_snow_sunshine_2019_agg_3=aggregate(pressure_snow_sunshine_2019[,5],
                                            by=list(pressure_snow_sunshine_2019$date),max)
colnames(pressure_snow_sunshine_2019_agg_3) <- c("date","snow_max")
pressure_snow_sunshine_2019_agg <- merge(pressure_snow_sunshine_2019_agg_1,pressure_snow_sunshine_2019_agg_2, by="date", all.x=T)
pressure_snow_sunshine_2019_agg <- merge(pressure_snow_sunshine_2019_agg,pressure_snow_sunshine_2019_agg_3, by="date", all.x=T)

pressure_snow_sunshine_0919_without_1417 <- rbind(pressure_snow_sunshine_2009_agg,pressure_snow_sunshine_2010_agg,
                                                  pressure_snow_sunshine_2011_agg,pressure_snow_sunshine_2012_agg,
                                                  pressure_snow_sunshine_2013_agg,pressure_snow_sunshine_2018_agg,
                                                  pressure_snow_sunshine_2019_agg)
air_weather_0919_without_1417_pressure <- left_join(air_weather_0919_without_1417,pressure_snow_sunshine_0919_without_1417,by="date")
dim(pressure_snow_sunshine_0919_without_1417)
head(pressure_snow_sunshine_0919_without_1417)


##### [5] basis 계산, yday, wday 만들기 #####
air_weather_0919_without_1417_pressure$yday <- yday(air_weather_0919_without_1417_pressure$date)
air_weather_0919_without_1417_pressure$wday <- wday(air_weather_0919_without_1417_pressure$date)
air_weather_0919_without_1417_pressure$wday <- as.factor(air_weather_0919_without_1417_pressure$wday)
colnames(air_weather_0919_without_1417_pressure)[22]="temp_mean_total" #temp_mean을 temp_mean_total로 바꿈
colnames(air_weather_0919_without_1417_pressure)[24]="humi_mean_total"
#temp basis
lag=7
kno_temp <- equalknots(air_weather_0919_without_1417_pressure$temp_mean_total,nk=2)
klag <- logknots(lag,nk=2)
ns.basis_temp <- crossbasis(air_weather_0919_without_1417_pressure$temp_mean_total,
                            argvar=list(knots=kno_temp),
                            group=air_weather_0919_without_1417_pressure$air_out_idx,
                            arglag=list(knots=klag),
                            lag=lag)
air_weather_0919_without_1417_pressure$ns.basis_temp <- crossbasis(air_weather_0919_without_1417_pressure$temp_mean_total,
                                                           argvar=list(knots=kno_temp),
                                                           group=air_weather_0919_without_1417_pressure$air_out_idx,
                                                           arglag=list(knots=klag),
                                                           lag=lag)

#yday basis
kno_yday <- equalknots(air_weather_0919_without_1417_pressure$yday,nk=2)
ns.basis_yday <- crossbasis(air_weather_0919_without_1417_pressure$yday,argvar=list(knots=kno_yday),
                            group=air_weather_0919_without_1417_pressure$air_out_idx)

air_weather_0919_without_1417_pressure$ns.basis_yday <- crossbasis(air_weather_0919_without_1417_pressure$yday,
                                                           argvar=list(knots=kno_yday),
                                                           group=air_weather_0919_without_1417_pressure$air_out_idx)

#humidity basis
kno_hum <- equalknots(air_weather_0919_without_1417_pressure$humi_mean_total,nk=2)

ns.basis_hum <- crossbasis(air_weather_0919_without_1417_pressure$humi_mean_total,argvar=list(knots=kno_hum),
                           group=air_weather_0919_without_1417_pressure$air_out_idx)

air_weather_0919_without_1417_pressure$ns.basis_hum <- crossbasis(air_weather_0919_without_1417_pressure$humi_mean_total,
                                            argvar=list(knots=kno_hum),group=air_weather_0919_without_1417_pressure$air_out_idx)

#pressure basis
kno_pressure <- equalknots(air_weather_0919_without_1417_pressure$pressure_mean,nk=2)
ns.basis_pressure <- crossbasis(air_weather_0919_without_1417_pressure$pressure_mean,argvar=list(knots=kno_pressure),
                                group=air_weather_0919_without_1417_pressure$air_out_idx)
air_weather_0919_without_1417_pressure$ns.basis_pressure <- crossbasis(air_weather_0919_without_1417_pressure$pressure_mean,
                                          argvar=list(knots=kno_pressure),group=air_weather_0919_without_1417_pressure$air_out_idx)


##### [6] air_weather_0919_without_1417이랑 14-17이랑 합치기 #####
air_weather_0919_without_1417_pressure_2 <- air_weather_0919_without_1417_pressure[,c(10,9,5:8,13:25,29:34)]
cols = colnames(air_weather_0919_without_1417_pressure_2)
load("day_asthma_1317_agg_seoul_step2_humi_pressure_agg.RData")
dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
colSums(is.na(day_asthma_1317_agg_seoul_step2_humi_pressure_agg))
365.25*4*25

dim(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
colSums(is.na(day_asthma_1317_agg_seoul_step2_humi_pressure_agg))
merge_1417_2 <- day_asthma_1317_agg_seoul_step2_humi_pressure_agg[,cols]
colnames(air_weather_0919_without_1417_pressure_2)==colnames(merge_1417_2)
data_0919_without_outcome = rbind(air_weather_0919_without_1417_pressure_2,merge_1417_2)
nrow(data_0919_without_outcome)/25/11 #연평균 38일 정도 na로 제거
dim(data_0919_without_outcome)
colnames(data_0919_without_outcome)
colnames(air_weather_0919_without_1417_pressure_2)
dim(air_weather_0919_without_1417_pressure_2)

setwd("D://SNUlab//IV_OHCA//")
write.csv(data_0919_without_outcome, 'data_0919_without_outcome.csv')
save.image("without_outcome_0509.RData")


##### [7] thermal inversion 붙이기 #####
# time     0시 = 오전 9시 / 6시 = 오후 3시 == 오전 9시, 오후 3시 모두 IV이 있으면 2 / 하나라도 있으면 1 / 둘 다 없으면 0
library(dplyr)
osan_0213 <- read.csv("Osan_0213_InversionAdded.csv")
colnames(osan_0213)[1] <- "dt"
osan_0213$date <- paste0(substr(osan_0213$dt,40,41),"-",substr(osan_0213$dt,43,45),"-",substr(osan_0213$dt,47,50))
osan_0213$time <- substr(osan_0213$dt,36,37)

osan_1421 <- read.csv("Osan_1421_InversionAdded.csv")
colnames(osan_1421)[1] <- "dt"
osan_1421$date <- paste0(substr(osan_1421$dt,40,41),"-",substr(osan_1421$dt,43,45),"-",substr(osan_1421$dt,47,50))
osan_1421$time <- substr(osan_1421$dt,36,37)

# osan csv로 뽑아서 엑셀에서 date structure 다시 바꾸기.
write.csv(osan_0213,"osan_0213.csv")
write.csv(osan_1421,"osan_1421.csv")
# osan csv로 뽑아서 엑셀에서 date structure 다시 바꾸기.

osan_iv_date_0213 <- read.csv("osan_0213.csv")
osan_iv_date_1421 <- read.csv("osan_1421.csv")
osan_iv_date <- rbind(osan_iv_date_0213,osan_iv_date_1421)
osan_iv_date2 <- osan_iv_date[osan_iv_date$time %in% c("0","6"),]
osan_iv_date2$year = substr(osan_iv_date2$date,1,4)
osan_dups <- osan_iv_date2[c("time", "date")]
osan_iv_nodup <- osan_iv_date2[!duplicated(osan_dups),]
osan_iv_nodup2 <- osan_iv_nodup[,c("inversion_300_combined","inversion_500_combined","inversion_700_combined","time","date","year")]
osan_IV_012 <- osan_iv_nodup2 %>%
  group_by(date) %>%
  summarise(
    IV_300 = sum(inversion_300_combined),
    IV_500 = sum(inversion_500_combined),
    IV_700 = sum(inversion_700_combined)
  )
table(osan_IV_012$IV_300)
osan_IV_012$IV_300 <- as.factor(osan_IV_012$IV_300)
osan_IV_012$IV_500 <- as.factor(osan_IV_012$IV_500)
osan_IV_012$IV_700 <- as.factor(osan_IV_012$IV_700)
data_0919_without_outcome_iv <- left_join(data_0919_without_outcome,osan_IV_012, by="date")
nrow(data_0919_without_outcome)-nrow(data_0919_without_outcome_iv)
data_0919_without_outcome_iv$IV_300_BI <- ifelse(data_0919_without_outcome_iv$IV_300==0,0,1)


##### [8] outcome 붙이기 #####
outcome = read.csv("group_patient_disease_final_idx_0511.csv",header = T)
colnames(outcome)
head(outcome,1)
colnames(outcome)[c(1,19)] <- c("date","air_out_idx")
#변수명 하나 바뀜 count_disease_psychogenic -> count_disease_cardiogenic : 질병성 심정지 중, 1. 심인성 case 만 aggregate
colnames(outcome)[c(14)] <- c("count_disease_cardiogenic")

data_0919_without_outcome_iv
final_data = left_join(data_0919_without_outcome_iv, outcome, by=c("date","air_out_idx")) 
  length(unique(final_data$air_out_idx))
final_data<-final_data %>% arrange(date, air_out_idx)
nrow(data_0919_without_outcome_iv)-nrow(final_data)
# dataset 새로 받으면 2019-10-19부터 다시 받은거 붙이기 : group_patient_disease_final_idx_0511.csv <- 이게 다시 받은거



##### [9] AQI, IV_300_BI lag 0~7 만들기 #####
final_data_lag<-as.data.frame(final_data)
final_data_lag <- 
  final_data %>%
  group_by(air_out_idx) %>%
  mutate("AQI_lag1"= lag(AQI,n=1),"AQI_lag2"= lag(AQI,n=2),"AQI_lag3"= lag(AQI,n=3),
         "AQI_lag4"= lag(AQI,n=4),"AQI_lag5"= lag(AQI,n=5),"AQI_lag6"= lag(AQI,n=6),
         "AQI_lag7"= lag(AQI,n=7),"AQI_lag8"= lag(AQI,n=8),"AQI_lag9"= lag(AQI,n=9),
         "AQI_lag10"= lag(AQI,n=10),"AQI_lag11"= lag(AQI,n=11),"AQI_lag12"= lag(AQI,n=12),
         "AQI_lag13"= lag(AQI,n=13),"AQI_lag14"= lag(AQI,n=14),
         
         "IV_300_BI_lag1"=lag(IV_300_BI,n=1), "IV_300_BI_lag2"=lag(IV_300_BI,n=2), "IV_300_BI_lag3"=lag(IV_300_BI,n=3), 
         "IV_300_BI_lag4"=lag(IV_300_BI,n=4), "IV_300_BI_lag5"=lag(IV_300_BI,n=5), "IV_300_BI_lag6"=lag(IV_300_BI,n=6), 
         "IV_300_BI_lag7"=lag(IV_300_BI,n=7), "IV_300_BI_lag8"=lag(IV_300_BI,n=8), "IV_300_BI_lag9"=lag(IV_300_BI,n=9), 
         "IV_300_BI_lag10"=lag(IV_300_BI,n=10), "IV_300_BI_lag11"=lag(IV_300_BI,n=11), "IV_300_BI_lag12"=lag(IV_300_BI,n=12),  
         "IV_300_BI_lag13"=lag(IV_300_BI,n=13), "IV_300_BI_lag14"=lag(IV_300_BI,n=14))

# final_data_lag<-arrange(final_data_lag$air_out_idx)
length(unique(final_data_lag$air_out_idx))
colnames(final_data_lag)

check_air_2 <- final_data_lag %>%
  filter (air_out_idx==2)

##### [10] Instrumental Variables Analysis #####
length(unique(final_data_lag_na$air_out_idx))

final_data_lag_na = final_data_lag[complete.cases(final_data_lag[,c(15,17,21:25,29,33,50:77)]),] ;colnames(final_data_lag_na)
final_data_lag_na$IV_300_BI <- as.numeric(final_data_lag_na$IV_300_BI)
for (i in c(32:44)){
  final_data_lag_na[,i] = as.numeric(final_data_lag_na[,i])
}
glm_iv <- iv.glm(model_formula = count_disease_SuddenInfantDeath ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_lag14,
                 instrument_formula = AQI_lag14 ~ IV_300_BI_lag14,
                 data=final_data_lag_na,family =quasipoisson, link = 'log') ;summary(glm_iv)

glm_iv <- iv.glm(model_formula = count_disease_SuddenInfantDeath ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=final_data_na,family =quasipoisson, link = 'log') ;summary(glm_iv)

length(unique(final_data_lag$air_out_idx))
check_1 <- final_data_lag_na %>%
  filter (SIGUNGU_NM=="마포구")
# 
library(dplyr)


# 성표 샘 원래 코드
# ##### [9] Instrumental Variables Analysis #####
# final_data_na = final_data[complete.cases(final_data[,c(15,17,21:25,29,33)]),]
# final_data_na$IV_300_BI <- as.numeric(final_data_na$IV_300_BI)
# for (i in c(32:44)){
#   final_data_na[,i] = as.numeric(final_data_na[,i])
# }
# glm_iv <- iv.glm(model_formula = count_disease_cardiogenic ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
#                  +ns.basis_hum+ns.basis_pressure+AQI,
#                  instrument_formula = AQI ~ IV_300_BI,
#                  data=final_data_na,family =quasipoisson, link = 'log') ;summary(glm_iv)
# 

# 
# final_data_na$count_new =  final_data_na$count_non_disease +final_data_na$count_non_disease_traffic 
# 
glm_iv <- iv.glm(model_formula = count_disease_cardiogenic ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=final_data_na,family =quasipoisson, link = 'log') ;summary(glm_iv)
# table(final_data_na$count_disease_psychogenic)
# table(final_data_na$count_disease)
# glm_iv$stage_one



















# 14-17 dataset 빼서 확인
final_data_na_14_17 = final_data_na[substr(final_data_na$date,1,4) %in% c("2014","2015","2016","2017"),]

glm_iv <- iv.glm(model_formula = count_non_disease ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=final_data_na_14_17,family =quasipoisson, link = 'log');summary(glm_iv)
table(final_data_na$count_non_disease_traffic)
table(final_data_na$count_non_disease)
colnames(day_asthma_1317_agg_seoul_step2_humi_pressure_agg)
day_asthma_1317_agg_seoul_step2_humi_pressure_agg2 = day_asthma_1317_agg_seoul_step2_humi_pressure_agg[,c(1:3)]
check_data = left_join(final_data_na_14_17,day_asthma_1317_agg_seoul_step2_humi_pressure_agg2,by=c("date","air_out_idx"))
glm_iv <- iv.glm(model_formula = ASTHMA_out_total_agg ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI,
                 data=check_data,family =quasipoisson, link = 'log')
summary(glm_iv)
summary(check_data$count)

getwd()
