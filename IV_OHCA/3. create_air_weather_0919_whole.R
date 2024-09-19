
## README ##

 # air_0919_new : 2009-2019 AIR raw data (dim 97217 9)
 # air_0919_whole : 2009-2019 total days index left joined with air data (dim 100425 9)

 # weather_0919_new : 2009-2019 WEATHER raw data (dim 98579 5)
 # weather_0919_new_humi_replaced : 2009-2019 WEATHER raw data, humidity NAs replaced with adjacent areas (dim 98579 5)
 # weather_0919_whole : 2009-2019 total days index left joined with weather data (dim 100425 5)
 # weather_0919_whole_humi_replaced : weather_0919_whole, humidity NAs replaced with adjacent areas (dim 100425 5)

 # OHCA 분석에 쓰이는 최종 데이터는 하단 rdata 파일 로드 후 'results' 에 저장된 데이터를 쓰시면 됩니다. (results_1208.csv)

############


## 0. library ##
library(data.table)
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
################


## 0. set directory ##
setwd("D://SNUlab//IV_OHCA")
load(file="# sudden_death_1208.RData")
######################


## 1. air 2009 - 2019 ##
  # air 0913
  air_0913_new <- copy(air_0913_total)
  dim(air_0913_new)
  colnames(air_0913_new)
  air_0913_new <- subset(air_0913_new, select = -c(지역, 측정소코드.x, 측정소명.x, 측정소코드.y, 측정소명.y))
  colSums(is.na(air_0913_new))
  
  # air 1417
  air_1417 <- read.csv('air_pollutant_1417.csv')
  air_1417_new <- subset(air_1417, select = -c(측정소코드, 측정소명, 주소))
  colnames(air_1417_new)
  
  # air 1819
  air_1819_new <- copy(air_1819_total)
  dim(air_1819_new)
  colnames(air_1819_new)
  air_1819_new <- subset(air_1819_new, select = -c(지역, 측정소코드.x, 측정소명.x, 측정소코드.y, 측정소명.y, so2_AQI, co_AQI, o3_AQI, no2_AQI, pm10_AQI, pm25_AQI, AQI))
  colSums(is.na(air_1819_new))
  
  # concat 0919
  air_0919_new <- rbind(air_0913_new, air_1417_new, air_1819_new)
  dim(air_0919_new) # 97217 9
  colnames(air_0919_new)
  colSums(is.na(air_0919_new))
  
## 1. air 2009 - 2019 end. ##


## 2. weather 2009 - 2019 ##
  # weather 0913
  weather_0913_new <- copy(weather_0913_3)
  dim(weather_0913_new)
  colnames(weather_0913_new)
  colnames(weather_0913_new)[3]="temp_mean_total" # temp_mean changed to temp_mean_total
  
  # weather 1417
  weather_1417 <- read.csv('weather_1417.csv')
  colnames(weather_1417_new)
  weather_1417_new <- subset(weather_1417, select=-c(temp_tc_total))
  
  # weather 1819
  weather_1819_new <- copy(weather_1819_3)
  dim(weather_1819_new)
  colnames(weather_1819_new)
  colnames(weather_1819_new)[3]="temp_mean_total" # temp_mean changed to temp_mean_total
  colnames(weather_1819_new)[5]="humi_mean_total"
  
  # concat 0919
  weather_0919_new <- rbind(weather_0913_new, weather_1417_new, weather_1819_new)
  dim(weather_0919_new) # 98579 5
  
  # create weather_0919_new_humi_replaced
  weather_0919_new_humi_replaced <- subset(weather_0919_new, select = -c(humi_mean_total))
  colnames(weather_0919_new_humi_replaced)
  weather_0919_new_humi_replaced <- left_join(weather_0919_new_humi_replaced,humi_replaced_final,by=c("date","air_out_idx"))
  colnames(weather_0919_new_humi_replaced)
  colSums(is.na(weather_0919_new_humi_replaced)) # humi na 7095
  dim(weather_0919_new_humi_replaced) # 98579 5

## 2. weather 2009 - 2019 end. ##
  
  
## 3. create air, weather whole data 2009-2019 (total nrows should be 100,425) ##
  index <- read.csv('ohca_whole_days_air_out_idx.csv')
  dim(index)
  head(index)
  
  # air
  air_0919_whole <- left_join(index, air_0919_new, by=c('date', 'air_out_idx'))
  dim(air_0919_whole)
  colSums(is.na(air_0919_whole))
  
  # weather
  weather_0919_whole <- left_join(index, weather_0919_new, by=c('date', 'air_out_idx'))
  dim(weather_0919_whole)
  colSums(is.na(weather_0919_whole))
  
  # weather with humi replaced
  weather_0919_whole_humi_replaced <- subset(weather_0919_whole, select = -c(humi_mean_total))
  colnames(weather_0919_whole_humi_replaced)
  weather_0919_whole_humi_replaced <- left_join(weather_0919_whole_humi_replaced,humi_replaced_final,by=c("date","air_out_idx"))
  colnames(weather_0919_whole_humi_replaced)
  colSums(is.na(weather_0919_whole_humi_replaced)) # humi na 7290
  dim(weather_0919_whole_humi_replaced) # 100425 5
## 3. create air, weather whole data 2009-2019 (total nrows should be 100,425) end. ##
  
  
# save.image(file='# sudden_death_1208.RData')
