

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
library(DescTools)
library(pscl)
library(SciViews)
#################

setwd('D:\\SNUlab\\ESF_\\')
asthma = read.csv('outcome\\asthma_sudogwon_agg_2015SESadded.csv')
df <- copy(asthma)
##### 1. Select only 2014 - 2017 ##### 
dim(df) 
df <- df[substr(df$dt, 1, 4) %in% c(2014, 2015, 2016, 2017),]
dim(df) 
# colSums(is.na(df))
##################################################

##### 오산 IV 계산 다시 하기 #####
    # time     0시 = 오전 9시 / 6시 = 오후 3시 == 오전 9시, 오후 3시 모두 IV이 있으면 2 / 하나라도 있으면 1 / 둘 다 없으면 0
    library(dplyr)
    osan_raw <- read.csv("D:\\SNUlab\\0. data\\thermal inversion\\Osan_1421_InversionAdded.csv", encoding = 'euc-kr')
    head(osan_raw)
    colnames(osan_raw)[1] <- "dt"
    osan_raw$date <- paste0(substr(osan_raw$dt,47,50),"-",substr(osan_raw$dt,43,45),"-",substr(osan_raw$dt,40,41))
    osan_raw$time <- substr(osan_raw$dt,36,37)
    head(osan_raw$date)
    osan_raw_date2 <- copy(osan_raw)
    osan_raw_date2 <- osan_raw_date2[osan_raw_date2$time %in% c("00","06"),]
    osan_raw_date2$year <- substr(osan_raw_date2$date,1,4)
    head(osan_raw_date2)
    head(osan_raw_date2$year)
    # 2014-2017만 뽑기
    osan_raw_date2 <- osan_raw_date2[osan_raw_date2$year %in% c("2014", "2015", "2016", "2017"),]
    osan_dups <- osan_raw_date2[c("time", "date")]
    head(osan_raw_date2)
    head(osan_raw_date2[!duplicated(osan_dups),])
    osan_raw_nodup <- osan_raw_date2[!duplicated(osan_dups),]
    osan_raw_nodup2 <- osan_raw_nodup[,c("inversion_300_combined","inversion_500_combined","inversion_700_combined","time","date")]
    osan_IV_012 <- osan_raw_nodup2 %>%
    group_by(date) %>%
    summarise(
        IV_300 = sum(inversion_300_combined),
        IV_500 = sum(inversion_500_combined),
        IV_700 = sum(inversion_700_combined)
    )
    table(osan_IV_012$IV_300)
    867+548+13
    365*4
    osan_IV_012$IV_300 <- as.factor(osan_IV_012$IV_300)
    osan_IV_012$IV_500 <- as.factor(osan_IV_012$IV_500)
    osan_IV_012$IV_700 <- as.factor(osan_IV_012$IV_700)

    # date 형식 바꾸기
    osan_IV_012$date <- gsub("Jan", "01", osan_IV_012$date)
    osan_IV_012$date <- gsub("Feb", "02", osan_IV_012$date)
    osan_IV_012$date <- gsub("Mar", "03", osan_IV_012$date)
    osan_IV_012$date <- gsub("Apr", "04", osan_IV_012$date)
    osan_IV_012$date <- gsub("May", "05", osan_IV_012$date)
    osan_IV_012$date <- gsub("Jun", "06", osan_IV_012$date)
    osan_IV_012$date <- gsub("Jul", "07", osan_IV_012$date)
    osan_IV_012$date <- gsub("Aug", "08", osan_IV_012$date)
    osan_IV_012$date <- gsub("Sep", "09", osan_IV_012$date)
    osan_IV_012$date <- gsub("Oct", "10", osan_IV_012$date)
    osan_IV_012$date <- gsub("Nov", "11", osan_IV_012$date)
    osan_IV_012$date <- gsub("Dec", "12", osan_IV_012$date)

    osan_IV_012$IV_300_BI <- ifelse(osan_IV_012$IV_300==0,0,1)
    osan_IV_012$IV_500_BI <- ifelse(osan_IV_012$IV_500==0,0,1)
    osan_IV_012$IV_700_BI <- ifelse(osan_IV_012$IV_700==0,0,1)

    osan_IV_012$year <- substr(osan_IV_012$date, 1, 4)
    osan_IV_012$month <- substr(osan_IV_012$date, 6, 7)
    osan_IV_012$year_month <- substr(osan_IV_012$date, 1, 7)

    table(osan_IV_012$year, osan_IV_012$IV_300_BI)
    table(osan_IV_012$year, osan_IV_012$IV_500_BI)
    table(osan_IV_012$year, osan_IV_012$IV_700_BI)

    table(osan_IV_012$month, osan_IV_012$IV_300_BI)
    table(osan_IV_012$month, osan_IV_012$IV_500_BI)
    table(osan_IV_012$month, osan_IV_012$IV_700_BI)

    table(osan_IV_012$year_month, osan_IV_012$IV_300_BI)
    table(osan_IV_012$year_month, osan_IV_012$IV_500_BI)
    table(osan_IV_012$year_month, osan_IV_012$IV_700_BI)

    head(osan_IV_012)
    dim(osan_IV_012)

#################################

##### AQI 계산 #####
    library(con2aqi)
    library(psych)
    #na 값 제거
    # df <- na.omit(air_pollutant) #36525-33121=3404행 제거. 날짜는 모두 살아있음.
    # df$pm10_day_mean[df$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
    colnames(df)
    df <- df[,c('dt', 'air_out_idx', 'SO2_mean', 'CO_mean', 'O3_mean', 'NO2_mean', 'PM10_mean')]
    df$PM10_mean[df$PM10_mean>600] <- 600
    summary(df)
    head(df)
    df<- na.omit(df)
    dim(df)
    df$so2_AQI = con2aqi("so2",df$SO2_mean*1000)
    df$co_AQI = con2aqi("co",df$CO_mean*10) 
    df$o3_AQI = con2aqi("o3",df$O3_mean,"1h")
    df$no2_AQI = con2aqi("no2",df$NO2_mean*1000)
    df$pm10_AQI = con2aqi("pm10",df$PM10_mean) 
    # df$pm25_AQI = con2aqi("pm25",df$PM25_mean) # 24시간 평균?
    # df$AQI = (df$so2_AQI+df$co_AQI+df$o3_AQI+df$no2_AQI+df$pm10_AQI+df$pm25_AQI)/6

    summary(df$so2_AQI)
    summary(df$co_AQI)
    summary(df$o3_AQI)
    summary(df$no2_AQI)
    summary(df$pm10_AQI)
#################################

colnames(df)
air_data <- copy(df)
colnames(air_data)[colnames(air_data) == "dt"] = "date"

##### IV 붙이기 #####
    step1_final <- merge(air_data,osan_IV_012, by="date", all.x = T)
    colSums(is.na(step1_final))
    dim(step1_final)

    step1_final$IV_300_BI <- ifelse(step1_final$IV_300==0,0,1)
    step1_final$IV_500_BI <- ifelse(step1_final$IV_500==0,0,1)
    step1_final$IV_700_BI <- ifelse(step1_final$IV_700==0,0,1)

    step1_final_na <- na.omit(step1_final)
    dim(step1_final_na)
#################################

colnames(step1_final_na)
require(plyr)
func <- function(xx)
{
return(data.frame(so2_COR = cor(xx$o3_AQI, xx$IV_300_BI)))
}

result <- ddply(step1_final_na, .(air_out_idx), func)
result.dataframe <- as.data.frame(as.matrix(result))
write.csv(result.dataframe, file = "D:\\SNUlab\\0. data\\dump\\IV~AQI COR\\o3_aqi_IV_cor_72idx.csv")
