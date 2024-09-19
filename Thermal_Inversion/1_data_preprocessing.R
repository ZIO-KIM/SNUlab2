install.packages("readxl")

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
library(readxl)
#################

# cran mirror 설정
options(repos = c(CRAN = "http://cran.rstudio.com"))

## rdata file load
# load(file="D:\\SNUlab\\thermal_inversion_0623_share\\220622_thermal_inversion.RData")
load(file="D:\\SNUlab\\thermal_inversion_0623_share\\220718.RData") # - humidity < 0 삭제 후 - but 오산 IV 잘못됨 !!!
### -> 이거 load 하고 하단 첫번째 오산 IV 계산 다시 하기 돌려야 함


##### 오산 IV 계산 다시 하기 ##### - zio
# time     0시 = 오전 9시 / 6시 = 오후 3시 == 오전 9시, 오후 3시 모두 IV이 있으면 2 / 하나라도 있으면 1 / 둘 다 없으면 0
library(dplyr)
osan_raw <- read.csv("D:\\SNUlab\\0. data\\thermal inversion\\Osan_1421_InversionAdded.csv", encoding = 'euc-kr')

# osan_raw <- fread("D:\\SNUlab\\0. data\\thermal inversion\\Osan_1421_InversionAdded.csv", encoding = 'UTF-8')

# #### 맨 처음 보냈던 파일로 테스트해보자 #### --> 문제 있는 파일 맞음
# osan_raw <- read.csv("C:\\Users\\user\\Downloads\\Osan_1421_InversionAdded_TEST.csv", encoding = 'euc-kr')

# osan_raw <- read.csv("C:\\Users\\user\\Downloads\\Osan_1421_InversionAdded.csv", encoding = 'euc-kr')

# #### 1417 파일로 테스트 ####
# osan_raw <- read.csv('D:\\SNUlab\\0. data\\thermal inversion\\Osan_1417_InversionAdded.csv', encoding = 'euc-kr')

head(osan_raw)
colnames(osan_raw)[1] <- "dt"
#osan_raw <- osan_raw[substr(osan_raw$dt,40,41) %in% c("2018"),] # 2018만 뽑기
osan_raw$date <- paste0(substr(osan_raw$dt,47,50),"-",substr(osan_raw$dt,43,45),"-",substr(osan_raw$dt,40,41))
osan_raw$time <- substr(osan_raw$dt,36,37)
head(osan_raw$date)
# osan csv로 뽑아서 엑셀에서 date structure 다시 바꾸기.
# write.csv(osan_raw,"osan_raw.csv")
# osan_raw_date <- read.csv("osan_raw.csv")
# remove(osan_raw)
# osan_raw_date <- copy(osan_raw)
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

# Set up your dataframe based on the length of days.
days <-seq(as.Date("2014-01-01"), as.Date('2017-12-31'), by="days")
DF_1 <- as.data.frame(matrix(ncol=1, nrow = length(days)))

# Then, add the date data to the first column in the initialized dataframe.
DF_2 <- DF_1 %>%
  dplyr::mutate(V1 = days)

head(DF_2)
DF_2$year <- substr(DF_2$V1, 1, 4)
DF_2$month <- substr(DF_2$V1, 6, 7)
DF_2$year_month <- substr(DF_2$V1, 1, 7)

tmp <- osan_IV_012[osan_IV_012$month %in% c('04', '05', '06', '07', '08', '09'),]
dim(tmp)

tmp2 <- DF_2[DF_2$month %in% c('04', '05', '06', '07', '08', '09'),]
dim(tmp2)

tmp <- osan_IV_012[osan_IV_012$month %in% c('10', '11', '12', '01', '02', '03'),]
dim(tmp)

tmp2 <- DF_2[DF_2$month %in% c('10', '11', '12', '01', '02', '03'),]
dim(tmp2)

365.25*4
1461-1428



##### 오산 IV 계산 - 2013 ##### - zio
# time     0시 = 오전 9시 / 6시 = 오후 3시 == 오전 9시, 오후 3시 모두 IV이 있으면 2 / 하나라도 있으면 1 / 둘 다 없으면 0
library(dplyr)
osan_raw <- read.csv("D:\\SNUlab\\0. data\\thermal inversion\\Osan_0213_InversionAdded.csv", encoding = 'euc-kr')
head(osan_raw)
colnames(osan_raw)[1] <- "dt"
#osan_raw <- osan_raw[substr(osan_raw$dt,40,41) %in% c("2018"),] # 2018만 뽑기
osan_raw$date <- paste0(substr(osan_raw$dt,47,50),"-",substr(osan_raw$dt,43,45),"-",substr(osan_raw$dt,40,41))
osan_raw$time <- substr(osan_raw$dt,36,37)
head(osan_raw)
# write.csv(osan_raw,"osan_raw.csv")
# osan_raw_date <- read.csv("osan_raw.csv")
# remove(osan_raw)
osan_raw_date <- copy(osan_raw)
osan_raw_date2 <- osan_raw_date[osan_raw_date$time %in% c("00","06"),]
osan_raw_date2$year = substr(osan_raw_date2$date,1,4)
osan_raw_date2$month = substr(osan_raw_date2$date,6,8)
# 2013.12만 뽑기
osan_raw_date2 <- osan_raw_date2[osan_raw_date2$year %in% c("2013"),]
osan_raw_date2 <- osan_raw_date2[osan_raw_date2$month %in% c("Dec"),]
osan_dups <- osan_raw_date2[c("time", "date")]
osan_raw_nodup <- osan_raw_date2[!duplicated(osan_dups),]
osan_raw_nodup2 <- osan_raw_nodup[,c("inversion_300_combined","inversion_500_combined","inversion_700_combined","time","date","year")]
osan_IV_012_13 <- osan_raw_nodup2 %>%
  group_by(date) %>%
  summarise(
    IV_300 = sum(inversion_300_combined),
    IV_500 = sum(inversion_500_combined),
    IV_700 = sum(inversion_700_combined)
  )
table(osan_IV_012_13$IV_300)
osan_IV_012_13$IV_300 <- as.factor(osan_IV_012_13$IV_300)
osan_IV_012_13$IV_500 <- as.factor(osan_IV_012_13$IV_500)
osan_IV_012_13$IV_700 <- as.factor(osan_IV_012_13$IV_700)

# date 형식 바꾸기
osan_IV_012_13$date <- gsub("Jan", "01", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Feb", "02", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Mar", "03", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Apr", "04", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("May", "05", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Jun", "06", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Jul", "07", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Aug", "08", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Sep", "09", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Oct", "10", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Nov", "11", osan_IV_012_13$date)
osan_IV_012_13$date <- gsub("Dec", "12", osan_IV_012_13$date)

head(osan_IV_012_13)
dim(osan_IV_012)
365.25*4
1461-1428


##### Melbourne IV 계산 ##### - 2013 ~ 2017 - zio
# 0720 negative control IV 취소 (AQI만 계산)

# time     0시 == 오전 10시, 12시 == 오후 10시 // 오전 10시에 있으면 1 없으면 0
library(dplyr)
library(data.table)

mel_raw <- read.csv("D:\\SNUlab\\0. data\\thermal inversion\\Melbourne_1317_InversionAdded.csv", encoding = 'euc-kr')
head(mel_raw)
colnames(mel_raw)[1] <- "dt"
#osan_raw <- osan_raw[substr(osan_raw$dt,40,41) %in% c("2018"),] # 2018만 뽑기
mel_raw$date <- paste0(substr(mel_raw$dt,57,60),"-",substr(mel_raw$dt,53,55),"-",substr(mel_raw$dt,50,51))
mel_raw$time <- substr(mel_raw$dt,46, 47)
head(mel_raw)
# write.csv(osan_raw,"osan_raw.csv")
# osan_raw_date <- read.csv("osan_raw.csv")
# remove(osan_raw)
mel_raw_date <- copy(mel_raw)
mel_raw_date2 <- mel_raw_date[mel_raw_date$time %in% c("00"),]
mel_raw_date2$year = substr(mel_raw_date2$date,1,4)
mel_raw_date2$month = substr(mel_raw_date2$date,6,8)
head(mel_raw_date2)
# # 2013.12만 뽑기
# osan_raw_date2 <- osan_raw_date2[osan_raw_date2$year %in% c("2013"),]
# osan_raw_date2 <- osan_raw_date2[osan_raw_date2$month %in% c("Dec"),]
mel_dups <- mel_raw_date2[c("time", "date")]
mel_raw_nodup <- mel_raw_date2[!duplicated(mel_dups),]
mel_raw_nodup2 <- mel_raw_nodup[,c("inversion_300_combined","inversion_500_combined","inversion_700_combined","time","date","year")]
mel_IV_012_13 <- mel_raw_nodup2 %>%
  group_by(date) %>%
  summarise(
    IV_300_mel = sum(inversion_300_combined),
    IV_500_mel = sum(inversion_500_combined),
    IV_700_mel = sum(inversion_700_combined)
  )
table(mel_IV_012_13$IV_300_mel)


# date 형식 바꾸기
mel_IV_012_13$date <- gsub("Jan", "01", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Feb", "02", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Mar", "03", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Apr", "04", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("May", "05", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Jun", "06", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Jul", "07", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Aug", "08", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Sep", "09", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Oct", "10", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Nov", "11", mel_IV_012_13$date)
mel_IV_012_13$date <- gsub("Dec", "12", mel_IV_012_13$date)

mel_IV_012_13 <- arrange(mel_IV_012_13, by=date)
head(mel_IV_012_13)
table(mel_IV_012_13$IV_300_mel)

# 2013년은 12월만 뽑기
mel_IV_012_13_2013 <- mel_IV_012_13[substr(mel_IV_012_13$date, 1, 4) %in% c("2013"), ]
mel_IV_012_13_1417 <- mel_IV_012_13[substr(mel_IV_012_13$date, 1, 4) %in% c("2014", "2015", "2016", "2017"), ]
mel_IV_012_13_2013_12 <- mel_IV_012_13_2013[substr(mel_IV_012_13_2013$date, 6, 7) %in% c("12"), ]

mel_IV_012_final <- rbind(mel_IV_012_13_2013_12, mel_IV_012_13_1417)


head(mel_IV_012_final)
table(mel_IV_012_final$IV_300_mel)


# # melbourne air data 불러오기 - API 만 있는 version
# mel_air <- read.csv('D:\\SNUlab\\0. data\\thermal inversion\\Australia_Victoria_Air\\alphington_1317_air.csv')
# head(mel_air)

# # mel_air + mel_inversion
# mel_final <- merge(mel_air, mel_IV_012_final, by = c('date'))
# head(mel_final)
# dim(mel_final)
# cbind(lapply(lapply(mel_final, is.na), sum))

# # iv api correlation 확인
# # mel_fit <- glm(IV_300_mel ~ API, data = mel_final, family = "binomial")
# mel_fit <- glm(API ~ IV_300_mel, data = mel_final, family = "gaussian")
# summary(mel_fit)
# summary(mel_final$API)
# exp(1.3764)
# cor(mel_final$API, mel_final$IV_300_mel)




###### AQI 만들기 위해서, 날짜별 09시 데이터 불러오기 & pm10, pm25는 하루 평균으로 계산. ######
# air_1317_sudogwon_3 <- read.csv("air_1317_sudogwon_3.csv")

# 2013년 데이터 추가 - zio
# pm10 데이터 먼저 만들기 위해 서울만 추출
air_2013_1Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2013_air\\2013년01분기.xlsx") # nolint
air_2013_2Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2013_air\\2013년02분기.xlsx")
air_2013_3Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2013_air\\2013년03분기.xlsx")
air_2013_4Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2013_air\\2013년04분기.xlsx")

air_2013_1Q <- air_2013_1Q[air_2013_1Q$지역=="서울",]
air_2013_2Q <- air_2013_2Q[air_2013_2Q$지역=="서울",]
air_2013_3Q <- air_2013_3Q[air_2013_3Q$지역=="서울",]
air_2013_4Q <- air_2013_4Q[air_2013_4Q$지역=="서울",]

air_2013 <- rbind(air_2013_1Q,air_2013_2Q,air_2013_3Q,air_2013_4Q)
# lag를 위해 12월만 필요하므로, 12월만 추출
air_2013_12 <- air_2013[substr(air_2013$측정일시, 5, 6) == "12",]

# date column 생성
air_2013_12$date <- paste0(substr(air_2013_12$측정일시,0,4),"-",substr(air_2013_12$측정일시,5,6),"-",substr(air_2013_12$측정일시,7,8))

summary(air_2013_12$PM10)

# -999 처리
air_2013_12$PM10[air_2013_12$PM10 == -999] <- NA

air_2013_12$PM10 <- as.numeric(air_2013_12$PM10)

# 09시 추출 전, pm10_day_mean 생성
pm10_mean_data_13 <- air_2013_12 %>% 
  group_by(date, 측정소코드) %>% 
  summarise(pm10_day_mean = mean(PM10, na.rm = TRUE))

# 나머지는 09시만 추출
air_2013_1Q <- air_2013_1Q[air_2013_1Q$지역=="서울" & substr(air_2013_1Q$측정일시,9,10)=="09",]
air_2013_2Q <- air_2013_2Q[air_2013_2Q$지역=="서울" & substr(air_2013_2Q$측정일시,9,10)=="09",]
air_2013_3Q <- air_2013_3Q[air_2013_3Q$지역=="서울" & substr(air_2013_3Q$측정일시,9,10)=="09",]
air_2013_4Q <- air_2013_4Q[air_2013_4Q$지역=="서울" & substr(air_2013_4Q$측정일시,9,10)=="09",]

air_2013 <- rbind(air_2013_1Q,air_2013_2Q,air_2013_3Q,air_2013_4Q)
# lag를 위해 12월만 필요하므로, 12월만 추출
air_2013_12 <- air_2013[substr(air_2013$측정일시, 5, 6) == "12",]
colnames(air_2013_12)
colnames(air_1417)
colnames(air_2014)
colnames(air_2015)
colnames(air_2016)



# #2014년
air_2014_1Q <- read.csv("2014년1분기.csv")
air_2014_1Q <- air_2014_1Q[air_2014_1Q$지역=="서울" & substr(air_2014_1Q$측정일시,9,10)=="09",]
air_2014_2Q <- read.csv("2014년2분기.csv")
air_2014_2Q <- air_2014_2Q[air_2014_2Q$지역=="서울" & substr(air_2014_2Q$측정일시,9,10)=="09",]
air_2014_3Q <- read.csv("2014년3분기.csv")
air_2014_3Q <- air_2014_3Q[air_2014_3Q$지역=="서울" & substr(air_2014_3Q$측정일시,9,10)=="09",]
air_2014_4Q <- read.csv("2014년4분기.csv")
air_2014_4Q <- air_2014_4Q[air_2014_4Q$지역=="서울" & substr(air_2014_4Q$측정일시,9,10)=="09",]
air_2014 <- rbind(air_2014_1Q,air_2014_2Q,air_2014_3Q,air_2014_4Q)
# #2015년
air_2015_1Q <- read.csv("2015년1분기.csv", fileEncoding="UCS-2LE")
air_2015_1Q <- air_2015_1Q[air_2015_1Q$지역=="서울" & substr(air_2015_1Q$측정일시,9,10)=="09",]
air_2015_2Q <- read.csv("2015년2분기.csv", fileEncoding="UCS-2LE")
air_2015_2Q <- air_2015_2Q[air_2015_2Q$지역=="서울" & substr(air_2015_2Q$측정일시,9,10)=="09",]
air_2015_3Q <- read.csv("2015년3분기.csv", fileEncoding="UCS-2LE")
air_2015_3Q <- air_2015_3Q[air_2015_3Q$지역=="서울" & substr(air_2015_3Q$측정일시,9,10)=="09",]
air_2015_4Q <- read.csv("2015년4분기.csv", fileEncoding="UCS-2LE")
air_2015_4Q <- air_2015_4Q[air_2015_4Q$지역=="서울" & substr(air_2015_4Q$측정일시,9,10)=="09",]
air_2015 <- rbind(air_2015_1Q,air_2015_2Q,air_2015_3Q,air_2015_4Q)
# #2016년
air_2016_1Q <- read.csv("2016년 1분기.csv")
air_2016_1Q <- air_2016_1Q[air_2016_1Q$지역=="서울" & substr(air_2016_1Q$측정일시,9,10)=="09",]
air_2016_2Q <- read.csv("2016년 2분기.csv")
air_2016_2Q <- air_2016_2Q[air_2016_2Q$지역=="서울" & substr(air_2016_2Q$측정일시,9,10)=="09",]
air_2016_3Q <- read.csv("2016년 3분기.csv")
air_2016_3Q <- air_2016_3Q[air_2016_3Q$지역=="서울" & substr(air_2016_3Q$측정일시,9,10)=="09",]
air_2016_4Q <- read.csv("2016년 4분기.csv")
air_2016_4Q <- air_2016_4Q[air_2016_4Q$지역=="서울" & substr(air_2016_4Q$측정일시,9,10)=="09",]
air_2016 <- rbind(air_2016_1Q,air_2016_2Q,air_2016_3Q,air_2016_4Q)
# #2017년
air_2017_1m <- read_excel("2017년 1월.xlsx")
air_2017_1m <- air_2017_1m[substr(air_2017_1m$지역,1,2)=="서울" & substr(air_2017_1m$측정일시,9,10)=="09",]
air_2017_2m <- read_excel("2017년 2월.xlsx")
air_2017_2m <- air_2017_2m[substr(air_2017_2m$지역,1,2)=="서울" & substr(air_2017_2m$측정일시,9,10)=="09",]
air_2017_3m <- read_excel("2017년 3월.xlsx")
air_2017_3m <- air_2017_3m[substr(air_2017_3m$지역,1,2)=="서울" & substr(air_2017_3m$측정일시,9,10)=="09",]
air_2017_4m <- read_excel("2017년 4월.xlsx")
air_2017_4m <- air_2017_4m[substr(air_2017_4m$지역,1,2)=="서울" & substr(air_2017_4m$측정일시,9,10)=="09",]
air_2017_5m <- read_excel("2017년 5월.xlsx")
air_2017_5m <- air_2017_5m[substr(air_2017_5m$지역,1,2)=="서울" & substr(air_2017_5m$측정일시,9,10)=="09",]
air_2017_6m <- read_excel("2017년 6월.xlsx")
air_2017_6m <- air_2017_6m[substr(air_2017_6m$지역,1,2)=="서울" & substr(air_2017_6m$측정일시,9,10)=="09",]
air_2017_7m <- read_excel("2017년 7월.xlsx")
air_2017_7m <- air_2017_7m[substr(air_2017_7m$지역,1,2)=="서울" & substr(air_2017_7m$측정일시,9,10)=="09",]
air_2017_8m <- read_excel("2017년 8월.xlsx")
air_2017_8m <- air_2017_8m[substr(air_2017_8m$지역,1,2)=="서울" & substr(air_2017_8m$측정일시,9,10)=="09",]
air_2017_9m <- read_excel("2017년 9월.xlsx")
air_2017_9m <- air_2017_9m[substr(air_2017_9m$지역,1,2)=="서울" & substr(air_2017_9m$측정일시,9,10)=="09",]
air_2017_10m <- read_excel("2017년 10월.xlsx")
air_2017_10m <- air_2017_10m[substr(air_2017_10m$지역,1,2)=="서울" & substr(air_2017_10m$측정일시,9,10)=="09",]
air_2017_11m <- read_excel("2017년 11월.xlsx")
air_2017_11m <- air_2017_11m[substr(air_2017_11m$지역,1,2)=="서울" & substr(air_2017_11m$측정일시,9,10)=="09",]
air_2017_12m <- read_excel("2017년 12월.xlsx")
air_2017_12m <- air_2017_12m[substr(air_2017_12m$지역,1,2)=="서울" & substr(air_2017_12m$측정일시,9,10)=="09",]
air_2017 <- rbind(air_2017_1m,air_2017_2m,air_2017_3m,air_2017_4m,air_2017_5m,air_2017_6m
                  ,air_2017_7m,air_2017_8m,air_2017_9m,air_2017_10m,air_2017_11m,air_2017_12m)
air_2017$망 <- NULL
head(air_2017)

# #2018년 추가 - zio
air_2018_1Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 1분기.xlsx")
air_2018_2Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 2분기.xlsx")
air_2018_3Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 3분기.xlsx")
air_2018_4Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 4분기.xlsx")
# "망" 삭제
air_2018_3Q <- dplyr::select(air_2018_3Q, -c("망"))
air_2018_4Q <- dplyr::select(air_2018_4Q, -c("망"))

unique(air_2018_1Q$지역)
# pm10, pm25 데이터 만들기 위해 서울만 추출
air_2018_1Q <- air_2018_1Q[substr(air_2018_1Q$지역, 1, 2)=="서울"]
air_2018_2Q <- air_2018_2Q[substr(air_2018_2Q$지역, 1, 2)=="서울"]
air_2018_3Q <- air_2018_3Q[substr(air_2018_3Q$지역, 1, 2)=="서울"]
air_2018_4Q <- air_2018_4Q[substr(air_2018_4Q$지역, 1, 2)=="서울"]

air_2018 <- rbind(air_2018_1Q,air_2018_2Q,air_2018_3Q,air_2018_4Q)

# date column 생성
air_2018$date <- paste0(substr(air_2018$측정일시,0,4),"-",substr(air_2018$측정일시,5,6),"-",substr(air_2018$측정일시,7,8))

summary(air_2018)

# 09시 추출 전, pm10_day_mean 생성
pm10_mean_data <- air_2018 %>% 
  group_by(date, 측정소코드) %>% 
  summarise(pm10_day_mean = mean(PM10, na.rm = TRUE))

# 09시 추출 전, pm25_day_mean 생성
pm25_mean_data <- air_2018 %>%
  group_by(date, 측정소코드) %>%
  summarise(pm25_day_mean = mean(PM25, na.rm = TRUE))

# 나머지는 09시만 추출
air_2018_1Q <- air_2018_1Q[substr(air_2018_1Q$지역, 1, 2)=="서울" & substr(air_2018_1Q$측정일시,9,10)=="09",]
air_2018_2Q <- air_2018_2Q[substr(air_2018_2Q$지역, 1, 2)=="서울" & substr(air_2018_2Q$측정일시,9,10)=="09",]
air_2018_3Q <- air_2018_3Q[substr(air_2018_3Q$지역, 1, 2)=="서울" & substr(air_2018_3Q$측정일시,9,10)=="09",]
air_2018_4Q <- air_2018_4Q[substr(air_2018_4Q$지역, 1, 2)=="서울" & substr(air_2018_4Q$측정일시,9,10)=="09",]

air_2018 <- rbind(air_2018_1Q,air_2018_2Q,air_2018_3Q,air_2018_4Q)
cbind(lapply(lapply(air_2018, is.na), sum))



#2014-2017 합치고 air_out_idx 붙이기
air_1417 <- rbind(air_2014,air_2015,air_2016,air_2017) # 수정

air_dups <- air_1317_sudogwon_3[c("air_out_idx", "측정소코드")]
air_idx <- air_dups[!duplicated(air_dups),]
write.csv(air_idx,"air_idx.csv")
air_1417_final <- merge(air_1417, air_idx, by="측정소코드",all.x=T)
summary(air_1417_final$air_out_idx)
length(unique(air_1417_final$air_out_idx))
#air_1417_final air_out_idx에 na가 있음. 확인.
air_1417_final$air_out_idx[is.na(air_1417_final$air_out_idx)] <- 99999 
check <- air_1417_final[air_1417_final$air_out_idx==99999,]
table(check$측정소명)
check2 <- air_1417_final[air_1417_final$air_out_idx!=99999,]
table(check2$측정소명)
#check에 있는 것들은 check2에도 있으므로 air_1417_final에서 air_out_idx가 99999인 애들은 삭제해도 됨.
#한강대로 = 용산구
air_1417_final2 <- check2



# 2018에 air_out_idx 붙이기 - zio
air_18_final <- merge(air_2018, air_idx, by="측정소코드",all.x=T)
summary(air_18_final)
# date column 생성
air_18_final$date <- paste0(substr(air_18_final$측정일시,0,4),"-",substr(air_18_final$측정일시,5,6),"-",substr(air_18_final$측정일시,7,8))
# pm10, pm25 day mean 데이터 만든 거 붙이기
air_18_final <- merge(air_18_final, pm10_mean_data, by=c("측정소코드", "date"), all.x=T)
air_18_final <- merge(air_18_final, pm25_mean_data, by=c("측정소코드", "date"), all.x=T)

summary(air_18_final$air_out_idx)
# air_18_final air_out_idx에 na가 있음. 확인.
air_18_final$air_out_idx[is.na(air_18_final$air_out_idx)] <- 99999 
check18 <- air_18_final[air_18_final$air_out_idx==99999,]
table(check18$측정소명)
check18_2 <- air_18_final[air_18_final$air_out_idx!=99999,]
table(check18_2$측정소명)
#check에 있는 것들은 check2에도 있으므로 air_1417_final에서 air_out_idx가 99999인 애들은 삭제해도 됨.
#한강대로 = 용산구
air_18_final2 <- check18_2



# 2013에 air_out_idx 붙이기 - zio
air_13_final <- merge(air_2013_12, air_idx, by="측정소코드",all.x=T)
summary(air_13_final)
# date column 생성
air_13_final$date <- paste0(substr(air_13_final$측정일시,0,4),"-",substr(air_13_final$측정일시,5,6),"-",substr(air_13_final$측정일시,7,8))
# pm10, pm25 day mean 데이터 만든 거 붙이기
air_13_final <- merge(air_13_final, pm10_mean_data_13, by=c("측정소코드", "date"), all.x=T)

summary(air_13_final$air_out_idx)
# air_13_final air_out_idx에 na가 있음. 확인.
air_13_final$air_out_idx[is.na(air_13_final$air_out_idx)] <- 99999 
check13 <- air_13_final[air_13_final$air_out_idx==99999,]
table(check13$측정소명)
check13_2 <- air_13_final[air_13_final$air_out_idx!=99999,]
table(check13_2$측정소명)
#check에 있는 것들은 check2에도 있으므로 air_1417_final에서 air_out_idx가 99999인 애들은 삭제해도 됨.
#한강대로 = 용산구
air_13_final2 <- check13_2


##### air_1317_sudogwon_3에서 2014-2017 서울만 자르고 거기서 pm10, pm25만 가져오기 ##### - 이미 air_1417_final2에 PM10, PM25 다 붙어있는데 이 작업 왜 한거지?????

from_air_1317_sudogwon <- air_1317_sudogwon_3[substr(air_1317_sudogwon_3$지역,1,2)=="서울" & substr(air_1317_sudogwon_3$dt,1,4) %in% c("2014","2015","2016","2017"),]
from_air_1317_sudogwon$date = paste0(substr(from_air_1317_sudogwon$dt,1,4),substr(from_air_1317_sudogwon$dt,6,7),substr(from_air_1317_sudogwon$dt,9,10))
from_air_1317_sudogwon$측정소명[from_air_1317_sudogwon$측정소명=="한강대로"] <- "용산구"
#from_air_1317_sudogwon에서 14-17로 짤라서 pm10 가져오고, 15-17 짤라서 pm25 갖다붙이기
from_air_1317_sudogwon_pm10 <- from_air_1317_sudogwon[,c("dt","측정소명","PM10_mean")]
colnames(from_air_1317_sudogwon_pm10)[1] <- "date"

from_air_1317_sudogwon_pm25 <- from_air_1317_sudogwon[substr(from_air_1317_sudogwon$dt,1,4) %in% c("2015","2016","2017"),c("dt","측정소명","PM25_mean")]
colnames(from_air_1317_sudogwon_pm25)[1] <- "date"
colnames(from_air_1317_sudogwon)

#2014년 pm25는 여기서 가져오기. 2015-2017은 지오선생님께.  - 실행 X
pm25_2014 <- read_excel("seoul_pm25_oct_13_dec_14.xlsx")
pm25_2014 <- pm25_2014[substr(pm25_2014$날짜,1,4)==2014,c("날짜","측정소명","초미세먼지\r\nPM2.5 (㎍/m3)")] 
pm25_2014 <- pm25_2014[pm25_2014$측정소명!="평균",]
colnames(pm25_2014)[1] <- "date"
air_1417_final2$date = paste0(substr(air_1417_final2$측정일시,1,4),"-",substr(air_1417_final2$측정일시,5,6),"-",substr(air_1417_final2$측정일시,7,8))
air_1417_final2$측정소명[air_1417_final2$측정소명=="한강대로"] <- "용산구"
pm25_2014_nodup <- pm25_2014[!duplicated(pm25_2014),]

air_1417_final3 <- merge(air_1417_final2,pm25_2014_nodup,by=c("date","측정소명"), all.x=T)
colnames(air_1417_final3)[14] <- "pm25_day_mean_14"
air_1417_final4 <- merge(air_1417_final3,from_air_1317_sudogwon_pm10,by=c("date","측정소명"), all.x=T)
colnames(air_1417_final4)[15] <- "pm10_day_mean"
air_1417_final5 <- merge(air_1417_final4,from_air_1317_sudogwon_pm25,by=c("date","측정소명"), all.x=T)
colnames(air_1417_final5)[16] <- "pm25_day_mean_1517"
air_1417_final6 <- air_1417_final5
air_1417_final6$pm25_day_mean_14[is.na(air_1417_final6$pm25_day_mean_14)] <- 0
air_1417_final6$pm25_day_mean_1517[is.na(air_1417_final6$pm25_day_mean_1517)] <- 0
#이렇게 해버리면 0인 날은 bias를 일으키겠다.
air_1417_final6$pm25_day_mean = air_1417_final6$pm25_day_mean_14+air_1417_final6$pm25_day_mean_1517
colnames(air_1417_final6)
air_pollutant <- air_1417_final6[,c(1:3,13,5,12,6:9,15,17)]
air_pollutant$pm25_day_mean[air_pollutant$pm25_day_mean==0] <- NA
summary(air_pollutant$pm25_day_mean)
colnames(air_pollutant)


# 2013년 12월 pm25 가져오기 - zio 
# 13년만 따로 작업해서 final에 붙이자
library(tidyr)
# pm25_2014 <- read_excel("seoul_pm25_oct_13_dec_14.xlsx")
pm25_2013 <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2013_air\\PM25_2013.xlsx")
pm25_2013 <- pm25_2013[pm25_2013$시 == 9,]
pm25_2013 <- pm25_2013[pm25_2013$월 == 12,]
pm25_2013 <- pm25_2013[1:31, ]
pm25_2013$date <- ifelse(pm25_2013$일 %in% c(1:9), paste0(pm25_2013$년, "-", pm25_2013$월, "-0", pm25_2013$일), paste0(pm25_2013$년, "-", pm25_2013$월, "-", pm25_2013$일))
pm25_2013_2 <- pm25_2013[,c(31,5:30)]
for (i in 2:27){
  pm25_2013_2[,i] <- as.numeric(unlist(pm25_2013_2[,i]))
}
colnames(pm25_2013_2)

pm25_2013_changeshape <- pm25_2013_2 %>%
  pivot_longer(cols = ends_with("구"), names_to = '측정소명') %>%
  group_by(date,측정소명) %>%
  summarise(pm25_day_mean = mean(value, na.rm=T)) %>%
  ungroup %>%
  arrange(측정소명) 
  # %>%
  # select(date, pm25_day_mean, 측정소명)

summary(pm25_2013_changeshape)
nrow(pm25_2013_changeshape)/25

unique(pm25_2013_changeshape$측정소명)
unique(step2_final_back$측정소명)

# 2013 데이터 + pm25 데이터 merge
air_2013_12_pm25 <- merge(air_13_final2, pm25_2013_changeshape, by=c("date", "측정소명"), all.x = T)

summary(air_2013_12_pm25$pm25_day_mean)
summary(air_2013_12_pm25$pm10_day_mean)




##### AQI 계산 #####
library(con2aqi)
library(psych)
#na 값 제거
air_not_na <- na.omit(air_pollutant) #36525-33121=3404행 제거. 날짜는 모두 살아있음.
dim(air_pollutant)
write.csv(air_pollutant, 'D:\\SNUlab\\IV_OHCA\\air_pollutant_1417.csv', row.names = FALSE)

air_not_na$pm10_day_mean[air_not_na$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_not_na$so2_AQI = con2aqi("so2",air_not_na$SO2*1000)
air_not_na$co_AQI = con2aqi("co",air_not_na$CO*10) 
air_not_na$o3_AQI = con2aqi("o3",air_not_na$O3,"1h")
air_not_na$no2_AQI = con2aqi("no2",air_not_na$NO2*1000)
air_not_na$pm10_AQI = con2aqi("pm10",air_not_na$pm10_day_mean) 
air_not_na$pm25_AQI = con2aqi("pm25",air_not_na$pm25_day_mean) # 24시간 평균?
air_not_na$AQI = (air_not_na$so2_AQI+air_not_na$co_AQI+air_not_na$o3_AQI+air_not_na$no2_AQI+air_not_na$pm10_AQI+air_not_na$pm25_AQI)/6
# save.image("220414_data_preprocessing.RData")
summary(air_not_na$so2_AQI)
summary(air_not_na$co_AQI)
summary(air_not_na$o3_AQI)
summary(air_not_na$no2_AQI)
summary(air_not_na$pm10_day_mean)
summary(air_not_na$pm25_AQI)

length(unique(air_not_na$air_out_idx))
head(air_not_na)
dim(air_not_na)
dim(air_pollutant)
colSums(is.na(air_pollutant)) ##### 여기!!!
air_pollutant_2016_Nov <- air_pollutant[substr(air_pollutant$date, 1, 7) %in% c("2016-11"),]
air_not_na_2016_Nov <- air_not_na[substr(air_not_na$date, 1, 7) %in% c("2016-11"),]
dim(air_pollutant_2016_Nov)
dim(air_not_na_2016_Nov)
summary(air_pollutant_2016_Nov)
750-657
summary(air_not_na_2016_Nov)
sd(air_not_na_2016_Nov$NO2)

air_pollutant_14_15_17_Nov <- air_pollutant[substr(air_pollutant$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]
air_not_na_14_15_17_Nov <- air_not_na[substr(air_not_na$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]
dim(air_pollutant_14_15_17_Nov)
dim(air_not_na_14_15_17_Nov)
2250-2123
summary(air_pollutant_14_15_17_Nov)
summary(air_not_na_14_15_17_Nov)
sd(air_not_na_14_15_17_Nov$NO2)


# write.csv(air_not_na,"air_not_na.csv")
##### weather 붙이기 #####
weather_raw <- read.csv("weather_air_final.csv")
weather_raw_miss <- weather_raw[weather_raw$humi_mean_total<0,]
dim(weather_raw_miss)
sum(table(weather_raw_miss$humi_mean_total))
hist(weather_raw$humi_mean)
summary(weather_raw$humi_mean_total)
weather <-weather_raw[substr(weather_raw$dt,1,4) %in% c("2014","2015","2016","2017") & substr(weather_raw$지역,1,2)=="서울",c(2,3,64,74,76,77)]
colnames(weather)[2] <- "date"
dim(weather)
write.csv(weather, 'D:\\SNUlab\\IV_OHCA\\weather_1417.csv', row.names = FALSE)
air_weather = merge(air_not_na,weather,by=c("air_out_idx","date"),all.x = T)
# save.image("220414_data_preprocessing.RData")
colnames(air_weather)
colSums(is.na(weather))
colnames(weather)

dim(weather)
weather_2016_Nov <- weather[substr(weather$date, 1, 7) %in% c("2016-11"),]
weather_14_15_17_Nov <- weather[substr(weather$date, 1, 7) %in% c("2014-11", "2015-11", "2017-11"),]
dim(weather_2016_Nov)
dim(weather_14_15_17_Nov)
colSums(is.na(weather_2016_Nov))
colSums(is.na(weather_14_15_17_Nov))

weather_2016_Nov <- weather_2016_Nov[weather_2016_Nov$humi_mean_total >= 0, ]
weather_14_15_17_Nov <- weather_14_15_17_Nov[weather_14_15_17_Nov$humi_mean_total >= 0, ]

summary(weather_2016_Nov)
sd(weather_2016_Nov$humi_mean_total)

summary(weather_14_15_17_Nov)
sd(weather_14_15_17_Nov$humi_mean_total)




##### AQI 계산 ##### - 2018 - zio
# - 각 air pollutant 별로 집계방식이 다름 - 성표샘께 물어보고 진행
library(con2aqi)
library(psych)
#na 값 제거 - 이거 하기 전에 PM10, PM25 column 제거 (pm10_day_mean, pm25_day_mean 을 사용해야 함)
air_18_final2 <- subset(air_18_final2, select = -c(PM10, PM25))
dim(air_18_final2)
air_not_na_18 <- na.omit(air_18_final2) #9125-8642=483행 제거.
dim(air_not_na_18)
summary(air_not_na_18$date)
length(unique(air_not_na_18$date)) # 날짜는 다 살아있으나 측정소별로 없는 날짜가 간간히 있음

air_not_na_18$pm10_day_mean[air_not_na_18$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_not_na_18$so2_AQI = con2aqi("so2",air_not_na_18$SO2*1000) 
air_not_na_18$co_AQI = con2aqi("co",air_not_na_18$CO*10)
air_not_na_18$o3_AQI = con2aqi("o3",air_not_na_18$O3,"1h") 

air_not_na_18$no2_AQI = con2aqi("no2",air_not_na_18$NO2*1000)
air_not_na_18$pm10_AQI = con2aqi("pm10",air_not_na_18$pm10_day_mean)
air_not_na_18$pm25_AQI = con2aqi("pm25",air_not_na_18$pm25_day_mean) 
air_not_na_18$AQI = (air_not_na_18$so2_AQI+air_not_na_18$co_AQI+air_not_na_18$o3_AQI+air_not_na_18$no2_AQI+air_not_na_18$pm10_AQI+air_not_na_18$pm25_AQI)/6
# save.image("220414_data_preprocessing.RData")
summary(air_not_na_18$AQI)
dim(air_not_na_18)
# write.csv(air_not_na_18,"D:\\SNUlab\\thermal_inversion_0623_share\\air_not_na_18.csv")



##### AQI 계산 ##### - 2013 - zio
library(con2aqi)
library(psych)
#na 값 제거
air_not_na_13 <- na.omit(air_2013_12_pm25) # 775-724=51행 제거.
summary(air_not_na_13$date)
summary(air_not_na_13)
length(unique(air_not_na_13$date)) # 날짜는 다 살아있으나 측정소별로 없는 날짜가 간간히 있음

# -999 처리
air_not_na_13$SO2[air_not_na_13$SO2 == "-999"] <- NA
air_not_na_13$CO[air_not_na_13$CO == "-999"] <- NA
air_not_na_13$O3[air_not_na_13$O3 == "-999"] <- NA
air_not_na_13$NO2[air_not_na_13$NO2 == "-999"] <- NA

# change to numeric
air_not_na_13$SO2 <- as.numeric(air_not_na_13$SO2)
air_not_na_13$CO <- as.numeric(air_not_na_13$CO)
air_not_na_13$O3 <- as.numeric(air_not_na_13$O3)
air_not_na_13$NO2 <- as.numeric(air_not_na_13$NO2)

# na 제거 다시
air_not_na_13 <- na.omit(air_not_na_13) # 724 - 696 = 28행 제거.
length(unique(air_not_na_13$date))

# cal aqi
air_not_na_13$pm10_day_mean[air_not_na_13$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_not_na_13$so2_AQI = con2aqi("so2",air_not_na_13$SO2*1000) 
air_not_na_13$co_AQI = con2aqi("co",air_not_na_13$CO*10)
air_not_na_13$o3_AQI = con2aqi("o3",air_not_na_13$O3,"1h") 

air_not_na_13$no2_AQI = con2aqi("no2",air_not_na_13$NO2*1000)
air_not_na_13$pm10_AQI = con2aqi("pm10",air_not_na_13$pm10_day_mean)
air_not_na_13$pm25_AQI = con2aqi("pm25",air_not_na_13$pm25_day_mean) 
air_not_na_13$AQI = (air_not_na_13$so2_AQI+air_not_na_13$co_AQI+air_not_na_13$o3_AQI+air_not_na_13$no2_AQI+air_not_na_13$pm10_AQI+air_not_na_13$pm25_AQI)/6
# save.image("220414_data_preprocessing.RData")
summary(air_not_na_13$AQI)
# write.csv(air_not_na,"air_not_na.csv")


##### AQI 계산 ##### - melbourne - zio
library(con2aqi)
library(psych)
# 불러오기 
# alphington data
mel_air <- read.csv('D:\\SNUlab\\0. data\\thermal inversion\\Australia_Victoria_Air\\alphington_1317_air_by_pollutants.csv')
# all aggregated data
mel_air <- read.csv('D:\\SNUlab\\0. data\\thermal inversion\\Australia_Victoria_Air\\aus_1317_air_mean_by_pollutants.csv')

dim(mel_air) # 1492 7
#na 값 제거
mel_air_not_na <- na.omit(mel_air) # 1465 - 1430 = 35행 제거.
dim(mel_air_not_na)
head(mel_air_not_na)
mel_air_not_na$PM10[mel_air_not_na$PM10>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
mel_air_not_na$so2_AQI_mel = con2aqi("so2",mel_air_not_na$SO2)
mel_air_not_na$co_AQI_mel = con2aqi("co",mel_air_not_na$CO*10) 
mel_air_not_na$o3_AQI_mel = con2aqi("o3",mel_air_not_na$O3 / 1000,"1h")
mel_air_not_na$no2_AQI_mel = con2aqi("no2",mel_air_not_na$NO2)
mel_air_not_na$pm10_AQI_mel = con2aqi("pm10",mel_air_not_na$PM10) 
mel_air_not_na$pm25_AQI_mel = con2aqi("pm25",mel_air_not_na$PM2.5) 

mel_air_not_na$AQI_mel = (mel_air_not_na$so2_AQI_mel + mel_air_not_na$co_AQI_mel + mel_air_not_na$o3_AQI_mel + mel_air_not_na$no2_AQI_mel + mel_air_not_na$pm10_AQI_mel + mel_air_not_na$pm25_AQI_mel)/6
# save.image("220414_data_preprocessing.RData")
summary(mel_air_not_na$AQI_mel)
summary(air_not_na$AQI)

summary(mel_air_not_na$so2_AQI_mel)
summary(mel_air_not_na$co_AQI_mel)
summary(mel_air_not_na$o3_AQI_mel)
summary(mel_air_not_na$no2_AQI_mel)
summary(mel_air_not_na$pm10_AQI_mel)
summary(mel_air_not_na$pm25_AQI_mel)

head(air_not_na)

# mel_air + mel_inversion
mel_final <- merge(mel_air_not_na, mel_IV_012_final, by = c('date'))

cbind(lapply(lapply(mel_IV_012_final, is.na), sum))




# ##### weather 붙이기 ##### 2018 - zio - 취소
# weather_18 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\weather_2018_processed.csv")
# colnames(weather_18)[3] <- "date"
# weather_index <- read.csv("D:\\SNUlab\\0. data\\out of hospital sudden cardiac arrest\\weather_index.csv")
# # air_not_na_18 에 붙어있는 air_out_idx 에서 서울만 뽑기
# air_out_idx_seoul = merge(air_not_na_18, weather_index, by=c("air_out_idx"), all.x = T)
# air_out_idx_seoul <- subset(air_out_idx_seoul, select=c("air_out_idx"))
# air_out_idx_seoul <- distinct(air_out_idx_seoul) # dropdup

# # weather_index_final = merge(weather_index_seoul,weather_index,by=c("air_out_idx"),all.x = T)
# # weather_index_final <- weather_index_final %>% distinct(air_out_idx, 지점, 지점명, .keep_all = TRUE)

# # weather_18 에서 서울만 뽑기
# weather_18_final = merge(weather_18, air_out_idx_seoul, by=c("air_out_idx"), all = FALSE)

# # air + AQI + weather
# air_weather_18 = merge(air_not_na_18,weather_18_final,by=c("air_out_idx","date"),all.x = T) 

# #3. index , date 별 temp/humi mean 구하기 
# air_weather_18=air_weather_18[is.na(air_weather_18$humi_mean)==FALSE,]
# air_weather_18_arrange <- air_weather_18 %>% arrange(air_out_idx, date)
# air_weather_18_arrange <- air_weather_18_arrange %>%
#   mutate(temp_tc=temp_max-temp_min) %>% 
  
#   group_by(air_out_idx,date) %>%
#   mutate(temp_mean_total=mean(temp_mean, na.rm=TRUE),
#          temp_min_total=mean(temp_min, na.rm=TRUE),
#          temp_tc_total=mean(temp_tc, na.rm=TRUE),
#          humi_mean_total=mean(humi_mean, na.rm=TRUE)
#   )

# # save.image("220414_data_preprocessing.RData")





##### weather 붙이기 ##### 2013 - zio
weather_0719 <- read.csv("D:\\SNUlab\\0. data\\dump\\weather_1317_5_sudogwon.csv") # 급성심정지에서 index 까지 전처리해놓은 데이터
colnames(weather_0719)
colnames(weather_0719)[1] <- "date"
head(weather_0719)
weather_13 <- weather_0719[substr(weather_0719$date, 1, 7) %in% c("2013-12"),]
weather_index <- read.csv("D:\\SNUlab\\0. data\\out of hospital sudden cardiac arrest\\weather_index.csv")
# air_not_na_13 에 붙어있는 air_out_idx 에서 서울만 뽑기
air_out_idx_seoul = merge(air_not_na_13, weather_index, by=c("air_out_idx"), all.x = T)
air_out_idx_seoul <- subset(air_out_idx_seoul, select=c("air_out_idx"))
air_out_idx_seoul <- distinct(air_out_idx_seoul) # dropdup
dim(air_out_idx_seoul)
# weather_index_final = merge(weather_index_seoul,weather_index,by=c("air_out_idx"),all.x = T)
# weather_index_final <- weather_index_final %>% distinct(air_out_idx, 지점, 지점명, .keep_all = TRUE)

# weather_18 에서 서울만 뽑기
weather_13_final = merge(weather_13, air_out_idx_seoul, by=c("air_out_idx"), all = FALSE)

# 필요한 열만 뽑기
colnames(weather_13_final)
weather_13_final <- weather_13_final[,c(1, 2, 45:67),]

# air + AQI + weather
air_weather_13 = merge(air_not_na_13,weather_13_final,by=c("air_out_idx","date"),all.x = T) 

air_weather_13=air_weather_13[is.na(air_weather_13$humi_mean)==FALSE,]
air_weather_13_arrange <- air_weather_13 %>% arrange(air_out_idx, date)





##### IV 붙이기 ##### - 2018 - zio 
dim(osan_IV_012)
step1_final <- merge(air_not_na_18,osan_IV_012, by="date",all.x = T)
step1_final$IV_300_BI <- ifelse(step1_final$IV_300==0,0,1)
step1_final$IV_500_BI <- ifelse(step1_final$IV_500==0,0,1)
step1_final$IV_700_BI <- ifelse(step1_final$IV_700==0,0,1)
# table(step1_final$IV_700_BI) -> 모두 0
step1_final_na <- na.omit(step1_final)
dim(step1_final_na)
dim(air_not_na_18)
25 * 365

# 여기까지 한번 내보내기
write.csv(step1_final_na,"D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_na_2018.csv")
length(unique(step1_final_na$date))
length(unique(step1_final_na$air_out_idx))




##### IV 붙이기 ##### - 2013 - zio
step1_final_13 <- merge(air_weather_13_arrange,osan_IV_012_13, by="date",all.x = T)
step1_final_13$IV_300_BI <- ifelse(step1_final_13$IV_300==0,0,1)
step1_final_13$IV_500_BI <- ifelse(step1_final_13$IV_500==0,0,1)
step1_final_13$IV_700_BI <- ifelse(step1_final_13$IV_700==0,0,1)
# table(step1_final$IV_700_BI) -> 모두 0
step1_final_13_na <- na.omit(step1_final_13)
25 * 365


length(unique(step1_final_13_na$date))
length(unique(step1_final_13_na$air_out_idx))


##### IV 붙이기 #####
step1_final <- merge(air_weather,osan_IV_012, by="date", all.x = T)
colSums(is.na(step1_final))
dim(step1_final)
colnames(step2_final_back)
colnames(step1_final)

step1_final$IV_300_BI <- ifelse(step1_final$IV_300==0,0,1)
step1_final$IV_500_BI <- ifelse(step1_final$IV_500==0,0,1)
step1_final$IV_700_BI <- ifelse(step1_final$IV_700==0,0,1)

step1_final_na <- na.omit(step1_final)
dim(step1_final_na)
# 백령도 붙일 필요 없음, 여기서 2_asthma_outcome_preprocessing.R로 넘어가면 됨


install.packages("REdaS")
##### wind 추가 #####
#wind 풍향, 풍속은 09시 / 일평균 이렇게 두 가지로 처리. 
#풍향을 계산할때는sin과 cos두개로 하고, degree가 아니라 radian으로 처리. sin(deg2rad(wind_2014_check$풍향.deg.)) 이렇게.
#wind랑 기존 데이터랑 합치기 위해서, wind_air_out_idx.csv 를 만들었음.
# install.packages("REdaS")
library(dplyr)
library(stringr)
library(REdaS)

setwd("D:\\SNUlab\\thermal_inversion_0623_share\\wind")
wind_air_out_idx= read.csv("wind_air_out_idx.csv", fileEncoding = 'euc-kr')
wind_air_out_idx=wind_air_out_idx[,c(1,3)]
colnames(wind_air_out_idx)[1] = "지점명"
wind_air_out_idx = na.omit(wind_air_out_idx)
length(unique(wind_air_out_idx$air_out_idx))

### 일평균 ###
wind_2013 <- read.csv("wind_2013_dec.csv", fileEncoding = 'euc-kr')
wind_2013$지점명 = str_trim(wind_2013$지점명)
colnames(wind_2013)[3:5] = c("raw_date","wind_direction","wind_speed")
wind_2013$date = substr(wind_2013$raw_date,1,10)
wind_2013_agg=aggregate(wind_2013[,4:5],by=c(list(wind_2013$지점명),list(wind_2013$date)),mean,na.rm=T)
#wind_2013_agg에서 풍향 agg값이 0인 것들이 21개, 풍속이 0인 것들이 2개 정도 됨. 0이 실제 가능한 수치라면 괜찮음.
colnames(wind_2013_agg) <- c("지점명","date","wind_direction_day_agg","wind_speed_day_agg")
wind_2013_agg$sin_direction_day_agg = sin(deg2rad(wind_2013_agg$wind_direction_day_agg))
wind_2013_agg$cos_direction_day_agg = cos(deg2rad(wind_2013_agg$wind_direction_day_agg))


wind_2014 <- read.csv("wind_2014.csv", fileEncoding = 'euc-kr')#9.12~9.14 결측
wind_2014$지점명 = str_trim(wind_2014$지점명)
colnames(wind_2014)[3:5] = c("raw_date","wind_direction","wind_speed")
wind_2014$date = substr(wind_2014$raw_date,1,10)
wind_2014_agg=aggregate(wind_2014[,4:5],by=c(list(wind_2014$지점명),list(wind_2014$date)),mean,na.rm=T)
#wind_2014_agg에서 풍향 agg값이 0인 것들이 15개, 풍속이 0인 것들이 3개 정도 됨. 0이 실제 가능한 수치라면 괜찮음.
colnames(wind_2014_agg) <- c("지점명","date","wind_direction_day_agg","wind_speed_day_agg")
wind_2014_agg$sin_direction_day_agg = sin(deg2rad(wind_2014_agg$wind_direction_day_agg))
wind_2014_agg$cos_direction_day_agg = cos(deg2rad(wind_2014_agg$wind_direction_day_agg))


wind_2015 <- read.csv("wind_2015.csv", fileEncoding = 'euc-kr')
wind_2015$지점명 = str_trim(wind_2015$지점명)
colnames(wind_2015)[3:5] = c("raw_date","wind_direction","wind_speed")
wind_2015$date = substr(wind_2015$raw_date,1,10)
wind_2015_agg=aggregate(wind_2015[,4:5],by=c(list(wind_2015$지점명),list(wind_2015$date)),mean,na.rm=T)
#wind_2015_agg에서 풍향 agg값이 0인 것들이 0개, 풍속이 0인 것들이 0개 정도 됨. 아마도 2014에서 나온 0은 결측이라고 생각해야할지도?
colnames(wind_2015_agg) <- c("지점명","date","wind_direction_day_agg","wind_speed_day_agg")
wind_2015_agg$sin_direction_day_agg = sin(deg2rad(wind_2015_agg$wind_direction_day_agg))
wind_2015_agg$cos_direction_day_agg = cos(deg2rad(wind_2015_agg$wind_direction_day_agg))

wind_2016 <- read.csv("wind_2016.csv", fileEncoding = 'euc-kr')
wind_2016$지점명 = str_trim(wind_2016$지점명)
colnames(wind_2016)[3:5] = c("raw_date","wind_direction","wind_speed")
wind_2016$date = substr(wind_2016$raw_date,1,10)
wind_2016_agg=aggregate(wind_2016[,4:5],by=c(list(wind_2016$지점명),list(wind_2016$date)),mean,na.rm=T)
#wind_2016_agg에서 풍향 agg값이 0인 것들이 0개, 풍속이 0인 것들이 0개 정도 됨. 아마도 2014에서 나온 0은 결측이라고 생각해야할지도?
colnames(wind_2016_agg) <- c("지점명","date","wind_direction_day_agg","wind_speed_day_agg")
wind_2016_agg$sin_direction_day_agg = sin(deg2rad(wind_2016_agg$wind_direction_day_agg))
wind_2016_agg$cos_direction_day_agg = cos(deg2rad(wind_2016_agg$wind_direction_day_agg))

wind_2017 <- read.csv("wind_2017.csv", fileEncoding = 'euc-kr')
wind_2017$지점명 = str_trim(wind_2017$지점명)
colnames(wind_2017)[3:5] = c("raw_date","wind_direction","wind_speed")
wind_2017$date = substr(wind_2017$raw_date,1,10)
wind_2017_agg=aggregate(wind_2017[,4:5],by=c(list(wind_2017$지점명),list(wind_2017$date)),mean,na.rm=T)
#wind_2017_agg에서 풍향 agg값이 0인 것들이 1개, 풍속이 0인 것들이 0개 정도 됨. 아마도 2014에서 나온 0은 결측이라고 생각해야할지도?
colnames(wind_2017_agg) <- c("지점명","date","wind_direction_day_agg","wind_speed_day_agg")
wind_2017_agg$sin_direction_day_agg = sin(deg2rad(wind_2017_agg$wind_direction_day_agg))
wind_2017_agg$cos_direction_day_agg = cos(deg2rad(wind_2017_agg$wind_direction_day_agg))

wind_1317 = rbind(wind_2013_agg,wind_2014_agg,wind_2015_agg,wind_2016_agg,wind_2017_agg)
wind_1317_2 = merge(wind_1317, wind_air_out_idx, by="지점명")#35,801


### 09시 ###
wind_2013_9am = wind_2013[substr(wind_2013$raw_date,12,13)=="09",]
colnames(wind_2013_9am)[4:5] = c("wind_direction_day_9am","wind_speed_day_9am")
wind_2013_9am = wind_2013_9am[,c("지점명","date","wind_direction_day_9am","wind_speed_day_9am")]
wind_2013_9am$sin_direction_day_9am = sin(deg2rad(wind_2013_9am$wind_direction_day_9am))
wind_2013_9am$cos_direction_day_9am = cos(deg2rad(wind_2013_9am$wind_speed_day_9am))

wind_2014_9am = wind_2014[substr(wind_2014$raw_date,12,13)=="09",]
colnames(wind_2014_9am)[4:5] = c("wind_direction_day_9am","wind_speed_day_9am")
wind_2014_9am = wind_2014_9am[,c("지점명","date","wind_direction_day_9am","wind_speed_day_9am")]
wind_2014_9am$sin_direction_day_9am = sin(deg2rad(wind_2014_9am$wind_direction_day_9am))
wind_2014_9am$cos_direction_day_9am = cos(deg2rad(wind_2014_9am$wind_speed_day_9am))

wind_2015_9am = wind_2015[substr(wind_2015$raw_date,12,13)=="09",]
colnames(wind_2015_9am)[4:5] = c("wind_direction_day_9am","wind_speed_day_9am")
wind_2015_9am = wind_2015_9am[,c("지점명","date","wind_direction_day_9am","wind_speed_day_9am")]
wind_2015_9am$sin_direction_day_9am = sin(deg2rad(wind_2015_9am$wind_direction_day_9am))
wind_2015_9am$cos_direction_day_9am = cos(deg2rad(wind_2015_9am$wind_speed_day_9am))

wind_2016_9am = wind_2016[substr(wind_2016$raw_date,12,13)=="09",]
colnames(wind_2016_9am)[4:5] = c("wind_direction_day_9am","wind_speed_day_9am")
wind_2016_9am = wind_2016_9am[,c("지점명","date","wind_direction_day_9am","wind_speed_day_9am")]
wind_2016_9am$sin_direction_day_9am = sin(deg2rad(wind_2016_9am$wind_direction_day_9am))
wind_2016_9am$cos_direction_day_9am = cos(deg2rad(wind_2016_9am$wind_speed_day_9am))

wind_2017_9am = wind_2017[substr(wind_2017$raw_date,12,13)=="09",]
colnames(wind_2017_9am)[4:5] = c("wind_direction_day_9am","wind_speed_day_9am")
wind_2017_9am = wind_2017_9am[,c("지점명","date","wind_direction_day_9am","wind_speed_day_9am")]
wind_2017_9am$sin_direction_day_9am = sin(deg2rad(wind_2017_9am$wind_direction_day_9am))
wind_2017_9am$cos_direction_day_9am = cos(deg2rad(wind_2017_9am$wind_speed_day_9am))

wind_1317_9am = rbind(wind_2013_9am, wind_2014_9am,wind_2015_9am,wind_2016_9am,wind_2017_9am)
wind_1317_9am_2 = merge(wind_1317_9am, wind_air_out_idx, by="지점명") #35,692

wind_1317_final = merge(wind_1317_2, wind_1317_9am_2,by=c("지점명","date"))#35,692
#air_out_idx 하나만 남기기
wind_1317_final2 = wind_1317_final[,c(1:11)]
colnames(wind_1317_final2)[7] = "air_out_idx"


# mine
dim(wind_1317_final2)
head(wind_1317_final2)
colSums(is.na(wind_1317_final2))

dim(unique(wind_1317_final2['air_out_idx']))
head(wind_1317_final2[wind_1317_final2['air_out_idx'] == 155, ])

wind_13_test = merge(wind_2013_agg, wind_air_out_idx, by="지점명")#35,801
dim(unique(wind_13_test['air_out_idx']))
wind_14_test = merge(wind_2014_agg, wind_air_out_idx, by="지점명")#35,801
dim(unique(wind_14_test['air_out_idx']))
wind_15_test = merge(wind_2015_agg, wind_air_out_idx, by="지점명")#35,801
dim(unique(wind_15_test['air_out_idx']))
wind_16_test = merge(wind_2016_agg, wind_air_out_idx, by="지점명")#35,801
dim(unique(wind_16_test['air_out_idx']))
wind_17_test = merge(wind_2017_agg, wind_air_out_idx, by="지점명")#35,801
dim(unique(wind_17_test['air_out_idx']))






# 2013 데이터 여기까지 만들고 step2_final_back 이랑 concat - 취소
# day_asthma_1317_agg_seoul_step2_humi_pressure_agg_lag 랑 붙이자
colnames(step1_final_13_na)
step1_final_13_na$측정일시 <- as.numeric(step1_final_13_na$측정일시)
# # step2_final_back 에서 중복 열 삭제
# step2_final_back <- subset(step2_final_back, select=-c(temp_tc_total.x, humi_mean_total.x, CO_mean.x, O3_mean.x))
summary(step1_final_13_na)
write.csv(step1_final_13_na, 'D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_13_na.csv')

# bind
# ##### 2013-12까지 붙인 final data ##### - 이거 안 씀 !! lag 만들 때 step1_final_13_na 만 사용
# step2_final_back_zio <- dplyr::bind_rows(step1_final_13_na, step2_final_back)
# write.csv(step2_final_back_zio, 'D:\\SNUlab\\thermal_inversion_0623_share\\step2_final_back_zio.csv')



##### IV 2SLS 1번 식 ####
fit_1_300 <- lm(AQI~rain_sum+temp_mean_total+factor(IV_300_BI),data = step1_final_na)
summary(fit_1_300)
fit_1_500 <- lm(AQI~rain_sum+temp_mean_total+factor(IV_500_BI),data = step1_final_na)
summary(fit_1_500)
# fit_1_700 <- lm(AQI~rain_sum+temp_mean_total+factor(IV_700_BI),data = step1_final_na)
# summary(fit_1_700)


# step1_final_na 다시 불러오기 - zio
step1_final_na_2018 <- read.csv("D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_na_2018.csv")

##### outcome 붙이기 #####
outcome <- read.csv("outcome_asthma3.csv")
colnames(outcome)[16]<-"date"
step2_final <- merge(step1_final_na,outcome, by=c("date","air_out_idx"),all.x = T)
step2_final$aqi_hat_1_300 <- fit_1_300$fitted.values
step2_final$aqi_hat_1_500 <- fit_1_500$fitted.values


##### IV 2SLS 2번 식 ####
fit_2_300 <- glm(ASTHMA_em_total~aqi_hat_1_300+rain_sum+temp_mean_total,data = step2_final, family = "quasipoisson")
summary(fit_2_300)
fit_2_500 <- glm(ASTHMA_em_total~aqi_hat_1_500+rain_sum+temp_mean_total,data = step2_final, family = "quasipoisson")
summary(fit_2_500)


##### x축 시간, y축 asthma sum plot #####
plotdata <- step2_final %>% 
  group_by(date) %>% 
  summarise(asthma_sum = sum(ASTHMA_em_total))
plotdata$date <- as.Date(plotdata$date)
plot(plotdata$date, plotdata$asthma_sum, xlab="date",ylab="sum_asthma em count")
save.image("220414_data_preprocessing.RData")


##### IV를 factor로 보지 않고, continuous 변수로 보기 위해서 slope를 붙이기 #####
slope_raw <- read_excel("osan_update_0415.xlsx")
colnames(slope_raw)[1] <- "dt"
slope_raw$date <- paste0(substr(slope_raw$dt,40,41),"-",substr(slope_raw$dt,43,45),"-",substr(slope_raw$dt,47,50))
slope_raw$time <- substr(slope_raw$dt,36,37)
slope_raw$year = substr(slope_raw$date,8,11)
slope_raw_2 <- slope_raw[slope_raw$time %in% c("00","06") & slope_raw$year %in% c("2014","2015","2016","2017"),]
# slope_raw_2로 뽑아서 엑셀에서 date structure 다시 바꾸기.
# write.csv(slope_raw_2,"slope_raw_2.csv")
# slope_raw_3 <- read.csv("slope_raw_2.csv")
# remove(osan_raw)
slope_dups <- slope_raw_3[c("time", "date")]
slope_raw_nodup <- slope_raw_3[!duplicated(slope_dups),]
slope_raw_nodup2 <- slope_raw_nodup[,c("max_slope","time","date","year")]
slope <- slope_raw_nodup2 %>%
  group_by(date) %>%
  summarise(
    slope = max(max_slope)
  )


##### slope를 활용한 2SLS 1번식 #####
step1_final_with_slope <- merge(step1_final_na,slope, by="date",all.x = T)
step1_final_with_slope_na <- na.omit(step1_final_with_slope) #44개 탈락
fit_1_slope <- lm(AQI~rain_sum+temp_mean_total+slope*factor(IV_300_BI),data = step1_final_with_slope_na)
summary(fit_1_slope)

#Case 1, inversion=0, slope=0.001(0.005이하) ==== 0.071755*0.001                            = 7.1755e-05
#Case 2, inversion=1, slope=0.005            ==== 0.071755*0.005+3.613+70.094*0.005         = 3.963829
#Case 3, inversion=1, slope=0.014            ==== 0.071755*0.014+3.613+70.094*0.014         = 4.595321
#Case 4, inversion=1, slope=1                ==== 0.071755*1+3.613+70.094*1                 = 73.77875
#Case 5, inversion=1, slope=2                ==== 0.071755*2+3.613+70.094*2                 = 143.9445

#Case 1, inversion=0, slope=0.001(0.005이하) ==== -0.088798*0.001                           = 7.1755e-05
#Case 2, inversion=1, slope=0.005            ==== -0.088798*0.005-2.962636+482.365579*0.005 = -0.5512521
#Case 3, inversion=1, slope=0.014            ==== -0.088798*0.014-2.962636+482.365579*0.014 = 3.789239
#Case 4, inversion=1, slope=1                ==== -0.088798*0.02-2.962636+482.365579*0.02   = 6.6829
#Case 5, inversion=1, slope=2                ==== -0.088798*0.025-2.962636+482.365579*0.025 = 9.094284

#Case 1, inversion=0, slope=0.001(0.005이하) ==== -0.190191*0.001                           = -0.000190191
#Case 2, inversion=1, slope=0.005            ==== -0.190191*0.005-0.751867+307.346906*0.005 = 0.7839166
#Case 3, inversion=1, slope=0.014            ==== -0.190191*0.014-0.751867+307.346906*0.014 = 3.548327
#Case 4, inversion=1, slope=1                ==== -0.190191*0.02-0.751867+307.346906*0.02   = 5.391267
#Case 5, inversion=1, slope=2                ==== -0.190191*0.03-0.751867+307.346906*0.03   = 8.462834
#Case 5, inversion=1, slope=2                ==== -0.190191*0.045-0.751867+307.346906*0.045 = 13.07019

##### slope를 활용한 2SLS 2번식 - gam model #####
library(mgcv)
library(splines)
step1_final_with_slope_na$slope_int=step1_final_with_slope_na$slope*step1_final_with_slope_na$IV_300_BI
summary(step1_final_with_slope_na$AQI)
quantile(step1_final_with_slope_na$slope_int,0.99)
step1_final_with_slope_na$slope_int2=step1_final_with_slope_na$slope_int
step1_final_with_slope_na$slope_int2[which(step1_final_with_slope_na$slope_int>0.04684685 )]=0.04684685
fit_1_slope_gam <- gam(AQI~rain_sum+temp_mean_total+slope+factor(IV_300_BI)+(slope_int2)+s(yday)+factor(wday),data = step1_final_with_slope_na)
fit_1_slope_gam2 <- lm(log(AQI)~rain_sum+temp_mean_total+factor(IV_300_BI)+ns(yday,df=9)+factor(wday),data = step1_final_with_slope_na)
fit_1_slope_gam3 <- lm((AQI)~rain_sum+ns(temp_mean_total,df=2)+factor(IV_300_BI)+ns(yday,df=2)+factor(wday),data = step1_final_with_slope_na)
fit_1_slope_gam4 <- gam((AQI)~s(rain_sum)+s(temp_mean_total)+factor(IV_300_BI)+s(yday)+factor(wday),data = step1_final_with_slope_na)

summary(fit_1_slope_gam3)
summary(fit_1_slope_gam4)
plot(fit_1_slope_gam4)

##### yday, wday 추가 #####
library(lubridate)
step1_final_with_slope_na$yday <- yday(step1_final_with_slope_na$date)
step1_final_with_slope_na$wday <- wday(step1_final_with_slope_na$date)
# save.image("thermal_inversion_220415.RData")


##### 백령도 AQI 보정변수 만들기 #####
#2014년
air_2014_1Q_back <- read.csv("2014년1분기.csv")
air_2014_1Q_back <- air_2014_1Q_back[air_2014_1Q_back$측정소명=="백령도",]
air_2014_2Q_back <- read.csv("2014년2분기.csv")
air_2014_2Q_back <- air_2014_2Q_back[air_2014_2Q_back$측정소명=="백령도",]
air_2014_3Q_back <- read.csv("2014년3분기.csv")
air_2014_3Q_back <- air_2014_3Q_back[air_2014_3Q_back$측정소명=="백령도",]
air_2014_4Q_back <- read.csv("2014년4분기.csv")
air_2014_4Q_back <- air_2014_4Q_back[air_2014_4Q_back$측정소명=="백령도",]
air_2014_back <- rbind(air_2014_1Q_back,air_2014_2Q_back,air_2014_3Q_back,air_2014_4Q_back)
#2015
air_2015_1Q_back <- read.csv("2015년1분기.csv", fileEncoding="UCS-2LE")
air_2015_1Q_back <- air_2015_1Q_back[air_2015_1Q_back$측정소명=="백령도",]
air_2015_2Q_back <- read.csv("2015년2분기.csv", fileEncoding="UCS-2LE")
air_2015_2Q_back <- air_2015_2Q_back[air_2015_2Q_back$측정소명=="백령도",]
air_2015_3Q_back <- read.csv("2015년3분기.csv", fileEncoding="UCS-2LE")
air_2015_3Q_back <- air_2015_3Q_back[air_2015_3Q_back$측정소명=="백령도",]
air_2015_4Q_back <- read.csv("2015년4분기.csv", fileEncoding="UCS-2LE")
air_2015_4Q_back <- air_2015_4Q_back[air_2015_4Q_back$측정소명=="백령도",]
air_2015_back <- rbind(air_2015_1Q_back,air_2015_2Q_back,air_2015_3Q_back,air_2015_4Q_back)
#2016
air_2016_1Q_back <- read.csv("2016년 1분기.csv")
air_2016_1Q_back <- air_2016_1Q_back[air_2016_1Q_back$측정소명=="백령도",]
air_2016_2Q_back <- read.csv("2016년 2분기.csv")
air_2016_2Q_back <- air_2016_2Q_back[air_2016_2Q_back$측정소명=="백령도",]
air_2016_3Q_back <- read.csv("2016년 3분기.csv")
air_2016_3Q_back <- air_2016_3Q_back[air_2016_3Q_back$측정소명=="백령도",]
air_2016_4Q_back <- read.csv("2016년 4분기.csv")
air_2016_4Q_back <- air_2016_4Q_back[air_2016_4Q_back$측정소명=="백령도",]
air_2016_back <- rbind(air_2016_1Q_back,air_2016_2Q_back,air_2016_3Q_back,air_2016_4Q_back)
#2017
# air_2017_1m_back <- read_excel("2017년 1월.xlsx")
# air_2017_1m_back <- air_2017_1m_back[air_2017_1m_back$측정소명=="백령도",]
# air_2017_2m_back <- read_excel("2017년 2월.xlsx")
# air_2017_2m_back <- air_2017_2m_back[air_2017_2m_back$측정소명=="백령도",]
# air_2017_3m_back <- read_excel("2017년 3월.xlsx")
# air_2017_3m_back <- air_2017_3m_back[air_2017_3m_back$측정소명=="백령도",]
# air_2017_4m_back <- read_excel("2017년 4월.xlsx")
# air_2017_4m_back <- air_2017_4m_back[air_2017_4m_back$측정소명=="백령도",]
# air_2017_5m_back <- read_excel("2017년 5월.xlsx")
# air_2017_5m_back <- air_2017_5m_back[air_2017_5m_back$측정소명=="백령도",]
# air_2017_6m_back <- read_excel("2017년 6월.xlsx")
# air_2017_6m_back <- air_2017_6m_back[air_2017_6m_back$측정소명=="백령도",]
# air_2017_7m_back <- read_excel("2017년 7월.xlsx")
# air_2017_7m_back <- air_2017_7m_back[air_2017_7m_back$측정소명=="백령도",]
# air_2017_8m_back <- read_excel("2017년 8월.xlsx")
# air_2017_8m_back <- air_2017_8m_back[air_2017_8m_back$측정소명=="백령도",]
# air_2017_9m_back <- read_excel("2017년 9월.xlsx")
# air_2017_9m_back <- air_2017_9m_back[air_2017_9m_back$측정소명=="백령도",]
# air_2017_10m_back <- read_excel("2017년 10월.xlsx")
# air_2017_10m_back <- air_2017_10m_back[air_2017_10m_back$측정소명=="백령도",]
# air_2017_11m_back <- read_excel("2017년 11월.xlsx")
# air_2017_11m_back <- air_2017_11m_back[air_2017_11m_back$측정소명=="백령도",]
# air_2017_12m_back <- read_excel("2017년 12월.xlsx")
# air_2017_12m_back <- air_2017_12m_back[air_2017_12m_back$측정소명=="백령도",]
air_2017_back <- rbind(air_2017_1m_back,air_2017_2m_back,air_2017_3m_back,air_2017_4m_back,air_2017_5m_back,air_2017_6m_back
                       ,air_2017_7m_back,air_2017_8m_back,air_2017_9m_back,air_2017_10m_back,air_2017_11m_back,air_2017_12m_back)
air_2017_back$망 <- NULL
air_1417_back <- rbind(air_2014_back,air_2015_back,air_2016_back,air_2017_back)

# SO2, CO, NO2, O3는 00시-14시 까지의 평균값으로 가져오기. ----------> 결측치가 너무 많음! 그냥 다 일평균으로 계산해보자. 밑에서! 
# air_1417_back_non_pm <- air_1417_back[substr(air_1417_back$측정일시,9,10) 
#                                       %in% c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14"),
#                                       c("측정일시","SO2","CO","O3","NO2")]
# air_1417_back_non_pm$date = paste0(substr(air_1417_back_non_pm$측정일시,1,4),"-",substr(air_1417_back_non_pm$측정일시,5,6),"-",substr(air_1417_back_non_pm$측정일시,7,8))
# colnames(air_1417_back_non_pm) <- c("측정일시","SO2_back","CO_back","O3_back","NO2_back","date")
# air_1417_back_non_pm2 <- air_1417_back_non_pm %>%
#   group_by(date) %>%
#   summarise(
#     SO2_mean_back = mean(SO2_back,na.rm=T),
#     CO_mean_back = mean(CO_back,na.rm=T),
#     O3_mean_back = mean(O3_back,na.rm=T),
#     NO2_mean_back = mean(NO2_back,na.rm=T),
#   )

# #2018년 추가 - zio
air_2018_1Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 1분기.xlsx")
air_2018_2Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 2분기.xlsx")
air_2018_3Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 3분기.xlsx")
air_2018_4Q <- read_excel("D:\\SNUlab\\thermal_inversion_0623_share\\2018_air\\2018년 4분기.xlsx")
# "망" 삭제
air_2018_3Q <- dplyr::select(air_2018_3Q, -c("망"))
air_2018_4Q <- dplyr::select(air_2018_4Q, -c("망"))

# 백령도만 추출
air_2018_1Q_back <- air_2018_1Q[air_2018_1Q$측정소명=="백령도",]
air_2018_2Q_back <- air_2018_2Q[air_2018_2Q$측정소명=="백령도",]
air_2018_3Q_back <- air_2018_3Q[air_2018_3Q$측정소명=="백령도",]
air_2018_4Q_back <- air_2018_4Q[air_2018_4Q$측정소명=="백령도",]

air_2018_back <- rbind(air_2018_1Q_back,air_2018_2Q_back,air_2018_3Q_back,air_2018_4Q_back)

# date column 생성
air_2018_back$date <- paste0(substr(air_2018_back$측정일시,0,4),"-",substr(air_2018_back$측정일시,5,6),"-",substr(air_2018_back$측정일시,7,8))
summary(air_2018_back$PM10)

# 09시 추출 전, pm10_day_mean 생성
pm10_mean_data_back <- air_2018_back %>% 
  group_by(date, 측정소코드) %>% 
  summarise(pm10_day_mean = mean(PM10, na.rm = TRUE))

# 09시 추출 전, pm25_day_mean 생성
pm25_mean_data_back <- air_2018_back %>%
  group_by(date, 측정소코드) %>%
  summarise(pm25_day_mean = mean(PM25, na.rm = TRUE))

# 나머지는 09시만 추출
air_2018_1Q_back <- air_2018_1Q_back[substr(air_2018_1Q_back$측정일시,9,10)=="09",]
air_2018_2Q_back <- air_2018_2Q_back[substr(air_2018_2Q_back$측정일시,9,10)=="09",]
air_2018_3Q_back <- air_2018_3Q_back[substr(air_2018_3Q_back$측정일시,9,10)=="09",]
air_2018_4Q_back <- air_2018_4Q_back[substr(air_2018_4Q_back$측정일시,9,10)=="09",]

air_2018_back <- rbind(air_2018_1Q_back,air_2018_2Q_back,air_2018_3Q_back,air_2018_4Q_back)
air_2018_back$date <- paste0(substr(air_2018_back$측정일시,0,4),"-",substr(air_2018_back$측정일시,5,6),"-",substr(air_2018_back$측정일시,7,8))
# - air_2018_back 365 check
# pm10, pm25 day mean 데이터 만든 거 붙이기
air_18_back_final <- merge(air_2018_back, pm10_mean_data_back, by=c("측정소코드", "date"), all.x=T)
air_18_back_final <- merge(air_18_back_final, pm25_mean_data_back, by=c("측정소코드", "date"), all.x=T)


##### AQI back 계산 ##### - 2018 - zio
library(con2aqi)
library(psych)
#na 값 제거
air_not_na_18_back <- na.omit(air_18_back_final) # 365 - 321 = 44일 제거
summary(air_not_na_18_back$date)
length(unique(air_not_na_18_back$date)) # 321일

air_not_na_18_back$pm10_day_mean[air_not_na_18_back$pm10_day_mean>600] <- 600 #600이상이면 오류 나기 때문에 600이상은 600으로 대체.
air_not_na_18_back$so2_back_AQI = con2aqi("so2",air_not_na_18_back$SO2*1000) 
air_not_na_18_back$co_back_AQI = con2aqi("co",air_not_na_18_back$CO*10)
air_not_na_18_back$o3_back_AQI = con2aqi("o3",air_not_na_18_back$O3,"1h") 

air_not_na_18_back$no2_back_AQI = con2aqi("no2",air_not_na_18_back$NO2*1000)
air_not_na_18_back$pm10_back_AQI = con2aqi("pm10",air_not_na_18_back$pm10_day_mean)
air_not_na_18_back$pm25_back_AQI = con2aqi("pm25",air_not_na_18_back$pm25_day_mean) 
air_not_na_18_back$AQI_back = (air_not_na_18_back$so2_back_AQI+air_not_na_18_back$co_back_AQI+air_not_na_18_back$o3_back_AQI+air_not_na_18_back$no2_back_AQI+air_not_na_18_back$pm10_back_AQI+air_not_na_18_back$pm25_back_AQI)/6
# save.image("220414_data_preprocessing.RData")
summary(air_not_na_18_back$AQI_back)

# AQI_back 기존 데이터에 붙이기 - zio
step1_final_na_back <- merge(step1_final_na_2018,air_not_na_18_back[, c("date", "so2_back_AQI", "co_back_AQI", "o3_back_AQI", "no2_back_AQI", "pm10_back_AQI", "pm25_back_AQI", "AQI_back")],by="date",all.x = T)
summary(step1_final_na_back$AQI_back)
write.csv(step1_final_na_back, "D:\\SNUlab\\thermal_inversion_0623_share\\step1_final_na_back_2018.csv")



# pm10은 일평균으로 계산하기. - 성표선생님 코드
air_1417_back_pm <- air_1417_back[,c("측정일시","PM10")]
air_1417_back_pm$date = paste0(substr(air_1417_back_pm$측정일시,1,4),"-",substr(air_1417_back_pm$측정일시,5,6),"-",substr(air_1417_back_pm$측정일시,7,8))
air_1417_back_pm2 <- air_1417_back_pm %>%
  group_by(date) %>%
  summarise(
    PM10_back = mean(PM10)
  )


air_1417_back_aqi <- merge(air_1417_back_non_pm2,air_1417_back_pm2,by="date",all.x = T)
air_1417_back_aqi_na <- na.omit(air_1417_back_aqi)
air_1417_back_aqi_na$so2_back_AQI = con2aqi("so2",air_1417_back_aqi_na$SO2_mean_back*1000)
air_1417_back_aqi_na$co_back_AQI = con2aqi("co",air_1417_back_aqi_na$CO_mean_back*10)
air_1417_back_aqi_na$o3_back_AQI = con2aqi("o3",air_1417_back_aqi_na$O3_mean_back,"1h")
air_1417_back_aqi_na$no2_back_AQI = con2aqi("no2",air_1417_back_aqi_na$NO2_mean_back*1000)
air_1417_back_aqi_na$pm10_back_AQI = con2aqi("pm10",air_1417_back_aqi_na$PM10_back)
air_1417_back_aqi_na$AQI_back = (air_1417_back_aqi_na$so2_back_AQI+
                                   air_1417_back_aqi_na$co_back_AQI+
                                   air_1417_back_aqi_na$o3_back_AQI+
                                   air_1417_back_aqi_na$no2_back_AQI+
                                   air_1417_back_aqi_na$pm10_back_AQI)/5

air_1417_back2 <- air_1417_back[,c("측정일시","SO2","CO","O3","NO2","PM10")]
air_1417_back2$date = paste0(substr(air_1417_back2$측정일시,1,4),"-",substr(air_1417_back2$측정일시,5,6),"-",substr(air_1417_back2$측정일시,7,8))
air_1417_back3 <- air_1417_back2 %>%
  group_by(date) %>%
  summarise(
    SO2_back = mean(SO2,na.rm=T),
    CO_back = mean(CO,na.rm=T),
    O3_back = mean(O3,na.rm=T),
    NO2_back = mean(NO2,na.rm=T),
    PM10_back = mean(PM10,na.rm=T)
  )

air_1417_back3_aqi_na <- na.omit(air_1417_back3)
air_1417_back3_aqi_na$so2_back_AQI = con2aqi("so2",air_1417_back3_aqi_na$SO2_back*1000)
air_1417_back3_aqi_na$co_back_AQI = con2aqi("co",air_1417_back3_aqi_na$CO_back*10)
air_1417_back3_aqi_na$o3_back_AQI = con2aqi("o3",air_1417_back3_aqi_na$O3_back,"1h")
air_1417_back3_aqi_na$no2_back_AQI = con2aqi("no2",air_1417_back3_aqi_na$NO2_back*1000)
air_1417_back3_aqi_na$pm10_back_AQI = con2aqi("pm10",air_1417_back3_aqi_na$PM10_back)
air_1417_back3_aqi_na$AQI_back = (air_1417_back3_aqi_na$so2_back_AQI+
                                    air_1417_back3_aqi_na$co_back_AQI+
                                    air_1417_back3_aqi_na$o3_back_AQI+
                                    air_1417_back3_aqi_na$no2_back_AQI+
                                    air_1417_back3_aqi_na$pm10_back_AQI)/5
summary(air_1417_back3_aqi_na)
# 기존 데이터에 붙이기.
step1_final_with_slope_na_back <- merge(step1_final_with_slope_na,air_1417_back3_aqi_na,by="date",all.x = T)
step1_final_with_slope_na_back_na <- na.omit(step1_final_with_slope_na_back)

##### 백령도 aqi 보정변수로 넣고 1번 모델 돌려보기 #####
fit_back <- lm(AQI~factor(IV_300_BI)+AQI_back,data = step1_final_with_slope_na_back_na)
step1_final_with_slope_na_back_na$aqi_hat <- fit_back$fitted.values
summary(fit_back)

##### 백령도 aqi 보정변수로 넣고 2번 모델 돌려보기 #####
step2_final_back <- merge(step1_final_with_slope_na_back_na,outcome, by=c("date","air_out_idx"),all.x = T)
fit_2_300 <- glm(ASTHMA_em_total~aqi_hat+ns(temp_mean_total,df=2)+ns(yday,df=2)+factor(wday),data = step2_final_back, family = "quasipoisson")
summary(fit_2_300)
# save.image("thermal_inversion_220415.RData")

##### 백령도 시정 데이터 추가해보기 #####
#2014
visibility_2014_raw <- read.csv("백령도_인천_시정_2014.csv")
visibility_2014_raw$date <- substr(visibility_2014_raw$일시,1,10)
back_visibility_2014 <- visibility_2014_raw[substr(visibility_2014_raw$일시,12,13)=="09"&visibility_2014_raw$지점==102,c("date","시정.10m.")]
incheon_visibility_2014 <- visibility_2014_raw[substr(visibility_2014_raw$일시,12,13)=="09"&visibility_2014_raw$지점==112,c("date","시정.10m.")]
colnames(back_visibility_2014)[2] <- "visibility_back"
colnames(incheon_visibility_2014)[2] <- "visibility_incheon"
#2015
visibility_2015_raw <- read.csv("백령도_인천_시정_2015.csv")
visibility_2015_raw$date <- substr(visibility_2015_raw$일시,1,10)
back_visibility_2015 <- visibility_2015_raw[substr(visibility_2015_raw$일시,12,13)=="09"&visibility_2015_raw$지점==102,c("date","시정.10m.")]
incheon_visibility_2015 <- visibility_2015_raw[substr(visibility_2015_raw$일시,12,13)=="09"&visibility_2015_raw$지점==112,c("date","시정.10m.")]
colnames(back_visibility_2015)[2] <- "visibility_back"
colnames(incheon_visibility_2015)[2] <- "visibility_incheon"
#2016
visibility_2016_raw <- read.csv("백령도_인천_시정_2016.csv")
visibility_2016_raw$date <- substr(visibility_2016_raw$일시,1,10)
back_visibility_2016 <- visibility_2016_raw[substr(visibility_2016_raw$일시,12,13)=="09"&visibility_2016_raw$지점==102,c("date","시정.10m.")]
incheon_visibility_2016 <- visibility_2016_raw[substr(visibility_2016_raw$일시,12,13)=="09"&visibility_2016_raw$지점==112,c("date","시정.10m.")]
colnames(back_visibility_2016)[2] <- "visibility_back"
colnames(incheon_visibility_2016)[2] <- "visibility_incheon"
#2017
visibility_2017_raw <- read.csv("백령도_인천_시정_2017.csv")
visibility_2017_raw$date <- substr(visibility_2017_raw$일시,1,10)
back_visibility_2017 <- visibility_2017_raw[substr(visibility_2017_raw$일시,12,13)=="09"&visibility_2017_raw$지점==102,c("date","시정.10m.")]
incheon_visibility_2017 <- visibility_2017_raw[substr(visibility_2017_raw$일시,12,13)=="09"&visibility_2017_raw$지점==112,c("date","시정.10m.")]
colnames(back_visibility_2017)[2] <- "visibility_back"
colnames(incheon_visibility_2017)[2] <- "visibility_incheon"
#백령도 전체
visibility_back <- rbind(back_visibility_2014,back_visibility_2015,back_visibility_2016,back_visibility_2017)
visibility_incheon <- rbind(incheon_visibility_2014,incheon_visibility_2015,incheon_visibility_2016,incheon_visibility_2017)
#인천 전체
step2_final_back <- merge(step2_final_back,visibility_back, by="date",all.x = T)
step2_final_back <- merge(step2_final_back,visibility_incheon, by="date",all.x = T)
# save.image("thermal_inversion_220419.RData")

##### 시정으로 1번 모델 돌려보기 #####
library(car)
fit_back_visibility <- lm(AQI~visibility_incheon+visibility_back+factor(IV_300_BI)+AQI_back,data = step2_final_back)
summary(fit_back_visibility)



# save.image("220421_to_yj.RData")
