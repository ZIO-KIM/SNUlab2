
## 0. set directory ##
setwd("D://SNUlab//IV_OHCA")
load(file="# sudden_death_1208.RData")
######################


### air - N ###
dim(air_0919_new)
air_0919_no_na <- na.omit(air_0919_new)
dim(air_0919_no_na)
dim(air_0919_whole)

100425-97217
100425-92859

colSums(is.na(air_0919_whole))

################################################


### weather - N ###

dim(weather_0919_new_humi_replaced)
dim(weather_0919_whole_humi_replaced)

colSums(is.na(weather_0919_whole_humi_replaced))

100425-98579

so2_zero <- results[results$SO2 == 0,]
dim(so2_zero)
sort(unique(results$SO2))
head(so2_zero$SO2)
dim(table(so2_zero$date))

CO_zero <- results[results$CO == 0,]
dim(CO_zero)
sort(unique(results$CO))
head(CO_zero$CO)
dim(table(CO_zero$date))

NO2_zero <- results[results$NO2 == 0,]
dim(NO2_zero)
sort(unique(results$NO2))
head(NO2_zero$NO2)
dim(table(NO2_zero$date))

O3_zero <- results[results$O3 == 0,]
dim(O3_zero)
sort(unique(results$O3))
head(O3_zero$O3)
dim(table(O3_zero$date))

t=as.data.frame(table(weather_0919_new[is.na(weather_0919_new$humi_mean_total) == TRUE, c('date', 'air_out_idx')]))
t$year=substr(t$date,1,4)
length(unique(t$air_out_idx))
a <- aggregate(t$Freq, by=list(t$year, t$air_out_idx), FUN=sum)
library(reshape2)
reshape(a, idvar='Group.1', timevar='Group.2', v.names='x', direction='wide')

###############################################


### air, weather - summary ###

humi_miss <- results[results$humi_mean_total<0,]
dim(humi_miss)

humi_zero <- results[results$humi_mean_total == 0,]
dim(humi_zero)
sort(unique(results$humi_mean_total))
head(humi_zero$humi_mean_total)
dim(table(humi_zero$date))
humi_under10 <- results[results$humi_mean_total < 10,]
dim(humi_under10)

humi_raw_positive <- weather_0919_new[weather_0919_new$humi_mean_total >0,]
dim(humi_raw_positive)

humi_raw_negative <- weather_0919_new[!(is.na(weather_0919_new$humi_mean_total)) & weather_0919_new$humi_mean_total < 0,]
dim(humi_raw_negative)
head(humi_raw_negative)

humi_raw_zero <- weather_0919_new[weather_0919_new$humi_mean_total == 0,]
dim(humi_raw_zero)
head(humi_raw_zero)

# 
colnames(results)
summary(results$pressure_mean)
sd(results$pressure_mean)

t=as.data.frame(table(weather_0919_whole[is.na(weather_0919_whole$temp_mean_total) == TRUE,'date']))
t$year=substr(t$Var1,1,4)
sum(table(t$year))

t2=as.data.frame(table(weather_0919_whole[is.na(weather_0919_whole$humi_mean_total) == TRUE,'date']))
t3=as.data.frame(table(weather_0919_whole[(weather_0919_whole$humi_mean_total) == 0,'date']))

t2$year=substr(t2$Var1,1,4)
(table(t2$year))
sum(t2$Freq)

###############################################


### pressure ###

pressure_snow_sunshine_0919 <- rbind(pressure_snow_sunshine_2009_agg,pressure_snow_sunshine_2010_agg,pressure_snow_sunshine_2011_agg,pressure_snow_sunshine_2012_agg,pressure_snow_sunshine_2013_agg,pressure_snow_sunshine_2014_agg,pressure_snow_sunshine_2015_agg, pressure_snow_sunshine_2016_agg, pressure_snow_sunshine_2017_agg, pressure_snow_sunshine_2018_agg,pressure_snow_sunshine_2019_agg)

pressure_snow_sunshine_0919_whole <- left_join(index, pressure_snow_sunshine_0919, by=c('date'))
dim(pressure_snow_sunshine_0919_whole)
colSums(is.na(pressure_snow_sunshine_0919_whole))

365.25*11

###############################################


