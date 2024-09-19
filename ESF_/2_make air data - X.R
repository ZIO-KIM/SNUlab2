

setwd('D:\\SNUlab\\data\\out of hospital sudden cardiac arrest\\')
#### step 6. air_korea 2015 수도권 전처리 ################################################
# map idx 불러오기(수도권에선 일산서구 빼자.)
map_idx <- read.csv("qgis_지도_idx.csv")
map_idx <- map_idx[,c(1:4)]
colnames(map_idx) = c("SIGUNGU_CD","SIGUNGU_NM","air_out_idx","sudogwon_check")
map_sudogwon_idx = map_idx[map_idx$sudogwon_check==1,]
map_sudogwon_idx <- map_sudogwon_idx[map_sudogwon_idx$air_out_idx!=21,]#일산서구 빼주기.

#2020년도 unique 기준으로 
air_2020 <- read.csv("air_year_2020.csv",header = T)
# write.csv(unique(air2020$주소),"air_2020_idx.csv")
air_2020_unique <- read.csv("air_2020_idx.csv")
colnames(air_2020_unique) <- c("X","주소","air_out_idx")

air_2020_final <- merge(air_2020,air_2020_unique,by="주소")
air_2020_idx <- air_2020_final[,c("주소","측정소코드","air_out_idx")]
air_2020_idx <- air_2020_idx[-which(duplicated(air_2020_idx$주소)),]

# 2022.01.04 zio
write.csv(air_2020_idx, file='air_2020_idx_zio.csv', row.names = FALSE)

air_2020_sugogwon_idx <- merge(air_2020_idx, map_sudogwon_idx, by="air_out_idx")
air_2020_sugogwon_idx2 <- air_2020_sugogwon_idx[-which(duplicated(air_2020_sugogwon_idx$측정소코드)),]
length(unique(air_2020_idx$측정소코드))


#2012.12~2017
library(readxl)
library(dplyr)
air_2012 <- read_excel("air_year_2012_yj.xlsx")
air_2012_1112 <- air_2012 %>% filter (substr(dt,1,7) %in% c("2012-12","2012-11"))

air_2013 <- read_excel("air_year_2013_yj.xlsx")
air_2014 <- read_excel("air_year_2014.xlsx")
air_2015 <- read_excel("air_year_2015.xlsx")
air_2016 <- read_excel("air_year_2016.xlsx")
air_2017 <- read_excel("air_year_2017.xlsx")

# air_2012_12_2 <- air_2012_12[c(1:41)]


#lag때문에 2012도 붙여놓기
air_1317 <- rbind(air_2012_1112, air_2013,air_2014,air_2015,air_2016,air_2017)
air_1317_2 <- merge(air_1317,air_2020_sugogwon_idx2,by="측정소코드")
length(unique(air_1317_2$측정소코드))
length(unique(air_2020_sugogwon_idx2$air_out_idx))

air_1317_3 <- air_1317_2[,c("주소.x","측정소코드","air_out_idx")]
length(unique(air_1317_3$air_out_idx))

colnames(air_1317_3) <- c("주소","측정소코드","air_out_idx")
air_1317_4 <- air_1317_3[-which(duplicated(air_1317_3$air_out_idx)),]
#dim(air_2015_4)
air_1317_4 <- cbind(air_1317_4,c(1:72))
colnames(air_1317_4)[4] <- "rownum"
#260,261,284번째
# 99587  충남 당진시 석문면 난지도리 533         534431 NA
# 99952  충남 당진시 송악면 정곡리 66-12번지     534432 NA
# 109077 전북 익산시 인북로 32길 1 (익산시의회)  735132 NA
#air_1317_4[260,]$air_out_idx <- 64
#air_1317_4[261,]$air_out_idx <- 64
#air_1317_4[284,]$air_out_idx <- 184
air_1317_5 <- merge(air_1317,air_1317_4,by="측정소코드",all.x=T)
air_1317_sudogwon <- merge(air_1317_5, map_sudogwon_idx,by="air_out_idx")
# length(unique(air_1317_sudogwon$air_out_idx))#72개 확인.
remove(air_1317)
remove(air_1317_2)
remove(air_1317_3)
remove(air_1317_5)
# remove(air_1317_final)
nrow(air_1317_sudogwon)/72
summary(air_1317_sudogwon)

air_1317_sudogwon_1<- air_1317_sudogwon %>% arrange(air_out_idx,dt)
table(is.na(air_1317_sudogwon_1$PM10_mean))
table(is.na(air_1317_sudogwon_1$NO2_mean))
table(is.na(air_1317_sudogwon_1$O3_mean))

library(pracma)
library(dplyr)

# O3 분당 없어서 (#114) #115로 대체
air_1317_sudogwon_2=air_1317_sudogwon_1
air_coo3 <- air_1317_sudogwon_2 %>% select("dt","air_out_idx","O3_mean", "CO_mean")
air_coo3_1 <- air_coo3 %>% filter(air_out_idx!=114)
length(unique(air_1317_sudogwon_2$air_out_idx))
A= as.data.frame(air_coo3 %>% 
                   filter(air_out_idx=="115") %>% 
                   select("dt","O3_mean") %>% 
                   mutate(air_out_idx=114))

air_coo3_2 <- left_join(air_coo3, A, by=c("dt","air_out_idx"),all=T)
air_coo3_2$O3_mean.x <- ifelse(is.na(air_coo3_2$O3_mean.x),air_coo3_2$O3_mean.y,air_coo3_2$O3_mean.x)
air_coo3_3 <- air_coo3_2 %>%
  rename("O3_mean"="O3_mean.x") %>%
  select("dt","air_out_idx","O3_mean","CO_mean")

air_1317_sudogwon_3 <- merge(air_1317_sudogwon_2,air_coo3_3, by=c("dt","air_out_idx")) 
air_1317_sudogwon_4 <- air_1317_sudogwon_3 %>%
  rename(CO_mean=CO_mean.y,
         O3_mean=O3_mean.y)
######################################################################################################

air_1317_sudogwon_5 <- air_1317_sudogwon_4
air_1317_sudogwon_5_1=air_1317_sudogwon_5[complete.cases(air_1317_sudogwon_5),]
length(unique(air_1317_sudogwon_5$air_out_idx))
sort(unique(air_1317_sudogwon_5$SIGUNGU_NM))
a=as.data.frame(unique(air_1317_sudogwon_5_1$air_out_idx))

a=as.data.frame(is.na(air_1317_sudogwon_5$O3_mean))
sum(is.na(air_1317_sudogwon_5$O3_mean))
sum(is.na(air_1317_sudogwon_5$CO_mean))
sum(is.na(air_1317_sudogwon_5$PM10_mean))
sum(is.na(air_1317_sudogwon_5$NO2_mean))
sum(is.na(air_1317_sudogwon_5$SO2_mean))

test=air_1317_sudogwon_5[is.na(air_1317_sudogwon_5$O3_mean),]
table(test$air_out_idx)
air_1317_sudogwon_5_1=air_1317_sudogwon_1[complete.cases(air_1317_sudogwon_1),]
length(unique(air_1317_sudogwon_5_1$air_out_idx))
a=as.data.frame(unique(air_1317_sudogwon_5_1$air_out_idx))



# additional test for correlation matrix of SO2
library("Hmisc")
View(SO2_1317_sudogwon)
colnames(air_1317_sudogwon_3)
dim(air_1317_sudogwon_3)
SO2_1317_sudogwon=air_1317_sudogwon_3[,c(8,48:87)]
head(SO2_1317_sudogwon)
SO2_1317_sudogwon2=SO2_1317_sudogwon[complete.cases(SO2_1317_sudogwon),]
res2 <- cor(SO2_1317_sudogwon2$SO2_mean,SO2_1317_sudogwon2$SO2_lag1)
write.csv(res2,file="SO2_corr matrix.csv")

res2 <- cor(as.matrix(SO2_1317_sudogwon2))
res2

