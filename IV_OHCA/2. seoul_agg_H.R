
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
library(sets)
library(data.table)

setwd("D://SNUlab")
load(file="IV_OHCA/# sudden_death_230112.RData")

ohca_0919 <- read_sas("IV_OHCA/sub/ohca_0919_3_seoul_agg_nodup2.sas7bdat")
# ppl data
# ohca_0919_3_seoul <- read_sas("IV_OHCA/sub/ohca_0919_3_seoul.sas7bdat")
# tmp<-left_join(ohca_0919_3_seoul,final_data_lag,by=c("date","air_out_idx"))
# dim(tmp)
# head(final_data_lag$AQI_lag1)
# colnames(final_data_lag)
# length(unique(final_data_lag$air_out_idx))
# dim(final_data_lag)
# write.csv(tmp, "IV_OHCA/sub/ohca_individual_data.csv")

colnames(final_data_lag)
final_data_lag_re<-left_join(final_data_lag,ohca_0919,by=c("date","air_out_idx"))
length(unique(final_data_lag_re$air_out_idx))
colnames(ohca_0919)
colnames(final_data_lag_re)
colSums(is.na(final_data_lag_re))

str(final_data_lag_re)
sapply(final_data_lag_re, class)

# final_data_lag_re2 <- final_data_lag_re %>%
#   mutate(cause_disease_agg = ifelse( !is.na(cause_disease_agg), cause_disease_agg, 0 ),
#          disease_cardiogenic_agg = ifelse( !is.na(disease_cardiogenic_agg), disease_cardiogenic_agg, 0 ),
#          disease_atrau_agg = ifelse( !is.na(disease_atrau_agg), disease_atrau_agg, 0 ),
         
#          cardiogenic_male_agg = ifelse( !is.na(cardiogenic_male_agg), cardiogenic_male_agg, 0 ),
#          cardiogenic_female_agg = ifelse( !is.na(cardiogenic_female_agg), cardiogenic_female_agg, 0 ),
         
#          cardiogenic_under70_agg = ifelse( !is.na(cardiogenic_under70_agg), cardiogenic_under70_agg, 0 ),
#          cardiogenic_above70_agg = ifelse( !is.na(cardiogenic_above70_agg), cardiogenic_above70_agg, 0 ),
#          cardiogenic_under65_agg = ifelse( !is.na(cardiogenic_under65_agg), cardiogenic_under65_agg, 0 ),
#          cardiogenic_above65_agg = ifelse( !is.na(cardiogenic_above65_agg), cardiogenic_above65_agg, 0 ),
#          cardiogenic_under60_agg = ifelse( !is.na(cardiogenic_under60_agg), cardiogenic_under60_agg, 0 ),
#          cardiogenic_above60_agg = ifelse( !is.na(cardiogenic_above60_agg), cardiogenic_above60_agg, 0 ),
#          cardiogenic_under55_agg = ifelse( !is.na(cardiogenic_under55_agg), cardiogenic_under55_agg, 0 ),
#          cardiogenic_above55_agg = ifelse( !is.na(cardiogenic_above55_agg), cardiogenic_above55_agg, 0 ),
#          cardiogenic_under50_agg = ifelse( !is.na(cardiogenic_under50_agg), cardiogenic_under50_agg, 0 ),
#          cardiogenic_above50_agg = ifelse( !is.na(cardiogenic_above50_agg), cardiogenic_above50_agg, 0 ),
#          cardiogenic_under45_agg = ifelse( !is.na(cardiogenic_under45_agg), cardiogenic_under45_agg, 0 ),
#          cardiogenic_above45_agg = ifelse( !is.na(cardiogenic_above45_agg), cardiogenic_above45_agg, 0 ),
#          cardiogenic_under40_agg = ifelse( !is.na(cardiogenic_under40_agg), cardiogenic_under40_agg, 0 ),
#          cardiogenic_above40_agg = ifelse( !is.na(cardiogenic_above40_agg), cardiogenic_above40_agg, 0 ),
#          cardiogenic_under35_agg = ifelse( !is.na(cardiogenic_under35_agg), cardiogenic_under35_agg, 0 ),
#          cardiogenic_above35_agg = ifelse( !is.na(cardiogenic_above35_agg), cardiogenic_above35_agg, 0 ),
#          cardiogenic_under30_agg = ifelse( !is.na(cardiogenic_under30_agg), cardiogenic_under30_agg, 0 ),
#          cardiogenic_above30_agg = ifelse( !is.na(cardiogenic_above30_agg), cardiogenic_above30_agg, 0 ),
#          cardiogenic_under25_agg = ifelse( !is.na(cardiogenic_under25_agg), cardiogenic_under25_agg, 0 ),
#          cardiogenic_above25_agg = ifelse( !is.na(cardiogenic_above25_agg), cardiogenic_above25_agg, 0 ),
#          cardiogenic_under20_agg = ifelse( !is.na(cardiogenic_under20_agg), cardiogenic_under20_agg, 0 ),
#          cardiogenic_above20_agg = ifelse( !is.na(cardiogenic_above20_agg), cardiogenic_above20_agg, 0 ),
#          cardiogenic_under15_agg = ifelse( !is.na(cardiogenic_under15_agg), cardiogenic_under15_agg, 0 ),
#          cardiogenic_above15_agg = ifelse( !is.na(cardiogenic_above15_agg), cardiogenic_above15_agg, 0 ),
#          cardiogenic_under10_agg = ifelse( !is.na(cardiogenic_under10_agg), cardiogenic_under10_agg, 0 ),
#          cardiogenic_above10_agg = ifelse( !is.na(cardiogenic_above10_agg), cardiogenic_above10_agg, 0 ),
#          cardiogenic_under5_agg = ifelse( !is.na(cardiogenic_under5_agg), cardiogenic_under5_agg, 0 ),
#          cardiogenic_above5_agg = ifelse( !is.na(cardiogenic_above5_agg), cardiogenic_above5_agg, 0 ),
#          cardiogenic_above20_under40_agg = ifelse( !is.na(cardiogenic_above20_under40_agg), cardiogenic_above20_under40_agg, 0 ),
#          cardiogenic_above40_under60_agg = ifelse( !is.na(cardiogenic_above40_under60_agg), cardiogenic_above40_under60_agg, 0 ),
#          cardiogenic_above60_under75_agg = ifelse( !is.na(cardiogenic_above60_under75_agg), cardiogenic_above60_under75_agg, 0 ),
#          cardiogenic_above75_agg = ifelse( !is.na(cardiogenic_above75_agg), cardiogenic_above75_agg, 0 ),
         
#          cardiogenic_HTN_agg = ifelse( !is.na(cardiogenic_HTN_agg), cardiogenic_HTN_agg, 0 ),
#          cardiogenic_noHTN_agg = ifelse( !is.na(cardiogenic_noHTN_agg), cardiogenic_noHTN_agg, 0 ),
         
#          cardiogenic_DM_agg = ifelse( !is.na(cardiogenic_DM_agg), cardiogenic_DM_agg, 0 ),
#          cardiogenic_noDM_agg = ifelse( !is.na(cardiogenic_noDM_agg), cardiogenic_noDM_agg, 0 ),
         
#          cardiogenic_HEART_agg = ifelse( !is.na(cardiogenic_HEART_agg), cardiogenic_HEART_agg, 0 ),
#          cardiogenic_noHEART_agg = ifelse( !is.na(cardiogenic_noHEART_agg), cardiogenic_noHEART_agg, 0 ),
         
#          cardiogenic_RENAL_agg = ifelse( !is.na(cardiogenic_RENAL_agg), cardiogenic_RENAL_agg, 0 ),
#          cardiogenic_noRENAL_agg = ifelse( !is.na(cardiogenic_noRENAL_agg), cardiogenic_noRENAL_agg, 0 ),

#          cardiogenic_RESPI_agg = ifelse( !is.na(cardiogenic_RESPI_agg), cardiogenic_RESPI_agg, 0 ),
#          cardiogenic_noRESPI_agg = ifelse( !is.na(cardiogenic_noRESPI_agg), cardiogenic_noRESPI_agg, 0 ),

#          cardiogenic_STROKE_agg = ifelse( !is.na(cardiogenic_STROKE_agg), cardiogenic_STROKE_agg, 0 ),
#          cardiogenic_noSTROKE_agg = ifelse( !is.na(cardiogenic_noSTROKE_agg), cardiogenic_noSTROKE_agg, 0 ),

#          cardiogenic_DYSLIPI_agg = ifelse( !is.na(cardiogenic_DYSLIPI_agg), cardiogenic_DYSLIPI_agg, 0 ),
#          cardiogenic_noDYSLIPI_agg = ifelse( !is.na(cardiogenic_noDYSLIPI_agg), cardiogenic_noDYSLIPI_agg, 0 ),
         
#          male_under70_agg  = ifelse( !is.na(male_under70_agg), male_under70_agg , 0 ),
#          male_above70_agg  = ifelse( !is.na(male_above70_agg), male_above70_agg , 0 ),
#          male_under65_agg  = ifelse( !is.na(male_under65_agg), male_under65_agg , 0 ),
#          male_above65_agg  = ifelse( !is.na(male_above65_agg), male_above65_agg , 0 ),
#          male_under60_agg  = ifelse( !is.na(male_under60_agg), male_under60_agg , 0 ),
#          male_above60_agg  = ifelse( !is.na(male_above60_agg), male_above60_agg , 0 ),
#          male_under55_agg  = ifelse( !is.na(male_under55_agg), male_under55_agg , 0 ),
#          male_above55_agg  = ifelse( !is.na(male_above55_agg), male_above55_agg , 0 ),
#          male_under50_agg  = ifelse( !is.na(male_under50_agg), male_under50_agg , 0 ),
#          male_above50_agg  = ifelse( !is.na(male_above50_agg), male_above50_agg , 0 ),
#          male_under45_agg  = ifelse( !is.na(male_under45_agg), male_under45_agg , 0 ),
#          male_above45_agg  = ifelse( !is.na(male_above45_agg), male_above45_agg , 0 ),
#          male_under40_agg  = ifelse( !is.na(male_under40_agg), male_under40_agg , 0 ),
#          male_above40_agg  = ifelse( !is.na(male_above40_agg), male_above40_agg , 0 ),
#          male_under35_agg  = ifelse( !is.na(male_under35_agg), male_under35_agg , 0 ),
#          male_above35_agg  = ifelse( !is.na(male_above35_agg), male_above35_agg , 0 ),
#          male_under30_agg  = ifelse( !is.na(male_under30_agg), male_under30_agg , 0 ),
#          male_above30_agg  = ifelse( !is.na(male_above30_agg), male_above30_agg , 0 ),
#          male_under25_agg  = ifelse( !is.na(male_under25_agg), male_under25_agg , 0 ),
#          male_above25_agg  = ifelse( !is.na(male_above25_agg), male_above25_agg , 0 ),
#          male_under20_agg  = ifelse( !is.na(male_under20_agg), male_under20_agg , 0 ),
#          male_above20_agg  = ifelse( !is.na(male_above20_agg), male_above20_agg , 0 ),
#          male_under15_agg  = ifelse( !is.na(male_under15_agg), male_under15_agg , 0 ),
#          male_above15_agg  = ifelse( !is.na(male_above15_agg), male_above15_agg , 0 ),
#          male_under10_agg  = ifelse( !is.na(male_under10_agg), male_under10_agg , 0 ),
#          male_above10_agg  = ifelse( !is.na(male_above10_agg), male_above10_agg , 0 ),
#          male_under5_agg  = ifelse( !is.na(male_under5_agg), male_under5_agg , 0 ),
#          male_above5_agg  = ifelse( !is.na(male_above5_agg), male_above5_agg , 0 ),
#          male_above20_under40_agg  = ifelse( !is.na(male_above20_under40_agg), male_above20_under40_agg , 0 ),
#          male_above40_under60_agg  = ifelse( !is.na(male_above40_under60_agg), male_above40_under60_agg , 0 ),
#          male_above60_under75_agg  = ifelse( !is.na(male_above60_under75_agg), male_above60_under75_agg , 0 ),
#          male_above75_agg  = ifelse( !is.na(male_above75_agg), male_above75_agg , 0 ),
         
#          female_under70_agg  = ifelse( !is.na(female_under70_agg), female_under70_agg , 0 ),
#          female_above70_agg  = ifelse( !is.na(female_above70_agg), female_above70_agg , 0 ),
#          female_under65_agg  = ifelse( !is.na(female_under65_agg), female_under65_agg , 0 ),
#          female_above65_agg  = ifelse( !is.na(female_above65_agg), female_above65_agg , 0 ),
#          female_under60_agg  = ifelse( !is.na(female_under60_agg), female_under60_agg , 0 ),
#          female_above60_agg  = ifelse( !is.na(female_above60_agg), female_above60_agg , 0 ),
#          female_under55_agg  = ifelse( !is.na(female_under55_agg), female_under55_agg , 0 ),
#          female_above55_agg  = ifelse( !is.na(female_above55_agg), female_above55_agg , 0 ),
#          female_under50_agg  = ifelse( !is.na(female_under50_agg), female_under50_agg , 0 ),
#          female_above50_agg  = ifelse( !is.na(female_above50_agg), female_above50_agg , 0 ),
#          female_under45_agg  = ifelse( !is.na(female_under45_agg), female_under45_agg , 0 ),
#          female_above45_agg  = ifelse( !is.na(female_above45_agg), female_above45_agg , 0 ),
#          female_under40_agg  = ifelse( !is.na(female_under40_agg), female_under40_agg , 0 ),
#          female_above40_agg  = ifelse( !is.na(female_above40_agg), female_above40_agg , 0 ),
#          female_under35_agg  = ifelse( !is.na(female_under35_agg), female_under35_agg , 0 ),
#          female_above35_agg  = ifelse( !is.na(female_above35_agg), female_above35_agg , 0 ),
#          female_under30_agg  = ifelse( !is.na(female_under30_agg), female_under30_agg , 0 ),
#          female_above30_agg  = ifelse( !is.na(female_above30_agg), female_above30_agg , 0 ),
#          female_under25_agg  = ifelse( !is.na(female_under25_agg), female_under25_agg , 0 ),
#          female_above25_agg  = ifelse( !is.na(female_above25_agg), female_above25_agg , 0 ),
#          female_under20_agg  = ifelse( !is.na(female_under20_agg), female_under20_agg , 0 ),
#          female_above20_agg  = ifelse( !is.na(female_above20_agg), female_above20_agg , 0 ),
#          female_under15_agg  = ifelse( !is.na(female_under15_agg), female_under15_agg , 0 ),
#          female_above15_agg  = ifelse( !is.na(female_above15_agg), female_above15_agg , 0 ),
#          female_under10_agg  = ifelse( !is.na(female_under10_agg), female_under10_agg , 0 ),
#          female_above10_agg  = ifelse( !is.na(female_above10_agg), female_above10_agg , 0 ),
#          female_under5_agg  = ifelse( !is.na(female_under5_agg), female_under5_agg , 0 ),
#          female_above5_agg  = ifelse( !is.na(female_above5_agg), female_above5_agg , 0 ),
#          female_above20_under40_agg  = ifelse( !is.na(female_above20_under40_agg), female_above20_under40_agg , 0 ),
#          female_above40_under60_agg  = ifelse( !is.na(female_above40_under60_agg), female_above40_under60_agg , 0 ),
#          female_above60_under75_agg  = ifelse( !is.na(female_above60_under75_agg), female_above60_under75_agg , 0 ),
#          female_above75_agg  = ifelse( !is.na(female_above75_agg), female_above75_agg , 0 ),
         
#          cardiogenic_cold_agg = ifelse( !is.na(cardiogenic_cold_agg), cardiogenic_cold_agg, 0 ),
#          cardiogenic_warm_agg = ifelse( !is.na(cardiogenic_warm_agg), cardiogenic_warm_agg, 0 ),

#          cold_male_agg = ifelse( !is.na(cold_male_agg), cold_male_agg, 0 ),
#          cold_female_agg = ifelse( !is.na(cold_female_agg), cold_female_agg, 0 ),
#          warm_male_agg = ifelse( !is.na(warm_male_agg), warm_male_agg, 0 ),
#          warm_female_agg = ifelse( !is.na(warm_female_agg), warm_female_agg, 0 ),
         
#          cardiogenic_dawn_agg = ifelse( !is.na(cardiogenic_dawn_agg), cardiogenic_dawn_agg, 0 ),
#          cardiogenic_working_agg = ifelse( !is.na(cardiogenic_working_agg), cardiogenic_working_agg, 0 ),
#          cardiogenic_night_agg = ifelse( !is.na(cardiogenic_night_agg), cardiogenic_night_agg, 0 ),
         
#          HTN_male_agg = ifelse( !is.na(HTN_male_agg), HTN_male_agg, 0 ),
#          noHTN_male_agg = ifelse( !is.na(noHTN_male_agg), noHTN_male_agg, 0 ),
         
#          HTN_female_agg = ifelse( !is.na(HTN_female_agg), HTN_female_agg, 0 ),
#          noHTN_female_agg = ifelse( !is.na(noHTN_female_agg), noHTN_female_agg, 0 ),
         
#          DM_male_agg = ifelse( !is.na(DM_male_agg), DM_male_agg, 0 ),
#          noDM_male_agg = ifelse( !is.na(noDM_male_agg), noDM_male_agg, 0 ),
         
#          DM_female_agg  = ifelse( !is.na(DM_female_agg), DM_female_agg, 0 ),
#          noDM_female_agg   = ifelse( !is.na(noDM_female_agg), noDM_female_agg, 0 ),
         
#          HEART_male_agg = ifelse( !is.na(HEART_male_agg), HEART_male_agg, 0 ),
#          noHEART_male_agg    = ifelse( !is.na(noHEART_male_agg), noHEART_male_agg, 0 ),
         
#          HEART_female_agg  = ifelse( !is.na(HEART_female_agg), HEART_female_agg, 0 ),
#          noHEART_female_agg  = ifelse( !is.na(noHEART_female_agg), noHEART_female_agg, 0 ),
         
#          HTN_under70_agg  = ifelse( !is.na(HTN_under70_agg), HTN_under70_agg, 0 ),
#          noHTN_under70_agg  = ifelse( !is.na(noHTN_under70_agg), noHTN_under70_agg, 0 ),
         
#          HTN_above70_agg  = ifelse( !is.na(HTN_above70_agg), HTN_above70_agg, 0 ),
#          noHTN_above70_agg  = ifelse( !is.na(noHTN_above70_agg), noHTN_above70_agg, 0 ),
         
#          DM_under70_agg = ifelse( !is.na(DM_under70_agg), DM_under70_agg, 0 ),
#          noDM_under70_agg  = ifelse( !is.na(noDM_under70_agg), noDM_under70_agg, 0 ),
         
#          DM_above70_agg    = ifelse( !is.na(DM_above70_agg  ), DM_above70_agg  , 0 ),
#          noDM_above70_agg    = ifelse( !is.na(noDM_above70_agg ), noDM_above70_agg , 0 ),
         
#          HEART_under70_agg     = ifelse( !is.na(HEART_under70_agg  ), HEART_under70_agg    , 0 ),
#          noHEART_under70_agg     = ifelse( !is.na(noHEART_under70_agg ), noHEART_under70_agg    , 0 ),
         
#          HEART_above70_agg    = ifelse( !is.na(HEART_above70_agg  ), HEART_above70_agg  , 0 ),
#          noHEART_above70_agg   = ifelse( !is.na(noHEART_above70_agg ), noHEART_above70_agg , 0 )
#   )

final_data_lag_re2 <- copy(final_data_lag_re)
loc <- grepl('agg', names(final_data_lag_re2)) ## agg가 포함된 column 모두 선택
final_data_lag_re2[loc] <-
  lapply(final_data_lag_re2[loc], function(x) {
    ifelse(!is.na(x),
           x,
           0)
  }) ## mutate이 알 수 없는 에러로 작동이 안됨 .. 위의 final_data_lag_re2 생성하는 mutate 함수 대체 (같은 기능)

summary(final_data_lag_re$female_above75_agg)
summary(final_data_lag_re2$female_above75_agg) # 잘 만들어진 거 확인
summary(final_data_lag_re$disease_cardiogenic_agg)
summary(final_data_lag_re2$disease_cardiogenic_agg)

colnames(final_data_lag_re2)
colSums(is.na(final_data_lag_re2))
head(final_data_lag_re2)
dim(final_data_lag_re2)

# humi == 0 -> NA로 대체 (raw 데이터에서 2009~2010은 NA 대신 0이 들어가 있음)
sum(is.na(final_data_lag_re2$humi_mean_total)) # humi na 17510
final_data_lag_re2$humi_mean_total[final_data_lag_re2$humi_mean_total == 0] <- NA
sum(is.na(final_data_lag_re2$humi_mean_total)) # humi na 30583

# humi_mean_total == na인 unique air_out_idx 리스트 뽑기 - 23개

  humi_unique_index = final_data_lag_re2$air_out_idx
  humi_unique_index <- unique(humi_unique_index)
  humi_na_list = unlist(unique(final_data_lag_re2[is.na(final_data_lag_re2$humi_mean_total)==TRUE,'air_out_idx']), use.names = FALSE)
  x <- as.set(humi_unique_index)
  y <- as.set(humi_na_list)
  
  setdiff(x, y) # 76, 196
  
  # 76 (동작) na check - 0
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '76' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 196 (종로) na check - 0
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '196' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  
  # t=as.data.frame(table(final_data_lag_re2[is.na(final_data_lag_re2$humi_mean_total) == TRUE, c('date', 'air_out_idx')]))
  # t$year=substr(t$date,1,4)
  # length(unique(t$air_out_idx))
  # a <- aggregate(t$Freq, by=list(t$year, t$air_out_idx), FUN=sum)
  # library(reshape2)
  # reshape(a, idvar='Group.1', timevar='Group.2', v.names='x', direction='wide')

### 각 air_out_idx 별 humi NA 확인 ###
  # 117 - 650개 (기존 11개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '117' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 3 - 596개 (기존 10개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '3' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 122 - 2360개 (기존 1719개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '122' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 2 - 1256개 (기존 556개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '2' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 5 - 656개 (기존 9개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '5' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 6 - 2346개 (기존 1715개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '6' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 27 - 819개 (기존 173개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '27' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 32 - 656개 (기존 10개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '32' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 35 - 648개 (기존 10개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '35' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 43 - 2421개 (기존 1780개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '43' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 58 - 657개 (기존 10개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '58' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 66 - 636개 (기존 8개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '66' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 73 - 641개 (기존 22개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '73' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 78 - 1046개 (기존 342개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '78' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 110 - 1690개 (동일)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '110' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 113 - 2486개(기존 1773개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '113' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 118 - 651개 (기존 13개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '118' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 155 - 2430개 (기존 1741개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '155' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 177 - 2445개 (기존 1753개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '177' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 197 - 1755개 (기존 1755개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '197' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 203 - 623개 (기존 8개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '203' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 144 - 2491개 (기존 1789개)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '144' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]
  # 168 - 614개 (동일)
  final_data_lag_re2[final_data_lag_re2$air_out_idx == '168' & is.na(final_data_lag_re2$humi_mean_total) == TRUE,]

### 각 air_out_idx 별 humi NA 확인 end. ###
  
### replace indexes (that has 100+a humi NA, total count of 23) with adjacent areas ###
  humi_all = final_data_lag_re2[,c('date', 'air_out_idx', 'humi_mean_total')] 
  dim(humi_all)
  
  ## 1. 2009-2010 - replace gangbuk with jongno (종로) / gangnam with dongjak (동작)
  humi_0910 <- humi_all[substr(humi_all$date,1,4) %in% c(2009, 2010), c('date', 'air_out_idx', 'humi_mean_total')]
  dim(humi_0910)
    # Gangbuk (117, 5, 32, 58, 66, 73, 78, 110, 118, 177, 197, 203, 168) -> Jongno (196)
    humi_0910_117 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_117$air_out_idx = 117
    humi_0910_5 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_5$air_out_idx = 5
    humi_0910_32 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_32$air_out_idx = 32
    humi_0910_58 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_58$air_out_idx = 58
    humi_0910_66 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_66$air_out_idx = 66
    humi_0910_73 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_73$air_out_idx = 73
    humi_0910_78 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_78$air_out_idx = 78
    humi_0910_110 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_110$air_out_idx = 110
    humi_0910_118 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_118$air_out_idx = 118
    humi_0910_177 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_177$air_out_idx = 177
    humi_0910_197 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_197$air_out_idx = 197
    humi_0910_203 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_203$air_out_idx = 203
    humi_0910_168 <- humi_0910[which(humi_0910$air_out_idx==196),]
    humi_0910_168$air_out_idx = 168
    
    humi_0910_gangbuk_with_replaced_na <- rbind(humi_0910_117, humi_0910_5, humi_0910_32, humi_0910_58, humi_0910_66, humi_0910_73, humi_0910_78, humi_0910_110, humi_0910_118, humi_0910_177, humi_0910_197, humi_0910_203, humi_0910_168)
    dim(humi_0910_gangbuk_with_replaced_na)
    colSums(is.na(humi_0910_gangbuk_with_replaced_na))
    ## Gangbuk end. ##
  
    ## Gangnam (3, 122, 2, 6, 27, 35, 43, 113, 155, 144) -> Dongjak (76)
    humi_0910_3 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_3$air_out_idx = 3
    humi_0910_122 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_122$air_out_idx = 122
    humi_0910_2 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_2$air_out_idx = 2
    humi_0910_6 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_6$air_out_idx = 6
    humi_0910_27 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_27$air_out_idx = 27
    humi_0910_35 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_35$air_out_idx = 35
    humi_0910_43 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_43$air_out_idx = 43
    humi_0910_113 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_113$air_out_idx = 113
    humi_0910_155 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_155$air_out_idx = 155
    humi_0910_144 <- humi_0910[which(humi_0910$air_out_idx==76),]
    humi_0910_144$air_out_idx = 144
    
    humi_0910_gangnam_with_replaced_na <- rbind(humi_0910_3, humi_0910_122, humi_0910_2, humi_0910_6, humi_0910_27, humi_0910_35, humi_0910_43, humi_0910_113, humi_0910_155, humi_0910_144)
    dim(humi_0910_gangnam_with_replaced_na)
    colSums(is.na(humi_0910_gangnam_with_replaced_na))
    ## Gangnam end. 
  
    # rbind gangbuk + gangnam and create 2009-2010 replaced dataset
    humi_0910_with_replaced_na <- rbind(humi_0910_gangnam_with_replaced_na, humi_0910_gangbuk_with_replaced_na)
    length(unique(humi_0910_with_replaced_na$air_out_idx)) # 23 index checked. 
    
    # create dataset for other two indexes (76, 196)
    humi_0910_with_no_na <- subset(humi_0910, air_out_idx %in% c(196, 76))
    length(unique(humi_0910_with_no_na$air_out_idx))
    colSums(is.na(humi_0910_with_no_na)) # humi na 0 checked. 
    
    # create humi_0910_final
    humi_0910_final <- rbind(humi_0910_with_replaced_na, humi_0910_with_no_na)
    dim(humi_0910_final)
  ## 1. 2009-2010 replace NA end. 
  
  # 2. 2011-2019
  humi_1119 <- humi_all[substr(humi_all$date,1,4) %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), c('date', 'air_out_idx', 'humi_mean_total')]
  head(humi_1119)
  dim(humi_1119)
  
    # 122 (Songpa) -> 3 (Gangdong)
    humi_122 = humi_1119[which(humi_1119$air_out_idx==3), ] 
    humi_122$air_out_idx=122
    
    # 2 (Gangnam) -> 3 (Gangdong)
    humi_2 = humi_1119[which(humi_1119$air_out_idx==3), ] 
    humi_2$air_out_idx=2
    
    # 6 (Gangseo) -> 35 (Guro)
    humi_6 = humi_1119[which(humi_1119$air_out_idx==35), ] 
    humi_6$air_out_idx=6
    
    # 27 (Gwanak) -> 76 (Dongjak)
    humi_27 = humi_1119[which(humi_1119$air_out_idx==76), ] 
    humi_27$air_out_idx=27
    
    # 43 (Geumchun) -> 76 (Dongjak)
    humi_43 = humi_1119[which(humi_1119$air_out_idx==76), ] 
    humi_43$air_out_idx=43
    
    # 78 (Mapo) -> 196 (Jongno)
    humi_78 = humi_1119[which(humi_1119$air_out_idx==196), ] 
    humi_78$air_out_idx=78
    
    # 110 (Seodaemun) -> 196 (Jongno)
    humi_110 = humi_1119[which(humi_1119$air_out_idx==196), ] 
    humi_110$air_out_idx=110
    
    # 113 (Seocho) -> 76 (Dongjak)
    humi_113 = humi_1119[which(humi_1119$air_out_idx==76), ] 
    humi_113$air_out_idx=113
    
    # 155 (Yeongdengpo) -> 35 (Guro)
    humi_155 = humi_1119[which(humi_1119$air_out_idx==35), ] 
    humi_155$air_out_idx=155
    
    # 177 (Eunpheong) -> 196 (Jongno)
    humi_177 = humi_1119[which(humi_1119$air_out_idx==196), ] 
    humi_177$air_out_idx=177
    
    # 197 (Seocho) -> 196 (Jongno)
    humi_197 = humi_1119[which(humi_1119$air_out_idx==196), ] 
    humi_197$air_out_idx=197
    
    # 144 (Yangcheon) -> 35 (Guro)
    humi_144 = humi_1119[which(humi_1119$air_out_idx==35), ] 
    humi_144$air_out_idx=144
    
    # 168 (Yongsan) -> 117 (Seongdong)
    humi_168 = humi_1119[which(humi_1119$air_out_idx==117), ] 
    humi_168$air_out_idx=168
  
    ## data concat (13 indexes) ## 
    humi_1119_with_replaced_na <- rbind(humi_122, humi_2, humi_6, humi_27, humi_43, humi_78, humi_110, humi_113, humi_155, humi_177, humi_197, humi_144, humi_168)
    length(unique(humi_1119_with_replaced_na$air_out_idx)) # 13 index checked. 
    colSums(is.na(humi_1119_with_replaced_na)) # humi na 3 checked. 
    
    # create dataset for other 12 indexes ##
    humi_1119_with_no_na <- subset(humi_1119, !(air_out_idx %in% unique(humi_1119_with_replaced_na$air_out_idx)))
    length(unique(humi_1119_with_no_na$air_out_idx)) # 12 index checked. 
    colSums(is.na(humi_1119_with_no_na)) # humi na 1 checked. 
    
    # create humi_1119_final 
    humi_1119_final <- rbind(humi_1119_with_replaced_na, humi_1119_with_no_na)
    dim(humi_1119_final)
  ## 2. 2011 - 2019 replace NA end. 
  
  ## create humi_replaced_final - 09-10 + 11-19 concat
  humi_replaced_final <- rbind(humi_0910_final, humi_1119_final)
  dim(humi_replaced_final)
  colSums(is.na(humi_replaced_final)) # humi na 4 checked. 
  length(unique(humi_replaced_final$air_out_idx)) # 25 index checked. 
  length(unique(humi_replaced_final$date))
  365.25*11*25
  
  # humi < 0 check
  humi_minus <- humi_replaced_final[humi_replaced_final$humi_mean_total < 0,]
  dim(humi_minus) # 8
  dim(humi_replaced_final) # 93143 3
  humi_replaced_final <- humi_replaced_final[humi_replaced_final$humi_mean_total > 0,]
  dim(humi_replaced_final) # 93139 3
  
### replace indexes (that has 100+a humi NA, total count of 23) with adjacent areas end. ###
  
# drop humi_mean_total from final_data_lag_re2, merge humi_replaced_final #
  final_data_lag_re2 <- subset(final_data_lag_re2, select = -c(humi_mean_total, ns.basis_hum))
  colnames(final_data_lag_re2)
  final_data_lag_re3 <- left_join(final_data_lag_re2,humi_replaced_final,by=c("date","air_out_idx"))
  colnames(final_data_lag_re3)
  colSums(is.na(final_data_lag_re3)) # humi na 2070
  dim(final_data_lag_re3) # 90159 224
  write.csv(final_data_lag_re3, 'IV_OHCA/final_data_lag_re3.csv', row.names = FALSE)
  
# humi na 2070 - replace with mean by date
  # 이것도 갑자기 mutate function error - 아래 코드로 대체 - 취소
  ## !! 어떻게 해도 mutate, summarise function이 적용이 안됨 !! - 파이썬에서 처리해서 다시 가져와야 함 (2-1.humidity modify ~~ 참조)
  # final_data_lag_re3$humi_mean_total = as.numeric(as.character(final_data_lag_re3$humi_mean_total))
  # unique(final_data_lag_re3$humi_mean_total)
  # final_data_lag_re3_tmp <- final_data_lag_re3 %>% group_by(air_out_idx) %>% mutate(humi_mean_total = ifelse(is.na(humi_mean_total), mean(humi_mean_total, na.rm = T), humi_mean_total))
  # final_data_lag_re3$humi_mean_total_meanbydate <- final_data_lag_re3$humi_mean_total %>% group_by(air_out_idx) 
  
  # humi_mean_total_meanbydate <- final_data_lag_re3 %>% group_by(air_out_idx) %>% 
  # summarise(humi_mean_total_meanbydate=mean(humi_mean_total),
  #           .groups = 'drop')

  # head(humi_mean_total_meanbydate)

  # summary(final_data_lag_re3$humi_mean_total)
  # dim(final_data_lag_re3)
  
  # setDT(final_data_lag_re3)
  # cols <- c("humi_mean_total")
  # final_data_lag_re3[, (cols) := lapply(.SD, function(x) nafill(x, type = "const", fill = mean(x, na.rm = TRUE)))
  #   , by = date
  #   , .SDcols = cols][]
  # dim(final_data_lag_re3)
  # tmp <- final_data_lag_re3[is.na(final_data_lag_re3$humi_mean_total) == TRUE,c('humi_mean_total')]
  # tmp <- final_data_lag_re3[,c('humi_mean_total')]
  # sapply(tmp, class)
  final_data_lag_re3_humichanged <- read.csv('IV_OHCA/final_data_lag_re3_humi_modified_python.csv')
  dim(final_data_lag_re3_humichanged)
  colnames(final_data_lag_re3_humichanged)
  summary(final_data_lag_re3_humichanged$humi_mean_total)
  dim(final_data_lag_re3_humichanged)
  sum(is.na(final_data_lag_re3_humichanged$humi_mean_total))
  colSums(is.na(final_data_lag_re3_humichanged))
  
  final_data_lag_re3 <- copy(final_data_lag_re3_humichanged)
  # final_data_lag_re3 <- subset(final_data_lag_re3, select = -c(ns.basis_hum))

### humidity basis 만 다시 생성 ###
kno_hum <- equalknots(final_data_lag_re3$humi_mean_total,nk=2)
  
ns.basis_hum <- crossbasis(final_data_lag_re3$humi_mean_total,argvar=list(knots=kno_hum),
                             group=final_data_lag_re3$air_out_idx)
  
final_data_lag_re3$ns.basis_hum <- crossbasis(final_data_lag_re3$humi_mean_total,argvar=list(knots=kno_hum),group=final_data_lag_re3$air_out_idx)
### humidity basis 만 다시 생성 end. ###

11*365.25*25

write.csv(final_data_lag_re3, 'IV_OHCA/sub/final_data_lag_re3.csv')
dim(final_data_lag_re3)
colSums(is.na(final_data_lag_re3))
colnames(final_data_lag_re3)

## lag0-3만 살리기 ##
## lag 14까지 추가 살리기 # 
# final_data_lag_re3 <- subset(final_data_lag_re3, select = -c(AQI_lag4, AQI_lag5, AQI_lag6, AQI_lag7, AQI_lag8, AQI_lag9, AQI_lag10, AQI_lag11, AQI_lag12, AQI_lag13, AQI_lag14))
# final_data_lag_re3 <- subset(final_data_lag_re3, select = -c(IV_300_BI_lag4, IV_300_BI_lag5, IV_300_BI_lag6, IV_300_BI_lag7, IV_300_BI_lag8, IV_300_BI_lag9, IV_300_BI_lag10, IV_300_BI_lag11, IV_300_BI_lag12, IV_300_BI_lag13, IV_300_BI_lag14))
final_data_lag_re3 <- subset(final_data_lag_re3, select = -c(SOCIAL))
colSums(is.na(final_data_lag_re3))
## lag0-3만 살리기 end. ##
  
final_data_lag_re2_na = final_data_lag_re3[complete.cases(final_data_lag_re3),] ; 
# lag14까지 살릴경우 na omit 하지 말고; 
final_data_lag_re2_na <- copy(final_data_lag_re3)
# final_data_lag_re3[,c(15,17,21:25,29,34,50:147)]

colnames(final_data_lag_re2_na)

colSums(is.na(final_data_lag_re2_na))
dim(final_data_lag_re2_na) # 87799 217
final_data_lag_re2_na$IV_300_BI <- as.numeric(final_data_lag_re2_na$IV_300_BI)
# for (i in c(32:44)){
#   final_data_lag_na2[,i] = as.numeric(final_data_lag_na2[,i])
# }

table(final_data_lag_re2_na$male_under55_agg)
table(final_data_lag_re2_na$male_above55_agg)
table(final_data_lag_re2_na$male_under50_agg)
table(final_data_lag_re2_na$male_above50_agg)
table(final_data_lag_re2_na$male_under45_agg)
table(final_data_lag_re2_na$male_above45_agg)
table(final_data_lag_re2_na$male_under40_agg)
table(final_data_lag_re2_na$male_above40_agg)
table(final_data_lag_re2_na$male_under35_agg)
table(final_data_lag_re2_na$male_above35_agg)

table(final_data_lag_re2_na$female_under55_agg)
table(final_data_lag_re2_na$female_above55_agg)
table(final_data_lag_re2_na$female_under50_agg)
table(final_data_lag_re2_na$female_above50_agg)
table(final_data_lag_re2_na$female_under45_agg)
table(final_data_lag_re2_na$female_above45_agg)
table(final_data_lag_re2_na$female_under40_agg)
table(final_data_lag_re2_na$female_above40_agg)
table(final_data_lag_re2_na$female_under35_agg)
table(final_data_lag_re2_na$female_above35_agg)


write.csv(final_data_lag_re2_na,"IV_OHCA/sub/final_data_lag_re2_na.csv")

library("fastDummies")
results <- fastDummies::dummy_cols(final_data_lag_re2_na, select_columns = "wday")

dim(results)
head(results)
colnames(results)
colSums(is.na(results))
IQR(results$AQI)
tail(results)
length(unique(results$air_out_idx)) # 25
25*365.25*11
dim(results) # 87799 226

## adding moving average ##
colnames(results)
# AQI
  results$AQI_lag0_lag1 <- rowMeans(results[,c('AQI', 'AQI_lag1')], na.rm = TRUE)
  results$AQI_lag0_lag2 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2')], na.rm = TRUE)
  results$AQI_lag0_lag3 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3')], na.rm = TRUE)
  results$AQI_lag0_lag4 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4')], na.rm = TRUE)
  results$AQI_lag0_lag5 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5')], na.rm = TRUE)
  results$AQI_lag0_lag6 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6')], na.rm = TRUE)
  results$AQI_lag0_lag7 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7')], na.rm = TRUE)
  results$AQI_lag0_lag8 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8')], na.rm = TRUE)
  results$AQI_lag0_lag9 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9')], na.rm = TRUE)
  results$AQI_lag0_lag10 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10')], na.rm = TRUE)
  results$AQI_lag0_lag11 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11')], na.rm = TRUE)
  results$AQI_lag0_lag12 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12')], na.rm = TRUE)
  results$AQI_lag0_lag13 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12', 'AQI_lag13')], na.rm = TRUE)
  results$AQI_lag0_lag14 <- rowMeans(results[,c('AQI', 'AQI_lag1', 'AQI_lag2', 'AQI_lag3', 'AQI_lag4', 'AQI_lag5', 'AQI_lag6', 'AQI_lag7', 'AQI_lag8', 'AQI_lag9', 'AQI_lag10', 'AQI_lag11', 'AQI_lag12', 'AQI_lag13', 'AQI_lag14')], na.rm = TRUE)
  # thermal inversion
  results$IV_300_BI_lag0_lag1 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag2 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag3 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag4 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag5 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag6 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag7 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag8 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag9 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag10 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag11 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag12 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag13 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12', 'IV_300_BI_lag13')], na.rm = TRUE)
  results$IV_300_BI_lag0_lag14 <- rowMeans(results[,c('IV_300_BI', 'IV_300_BI_lag1', 'IV_300_BI_lag2', 'IV_300_BI_lag3', 'IV_300_BI_lag4', 'IV_300_BI_lag5', 'IV_300_BI_lag6', 'IV_300_BI_lag7', 'IV_300_BI_lag8', 'IV_300_BI_lag9', 'IV_300_BI_lag10', 'IV_300_BI_lag11', 'IV_300_BI_lag12', 'IV_300_BI_lag13', 'IV_300_BI_lag14')], na.rm = TRUE)
  colSums(is.na(results))  

colSums(is.na(results))  
dim(results) # 87799 232 # na omit 안하고 lag14까지 살리면 90138 279

#### adding moving average end. ####


colnames(results)

write.csv(results,"IV_OHCA/sub/results_230317_NAdroped.csv")
colSums(is.na(results))
save.image("IV_OHCA/# sudden_death_230112.RData")

table(results$disease_non_agg)
table(results$disease_cardiogenic_agg)

##summary
# 09-19 dataset 만 빼서 확인
results0919 = results[substr(results$date,1,4) %in% c("2009"),] ; sum(results0919$cardiogenic_female_agg)
results0919 = results[substr(results$date,1,4) %in% c("2013"),] ; sum(results0919$count_disease)


results0919 = results[substr(results$date,1,4) %in% c("2014"),] ; sum(results0919$count_disease)
results0919 = results[substr(results$date,1,4) %in% c("2015"),] ; sum(results0919$count_disease)

# zio
sum(results$noDM_female_agg)
sum(results$cardiogenic_under5_agg)
quantile(results$humi_mean_total, c(0.05, 0.95))
mean(results$SO2)
sd(results$SO2)
cor(results$AQI, results$IV_300_BI)

##### [10] Instrumental Variables Analysis #####
length(unique(final_data_lag_re2_na$air_out_idx))
AQI_IQR <- IQR(final_data_lag_re2_na$AQI)

glm_iv <- iv.glm(model_formula = count_disease_cardiogenic ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI,
                 instrument_formula = AQI ~ IV_300_BI, 
                 data=final_data_lag_re2_na,family =quasipoisson, link = 'log') ;summary(glm_iv)
# lagged days
glm_iv <- iv.glm(model_formula = cardiogenic_DYSLIPI_agg ~ rain_sum+ ns.basis_temp+ns.basis_yday+wday
                 +ns.basis_hum+ns.basis_pressure+AQI_lag5,
                 instrument_formula = AQI_lag5 ~ IV_300_BI_lag5,
                 data=final_data_lag_re2_na,family =quasipoisson, link = 'log') ;summary(glm_iv)



.# final_data_lag_na3[,!names(final_data_lag_na3) %in% c("SOCIAL")]
