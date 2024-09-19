

test <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\dump\\lag0_dump.csv')
test365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\dump\\asthma_iv_neg_control_lag0_test.csv')


test_final <- merge(test365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365","AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], test, by = c("air_out_idx", "date"), all.x = T)

colnames(test365)
colnames(test_final)

write.csv(test_final, "D:\\SNUlab\\thermal_inversion_0623_share\\dump\\testfinal.csv")



# lag 0
lag0 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag0.csv')
lag0_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag0.csv')
lag0_final <- merge(lag0_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag0, by = c("air_out_idx", "date"), all.x = T)
colnames(lag0_final)
write.csv(lag0_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag0_aqi_aqi365.csv')
cor(lag0_final$AQI,lag0_final$AQI_lead365)
# lag 1
lag1 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag1.csv')
lag1_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag1.csv')
lag1_final <- merge(lag1_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag1, by = c("air_out_idx", "date"), all.x = T)
colnames(lag1_final)
write.csv(lag1_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag1_aqi_aqi365.csv')

# lag 2
lag2 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag2.csv')
lag2_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag2.csv')
lag2_final <- merge(lag2_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag2, by = c("air_out_idx", "date"), all.x = T)
colnames(lag2_final)
write.csv(lag2_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag2_aqi_aqi365.csv')

# lag 3
lag3 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag3.csv')
lag3_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag3.csv')
lag3_final <- merge(lag3_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag3, by = c("air_out_idx", "date"), all.x = T)
colnames(lag3_final)
write.csv(lag3_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag3_aqi_aqi365.csv')

# lag 4
lag4 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag4.csv')
lag4_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag4.csv')
lag4_final <- merge(lag4_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag4, by = c("air_out_idx", "date"), all.x = T)
colnames(lag4_final)
write.csv(lag4_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag4_aqi_aqi365.csv')

# lag 5
lag5 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag5.csv')
lag5_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag5.csv')
lag5_final <- merge(lag5_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag5, by = c("air_out_idx", "date"), all.x = T)
colnames(lag5_final)
write.csv(lag5_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag5_aqi_aqi365.csv')

# lag 6
lag6 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag6.csv')
lag6_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag6.csv')
lag6_final <- merge(lag6_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag6, by = c("air_out_idx", "date"), all.x = T)
colnames(lag6_final)
write.csv(lag6_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag6_aqi_aqi365.csv')

# lag 7
lag7 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_lag7.csv')
lag7_365 <- read.csv('D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\asthma_iv_neg_control_lag7.csv')
lag7_final <- merge(lag7_365[, c("air_out_idx", "date", "AQI_lead365", "AQI_lead365_lag1","AQI_lead365_lag2","AQI_lead365_lag3","AQI_lead365_lag4","AQI_lead365_lag5","AQI_lead365_lag6","AQI_lead365_lag7",
          "IV_300_BI_lead365","IV_300_BI_lead365_lag1","IV_300_BI_lead365_lag2","IV_300_BI_lead365_lag3","IV_300_BI_lead365_lag4","IV_300_BI_lead365_lag5","IV_300_BI_lead365_lag6","IV_300_BI_lead365_lag7")], 
          lag7, by = c("air_out_idx", "date"), all.x = T)
colnames(lag7_final)
write.csv(lag7_final, 'D:\\SNUlab\\thermal_inversion_0623_share\\lag datas with index\\aqi + aqi365\\asthma_lag7_aqi_aqi365.csv')

