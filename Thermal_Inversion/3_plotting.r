
# install.packages('reshape')
library(ggplot2)
library(reshape)

main_result <- read_excel('D:\\SNUlab\\1. Results\\Thermal Inversion\\figures\\main_result.xlsx')
main_result_python <- read_excel('D:\\SNUlab\\1. Results\\Thermal Inversion\\figures\\main_result_python.xlsx')

plot_data <- copy(main_result)
plot_data <- copy(main_result_python)
plot_data <- cast(plot_data, disease ~ lag) # long to wide format
plot_data <- as.matrix(plot_data)

# bar - wide format
mybar <- barplot(plot_data, 
                beside = TRUE,
                las = 0.1, 
                legend = TRUE)

# scatter - long format
ggplot(plot_data, aes(lag, RR,colour=disease)) + 
    geom_line() + 
    geom_point()

# scatter - long format - outcome 별로
ggplot(plot_data, aes(lag, asthma), color = asthma) + 
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(ymin = as_conf_min, ymax = as_conf_max), width = 0.2)

# line - long format
plot_data %>%
  ggplot( aes(x=lag, y=RR, group=disease, color=disease)) +
    geom_line()



# asthma out, rhinitis out, aqi, aqi_365 lead plot 그려보기
df_as <- copy(lead365_lag)
df_rh <- copy(lead365_lag)  # rhinitis 에서 만들고 올것


df_f <- merge(df_rh, df_as[,c('air_out_idx', 'date', 'ASTHMA_out_total_agg')], by=c('air_out_idx', 'date'), all.x = T)
df_f <- na.omit(df_f)

# 2014-2015만 뽑아보자
df_f <- df_f[substr(df_f$date, 1, 4) %in% c(2014:2015),]
df_f$date_month <- substr(df_f$date, 1, 7)
head(df_f)

# line - long format
# 
df_f %>%
  ggplot(aes(x=date, y=ASTHMA_out_total_agg)) +
    geom_line()

df_f %>%
  ggplot(aes(x=date, y=RHINITIS_out_total_agg)) +
    geom_line()

df_f %>%
  ggplot(aes(x=date, y=AQI)) +
    geom_line()

df_f %>%
  ggplot(aes(x=date, y=AQI_lead365)) +
    geom_line()




df_f %>%
  ggplot(aes(x=date_month, y=ASTHMA_out_total_agg)) +
    geom_line()