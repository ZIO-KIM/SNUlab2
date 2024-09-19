##### 7. offset added model (ESF X) #####

    # lag 추가
    df = df%>%
        group_by(air_out_idx)%>%
        mutate(ATOPIC_in_total_lag7=lag(ATOPIC_in_total,7))%>%
        mutate(ATOPIC_in_total_lag14=lag(ATOPIC_in_total,14))
    dim(df)

    # 7일 제거
    df <- na.omit(df)
    table(df$ATOPIC_in_total_lag7)
    dim(df)
    # offset(log(df$ATOPIC_in_total_lag7))

    # outcome_lag 변수들에 + 1 (for log)
    df$ATOPIC_in_total_lag7 = df$ATOPIC_in_total_lag7 + 1
    df$ATOPIC_in_total_lag14 = df$ATOPIC_in_total_lag14 + 1


  # set path to folder where plots to be saved
  setwd(path)

  # 6-1. ns.basis_no
    # fit
    ns_lag_no <- glm(ATOPIC_in_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  # + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_in_total_lag7))
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 6-2. ns.basis_so
    # fit
    ns_lag_so <- glm(ATOPIC_in_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    # crosspred
    percentiles_so <- round(quantile(df$SO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_so
    ns.pred_so <- crosspred(ns.basis_so,ns_lag_so,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1])

    low = round(ns.pred_so$allRRlow[length(ns.pred_so$allRRfit)], digits = 2)
    mid = round(ns.pred_so$allRRfit[length(ns.pred_so$allRRfit)], digits = 2)
    high = round(ns.pred_so$allRRhigh[length(ns.pred_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean.png")
    plot(ns.pred_so,zlab="RR",xlab="SO2_mean")
    dev.off()
    png(filename="SO2_mean_overall.png")
    plot(ns.pred_so,"overall",xlab="SO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 6-3. ns.basis_co
    # fit
    ns_lag_co <- glm(ATOPIC_in_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    # crosspred
    percentiles_co <- round(quantile(df$CO_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_co
    ns.pred_co <- crosspred(ns.basis_co,ns_lag_co,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1])

    low = round(ns.pred_co$allRRlow[length(ns.pred_co$allRRfit)], digits = 2) 
    mid = round(ns.pred_co$allRRfit[length(ns.pred_co$allRRfit)], digits = 2) 
    high = round(ns.pred_co$allRRhigh[length(ns.pred_co$allRRfit)], digits = 2) 

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean.png")
    plot(ns.pred_co,zlab="RR",xlab="CO_mean")
    dev.off()
    png(filename="CO_mean_overall.png")
    plot(ns.pred_co,"overall",xlab="CO_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 6-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(ATOPIC_in_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    # crosspred
    percentiles_o3 <- round(quantile(df$O3_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_o3
    ns.pred_o3 <- crosspred(ns.basis_o3,ns_lag_o3,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1])

    low = round(ns.pred_o3$allRRlow[length(ns.pred_o3$allRRfit)], digits = 2) 
    mid = round(ns.pred_o3$allRRfit[length(ns.pred_o3$allRRfit)], digits = 2) 
    high = round(ns.pred_o3$allRRhigh[length(ns.pred_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean.png")
    plot(ns.pred_o3,zlab="RR",xlab="O3_mean")
    dev.off()
    png(filename="O3_mean_overall.png")
    plot(ns.pred_o3,"overall",xlab="O3_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 6-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(ATOPIC_in_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(ATOPIC_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    # crosspred
    percentiles_pm <- round(quantile(df$PM10_mean,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_pm
    ns.pred_pm <- crosspred(ns.basis_pm,ns_lag_pm,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1])

    low = round(ns.pred_pm$allRRlow[length(ns.pred_pm$allRRfit)], digits = 2) 
    mid = round(ns.pred_pm$allRRfit[length(ns.pred_pm$allRRfit)], digits = 2) 
    high = round(ns.pred_pm$allRRhigh[length(ns.pred_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean.png")
    plot(ns.pred_pm,zlab="RR",xlab="PM10_mean")
    dev.off()
    png(filename="PM10_mean_overall.png")
    plot(ns.pred_pm,"overall",xlab="PM10_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

##### offset added model (ESF X) end. #####
######################################
