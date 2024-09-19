path4 = paste0(path, '\\1. general model (ESF X)')
unlink(path4)
dir.create(path4)
##### 4. general model (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path4)

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- glm(RHINITIS_in_total ~
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
                  ,	family = quasipoisson(), df); 

    test <- glm(RHINITIS_in_total ~ 1, data=df, family=quasipoisson())          
    with(summary(test), null.deviance)
    summary(test)

    logLik(ns_lag_no)
    logLik(ns_lag_no.init)
    summary(ns_lag_no)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 36)/null.deviance), digits = 3)

    with(summary(ns_lag_no), deviance)
    with(summary(ns_lag_no), null.deviance)

    logLik(ns_lag_no)

    r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)
    PseudoR2(r.glm)
    PseudoR2(r.glm, c("McFadden", "Nagel"))
    
    pR2(ns_lag_no)['McFadden']

    # anova(ns_lag_no)
    # 1-(54961943/100686999)
    PseudoR2(ns_lag_no, c("McFaddenAdj"))

    # crosspred
    percentiles_no <- round(quantile(df$NO2_mean,c(0.05,0.25,0.75,0.95),na.rm=TRUE), digits=4); percentiles_no
    ns.pred_no <- crosspred(ns.basis_no,ns_lag_no,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1])

    summary(ns.pred_no)
    with(summary(ns.pred_no), 1 - (deviance - 12)/null.deviance)

    low = round(ns.pred_no$allRRlow[length(ns.pred_no$allRRfit)], digits = 2)
    mid = round(ns.pred_no$allRRfit[length(ns.pred_no$allRRfit)], digits = 2)
    high = round(ns.pred_no$allRRhigh[length(ns.pred_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

    # draw plot and save
    png(filename="NO2_mean.png")
    plot(ns.pred_no,zlab="RR",xlab="NO2_mean")
    dev.off()
    png(filename="NO2_mean_overall.png")
    plot(ns.pred_no,"overall",xlab="NO2_mean")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(RHINITIS_in_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)
    
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(RHINITIS_in_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(RHINITIS_in_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(RHINITIS_in_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

  # # 4-6. ns.basis_temp_mean
  #   # fit
  #   ns_lag_temp <- glm(RHINITIS_in_total ~ 
  #                 # + ns.basis_temp_tc
  #                 + ns.basis_temp_mean
  #                  #+ ns.basis_temp_max
  #                 + ns.basis_humi 
                  
  #                 # + ns.basis_temp_min
  #                 + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
  #                 + factor(day) + ns(doy,4)
  #                 # + esf_em
  #                 ,	family=quasipoisson(), df); 
  #   summary(ns_lag_temp)

  #   # crosspred
  #   percentiles_temp <- round(quantile(df$temp_mean_total,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_temp
  #   ns.pred_temp <- crosspred(ns.basis_temp_mean,ns_lag_temp,at=seq(percentiles_temp[1],percentiles_temp[4], 0.1),cen = percentiles_temp[1])

  #   # # # crosspred
  #   #  percentiles_tempmax <- round(quantile(df$temp_max,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_tempmax
  #   #  ns.pred_tempmax <- crosspred(ns.basis_temp_max,ns_lag_temp,at=seq(percentiles_tempmax[1],percentiles_tempmax[4], 0.1),cen = percentiles_tempmax[1])

  #   # # crosspred
  #   # percentiles_tempmin <- round(quantile(df$temp_min,c(0.05,0.25,0.75,0.95)),digits=4) ; percentiles_tempmin
  #   # ns.pred_tempmin <- crosspred(ns.basis_temp_min,ns_lag_temp,at=seq(percentiles_tempmin[1],percentiles_tempmin[4], 0.1),cen = percentiles_tempmin[1])

  #   # draw plot and save
  #   png(filename="Temp_mean_overall.png")
  #   plot(ns.pred_temp,"overall",xlab="Temp_mean")
  #   dev.off()

  #   # # draw plot and save
  #   # png(filename="Temp_max_overall.png")
  #   # plot(ns.pred_tempmax,"overall",xlab="Temp_max")
  #   # dev.off()

  #   #  # draw plot and save
  #   # png(filename="Temp_min_overall.png")
  #   # plot(ns.pred_tempmin,"overall",xlab="Temp_min")
  #   # dev.off()

  # ##### ns.basis_temp_mean end. #####

  # # 4-7. ns.basis_humi
  #   # fit
  #   ns_lag_humi <- glm(RHINITIS_in_total ~ 
  #                 # + ns.basis_temp_tc
  #                 + ns.basis_temp_mean
  #                 + ns.basis_humi 
                  
  #                 # + ns.basis_temp_min
  #                 + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
  #                 + factor(day) + ns(doy,4)
  #                 # + esf_em
  #                 ,	family=quasipoisson(), df); 
  #   summary(ns_lag_humi)

  #   # crosspred
  #   percentiles_humi <- round(quantile(df$humi_mean_total,c(0.01,0.25,0.75,0.95)),digits=4) ; percentiles_humi
  #   ns.pred_humi <- crosspred(ns.basis_humi,ns_lag_humi,at=seq(percentiles_humi[1],percentiles_humi[4], 0.1),cen = percentiles_humi[1])

  #   # draw plot and save
  #   png(filename="Humidity_mean_overall.png")
  #   plot(ns.pred_humi,"overall",xlab="Humidity_mean")
  #   dev.off()
  # ##### ns.basis_humi end. #####

##### general model (ESF X) end. #####
######################################

path5 = paste0(path, '\\2. ESF model')
unlink(path5)
dir.create(path5)
##### 5. create df_esf data & fit model with ESF #####

  setwd(path5)
  # 5-1. ns.basis_no
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon) 
    
    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm)
    summary(esf.ccm)$adj.r.squared
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
    dim(esf)

    # fit
    ns_lag3_esf <- glm(RHINITIS_in_total ~  
                  ns.basis_no
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    logLik(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    
    # anova(ns_lag3_esf)

    # table(df_esf$RHINITIS_in_total)
    # mean(df_esf$RHINITIS_in_total)
    # poi_test=rpois(n=sum(df_esf$RHINITIS_in_total),lambda=0.1531)
    # table(poi_test)   

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)


    # crosspred
    ns.pred2_no <- crosspred(ns.basis_no,ns_lag3_esf,at=seq(percentiles_no[1],percentiles_no[4], 0.001),cen = percentiles_no[1]);
    
    low = round(ns.pred2_no$allRRlow[length(ns.pred2_no$allRRfit)], digits = 2)
    mid = round(ns.pred2_no$allRRfit[length(ns.pred2_no$allRRfit)], digits = 2)
    high = round(ns.pred2_no$allRRhigh[length(ns.pred2_no$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="NO2_mean_esf.png")
    plot(ns.pred2_no,zlab="RR",xlab="NO2_mean_esf")
    dev.off()
    png(filename="NO2_mean_esf_overall.png")
    plot(ns.pred2_no,"overall",xlab="NO2_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 5-2. ns.basis_so
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(RHINITIS_in_total ~  
                  ns.basis_so
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)
    # anova(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_so <- crosspred(ns.basis_so,ns_lag3_esf,at=seq(percentiles_so[1],percentiles_so[4], 0.0001),cen = percentiles_so[1]);
    
    low = round(ns.pred2_so$allRRlow[length(ns.pred2_so$allRRfit)], digits = 2)
    mid = round(ns.pred2_so$allRRfit[length(ns.pred2_so$allRRfit)], digits = 2)
    high = round(ns.pred2_so$allRRhigh[length(ns.pred2_so$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="SO2_mean_esf.png")
    plot(ns.pred2_so,zlab="RR",xlab="SO2_mean_esf")
    dev.off()
    png(filename="SO2_mean_esf_overall.png")
    plot(ns.pred2_so,"overall",xlab="SO2_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_so end. #####

  # 5-3. ns.basis_co
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(RHINITIS_in_total ~  
                  ns.basis_co
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_co <- crosspred(ns.basis_co,ns_lag3_esf,at=seq(percentiles_co[1],percentiles_co[4], 0.01),cen = percentiles_co[1]);
    
    low = round(ns.pred2_co$allRRlow[length(ns.pred2_co$allRRfit)], digits = 2)
    mid = round(ns.pred2_co$allRRfit[length(ns.pred2_co$allRRfit)], digits = 2)
    high = round(ns.pred2_co$allRRhigh[length(ns.pred2_co$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="CO_mean_esf.png")
    plot(ns.pred2_co,zlab="RR",xlab="CO_mean_esf")
    dev.off()
    png(filename="CO_mean_esf_overall.png")
    plot(ns.pred2_co,"overall",xlab="CO_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_co end. #####

  # 5-4. ns.basis_o3
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(RHINITIS_in_total ~  
                  ns.basis_o3
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_o3 <- crosspred(ns.basis_o3,ns_lag3_esf,at=seq(percentiles_o3[1],percentiles_o3[4], 0.001),cen = percentiles_o3[1]);
    
    low = round(ns.pred2_o3$allRRlow[length(ns.pred2_o3$allRRfit)], digits = 2)
    mid = round(ns.pred2_o3$allRRfit[length(ns.pred2_o3$allRRfit)], digits = 2)
    high = round(ns.pred2_o3$allRRhigh[length(ns.pred2_o3$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="O3_mean_esf.png")
    plot(ns.pred2_o3,zlab="RR",xlab="O3_mean_esf")
    dev.off()
    png(filename="O3_mean_esf_overall.png")
    plot(ns.pred2_o3,"overall",xlab="O3_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_o3 end. #####

  # 5-5. ns.basis_pm
    # create df_esf (ns_lag_ 모델 이름만 바뀌면 됨)
    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    head(test)
    dim(test)
    Moran.I(test$x,weight=C_sudogwon)

    col.poly_sudogwon$One_to_N=c(1:72)
    test$Group.1[col.poly_sudogwon$One_to_N]
    E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
    colnames(E_re_sudogwon2)[73]="rsum_asthma"
    lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
    lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
    esf.ccm<-step(lm.full,direction="backward",verbose=T)
    dim(summary(esf.ccm)$coefficients)
    summary(esf.ccm$fitted.values)
    esf=as.data.frame(esf.ccm$fitted.values)
    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
    colnames(esf)=c("esf_em", "air_out_idx")
    df_esf <- merge(df,esf,by=c("air_out_idx"))
    dim(esf)

    # fit
    ns_lag3_esf <- glm(RHINITIS_in_total ~  
                  ns.basis_pm
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                    + esf_em
                  ,	family=quasipoisson(), df_esf); 
    summary(ns_lag3_esf)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag3_esf), 1 - (deviance - 30)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

    # check Moran's I
    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
    Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

    # crosspred
    ns.pred2_pm <- crosspred(ns.basis_pm,ns_lag3_esf,at=seq(percentiles_pm[1],percentiles_pm[4], 3),cen = percentiles_pm[1]);
    
    low = round(ns.pred2_pm$allRRlow[length(ns.pred2_pm$allRRfit)], digits = 2)
    mid = round(ns.pred2_pm$allRRfit[length(ns.pred2_pm$allRRfit)], digits = 2)
    high = round(ns.pred2_pm$allRRhigh[length(ns.pred2_pm$allRRfit)], digits = 2)

    RR <- paste0("RR: ", mid, " (", low, "-", high, ")")

    # draw plot and save
    png(filename="PM10_mean_esf.png")
    plot(ns.pred2_pm,zlab="RR",xlab="PM10_mean_esf")
    dev.off()
    png(filename="PM10_mean_esf_overall.png")
    plot(ns.pred2_pm,"overall",xlab="PM10_mean_esf")
    legend("topright", legend = RR, box.lty = 0, bg='transparent', cex = 0.8)
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
  ##### ns.basis_pm end. #####

##### create df_esf data & fit model with ESF end. #####
#############################################################

path6 = paste0(path, '\\3. fixed effect model (ESF X)')
unlink(path6)
dir.create(path6)
##### 6. model with fixed effect (ESF X) #####
  # set path to folder where plots to be saved
  setwd(path6)

  # 4-1. ns.basis_no
    # fit
    ns_lag_no <- glm(RHINITIS_in_total ~
                  ns.basis_no
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  #+ ns.basis_temp_min
                  #+ ns.basis_temp_max
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(day) 
                  + factor(air_out_idx)
                  + ns(doy,4)
                  # + esf_em
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 107)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 4-2. ns.basis_so
    # fit
    ns_lag_so <- glm(RHINITIS_in_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 107)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 4-3. ns.basis_co
    # fit
    ns_lag_co <- glm(RHINITIS_in_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 107)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 4-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(RHINITIS_in_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 107)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 4-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(RHINITIS_in_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 107)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()
    
  ##### ns.basis_pm end. #####

##### model with fixed effect (ESF X) end. #####
######################################

path7 = paste0(path, '\\4. offset model (ESF X)')
unlink(path7)
dir.create(path7)
##### 7. model with offset (ESF X) #####

    setwd(path7)
    # lag 추가
    df = df%>%
        group_by(air_out_idx)%>%
        mutate(RHINITIS_in_total_lag7=lag(RHINITIS_in_total,7))%>%
        mutate(RHINITIS_in_total_lag14=lag(RHINITIS_in_total,14))
    dim(df)

    # 7일 제거
    df <- na.omit(df)
    table(df$RHINITIS_in_total_lag7)
    dim(df)
    # offset(log(df$RHINITIS_in_total_lag7))

    # outcome_lag 변수들에 + 1 (for log)
    df$RHINITIS_in_total_lag7 = df$RHINITIS_in_total_lag7 + 1
    df$RHINITIS_in_total_lag14 = df$RHINITIS_in_total_lag14 + 1

  # 6-1. ns.basis_no
    # fit
    ns_lag_no <- glm(RHINITIS_in_total ~
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
                  + offset(log(RHINITIS_in_total_lag7))
                  ,	family = quasipoisson(), df); 
    
    summary(ns_lag_no)
    # anova(ns_lag_no)
    # 1-(54961943/100686999)

    test = aggregate(ns_lag_no$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    logLik(ns_lag_no)
    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_no), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_no end. #####

  # 6-2. ns.basis_so
    # fit
    ns_lag_so <- glm(RHINITIS_in_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(RHINITIS_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_so)

    test = aggregate(ns_lag_so$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_so), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_so end. #####

  # 6-3. ns.basis_co
    # fit
    ns_lag_co <- glm(RHINITIS_in_total ~ 
                  ns.basis_co
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(RHINITIS_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_co)

    test = aggregate(ns_lag_co$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_co), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_co end. #####

  # 6-4. ns.basis_o3
    # fit
    ns_lag_o3 <- glm(RHINITIS_in_total ~ 
                  ns.basis_o3
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) +ns(doy,4)
                  # + esf_em
                  + offset(log(RHINITIS_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_o3)

    test = aggregate(ns_lag_o3$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_o3), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

  ##### ns.basis_o3 end. #####

  # 6-5. ns.basis_pm
    # fit
    ns_lag_pm <- glm(RHINITIS_in_total ~ 
                  ns.basis_pm
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  # + factor(air_out_idx)
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  + offset(log(RHINITIS_in_total_lag7))
                  ,	family=quasipoisson(), df); 
    summary(ns_lag_pm)
    # anova(ns_lag_pm)

    test = aggregate(ns_lag_pm$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
    Moran.I(test$x,weight=C_sudogwon) 

    #calculate McFadden's R-squared for model
    adjMFr2 <- round(with(summary(ns_lag_pm), 1 - (deviance - 36)/null.deviance), digits = 3)
    adjMFr2 <- paste0("McFadden's adjusted R2: ", adjMFr2)
    adjMFr2

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
    legend("topleft", legend = adjMFr2, box.lty = 0, bg='transparent', cex = 0.8)
    dev.off()

##### model with offset (ESF X) end. #####
######################################