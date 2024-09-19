

# find best parameter function

find_best_param_NO2 <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {
    
    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) {
            stop("too much iteration")
        }
        return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N - 1
                if (N == 0) {
                    stop("too much iteration")
                }
                return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N - 1
                    if (N == 0) {
                        stop("too much iteration")
                    }
                    return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_no
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed <= 0.15) {
                        if (esf_added_MI$observed < mi_threshold) {
                            print("Moran's I is under %s! Value is: ", mi_threshold)
                            print(esf_added_MI$observed)
                            print(param_index)
                            return (param_index)
                        }
                        else {
                            print("Moran's I is under 0.15 BUT not enough ... Value is: ")
                            print(esf_added_MI$observed)
                            print(param_index)
                            N = N - 1
                            if (N == 0) {
                                stop("too much iteration")
                            }
                            return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                        }
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        print(param_index)
                        N = N - 1
                        if (N == 0) {
                            stop("too much iteration")
                        }
                        return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                    }
                }
            }
        )  
    }
}

find_best_param_NO2_ses <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {
    
    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) {
            stop("too much iteration")
        }
        return (find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N - 1
                if (N == 0) {
                    stop("too much iteration")
                }
                return (find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N - 1
                    if (N == 0) {
                        stop("too much iteration")
                    }
                    return (find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_no
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                + 요양기관.수 + 재정자립도.세입과목개편후. 
                                # + 도시인구.용도지역기준.
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed <= 0.15) {
                        if (esf_added_MI$observed < mi_threshold) {
                            print("Moran's I is under %s! Value is: ", mi_threshold)
                            print(esf_added_MI$observed)
                            print(param_index)
                            return (param_index)
                        }
                        else {
                            print("Moran's I is under 0.15 BUT not enough ... Value is: ")
                            print(esf_added_MI$observed)
                            print(param_index)
                            N = N - 1
                            if (N == 0) {
                                stop("too much iteration")
                            }
                            return (find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                        }
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        # print(param_index)
                        N = N - 1
                        if (N == 0) {
                            stop("too much iteration")
                        }
                        return (find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                    }
                }
            }
        )  
    }
}

find_best_param_SO2 <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {

    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) { stop("too much iteration")}
        return (find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N-1
                if (N == 0) { stop("too much iteration")}
                return (find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N-1
                    if (N == 0) { stop("too much iteration")}
                    return (find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_so
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed <= 0.15) {
                        if (esf_added_MI$observed < mi_threshold) {
                            print("Moran's I is under %s! Value is: ", mi_threshold)
                            print(esf_added_MI$observed)
                            print(param_index)
                            return (param_index)
                        }
                        else {
                            print("Moran's I is under 0.15 BUT not enough ... Value is: ")
                            print(esf_added_MI$observed)
                            print(param_index)
                            N = N - 1
                            if (N == 0) {
                                stop("too much iteration")
                            }
                            return (find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                        }
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        print(param_index)
                        N = N-1
                        if (N == 0) { stop("too much iteration")}
                        return (find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                    }
                }
            }
        )  
    }
}

find_best_param_SO2_ses <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {

    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) { stop("too much iteration")}
        return (find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N-1
                if (N == 0) { stop("too much iteration")}
                return (find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N-1
                    if (N == 0) { stop("too much iteration")}
                    return (find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_so
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                + 요양기관.수 + 재정자립도.세입과목개편후. 
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed <= 0.15) {
                        if (esf_added_MI$observed < mi_threshold) {
                            print("Moran's I is under %s! Value is: ", mi_threshold)
                            print(esf_added_MI$observed)
                            print(param_index)
                            return (param_index)
                        }
                        else {
                            print("Moran's I is under 0.15 BUT not enough ... Value is: ")
                            print(esf_added_MI$observed)
                            print(param_index)
                            N = N - 1
                            if (N == 0) {
                                stop("too much iteration")
                            }
                            return (find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                        }
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        print(param_index)
                        N = N-1
                        if (N == 0) { stop("too much iteration")}
                        return (find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                    }
                }
            }
        )  
    }
}

find_best_param_CO <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {

    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) { stop("too much iteration")}
        return (find_best_param_CO(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N-1
                if (N == 0) { stop("too much iteration")}
                return (find_best_param_CO(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N-1
                    if (N == 0) { stop("too much iteration")}
                    return (find_best_param_CO(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_co
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed < mi_threshold) {
                        print("Moran's I is under %s! Value is: ", mi_threshold)
                        print(esf_added_MI$observed)
                        return (param_index)
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        N = N-1
                        if (N == 0) { stop("too much iteration")}
                        return (find_best_param_CO(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                    }
                }
            }
        )  
    }
}

find_best_param_PM <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {

    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        N = N-1
        if (N == 0) { stop("too much iteration")}
        find_best_param_PM(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N)
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                N = N-1
                if (N == 0) { stop("too much iteration")}
                find_best_param_PM(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N)
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    N = N-1
                    if (N == 0) { stop("too much iteration")}
                    find_best_param_PM(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N)
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_pm
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed <= 0.15) {
                        if (esf_added_MI$observed < mi_threshold) {
                            print("Moran's I is under %s! Value is: ", mi_threshold)
                            print(esf_added_MI$observed)
                            print(param_index)
                            return (param_index)
                        }
                        else {
                            print("Moran's I is under 0.15 BUT not enough ... Value is: ")
                            print(esf_added_MI$observed)
                            print(param_index)
                            N = N - 1
                            if (N == 0) {
                                stop("too much iteration")
                            }
                            return (find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N))
                        }
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        N = N-1
                        if (N == 0) { stop("too much iteration")}
                        find_best_param_PM(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N)
                    }
                }
            }
        )  
    }
}

find_best_param_O3 <- function(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon, N) {

    X_all <- as.matrix(cbind(1, 1:72, spring_mean[,c(2:8)], summer_mean[c(2:8)], fall_mean[c(2:8)], winter_mean[,c(2:8)]))
    N_param <- sample(4:30, 1)
    param_index <- sample(1:30, N_param)
    X <- X_all[,param_index]

    if (det(crossprod(X)) < 0.01) {
        print("det(crossprod(X)) is too small!")
        find_best_param_O3(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon)
    }
    else {
        print("det(crossprod(X)) is large enough to proceed")
        M_sudogwon <- diag(1,n_sudogwon)-tcrossprod(X%*%qr.solve(crossprod(X)),X)
        MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
        eig_sudogwon <- eigen(MCM_sudogwon)
        E_sudogwon <- eigen(MCM_sudogwon)$vectors
        eigenvalues_sudogwon=c(Re(eig_sudogwon$values))
        E_re_sudogwon=as.data.frame(Re(eig_sudogwon$vectors))

        test = aggregate(pollutant$residuals,by=list(df$air_out_idx),mean) # residual을 지역별로 aggregate 해서 평균냄
        Moran.I(test$x,weight=C_sudogwon) 
    
        col.poly_sudogwon$One_to_N=c(1:72)
        test$Group.1[col.poly_sudogwon$One_to_N]
        E_re_sudogwon2=cbind(E_re_sudogwon,test$x[col.poly_sudogwon$One_to_N])
        colnames(E_re_sudogwon2)[73]="rsum_asthma"
        lm.init <- lm(rsum_asthma ~ 1, data=E_re_sudogwon2) 
        lm.full <- lm(rsum_asthma ~ ., data=E_re_sudogwon2) 
        # print(extractAIC(step(lm.full,direction="backward",verbose=T)))

        tryCatch(
            expr = {
                esf.ccm<-step(lm.full,direction="backward",verbose=T, trace = 0)
                print("step function conducted successfully")
            },
            error = function(e) {
                print("AIC is -infinity!")
                find_best_param_O3(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon)
            },
            finally = {
                print("trycatch is finished")

                if (summary(esf.ccm)$adj.r.squared < 0.8) {
                    print("Adjusted R squared is under 0.8! Proceed again")
                    find_best_param_O3(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon)
                }
                else {
                    print("Adjusted R squared is over 0.8! Value is: ")
                    print(summary(esf.ccm)$adj.r.squared)
                    # return (X)

                    esf=as.data.frame(esf.ccm$fitted.values)
                    esf$air_out_idx=as.numeric(col.poly_sudogwon$OBJECTID)
                    colnames(esf)=c("esf_em", "air_out_idx")
                    df_esf <- merge(df,esf,by=c("air_out_idx"))
    
                    dim(esf)

                    # fit
                    ns_lag3_esf <- glm(RHINITIS_in_total ~ 
                                ns.basis_o3
                                # + ns.basis_temp_mean
                                # + ns.basis_humi 
                  
                                # + ns.basis_temp_min
                                + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                                # + factor(air_out_idx)
                                + factor(day) + ns(doy,4)
                                + esf_em
                                ,	family=quasipoisson(), df_esf); 

                    # check Moran's I
                    test2=aggregate(ns_lag3_esf$residuals,by=list(df_esf$air_out_idx),mean)
                    Moran.I(test$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)
                    esf_added_MI <- Moran.I(test2$x[col.poly_sudogwon$One_to_N],weight=C_sudogwon)

                    if (esf_added_MI$observed < mi_threshold) {
                        print("Moran's I is under %s! Value is: ", mi_threshold)
                        print(esf_added_MI$observed)
                        return (param_index)
                    }
                    else {
                        print("Moran's I is too big! Proceed again")
                        print(esf_added_MI$observed)
                        find_best_param_O3(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, pollutant, df, outcome, C_sudogwon, col.poly_sudogwon)
                    }
                }
            }
        )  
    }
}

# mi_threshold 설정
mi_threshold = 0.07
N = 100

# NO2
    param_index <- find_best_param_NO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_no, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# NO2 - ses
    param_index <- find_best_param_NO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_no, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# SO2
    param_index <- find_best_param_SO2(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_so, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# SO2 - ses
    param_index <- find_best_param_SO2_ses(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_so, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# CO
    param_index <- find_best_param_CO(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_co, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# PM10
    param_index <- find_best_param_PM(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_pm, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)

# O3
    param_index <- find_best_param_O3(mi_threshold, spring_mean, summer_mean, fall_mean, winter_mean, ns_lag_o3, df, "RHINITIS_in_total", C_sudogwon, col.poly_sudogwon, N)





## test
AIC(step(lm.full,direction="backward",verbose=T))

if (is.null(AIC(step(lm.full,direction="backward",verbose=T))) == FALSE) {
    print("1")
}


tryCatch(
    expr = {
        # 1 + "1"
        print("everything was fine")
    },
    error = function(e) {
        print("error!")
    },
    finally = {
        print("trycatch is finished")
    }
)
