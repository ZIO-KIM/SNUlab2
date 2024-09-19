

# install.packages('spfilteR')

library(spfilteR)

data("fakedata")

# take a look at the connectivity matrix and the variables
dim(W)

head(fakedataset)
tail(fakedataset)
colnames(fakedataset)
y.count <- fakedataset$count
length(y.count)
head(W)

# NO2
ols.resid <- resid(glm(ASTHMA_em_total ~
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
                  ,	family = quasipoisson(), df))

# residual을 지역별로 aggregate 해서 평균냄
test = aggregate(ols.resid,by=list(df$air_out_idx),mean)

# Moran test of residual spatial autocorrelation
MI.resid(resid = test$x, W = C_sudogwon, alternative = 'greater')

# ESF poisson model
y.asthma <- df$ASTHMA_em_total
poisson.esf <- glmFilter(y = y.asthma, x = NULL, W = E_re_sudogwon, objfn = 'BIC', model = 'poisson',
                          optim.method = 'BFGS', positive = TRUE, ideal.setsize = FALSE,
                          alpha = .25, resid.type = 'deviance', boot.MI = 100)

dim(E_re_sudogwon)
length(y.asthma)
tail(df)
tmp <- getEVs(W = C_sudogwon)
summary(tmp)
esf.ccm.test <- step(lm.full,direction="backward",verbose=T)


y.count <- fakedataset$count
poisson.esf <- glmFilter(y = y.count, x = NULL, W = W, objfn = 'BIC', model = 'poisson',
                          optim.method = 'BFGS', positive = TRUE, ideal.setsize = FALSE,
                          alpha = .25, resid.type = 'deviance', boot.MI = 100)
summary(poisson.esf)
length(y.count)



# SO2 
ols.resid <- resid(glm(ASTHMA_em_total ~ 
                  ns.basis_so
                  # + ns.basis_temp_tc
                  + ns.basis_temp_mean
                  + ns.basis_humi 
                  
                  # + ns.basis_temp_min
                  + ns(temp_mean_total, df=4) + ns(humi_mean_total, df=4)
                  
                  + factor(day) + ns(doy,4)
                  # + esf_em
                  ,	family=quasipoisson(), df))

# residual을 지역별로 aggregate 해서 평균냄
test = aggregate(ols.resid,by=list(df$air_out_idx),mean)

# Moran test of residual spatial autocorrelation
MI.resid(resid = test$x, W = C_sudogwon, alternative = 'greater')
