
# rm(list=ls())
####  step 0. 라이브러리 불러오기 ########################################################
# install.packages(c("spdep","lmtest","sm","maptools","rgdal","sf","haven"))
# install.packages("Rcpp")
# install.packages("MCMCglmm")
#setwd("C:/Users/USER/Downloads/#ASTHMA_FINAL")
setwd("D:\\SNUlab\\data\\out of hospital sudden cardiac arrest")
library(spdep)
library(lmtest)
library(sm)
library(maptools)
library(rgdal)
library(sf)

library(haven)
library(dplyr)
library(TTR)
library(Rcpp)
##########################################################################################



#### step 1.shp 파일을 불러와서 시각화하는 function 짜기 #################################
plot.map <- function(theme, poly, color=NULL, ncl=9,main=NULL){
  require(RColorBrewer); require(maptools); require(classInt)
  if(is.null(color)) color <- "YlGnBu"
  int <- classIntervals(theme,n=ncl,style="quantile")$brks
  pal <- brewer.pal(ncl,color)
  cols <- pal[findInterval(theme,int,rightmost.closed=T)]
  plot(poly,col=cols)
  title(main=main)
}
##########################################################################################



#### step 2-1.shp 파일 불러오기 ##########################################################
#수도권
# 01.14 수도권 75개 인덱스 있는 지도 파일로 수정함
col.poly_sudogwon <- st_read("Z_SOP_BND_SIGUNGU_sudogwon_75\\Z_SOP_BND_SIGUNGU_sudogwon_75_2.shp", options = "ENCODING=UTF-8")
col.data_sudogwon <- col.poly_sudogwon$geometry
##########################################################################################

col.poly_sudogwon$Index
col.poly_sudogwon$One_to_N

#### step 2-2.인접 지역 가중치 주기 ######################################################
#수도권

sf::sf_use_s2(FALSE)
col.nb_sudogwon <- poly2nb(col.poly_sudogwon$geometry,queen=F)
col.listw_sudogwon <- nb2listw(col.nb_sudogwon,style="C")

# plot(col.poly_sudogwon,col="gray",border="white")
# coords2 <- coordinates(col.poly_sudogwon)
# plot(col.nb_sudogwon,coords2,add=T)
##########################################################################################




#### step 5. 일련의 가상의 잠재적 공간 패턴을 생성 #######################################
#수도권l.nb_sudogwon)
n_sudogwon=75  # 01.14 75로 수정 

C_sudogwon <- listw2mat(col.listw_sudogwon)
M_sudogwon <- diag(1,n_sudogwon)-1/n_sudogwon
MCM_sudogwon <- M_sudogwon%*%C_sudogwon%*%M_sudogwon
eig_sudogwon <- eigen(MCM_sudogwon)
E_sudogwon <- eigen(MCM_sudogwon)$vectors
##########################################################################################
