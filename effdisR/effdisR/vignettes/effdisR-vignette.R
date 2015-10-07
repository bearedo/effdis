## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5, fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)

## ----attach libraries, include =TRUE, echo=T, message=FALSE,error=FALSE,warning=FALSE----
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(mgcv)
library(maps)
library(mapdata)
library(reshape2)
library(rgeos)
library(lattice)
library(pander)
library(kfigr)
library(doMC)

## ----install effdis R library,eval=FALSE,include=TRUE,echo=TRUE----------
#  install.packages('/path/to/package', repos = NULL, type="source")

## ----load effdis R library,eval=TRUE,include=TRUE,echo=TRUE--------------
library(effdisR)

## ----connect to db,include=TRUE,echo=TRUE,eval=TRUE----------------------
chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)

## ----get ll data for five flags,include=TRUE,echo=TRUE,fig.height=5------
ll <- sqlQuery(chan,"SELECT yearc AS year, trend, timeperiodid AS month, 
flagname, region, geargrpcode,longitude,latitude, catchunit, dsettype, eff1, eff1type
FROM t2ce
WHERE region ='AT' AND timeperiodid < 13 AND eff1type='NO.HOOKS' AND geargrpcode = 'LL' 
AND flagname IN ('Japan') 
AND catchunit != '--' ;")

## ----temporal confounding Japan,include=TRUE,echo=TRUE-------------------
par(mfrow=c(1,1),mar=c(2,2,2,2),oma=c(1,1,1,1))
yr.month.coverage.task2(tdata=ll,which.gear='LL',
              start.year=1960,end.year=2010,which.flag='Japan')

## ----spatial confounding Japan, include=TRUE,echo=TRUE-------------------
par(mfrow=c(1,1),mar=c(2,2,2,2))
spatial.coverage.by.year.task2(tdata=ll,
        start.year=2000,end.year=2000,which.gear='LL',which.flag='Japan')

## ----no of hooks shot by Japan in 2006 from task 2,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
par(mfrow=c(1,1))
three.d.effort.by.year(tdata=ll,what.year=2006,
                         what.flag='Japan',scaling.f=10000,effort.type='NO.HOOKS')  

## ----extract kg tunas caught by Japan in 2009 from task 2,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
alb <- sqlQuery(chan,"SELECT yearc AS year, trend, 
timeperiodid AS month, flagname, region, geargrpcode,longitude,latitude, 
catchunit, dsettype, eff1, eff1type, alb as measured_catch
FROM t2ce
WHERE yearc = 2009 AND region ='AT' AND timeperiodid < 13 AND eff1type IN ('NO.HOOKS') AND 
geargrpcode IN ('LL') AND flagname IN ('Japan') AND catchunit != '--' ;")
alb$species <- 'alb'

## ----weight of albacore caught by location in 2006 from task 2,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
three.d.catch.by.year(tdata=alb,what.year=2009,what.gear='LL',what.species='alb',what.flag='Japan',scaling.f=10,catchunit='nr')

## ----get japanese task 2 longline data,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  ll_n  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',
#                                which.gear='LL',which.flag='All',which.dsettype = 'n-')
#  ll_nw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',
#                                which.gear='LL',which.flag='All',which.dsettype = 'nw')
#  ll_w  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',
#                                which.gear='LL',which.flag='All',which.dsettype = '-w')
#  long_line <- rbind(ll_n,ll_nw,ll_w)

## ----identify data in Atlantic,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  long_line<-find.ocean(long_line)
#  long_line <- long_line[long_line$which.ocean == 'atl',]

## ----get Japanese task 2 longline data,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  long_line<-prepare.effdis.data(input=long_line)

## ----Convert data to long format,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  long_line_lf <- convert2long.format.t2(input =long_line)

## ----model weights as a function of numbers,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  bm <- model.nos.kgs(input=long_line_lf,which.gear='LL')
#  long_line_lf <- kgs.from.nos(long_line_lf)

## ----model catches as a function of space and time,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  j_bft_ll <- fit2stageGAMtoCatch(input=long_line_lf,
#                                    which.flag='Japan',which.species='bft',start.year=1990,end.year=2010)
#  j_emod_ll <- fitGAMtoEffort(input=long_line_lf,
#                                which.flag='Japan',which.effort='NO.HOOKS',start.year=1990,end.year=2010)

## ----predict catches over grid,eval=FALSE,include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  j_bft.ll.pred <- predict.effdis.t2.data(cmod=j_bft_ll,
#                                            effmod=j_emod_ll,grid.res=5,
#                                            start.year=1990,end.year=2010,which.flag='Japan')

## ----load predictions for Japanese bluefin long-line,eval=TRUE,include=FALSE,echo=FALSE,message = FALSE, error=FALSE, warnings=FALSE----
load('/home/doug/effdis/data/j_bft_ll.RData')
load('/home/doug/effdis/data/j_bft.ll.pred.RData')

## ----plot P over grid,eval=TRUE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
par(mfrow=c(1,1))
plot.mods(input=j_bft.ll.pred,cmod=j_bft_ll,
            which.year = 1995,which.month=1,which.value = 'prob',grid.res=5,which.gear='LL')

## ----get task 1 data,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  long_line.t1 <- get.effdis.t1.data(which.dsn='effdis-tuna-cc1',
#                                       which.gear = 'LL',which.region='AT',which.flag='Japan')
#  long_line.sum.t1 <- aggregate(list(qty_t=long_line.t1$qty_t),
#                                list(year=long_line.t1$yearc),sum,na.rm=T)

## ----bind task 2 estimates ,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  model.data <- rbind(j_alb.ll.pred,j_bft.ll.pred,j_bet.ll.pred,
#                      j_bum.ll.pred,j_skj.ll.pred,j_yft.ll.pred,
#                      j_swo.ll.pred,j_sai.ll.pred,j_whm.ll)

## ----block out unsampled grids ,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  model.data$catch[big$observation == FALSE] <- NA
#  model.data$prob[big$observation == FALSE] <- NA
#  model.data$measured_catch[big$observation == FALSE] <- NA
#  model.data$eff[big$observation == FALSE]  <- NA

## ----convert task 2 to tonnes ,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  model.data$catch <- model.data$catch/1000

## ----sum modeled task 2 over year ,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  model.data.totals <- aggregate(list(catch=model.data$catch,catch=model.data$catch,eff=model.data$eff),
#                    by=list(year=model.data$year),sum,na.rm=T)

## ----merge task 1 and task 2 totals,eval=FALSE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
#  t1.t2.merged <- merge(model.data.totals,long_line.sum.t1)
#  t1.t2.merged$cpue <- t1.t2.merged$catch/t1.t2.merged$eff
#  t1.t2.merged$raised.effort <- t1.t2.merged$qty_t/t1.t2.merged$cpue

## ----plot estimate for Japan 1990-2010,eval=TRUE, include=TRUE,echo=TRUE,message = FALSE, error=FALSE, warnings=FALSE----
data("japan-effdis-estimates")
par(mfrow=c(1,1),mar=c(5,5,4,4))
plot(effdis$year,effdis$raised.effort/1000000,xlab='',ylab='hooks',type='l',lwd=3,xlim=c(1990,2010))
abline(v=seq(1960,2010,by=5),lty=2,col='blue')
title('Task 1 Catch / Task 2 CPUE for Japanese long-liners')

