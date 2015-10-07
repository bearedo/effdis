
# Load libraries

#library(rio)
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(gam)
library(maps)
library(mapdata)
#library(COZIGAM)
library(RODBC)
library(reshape2)
library(mgcv)
library(doMC)
library(effdisR)


#########################
## Purse-seiners example ##
#########################

# Purse-seine example #


# Get data for each dsettype

psn  <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = 'n-')
psnw <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = 'nw')
psw  <- get.effdis.t2.data.r(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = '-w')

ps1 <- rbind(psn,psnw,psw)

table(ps1$squaretypecode,ps1$flagname)

#t2 <- tapply(ll1$totsp9,list(ll1$flagname,ll1$year),sum,na.rm=T)

ps1<-find.ocean.r(ps1)
ps1 <- ps1[ps1$which.ocean == 'atl',]
ps1<-prepare.effdis.data.r(input=ps1)
ps1<-ps1[ps1$squaretypecode == '1x1',]

library(reshape2)
pslf <- convert2long.format.t2.r(input =ps1)

#bm <- model.nos.kgs.r(input=pslf,which.gear='PS')

#pslf <- kgs.from.nos.r(pslf) # Not relevant for PS 

table(pslf$eff1type)

# D.AT SEA    D.FISH  NO.BOATS    -none- FISH.HOUR HOURS.SEA   NO.SETS  NO.TRIPS  SUC.D.FI  SUC.SETS 
# 2457    643176       297      2376    925920         0     28008       423         9        72 

pslf <- pslf[pslf$eff1type %in% c('D.FISH','FISH.HOUR'),]
pslf$eff1[pslf$eff1type == 'D.FISH'] <- pslf$eff1[pslf$eff1type == 'D.FISH']*24
pslf$eff1type <- 'FISH.HOUR'

w0 <- (1:length(pslf$year))[pslf$catchunit == 'kg']

round(tapply(pslf$measured_catch[w0],list(pslf$flagname[w0],pslf$species[w0]),sum,na.rm=T))/1000

#                alb       bft        bet         skj         yft   swo   bum  sai whm
# U.S.A.         25.825 31397.963   2360.765  128429.735  103531.315 0.000 0.000 0.00   0
# EU.España    4521.090   903.900 168162.540 1140806.620 1216948.670 0.000 0.000 0.00   0
# Other        4299.320  3451.287 274045.180 1279444.048 1629395.695 0.000 0.000 0.00   0
# Belize         96.125     0.000      0.000       0.000       0.000 0.000 0.000 0.00   0
# Brasil          0.000     0.000      0.000     808.425     504.000 0.000 0.000 0.00   0
# EU.Malta           NA        NA         NA          NA          NA    NA    NA   NA  NA
# EU.Portugal     7.120     0.865      0.000      14.069       0.437 0.026 0.797 0.31   0
# Japan          25.100     0.000    182.700   23822.400   18998.900 0.000 0.000 0.00   0
# Panama         38.530     0.000  16523.880   76855.650   43251.450 0.000 0.000 0.00   0
# South Africa    0.000     0.000      0.000     122.000     151.000 0.000 0.000 0.00   0
# U.S.S.R.           NA        NA         NA          NA          NA    NA    NA   NA  NA
# Venezuela    2941.085     0.000   6041.951  120893.778  194309.645 0.000 0.000 0.00   0
            
alb.ps <- fit2stageGAMtoCatch.r(input=pslf,which.flag='EU.España',which.species='alb',start.year=1970,end.year=2010)
#bft.ps <- fit2stageGAMtoCatch.r(input=pslf,which.flag='EU.España',which.species='bft',start.year=1970,end.year=2010,kk=3)
bet.ps <- fit2stageGAMtoCatch.r(input=pslf,which.flag='EU.España',which.species='bet',start.year=1970,end.year=2010)
skj.ps <- fit2stageGAMtoCatch.r(input=pslf,which.flag='EU.España',which.species='skj',start.year=1970,end.year=2010)
yft.ps <- fit2stageGAMtoCatch.r(input=pslf,which.flag='EU.España',which.species='yft',start.year=1970,end.year=2010)

# Do we just assume here that once we've modeled NO.HOOKS as a function of time we can use that sensibly ?

emod.ps <- fitGAMtoEffort.r(input=pslf,which.flag='EU.España',which.effort='FISH.HOUR',start.year=1990,end.year=2010)

# Create grids and predict over them ###

alb.aa.ps <- predict.effdis.t2.data.r(cmod=alb.ps, effmod=emod.ps,grid.res=1,start.year=1970,end.year=2010,which.flag='EU.España',which.gear = 'PS')
#bft.aa.ps <- predict.effdis.t2.data.r(cmod=bft.ps, effmod=emod.ps,grid.res=5,start.year=1970,end.year=2010,which.flag='EU.España',which.gear = 'PS')
bet.aa.ps <- predict.effdis.t2.data.r(cmod=bet.ps, effmod=emod.ps,grid.res=1,start.year=1970,end.year=2010,which.flag='EU.España',which.gear = 'PS')
skj.aa.ps <- predict.effdis.t2.data.r(cmod=skj.ps, effmod=emod.ps,grid.res=1,start.year=1970,end.year=2010,which.flag='EU.España',which.gear = 'PS')
yft.aa.ps <- predict.effdis.t2.data.r(cmod=yft.ps, effmod=emod.ps,grid.res=1,start.year=1970,end.year=2010,which.flag='EU.España',which.gear = 'PS')


# Plot data 
par(mfrow=c(3,3))
plot.mods.r(input=alb.aa.ps,cmod=alb.ps,what.year = 1992,what.month=1,what.value = 'catch',grid.res=1)
#plot.mods.r(input=bft.aa,cmod=bft,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=bet.aa.ps,cmod=bet.ps,what.year = 1994,what.month=3,what.value = 'catch',grid.res=1,plot.samples.only=T)
plot.mods.r(input=skj.aa.ps,cmod=skj.ps,what.year = 1995,what.month=1,what.value = 'catch',grid.res=1)
par(mfrow=c(3,4))
for(i in 1:12)
{
plot.mods.r(input=yft.aa.ps,cmod=yft.ps,what.year = 1995,what.month=i,what.value = 'prob',grid.res=1)
}

plot(alb.aa$longitude,alb.aa$latitude)
points(alb$pmod.data$longitude,alb$pmod.data$latitude,col='red')

#Get Task 1 data

ps.t1 <- get.effdis.t1.data.r(which.dsn='effdis-local',which.gear = 'PS',which.region='AT',which.flag='EU.España')
#ll.t1 <- get.effdis.t1.data.r(which.dsn='effdis-local',which.gear = 'LL',which.region='AT',which.flag='All')

w1 <- (1:length(ps.t1[,1]))[ps.t1$yearc == 1990]
tapply(ps.t1$qty_t[w1],list(ps.t1$flag[w1],ps.t1$species[w1]),sum,na.rm=T)

# See task 1 totals by species

t1x <- aggregate(list(qty_t=ps.t1$qty_t),list(year=ps.t1$yearc,species = ps.t1$species),sum,na.rm=T)
t1x[t1x$species == 'bet',]

#Bind up estimates for 4 different species

big <- rbind(alb.aa.ps,bet.aa.ps,skj.aa.ps,yft.aa.ps)

big<- big[big$which.ocean == "atl",]

big$catch[big$observation == FALSE] <- NA
big$prob[big$observation == FALSE] <- NA
big$measured_catch[big$observation == FALSE] <- NA
big$eff[big$observation == FALSE]  <- NA

#xxx <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
#by=list(year=big$year,species=big$species),sum,na.rm=T)

#xxx.90 <- xxx[xxx$year == 1990 & xxx$species == 'bft',]
#sum(xxx.90$catch)/1000


#Convert catch to tonnes from kgs

big$catch <- big$catch/1000
big$measured_catch <- big$measured_catch/1000
big$eff <- big$eff/4 # Effort is the same for each species so divide by 4


big1 <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
                  by=list(year=big$year),sum,na.rm=T)

big1$cpue <- big1$catch/big1$eff


# Aggregate estimates from Task 1

sum.t1 <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc),sum,na.rm=T)

# Merge t1 and t2

big2 <- merge(big1,sum.t1)

big2$cpue <- big2$measured_catch/big2$eff


plot(big2$year,big2$catch,ylim=c(range(big2$catch,big2$qty_t)))
lines(big2$year,big2$qty_t)


big2$new.effort <- big2$qty_t/big2$cpue

write.table(big2,'/home/doug/effdis/data/effdis-estimate.csv',sep=',',row.names=F)

par(mfrow=c(1,1))
plot(big2$year,big2$new.effort/1000000,xlim=c(1970,2010),type='l')

##########################################
######## Longliners  #####################
##########################################

# Get data for each dsettype

lln  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'n-')
llnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'nw')
llw  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = '-w')
ll1 <- rbind(lln,llnw,llw)

#t2 <- tapply(ll1$totsp9,list(ll1$flagname,ll1$year),sum,na.rm=T)

ll1<-find.ocean(ll1)
ll1 <- ll1[ll1$which.ocean == 'atl',]
lllf <- convert2long.format.t2(input =ll1)
lllf<-prepare.effdis.data(input=lllf)

bm <- model.nos.kgs(input=lllf,which.gear='LL')

lllf <- kgs.from.nos(lllf) # for those fleets that supply only number

three.d.catch.by.year(tdata=lllf,scaling.f=100)

# t2 <- aggregate(list(measured_catch=lllf$measured_catch),
#                 list(species=lllf$species,catchunit=lllf$catchunit,flagname=lllf$flagname,year=lllf$year),sum,na.rm=T)
# 
# xx <- t2[t2$year == 1990 & t2$flagname == 'Japan' & t2$catchunit == 'kg',]
# 
# xx$measured_catch<- xx$measured_catch/1000

table(lllf$eff1type)

# D.AT SEA   D.FISH NO.BOATS NO.HOOKS   -none-  NO.SETS NO.TRIPS SUC.D.FI SUC.SETS 
# 252     6561       27  2139498    67959     1593      810       36      504 

lllf <- lllf[lllf$eff1type=='NO.HOOKS',]

w0 <- (1:length(lllf$year))[lllf$catchunit == 'kg' & lllf$year >= 1990]

round(tapply(lllf$measured_catch[w0],list(lllf$flagname[w0],lllf$species[w0]),sum,na.rm=T))/1000

setwd('/home/doug/effdis/effdis-estimates')

alb <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='alb',start.year=2000,end.year=2010)
bft <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bft',start.year=1970,end.year=2010)
bet <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bet',start.year=1970,end.year=2010)
skj <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='skj',start.year=1970,end.year=2010)
yft <- fit2stageGAMtoCatch.input=lllf,which.flag='Japan',which.species='yft',start.year=1970,end.year=2010)
swo <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='swo',start.year=1970,end.year=2010)
bum <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bum',start.year=1970,end.year=2010)
sai <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='sai',start.year=1970,end.year=2010)
whm <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='whm',start.year=1970,end.year=2010)


#Try calculating effort directly from the raw data instead

big <- aggt2data()
big$catch <-big$measured_catch




# Do we just assume here that once we've modeled NO.HOOKS as a function of time we can use that sensibly ?

emod <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=2000,end.year=2010)

# Create grids and predict over them ###

alb.aa <- predict.effdis.t2.data(cmod=alb, effmod=emod,grid.res=5,start.year=2000,end.year=2010,which.flag='Japan')
bft.aa <- predict.effdis.t2.data(cmod=bft, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
bet.aa <- predict.effdis.t2.data(cmod=bet, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
skj.aa <- predict.effdis.t2.data(cmod=skj, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
yft.aa <- predict.effdis.t2.data(cmod=yft, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
swo.aa <- predict.effdis.t2.data(cmod=swo, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
bum.aa <- predict.effdis.t2.data.r(cmod=bum, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
sai.aa <- predict.effdis.t2.data.r(cmod=sai, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')
whm.aa <- predict.effdis.t2.data.r(cmod=whm, effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Japan')

#Plot data

plot.mods(input=alb.aa,cmod=alb,which.year=2005,grid.res=5,which.value='measured_catch.se.fit',which.gear='LL',plot.samples.only=TRUE)

# Loop around all flags and species #

us <- sort(as.character(unique(lllf$species)))
uf <- sort(as.character(unique(lllf$flagname)))
uf <- uf[-c(7,10,15,17,18,19,21)]

setwd('/home/doug/effdis/data')

#Brasil

emod <- fitGAMtoEffort.r(input=lllf,which.flag='Brasil',which.effort='NO.HOOKS',start.year=1970,end.year=2010)

mod <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod[[i]] <- fit2stageGAMtoCatch.r(input=lllf,which.flag='Brasil',which.species=sp,start.year=1970,end.year=2010)
  print(sp)
}
  
for(i in 1:9)
{
  if(mod[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
  aa <- predict.effdis.t2.data.r(cmod=mod[[i]], effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag='Brasil')
  rm(aa)
  gc(reset=T)
}
}

#China P.R

wf <- 3

emod <- fitGAMtoEffort.r(input=lllf,which.flag=uf[3],which.effort='NO.HOOKS',start.year=1970,end.year=2010)

mod <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod[[i]] <- fit2stageGAMtoCatch.r(input=lllf,which.flag=uf[wf],which.species=sp,start.year=1970,end.year=2010)
}

for(i in 1:9)
{
  if(mod[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data.r(cmod=mod[[i]], effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag=uf[wf])
    rm(aa)
    gc(reset=T)
  }
}


#Chinese Taipei

wf <- 16

emod <- fitGAMtoEffort.r(input=lllf,which.flag=uf[wf],which.effort='NO.HOOKS',start.year=1970,end.year=2010)

mod <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod[[i]] <- fit2stageGAMtoCatch.r(input=lllf,which.flag=uf[wf],which.species=sp,start.year=1970,end.year=2010)
}

for(i in 1:9)
{
  if(mod[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data.r(cmod=mod[[i]], effmod=emod,grid.res=5,start.year=1970,end.year=2010,which.flag=uf[wf])
    rm(aa)
    gc(reset=T)
  }
}


# Problems: Spain ?? Korea, Mexico, Namibia, SA, 

# Plot data 


par(mfrow=c(3,3))
plot.mods.r(input=alb.aa,cmod=alb,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=bft.aa,cmod=bft,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=bet.aa,cmod=bet,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=skj.aa,cmod=skj,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=yft.aa,cmod=yft,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=swo.aa,cmod=swo,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=bum.aa,cmod=bum,what.year = 1995,what.month=1,what.value = 'prob',grid.res=5)
plot.mods.r(input=sai.aa,cmod=sai,what.year = 1995,what.month=6,what.value = 'prob',grid.res=5)
plot.mods.r(input=whm.aa,cmod=whm,what.year = 1995,what.month=6,what.value = 'prob',grid.res=5)

plot(alb.aa$longitude,alb.aa$latitude)
points(alb$pmod.data$longitude,alb$pmod.data$latitude,col='red')

#Get Task 1 data

ll.t1 <- get.effdis.t1.data.r(which.dsn='effdis-tuna-cc1',which.gear = 'LL',which.region='AT',which.flag='Japan')
#ll.t1 <- get.effdis.t1.data.r(which.dsn='effdis-local',which.gear = 'LL',which.region='AT',which.flag='All')

w1 <- (1:length(ll.t1[,1]))[ll.t1$yearc == 1990]
tapply(ll.t1$qty_t[w1],list(ll.t1$flag[w1],ll.t1$species[w1]),sum,na.rm=T)

# See task 1 totals by species

t1x <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,species = ll.t1$species),sum,na.rm=T)
t1x[t1x$species == 'bft',]


#Bind up estimates for 9 different species

big <- rbind(alb.aa,bet.aa,bft.aa,bum.aa,sai.aa,skj.aa,swo.aa,whm.aa,yft.aa)

big<- big[big$which.ocean == "atl",]

big$catch[big$observation == FALSE] <- NA
big$prob[big$observation == FALSE] <- NA
big$measured_catch[big$observation == FALSE] <- NA
big$eff[big$observation == FALSE]  <- NA

#xxx <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
                  #by=list(year=big$year,species=big$species),sum,na.rm=T)

#xxx.90 <- xxx[xxx$year == 1990 & xxx$species == 'bft',]
#sum(xxx.90$catch)/1000


#Convert catch to tonnes from kgs

big$catch <- big$catch/1000
big$measured_catch <- big$measured_catch/1000
big$eff <- big$eff/9 # Effort is the same for each species so divide by 9


big1 <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
                  by=list(year=big$year),sum,na.rm=T)

big1$cpue <- big1$catch/big1$eff


# Aggregate estimates from Task 1

sum.t1 <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc),sum,na.rm=T)

# Merge t1 and t2

big2 <- merge(big1,sum.t1)

big2$cpue <- big2$catch/big2$eff


plot(big2$year,big2$catch,ylim=c(range(big2$catch,big2$qty_t)))
lines(big2$year,big2$qty_t)


big2$raised.effort <- big2$qty_t/big2$cpue

write.table(big2,'/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',row.names=F)

par(mfrow=c(1,1))
plot(big2$year,big2$raised.effort/1000000,type='l')



plot(aa$month,aa$prob)
boxplot(catch~month,data=aa)
plot(aa$trend,log(aa$catch/1000),pch='.')
abline(v=seq(1,756,by=12),lty=2,col='blue')












