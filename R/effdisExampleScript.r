
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

#plot(ll1$longitude[ll1$flagname == 'Japan'],ll1$latitude[ll1$flagname =='Japan'],pch='+')
#points(ll1$longitude[ll1$flagname == 'EU.Portugal'],ll1$latitude[ll1$flagname =='EU.Portugal'],pch='+',col='red')

ll1 <- convert.grid.res(ll1) # Some data, eg. Portugal are supplied at 1x1 but for the modeling we need to be consistent.

#points(ll2$longitude[ll2$flagname == 'EU.Portugal'],ll2$latitude[ll2$flagname =='EU.Portugal'],pch='+',col='blue')
#points(ll2$longitude[ll2$flagname == 'U.S.A.'],ll2$latitude[ll2$flagname =='U.S.A.'],pch='+',col='green')

#t2 <- tapply(ll1$totsp9,list(ll1$flagname,ll1$year),sum,na.rm=T)

data("seas")
ll1<-find.ocean(ll1)
ll1 <- ll1[ll1$which.ocean == 'atl',]
ll1 <- ll1[ll1$squaretypecode == '5x5',]

lllf <- convert2long.format.t2(input =ll1)
lllf<-prepare.effdis.data(input=lllf)

bm <- model.nos.kgs(input=lllf,which.gear='LL')

lllf <- kgs.from.nos(lllf) # for those fleets that supply only number
lllf <- lllf[lllf$eff1type=='NO.HOOKS',]

three.d.catch.by.year(tdata=lllf,scaling.f=100)

##Japan##

setwd('/home/doug/effdis/effdis-estimates')

alb <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='alb',start.year=1950,end.year=2010)
bft <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bft',start.year=1950,end.year=2010)
bet <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bet',start.year=1950,end.year=2010)
skj <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='skj',start.year=1950,end.year=2010)
yft <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='yft',start.year=1950,end.year=2010)
swo <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='swo',start.year=1950,end.year=2010)
bum <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='bum',start.year=1950,end.year=2010)
sai <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='sai',start.year=1950,end.year=2010)
whm <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species='whm',start.year=1950,end.year=2010)


#Try calculating effort directly from the raw data instead

#big <- aggt2data()
#big$catch <-big$measured_catch

# Do we just assume here that once we've modeled NO.HOOKS as a function of time we can use that sensibly ?

emod <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1950,end.year=2010)

# Create grids and predict over them ###

setwd('/home/doug/effdis/effdis-estimates')
alb.aa <- predict.effdis.t2.data(cmod=alb, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
bft.aa <- predict.effdis.t2.data(cmod=bft, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
bet.aa <- predict.effdis.t2.data(cmod=bet, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
skj.aa <- predict.effdis.t2.data(cmod=skj, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
yft.aa <- predict.effdis.t2.data(cmod=yft, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
swo.aa <- predict.effdis.t2.data(cmod=swo, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
bum.aa <- predict.effdis.t2.data(cmod=bum, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
sai.aa <- predict.effdis.t2.data(cmod=sai, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')
whm.aa <- predict.effdis.t2.data(cmod=whm, effmod=emod,grid.res=5,start.year=1950,end.year=2010,which.flag='Japan')

# #Plot data
# 
# plot.mods(input=alb.aa,cmod=alb,which.year=2005,grid.res=5,which.value='measured_catch.se.fit',which.gear='LL',plot.samples.only=TRUE)
# plot.mods(input=alb.aa,cmod=alb,which.year=2005,grid.res=5,which.value='measured_catch',which.gear='LL',plot.samples.only=TRUE)
# plot.mods(input=bet.aa,cmod=bet,which.month=9,which.year=2000,grid.res=5,which.value='measured_catch',which.gear='LL',plot.samples.only=F)
# plot.mods(input=whm.aa,cmod=whm,which.year=1995,grid.res=5,which.value='cpue',which.gear='LL',plot.samples.only=TRUE)

# Loop around all flags and species #

#table(as.character(lllf$flagname))

us <- sort(as.character(unique(lllf$species)))
#uf <- sort(as.character(unique(lllf$flagname)))
#uf <- uf[-c(7,10,15,17,18,19,21)]

#setwd('/home/doug/effdis/data')


#Japan

emod.jap <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.jap <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.jap[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species=sp,start.year=1960,end.year=2010,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.jap[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.jap[[i]], effmod=emod.jap,grid.res=5,start.year=1960,end.year=2010,which.flag='Japan')
    rm(aa)
    gc(reset=T)
  }
}



#Belize

emod.belize <- fitGAMtoEffort(input=lllf,which.flag='Belize',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.belize <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.belize[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Belize',which.species=sp,start.year=1960,end.year=2010,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.belize[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.belize[[i]], effmod=emod.belize,grid.res=5,start.year=1960,end.year=2010,which.flag='Belize')
    rm(aa)
    gc(reset=T)
  }
}


#Brasil

emod.brasil <- fitGAMtoEffort(input=lllf,which.flag='Brasil',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.brasil <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.brasil[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Brasil',which.species=sp,start.year=1960,end.year=2010)
  print(sp)
}
  
for(i in 1:9)
{
  if(mod.brasil[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
  aa <- predict.effdis.t2.data(cmod=mod.brasil[[i]], effmod=emod.brasil,grid.res=5,start.year=1960,end.year=2010,which.flag='Brasil')
  rm(aa)
  gc(reset=T)
}
}

#China P.R

wf <- 3

emod.china.pr <- fitGAMtoEffort(input=lllf,which.flag='China P.R.',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.china.pr <- as.list(1:9)

for(i in c(1:2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.china.pr[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='China P.R.',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1:2,4:9))
{
  if(mod.china.pr[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.china.pr[[i]], effmod=emod.china.pr,grid.res=5,start.year=1960,end.year=2010,which.flag='China P.R.')
    rm(aa)
    gc(reset=T)
  }
}


#Chinese Taipei

emod.ct <- fitGAMtoEffort(input=lllf,which.flag='Chinese Taipei',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.ct <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.ct[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Chinese Taipei',which.species=sp,start.year=1960,end.year=2010)
}

for(i in 1:9)
{
  if(mod.ct[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ct[[i]], effmod=emod.ct,grid.res=5,start.year=1960,end.year=2010,which.flag='Chinese Taipei')
    rm(aa)
    gc(reset=T)
  }
}


# Cuba
emod.cuba <- fitGAMtoEffort(input=lllf,which.flag='Cuba',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.cuba <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.cuba[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Cuba',which.species=sp,start.year=1960,end.year=2010)
}

for(i in 1:9)
{
  if(mod.cuba[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cuba[[i]], effmod=emod.cuba,grid.res=5,start.year=1960,end.year=2010,which.flag='Cuba')
    rm(aa)
    gc(reset=T)
  }
}

# Spain
emod.spain <- fitGAMtoEffort(input=lllf,which.flag='EU.España',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.spain <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.spain[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='EU.España',which.species=sp,start.year=1960,end.year=2010)
}

for(i in 1:9)
{
  if(mod.spain[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.spain[[i]], effmod=emod.spain,grid.res=5,start.year=1960,end.year=2010,which.flag='EU.España')
    rm(aa)
    gc(reset=T)
  }
}


# Portugal

emod.port <- fitGAMtoEffort(input=lllf,which.flag='EU.Portugal',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.port <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.port[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='EU.Portugal',which.species=sp,start.year=1960,end.year=2010)
}

for(i in 1:9)
{
  if(mod.port[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.port[[i]], effmod=emod.port,grid.res=5,start.year=1960,end.year=2010,which.flag='EU.Portugal')
    rm(aa)
    gc(reset=T)
  }
}

# Korea

emod.kor <- fitGAMtoEffort(input=lllf,which.flag='Korea Rep.',which.effort='NO.HOOKS',start.year=1960,end.year=2010)

mod.kor <- as.list(1:9)

for(i in c(1,2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.kor[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Korea Rep.',which.species=sp,start.year=1960,end.year=2010,kk=5)
}

for(i in c(1,2,4:9))
{
  if(mod.kor[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.kor[[i]], effmod=emod.kor,grid.res=5,start.year=1960,end.year=2010,which.flag='Korea Rep.')
    rm(aa)
    gc(reset=T)
  }
}

# Maroc

emod.mar <- fitGAMtoEffort(input=lllf,which.flag='Maroc',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=2)

mod.mar <- as.list(1:9)

for(i in c(1,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.mar[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Maroc',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,4:9))
{
  if(mod.mar[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.mar[[i]], effmod=emod.mar,grid.res=5,start.year=1960,end.year=2010,which.flag='Maroc')
    rm(aa)
    gc(reset=T)
  }
}



#Need to investigate Maroc


# Namibia

emod.nam <- fitGAMtoEffort(input=lllf,which.flag='Namibia',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.nam <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.nam[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Namibia',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.nam[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.nam[[i]], effmod=emod.nam,grid.res=5,start.year=1960,end.year=2010,which.flag='Namibia')
    rm(aa)
    gc(reset=T)
  }
}


# Panama

emod.pan <- fitGAMtoEffort(input=lllf,which.flag='Panama',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.pan <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.pan[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Panama',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.pan[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.pan[[i]], effmod=emod.pan,grid.res=5,start.year=1960,end.year=2010,which.flag='Panama')
    rm(aa)
    gc(reset=T)
  }
}


# Philippines

emod.phi <- fitGAMtoEffort(input=lllf,which.flag='Philippines',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.phi <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.phi[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Philippines',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4,5,7:9))
{
  if(mod.phi[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.phi[[i]], effmod=emod.phi,grid.res=5,start.year=1960,end.year=2010,which.flag='Philippines')
    rm(aa)
    gc(reset=T)
  }
}


# Mexico

emod.mex <- fitGAMtoEffort(input=lllf,which.flag='Mexico',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.mex <- as.list(1:9)

for(i in c(1,2,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.mex[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Mexico',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,4:9))
{
  if(mod.mex[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.mex[[i]], effmod=emod.mex,grid.res=5,start.year=1960,end.year=2010,which.flag='Mexico')
    rm(aa)
    gc(reset=T)
  }
}


# Other

emod.oth <- fitGAMtoEffort(input=lllf,which.flag='Other',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.oth <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.oth[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Other',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.oth[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.oth[[i]], effmod=emod.oth,grid.res=5,start.year=1960,end.year=2010,which.flag='Other')
    rm(aa)
    gc(reset=T)
  }
}

# South Africa

emod.sa <- fitGAMtoEffort(input=lllf,which.flag='South Africa',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=6)

mod.sa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.sa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='South Africa',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.sa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.sa[[i]], effmod=emod.sa,grid.res=5,start.year=1960,end.year=2010,which.flag='South Africa')
    rm(aa)
    gc(reset=T)
  }
}

# St Vincent & Grenadines

emod.svg <- fitGAMtoEffort(input=lllf,which.flag='St. Vincent and Grenadines',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=6)

mod.svg <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.svg[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='St. Vincent and Grenadines',which.species=sp,start.year=1960,end.year=2010,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.svg[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.svg[[i]], effmod=emod.svg,grid.res=5,start.year=1960,end.year=2010,which.flag='St. Vincent and Grenadines')
    rm(aa)
    gc(reset=T)
  }
}


# Trinidad & Tobago

# emod.tri <- fitGAMtoEffort(input=lllf,which.flag='Trinidad and Tobago',which.effort='NO.HOOKS',start.year=1960,end.year=2010)
# 
# mod.tri <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.tri[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Trinidad and Tobago',which.species=sp,start.year=1960,end.year=2010,kk=3)
# }
# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.tri[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.tri[[i]], effmod=emod.tri,grid.res=5,start.year=1960,end.year=2010,which.flag='Trinidad and Tobago')
#     rm(aa)
#     gc(reset=T)
#   }
# }

# Uruguay

emod.uru <- fitGAMtoEffort(input=lllf,which.flag='Uruguay',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.uru <- as.list(1:9)
for(i in c(1,2,3,4,5:8))
{
  sp <- us[i]
  print(sp)
  mod.uru[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Uruguay',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:8))
{
  if(mod.uru[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.uru[[i]], effmod=emod.uru,grid.res=5,start.year=1960,end.year=2010,which.flag='Uruguay')
    gc(reset=T)
  }
}


# U.S.A.

emod.usa <- fitGAMtoEffort(input=lllf,which.flag='U.S.A.',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=6)

mod.usa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.usa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.A.',which.species=sp,start.year=1960,end.year=2010,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.usa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.usa[[i]], effmod=emod.usa,grid.res=5,start.year=1960,end.year=2010,which.flag='U.S.A.')
    gc(reset=T)
  }
}


# U.S.S.R.

emod.uss <- fitGAMtoEffort(input=lllf,which.flag='U.S.S.R.',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.uss <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.uss[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.S.R.',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.uss[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.uss[[i]], effmod=emod.uss,grid.res=5,start.year=1960,end.year=2010,which.flag='U.S.S.R.')
    gc(reset=T)
  }
}


# Vanuatu.

emod.van <- fitGAMtoEffort(input=lllf,which.flag='Vanuatu',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=3)

mod.van <- as.list(1:9)
for(i in c(1,2,3,5:9))
{
  sp <- us[i]
  print(sp)
  mod.van[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Vanuatu',which.species=sp,start.year=1960,end.year=2010,kk=3)
}
mod.van[[4]] <- 'Insufficient data to support model'

for(i in c(1,2,3,5:9))
{
  if(mod.van[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.van[[i]], effmod=emod.van,grid.res=5,start.year=1960,end.year=2010,which.flag='Vanuatu')
    gc(reset=T)
  }
}


# Venezuela.

setwd('/home/doug/effdis/effdis-estimates')

emod.ven <- fitGAMtoEffort(input=lllf,which.flag='Venezuela',which.effort='NO.HOOKS',start.year=1960,end.year=2010,kk=6)

mod.ven <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.ven[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Venezuela',which.species=sp,start.year=1960,end.year=2010,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.ven[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ven[[i]], effmod=emod.ven,grid.res=5,start.year=1960,end.year=2010,which.flag='Venezuela')
    gc(reset=T)
    
  }}

################# Raise the data to Task I ####################################################


setwd('/home/doug/effdis/effdis-estimates')
  system('rm effdis-estimates.csv')
  system('cat *csv > effdis-estimates.csv')


effdis_estimates <- read.table('effdis-estimates.csv',sep=',')
dim(effdis_estimates)

dimnames(effdis_estimates)[[2]] <- c("longitude","latitude","which.ocean","year","month","trend","flagname","geargrp","prob","prob.se.fit","measured_catch","measured_catch.se.fit",
                                     "eff","eff.se.fit","species","catch","cpue","observation")          



#Get Task 1 data

ll.t1 <- get.effdis.t1.data(which.dsn='effdis-tuna-cc1',which.gear = 'LL',which.region='AT',which.flag='All')
#ll.t1 <- get.effdis.t1.data.r(which.dsn='effdis-local',which.gear = 'LL',which.region='AT',which.flag='All')

#Convert Task 1 to 'Other'

uf2 <- uf
uf1 <- as.character(sort(unique(ll.t1$flag)))
ll.t1$flag <- as.character(ll.t1$flag)

mm <- match(ll.t1$flag,uf2)
ll.t1$flag[is.na(mm)] <- 'Other'

w1 <- (1:length(ll.t1[,1]))[ll.t1$yearc == 1990]
tapply(ll.t1$qty_t[w1],list(ll.t1$flag[w1],ll.t1$species[w1]),sum,na.rm=T)

# See task 1 totals by species

t1x <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,species = ll.t1$species),sum,na.rm=T)
t1x[t1x$species == 'bft',]


# effdis_estimates<- effdis_estimates[effdis_estimates$which.ocean == "atl",]
# 
# effdis_estimates$catch[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$prob[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$measured_catch[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$eff[big$observation == FALSE]  <- NA

#xxx <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
                  #by=list(year=big$year,species=big$species),sum,na.rm=T)

#xxx.90 <- xxx[xxx$year == 1990 & xxx$species == 'bft',]
#sum(xxx.90$catch)/1000


#Convert catch to tonnes from kgs

effdis_estimates$catch <- effdis_estimates$catch/1000
effdis_estimates$measured_catch <- effdis_estimates$measured_catch/1000
#effdis_estimates$eff <- effdis_estimates$eff/9 # Effort is the same for each species so divide by 9


effdis_estimates1 <- aggregate(list(measured_catch=effdis_estimates$measured_catch,catch=effdis_estimates$catch,eff=effdis_estimates$eff), 
                  by=list(year=effdis_estimates$year,flagname=effdis_estimates$flagname),sum,na.rm=T)

effdis_estimates1$cpue <- effdis_estimates1$catch/effdis_estimates1$eff

# Aggregate estimates from Task 1

sum.t1 <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,flagname=ll.t1$flag),sum,na.rm=T)

# Merge t1 and t2

effdis_estimates2 <- merge(effdis_estimates1,sum.t1)

effdis_estimates2$cpue <- effdis_estimates2$catch/effdis_estimates2$eff

library(lattice)
xyplot(cpue~year|flagname,data=effdis_estimates2,type='b',ylim=c(0,.0001))

plot(effdis_estimates2$year,effdis_estimates2$catch,ylim=c(range(effdis_estimates2$catch,effdis_estimates2$qty_t)))
lines(effdis_estimates2$year,effdis_estimates2$qty_t)


effdis_estimates2$raised.effort <- effdis_estimates2$qty_t/effdis_estimates2$cpue

#write.table(big2,'/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',row.names=F)

xxx <- read.table('/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',header=T)
xxx1 <- effdis_estimates2[effdis_estimates2$flagname == 'Japan',]

par(mfrow=c(1,1))
plot(effdis_estimates2$year[effdis_estimates2$flagname == 'Japan'],
     effdis_estimates2$raised.effort[effdis_estimates2$flagname == 'Japan']/1000000,type='l')

xyplot(raised.effort~year|flagname,data=effdis_estimates2[effdis_estimates2$flagname == 'Japan',],type='b')


plot(aa$month,aa$prob)
boxplot(catch~month,data=aa)
plot(aa$trend,log(aa$catch/1000),pch='.')
abline(v=seq(1,756,by=12),lty=2,col='blue')












