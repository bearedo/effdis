
# Load libraries

library(rio)
library(spatial)
library(sp)
library(doBy)
library(rgdal)
library(RODBC)
library(RColorBrewer)
library(ggplot2)
library(vmstools)
library(maps)
library(mapdata)
#library(COZIGAM)
library(RODBC)
library(reshape2)
library(mgcv)
#library(doMC)
library(effdisR)
library(lattice)

#setwd('d://Dropbox/AOTTP/Data/effdis/effdis/data/effdis-estimates/PS')

setwd("D://dbeare/dbeare/effdis/data/effdis-estimates-2019/PS")

#########################
## Purse-seiners example ##
#########################

# Purse-seine example #

# Get data for each dsettype

psn  <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = 'n-')
psnw <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = 'nw')
psw  <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='PS',which.flag='All',which.dsettype = '-w')

ps1 <- rbind(psn,psnw,psw)
dim(ps1) #= 279133     27 (2019)

# # Double-check that lat and long conversions have been done properly #
# df <- data.frame(quad=ps1$quadid,lat=ps1$lat,lon=ps1$lon,square=ps1$squaretypecode) # simplify data.frame
# df$square <- as.character(df$square) # function requires input to be 'character'
# 
# hl <- length(df[,1])
# ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
# for(i in 1:hl){
# ndf[i,] <- latLon(df[i,]) # calculate the central lat long of each grid looping thro' each row
# }
# 
# plot(ndf$lon,ndf$lat,pch=".")
# 
# ps1$longitude <- ndf$lon # add on lat and long to main database
# ps1$latitude <- ndf$lat

table(ps1$squaretypecode,ps1$flagname)

t2 <- tapply(ps1$totsp9,list(ps1$flagname,ps1$year),sum,na.rm=T)

xyplot(log(eff1)~year|flagname,data=ps1[ps1$eff1type=='FISH.HOUR',])

table(ps1$flagname,ps1$eff1type)



data("seas")
ps1<-find.ocean(ps1)
ps1 <- ps1[ps1$which.ocean == 'atl',]
dim(ps1)#=271271

ps1<-prepare.effdis.data(input=ps1)
dim(ps1)#=153568

ps1<-ps1[ps1$squaretypecode == '1x1',]

dim(ps1)#=152561

## Throw out Brazil due to large outliers

ps1 <- ps1[ps1$flagname != 'Brazil',]

dim(ps1) #=152447     28


library(reshape2)

pslf <- convert2long.format.t2(input =ps1)
dim(pslf)#=1372023

#bm <- model.nos.kgs(input=pslf,which.gear='PS')

#pslf <- kgs.from.nos.r(pslf) # Not relevant for PS 

table(pslf$eff1type) # 2017

#D.AT SEA    D.FISH FISH.HOUR    -none-   NO.SETS  NO.TRIPS  SUC.D.FI  SUC.SETS 
#1881    237051    531621      9729     24642       369        27       126 

table(pslf$eff1type) # 2019

#-none-  D.AT SEA    D.FISH FISH.HOUR   NO.SETS  SUC.D.FI  SUC.SETS 
#4680      1755    241416   1109358     14535        27       252 

pslf <- pslf[pslf$eff1type %in% c('D.FISH','FISH.HOUR'),]
pslf$eff1[pslf$eff1type == 'D.FISH'] <- pslf$eff1[pslf$eff1type == 'D.FISH']*24
pslf$eff1type <- 'FISH.HOUR'

w0 <- (1:length(pslf$year))[pslf$catchunit == 'kg']

pslf$measured_catch <- as.numeric(pslf$measured_catch)

round(tapply(pslf$measured_catch[w0],list(pslf$flagname[w0],pslf$species[w0]),sum,na.rm=T)/1000)

 dim(pslf) # = 768672 (2017)
 
 dim(pslf) # = 1350774 (2019)

pslf <- pslf[pslf$catchunit == "kg",]
pslf$species <- as.character(pslf$species)
pslf$flagname<- as.character(pslf$flagname)
pslf$fleetcode <- as.character(pslf$fleetcode)
pslf$geargrpcode <- as.character(pslf$geargrpcode)
pslf$dsettype <- as.character(pslf$dsettype)
pslf$catchunit <- as.character(pslf$catchunit)
dim(pslf) #=  768672 (2017)
dim(pslf) #=  1350774 (2019)

us <- sort(as.character(unique(pslf$species)))
uf <- sort(as.character(unique(pslf$flagname[pslf$year > 1989])))

 uf # 2017
 
 # [1] "Belize"             "Brazil"             "Cape Verde"         "Côte D'Ivoire"      "Curaçao"            "EU.España"          "EU.France"         
 # [8] "EU.Portugal"        "Ghana"              "Guatemala"          "Guinée Rep."        "Japan"              "Mixed flags (FIS)"  "NEI (ETRO)"        
 # [15] "Panama"             "Russian Federation" "U.S.A."             "Venezuela"  

  dim(pslf) #=  768672 (2015)
  
  
uf # 2019
  
# [1] "Belize"                     "Cape Verde"                 "Côte d'Ivoire"             
# [4] "Curaçao"                    "El Salvador"                "EU.España"                 
# [7] "EU.France"                  "EU.Portugal"                "Ghana"                     
# [10] "Guatemala"                  "Guinée Rep."                "Japan"                     
# [13] "Maroc"                      "NEI (ETRO)"                 "Norway"                    
# [16] "Panama"                     "Russian Federation"         "Senegal"                   
# [19] "St. Vincent and Grenadines" "U.S.A."                     "Vanuatu"                   
# [22] "Venezuela"                 
  
pslf$flagname[pslf$flagname == "CÃ´te d'Ivoire"] <- "Côte D'Ivoire"
pslf$flagname[pslf$flagname == "EU.EspaÃ±a"] <-  "EU.España"
pslf$flagname[pslf$flagname == "CuraÃ§ao"] <-  "Curaçao"
pslf$flagname[pslf$flagname == "GuinÃ©e Rep."] <- "Guinée Rep." 


# Flags to add to "others"- 

oth <-c("Belize", "Brazil", "Canada","Côte D'Ivoire","EU.Portugal","Russian Federation","U.S.A.")
idx <- (1:length(pslf[,1]))[pslf$flagname %in% oth]
pslf$flagname[idx] <- "Other"

#######################PURSE SEINERS Start here ###################
#Japan

emod.jap <- fitGAMtoEffort(input=pslf,which.flag='Japan',which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=3)

mod.jap <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.jap[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag='Japan',which.species=sp,start.year=1990,end.year=2017,kk=3)
  print(sp)
}


for(i in 1:9)
{
  if(mod.jap[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.jap[[i]],which.gear="PS", effmod=emod.jap,grid.res=1,start.year=1990,end.year=2017,which.flag='Japan')
    rm(aa)
    gc(reset=T)
  }
}


aa <- predict.effdis.t2.data(cmod=mod.jap[[6]],which.gear="PS", effmod=emod.jap,grid.res=1,start.year=1990,end.year=2017,which.flag='Japan',observations=F)
plot.mods(input=aa,cmod=mod.jap[[6]],which.year=1995,which.month=1,grid.res=7,which.value = 'prob',which.gear='PS',plot.samples.only=F)
    

#Cape Verde

emod<- pslf[pslf$flagname == "Cape Verde",]

emod.cv <- fitGAMtoEffort(input=pslf,which.flag='Cape Verde',which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=3)

mod.cv <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.cv[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag='Cape Verde',which.species=sp,start.year=1990,end.year=2017,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.cv[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cv[[i]],which.gear="PS", effmod=emod.cv,grid.res=1,start.year=1990,end.year=2017,which.flag='Cape Verde')
    rm(aa)
    gc(reset=T)
  }
}

#Curaçao

emod.cur <- fitGAMtoEffort(input=pslf,which.flag="Curaçao",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=3)

mod.cur <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.cur[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Curaçao",which.species=sp,start.year=1990,end.year=2017,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.cur[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cur[[i]],which.gear="PS", effmod=emod.cur,grid.res=1,start.year=1990,end.year=2017,which.flag="Curaçao")
    rm(aa)
    gc(reset=T)
  }
}

#EU.Espana

emod.esp <- fitGAMtoEffort(input=pslf,which.flag="EU.España",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.esp <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.esp[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="EU.España",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.esp[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.esp[[i]],which.gear="PS", effmod=emod.esp,grid.res=1,start.year=1990,end.year=2017,which.flag="EU.España")
    rm(aa)
    gc(reset=T)
  }
}


#EU.France

emod.fra <- fitGAMtoEffort(input=pslf,which.flag="EU.France",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.fra <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.fra[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="EU.France",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.fra[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.fra[[i]],which.gear="PS", effmod=emod.fra,grid.res=1,start.year=1990,end.year=2017,which.flag="EU.France")
    rm(aa)
    gc(reset=T)
  }
}


#Ghana

emod.gha <- fitGAMtoEffort(input=pslf,which.flag="Ghana",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.gha <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.gha[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Ghana",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.gha[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.gha[[i]],which.gear="PS", effmod=emod.gha,grid.res=1,start.year=1990,end.year=2017,which.flag="Ghana")
    rm(aa)
    gc(reset=T)
  }
}



#Guatemala

emod.gua <- fitGAMtoEffort(input=pslf,which.flag="Guatemala",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.gua <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.gua[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Guatemala",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.gua[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.gua[[i]],which.gear="PS", effmod=emod.gua,grid.res=1,start.year=1990,end.year=2017,which.flag="Guatemala")
    rm(aa)
    gc(reset=T)
  }
}

##Guinee Rep.

emod.gui <- fitGAMtoEffort(input=pslf,which.flag="Guinée Rep.",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.gui <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.gui[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Guinée Rep.",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.gui[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.gui[[i]],which.gear="PS", effmod=emod.gui,grid.res=1,start.year=1990,end.year=2017,which.flag="Guinée Rep.")
    rm(aa)
    gc(reset=T)
  }
}

##Panama

emod.pan <- fitGAMtoEffort(input=pslf,which.flag="Panama",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=6)

mod.pan <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.pan[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Panama",which.species=sp,start.year=1990,end.year=2017,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.pan[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.pan[[i]],which.gear="PS", effmod=emod.pan,grid.res=1,start.year=1990,end.year=2017,which.flag="Panama")
    rm(aa)
    gc(reset=T)
  }
}


##U.S.A.

# emod.usa <- fitGAMtoEffort(input=pslf,which.flag="U.S.A.",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=3)
# 
# mod.usa <- as.list(1:9)
# for(i in 1:9)
# {
#   sp <- us[i]
#   mod.usa[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="U.S.A.",which.species=sp,start.year=1990,end.year=2017,kk=9)
#   print(sp)
# }
# 
# for(i in 1:9)
# {
#   if(mod.usa[[i]]=='Insufficient data to support model')
#   {print('no model')
#   }
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.usa[[i]],which.gear="PS", effmod=emod.usa,grid.res=1,start.year=1990,end.year=2017,which.flag="U.S.A.")
#     rm(aa)
#     gc(reset=T)
#   }
# }


##Venezuela

emod.ven <- fitGAMtoEffort(input=pslf,which.flag="Venezuela",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.ven <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.ven[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Venezuela",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.ven[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ven[[i]],which.gear="PS", effmod=emod.ven,grid.res=1,start.year=1990,end.year=2017,which.flag="Venezuela")
    rm(aa)
    gc(reset=T)
  }
}


##Mixed flags (FIS)

emod.mf <- fitGAMtoEffort(input=pslf,which.flag="Mixed flags (FIS)",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=9)

mod.mf <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.mf[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Mixed flags (FIS)",which.species=sp,start.year=1990,end.year=2017,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.mf[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.mf[[i]],which.gear="PS", effmod=emod.mf,grid.res=1,start.year=1990,end.year=2017,which.flag="Mixed flags (FIS)")
    rm(aa)
    gc(reset=T)
  }
}

### (NEI (ETRO))

#ussr <- ps.t1[ps.t1$flag %in% c("U.S.S.R.","Russian Federation"),]

emod.nei <- fitGAMtoEffort(input=pslf,which.flag="NEI (ETRO)",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=6)

mod.nei <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.nei[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="NEI (ETRO)",which.species=sp,start.year=1990,end.year=2017,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.nei[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.nei[[i]],which.gear="PS", effmod=emod.nei,grid.res=1,start.year=1990,end.year=2017,which.flag="NEI (ETRO)")
    rm(aa)
    gc(reset=T)
  }
}


## Other

emod.oth <- fitGAMtoEffort(input=pslf,which.flag="Other",which.effort='FISH.HOUR',start.year=1990,end.year=2017,kk=6)

mod.oth <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.oth[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Other",which.species=sp,start.year=1990,end.year=2017,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.oth[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.oth[[i]],which.gear="PS", effmod=emod.oth,grid.res=1,start.year=1990,end.year=2017,which.flag="Other")
    rm(aa)
    gc(reset=T)
  }
}


################# Raise the data to Task I ####################################################

#setwd('/home/doug/effdis/effdis-estimates')
# system('rm effdis-estimates.csv')
#system('cat *csv > effdis-estimates.csv')
#effdis_estimates <- read.table('effdis-estimates.csv',sep=',')

#setwd("/home/dbeare/effdis/effdis/data/effdis-estimates/PS")
setwd("D://dbeare/dbeare/effdis/data/effdis-estimates-2019/PS")


lf <- list.files()
wos <- grep('PS-1990-2017.csv',lf)
lf<-lf[wos]

effdis_estimates <- as.list(length(lf))
for( i in 1: length(lf)){
  effdis_estimates[[i]] <- read.table(lf[i],sep=",")
  print(lf[i])
}

effdis_estimates<-do.call("rbind",effdis_estimates)
dim(effdis_estimates) # = 226113 (2017)
#380168 (2019)

dimnames(effdis_estimates)[[2]] <- c("longitude","latitude","which.ocean","year","month","trend","flagname","geargrp","prob","prob.se.fit","measured_catch","measured_catch.se.fit",
                                     "eff","eff.se.fit","species","catch","cpue","observation")          

effdis_estimates <- orderBy(~trend+flagname+species,data=effdis_estimates)

# nidx <- (1:length(effdis_estimates$year))[effdis_estimates$catch > 7000000]

# Exclude these as they are way too big

# effdis_estimates <- effdis_estimates[-nidx,]

ufe <- sort(unique(as.character(effdis_estimates$flagname)))

#Check against the raw data

wf <- ufe[8]
print(wf)
mod1 <- effdis_estimates[effdis_estimates$flagname == wf & effdis_estimates$species == "bet",]
par(mfrow=c(2,1),mar=c(2,2,2,2))
#Effort
effort <- aggt2data(input=pslf,which.effort="FISH.HOUR",which.flag=wf,start.year=1990, end.year=2019)
plot(effort$trend,effort$eff1,pch=".",xlab="",ylab="")
title(wf)
points(mod1$trend,mod1$eff,col="red",pch="*")

te <- tapply(effort$eff1,effort$year,sum)
tm <- tapply(mod1$eff,mod1$year,sum)

plot(as.numeric(names(te)),te/1000000)
lines(as.numeric(names(te)),tm/1000000)


catch <- aggt2catchdata(input=pslf,which.effort="FISH.HOUR",which.flag=wf,start.year=1990, end.year=2019)
spp <- "bet"
par(mfrow=c(1,1))
plot(catch$trend[catch$species == spp],catch$raw_measured_catch[catch$species == spp],pch=".",
     xlab="",ylab="")


mod2 <- effdis_estimates[effdis_estimates$flagname == wf ,]
points(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="red",pch=".")

plot(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="green",pch="*")

xyplot(eff~trend|flagname,data=effdis_estimates[effdis_estimates$species == "bet",],pch=".")

#Which species do we not have by flag ?

n1 <-table(effdis_estimates$flagname,as.character(effdis_estimates$species))
n1 <- ifelse(n1==0,F,T)
n1

#################################
### Get Task 1 data##############
#################################

## For Purse-seine
ps.t1 <- get.effdis.t1.data(which.dsn='effdis-local',which.gear = 'PS',which.region='AT',which.flag='All')
str(ps.t1)
for(i in c(3,4,5,8:19,21)){ps.t1[,i] <- as.character(ps.t1[,i])}
ps.t1<- ps.t1[ps.t1$yearc > 1989,]
summary(ps.t1)


# Catchtypecode is either C (catch), L (Landings), DD (Discards), DM (?) and Landings

# If catch is available use that otherwise sum landings and discards.

ps.t1.c <- ps.t1[ps.t1$catchtypecode == "C",]

ps.t1.ld <- ps.t1[ps.t1$catchtypecode %in% c("DD","DM","L"),]

ps.t1.ld <- aggregate(qty_t~rownames+species+yearc+decade+status+flag+fleet+
                        stock+region+area+spcgeargrp+geargrp+gearcode,data=ps.t1.ld,sum,na.rm=T)

ps.t1<-rbind(ps.t1.c[,match(colnames(ps.t1.ld),colnames(ps.t1.c))],ps.t1.ld)

ps.t1 <- orderBy(~yearc,data=ps.t1)

# Plot Task 1 data for selected flags

# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Brazil",],type="p")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Belize",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="EU.España",],type="l",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="EU.Portugal",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Japan",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Panama",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="South Africa",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="U.S.A.",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Venezuela",],type="p",pch="*")
# xyplot(qty_t~yearc|species,groups=fleet,data=ps.t1[ps.t1$flag=="Other",],type="p",pch="*"
# 

#Convert Task 1 to 'Other

uf<-sort(unique(pslf$flagname[pslf$year > 1989]))
uf2 <- uf

uf1 <- as.character(sort(unique(ps.t1$flag)))
uf1

# [1] "Angola"              "Argentina"           "Belize"              "Brazil"              "Canada"              "Cape Verde"          "Cayman Islands"     
# [8] "Colombia"            "Congo"               "Côte D'Ivoire"       "Cuba"                "Curaçao"             "EU.España"           "EU.France"          
# [15] "EU.Portugal"         "EU.United Kingdom"   "Ghana"               "Guatemala"           "Guinea Ecuatorial"   "Guinée Rep."         "Japan"              
# [22] "Korea Rep."          "Libya"               "Maroc"               "Mixed flags (FR+ES)" "NEI (ETRO)"          "NEI (Flag related)"  "Norway"             
# [29] "Panama"              "Russian Federation"  "Senegal"             "South Africa"        "S. Tomé e Príncipe"  "U.S.A."              "U.S.S.R."           
# [36] "Venezuela"          


ps.t1$flag <- as.character(ps.t1$flag)

# Remove Brazil

ps.t1 <- ps.t1[ps.t1$flag != 'Brazil',]


mm <- match(ps.t1$flag,uf2)
ps.t1$flag[is.na(mm)] <- 'Other'

w1 <- (1:length(ps.t1[,1]))[ps.t1$yearc == 2014]
round(tapply(ps.t1$qty_t[w1],list(ps.t1$flag[w1],ps.t1$species[w1]),sum,na.rm=T))

# See task 1 totals by species

t1x <- aggregate(list(qty_t=ps.t1$qty_t),list(year=ps.t1$yearc,species = ps.t1$species),sum,na.rm=T)
t1x[t1x$species == 'BFT',]
t1x[t1x$species == 'BET',]
t1x[t1x$species == 'YFT',]
t1x[t1x$species == 'SKJ',]


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
head(effdis_estimates)
effdis_estimates$catch <- effdis_estimates$catch/1000

effdis_estimates$measured_catch <- effdis_estimates$measured_catch/1000
effdis_estimates$measured_catch.se.fit <- effdis_estimates$measured_catch.se.fit/1000
effdis_estimates$cpue <- effdis_estimates$catch/effdis_estimates$eff # Re-calculate by tonne

# Start combining Task 1 and 2

# Catch

catch_by_year_flag <- aggregate(list(measured_catch=effdis_estimates$measured_catch,catch=effdis_estimates$catch), 
                                by=list(year=effdis_estimates$year,flagname=effdis_estimates$flagname),sum,na.rm=T)

effdis_estimates$flagname <- as.character(effdis_estimates$flagname)
effdis_estimates$species <- as.character(effdis_estimates$species)


# Create modeled effort file - we have this problem that effort is repeated in the long format.

effdis_estimates.skj <- effdis_estimates[effdis_estimates$species == 'skj',] # Just take the skj since all countries catch them

z5 <- data.frame(eff=effdis_estimates.skj$eff,longitude=effdis_estimates.skj$longitude,latitude=effdis_estimates.skj$latitude,year=effdis_estimates.skj$year,
                 month=effdis_estimates.skj$month,trend=effdis_estimates.skj$trend,flagname =effdis_estimates.skj$flagname) 

effort_by_year_flag <- aggregate(list(eff=z5$eff), 
                                 by=list(year=z5$year,flagname=z5$flagname)
                                 ,sum,na.rm=T)

# Total catches of tunas

xyplot(catch~year|flagname,data=catch_by_year_flag,type="l",
       scales= list(relation="free"))

# Total Task 2 modeled effort

xyplot(eff~year|flagname,data=effort_by_year_flag,type="l",
       scales= list(relation="free"))


# Sum Task 1 over year and flag

sum.t1 <- aggregate(list(qty_t=ps.t1$qty_t),list(year=ps.t1$yearc,flagname=ps.t1$flag),sum,na.rm=T)

## Merge task1 and task 2
# Put on modeled catch

sum.t1.t2 <-merge(sum.t1,catch_by_year_flag)

# Put on effort

all.t1.t2 <- merge(effort_by_year_flag,sum.t1.t2)

all.t1.t2$cpue <- all.t1.t2$catch/all.t1.t2$eff

plot(all.t1.t2$year,log(all.t1.t2$cpue),type="n")
text(all.t1.t2$year,log(all.t1.t2$cpue),as.character(all.t1.t2$flagname),cex=.5)

all.t1.t2$raised_effort <- all.t1.t2$qty_t/all.t1.t2$cpue

all.t1.t2$rf <- all.t1.t2$raised_effort/all.t1.t2$eff

# Put the raising factor on effdis estimates

m1 <- paste(effdis_estimates$year,effdis_estimates$flagname)
m2 <- paste(all.t1.t2$year,all.t1.t2$flagname)

nrf <- all.t1.t2$rf[match(m1,m2)]
effdis_estimates$rf <- nrf

effdis_estimates$fishhourEst <- effdis_estimates$eff*effdis_estimates$rf

effdis_estimates$fishhourObs <- effdis_estimates$eff

# # Put the raising factor on effdis EFFORT only estimates, in this case the skipjack only data

# 
 m1 <- paste(z5$year,z5$flagname)
 m2 <- paste(all.t1.t2$year,all.t1.t2$flagname)
# 
 nrf <- effdis_estimates$rf[match(m1,m2)]
 z5$rf <- nrf
# 
 z5$fishhourEst <- z5$eff*z5$rf
# 
 z5$fishhourObs <- z5$eff
 z5 <- z5[,-1]

library(lattice)

xyplot(raised_effort~year|flagname,data=all.t1.t2,type='b',
       scales= list(relation="free"))


# #write.table(big2,'/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',row.names=F)
# 
# xxx <- read.table('/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',header=T)
# xxx1 <- effdis_estimates2[effdis_estimates2$flagname == 'Japan',]
# 
# par(mfrow=c(2,1))
# plot(effdis_estimates2$year[effdis_estimates2$flagname == 'Japan'],
#      effdis_estimates2$raised.effort[effdis_estimates2$flagname == 'Japan']/1000000/9,type='l',ylim=c(0,120))
# 
# plot(effdis_estimates2$year[effdis_estimates2$flagname == 'Japan'],
#      effdis_estimates2$raw_raised.effort[effdis_estimates2$flagname == 'Japan']/1000000,type='l',ylim=c(0,120))


z5.by.year.flag.sum <- aggregate(list(fishhourEst=z5$fishhourEst),by=list(flagname=z5$flagname,year=z5$year),sum,na.rm=T)

p1 <- ggplot(data = all.t1.t2, aes(x = year, y = raised_effort)) +
  geom_line() +
  facet_wrap(~ flagname, scales = "free_y") +
  theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1))

p1

ggsave(p1,file="EffdisEffortEstimatesByCountry2019.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")

z5.ps.sum <- aggregate(list(fishhourEst=z5$fishhourEst),
                       by=list(year=z5$year),sum,na.rm=T)

zz5 <- read.table("D:\\dbeare/dbeare/effdis/data/effdis-estimates-2017/effdis_ps_1990_2015.csv",sep=',',header=T)
zz5 <- effdis_ps.1990.2014

zz5.ps.sum <- aggregate(list(fishhourEst=zz5$fishhourEst),
                           by=list(year=zz5$year),sum,na.rm=T)

# Get rid of some really odd large numbers:

bn <- log(zz5.ps.sum$fishhourEst)
fo <-(1:length(bn))[bn>14]
all.t1.t2.sum$raised_effort[fo] <- NA


p <- ggplot() + 
  geom_line(data = z5.ps.sum, aes(x = year, y = fishhourEst)) +
  geom_line(data = zz5.ps.sum, aes(x = year, y = fishhourEst),col='red') +
  ggtitle("Total number of fishing hours raised with Task I data\n")+
  xlab('Year') +
  ylab('No Hours')+
  theme(axis.text.x = element_text(angle = -30, hjust =0,size=15),
        plot.title = element_text(lineheight=1.2, face="bold",size = 17, colour = "grey20"),
        panel.border = element_rect(colour = "black",fill=F,size=0.5),
        panel.grid.major = element_line(colour = "grey",size=0.75,linetype='longdash'),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(size=15,colour="grey20"),
        axis.title.x=element_text(size=15,colour="grey20"),
        axis.text.y=element_text(size=15,colour="grey20"),
        panel.background = element_rect(fill = NA,colour = "black"))

p

ggsave(p,file="GlobalEffdisEstimates2019.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")


z5$geargrp <- "PS"

write.table(z5,file="effdis_ps.1990.2019.csv",sep=",",row.names=F)

effdis_ps.1990.2019<-read.table("effdis_ps.1990.2019.csv",sep=",",header=T)

effdis_ps.1990.2014<-read.table("D://Dropbox/AOTTP/DataExploration/ICCAT-Task2-Database/effdis_ps.1990.2014.csv",sep=",",header=T)

plot(effdis_ps.1990.2014$trend,effdis_ps.1990.2014$fishhourEst,ylim=c(0,30000),xlim=c(480,820))
points(effdis_ps.1990.2019$trend,effdis_ps.1990.2019$fishhourEst,col='red')

xyplot(fishhourEst~trend,groups=flagname,data=effdis_ps.1990.2014)
xyplot(fishhourEst~trend,groups=flagname,data=effdis_ps.1990.2019)

# Crazy shit going on with 'Other' group.

chan <- odbcConnect("effdis-local", case="postgresql", believeNRows=FALSE)
sqlQuery(chan,'drop table effdis_ps_1980_2014')
sqlSave(chan,z5,tablename='effdis_ps_1980_2014')


######################################################################################
######## Longliners  #################################################################
######################################################################################

setwd("/home/dbeare/effdis/effdis/data/effdis-estimates/LL")

setwd("D://dbeare/dbeare/effdis/data/effdis-estimates-2019/LL")


# # Get data for each dsettype
# 
 lln  <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = 'n-')
 llnw <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = 'nw')
 llw  <- get.effdis.t2.data(which.dsn='effdis-local',which.gear='LL',which.flag='All',which.dsettype = '-w')
# 
 ll1 <- rbind(lln,llnw,llw)
 dim(ll1) #= 307389 (2019)
 
 ll1nhks <- ll1[ll1$eff1type=="NO.HOOKS",]
 head(ll1nhks)
 
 nhks.y <- aggregate(list(eff1=ll1nhks$eff1),list(year=ll1nhks$year),sum,na.rm=T)
 nhks.y
 
 
 chan <- odbcConnect('effdis-local', case="postgresql", believeNRows=FALSE) # Connect to database
 
 which.gear <- "LL"
 which.region <- "AT"
 which.effort <- "NO.HOOKS"

 query <- paste0("SELECT yearc AS year, trend, timeperiodid AS month, flagname, fleetcode,region, squaretypecode,quadid,
geargrpcode,lon,lat,longitude,latitude, catchunit, dsettype, eff1, eff1type,
                 alb,bft,bet,skj,yft,swo,bum,sai,whm,totsp9
                 FROM t2ce_2019
                 WHERE region =","'",which.region,"'","AND timeperiodid < 13 AND geargrpcode=","'",which.gear,"'")
 
 out <- sqlQuery(chan,query)
 
 odbcClose(chan)
 
 out<- out[out$flagname != 'St. Vincent and Grenadines',]
 
 nhks.y1 <- aggregate(list(eff1=out$eff1),list(year=out$year),sum,na.rm=T)
 plot(nhks.y1$year,nhks.y1$eff1,type='l',lwd=2,col='black',xlim=c(1950,2020),xlab="",ylab='#hooks');
 abline(v=seq(1950,2020,by=10),lty=2,col='blue')
 title('NO.HOOKS SUM OVER YEAR TASK 2 - No St Vincent & Grenadines')
 
 
 nhks.y1.f <- aggregate(list(eff1=out$eff1),list(year=out$year,flag=out$flagname),sum,na.rm=T)
 nhks.y1.f
 
 xyplot(eff1~year|flag,data=nhks.y1.f,type='l',lwd=3)
 
# 
# # Double-check that lat and long conversions have been done properly #
# df <- data.frame(quad=ll1$quadid,lat=ll1$lat,lon=ll1$lon,square=ll1$squaretypecode) # simplify data.frame
# df$square <- as.character(df$square) # function requires input to be 'character'
# hl <- length(df[,1])
# ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
# for(i in 1:hl){
#   ndf[i,] <- latLon(df[i,]) # calculate the central lat long of each grid looping thro' each row
# }
# plot(ndf$lon,ndf$lat,pch=".")
# 
 # ll1$longitude <- ndf$lon # add on lat and long to main database
 # ll1$latitude <- ndf$lat

# Set up data for modeled estimate (have to have some spatial detail)
ll1 <- convert.grid.res(ll1) # Some data, eg. Portugal are supplied at 1x1 but for the modeling we need to be consistent.
data("seas")
ll1<-find.ocean(ll1)
ll1 <- ll1[ll1$which.ocean == 'atl',]
#table(ll1$squaretypecode,ll1$flagname)
ll1 <- ll1[ll1$squaretypecode == '5x5',]
ll1<-prepare.effdis.data(input=ll1)

lllf <- convert2long.format.t2(input =ll1) # NB I do this not because it matters for the modeling but because I need to know where the data were and want to do predictions on an ordely grid?

bm <- model.nos.kgs(input=lllf,which.gear='LL')
lllf <- kgs.from.nos(lllf) # for those fleets that supply only number
#lllf <- lllf[lllf$eff1type=='NO.HOOKS',] # Quite a no of eff1type = '-none-' but they are important e.g. Spanish LL so leave them in here, model them and use the model to predict effort at the places where it is none or something else.
lllf <- lllf[lllf$catchunit == "kg",]
lllf$species <- as.character(lllf$species)
# 
 dim(lllf) #= 2079549 (2010) 1900872 (2015) 1944441 (2019)

# # # Set up data to estimate from the raw data
# data("seas")
# ll2<-find.ocean(ll1)
# ll2 <- ll2[ll2$which.ocean == 'atl',]
# lllf2 <- convert2long.format.t2(input =ll2)
# lllf2<-prepare.effdis.data(input=lllf2)
# bm2 <- model.nos.kgs(input=lllf2,which.gear='LL')
# lllf2 <- kgs.from.nos(lllf2) # for those fleets that supply only number
# lllf2 <- lllf2[lllf2$eff1type=='NO.HOOKS',]
# dim(lllf2) #= 

us <- sort(as.character(unique(lllf$species)))
uf <- sort(as.character(unique(lllf$flagname)))
uf

#     "Angola"                        "Argentina"                     "Barbados"                      "Belize"                       
# [5] "Brazil"                        "Canada"                        "Cape Verde"                    "China (ICCAT program)"        
# [9] "China PR"                      "Chinese Taipei"                "Chinese Taipei (foreign obs.)" "Côte D'Ivoire"                
# [13] "Cuba"                          "Cuba (ICCAT program)"          "Dominica"                      "EU.España"                    
# [17] "EU.France"                     "EU.Portugal"                   "EU.United Kingdom"             "Faroe Islands"                
# [21] "FR.St Pierre et Miquelon"      "Grenada"                       "Guinea Ecuatorial"             "Honduras"                     
# [25] "Iceland"                       "Japan"                         "Japan (foreign obs.)"          "Korea Rep."                   
# [29] "Maroc"                         "Mexico"                        "Mixed flags (KR+PA)"           "Namibia"                      
# [33] "Panama"                        "Philippines"                   "Senegal"                       "Sierra Leone"                 
# [37] "South Africa"                  "St. Vincent and Grenadines"    "Trinidad and Tobago"           "U.S.A."                       
# [41] "U.S.S.R."                      "UK.Bermuda"                    "UK.Sta Helena"                 "UK.Turks and Caicos"          
# [45] "Uruguay"                       "Vanuatu"                       "Venezuela"                    

# Make Cuba and Cuba (ICCAT program) the same

wic <- (1:length(lllf[,1]))[lllf$flagname == 'Cuba (ICCAT program)']
lllf$flagname[wic] <- 'Cuba'


#Flags to add to "others"- 


oth <-c("Argentina", "Angola", "Barbados", "Cape Verde","China (ICCAT program)", "Côte D'Ivoire","EU.France",
        "Chinese Taipei (foreign obs.)" ,"Dominica","EU.United Kingdom","Faroe Islands",
        "FR.St Pierre et Miquelon","Grenada","Guinea Ecuatorial","Honduras","Iceland","Japan (foreign obs.)",
        "Maroc","Mexico","Philippines","Sierra Leone","Trinidad and Tobago","UK.Sta Helena","UK.Bermuda",
        "Mixed flags (KR+PA)","UK.Turks and Caicos","Uruguay")

lllf$flagname <- as.character(lllf$flagname)

idx <- (1:length(lllf[,1]))[lllf$flagname %in% oth]

lllf$flagname[idx] <- "Other"

port <- lllf[lllf$flagname == "EU.Portugal",]
plot(port$trend,port$eff1)
plot(port$year,port$eff1)
hist(port$eff1))

spain <- lllf[lllf$flagname == "EU.España",] 
plot(spain$trend,spain$eff1)
points(port$trend,port$eff1,col='red')
xyplot(measured_catch/1000~trend|species,data=spain)
xyplot(measured_catch/1000~trend|species,data=port)

xyplot(eff1~trend|species,data=spain)


japan <- lllf[lllf$flagname == "Japan",]
plot(japan$trend,japan$eff1)
points(port$trend,port$eff1,col='red')

#Argentina

#emod.arg <- fitGAMtoEffort(input=lllf,which.flag='Argentina',which.effort='NO.HOOKS',start.year=1990,end.year=2017,kk=6)
# 
# mod.arg <- as.list(1:9)
# for(i in 1:9)
# {
#   sp <- us[i]
#   mod.arg[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Argentina',which.species=sp,start.year=1990,end.year=2017,kk=3)
#   print(sp)
# }
# 
# for(i in 1:9)
# {
#   if(mod.arg[[i]]=='Insufficient data to support model')
#   {print('no model')
#   }
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.arg[[i]], effmod=emod.arg,grid.res=5,start.year=1990,end.year=2017,which.flag='Argentina')
#     rm(aa)
#     gc(reset=T)
#   }
# }

# Not enough data for Argentina


#Japan

emod.jap <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)

mod.jap <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.jap[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species=sp,start.year=1950,end.year=2017,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.jap[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.jap[[i]], effmod=emod.jap,grid.res=5,start.year=1950,end.year=2017,which.flag='Japan')
    rm(aa)
    gc(reset=T)
  }
}


#Belize

belize <- lllf[lllf$flagname == "Belize" & lllf$species == "alb",]
plot(belize$trend,belize$eff1)
belize.big <- belize[belize$eff1>1000000,]

plot(lllf$trend,lllf$eff1)
points(belize.big$trend,belize.big$eff1,col='red')


# Divide Belize data by 10 for 2010 and 2012

q0 <- (1:length(lllf$flagname))[lllf$flagname == "Belize" & lllf$year %in% c(2010,2012)]
lllf$eff1[q0] <- lllf$eff1[q0]/10

emod.belize <- fitGAMtoEffort(input=lllf,which.flag='Belize',which.effort='NO.HOOKS',start.year=1990,end.year=2017,kk=3)

mod.belize <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.belize[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Belize',which.species=sp,start.year=1990,end.year=2017,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.belize[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.belize[[i]], effmod=emod.belize,grid.res=5,start.year=1990,end.year=2017,which.flag='Belize')
    rm(aa)
    gc(reset=T)
  }
}

#Brasil

emod.brasil <- fitGAMtoEffort(input=lllf,which.flag='Brazil',which.effort='NO.HOOKS',start.year=1950,end.year=2017,k=3)

mod.brasil <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.brasil[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Brazil',which.species=sp,start.year=1950,end.year=2017)
  print(sp)
}
  
for(i in 1:9)
{
  if(mod.brasil[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
  aa <- predict.effdis.t2.data(cmod=mod.brasil[[i]], effmod=emod.brasil,grid.res=5,start.year=1950,end.year=2017,which.flag='Brazil')
  rm(aa)
  gc(reset=T)
}
}

#Canada

emod.canada <- fitGAMtoEffort(input=lllf,which.flag='Canada',which.effort=c('-none-','NO.HOOKS'),start.year=1950,end.year=2017,k=3)

mod.can <- as.list(1:9)
for(i in c(1,2,4:7,9))
{
  sp <- us[i]
  mod.can[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Canada',which.species=sp,start.year=1950,end.year=2017,k=3)
  print(sp)
}

for(i in c(1,2,4:7,9))
{
  if(mod.can[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.can[[i]], effmod=emod.canada,grid.res=5,start.year=1950,end.year=2017,which.flag='Canada')
    rm(aa)
    gc(reset=T)
   
  }
}

#China P.R

emod.china.pr <- fitGAMtoEffort(input=lllf,which.flag='China PR',which.effort='NO.HOOKS',start.year=1950,end.year=2017)

mod.china.pr <- as.list(1:9)

for(i in c(1:2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.china.pr[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='China PR',which.species=sp,start.year=1950,end.year=2017,kk=6)
}

for(i in c(1:2,4:9))
{
  if(mod.china.pr[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.china.pr[[i]], effmod=emod.china.pr,grid.res=5,start.year=1950,end.year=2017,which.flag='China PR')
    rm(aa)
    gc(reset=T)
  }
}

#Chinese Taipei

emod.ct <- fitGAMtoEffort(input=lllf,which.flag='Chinese Taipei',which.effort='NO.HOOKS',start.year=1950,end.year=2017)

mod.ct <- as.list(1:9)

for(i in c(1,2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.ct[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Chinese Taipei',which.species=sp,start.year=1950,end.year=2017,kk=6)
}

for(i in c(1,2,4:9))
{
  if(mod.ct[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ct[[i]], effmod=emod.ct,grid.res=5,start.year=1950,end.year=2017,which.flag='Chinese Taipei')
    rm(aa)
    gc(reset=T)
  }
}

#### Cuba

 emod.cuba <- fitGAMtoEffort(input=lllf,which.flag='Cuba',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)
# 
mod.cuba <- as.list(1:9)

for(i in c(1:6,8:9))
{
  sp <- us[i]
  print(sp)
  mod.cuba[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Cuba',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

for(i in c(1:6,8:9))
{
  if(mod.cuba[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cuba[[i]], effmod=emod.cuba,grid.res=5,start.year=1950,end.year=2017,which.flag='Cuba')
    rm(aa)
    gc(reset=T)
  }
}

### Spain

emod.spain <- fitGAMtoEffort(input=lllf,which.flag='EU.España',which.effort=c('-none-','NO.HOOKS'),start.year=1950,end.year=2017,kk=9)

mod.spain <- as.list(1:9)

for(i in 1:9)
{
  sp<-us[i]
  print(sp)
  mod.spain[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag="EU.España",which.species=sp,start.year=1950,end.year=2017,kk=9)
}

for(i in 1:9)
{
  if(mod.spain[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.spain[[i]], effmod=emod.spain,grid.res=5,start.year=1950,end.year=2017,which.flag='EU.España')
    rm(aa)
    gc(reset=T)
  }
}


### Portugal


# 2009-2011 numbers are just too big for Port so divide by 100.


#s0 <- (1:length(lllf$flagname))[lllf$flagname == "EU.Portugal" & lllf$year %in% c(2009,2010,2011)]
#lllf$eff1[s0] <- lllf$eff1[s0]/10




emod.port <- fitGAMtoEffort(input=lllf,which.flag='EU.Portugal',which.effort=c('-none-','NO.HOOKS'),start.year=1950,end.year=2017,kk=2)

mod.port <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.port[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='EU.Portugal',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

for(i in 1:9)
{
  if(mod.port[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.port[[i]], effmod=emod.port,grid.res=5,start.year=1950,end.year=2017,which.flag='EU.Portugal')
    rm(aa)
    gc(reset=T)
  }
}




# Korea

emod.kor <- fitGAMtoEffort(input=lllf,which.flag='Korea Rep.',which.effort='NO.HOOKS',start.year=1950,end.year=2017)

mod.kor <- as.list(1:9)

for(i in c(1,2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.kor[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Korea Rep.',which.species=sp,start.year=1950,end.year=2017,kk=5)
}

for(i in c(1,2,4:9))
{
  if(mod.kor[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.kor[[i]], effmod=emod.kor,grid.res=5,start.year=1950,end.year=2017,which.flag='Korea Rep.')
    rm(aa)
    gc(reset=T)
  }
}

#Need to investigate Maroc


# Namibia

emod.nam <- fitGAMtoEffort(input=lllf,which.flag='Namibia',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)

mod.nam <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.nam[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Namibia',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.nam[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.nam[[i]], effmod=emod.nam,grid.res=5,start.year=1950,end.year=2017,which.flag='Namibia')
    rm(aa)
    gc(reset=T)
  }
}

### Panama ########

emod.pan <- fitGAMtoEffort(input=lllf,which.flag='Panama',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)

mod.pan <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.pan[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Panama',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.pan[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.pan[[i]], effmod=emod.pan,grid.res=5,start.year=1950,end.year=2017,which.flag='Panama')
    rm(aa)
    gc(reset=T)
  }
}


# Other

emod.oth <- fitGAMtoEffort(input=lllf,which.flag='Other',which.effort=c('-none-','NO.HOOKS'),start.year=1950,end.year=2017,kk=6)

mod.oth <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.oth[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Other',which.species=sp,start.year=1950,end.year=2017,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.oth[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.oth[[i]], effmod=emod.oth,grid.res=5,start.year=1950,end.year=2017,which.flag='Other')
    rm(aa)
    gc(reset=T)
  }
}

# South Africa

emod.sa <- fitGAMtoEffort(input=lllf,which.flag='South Africa',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)

mod.sa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.sa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='South Africa',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.sa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.sa[[i]], effmod=emod.sa,grid.res=5,start.year=1950,end.year=2017,which.flag='South Africa')
    rm(aa)
    gc(reset=T)
  }
}

# Senegal


# senegal <- lllf[lllf$flagname == "Senegal",]
# 
# emod.sen <- fitGAMtoEffort(input=lllf,which.flag='Senegal',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)
# 
# mod.sen <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.sen[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Senegal',which.species=sp,start.year=1950,end.year=2017,kk=3)
# }


# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.sen[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.sen[[i]], effmod=emod.sen,grid.res=5,start.year=1950,end.year=2017,which.flag='Senegal')
#     rm(aa)
#     gc(reset=T)
#   }
# }
# 

# St Vincent & Grenadines

#setwd('/home/doug/effdis/effdis-estimates')

vg <- lllf[lllf$flagname == "St. Vincent and Grenadines" & lllf$species == "alb" ,]
plot(vg$year,vg$eff1)
vg.big <- vg[vg$eff1>1000000,]
plot(vg.big$trend,vg.big$eff1)

# 2012-2013 numbers are just too big for St V&G so divide by 10.


s0 <- (1:length(lllf$flagname))[lllf$flagname == "St. Vincent and Grenadines" & lllf$year %in% c(2012,2013,2014)]
lllf$eff1[s0] <- lllf$eff1[s0]/10


emod.svg <- fitGAMtoEffort(input=lllf,which.flag='St. Vincent and Grenadines',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)

mod.svg <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.svg[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='St. Vincent and Grenadines',which.species=sp,start.year=1950,end.year=2017,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.svg[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.svg[[i]], effmod=emod.svg,grid.res=5,start.year=1950,end.year=2017,which.flag='St. Vincent and Grenadines')
    rm(aa)
    gc(reset=T)
  }
}


# Trinidad & Tobago

#emod.tri <- fitGAMtoEffort(input=lllf,which.flag='Trinidad and Tobago',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)
# 
# mod.tri <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.tri[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Trinidad and Tobago',which.species=sp,start.year=1950,end.year=2017,kk=3)
# }
# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.tri[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.tri[[i]], effmod=emod.tri,grid.res=5,start.year=1950,end.year=2017,which.flag='Trinidad and Tobago')
#     rm(aa)
#     gc(reset=T)
#   }
# }

# Uruguay

# emod.uru <- fitGAMtoEffort(input=lllf,which.flag='Uruguay',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=6)
# 
# mod.uru <- as.list(1:9)
# for(i in c(1,2,3,4,5:8))
# {
#   sp <- us[i]
#   print(sp)
#   mod.uru[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Uruguay',which.species=sp,start.year=1950,end.year=2017,kk=3)
# }
# 
# for(i in c(1,2,3,4:8))
# {
#   if(mod.uru[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.uru[[i]], effmod=emod.uru,grid.res=5,start.year=1950,end.year=2017,which.flag='Uruguay')
#     gc(reset=T)
#   }
# }


# U.S.A.

emod.usa <- fitGAMtoEffort(input=lllf,which.flag='U.S.A.',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=9)

mod.usa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.usa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.A.',which.species=sp,start.year=1950,end.year=2017,kk=9)
}

for(i in c(1,2,3,4:9))
{
  if(mod.usa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.usa[[i]], effmod=emod.usa,grid.res=5,start.year=1950,end.year=2017,which.flag='U.S.A.')
    gc(reset=T)
  }
}


# # U.S.S.R.

 emod.uss <- fitGAMtoEffort(input=lllf,which.flag='U.S.S.R.',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)

 mod.uss <- as.list(1:9)
 for(i in c(1,2,3,4,5:9))
 {
   sp <- us[i]
   print(sp)
   mod.uss[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.S.R.',which.species=sp,start.year=1950,end.year=2017,kk=3)
 }
# 
 for(i in c(1,2,3,4:9))
 {
   if(mod.uss[[i]]=='Insufficient data to support model')
   {print('no model')}
   else{
     aa <- predict.effdis.t2.data(cmod=mod.uss[[i]], effmod=emod.uss,grid.res=5,start.year=1950,end.year=2017,which.flag='U.S.S.R.')
     gc(reset=T)
   }
 }

# Vanuatu.

emod.van <- fitGAMtoEffort(input=lllf,which.flag='Vanuatu',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)

mod.van <- as.list(1:9)
for(i in c(1,2,3,5:9))
{
  sp <- us[i]
  print(sp)
  mod.van[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Vanuatu',which.species=sp,start.year=1950,end.year=2017,kk=3)
}
mod.van[[4]] <- 'Insufficient data to support model'

for(i in c(1,2,3,5:9))
{
  if(mod.van[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.van[[i]], effmod=emod.van,grid.res=5,start.year=1950,end.year=2017,which.flag='Vanuatu')
    gc(reset=T)
  }
}


# Venezuela.

emod.ven <- fitGAMtoEffort(input=lllf,which.flag='Venezuela',which.effort='NO.HOOKS',start.year=1950,end.year=2017,kk=3)

mod.ven <- as.list(1:9)
for(i in c(1,2,3,4,5,7:9))
{
  sp <- us[i]
  print(sp)
  mod.ven[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Venezuela',which.species=sp,start.year=1950,end.year=2017,kk=3)
}

mod.ven[[6]] <- 'Insufficient data to support model'


for(i in c(1,2,3,4:5,7:9))
{
  if(mod.ven[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ven[[i]], effmod=emod.ven,grid.res=5,start.year=1950,end.year=2017,which.flag='Venezuela')
    gc(reset=T)
    
  }}

################# Raise the data to Task I ####################################################


#setwd('/home/doug/effdis/effdis-estimates')
# system('rm effdis-estimates.csv')
#system('cat *csv > effdis-estimates.csv')
#effdis_estimates <- read.table('effdis-estimates.csv',sep=',')
#setwd("/home/dbeare/effdis/effdis/data/effdis-estimates/LL")

setwd("D://dbeare/dbeare/effdis/data/effdis-estimates-2019/LL")


lf <- list.files()
lf<- lf[grep('model',lf)]

effdis_estimates <- as.list(length(lf))
for( i in 1: length(lf)){
  effdis_estimates[[i]] <- read.table(lf[i],sep=",")
  print(lf[i])
}

effdis_estimates<-do.call("rbind",effdis_estimates)
dim(effdis_estimates) # = 891599 (2017) = 936186 (2019)


dimnames(effdis_estimates)[[2]] <- c("longitude","latitude","which.ocean","year","month","trend","flagname","geargrp","prob","prob.se.fit","measured_catch","measured_catch.se.fit",
                                     "eff","eff.se.fit","species","catch","cpue","observation")          

effdis_estimates <- orderBy(~trend+flagname+species,data=effdis_estimates)

summary(effdis_estimates$catch)
summary(effdis_estimates$eff)


japan <- effdis_estimates[effdis_estimates$flagname == 'Japan',]

# Chuck out Belize as there is something odd about them ??? Do it right at the end ?

#xyplot(measured_catch~trend|flagname,data=lllf)

#xyplot(catch~trend|flagname,data=effdis_estimates) # catches by flag

nidx <- (1:length(effdis_estimates$year))[effdis_estimates$species == "skj" & effdis_estimates$catch > 150000 ]

# Exclude these as they are way too big

effdis_estimates <- effdis_estimates[-nidx,]

ufe <- sort(unique(as.character(effdis_estimates$flagname)))

#Check against the raw data

wf <- ufe[9]
print(wf)
mod1 <- effdis_estimates[effdis_estimates$flagname == wf & effdis_estimates$species == "bet",]
par(mfrow=c(1,1),mar=c(4,4,2,2))
#Effort
effort <- aggt2data(input=lllf,which.effort=c("NO.HOOKS","-none-"),which.flag=wf,start.year=1950, end.year=2019)
plot(effort$trend,effort$eff1,pch=16,xlab="",ylab="",xaxt='n')
abline(v=seq(1,816,12),lty=2,col='blue')
title(wf)
points(mod1$trend,mod1$eff,col="red",pch=16)



te <- tapply(effort$eff1,effort$year,sum)
tm <- tapply(mod1$eff,mod1$year,sum)

plot(as.numeric(names(te)),te/1000000)
lines(as.numeric(names(tm)),tm/1000000)


catch <- aggt2catchdata(input=lllf,which.effort="NO.HOOKS",which.flag=wf,start.year=1950, end.year=2019)
spp <- "alb"
par(mfrow=c(1,1))
plot(catch$trend[catch$species == spp],catch$raw_measured_catch[catch$species == spp],pch=".",
     xlab="",ylab="")
mod2 <- effdis_estimates[effdis_estimates$flagname == wf ,]
points(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="red",pch=".")

plot(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="green",pch="*")

xyplot(eff/1000000~trend|flagname,data=effdis_estimates[effdis_estimates$species == "bet",],pch=".")


#Which species do we not have by flag ?

n1 <-table(effdis_estimates$flagname,as.character(effdis_estimates$species))
n1 <- ifelse(n1==0,F,T)
n1

##############################################################################################################
##### Get Task 1 data #####
##### For Long-line #######
##############################################################################################################

ll.t1 <- get.effdis.t1.data(which.dsn='effdis-local',which.gear = 'LL',which.region='AT',which.flag='All')
str(ll.t1)
for(i in c(3:5,8:19,21)){ll.t1[,i] <- as.character(ll.t1[,i])}
str(ll.t1)
table(ll.t1$flag)
summary(ll.t1)
# If catch is available use that otherwise sum landings and discards.

ll.t1.c <- ll.t1[ll.t1$catchtypecode == "C",]

ll.t1.ld <- ll.t1[ll.t1$catchtypecode %in% c("DD","DM","L"),]
ll.t1.ld <- aggregate(qty_t~rownames+species+yearc+decade+status+flag+fleet+
                        stock+region+area+spcgeargrp+geargrp+gearcode,data=ll.t1.ld,sum,na.rm=T)

ll.t1<-rbind(ll.t1.c[,match(colnames(ll.t1.ld),colnames(ll.t1.c))],ll.t1.ld)


ll.t1 <- orderBy(~yearc,data=ll.t1)

# Plot Task 1 data for selected flags

xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Brazil",],type="p")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Belize",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="China PR",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Chinese Taipei",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Cuba",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="EU.España",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="EU.Portugal",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Japan",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Korea Rep.",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Namibia",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Panama",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Philippines",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="South Africa",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag== "St. Vincent and Grenadines",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="U.S.A.",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag== "Vanuatu",],type="p",pch="*")
xyplot(qty_t~yearc|species,groups=fleet,data=ll.t1[ll.t1$flag=="Venezuela",],type="p",pch="*")


#Convert Task 1 to 'Other

uf<-sort(unique(lllf$flagname))

uf2 <- uf

uf1 <- as.character(sort(unique(ll.t1$flag)))

# "Angola"                     "Barbados"                   "Belize"                    
# [4] "Brazil"                     "Cambodia"                   "Canada"                    
# [7] "Cape Verde"                 "China PR"                   "Chinese Taipei"            
# [10] "Côte D'Ivoire"              "Cuba"                       "Dominica"                  
# [13] "EU.España"                  "EU.France"                  "EU.Ireland"                
# [16] "EU.Portugal"                "EU.United Kingdom"          "Faroe Islands"             
# [19] "FR.St Pierre et Miquelon"   "Grenada"                    "Guinea Ecuatorial"         
# [22] "Honduras"                   "Iceland"                    "Japan"                     
# [25] "Korea Rep."                 "Libya"                      "Maroc"                     
# [28] "Mexico"                     "Namibia"                    "NEI (BIL)"                 
# [31] "NEI (ETRO)"                 "NEI (Flag related)"         "Panama"                    
# [34] "Philippines"                "Russian Federation"         "Senegal"                   
# [37] "Seychelles"                 "Sierra Leone"               "South Africa"              
# [40] "St. Vincent and Grenadines" "Suriname"                   "Trinidad and Tobago"       
# [43] "U.S.A."                     "U.S.S.R."                   "UK.Bermuda"                
# [46] "UK.British Virgin Islands"  "UK.Sta Helena"              "UK.Turks and Caicos"       
# [49] "Uruguay"                    "Vanuatu"                    "Venezuela"                      

ll.t1$flag <- as.character(ll.t1$flag)

ussr <- ll.t1[ll.t1$flag %in% c("U.S.S.R.","Russian Federation"),]


mm <- match(ll.t1$flag,uf2)
ll.t1$flag[is.na(mm)] <- 'Other'

uf1 <- sort(unique(ll.t1$flag))

w1 <- (1:length(ll.t1[,1]))[ll.t1$yearc == 2017]
round(tapply(ll.t1$qty_t[w1],list(ll.t1$flag[w1],ll.t1$species[w1]),sum,na.rm=T))


# See task 1 totals by species

t1x <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,species = ll.t1$species),sum,na.rm=T)
t1x[t1x$species == 'BFT',]
t1x[t1x$species == 'BET',]
t1x[t1x$species == 'YFT',]
t1x[t1x$species == 'SKJ',]


# effdis_estimates<- effdis_estimates[effdis_estimates$which.ocean == "atl",]
# 
# effdis_estimates$catch[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$prob[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$measured_catch[effdis_estimates$observation == FALSE] <- NA
# effdis_estimates$eff[big$observation == FALSE]  <- NA

#xxx <- aggregate(list(measured_catch=big$measured_catch,catch=big$catch,eff=big$eff), 
#by=list(year=big$year,species=big$species),sum,na.rm=T)

#xxx.90 <- xxx[xxx$year == 1950 & xxx$species == 'bft',]
#sum(xxx.90$catch)/1000


#Convert catch to tonnes from kgs

head(effdis_estimates)
effdis_estimates$catch <- effdis_estimates$catch/1000

effdis_estimates$measured_catch <- effdis_estimates$measured_catch/1000
effdis_estimates$measured_catch.se.fit <- effdis_estimates$measured_catch.se.fit/1000
effdis_estimates$cpue <- effdis_estimates$catch/effdis_estimates$eff # Re-calculate by tonne

##### Start combining Task 1 and 2 #########

# Catch

# Sum catch over year and flag

catch_by_year_flag <- aggregate(list(measured_catch=effdis_estimates$measured_catch,catch=effdis_estimates$catch), 
                                by=list(year=effdis_estimates$year,flagname=effdis_estimates$flagname),sum,na.rm=T)


xyplot(log(measured_catch)~log(catch)|flagname,data=catch_by_year_flag)

# Create modeled effort file - we have this problem that effort is repeated in the long format. All flags report Yellowfin.

# z1 <- effdis_estimates[effdis_estimates$flagname != "EU.España",]
# z2 <- z1[z1$species == "bet",]
# z3 <- effdis_estimates[effdis_estimates$flagname == "EU.España",]
# z4 <- rbind(z2,z3)

effdis_estimates.bet <- effdis_estimates[effdis_estimates$species == 'bet',] 

z5 <- data.frame(eff=effdis_estimates.bet$eff,longitude=effdis_estimates.bet$longitude,latitude=effdis_estimates.bet$latitude,year=effdis_estimates.bet$year,
                 month=effdis_estimates.bet$month,trend=effdis_estimates.bet$trend,flagname =effdis_estimates.bet$flagname) 

effort_by_year_flag <- aggregate(list(eff=z5$eff), 
                                 by=list(year=z5$year,flagname=z5$flagname),sum,na.rm=T)

#effort_by_year_flag$eff[effort_by_year_flag$flagname=='EU.Portugal'] <- effort_by_year_flag$eff[effort_by_year_flag$flagname=='EU.Portugal']/100

# Total catches of tunas

xyplot(catch~year|flagname,data=catch_by_year_flag,type="l",
       scales= list(relation="free"))

# Total modeled effort

xyplot(eff/1000000~year|flagname,data=effort_by_year_flag,type="l",
       scales= list(relation="free"))


# Sum Task 1 over year and flag

sum.t1 <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,flagname=ll.t1$flag),sum,na.rm=T)

## Merge task1 and task 2

## Put on modeled catch

sum.t1.t2 <-merge(sum.t1,catch_by_year_flag)

sum.t1.t2 <- orderBy(~flagname+year,data=sum.t1.t2)

plot(sum.t1.t2$qty_t,sum.t1.t2$catch) # Belize massive outlier

abline(1,1)

# Put on effort

all.t1.t2 <- merge(effort_by_year_flag,sum.t1.t2)

# Calculate CPUE (modeled cpue/modeled catch)

all.t1.t2$cpue <- all.t1.t2$catch/all.t1.t2$eff

# plot(all.t1.t2$year,log(all.t1.t2$cpue),type="n")
# text(all.t1.t2$year,log(all.t1.t2$cpue),as.character(all.t1.t2$flagname),cex=.5)

all.t1.t2$raised_effort <- all.t1.t2$qty_t/all.t1.t2$cpue

all.t1.t2$rf <- all.t1.t2$raised_effort/all.t1.t2$eff


# Put the raising factor on the effdis estimates

m1 <- paste(effdis_estimates$year,effdis_estimates$flagname)
m2 <- paste(all.t1.t2$year,all.t1.t2$flagname)

nrf <- all.t1.t2$rf[match(m1,m2)]
effdis_estimates$rf <- nrf
effdis_estimates$hooksEst <- effdis_estimates$eff*effdis_estimates$rf
effdis_estimates$hooksObs <- effdis_estimates$eff

# Put the raising factor on effdis EFFORT only estimates

m1 <- paste(z5$year,z5$flagname)
m2 <- paste(all.t1.t2$year,all.t1.t2$flagname)

nrf <- all.t1.t2$rf[match(m1,m2)]
z5$rf <- nrf

z5$hooksEst <- z5$eff*z5$rf

z5$hooksObs <- z5$eff
z5 <- z5[,-1]

#z5$hooksEst[z5$flagname=='U.S.A.']<- z5$hooksEst[z5$flagname=='U.S.A.']*3

#z5$hooksEst[z5$flagname=='EU.Portugal']<- z5$hooksEst[z5$flagname=='EU.Portugal']/100

library(lattice)


# #write.table(big2,'/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',row.names=F)
# 
# xxx <- read.table('/home/doug/effdis/data/japan-effdis-estimate.csv',sep=',',header=T)
# xxx1 <- effdis_estimates2[effdis_estimates2$flagname == 'Japan',]
# 
# par(mfrow=c(2,1))
# plot(effdis_estimates2$year[effdis_estimates2$flagname == 'Japan'],
#      effdis_estimates2$raised.effort[effdis_estimates2$flagname == 'Japan']/1000000/9,type='l',ylim=c(0,120))
# 
# plot(effdis_estimates2$year[effdis_estimates2$flagname == 'Japan'],
#      effdis_estimates2$raw_raised.effort[effdis_estimates2$flagname == 'Japan']/1000000,type='l',ylim=c(0,120))

z5.by.year.sum<-aggregate(list(hooksEst=z5$hooksEst),by=list(year=z5$year,flagname=z5$flagname),sum,na.rm=T)

p1 <- ggplot(data = z5.by.year.sum, aes(x = year, y = hooksEst/1000000)) +
  geom_line() +
  facet_wrap(~ flagname, scales = "free_y") +
  theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1))

p1

ggsave(p1,file="EffdisLLEffortEstimatesByCountry2019.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")

`%nin%` <- Negate(`%in%`)

z5.total.sum<- z5[z5$flagname %nin% c("Belize","EU.España","EU.Portugal"),]

z5.total.sum <- aggregate(list(hooksEst=z5.total.sum$hooksEst),
                           by=list(year=z5.total.sum$year),sum,na.rm=T)

zz5<-read.table(file="D://dbeare/dbeare/effdis/data/effdis-estimates-2017/effdis_ll.1950.2015.csv",sep=",",header=T) #last time
z5 <- read.table(file="D://dbeare/dbeare/effdis/data/effdis-estimates-2019/LL/effdis_ll.1950.2019.csv",sep=",",header=T)
dimnames(zz5)[[2]][9]<-'hooksRep'
dimnames(z5)[[2]][9]<-'hooksRep'

z5 <- z5[z5$flagname %nin% c("Belize","EU.España","EU.Portugal"),]

rr <- range(z5$hooksEst,na.rm=T)
plot(z5$hooksEst,z5$hooksRep,xlim=rr,ylim=rr,ylab='hooks Reported in Task 2',xlab='hooks Raised by Task 1',type='n')
text(z5$hooksEst,z5$hooksRep,as.character(z5$year),cex=.5)
abline(1,1,col='green')
cor(z5$hooksEst,z5$hooksRep,use='pairwise')

tt1 <- tapply(z5$hooksRep,z5$year,sum,na.rm=T)
plot(z5$year,z5$hooksRep)
tt2 <- tapply(z5$hooksEst,z5$year,sum,na.rm=T)
par(mfrow=c(1,1))
rr<-range(tt2)
plot(1956:2017,tt1,type='l',xlab='',ylab='# hooks',lwd=2,ylim=rr)
points(1956:2017,tt2,type='l',lwd=2,col='red')
abline(v=seq(1950,2020,by=10),col='blue',lty=2)
legend(1962,409356814,c('HooksReported','hooksEstimated'),lty=c(1,1),lwd=c(2,2),col=c('black','red'))

rr[2] <- log(rr[2])
plot(log(z5$hooksEst),log(z5$hooksRep),xlim=rr,ylim=rr,ylab='hooks Reported in Task 2',xlab='hooks Raised by Task 1')
text(log(z5$hooksEst),log(z5$hooksRep),as.character(z5$year),cex=.3)

abline(1,1,col='green')
cor(z5$hooksEst,z5$hooksRep,use='pairwise')



zz5.total.sum <- aggregate(list(hooksEst=zz5$hooksEst),
                          by=list(year=zz5$year),sum,na.rm=T)


p <- ggplot() + 
  geom_line(data = z5.total.sum, aes(x = year, y = hooksEst/1000000))+
geom_line(data = zz5.total.sum, aes(x = year, y = hooksEst/1000000),col='red') +
ggtitle("Total number of hooks raised with Task I data\n")+
  xlab('Year') +
  ylab('No Hooks (millions)')+
  
  
  theme(axis.text.x = element_text(angle = -30, hjust =0,size=15),
        
        
        plot.title = element_text(lineheight=1.2, face="bold",size = 17, colour = "grey20"),
        panel.border = element_rect(colour = "black",fill=F,size=0.5),
        panel.grid.major = element_line(colour = "grey",size=0.75,linetype='longdash'),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(size=15,colour="grey20"),
        axis.title.x=element_text(size=15,colour="grey20"),
        axis.text.y=element_text(size=15,colour="grey20"),
        panel.background = element_rect(fill = NA,colour = "black"))

p

ggsave(p,file="GlobalEffdisEstimates2019.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")


z5$geargrp <- "LL"

write.table(z5,file= "D://dbeare/dbeare/effdis/data/effdis-estimates-2019/LL/effdis_ll.1950.2019.csv",sep=",",row.names=F)
zz5<-read.table(file="D://dbeare/dbeare/effdis/data/effdis-estimates-2017/effdis_ll.1950.2015.csv",sep=",",header=T) #last time

effdis_v1 <- zz5
effdis_v1$flagname <- as.character(effdis_v1$flagname)

effdis_v1$flagname[effdis_v1$flagname == "EU.EspaÃ±a"] <-  "EU.España"


effdis_v2 <- z5

chan <- odbcConnect("effdis-local", case="postgresql", believeNRows=FALSE)
sqlQuery(chan,'drop table effdis_ll_1950_2014')
sqlSave(chan,z5,tablename='effdis_ll_1950_2014')


# Compare my estimates with Carlos' by flag

effdis_v0 <- import('d://Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/EffDIS_v0.xlsx',sheet=2)
headr <- as.character(effdis_v0[4,])
dat <- effdis_v0[5:64,]
dimnames(dat)[[2]] <- headr
for(i in 1:25){ dat[,i] <- round(as.numeric(dat[,i]))}
dimnames(dat)[[2]][25] <- 'Total'
dimnames(dat)[[2]][5] <- 'Brazil'
dimnames(dat)[[2]][24] <- 'Other'
dimnames(dat)[[2]][15] <- 'China PR'
dat$Total <- as.numeric(dat$Total)
effdis_v0 <- dat
effdis_v0 <- melt(effdis_v0,id=c("YearC"))
dimnames(effdis_v0)[[2]] <- c('year','flagname','hooksEst0')

effdis_v0$flagname <- as.character(effdis_v0$flagname)
effdis_v1$flagname <- as.character(effdis_v1$flagname)
effdis_v2$flagname <- as.character(effdis_v2$flagname)


effdis_v1 <- aggregate(list(hooksEst1=effdis_v1$hooksEst),list(year=effdis_v1$year,flagname=effdis_v1$flagname),sum)
effdis_v2 <- aggregate(list(hooksEst2=effdis_v2$hooksEst),list(year=effdis_v2$year,flagname=effdis_v2$flagname),sum)

######### Read in the long format from Carlos' file ###############

effdis_v0.lf <- import('d://Dropbox/Globefish-Consultancy-Services-2015/ICCAT-Effdis-Contract-2015/Data/EffDIS_v0.xlsx',sheet=1)
effdis_v0.lf.agg<- aggregate(list(obsHooks=effdis_v0.lf$obsHooks,estHooks=effdis_v0.lf$estHooks),list(year=effdis_v0.lf$YearC),sum,na.rm=T)

rr<-range(effdis_v0.lf.agg$obsHooks,effdis_v0.lf.agg$estHooks)
plot(effdis_v0.lf.agg$year,effdis_v0.lf.agg$obsHooks,type='l',xlim=c(1950,2017),
     ylim=rr,col='black',lwd=2,ylab='# Hooks',xlab="")
points(effdis_v0.lf.agg$year,effdis_v0.lf.agg$estHooks,type='l',col='red',lwd=2)
legend(1962,599899408,c('HooksReported','hooksEstimated'),lty=c(1,1),lwd=c(2,2),col=c('black','red'))
abline(v=seq(1950,2020,by=10),lty=2,col='blue')
title('ICCAT Estimate 2015')



sort(unique(effdis_v2$flagname))
sort(unique(effdis_v1$flagname))
sort(unique(effdis_v0$flagname))


effdis_compare <- merge(effdis_v0,effdis_v1,all=T)
effdis_compare <- merge(effdis_compare,effdis_v2,all=T)

par(mfrow=c(4,5))
uuf <- sort(unique(as.character(effdis_compare$flagname)))

for(i in c(1:10,13,14,15,17,18,21,22,24:25)){
  rr <- range(c(effdis_compare$hooksEst0[effdis_compare$flagname == uuf[i]],effdis_compare$hooksEst1[effdis_compare$flagname == uuf[i]],effdis_compare$hooksEst2[effdis_compare$flagname == uuf[i]]),na.rm=T)
  plot(effdis_compare$year[effdis_compare$flagname == uuf[i] ],effdis_compare$hooksEst1[effdis_compare$flagname == uuf[i]],xlim=c(1950,2020),ylim=rr,type='l',lwd=2)
title(uuf[i])
points(effdis_compare$year[effdis_compare$flagname == uuf[i] ],effdis_compare$hooksEst2[effdis_compare$flagname == uuf[i]],col='green',type='l',lwd=2)
points(effdis_compare$year[effdis_compare$flagname == uuf[i] ],effdis_compare$hooksEst2[effdis_compare$flagname == uuf[i]],col='red',type='l',lwd=2)
}





xyplot(hooksEst1~hooksEst2|flagname,data=effdis_compare
       )

xyplot(hooksEst0~hooksEst1|flagname,data=effdis_compare
)


wf <- 'Belize'
plot(effdis$)



