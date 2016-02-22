
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
#library(doMC)
library(effdisR)
library(lattice)



#########################
## Purse-seiners example ##
#########################

# Purse-seine example #

# Get data for each dsettype

psn  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='PS',which.flag='All',which.dsettype = 'n-')
psnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='PS',which.flag='All',which.dsettype = 'nw')
psw  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='PS',which.flag='All',which.dsettype = '-w')

ps1 <- rbind(psn,psnw,psw)

# Double-check that lat and long conversions have been done properly #

df <- data.frame(quad=ps1$quadid,lat=ps1$lat,lon=ps1$lon,square=ps1$squaretypecode) # simplify data.frame
df$square <- as.character(df$square) # function requires input to be 'character'

hl <- length(df[,1])
ndf <- data.frame(quad=rep(NA,hl),lat=rep(NA,hl),lon=rep(NA,hl),square=rep(NA,hl))
for(i in 1:hl){
  ndf[i,] <- latLon(df[i,]) # calculate the central lat long of each grid looping thro' each row
}

plot(ndf$lon,ndf$lat,pch=".")

ps1$longitude <- ndf$lon # add on lat and long to main database
ps1$latitude <- ndf$lat

table(ps1$squaretypecode,ps1$flagname)

t2 <- tapply(ps1$totsp9,list(ps1$flagname,ps1$year),sum,na.rm=T)

data("seas")
ps1<-find.ocean(ps1)
ps1 <- ps1[ps1$which.ocean == 'atl',]
ps1<-prepare.effdis.data(input=ps1)

ps1<-ps1[ps1$squaretypecode == '1x1',]

library(reshape2)
pslf <- convert2long.format.t2(input =ps1)

#bm <- model.nos.kgs(input=pslf,which.gear='PS')

#pslf <- kgs.from.nos.r(pslf) # Not relevant for PS 

table(pslf$eff1type)

# -none-  D.AT SEA    D.FISH FISH.HOUR   NO.SETS  NO.TRIPS  SUC.D.FI  SUC.SETS 
# 9729      1881    237051    531621     24642       369        27       126 

pslf <- pslf[pslf$eff1type %in% c('D.FISH','FISH.HOUR'),]
pslf$eff1[pslf$eff1type == 'D.FISH'] <- pslf$eff1[pslf$eff1type == 'D.FISH']*24
pslf$eff1type <- 'FISH.HOUR'

w0 <- (1:length(pslf$year))[pslf$catchunit == 'kg']

round(tapply(pslf$measured_catch[w0],list(pslf$flagname[w0],pslf$species[w0]),sum,na.rm=T))/1000

pslf <- pslf[pslf$catchunit == "kg",]
pslf$species <- as.character(pslf$species)

dim(pslf) #=  768672 (2015)

us <- sort(as.character(unique(pslf$species)))
uf <- sort(as.character(unique(pslf$flagname)))

 uf

#  [1] "Belize"                "Brazil"                "Canada"                "Cape Verde"            "Congo"                 "Côte D'Ivoire"        
#  [7] "Curaçao"               "EU.España"             "EU.France"             "EU.Portugal"           "Ghana"                 "Ghana (ICCAT program)"
#  [13] "Guatemala"             "Guinée Rep."           "Japan"                 "Mixed flags (FIS)"     "NEI (ETRO)"            "Panama"               
#  [19] "Russian Federation"    "U.S.A."                "Venezuela"            
#  
 
 
setwd('/home/dbeare/effdis/effdis/data/effdis-estimates')
setwd("c:/Users/DBeare/effdis/effdis/data/effdis-estimates")

#Flags to add to "others"- 

# oth <-c("Argentina", "Angola", "Barbados", "Cape Verde","China (ICCAT program)", "Côte D'Ivoire","EU.France",
#         "Chinese Taipei (foreign obs.)", "Cuba (ICCAT program)" ,"Dominica","EU.United.Kingdom","Faroe Islands",
#         "FR.St Pierre et Miquelon","Grenada","Guinea Ecuatorial","Honduras","Iceland","Japan (foreign obs.)",
#         "Maroc","Mexico","Panama","Philippines","Sierra Leone","Trinidad and Tobago","UK.Sta Helena","UK.Turks and Caicos","Uruguay","U.S.S.R.")
# 
# idx <- (1:length(pslf[,1]))[pslf$flagname %in% oth]
# 
# pslf$flagname[idx] <- "Other"


#Japan

emod.jap <- fitGAMtoEffort(input=pslf,which.flag='Japan',which.effort='FISH.HOUR',start.year=1980,end.year=2015,kk=6)

mod.jap <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.jap[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag='Japan',which.species=sp,start.year=1980,end.year=2015,kk=6)
  print(sp)
}


for(i in 1:9)
{
  if(mod.jap[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.jap[[i]],which.gear="PS", effmod=emod.jap,grid.res=1,start.year=1990,end.year=2015,which.flag='Japan')
    rm(aa)
    gc(reset=T)
  }
}


aa <- predict.effdis.t2.data(cmod=mod.jap[[9]],which.gear="PS", effmod=emod.jap,grid.res=1,start.year=1990,end.year=2015,which.flag='Japan',observations=F)
plot.mods(input=aa,cmod=mod.jap[[9]],which.year=1995,which.month=1,grid.res=7,which.value = 'prob',which.gear='PS',plot.samples.only=F)
    

#Cape Verde

 emod<- pslf[pslf$flagname == "Cape Verde",]

emod.cv <- fitGAMtoEffort(input=pslf,which.flag='Cape Verde',which.effort='FISH.HOUR',start.year=1980,end.year=2015,kk=3)

mod.cv <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.cv[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag='Cape Verde',which.species=sp,start.year=1980,end.year=2015,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.cv[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cv[[i]],which.gear="PS", effmod=emod.cv,grid.res=1,start.year=1980,end.year=2015,which.flag='Cape Verde')
    rm(aa)
    gc(reset=T)
  }
}

#Curaçao

emod.cur <- fitGAMtoEffort(input=pslf,which.flag="Curaçao",which.effort='FISH.HOUR',start.year=1980,end.year=2015,kk=3)

mod.cur <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.cur[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="Curaçao",which.species=sp,start.year=1980,end.year=2015,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.cur[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.cur[[i]],which.gear="PS", effmod=emod.cur,grid.res=1,start.year=1980,end.year=2015,which.flag="Curaçao")
    rm(aa)
    gc(reset=T)
  }
}

#EU.Espana

emod.esp <- fitGAMtoEffort(input=pslf,which.flag="EU.España",which.effort='FISH.HOUR',start.year=1980,end.year=2015,kk=9)

mod.esp <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.esp[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="EU.España",which.species=sp,start.year=1980,end.year=2015,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.esp[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.esp[[i]],which.gear="PS", effmod=emod.esp,grid.res=1,start.year=1980,end.year=2015,which.flag="EU.España")
    rm(aa)
    gc(reset=T)
  }
}


#EU.France

emod.fra <- fitGAMtoEffort(input=pslf,which.flag="EU.France",which.effort='FISH.HOUR',start.year=1980,end.year=2015,kk=9)

mod.fra <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.fra[[i]] <- fit2stageGAMtoCatch(input=pslf,which.flag="EU.France",which.species=sp,start.year=1980,end.year=2015,kk=9)
  print(sp)
}

for(i in 1:9)
{
  if(mod.fra[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.esp[[i]],which.gear="PS", effmod=emod.esp,grid.res=1,start.year=1980,end.year=2015,which.flag="EU.France")
    rm(aa)
    gc(reset=T)
  }
}


















##########################################
######## Longliners  #####################
##########################################

# Get data for each dsettype

lln  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'n-')
llnw <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = 'nw')
llw  <- get.effdis.t2.data(which.dsn='effdis-tuna-cc1',which.gear='LL',which.flag='All',which.dsettype = '-w')


ll1 <- rbind(lln,llnw,llw)







# Set up data for modeled estimate (have to have some spatial detail)
ll1 <- convert.grid.res(ll1) # Some data, eg. Portugal are supplied at 1x1 but for the modeling we need to be consistent.
data("seas")
ll1<-find.ocean(ll1)
ll1 <- ll1[ll1$which.ocean == 'atl',]
#table(ll1$squaretypecode,ll1$flagname)

ll1 <- ll1[ll1$squaretypecode == '5x5',]
lllf <- convert2long.format.t2(input =ll1) # NB I do this not because it matters for the modeling but because I need to know where the data were and want to do predictions on an ordely grid?
lllf<-prepare.effdis.data(input=lllf)

bm <- model.nos.kgs(input=lllf,which.gear='LL')
lllf <- kgs.from.nos(lllf) # for those fleets that supply only number
#lllf <- lllf[lllf$eff1type=='NO.HOOKS',] # Quite a no of eff1type = '-none-' but they are important e.g. Spanish LL so leave them in here, model them and use the model to predict effort at the places where it is none or something else.
lllf <- lllf[lllf$catchunit == "kg",]
lllf$species <- as.character(lllf$species)

dim(lllf) #= 2079549 (2010) 1690155 (2015)

# # Set up data to estimate from the raw data
# data("seas")
# ll2<-find.ocean(ll1)
# ll2 <- ll2[ll2$which.ocean == 'atl',]
# lllf2 <- convert2long.format.t2(input =ll2)
# lllf2<-prepare.effdis.data(input=lllf2)
# bm2 <- model.nos.kgs(input=lllf2,which.gear='LL')
# lllf2 <- kgs.from.nos(lllf2) # for those fleets that supply only number
# lllf2 <- lllf2[lllf2$eff1type=='NO.HOOKS',]
# dim(lllf2) #= 2139498

us <- sort(as.character(unique(lllf$species)))
uf <- sort(as.character(unique(lllf$flagname)))


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


#setwd('/home/doug/effdis/data')
setwd("c:/Users/DBeare/effdis/effdis/data/effdis-estimates")

#Flags to add to "others"- 

oth <-c("Argentina", "Angola", "Barbados", "Cape Verde","China (ICCAT program)", "Côte D'Ivoire","EU.France",
        "Chinese Taipei (foreign obs.)", "Cuba (ICCAT program)" ,"Dominica","EU.United.Kingdom","Faroe Islands",
        "FR.St Pierre et Miquelon","Grenada","Guinea Ecuatorial","Honduras","Iceland","Japan (foreign obs.)",
        "Maroc","Mexico","Panama","Philippines","Sierra Leone","Trinidad and Tobago","UK.Sta Helena","UK.Turks and Caicos","Uruguay","U.S.S.R.")

idx <- (1:length(lllf[,1]))[lllf$flagname %in% oth]

lllf$flagname[idx] <- "Other"

bra <- lllf[lllf$flagname == "Brazil",]

plot(bra$year,bra$eff1)

#Argentina

#emod.arg <- fitGAMtoEffort(input=lllf,which.flag='Argentina',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)
# 
# mod.arg <- as.list(1:9)
# for(i in 1:9)
# {
#   sp <- us[i]
#   mod.arg[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Argentina',which.species=sp,start.year=1990,end.year=2015,kk=3)
#   print(sp)
# }
# 
# for(i in 1:9)
# {
#   if(mod.arg[[i]]=='Insufficient data to support model')
#   {print('no model')
#   }
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.arg[[i]], effmod=emod.arg,grid.res=5,start.year=1990,end.year=2015,which.flag='Argentina')
#     rm(aa)
#     gc(reset=T)
#   }
# }

# Not enough data for Argentina


#Japan

#jap <- lllf[lllf$flagname == "Japan" & lllf$year == 1956 & lllf$month == 6,]


plot(jap$trend,jap$eff1)
jap.big <- jap[jap$eff1>1000000,]
plot(jap.big$trend,jap.big$eff1)

emod.jap <- fitGAMtoEffort(input=lllf,which.flag='Japan',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)

mod.jap <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.jap[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Japan',which.species=sp,start.year=1990,end.year=2015,kk=6)
  print(sp)
}

for(i in 1:9)
{
  if(mod.jap[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.jap[[i]], effmod=emod.jap,grid.res=5,start.year=1990,end.year=2015,which.flag='Japan')
    rm(aa)
    gc(reset=T)
  }
}



#Belize


belize <- lllf[lllf$flagname == "Belize" & lllf$species == "alb",]
plot(belize$trend,belize$eff1)
belize.big <- belize[belize$eff1>1000000,]
plot(belize.big$trend,belize.big$eff1)

# Divide Belize data by 10 for 2010 and 2012

q0 <- (1:length(lllf$flagname))[lllf$flagname == "Belize" & lllf$year %in% c(2010,2012)]
lllf$eff1[q0] <- lllf$eff1[q0]/10

emod.belize <- fitGAMtoEffort(input=lllf,which.flag='Belize',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)

mod.belize <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.belize[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Belize',which.species=sp,start.year=1990,end.year=2015,kk=3)
  print(sp)
}

for(i in 1:9)
{
  if(mod.belize[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.belize[[i]], effmod=emod.belize,grid.res=5,start.year=1990,end.year=2015,which.flag='Belize')
    rm(aa)
    gc(reset=T)
  }
}


#Brasil

emod.brasil <- fitGAMtoEffort(input=lllf,which.flag='Brazil',which.effort='NO.HOOKS',start.year=1990,end.year=2015,k=3)

mod.brasil <- as.list(1:9)
for(i in 1:9)
{
  sp <- us[i]
  mod.brasil[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Brazil',which.species=sp,start.year=1990,end.year=2015)
  print(sp)
}
  
for(i in 1:9)
{
  if(mod.brasil[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
  aa <- predict.effdis.t2.data(cmod=mod.brasil[[i]], effmod=emod.brasil,grid.res=5,start.year=1990,end.year=2015,which.flag='Brazil')
  rm(aa)
  gc(reset=T)
}
}

#Canada

emod.canada <- fitGAMtoEffort(input=lllf,which.flag='Canada',which.effort='NO.HOOKS',start.year=1990,end.year=2015,k=3)

mod.can <- as.list(1:9)
for(i in c(1,2,4:7,9))
{
  sp <- us[i]
  mod.can[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Canada',which.species=sp,start.year=1990,end.year=2015,k=3)
  print(sp)
}

for(i in c(1,2,4:7,9))
{
  if(mod.can[[i]]=='Insufficient data to support model')
  {print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.can[[i]], effmod=emod.canada,grid.res=5,start.year=1990,end.year=2015,which.flag='Canada')
    rm(aa)
    gc(reset=T)
  }
}


#China P.R


emod.china.pr <- fitGAMtoEffort(input=lllf,which.flag='China PR',which.effort='NO.HOOKS',start.year=1990,end.year=2015)

mod.china.pr <- as.list(1:9)

for(i in c(1:2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.china.pr[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='China PR',which.species=sp,start.year=1990,end.year=2015,kk=6)
}

for(i in c(1:2,4:9))
{
  if(mod.china.pr[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.china.pr[[i]], effmod=emod.china.pr,grid.res=5,start.year=1990,end.year=2015,which.flag='China PR')
    rm(aa)
    gc(reset=T)
  }
}


#Chinese Taipei

emod.ct <- fitGAMtoEffort(input=lllf,which.flag='Chinese Taipei',which.effort='NO.HOOKS',start.year=1990,end.year=2015)

mod.ct <- as.list(1:9)

for(i in c(1,2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.ct[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Chinese Taipei',which.species=sp,start.year=1990,end.year=2015,kk=6)
}

for(i in c(1,2,4:9))
{
  if(mod.ct[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ct[[i]], effmod=emod.ct,grid.res=5,start.year=1990,end.year=2015,which.flag='Chinese Taipei')
    rm(aa)
    gc(reset=T)
  }
}




#### Cuba

# emod.cuba <- fitGAMtoEffort(input=lllf,which.flag='Cuba',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)
# 
# mod.cuba <- as.list(1:9)
# 
# for(i in c(1:6,8:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.cuba[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Cuba',which.species=sp,start.year=1990,end.year=2015,kk=3)
# }
# 
# for(i in c(1:6,8:9))
# {
#   if(mod.cuba[[i]]=='Insufficient data to support model')
#   {
#     print('no model')
#   }
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.cuba[[i]], effmod=emod.cuba,grid.res=5,start.year=1990,end.year=2015,which.flag='Cuba')
#     rm(aa)
#     gc(reset=T)
#   }
# }

### Spain


emod.spain <- fitGAMtoEffort(input=lllf,which.flag='EU.España',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=9)

mod.spain <- as.list(1:9)

for(i in 1:9)
{
  sp<-us[i]
  print(sp)
  mod.spain[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag="EU.España",which.species=sp,start.year=1990,end.year=2015,kk=9)
}

for(i in 1:9)
{
  if(mod.spain[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.spain[[i]], effmod=emod.spain,grid.res=5,start.year=1990,end.year=2015,which.flag='EU.España')
    rm(aa)
    gc(reset=T)
  }
}


### Portugal

emod.port <- fitGAMtoEffort(input=lllf,which.flag='EU.Portugal',which.effort='NO.HOOKS',start.year=1990,end.year=2015)

mod.port <- as.list(1:9)

for(i in 1:9)
{
  sp <- us[i]
  print(sp)
  mod.port[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='EU.Portugal',which.species=sp,start.year=1990,end.year=2015)
}

for(i in 1:9)
{
  if(mod.port[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.port[[i]], effmod=emod.port,grid.res=5,start.year=1990,end.year=2015,which.flag='EU.Portugal')
    rm(aa)
    gc(reset=T)
  }
}

# Korea

emod.kor <- fitGAMtoEffort(input=lllf,which.flag='Korea Rep.',which.effort='NO.HOOKS',start.year=1990,end.year=2015)

mod.kor <- as.list(1:9)

for(i in c(1,2,4:9))
{
  sp <- us[i]
  print(sp)
  mod.kor[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Korea Rep.',which.species=sp,start.year=1990,end.year=2015,kk=5)
}

for(i in c(1,2,4:9))
{
  if(mod.kor[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.kor[[i]], effmod=emod.kor,grid.res=5,start.year=1990,end.year=2015,which.flag='Korea Rep.')
    rm(aa)
    gc(reset=T)
  }
}





#Need to investigate Maroc


# Namibia

emod.nam <- fitGAMtoEffort(input=lllf,which.flag='Namibia',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)

mod.nam <- as.list(1:9)

for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.nam[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Namibia',which.species=sp,start.year=1990,end.year=2015,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.nam[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.nam[[i]], effmod=emod.nam,grid.res=5,start.year=1990,end.year=2015,which.flag='Namibia')
    rm(aa)
    gc(reset=T)
  }
}




# Other

emod.oth <- fitGAMtoEffort(input=lllf,which.flag='Other',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)

mod.oth <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.oth[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Other',which.species=sp,start.year=1990,end.year=2015,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.oth[[i]]=='Insufficient data to support model')
  {
    print('no model')
  }
  else{
    aa <- predict.effdis.t2.data(cmod=mod.oth[[i]], effmod=emod.oth,grid.res=5,start.year=1990,end.year=2015,which.flag='Other')
    rm(aa)
    gc(reset=T)
  }
}

# South Africa

emod.sa <- fitGAMtoEffort(input=lllf,which.flag='South Africa',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)

mod.sa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.sa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='South Africa',which.species=sp,start.year=1990,end.year=2015,kk=3)
}

for(i in c(1,2,3,4:9))
{
  if(mod.sa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.sa[[i]], effmod=emod.sa,grid.res=5,start.year=1990,end.year=2015,which.flag='South Africa')
    rm(aa)
    gc(reset=T)
  }
}

# Senegal


# senegal <- lllf[lllf$flagname == "Senegal",]
# 
# emod.sen <- fitGAMtoEffort(input=lllf,which.flag='Senegal',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)
# 
# mod.sen <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.sen[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Senegal',which.species=sp,start.year=1990,end.year=2015,kk=3)
# }
# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.sen[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.sen[[i]], effmod=emod.sen,grid.res=5,start.year=1990,end.year=2015,which.flag='Senegal')
#     rm(aa)
#     gc(reset=T)
#   }
# }
# 

# St Vincent & Grenadines

#setwd('/home/doug/effdis/effdis-estimates')

vg <- lllf[lllf$flagname == ufe[14] & lllf$species == "alb" ,]
plot(vg$trend,vg$eff1)
vg.big <- vg[vg$eff1>1000000,]
plot(vg.big$trend,vg.big$eff1)


# 2013 is just too big for St V&G.
s0 <- (1:length(lllf$flagname))[lllf$flagname == ufe[14] & lllf$year %in% c(2013)]
lllf$eff1[s0] <- lllf$eff1[s0]/10


emod.svg <- fitGAMtoEffort(input=lllf,which.flag='St. Vincent and Grenadines',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)

mod.svg <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.svg[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='St. Vincent and Grenadines',which.species=sp,start.year=1990,end.year=2015,kk=6)
}

for(i in c(1,2,3,4:9))
{
  if(mod.svg[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.svg[[i]], effmod=emod.svg,grid.res=5,start.year=1990,end.year=2015,which.flag='St. Vincent and Grenadines')
    rm(aa)
    gc(reset=T)
  }
}


# Trinidad & Tobago

#emod.tri <- fitGAMtoEffort(input=lllf,which.flag='Trinidad and Tobago',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)
# 
# mod.tri <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.tri[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Trinidad and Tobago',which.species=sp,start.year=1990,end.year=2015,kk=3)
# }
# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.tri[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.tri[[i]], effmod=emod.tri,grid.res=5,start.year=1990,end.year=2015,which.flag='Trinidad and Tobago')
#     rm(aa)
#     gc(reset=T)
#   }
# }

# Uruguay

# emod.uru <- fitGAMtoEffort(input=lllf,which.flag='Uruguay',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=6)
# 
# mod.uru <- as.list(1:9)
# for(i in c(1,2,3,4,5:8))
# {
#   sp <- us[i]
#   print(sp)
#   mod.uru[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Uruguay',which.species=sp,start.year=1990,end.year=2015,kk=3)
# }
# 
# for(i in c(1,2,3,4:8))
# {
#   if(mod.uru[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.uru[[i]], effmod=emod.uru,grid.res=5,start.year=1990,end.year=2015,which.flag='Uruguay')
#     gc(reset=T)
#   }
# }


# U.S.A.

emod.usa <- fitGAMtoEffort(input=lllf,which.flag='U.S.A.',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=9)

mod.usa <- as.list(1:9)
for(i in c(1,2,3,4,5:9))
{
  sp <- us[i]
  print(sp)
  mod.usa[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.A.',which.species=sp,start.year=1990,end.year=2015,kk=9)
}

for(i in c(1,2,3,4:9))
{
  if(mod.usa[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.usa[[i]], effmod=emod.usa,grid.res=5,start.year=1990,end.year=2015,which.flag='U.S.A.')
    gc(reset=T)
  }
}


# # U.S.S.R.
# 
# emod.uss <- fitGAMtoEffort(input=lllf,which.flag='U.S.S.R.',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)
# 
# mod.uss <- as.list(1:9)
# for(i in c(1,2,3,4,5:9))
# {
#   sp <- us[i]
#   print(sp)
#   mod.uss[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='U.S.S.R.',which.species=sp,start.year=1990,end.year=2015,kk=3)
# }
# 
# for(i in c(1,2,3,4:9))
# {
#   if(mod.uss[[i]]=='Insufficient data to support model')
#   {print('no model')}
#   else{
#     aa <- predict.effdis.t2.data(cmod=mod.uss[[i]], effmod=emod.uss,grid.res=5,start.year=1990,end.year=2015,which.flag='U.S.S.R.')
#     gc(reset=T)
#   }
# }


# Vanuatu.

emod.van <- fitGAMtoEffort(input=lllf,which.flag='Vanuatu',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)

mod.van <- as.list(1:9)
for(i in c(1,2,3,5:9))
{
  sp <- us[i]
  print(sp)
  mod.van[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Vanuatu',which.species=sp,start.year=1990,end.year=2015,kk=3)
}
mod.van[[4]] <- 'Insufficient data to support model'

for(i in c(1,2,3,5:9))
{
  if(mod.van[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.van[[i]], effmod=emod.van,grid.res=5,start.year=1990,end.year=2015,which.flag='Vanuatu')
    gc(reset=T)
  }
}


# Venezuela.



emod.ven <- fitGAMtoEffort(input=lllf,which.flag='Venezuela',which.effort='NO.HOOKS',start.year=1990,end.year=2015,kk=3)

mod.ven <- as.list(1:9)
for(i in c(1,2,3,4,5,7:9))
{
  sp <- us[i]
  print(sp)
  mod.ven[[i]] <- fit2stageGAMtoCatch(input=lllf,which.flag='Venezuela',which.species=sp,start.year=1990,end.year=2015,kk=3)
}

mod.ven[[6]] <- 'Insufficient data to support model'


for(i in c(1,2,3,4:5,7:9))
{
  if(mod.ven[[i]]=='Insufficient data to support model')
  {print('no model')}
  else{
    aa <- predict.effdis.t2.data(cmod=mod.ven[[i]], effmod=emod.ven,grid.res=5,start.year=1990,end.year=2015,which.flag='Venezuela')
    gc(reset=T)
    
  }}

################# Raise the data to Task I ####################################################


#setwd('/home/doug/effdis/effdis-estimates')
 # system('rm effdis-estimates.csv')
  #system('cat *csv > effdis-estimates.csv')
#effdis_estimates <- read.table('effdis-estimates.csv',sep=',')

setwd("/home/dbeare/effdis/effdis/data/effdis-estimates")
lf <- list.files()

effdis_estimates <- as.list(length(lf))
for( i in 1: length(lf)){
effdis_estimates[[i]] <- read.table(lf[i],sep=",")
print(lf[i])
}

effdis_estimates<-do.call("rbind",effdis_estimates)
dim(effdis_estimates) # = 490513

dimnames(effdis_estimates)[[2]] <- c("longitude","latitude","which.ocean","year","month","trend","flagname","geargrp","prob","prob.se.fit","measured_catch","measured_catch.se.fit",
                                     "eff","eff.se.fit","species","catch","cpue","observation")          

effdis_estimates <- orderBy(~trend+flagname+species,data=effdis_estimates)

nidx <- (1:length(effdis_estimates$year))[effdis_estimates$flagname == "Other" & effdis_estimates$species == "skj" & effdis_estimates$catch > 150000 ]

# Exclude these as they are way too big

effdis_estimates <- effdis_estimates[-nidx,]


ufe <- sort(unique(as.character(effdis_estimates$flagname)))

#Check against the raw data

wf <- ufe[14]
print(wf)
mod1 <- effdis_estimates[effdis_estimates$flagname == wf & effdis_estimates$species == "bet",]
par(mfrow=c(2,1),mar=c(2,2,2,2))
#Effort
effort <- aggt2data(input=lllf,which.effort="NO.HOOKS",which.flag=wf,start.year=1990, end.year=2015)
plot(effort$trend,effort$eff1,pch=".",xlab="",ylab="")
title(wf)
points(mod1$trend,mod1$eff,col="red",pch="*")

te <- tapply(effort$eff1,effort$year,sum)
tm <- tapply(mod1$eff,mod1$year,sum)
tem <-cbind(te,tm)

plot(te/1000000)
lines(tm/1000000)


catch <- aggt2catchdata(input=lllf,which.effort="NO.HOOKS",which.flag=wf,start.year=1990, end.year=2015)
spp <- "alb"
par(mfrow=c(1,1))
plot(catch$trend[catch$species == spp],catch$raw_measured_catch[catch$species == spp],pch=".",
     xlab="",ylab="")


mod2 <- effdis_estimates[effdis_estimates$flagname == wf ,]
points(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="red",pch=".")

plot(mod2$trend[mod2$species == spp],mod2$catch[mod2$species == spp],col="green",pch="*")

xyplot(eff/1000000~trend|flagname,data=effdis_estimates[effdis_estimates$species == "yft",],pch=".")


#Which species do we not have by flag ?

n1 <-table(effdis_estimates$flagname,as.character(effdis_estimates$species))
n1 <- ifelse(n1==0,F,T)

# Get Task 1 data
# For Long-line
ll.t1 <- get.effdis.t1.data(which.dsn='effdis-tuna-cc1',which.gear = 'LL',which.region='AT',which.flag='All')
str(ll.t1)
for(i in c(2,5:15)){ll.t1[,i] <- as.character(ll.t1[,i])}

#For bait boat
# bb.t1 <- get.effdis.t1.data(which.dsn='effdis-tuna-cc1',which.gear = 'BB',which.region='AT',which.flag='All')
# str(bb.t1)
# for(i in c(2,5:15)){bb.t1[,i] <- as.character(bb.t1[,i])}
# 
# bb.t1.rsa <- bb.t1[bb.t1$flag == 'South Africa',]
# 
# tapply(bb.t1.rsa$qty_t,list(bb.t1.rsa$year,bb.t1.rsa$species),sum,na.rm=T)
# 



1350# Datatype is either C (catch), L (Landings), DD (Discards), DM (?) and Landings

# If catch is available use that otherwise sum landings and discards.

ll.t1.c <- ll.t1[ll.t1$datatype == "C",]

ll.t1.ld <- ll.t1[ll.t1$datatype %in% c("DD","DM","L"),]
ll.t1.ld <- aggregate(qty_t~rownames+species+yearc+decade+status+flag+fleet+
                 stock+region+area+geargrp2+spcgeargrp+geargrp+gearcode,data=ll.t1.ld,sum,na.rm=T)
ll.t1<-rbind(ll.t1.c[,-15],ll.t1.ld)

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

w1 <- (1:length(ll.t1[,1]))[ll.t1$yearc == 2014]
round(tapply(ll.t1$qty_t[w1],list(ll.t1$flag[w1],ll.t1$species[w1]),sum,na.rm=T))

# See task 1 totals by species

t1x <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,species = ll.t1$species),sum,na.rm=T)
t1x[t1x$species == 'bft',]
t1x[t1x$species == 'bet',]
t1x[t1x$species == 'yft',]


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

# Create modeled effort file - we have this problem that effort is repeated in the long format. Only Spain doesnt report Bigeye.

z1 <- effdis_estimates[effdis_estimates$flagname != "EU.España",]
z2 <- z1[z1$species == "bet",]
z3 <- effdis_estimates[effdis_estimates$flagname == "EU.España",]
z4 <- rbind(z2,z3)
z5 <- data.frame(eff=z4$eff,longitude=z4$longitude,latitude=z4$latitude,year=z4$year,
                                  month=z4$month,trend=z4$trend,flagname =z4$flagname) 

effort_by_year_flag <- aggregate(list(eff=z5$eff), 
                                by=list(year=z5$year,flagname=z5$flagname),sum,na.rm=T)

# Total catches of tunas

xyplot(catch~year|flagname,data=catch_by_year_flag,type="l",
       scales= list(relation="free"))

# Total modeled effort

xyplot(eff/1000000~year|flagname,data=effort_by_year_flag,type="l",
       scales= list(relation="free"))


# Sum Task 1 over year and flag

sum.t1 <- aggregate(list(qty_t=ll.t1$qty_t),list(year=ll.t1$yearc,flagname=ll.t1$flag),sum,na.rm=T)

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

effdis_estimates$hooksEst <- effdis_estimates$eff*effdis_estimates$rf

effdis_estimates$hooksObs <- effdis_estimates$eff

# Put the raising factor on effdis EFFORT only estimates

m1 <- paste(z5$year,z5$flagname)
m2 <- paste(z5$year,z5$flagname)

nrf <- all.t1.t2$rf[match(m1,m2)]
z5$rf <- nrf

z5$hooksEst <- z5$eff*z5$rf

z5$hooksObs <- z5$eff
z5 <- z5[,-1]

library(lattice)

xyplot(raised_effort/1000000~year|flagname,data=all.t1.t2,type='b',
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




p1 <- ggplot(data = all.t1.t2, aes(x = year, y = raised_effort/1000000)) +
  geom_line() +
  facet_wrap(~ flagname, scales = "free_y") +
  theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1))

p1

ggsave(p1,file="EffdisEffortEstimatesByCountry.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")



all.t1.t2.sum <- aggregate(list(raised_effort=all.t1.t2$raised_effort),
                               by=list(year=all.t1.t2$year),sum,na.rm=T)


p <- ggplot() + 
  geom_line(data = all.t1.t2.sum, aes(x = year, y = raised_effort/1000000)) +
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

ggsave(p,file="GlobalEffdisEstimates.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")


z5$geargrp <- "LL"

write.table(z5,file="effdis_ll.1990.2014.csv",sep=",",row.names=F)
z5<-read.table("effdis_ll.1990.2014.csv",sep=",",header=T)

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlQuery(chan,'drop table effdis_ll_1990_2014')
sqlSave(chan,z5,tablename='effdis_ll_1990_2014')







plot(aa$month,aa$prob)
boxplot(catch~month,data=aa)
plot(aa$trend,log(aa$catch/1000),pch='.')
abline(v=seq(1,756,by=12),lty=2,col='blue')












