### Code to estimate long-line effort in the Atlantic and Med ##

library(rio)
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
library(RPostgreSQL)
#library(COZIGAM)
library(reshape2)
library(rgeos)


## Read in task 2 longline data from ICCAT DB ##

chan <- odbcConnect("effdis-tuna-cc1", case="postgresql", believeNRows=FALSE)
sqlTables(chan)  #List all tables in the DB
#t2ce_lf_ll <- sqlQuery(chan, "SELECT * from t2ce_long_format_ll;") # Return a table as a dataframe. Note unless you have a good connection this will take a while.

t2ce_lf_ll <- task2.lf[task2.lf$geargrpcode == 'LL' & task2.lf$month < 13,]
t2ce_lf_ll <- orderBy(~flagname+trend,data=t2ce_lf_ll)

for(i in c(4:7,12:14)) {t2ce_lf_ll[,i] <- ac(t2ce_lf_ll[,i])}

dim(t2ce_lf_ll) # 2,454,696

head(t2ce_lf_ll)

# EDA # 

table(t2ce_lf_ll$eff1type)

# D.AT SEA    D.FISH FISH.HOUR HOURS.SEA   KM.SETS LINE.DAYS  NO.BOATS  NO.HOOKS  NO.LINES    -none-   NO.NETS  NO.POLES   NO.SETS  NO.TRAPS  NO.TRIPS 
# 0       999     42093         0         0         0         0       126   2326977         0     82845         0         0      2682         0      1827 
# N.POLE-D  SUC.D.FI  SUC.SETS    TRAP D 
# 0        36       549         0 

# Only use NO.HOOKS

t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$eff1type == 'NO.HOOKS',]


table(t2ce_lf_ll$catchunit,t2ce_lf_ll$flagname)
# 
# 
# Belize Brasil China P.R. Chinese Taipei   Cuba EU.Cyprus EU.España EU.Greece EU.Italy EU.Malta EU.Portugal  Japan Korea Rep.  Maroc Mexico Namibia  Other Panama
# --      0    351         27            684      9         0        54      1575      486      810           0   3465        585     45     45     189    981     54
# kg   5670  74709      10593         202158    369      1863     95517     25218    10962     9198       15291      0      73935    585   4662    7560  76770   4869
# nr      0   9477        828         138825  20691         0     91872      5022       18      162           0 314343      17316      0    702       0    576      0
# 
# Philippines South Africa St. Vincent and Grenadines Trinidad and Tobago Uruguay U.S.A. U.S.S.R. Vanuatu Venezuela
# --           9          207                         54                   0     315  21753        0      45      2466
# kg        4509        13383                       9972                 756    9279      0     1494   85860    162270
# nr           0            0                          0                   0    7812 687609        0   85860      4203
# 

t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$eff1type != '--',]
t2ce_lf_ll <- t2ce_lf_ll[t2ce_lf_ll$catchunit != '--',]

## Get the depths ## 

# t2ce_distinct_locations <- readOGR(dsn="PG:host=134.213.29.249 user=postgres password=Postgres1 dbname=effdis port=5432", layer = "public.t2ce_distinct_locations_covariates", verbose = TRUE)

# t2ce_distinct_locations <- readOGR(dsn="PG:dbname=effdis host=134.213.29.249 user=postgres password=Postgres1", layer = "public.world_seas", verbose = TRUE)

t2ce_lf_ll <- add.covariates.r(input = t2ce_lf_ll, what.dsn = 'effdis-tuna-cc1')$output

# Who reports kgs and/or nrs ?

tt0<- table(t2ce_lf_ll$flagname,t2ce_lf_ll$catchunit,t2ce_lf_ll$year)
tt1<- melt(tt0)

t_kg <- tt1[tt1[,2]=='kg',]
t_nr <- tt1[tt1[,2]=='nr',]
tt2 <- data.frame(t_kg,t_nr[,4])

only.nrs <- (1:length(tt2[,1]))[tt2$value ==0 & tt2$t_nr...4. > 0] # Combinations who only reported numbers. Mostly Japan and USA.

# Who reports kgs and/or nrs by species ?

pp0<- table(t2ce_lf_ll$flagname,t2ce_lf_ll$catchunit,t2ce_lf_ll$year,t2ce_lf_ll$species)
pp1<- melt(pp0)
dimnames(pp1)[[2]] <- c('flagname','catchunit','year','species','value')

# Plot the relationship between nrs and kgs for different species, flag combinations

library(lattice)
xyplot(value~year|catchunit*species,data=pp1)

plot(pp1$value[pp1$catchunit=='nr'],pp1$value[pp1$catchunit =='kg'])

# Just the positive part will do

pp1$value[pp1$value ==0] <- NA

pp2 <- pp1[!is.na(pp1),]

plot(pp1$value[pp1$catchunit=='nr'],pp1$value[pp1$catchunit =='kg'],type='n',xlim=c(0,2000),ylim=c(0,2000))
text(pp1$value[pp1$catchunit=='nr'],pp1$value[pp1$catchunit =='kg'],pp1$flagname[pp1$catchunit=='kg'],cex=.5)

# Model kg data as a function of nr, flag and species

p_kg <- pp1[pp1$catchunit=='kg',]
p_nr <- pp1[pp1$catchunit=='nr',]
ppp <- data.frame(p_kg,nr=p_nr$value)
dimnames(ppp)[[2]][5] <- 'kg'

plot(ppp$kg[ppp$species == 'bet'],ppp$nr[ppp$species == 'bet'])

ppp$lkg <- log(ppp$kg)
ppp$lnr <- log(ppp$nr)

m1 <- lm(lkg~lnr,data=ppp,na.action='na.omit')
m2 <- lm(lkg~lnr+year,data=ppp,na.action='na.omit')
m3 <- lm(lkg~lnr+year+species,data=ppp,na.action='na.omit')
m4 <- lm(lkg~lnr+year+species+flagname,data=ppp,na.action='na.omit')
m5 <- lm(lkg~lnr+year+flagname,data=ppp,na.action='na.omit')

anova(m1,m2,m3,m4)

summary(m3)

t_kg <- tt1[tt1[,2]=='kg',]
t_nr <- tt1[tt1[,2]=='nr',]
tt2 <- data.frame(t_kg,t_nr[,4])

only.nrs <- (1:length(tt2[,1]))[tt2$value ==0 & tt2$t_nr...4. > 0] # Combinations who only reported numbers. Mostly Japan and USA.
tt1[only.nrs,]

# Do data with kgs only first #

three.d.effort.by.year.r(what.year='2006',what.flag='Belize',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Brasil',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='China P.R.',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Chinese Taipei',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Cuba',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Cyprus',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.España',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Greece',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Italy',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Malta',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='EU.Portugal',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Japan',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Korea Rep',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Maroc',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Mexico',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Namibia',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Other',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Panama',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Philippines',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='South Africa',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='St. Vincent and Grenadines',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Trinidad and Tobago',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Uruguay',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='U.S.A',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='U.S.S.R',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Vanuatu',scaling.f=100000)
three.d.effort.by.year.r(what.year='2006',what.flag='Venezuela',scaling.f=100000)


three.d.catch.by.year.r(tdata=t2ce_lf_ll,what.year='2006',what.flag='All',what.species='alb',scaling.f=100,catchunit = "kg")

## Loop round catch and effort data ##


yrs <- as.character(1950:2010)
us <- ac(sort(unique(task2.lf$species)))
flgs <- ac(sort(unique(task2.lf$flagname)))

# Catch by flag (kg)

for(i in us){
  for(j in yrs){
    for(k in flgs){
      print(c(i,j,k))
three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag=k,what.species=i,scaling.f=10,catchunit='kg')
    }}}

# Catch by flag (nr)

for(i in us){
  for(j in yrs){
    for(k in flgs){
      print(c(i,j,k))
      three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag=k,what.species=i,scaling.f=10,catchunit='nr')
    }}}

# Catch by All (kg)

for(i in us){
  gc()
  for(j in yrs){
    gc()
      print(c(i,j))
      three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag='All',what.species=i,scaling.f=10,catchunit='kg')
    }}

# Catch by All (nr)
for(i in us){
  gc()
  for(j in yrs){
    gc()
      print(c(i,j))
      three.d.catch.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag='All',what.species=i,scaling.f=10,catchunit='nr')
    }}


# Effort by flag (kg)

  for(j in yrs){
    for(k in flgs){
      print(c(i,j))
      three.d.effort.by.year.r(tdata = t2ce_lf_ll,what.year=j,what.flag=k,scaling.f=10,effort.type='NO.HOOKS')
    }}

## Estimate kgs for countries with only nrs using m3 ##

head(t2ce_lf_ll)

## Split data into kg and nr ##

t2ce_lf_ll_kg <- t2ce_lf_ll[t2ce_lf_ll$dsettype %in% c('-w','nw'),]
t2ce_lf_ll_kg <- t2ce_lf_ll_kg[t2ce_lf_ll_kg$catchunit  != "--",]

t2ce_lf_ll_nr <- t2ce_lf_ll[t2ce_lf_ll$dsettype == 'n-',] # Only data where only nunmbers have been supplied

# Convert measured catch to kgs with m5 #

t2ce_lf_ll_nr$lnr<- log(t2ce_lf_ll_nr$measured_catch)

aa <- predict(m1,t2ce_lf_ll_nr,type='response')

t2ce_lf_ll_nr$measured_catch <- exp(aa)
aa[t2ce_lf_ll_nr$measured_catch == 0] <- 0

# Recombine with t2ce_lf_ll_kg #

t2ce_lf_ll_nr$dsettype <- '-w'
t2ce_lf_ll_nr$catchunit <- 'kg'
t2ce_lf_ll_nr <- t2ce_lf_ll_nr[,-20]

head(t2ce_lf_ll_nr)

t2ce_lf_ll <- rbind(t2ce_lf_ll_kg,t2ce_lf_ll_nr)

# Make depth +ve

t2ce_lf_ll$ldepth_m <- log(t2ce_lf_ll$depth_m* -1)

# Binarary variable

t2ce_lf_ll$bin <- ifelse(t2ce_lf_ll$measured_catch==0,0,1)

t2ce_lf_ll$ldepth_m[t2ce_lf_ll$ldepth_m == 0] <- NA


###########################################
# Model measured catch in kilos with gam #
##########################################


## Prepare data ## 

us <- sort(unique(t2ce_lf_ll$species))

for (i in us) {
  print(i)
what.species <- i


setwd('/home/doug/effdis/data')

dat <- t2ce_lf_ll[t2ce_lf_ll$species ==what.species,]

dat <- find.ocean.r(dat)$output
dat <- dat[dat$which.ocean == 'atl',]

## CPUE by species ##

dat1 <- aggregate(list(measured_catch=dat$measured_catch,eff1=dat$eff1), 
                  by=list(trend=dat$trend,month=dat$month,longitude=dat$longitude,latitude=dat$latitude),sum)

#dat2 <- add.covariates.r(input=dat1)$output
dat2 <- as.data.frame(dat2)

#dat2$depth_m <- dat2$depth_m * -1
#dat2$ldepth_m <- log(dat2$depth_m)

#dat <- dat[!is.na(dat$ldepth_m),]

bin <- ifelse(dat2$measured_catch==0,0,1)
dat2$bin <- bin
#dat2$lmeasured_catch <- log(dat2$measured_catch+1)

#Binomial model for probability of catch

bs<-"cr"

b1 <- gam(bin~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=quasibinomial(link="logit"),method="REML",data=dat2)

#Gamma model for task 2 catch 

g1 <- gam(measured_catch~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=Gamma(link="log"),method="REML",data=dat2[dat2$bin==1,])

# Poisson model for nhooks only needs to be done once
 if (what.species == 'alb')
   {
h1 <- gam(eff1~te(longitude,latitude,k=12,bs=bs)+te(trend,k=6,bs=bs)+te(month,k=3,bs=bs),family=quasipoisson(link="log"),method="REML",data=dat2)
}

## Build grid for predictions ## 

min.lat <- min(dat2$latitude+1)
max.lat <- max(dat2$latitude-1)
min.lon <- min(dat2$longitude+1)
max.lon <- max(dat2$longitude-1)
grid.res <- 5
t1 <- min(dat2$trend)
t2 <- max(dat2$trend)
lonnie <- seq(min.lon,max.lon,by=grid.res)
lattie <- seq(min.lat,max.lat,by=grid.res)
lo <- length(lonnie)
la <- length(lattie)

grd <-data.frame(expand.grid(longitude=lonnie,latitude=lattie))
grd <- find.ocean.r(input=grd[,c(1,2)])$output

plot(grd$longitude[grd$which.ocean=='atl'],grd$latitude[grd$which.ocean=='atl'])

#grd <- add.covariates.r(input=grd)$output
#grd <- as.data.frame(grd)

#grd$ldepth_m <- grd$depth_m*-1
#grd$ldepth_m <- log(grd$ldepth_m)


start.year <- 1950
end.year   <-  2010
lyrs       <- length(start.year:end.year)
ltrnd     <- lyrs*12

ngrd <- data.frame(longitude=rep(grd$longitude,ltrnd),latitude=rep(grd$latitude,ltrnd),which.ocean=rep(grd$which.ocean,ltrnd),
         year = rep(start.year:end.year,rep((lo*la),lyrs)), month = rep(rep(1:12,rep(lo*la,12)),lyrs))
ngrd$trend <- trend.r(ngrd$year,ngrd$month,start.year=1950)

# Do the predictions over the grid #

prob <- predict(b1,ngrd,type="response")
measured_catch <- predict(g1,ngrd,type="response")
eff <- predict(h1,ngrd,type="response")

# Block out the land #

prob[ngrd$which.ocean %in% c('land','med','pac')] <- NA
measured_catch[ngrd$which.ocean %in% c('land','med','pac')] <- NA
eff[ngrd$which.ocean %in% c('land','med','pac')] <- NA

# Convert to vectors

ngrd$prob <- as.vector(prob)
ngrd$measured_catch <- as.vector(measured_catch)
ngrd$eff <- as.vector(eff)

filename <- paste('model-data-',what.species,'.csv',sep='')
write.table(ngrd,file=filename,sep=',',row.names=F)

}






### Plot the data to see if they look sensible ####

which.trend <- 649

image(lonnie,lattie,matrix(ngrd$prob[ngrd$trend == which.trend],length(lonnie),length(lattie)))
contour(lonnie,lattie,matrix(ngrd$prob,length(lonnie),length(lattie)),add=T)

image(lonnie,lattie,matrix(log(ngrd$measured_catch),length(lonnie),length(lattie)),col=topo.colors(1000))
contour(lonnie,lattie,matrix(ngrd$measured_catch,length(lonnie),length(lattie)),add=T)


image(lonnie,lattie,matrix(ngrd$eff,length(lonnie),length(lattie)),col=topo.colors(10))
contour(lonnie,lattie,matrix(ngrd$eff,length(lonnie),length(lattie)),add=T)

ngrd$catch <- ngrd$prob*ngrd$measured_catch
ngrd$cpue <- ngrd$catch/ngrd$eff

image(lonnie,lattie,matrix(log(ngrd$catch),length(lonnie),length(lattie)),col=topo.colors(100))
contour(lonnie,lattie,matrix(log(ngrd$catch),length(lonnie),length(lattie)),add=T)

#par(mfrow=c(1,1))
for(i in 649:696){
  print(i)
symbols(dat2$longitude[dat2$trend == i],dat2$latitude[dat2$trend == i],sqrt(dat2$measured_catch[dat2$trend == i])/1000,add=T)
}

cc <- sum(grd$catch,na.rm=T)
ee <- sum(grd$eff,na.rm=T)

head(t1det9sp)

xx <- t1det9sp[t1det9sp$geargrp == 'LL' & t1det9sp$year == 2006 & t1det9sp$species == 'alb',]

t1 <- sum(xx$qty_t) *1000
cpue <- cc/ee
t1/cpue
sum(dat1$eff1)

