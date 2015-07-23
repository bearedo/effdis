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

t2ce_distinct_locations_covariates <- sqlQuery(chan, "SELECT *, ST_AsText(the_geom_4326) AS the_point from t2ce_distinct_locations_covariates;") 

# m <- dbDriver("PostgreSQL")
# con <- dbConnect(m, dbname="effdis", host = "134.213.29.249", user = "postgres")
# q <- "SELECT *, ST_AsText(the_geom_4326) AS geom from t2ce_distinct_locations_covariates;"
# rs <-  dbSendQuery(con,q)
# df <-  fetch(rs,n=-1)

tdlc     <- SpatialPointsDataFrame(cbind(x=an(ac(t2ce_distinct_locations_covariates$longitude)),y=an(ac(t2ce_distinct_locations_covariates$lat))),data=t2ce_distinct_locations_covariates)

geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.

tdlc@proj4string <- geogWGS84

# Put covariates on

t2ce_lf_ll$depth_m <- tdlc@data$depth_m[match(paste(t2ce_lf_ll$longitude,t2ce_lf_ll$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
t2ce_lf_ll$m_ann_sst <- tdlc@data$m_ann_sst[match(paste(t2ce_lf_ll$longitude,t2ce_lf_ll$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
t2ce_lf_ll$aq_prim_pro <- tdlc@data$aq_prim_pro[match(paste(t2ce_lf_ll$longitude,t2ce_lf_ll$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
t2ce_lf_ll$m_ann_chla <- tdlc@data$m_ann_chla[match(paste(t2ce_lf_ll$longitude,t2ce_lf_ll$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]

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


three.d.catch.by.year.r(tdata=t2ce_lf_ll,what.year='1984',what.flag='EU.Italy',what.species='alb',scaling.f=1,catchunit = "kg")

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

## Split data into kg and nr

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


dat <- t2ce_lf_ll[t2ce_lf_ll$year %in% c(2006,2007,2008) & t2ce_lf_ll$species =='alb',]
# CPUE over all 9 species
dat <- aggregate(list(measured_catch=dat$measured_catch,eff1=dat$eff1), 
                  by=list(trend=dat$trend,longitude=dat$longitude,latitude=dat$latitude),sum)


dat$depth_m <- as.vector(tdlc@data$depth_m)[match(paste(dat$longitude,dat$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
dat$depth_m <- dat$depth_m 

#dat <- dat[!is.na(dat$ldepth_m),]
dat$bin <- ifelse(dat$measured_catch==0,0,1)
dat$lmeasured_catch <- log(dat$measured_catch+1)


#Binomial model for probability of catch
#b0 <- gam(bin~1,family=quasibinomial(link="logit"),data=dat)
b1 <- gam(bin~te(longitude,latitude,by = trend,k=11),family=quasibinomial(link="logit"),method="REML",data=dat)

#Gamma model for task 2 catch
zz1 <- gam(measured_catch~te(longitude,latitude,by=trend,k=11),family=Gamma(link="log"),method="REML",
           data=dat[dat$bin==1,])




#b2 <- gam(bin~lo(trend)+lo(month)+lo(lon)+lo(lat)*species,family=quasibinomial(link="logit"),data=alb)
#Poisson model for nhooks
h0 <- gam(eff1~1,family=quasipoisson(link="log"),data=dat)
h1 <- gam(eff1~te(longitude,latitude,by=trend,k=11),family=quasipoisson(link="log"),method="REML",data=dat)
#detach("package:mgcv", unload=TRUE)

min.lat <- min(dat$latitude)
max.lat <- max(dat$latitude)
min.lon <- min(dat$longitude)
max.lon <- max(dat$longitude)
grid.res <- 5
t1 <- min(dat$trend)
t2 <- max(dat$trend)
lonnie <- seq(min.lon,max.lon,by=grid.res)
lattie <- seq(min.lat,max.lat,by=grid.res)
lo <- length(lonnie)
la <- length(lattie)

grd <-data.frame(expand.grid(longitude=lonnie,latitude=lattie))

find.ocean.r <- function(data=grd)
  {

   # Function takes a data frame of locations (must be called latitude and longitue) and adds a vector telling you 
  # whether it is in Atlantic, Pacific or Med. The definitions of the 'Atlantic' etc are very general.
  
seas <- readOGR(dsn="/home/doug/effdis/data", layer="World_Seas") # World seas and oceans
seas@proj4string <- geogWGS84
seas.polys <- as.character(sort(unique(seas@data$NAME)))

wo <- grep('Atl',seas.polys) # Find all relevant polygons
wi <- grep('Med',seas.polys)
wj <- grep('Adriatic',seas.polys) # Find all relevant polygons
wk <- grep('Aegean',seas.polys)
wl <- grep('Balearic',seas.polys) # Find all relevant polygons
wm <- grep('Bay of Biscay',seas.polys)
wn <- grep('Bristol',seas.polys) # Find all relevant polygons
wp <- grep('Caribbean',seas.polys)
wq <- grep('Celtic',seas.polys) # Find all relevant polygons
wr <- grep('English Channel',seas.polys)
ws <- grep('Lawrence',seas.polys) # Find all relevant polygons
wt <- grep('Inner Seas',seas.polys)
wu <- grep('Ionian',seas.polys) # Find all relevant polygons
wv <- grep('Irish',seas.polys)
wx <- grep('North Sea',seas.polys) # Find all relevant polygons
wz <- grep('Gibra',seas.polys) # Find all relevant polygons
wa <- grep('Ligurian',seas.polys) # Find all relevant polygons
wzz <- grep('Tyrr',seas.polys) # Find all relevant polygons
wxx <- grep('Alb',seas.polys) # Find all relevant polygons
wmm <- grep('Mex',seas.polys) # Find all relevant polygons
wpa  <- grep('Pacific',seas.polys)
gog  <- grep('Guin',seas.polys)

# Create multi-polygons for selection.
atlantic <- seas[seas@data$NAME %in% seas.polys[c(wo,wm,wn,wp,wq,wr,ws,wt,wv,wx,wmm,gog)],] # create object of Atlantic polygons
med <-      seas[seas@data$NAME %in% seas.polys[c(wi,wj,wk,wl,wu,wz,wa,wzz,wxx)],]
pacific   <- seas[seas@data$NAME %in% seas.polys[c(wpa)],]

# Make SpatialPointsDataFrame

data.spdf      <- SpatialPointsDataFrame(cbind(x=an(ac(data$longitude)),y=an(ac(grd$latitude))),data=grd)
geogWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Make sure proj is what we think it is.
data.spdf@proj4string <- geogWGS84

# Work out which ocean the data are in
idx.atl <- over(data.spdf,atlantic)
idx.med <- over(data.spdf,med)
idx.pac <- over(data.spdf,pacific)

# Add vector denoting which ocean

which.ocean      <- rep(NA,length(grd[,1]))
which.ocean[which(is.na(idx.atl[,1])==TRUE)] <- 'at'
which.ocean[which(is.na(idx.med[,1])==FALSE)] <- 'me'
which.ocean[which(is.na(idx.pac[,1])==FALSE)] <- 'pa'

grd$which.ocean <- which.ocean

out <- list(output=data)
out}

grd$depth_m <- as.vector(tdlc@data$depth_m)[match(paste(grd$longitude,grd$latitude), paste(tdlc@data$longitude,tdlc@data$latitude))]
grd$ldepth_m <- grd$depth_m*-1
grd$ldepth_m <- log(grd$ldepth_m)

grd$month <- 6
grd$trend <- 673
grd$year <- 2006
grd$species <- as.factor('ALB')
prob <- predict(b1,grd,type="response")
measured_catch <- predict(zz1,grd,type="response")
eff <- predict(h1,grd,type="response")

prob[which.ocean != 'AT'] <- NA
measured_catch[which.ocean != 'AT'] <- NA
eff[which.ocean != 'AT'] <- NA

grd$prob <- as.vector(prob)
grd$measured_catch <- as.vector(measured_catch)
grd$eff <- as.vector(eff)
image(lonnie,lattie,matrix(grd$prob,length(lonnie),length(lattie)))
contour(lonnie,lattie,matrix(grd$prob,length(lonnie),length(lattie)),add=T)

image(lonnie,lattie,matrix(grd$measured_catch,length(lonnie),length(lattie)),col=topo.colors(10))
contour(lonnie,lattie,matrix(grd$measured_catch,length(lonnie),length(lattie)),add=T)

image(lonnie,lattie,matrix(grd$eff,length(lonnie),length(lattie)),col=topo.colors(10))
contour(lonnie,lattie,matrix(grd$eff,length(lonnie),length(lattie)),add=T)

grd$catch <- grd$prob*grd$measured_catch
grd$cpue <- grd$catch/grd$eff

image(lonnie,lattie,matrix(grd$catch,length(lonnie),length(lattie)),col=topo.colors(10000))
contour(lonnie,lattie,matrix(grd$catch,length(lonnie),length(lattie)),add=T)

cc <- sum(grd$catch,na.rm=T)
ee <- sum(grd$eff,na.rm=T)

head(t1det9sp)

xx <- t1det9sp[t1det9sp$geargrp == 'LL' & t1det9sp$year == 2006 & t1det9sp$species == 'alb',]

t1 <- sum(xx$qty_t) *1000
cpue <- cc/ee
t1/cpue
sum(dat1$eff1)

